library(SWOMSE)

data.frame(Year=SWOData@Year, Catch=SWOData@Cat[1,], Index=SWOData@Ind[1,])

# Reference OMs - Historical ----
OM.root <- 'G:/My Drive/1_Projects/North_Atlantic_Swordfish/OMs'


Ref_OMs <- OM_DF |> dplyr::filter(Class=='Reference')

getTS <- function(i) {
  dir <- basename(OM_DF$dir[i])
  TSbio <- readRDS(file.path(OM.root, '/OM_objects/Timeseries', dir, 'timeseries.rda'))
  df <- OM_DF[i,]
  df <- df %>% select(-dir)
  cbind(df, TSbio)
}

getTS2022 <- function(i) {
  dir <- basename(OM_DF$dir[i])
  TSbio <- readRDS(file.path(OM.root, '/OM_objects_2022/Timeseries', dir, 'timeseries.rda'))
  df <- OM_DF[i,]
  df <- df %>% select(-dir)
  cbind(df, TSbio)
}

TSBio_List <- lapply(1:nrow(Ref_OMs), getTS)
TSBio <- do.call('rbind', TSBio_List)

p <- ggplot(TSBio, aes(x=year, y=B.Bmsy)) +
  facet_grid(steepness~M) +
  geom_line(size=1) +
  geom_hline(yintercept = 1, color='darkgray', size=1, linetype=2) +
  expand_limits(y=0) +
  theme_bw() +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Natural Mortality (M)", breaks = NULL, labels = NULL),
                     expand = c(0, .5)) +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Steepness (h)", breaks = NULL, labels = NULL),
                     expand = c(0, .1)) +
  labs(x='Year', y=expression(SB/SB[MSY])) +
  theme(strip.text = element_text(size=12),
        axis.title=element_text(size=18),
        axis.text=element_text(size=12))

ggsave('Figures/OM-Reference-SB_SBMSY-Historical.png', p)

# Compare with previous
TSBio_List <- lapply(1:nrow(Ref_OMs), getTS2022)
TSBio2022 <- do.call('rbind', TSBio_List)

TSBio$Year <- 2024
TSBio2022$Year <- 2022

DF <- dplyr::bind_rows(TSBio, TSBio2022)
DF$Year <- factor(DF$Year)

p <- ggplot(DF, aes(x=year, y=B.Bmsy, color=Year)) +
  facet_grid(steepness~M) +
  geom_line(size=1) +
  geom_hline(yintercept = 1, color='darkgray', size=1, linetype=2) +
  expand_limits(y=0) +
  theme_bw() +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Natural Mortality (M)", breaks = NULL, labels = NULL),
                     expand = c(0, .5)) +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Steepness (h)", breaks = NULL, labels = NULL),
                     expand = c(0, .1)) +
  labs(x='Year', y=expression(SB/SB[MSY])) +
  theme(strip.text = element_text(size=12),
        axis.title=element_text(size=18),
        axis.text=element_text(size=12))
p

ggsave('Figures/OM-Reference-SB_SBMSY-Historical-Comparison.png', p)

# CMP Tuning ----


MSE_files <- list.files('MSE_Objects')
MPs <- c('CE', 'MCC85a', 'MCC97c', 'SPSSFox', 'SPSSFox2')
# 'MCC85a' = MCC9, 'MCC97c = MCC11'


# Example plot
mp <- 'MCC9'
mm <- 2

MSE_list <- list()
MP_files <- MSE_files[grepl(paste0(MPs[mm],'-Reference'), MSE_files)]
for (i in seq_along(MP_files)) {
  MSE_list[[i]] <- readRDS(file.path('MSE_Objects', MP_files[i]))
}

PMs <- c('PGK', 'PBMSY')

Period <- c('Short', 'Medium', 'Long', 'All')
Yrs <- list(Short=c(3,12),
            Medium=c(13,22),
            Long=c(23, 32),
            All=c(3,32))

Year <- 2025:2054

PeriodDF <- data.frame(Year=Year, Period='Short')
PeriodDF$Period[PeriodDF$Year %in% 2035:2044] <- 'Medium'
PeriodDF$Period[PeriodDF$Year >2044] <- 'Long'
PeriodDF$Period <- factor(PeriodDF$Period, levels=unique(PeriodDF$Period), ordered = TRUE)

PMs <- c('PGK', 'PBMSY', 'PNOF')

Period <- c('Short', 'Medium', 'Long', 'All')
Yrs <- list(Short=c(3,12),
            Medium=c(13,22),
            Long=c(23, 32),
            All=c(3,32))

## overall ----
Overall <- combine_MMSE(MSE_list, 'MCC9')

sb <- Overall@SB_SBMSY[,1,1,3:32]
median <- apply(sb, 2, median)
lower <- apply(sb, 2, quantile, 0.05)
upper <- apply(sb, 2, quantile, 0.95)
df <- data.frame(
  Year=Year,
  Median=median,
  Lower=lower,
  Upper=upper)

f_fmsy <- Overall@F_FMSY[,1,1,1,3:32]
median <- apply(f_fmsy, 2, median)
lower <- apply(f_fmsy, 2, quantile, 0.05)
upper <- apply(f_fmsy, 2, quantile, 0.95)
fdf <- data.frame(
  Year=Year,
  Median=median,
  Lower=lower,
  Upper=upper)


pm_list <- list()
for (pm in seq_along(PMs)) {
  period_list <- list()
  fun <- get(PMs[pm])
  for (period in seq_along(Period)) {
    period_list[[period]] <- data.frame(OM=i,
                                        MP=MPs[mm],
                                        Tuning=c('a', 'b'),
                                        PM=PMs[pm],
                                        Period=Period[period],
                                        Value=fun(Overall,
                                                  Yrs=Yrs[[period]])@Mean)
  }
  pm_list[[pm]] <- do.call('rbind', period_list)
}

tbl <- do.call('rbind', pm_list) |>
  dplyr::filter(Tuning=='a') |>
  dplyr::select(PM, Period, Value)
tbl$Value <- signif(tbl$Value,2)

tt <- tbl |> dplyr::filter(PM=='PGK') |>
  dplyr::select(PGK=Period, Value)

t2 <- tbl |> dplyr::filter(PM=='PBMSY') |>
  dplyr::select(PBMSY=Period, Value)

t3 <- tbl |> dplyr::filter(PM=='PNOF') |>
  dplyr::select(PNOF=Period, Value)

PGK_table <- tibble(x = -Inf,
              y = -Inf,
              tbl = list(tt))

PBMSY_table <- tibble(
                x = Inf,
                y = -Inf,
                tbl = list(t2))

PFMSY_table <- tibble(
  x = -Inf,
  y = -Inf,
  tbl = list(t3))

p1 <- ggplot(df, aes(x=Year)) +
  geom_ribbon(aes(ymin=Lower , ymax=Upper), alpha=0.7, fill='grey') +
  geom_line(aes(y=Median), linewidth=1.2) +
  expand_limits(y=c(0)) +
  geom_line(data=PeriodDF, aes(x=Year, y = 1, color=Period),
            linetype=2, linewidth=1) +
  ggpp::geom_table(data = PGK_table,
                   aes(x = x, y = y, label = tbl),
                   hjust = 'inward', vjust = 'inward') +
  ggpp::geom_table(data = PBMSY_table,
                   aes(x = x, y = y, label = tbl),
                   hjust = 'inward', vjust = 'inward') +
  labs(y='SB/SBMSY') +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0.01)) +
  scale_color_manual(values=c('blue', 'green', 'red'))+
  guides(color='none') +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16))

p2 <- ggplot(fdf, aes(x=Year)) +
  geom_ribbon(aes(ymin=Lower , ymax=Upper), alpha=0.7, fill='grey') +
  geom_line(aes(y=Median), linewidth=1.2) +
  expand_limits(y=c(0)) +
  geom_line(data=PeriodDF, aes(x=Year, y = 1, color=Period),
            linetype=2, linewidth=1) +
  ggpp::geom_table(data = PFMSY_table,
                   aes(x = x, y = y, label = tbl),
                   hjust = 'inward', vjust = 'inward') +
  labs(y='F/FMSY') +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0.01)) +
  scale_color_manual(values=c('blue', 'green', 'red'))+
  guides(color='none') +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16))

library(patchwork)
p <- p1 + p2

ggsave('Figures/CMP-tune_overall.png', p, width=9, height=4.5)

## by OM ----

omList <- list()
PGKlist <- PBMSYlist <- PNOFlist <- list()
for (i in seq_along(MP_files)) {
  sb <- MSE_list[[i]]@SB_SBMSY[,1,1,3:32]
  median <- apply(sb, 2, median)
  lower <- apply(sb, 2, quantile, 0.05)
  upper <- apply(sb, 2, quantile, 0.95)
  omList[[i]] <- data.frame(OM=i,
                            Year=Year,
                            Median=median,
                            Lower=lower,
                            Upper=upper)

  pm_list <- list()
  for (pm in seq_along(PMs)) {
    period_list <- list()
    fun <- get(PMs[pm])
    for (period in seq_along(Period)) {
      period_list[[period]] <- data.frame(OM=i,
                                          MP=MPs[mm],
                                          Tuning=c('a', 'b'),
                                          PM=PMs[pm],
                                          Period=Period[period],
                                          Value=fun(MSE_list[[i]],
                                                    Yrs=Yrs[[period]])@Mean)
    }
    pm_list[[pm]] <- do.call('rbind', period_list)
  }
  tbl <- do.call('rbind', pm_list) |>
    dplyr::filter(Tuning=='a') |>
    dplyr::select(PM, Period, Value)
  tbl$Value <- signif(tbl$Value,2)

  tt <- tbl |> dplyr::filter(PM=='PGK') |>
    dplyr::select(PGK=Period, Value)

  t2 <- tbl |> dplyr::filter(PM=='PBMSY') |>
    dplyr::select(PBMSY=Period, Value)

  t3 <- tbl |> dplyr::filter(PM=='PNOF') |>
    dplyr::select(PNOF=Period, Value)

  PGKlist[[i]] <- tibble(OM=i,
                         x = -Inf,
                         y = -Inf,
                         tbl = list(tt))

  PBMSYlist[[i]] <- tibble(OM=i,
                         x = Inf,
                         y = -Inf,
                         tbl = list(t2))

  PNOFlist[[i]] <- tibble(OM=i,
                           x = -Inf,
                           y = Inf,
                           tbl = list(t3))
}


DF <- do.call('rbind', omList)
PGK_table <- do.call('rbind', PGKlist)
PBMSY_table <- do.call('rbind', PBMSYlist)
PNOF_table <- do.call('rbind', PNOFlist)

p <- ggplot(DF, aes(x=Year)) +
  facet_wrap(~OM) +
  geom_ribbon(aes(ymin=Lower , ymax=Upper), alpha=0.7, fill='grey') +
  geom_line(aes(y=Median), linewidth=1.2) +
  expand_limits(y=c(0)) +
  # geom_hline(yintercept = 1, linetype=2, color='darkgray', linewidth=1) +
  geom_line(data=PeriodDF, aes(x=Year, y = 1, color=Period),
            linetype=2, linewidth=1) +
  ggpp::geom_table(data = PGK_table,
                   aes(x = x, y = y, label = tbl),
                   hjust = 'inward', vjust = 'inward') +
  ggpp::geom_table(data = PBMSY_table,
                   aes(x = x, y = y, label = tbl),
                   hjust = 'inward', vjust = 'inward') +
  ggpp::geom_table(data = PNOF_table,
                   aes(x = x, y = y, label = tbl),
                   hjust = 'inward', vjust = 'inward') +
  labs(y='SB/SBMSY') +
  guides(color='none') +
  scale_color_manual(values=c('blue', 'green', 'red'))+
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        strip.text=element_text(size=16))


ggsave('Figures/CMP-tune_by_OM.png', p, width=16, height=12)

# MP Rankings ----

summary_PM_results <- readRDS('inst/shiny_apps/SWOMSE/data/PM_results.rda')

df <- summary_PM_results |> dplyr::filter(Model=='Reference')

df <- df |> select(PM, MP, Value, Target) |>
  filter(PM %in% c("AvTAC_long", "AvTAC_med", "AvTAC_short", 'TAC1',
                   'PGK'))

AvTAC <- df |> filter(PM%in% c('AvTAC_short', 'AvTAC_med', 'AvTAC_long')) |>
  group_by(MP, Target) |>
  summarise(PM='AvTAC', Value=mean(Value)) |>
  select(PM, MP, Value, Target) |>
  arrange(MP)

df <- bind_rows(df, AvTAC) |>
  arrange(MP)

rank_PM <- 'AvTAC_short'
df_rank <- df |>
  dplyr::filter(PM==rank_PM) |>
  arrange(desc(Value)) |>
  mutate(RelTAC=Value/Value[1])

df_rank_top <- df_rank |> filter(RelTAC==1)
df_rank_top <- df |> dplyr::filter(MP==df_rank_top$MP)

df$RefValue <- df_rank_top$Value

df |> dplyr::filter(MP=='MCC11_b')

relDF <- df |> dplyr::group_by(PM) |>
  mutate(relValue=Value/RefValue)

relDF |> dplyr::filter(MP=='MCC11_b')


pm <- 'AvTAC_short'
t1 <- relDF |> filter(PM==pm) |>
  # mutate(relValue=format(signif(relValue,3), nsmall=2)) |>
  tidyr::pivot_wider(names_from = PM, values_from = relValue) |>
  select(MP, all_of(pm)) |>
  arrange(desc(.data[[pm]]))

relDF$MP <- factor(relDF$MP, levels=t1$MP, ordered = TRUE)

pm <- 'AvTAC_med'
t2 <- relDF |> filter(PM==pm) |>
  # mutate(relValue=format(signif(relValue,3), nsmall=2)) |>
  tidyr::pivot_wider(names_from = PM, values_from = relValue) |>
  select(MP, all_of(pm)) |>
  arrange(MP)

pm <- 'AvTAC_long'
t3 <- relDF |> filter(PM==pm) |>
  # mutate(relValue=format(signif(relValue,3), nsmall=2)) |>
  tidyr::pivot_wider(names_from = PM, values_from = relValue) |>
  select(MP, all_of(pm)) |>
  arrange(MP)

pm <- 'AvTAC'
t4 <- relDF |> filter(PM==pm) |>
  # mutate(relValue=format(signif(relValue,3), nsmall=2)) |>
  tidyr::pivot_wider(names_from = PM, values_from = relValue) |>
  select(MP, all_of(pm)) |>
  arrange(MP)

pm <- 'TAC1'
t5 <- relDF |> filter(PM==pm) |>
  # mutate(relValue=format(signif(relValue,3), nsmall=2)) |>
  tidyr::pivot_wider(names_from = PM, values_from = relValue) |>
  select(MP, all_of(pm)) |>
  arrange(MP)

pm <- 'PGK'
t6 <- relDF |> filter(PM==pm) |>
  # mutate(Value=format(signif(Value,2)), nsmall=2) |>
  tidyr::pivot_wider(names_from = PM, values_from = Value) |>
  select(MP, all_of(pm)) |>
  arrange(MP)

DF <- t1 |> left_join(t2) |>
  left_join(t3) |>
  left_join(t4) |>
  left_join(t5) |>
  left_join(t6)

DF

library(DT)

DT::datatable(DF,
              options = list(
                dom = 't',
                lengthChange = FALSE,
                columnDefs = list(list(width = '50px', targets = 0:6)))) |>
  DT::formatRound(columns=2:7, digits=2)



# Robustness ----

summary_TS_results <- readRDS('inst/shiny_apps/SWOMSE/data/summary_TS_results.rda')
summary_PM_results <- readRDS('inst/shiny_apps/SWOMSE/data/PM_results.rda')
kobe_results <- readRDS('inst/shiny_apps/SWOMSE/data/kobe_results.rda')

KobeDF <- kobe_results %>% filter(Model!='Reference') %>%
  tidyr::pivot_longer(., cols=6:9)
KobeDF$name <- factor(KobeDF$name, levels=c('br', 'tr', 'bl', 'tl'),
                      ordered = TRUE)
cols <- c('green', 'orange', 'yellow', 'red')

KobeDF$Tuning <- 'b'
KobeDF$Tuning[grepl('_c', KobeDF$MP)] <- 'c'




kobe_df <- bind_rows(
  data.frame(x=c(0,0, 1, 1), y=c(0,1, 1,0), fill='bl'),
  data.frame(x=c(1,1, 2, 2), y=c(0,1, 1,0), fill='br'),
  data.frame(x=c(0,0, 1, 1), y=c(1,2, 2,1), fill='tl'),
  data.frame(x=c(1,1, 2, 2), y=c(1,2, 2,1), fill='tr'))


ggplot(kobe_df) +
  geom_polygon(aes(x=x, y=y, fill=fill)) +
  scale_fill_manual(values=c('yellow',  'green', "red", 'orange')) +
  theme_bw() +
  guides(color='none', fill='none', alpha='none') +
  labs(x=expression(SB/SB[MSY]), y=expression(F/F[MSY])) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 2)) +
  theme(axis.title = element_text(size=14),
        axis.text=element_text(size=12))

ggsave('Figures/Kobe.png', width=4, height=4)



getTimeSeriesDF <- function(model='R1') {
  MPs <- summary_PM_results$MP |> unique()
  b_MPs <- MPs[grepl('_b',MPs)]
  c_MPs <- MPs[grepl('_c',MPs)]

  b1_list <- b2_list <- list()
  for (i in seq_along(b_MPs)) {
    b1_list[[i]] <- summary_TS_results %>% filter(MP==b_MPs[i], Model==model)
    b2_list[[i]]  <- summary_PM_results %>% filter(MP==b_MPs[i], Model==model)
  }

  c1_list <- c2_list <- list()
  for (i in seq_along(b_MPs)) {
    c1_list[[i]] <- summary_TS_results %>% filter(MP==c_MPs[i], Model==model)
    c2_list[[i]]  <- summary_PM_results %>% filter(MP==c_MPs[i], Model==model)
  }

  list(PKG60=list(DF=do.call('rbind',b1_list),
                    PM_ResultsMP=do.call('rbind',b2_list)),
            PGK70=list(DF=do.call('rbind',c1_list),
                    PM_ResultsMP=do.call('rbind',c2_list))
  )
}

plotKobe <- function(df, models=c('R0', 'R1'), tuning='b') {
  ggplot(df |> dplyr::filter(Tuning==tuning,
                                  Model %in% models),
         aes(x=Year, y=value, fill=name)) +
    facet_grid(Model~MP) +
    geom_bar(position="stack", stat="identity", width = 1) +
    scale_x_continuous(expand = c(0, 0),
                       breaks=seq(2025, by=10, to=2055),
                       labels=seq(2025, by=10, to=2055)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_manual(values=cols) +
    guides(fill='none') +
    labs(y="Percent of total simulatons (%)",
         x='Projection Year') +
    theme_bw() +
    theme(strip.text = element_text(size=20),
          axis.title = element_text(size=18),
          axis.text=element_text(size=14))
}

make_TS_plot <- function(model, tuning='b', width=16, height=8) {
  ind <- switch(tuning,
                'b'=1,
                'c'=2)

  tsDF <- getTimeSeriesDF(model)[[ind]]

  TimeSeriesPlot2(DF=tsDF$DF,
                  PM_ResultsMP=tsDF$PM_ResultsMP,
                  includeOM=FALSE,
                  incTable=TRUE) +
    labs(title=model) +
    theme(title=element_text(size=14))

  name <- paste0(paste('TS', model, tuning, sep='_'), '.png')
  ggsave(file.path('Figures', name),
         width=width, height=height)

  TimeSeriesPlot2(DF=tsDF$DF,
                  PM_ResultsMP=tsDF$PM_ResultsMP,
                  includeOM=FALSE,
                  incTable=FALSE) +
    labs(title=model) +
    theme(title=element_text(size=14))

  name <- paste0(paste('TS', model, tuning, 'no_table', sep='_'), '.png')
  ggsave(file.path('Figures', name),
         width=width, height=height)
}

make_TS_plot('R0')
make_TS_plot('R0', 'c')

make_TS_plot('R1')
make_TS_plot('R1', 'c')

make_TS_plot('R2')
make_TS_plot('R2', 'c')

make_TS_plot('R3')
make_TS_plot('R3', 'c')

make_TS_plot('R4')
make_TS_plot('R4', 'c')

make_TS_plot('R5')
make_TS_plot('R5', 'c')

make_TS_plot('R6')
make_TS_plot('R6', 'c')

make_TS_plot('R7')
make_TS_plot('R7', 'c')



## R0 ----

plotKobe(KobeDF, c('R0')) +
  theme(axis.title = element_text(size=14),
        strip.text = element_text(size=14))
ggsave('Figures/Kobe_R0.png', width=12, height=3)




## R1 & R2 -----

R0 <- getTS(i=5)
R1 <- getTS(i=10)

R0$Model <- 'R0'
R1$Model <- 'R1'

DF <- bind_rows(R0, R1) |>
  tidyr::pivot_longer(cols=c('B.Bmsy', 'F.Fmsy')) |>
  select(Model, year, name, value )

DF |> filter(name=='B.Bmsy')
ggplot(DF, aes(x=year, y=value, color=Model)) +
  facet_grid(~name, scales='free') +
  geom_line(linewidth=1.2) +
  geom_hline(yintercept = 1, linetype=2, color='darkgray') +
  expand_limits(y=0) +
  theme_bw()

ggsave('Figures/R0_R1.png', width=8, height=4)



plotKobe(KobeDF, c('R0', 'R1', 'R2'))
ggsave('Figures/Kobe_R0_R1.png', width=12, height=6)


## R3 ----

Ref_OM <- OM_DF %>% filter(M==0.2, steepness==0.8, Class=='Reference')
Rob_OM_a <- OM_DF %>% filter(M==0.2, steepness==0.8, Class=='R1a. Increasing Q2')

Ref_Hist <- readRDS(file.path('Hist_Objects/Reference', paste0(Ref_OM$OM.object, '.hist')))
Rob_Hist_a <- readRDS(file.path('Hist_Objects/R1a_Increasing_q', paste0(Rob_OM_a$OM.object, '.hist')))

Ref_DF_SB <- get_SSB(Ref_Hist) %>% filter(Sim==1, Stock=='Female')
Ref_DF_F <- get_F(Ref_Hist) %>% filter(Sim==1, Stock=='Female')
Ref_DF_SB$SB_SBMSY <- Ref_DF_SB$Value/Ref_Hist$Female$`Fleet 1`@Ref$ReferencePoints$SSBMSY[1]
Ref_DF_F$F_FMSY <- Ref_DF_F$Value/Ref_Hist$Female$`Fleet 1`@Ref$ReferencePoints$FMSY[1]

Ref_DF <- left_join(Ref_DF_SB %>% select(Year, SB_SBMSY),
                    Ref_DF_F %>% select(Year, F_FMSY) )
Ref_DF$Model <- 'Reference'

Rob_DF_SB_a <- get_SSB(Rob_Hist_a) %>% filter(Sim==1, Stock=='Female')
Rob_DF_F_a <- get_F(Rob_Hist_a) %>% filter(Sim==1, Stock=='Female')

Rob_DF_SB_a$SB_SBMSY <- Rob_DF_SB_a$Value/Rob_Hist_a$Female$`Fleet 1`@Ref$ReferencePoints$SSBMSY[1]
Rob_DF_F_a$F_FMSY <- Rob_DF_F_a$Value/Rob_Hist_a$Female$`Fleet 1`@Ref$ReferencePoints$FMSY[1]

Rob_DF_a <- left_join(Rob_DF_SB_a %>% select(Year, SB_SBMSY),
                      Rob_DF_F_a %>% select(Year, F_FMSY) )
Rob_DF_a$Model <- 'R3'


DF <- bind_rows(Ref_DF, Rob_DF_a) %>%
  mutate(B.Bmsy=SB_SBMSY, F.Fmsy=F_FMSY) |>
  tidyr::pivot_longer(cols=c(B.Bmsy, F.Fmsy))
DF$Model[DF$Model=='Reference'] <- 'R0'
DF$Model <- factor(DF$Model, levels=c('R0', 'R3', ordered=TRUE))

ggplot(DF, aes(x=Year, y=value, color=Model)) +
  facet_wrap(~name, scales='free_y') +
  expand_limits(y=0) +
  geom_line(linewidth=1) +
  geom_hline(yintercept=1, linetype=2) +
  theme_bw()

ggsave('Figures/R0_R3.png', width=8, height=4)



plotKobe(KobeDF, c('R0', 'R3'))
ggsave('Figures/Kobe_R0_R3.png', width=12, height=4)




## R4 & R5 ----

plotKobe(KobeDF, c('R0', 'R4', 'R5'))
ggsave('Figures/Kobe_R0_R4.png', width=12, height=6)



## R6 ----

plotKobe(KobeDF, c('R0', 'R6'))
ggsave('Figures/Kobe_R0_R6.png', width=12, height=4)



## R7 ----

plotKobe(KobeDF, c('R0', 'R7'))
ggsave('Figures/Kobe_R0_R7.png', width=12, height=4)



## Barplot ----

pms <- c('PGK',
         'nLRP',
         'AvTAC_short',
         'AvTAC_med',
         'AvTAC_long'
         )

df <- summary_PM_results |>
  dplyr::filter(Model !='Reference',
                PM %in% pms)

df$PM <- factor(df$PM, levels=pms, ordered = TRUE)

recode <- function(fact) {
  out <- rep('PGK 60 (b)', length(fact))
  out[fact==0.7] <- 'PGK 70 (c)'
  out
}

df$Target <- forcats::fct_relabel(df$Target, recode)


ggplot(df |> dplyr::filter(
                           PM=='PGK'),
       aes(x=Model, y=Value, fill=MP_name)) +
  facet_grid(PM~Target, scales='free') +
  geom_hline(yintercept = 0, color='lightgray') +
  geom_col(position="dodge") +
  theme_bw() +
  geom_vline(xintercept = (0:7)+0.5, linewidth=0.4, color='darkgray') +
  labs(fill='CMP', y='PI Value') +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12))

ggsave('Figures/Robustness_Barplot_example.png', width=12, height=4)

ggplot(df, aes(x=Model, y=Value, fill=MP_name)) +
  facet_grid(PM~Target, scales='free') +
  geom_hline(yintercept = 0, color='lightgray') +
  geom_col(position="dodge") +
  theme_bw() +
  geom_vline(xintercept = (0:7)+0.5, linewidth=0.4, color='darkgray') +
  labs(fill='CMP', y='PI Value') +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12))

ggsave('Figures/Robustness_Barplot.png', width=12, height=6)


# relative to R0

ref_PM <- summary_PM_results |>
  dplyr::filter(Model !='Reference',
                PM%in%pms,
                Model=='R0') |>
  select(Reference=Value, Target, MP_name, PM)

ref_PM$Target <- forcats::fct_relabel(ref_PM$Target, recode)

df2 <- left_join(df, ref_PM) |>
  group_by(Model, Target) |>
  mutate(Value=(Value/Reference)-1)

df2$PM <- factor(df2$PM, levels=pms, ordered = TRUE)

ggplot(df2 |> dplyr::filter(Model!='R0'), aes(x=Model, y=Value, fill=MP_name)) +
  facet_grid(PM~Target, scales='free') +
  geom_hline(yintercept = 0, color='lightgray') +
  geom_col(position="dodge") +
  theme_bw() +
  geom_vline(xintercept = (0:7)+0.5, linewidth=0.4, color='darkgray') +
  labs(fill='CMP',
       y='PI Value relative to R0') +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12))

ggsave('Figures/Robustness_Barplot2.png', width=12, height=6)


# relative to MCC11_b

ref_PM <- summary_PM_results |>
  dplyr::filter(Model !='Reference',
                PM%in%pms,
                MP=='MCC11_b') |>
  select(PM, Reference=Value, Model)
# ref_PM$Target <- forcats::fct_relabel(ref_PM$Target, recode)

df2 <- left_join(df, ref_PM) |>
  group_by(Model, Target) |>
  mutate(Value=(Value/Reference)-1)

df2$PM <- factor(df2$PM, levels=pms, ordered = TRUE)

ggplot(df2, aes(x=Model, y=Value, fill=MP_name)) +
  facet_grid(PM~Target, scales='free') +
  geom_hline(yintercept = 0, color='lightgray') +
  geom_col(position="dodge") +
  theme_bw() +
  geom_vline(xintercept = (0:7)+0.5, linewidth=0.4, color='darkgray') +
  labs(fill='CMP',
       y='PI Value relative to MCC11_b') +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12))

ggsave('Figures/Robustness_Barplot3.png', width=12, height=6)

## Kobe ----

kobe_results <- readRDS('inst/shiny_apps/SWOMSE/data/kobe_results.rda')

df <-kobe_results %>% filter(Model!='Reference') %>%
  tidyr::pivot_longer(., cols=6:9)
df$name <- factor(df$name, levels=c('br', 'tr', 'bl', 'tl'), ordered = TRUE)
cols <- c('green', 'orange', 'yellow', 'red')

df$Tuning <- 'b'
df$Tuning[grepl('_c', df$MP)] <- 'c'

p <- ggplot(df |> dplyr::filter(Tuning=='b'), aes(x=Year, y=value, fill=name)) +
  facet_grid(Model~MP) +
  geom_bar(position="stack", stat="identity", width = 1) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values=cols) +
  guides(fill='none') +
  labs(y="Percent of total simulatons (%)",
       x='Projection Year') +
  theme_bw() +
  theme(strip.text = element_text(size=20),
        axis.title = element_text(size=18),
        axis.text=element_text(size=14))

ggsave('Figures/Robustness_Kobe_b.png', p, width=16, height=10)

p <- ggplot(df |> dplyr::filter(Tuning=='b'), aes(x=Year, y=value, fill=name)) +
  facet_grid(MP~Model) +
  geom_bar(position="stack", stat="identity", width = 1) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values=cols) +
  guides(fill='none') +
  labs(y="Percent of total simulatons (%)",
       x='Projection Year') +
  theme_bw() +
  theme(strip.text = element_text(size=20),
        axis.title = element_text(size=18),
        axis.text=element_text(size=14))

ggsave('Figures/Robustness_Kobe_b_2.png', p, width=16, height=10)


## R1 -----
p <- ggplot(df |> dplyr::filter(Tuning=='b',
                                Model %in% c('R0', 'R1')), aes(x=Year, y=value, fill=name)) +
  facet_grid(Model~MP) +
  geom_bar(position="stack", stat="identity", width = 1) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values=cols) +
  guides(fill='none') +
  labs(y="Percent of total simulatons (%)",
       x='Projection Year') +
  theme_bw() +
  theme(strip.text = element_text(size=20),
        axis.title = element_text(size=18),
        axis.text=element_text(size=14))
p



## R3 -----
p <- ggplot(df |> dplyr::filter(Tuning=='b',
                                Model %in% c('R0', 'R3')), aes(x=Year, y=value, fill=name)) +
  facet_grid(Model~MP) +
  geom_bar(position="stack", stat="identity", width = 1) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values=cols) +
  guides(fill='none') +
  labs(y="Percent of total simulatons (%)",
       x='Projection Year') +
  theme_bw() +
  theme(strip.text = element_text(size=20),
        axis.title = element_text(size=18),
        axis.text=element_text(size=14))
p

### SPSSFox vs SPSSFox2  ----

ggplot(df |> dplyr::filter(MP %in% c('SPSSFox_b', 'SPSSFox2_b')),
       aes(x=Year, y=value, fill=name)) +
  facet_grid(Model~MP) +
  geom_bar(position="stack", stat="identity", width = 1) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values=cols) +
  guides(fill='none') +
  labs(y="Percent of total simulatons (%)",
       x='Projection Year') +
  theme_bw() +
  theme(strip.text = element_text(size=14),
        axis.title = element_text(size=14),
        axis.text=element_text(size=12))



df <- summary_PM_results |> dplyr::filter(MP_name %in% c('SPSSFox', 'SPSSFox2'),
                                          Model!='Reference',
                                          PM %in% c('PGK', 'AvTAC_short', 'AvTAC_med', 'AvTAC_long', 'TAC1', 'VarC')) |>
  select(Model, PM,MP, MP_name, Target, Value)


AvTAC <- df |> filter(PM%in% c('AvTAC_short', 'AvTAC_med', 'AvTAC_long')) |>
  group_by(Model, MP, MP_name, Target) |>
  summarise(PM='AvTAC', Value=mean(Value)) |>
  select(Model, PM, MP, MP_name, Target, Value) |>
  arrange(MP)

df <- bind_rows(df, AvTAC) |>
  filter(PM %in% c('PGK', 'AvTAC_med', 'AvTAC_long', 'VarC'))

stdf <- df |> group_by(Model, PM, Target) |>
  mutate(Value=Value/Value[MP_name=='SPSSFox']) |>
  ungroup()

stdf |> filter(PM=='AvTAC_med', Target==0.6, Model=='R3')

DF <- stdf |> tidyr::pivot_wider(names_from=Model, values_from = Value) |>
  select(PM,MP_name, Target, R0, R1, R2, R3, R4, R5, R6, R7) |>
  arrange(PM, Target, MP_name)

DF
DT::datatable(DF |> dplyr::filter(MP_name=='SPSSFox2'),
              options = list(
                dom = 't',
                lengthChange = FALSE,
                columnDefs = list(list(width = '50px', targets = 0:11)))) |>
  DT::formatRound(columns=4:11, digits=2)


  DT::formatStyle(
    columns=0:11,
    backgroundColor = styleEqual()
  )





ggplot(df |> filter(MP_name %in% c('SPSSFox', 'SPSSFox2')), aes(x=Year, y=value, fill=name)) +
  facet_grid(Model~MP) +
  geom_bar(position="stack", stat="identity", width = 1) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values=cols) +
  guides(fill='none') +
  labs(y="Percent of total simulatons (%)",
       x='Projection Year') +
  theme_bw() +
  theme(strip.text = element_text(size=16),
        axis.title = element_text(size=14),
        axis.text=element_text(size=12))




ggplot(df |> filter(Model %in% c('R0', 'R6', 'R7')), aes(x=Year, y=value, fill=name)) +
  facet_grid(Model~MP) +
  geom_bar(position="stack", stat="identity", width = 1) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values=cols) +
  guides(fill='none') +
  labs(y="Percent of total simulatons (%)",
       x='Projection Year') +
  theme_bw() +
  theme(strip.text = element_text(size=16),
        axis.title = element_text(size=14),
        axis.text=element_text(size=12))

ggsave('Figures/Robustness_Kobe_R6_R7.png', width=22, height=5)


TSBio_List <- lapply(1:nrow(Ref_OMs), getTS)
TSBio <- do.call('rbind', TSBio_List)



R0_Hist <- readRDS('Hist_Objects/Reference/MOM_005.hist')
R1_Hist <- readRDS('Hist_Objects/R1_Increasing_q/MOM_010.hist')


Kobe_Time()



by <- 0.1
cuts <-  seq(0.9, 1, by=by) # c(0.8, 0.85, 0.9, 0.95, 1) #
mids <- seq(cuts[1]-by, by=by, length.out=length(cuts)+1)
values <- colorRampPalette(c("lightgreen", "darkgreen"), alpha=TRUE)(length(cuts)+1)
values <- colorRampPalette(c(rgb(235, 61, 52,1, maxColorValue = 255),
                             rgb(61, 235, 52, 0.5, maxColorValue = 255)))(length(cuts)+1)

|>
  formatStyle(columns = 0:6, width='20px')


>
  DT::formatStyle(
    columns=2:5,
    backgroundColor = styleInterval(cuts, values))

cbind(mids, values)


df |> dplyr::filter(PM=='TAC1') |>
  mutate(TAC1=Value/df_rank_top$Value[df_rank_top$PM=='TAC1']) |>
  arrange(desc(RelTAC))

df |> dplyr::filter(PM=='AvTAC_med') |>
  mutate(RelTAC=Value/df_rank_top$Value[df_rank_top$PM=='AvTAC_med']) |>
  arrange(desc(RelTAC))


df |> group_by(Target, MP_name) |>
  dplyr::filter(PM=='TAC1') |>
  arrange(desc(Value)) |>
  ungroup() |>
  mutate(RelTAC=Value/Value[1])

df |> group_by(Target, MP_name) |>
  dplyr::filter(PM=='AvTAC_short') |>
  arrange(desc(Value)) |>
  ungroup() |>
  mutate(RelTAC=Value/Value[1])

df |> group_by(Target, MP_name) |>
  dplyr::filter(PM=='AvTAC_med') |>
  arrange(desc(Value)) |>
  ungroup() |>
  mutate(RelTAC=Value/Value[1])

df |> group_by(Target, MP_name) |>
  dplyr::filter(PM=='AvTAC_long') |>
  arrange(desc(Value)) |>
  ungroup() |>
  mutate(RelTAC=Value/Value[1])



df1 <- df |> dplyr::filter(PM %in% c('AvTAC_short', 'AvTAC_med')) |>
  dplyr::select(MP=MP_name , PM, Target, Value) |>
  tidyr::pivot_wider(names_from=PM, values_from = Value)

ggplot(df1, aes(x=AvTAC_short, y=AvTAC_med, color=Target, shape=MP )) +
  geom_point(size=6) +
  theme_bw()


PGK_pms <- df$PM[grepl('PGK', df$PM)]

df1 <- df |> dplyr::filter(PM %in% pms, Target==0.6) |>
  dplyr::select(Target, PGK60=Value)

df2 <- df |> dplyr::filter(PM %in% pms, Target==0.7) |>
  dplyr::select(Target, PGK70=Value)


Pdf <- bind_cols(df1, df2)


