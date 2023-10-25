library(SWOMSE)

PMs <- c("AvTAC_long", "AvTAC_med",  "AvTAC_short", "LRP", "LRP_long",
         "LRP_med", "LRP_short", "nLRP", "nLRP_long", "nLRP_med",
         "nLRP_short", "PGK", "PGK_30", "PGK_long", "PGK_med", "PGK_short",
         "PNOF", "POF", "TAC1", "VarC")


# ---- Process Results ----
Process_MSE_Results(PMs=PMs)

# ---- Save Results to Shiny App ----

Results_dirs <- list.dirs('Results', recursive = FALSE)
Results_dirs <- Results_dirs[Results_dirs!='Results/Final_MP_Results']

PM_list <- list()
TS_list <- list()
VarC_list <- list()

for (d in seq_along(Results_dirs)) {
  dir <- basename(Results_dirs[d])
  if (dir!='Reference')
    dir <- strsplit(dir, '_')[[1]][1]

  PM_results <- readRDS(file.path(Results_dirs[d], 'PM_values.rda'))
  PM_results$Model <- dir
  PM_list[[d]] <- PM_results

  TS_results <- readRDS(file.path(Results_dirs[d], 'TS_values.rda'))
  TS_results$Model <- dir
  TS_list[[d]] <- TS_results

  VarC_results  <- readRDS(file.path(Results_dirs[d], 'VarC_results.rda'))
  VarC_results$Model <- dir
  VarC_list[[d]] <- VarC_results

}

PM_results <- do.call('rbind', PM_list)
TS_results <- do.call('rbind', TS_list) %>% filter(Year>=2024)
VarC_results <- do.call('rbind', VarC_list)


# manually drop CMPs
# worse or equal performance to other variants
PM_results <- PM_results %>% filter(!MP_name%in%c('MCC3', 'CE2'))
TS_results <- TS_results %>% filter(!MP_name%in%c('MCC3', 'CE2'))
VarC_results <- VarC_results %>% filter(!MP %in%c('MCC3_a', 'MCC3_b', 'MCC3_b',
                                                 'CE2_a', 'CE2_b', 'CE2_c'))


# process TS data
summary_TS_results <- TS_results %>% tidyr::pivot_longer(., cols=c(SB_SBMSY, F_FMSY, TAC))

percentiles <- c(0.6, 0.7,  0.9)
fills <- c('#373737', '#363639', '#CDCDCD')
quantiles <- data.frame(Lower=1-percentiles, Upper=percentiles)
TS_list <- list()
for (i in 1:nrow(quantiles)) {
  TS_list[[i]] <- summary_TS_results %>%
    group_by(Year, MP, MP_name, Model, name) %>%
    summarise(Median=median(value),
              Lower=quantile(value, quantiles$Lower[i]),
              Upper=quantile(value, quantiles$Upper[i]),
              fill=fills[i],
              .groups = 'drop')
}

summary_TS_results <- do.call('rbind', TS_list)


# Kobe time results
kobe_results <- TS_results %>%
  group_by(Year, MP, MP_name, Model) %>%
  summarise(nsim=sum(SB_SBMSY>0),
            bl=sum(SB_SBMSY<1 & F_FMSY<1)/nsim*100,
            tl=sum(SB_SBMSY<1 & F_FMSY>1)/nsim*100,
            br=sum(SB_SBMSY>1 & F_FMSY<1)/nsim*100,
            tr=sum(SB_SBMSY>1 & F_FMSY>1)/nsim*100,
            .groups='drop')


saveRDS(PM_results,'inst/shiny_apps/SWOMSE/data/PM_results.rda')
saveRDS(summary_TS_results,'inst/shiny_apps/SWOMSE/data/summary_TS_results.rda')
saveRDS(kobe_results,'inst/shiny_apps/SWOMSE/data/kobe_results.rda')

saveRDS(VarC_results,'inst/shiny_apps/SWOMSE/data/Violin_results.rda')






PM_results <- readRDS('inst/shiny_apps/SWOMSE/data/PM_results.rda')
head(PM_results)
head(TS_results)



PM_results %>% filter(MP_name=='SP2', PM=='PGK_med', Value>=0.51)
PM_results %>% filter(MP_name=='SP2', PM=='LRP')

PM_results$Model %>% unique()

# ---- Identify MPs that fail LRP ----

tt <- PM_results %>% mutate(PassLRP=ifelse(Value[PM=='LRP']>0.15, TRUE, FALSE))

  filter(PM=='LRP', Value>=0.15) %>% mutate(PassLRP=FALSE)

t1 <- left_join(PM_results, tt)
head(t1)





# ---- Identify MPs that fail LRP for Reference OMs ----
PM_results <- readRDS('Results/Reference/PM_values.rda')

failLRP <- PM_results %>% filter(PM=='LRP', Value>=0.15)
failLRP

Trade_Off(PM_results, 'PGK_short', 'nLRP', xline=c(0.51,0.6,0.7),
          yline=c(0.85, 0.9, 0.95),
          lab.MPs = failLRP$MP)

Pass_LRP <- PM_results %>% filter(!MP %in% failLRP$MP)


df <- PM_results %>% filter(MP %in% failLRP$MP)
df$Value <- round(df$Value,2)
df <- df %>% filter(PM%in%c('nLRP', 'PGK_med', 'TAC1', 'AvTAC_med'))
df <- df %>% select(PM, MP, Value)
df <- df %>% tidyr::pivot_wider(., names_from=PM, values_from=Value)
DT::datatable(df,options = list(dom = 't', pageLength =50))

# ---- Identify MPs that fail PGK < 0.51 for Reference OMs ----

fail_PGK <- Pass_LRP %>%
  filter(PM %in% c('PGK_short', 'PGK_med'), Value<0.50)

fail_PGK

Trade_Off(Pass_LRP, 'PGK_short', 'PGK_med',
          xline=c(0.50),
          yline=c(0.50),
          lab.MPs = fail_PGK$MP)

Pass_PGK <- Pass_LRP %>% filter(!MP %in% fail_PGK$MP)

Pass_LRP$MP %>% unique() %>% sort()
Pass_PGK$MP %>% unique() %>% sort()


# ---- Calculated Dominated Across Reference OMs -----

# up to here

# !! make plot and report PMs !!

# fix calc_dominated function!!




DomPMs <- data.frame(PM=c('PGK_short', 'PGK_med', 'PGK_long',
                          'AvTAC_short', 'AvTAC_med', 'AvTAC_long',
                          'nLRP', 'VarC'),
                     Greater=TRUE)

DomPMs$Greater[DomPMs$PM=='VarC'] <- FALSE


PM_grid <- data.frame(PM1=c('PGK_short', 'PGK_med', 'PGK_long',
                            'nLRP', 'nLRP', 'nLRP'),
                      PM2=rep(c('AvTAC_short', 'AvTAC_med', 'AvTAC_long'), 2),
                      Non=NA,
                      Dom=NA)


Calculate_Dominated <- function(PM_grid, df) {
  for (i in 1:nrow(PM_grid)) {
    domlist <- Calc_Dominated(PM_grid[i,1:2], df)
    PM_grid$Non[i] <- list(domlist$Non)
    PM_grid$Dom[i] <- list(domlist$Dom)
  }
  PM_grid
}



Ref_dom <- Calculate_Dominated(PM_grid, Pass_PGK)

Ref_nondomMPs <- unlist(Ref_dom$Non) %>% unique() %>% sort()



# Robustness Test
# R0
R0PM_results <- readRDS('Results/R0_Ref/PM_values.rda')
R0_Pass <- R0PM_results %>% filter(MP %in% Pass_PGK$MP)
R0_dom <- Calculate_Dominated(PM_grid, R0_Pass)
R0_nondomMPs <- unlist(R0_dom$Non) %>% unique() %>% sort()

# R1
R1PM_results <- readRDS('Results/R1_Increasing_q/PM_values.rda')
R1_Pass <- R1PM_results %>% filter(MP %in% Pass_PGK$MP)
R1_dom <- Calculate_Dominated(PM_grid, R1_Pass)
R1_nondomMPs <- unlist(R1_dom$Non) %>% unique() %>% sort()

# R2
R2PM_results <- readRDS('Results/R2_Increasing_q/PM_values.rda')
R2_Pass <- R2PM_results %>% filter(MP %in% Pass_PGK$MP)
R2_dom <- Calculate_Dominated(PM_grid, R2_Pass)
R2_nondomMPs <- unlist(R2_dom$Non) %>% unique() %>% sort()

# R3
R3PM_results <- readRDS('Results/R3_CC/PM_values.rda')
R3_Pass <- R3PM_results %>% filter(MP %in% Pass_PGK$MP)
R3_dom <- Calculate_Dominated(PM_grid, R3_Pass)
R3_nondomMPs <- unlist(R3_dom$Non) %>% unique() %>% sort()

# R4
R4PM_results <- readRDS('Results/R4_Imp/PM_values.rda')
R4_Pass <- R4PM_results %>% filter(MP %in% Pass_PGK$MP)
R4_dom <- Calculate_Dominated(PM_grid, R4_Pass)
R4_nondomMPs <- unlist(R4_dom$Non) %>% unique() %>% sort()


Non_Dom_MPs <- unique(c(Ref_nondomMPs,
                        R1_nondomMPs,
                        R2_nondomMPs,
                        R3_nondomMPs,
                        R4_nondomMPs))


# plots

dom_results <- list(Reference=Ref_dom,
                    R0=R0_dom,
                    R1=R1_dom,
                    R2=R2_dom,
                    R3=R3_dom,
                    R4=R4_dom)

for (x in 1:length(dom_results)) {
  plot_list <- list()
  for (i in 1:nrow(PM_grid)) {
    tt <- PM_grid[i,]
    dom_MPs <- dom_results[[x]]$Non[[i]]
    df$nondom <- FALSE
    df$nondom[df$MP %in% dom_MPs] <- TRUE

    plot_list[[i]] <- ggplot(df, aes_string(x=tt$PM1, y=tt$PM2)) +
      theme_bw() +
      labs(x=caption_df[[tt$PM1]], y=caption_df[[tt$PM2]]) +
      geom_point(data=df, aes(color=nondom)) +
      ggrepel::geom_text_repel(data=df, aes(label=MP, color=nondom)) +
      guides(color='none')

  }
  p <- cowplot::plot_grid(plotlist=plot_list)

  ggsave(file.path('img/Dominated/', names(dom_results)[x], 'Domplot.png'), width=16, height=8)
}


domlist <- Calc_Dominated(pms=PM_grid[i,1:2], PM_results=Pass_PGK)






# ---- Table of PM Values -----
final_MPs <- list(Reference=Pass_PGK,
                  R0=R0_Pass,
                  R1=R1_Pass,
                  R2=R2_Pass,
                  R3=R3_Pass,
                  R4=R4_Pass)

for (i in seq_along(final_MPs)) {
  df <- final_MPs[[i]] %>%
    filter(MP%in%Non_Dom_MPs) %>%
    select(PM, MP, MP_name, Value)
  fl <- paste0(names(final_MPs[i]), '.rdata')
  saveRDS(df, file.path('Results/Final_MP_Results', fl))
}



# ---- Trade-Off Plots ----
for (i in seq_along(final_MPs)) {
  df <- final_MPs[[i]]

  caption_df <- df %>% distinct(PM, caption) %>%
    tidyr::pivot_wider(., names_from = PM, values_from = caption)

  df <- df %>% select(PM, Value, MP) %>%
    tidyr::pivot_wider(., names_from = PM, values_from = Value)
  df$VarC <- -df$VarC

  p1 <- ggplot(df, aes(x=AvTAC_short, y=AvTAC_med)) +
    theme_bw() +
    labs(x=caption_df$AvTAC_short, y=caption_df$AvTAC_med) +
    geom_point(data=df, aes(color=MP)) +
    ggrepel::geom_text_repel(data=df, aes(label=MP, color=MP)) +
    guides(color='none')


  p2 <- ggplot(df, aes(x=PGK_med, y=AvTAC_med)) +
    theme_bw() +
    labs(x=paste(caption_df$PGK_med, '(negative)'), y=caption_df$AvTAC_med) +
    geom_point(data=df, aes(color=MP)) +
    ggrepel::geom_text_repel(data=df, aes(label=MP, color=MP)) +
    guides(color='none') +
    expand_limits(x=c(0.5,1))

  p3 <- ggplot(df, aes(x=VarC, y=PNOF)) +
    theme_bw() +
    labs(x=paste(caption_df$VarC, '(negative)'), y=caption_df$PNOF) +
    geom_point(data=df, aes(color=MP)) +
    ggrepel::geom_text_repel(data=df, aes(label=MP, color=MP)) +
    guides(color='none') +
    expand_limits(x=c(-0.25,0), y=1)


  p4 <- ggplot(df, aes(x=VarC, y=AvTAC_med)) +
    theme_bw() +
    labs(x=paste(caption_df$VarC, '(negative)'), y=caption_df$AvTAC_med) +
    geom_point(data=df, aes(color=MP)) +
    ggrepel::geom_text_repel(data=df, aes(label=MP, color=MP)) +
    guides(color='none') +
    expand_limits(x=c(-0.25,0))

  pout <- cowplot::plot_grid(p1,p2,p3, nrow=3)
  fl <- paste0(names(final_MPs[i]), '.png')
  ggsave(file.path('img/Tradeoffs', fl), width=4, height=12)
}



# ---- MP Reports ----

MP_Report <- function(MP_name=NULL, classes=NULL) {
  all_classes <- basename(list.dirs('Results', recursive = FALSE))
  all_classes <- all_classes[!all_classes=="Final_MP_Results"]
  if (!classes %in% all_classes) stop('Directory ', classes , ' not found in `Results`')

  # load results
  for (cl in classes) {
    MP_Report_class(MP_name, cl)
  }
}

mps <-  PM_results$MP_name %>% unique()
classes <- list.dirs('Results', recursive = FALSE) %>% basename()
classes  <- classes[!classes=="Final_MP_Results"]
for (mp in mps) {
  for (cl in classes) {
    MP_Report(mp, cl)
  }
}



# Render CMP_Reports.rmd and add link on homepage
















# R1PM_results <- readRDS('Results/R1_Increasing_q/PM_values.rda')


# ---- Produce Figures ----

R1_Pass %>% filter(PM=='LRP', Value>=0.15)

R1_Pass %>%
  filter(PM %in% c('PGK_short', 'PGK_med', 'PGK_long'), Value<0.51)



# Time-Series Plots
TS_results <- readRDS('Results/Reference/TS_values.rda')
R1TS_results <- readRDS('Results/R1_Increasing_q/TS_values.rda')

final_TS_results <- TS_results %>% filter(MP %in% Non_Dom_MPs)
final_R1TS_results <- R1TS_results %>% filter(MP %in% Non_Dom_MPs)
#
# Time_Series_Plot <- function(df) {
#
#   alpha <- 0.7
#   fill1 <- 'darkgrey'
#   fill2 <- 'lightgrey'
#   yline <- NULL
#
#   df <- df %>% filter(Year>=2024) %>%
#     tidyr::pivot_longer(., cols=c('SB_SBMSY','TAC')) %>%
#     group_by(Year, MP, name) %>%
#     summarise(Median=median(value),
#               Lower1=quantile(value, 0.1),
#               Lower2=quantile(value, 0.25),
#               Upper1=quantile(value, 0.9),
#               Upper2=quantile(value, 0.75))
#
#   p <- ggplot(df, aes(x=Year)) +
#     facet_grid(name~MP, scales='free') +
#     geom_ribbon(aes(ymin=Lower1 , ymax=Upper1), fill=fill1, alpha=alpha) +
#     geom_ribbon(aes(ymin=Lower2 , ymax=Upper2), fill=fill2, alpha=alpha) +
#     geom_line(aes(y=Median)) +
#     expand_limits(y=0) +
#     theme_bw() +
#     geom_hline(yintercept=yline, linetype=2) +
#     scale_x_continuous(expand = c(0, 0)) +
#     scale_y_continuous(expand = c(0, 0)) +
#     geom_hline(yintercept = c(0.4,1), linetype=2)
#   p
#
#
# }

Time_Series_Plot(final_TS_results)
ggsave('img/August_2023/Ref_TS.png', width=12, height=4)

Time_Series_Plot(final_R1TS_results)
ggsave('img/August_2023/R1_TS.png', width=12, height=4)


final_TS_results %>% filter(MP=='SPSSFox_c', Year==2025, Sim%in%1:5)

final_TS_results %>% filter(MP=='MCC4_c', SB_SBMSY<=0.4)

df %>% filter(MP=='MCC4_c')


# Trade-Off Plots
df <- Pass_PGK %>% filter(MP %in% Non_Dom_MPs)

df <- R1_Pass %>% filter(MP %in% Non_Dom_MPs)

caption_df <- df %>% distinct(PM, caption) %>%
  tidyr::pivot_wider(., names_from = PM, values_from = caption)

df <- df %>% select(PM, Value, MP) %>%
  tidyr::pivot_wider(., names_from = PM, values_from = Value)


p1 <- ggplot(df, aes(x=PGK_med, y=AvTAC_med)) +
  theme_bw() +
  labs(x=caption_df$PGK_med, y=caption_df$AvTAC_med) +
  geom_point(data=df, aes(color=MP)) +
  ggrepel::geom_text_repel(data=df, aes(label=MP, color=MP)) +
  guides(color='none') +
  expand_limits(x=c(0,1))

p1

Kobe_Time()

Pass_PGK %>% filter(MP=='GSC2_c', PM=='AvTAC_short')
R1_Pass %>% filter(MP=='GSC2_c', PM=='AvTAC_short')

Pass_PGK %>% filter(MP=='GSC2_c', PM=='PGK_med')
R1_Pass %>% filter(MP=='GSC2_c', PM=='PGK_med')


p2 <- ggplot(df %>% filter(!MP %in% df_nondom$MP), aes(x=VarC, y=AvTAC_med)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label=MP)) +
  geom_point(data=df_nondom, aes(color=MP)) +
  ggrepel::geom_text_repel(data=df_nondom, aes(label=MP, color=MP)) +
  theme_bw() +
  labs(x=caption_df$VarC, y='') +
  guides(color='none') +
  theme(axis.text.y=element_blank())

p3 <- ggplot(df %>% filter(!MP %in% df_nondom$MP), aes(x=PGK_med, y=AvTAC_med)) +
  geom_point() +
  expand_limits(x=0.5) +
  ggrepel::geom_text_repel(aes(label=MP)) +
  geom_point(data=df_nondom, aes(color=MP)) +
  ggrepel::geom_text_repel(data=df_nondom, aes(label=MP, color=MP)) +
  theme_bw() +
  labs(x=caption_df$PGK_med, y=caption_df$AvTAC_med) +
  guides(color='none')

p4 <- ggplot(df %>% filter(!MP %in% df_nondom$MP), aes(x=nLRP, y=AvTAC_med)) +
  geom_point() +
  expand_limits(x=0.95) +
  ggrepel::geom_text_repel(aes(label=MP)) +
  geom_point(data=df_nondom, aes(color=MP)) +
  ggrepel::geom_text_repel(data=df_nondom, aes(label=MP, color=MP)) +
  theme_bw() +
  labs(x=caption_df$nLRP, y='') +
  guides(color='none') +
  theme(axis.text.y=element_blank())


cowplot::plot_grid(p1,p2,p3, p4, nrow=2, rel_widths = c(1.05, 1))


df <- TS_results %>% filter(MP_name %in% c('SPSSFox'))

df <- df %>% group_by(Year, MP) %>%
  summarise(Median=median(SB_SBMSY),
            Lower=quantile(SB_SBMSY,.1),
            Upper=quantile(SB_SBMSY,.9))
plot

alpha <- 0.7
fill1 <- 'darkgrey'
fill2 <- 'lightgrey'
ggplot(df, aes(x=Year, y=Median)) +
  facet_grid(~MP) +
  geom_ribbon(aes(ymin=Lower , ymax=Upper), fill=fill1, alpha=alpha) +
  geom_ribbon(aes(ymin=Lower , ymax=Upper), fill=fill2, alpha=alpha) +
  geom_line() +
  theme_bw() +
  expand_limits(y=0) +
  geom_hline(yintercept = c(0.4,1), linetype=2)




PM_results %>% filter(MP_name=='SPSS', PM%in% c('LRP', 'nLRP'))
0.82222+0.177777

PM_results %>% filter(MP_name %in% c('SPSS', 'SPSS2'),
                      PM=='PGK_med')


SPSS
SPSS2

# ---- R1. ------

MSE <- readRDS(file.path(MSE.dir, MSE.files[1]))

colSums(MSE@TAC[1,,1,1,]) %>% plot(., ylim=c(0,16000), type='l')
colSums(MSE@Catch[1,,1,1,]) %>% lines(col='blue')
colSums(MSE@Removals[1,,1,1,]) %>% lines(col='red')

colSums(MSE@TAC[1,,1,1,])
colSums(MSE@Catch[1,,1,1,])
Data <- MSE@PPD[[1]][[1]][[1]]
Data@MPrec[1]
Data2 <- Trim_Data(MSE@PPD[[1]][[1]][[1]], 2023)
Data2@MPrec[1]

PGK_short(MSE)
PGK_med(MSE)
EA1


Data <- Trim_Data(MSE@PPD[[1]][[1]][[1]], 2023)
Data@MPrec[1]
EA1_a(1, Data)



CE_a(1, Data)



MSE.files <- list.files('MSE_Objects', pattern='.mse')

mmse <- readRDS(file.path('MSE_Objects', MSE.files[3]))

which.min(mmse@SB_SBMSY[,1,1,33])

x <- 33
seq(2024, by=3, length.out=10)
Data <- mmse@PPD[[1]][[1]][[1]]
Data <- Trim_Data(Data, 2041)
Data@Year

Data@MPrec[x]
CI1(x, Data)

par(mfrow=c(1,2))
plot(2021:2053,mmse@SB_SBMSY[x,1,1,], type='l')
plot(2021:2053, apply(mmse@TAC[x,,1,1,], 2, sum), type='l')


Biomass <- get_Biomass(mmse) %>% filter(Sim==3, MP %in% c(NA, 'CE_a'))
get_Index <- function(MMSE) {
  nsim <- MMSE@nsim
  nstocks <- MMSE@nstocks
  nfleets <- MMSE@nfleets
  Years <- get_Years(MMSE)
  Years <- Years$Year[1:(nrow(Years)-1)]

  MPs <- MMSE@MPs[[1]]
  nMPs <- length(MPs)

  df_list <- list()
  for (mm in 1:nMPs) {
    df_list[[mm]] <- data.frame(Sim=1:nsim,
                                Year=rep(Years, each=nsim),
                                Index=as.vector(MMSE@PPD[[1]][[1]][[mm]]@Ind),
                                MP=MPs[mm])
  }
  do.call('rbind',df_list )
}

CI <- get_Index(mmse) %>% filter(Sim==1)

Biomass <- Biomass %>% group_by(Year) %>% summarise(Value=sum(Value))

Biomass$include <- FALSE
Biomass$include[Biomass$Year %in% 1999:2020] <- TRUE
b_mean <- mean(Biomass$Value[Biomass$include==TRUE])
Biomass$stB <- Biomass$Value/b_mean

CI$StIndex <- CI$Index/mean(CI$Index[CI$Year %in% 1999:2020])

df <- left_join(Biomass, CI)

ggplot(df, aes(x=Year)) +
  geom_line(aes(y=stB), color='blue') +
  geom_line(aes(y=StIndex), color='red') +
  expand_limits(y=0) +
  labs(y='Combined Index') +
  theme_bw()


