
library(SWOMSE)

CMP_files <- list.files('CMPs')
for (fl in CMP_files) source(file.path('CMPs', fl))

Tune_MPs <- c('SPFox3', 'SPS2', 'MCC', 'GSC')

# Scope
Tuning_OMs <- OM_DF %>% filter(Class=='Reference')
Tuning_OMs <- Tuning_OMs$OM.object

# Scope & Tune
for (i in seq_along(Tune_MPs)) {
  MP_name <- Tune_MPs[i]
  tt <- try(Scope(MP_name, Tuning_OMs, TuneTargets))
  #  Plot_Scope(MP_name)
}

TuneTarget <- TuneTargets %>% filter(Code=='a')

for (i in seq_along(Tune_MPs)) {
  MP_name <- Tune_MPs[i]
  tt <- try(Tune(MP_name, Tuning_OMs, TuneTarget))
}


TuneTarget <- TuneTargets %>% filter(Code=='b')

for (i in seq_along(Tune_MPs)) {
  MP_name <- Tune_MPs[i]
  tt <- try(Tune(MP_name, Tuning_OMs, TuneTarget))
}

TuneTarget <- TuneTargets %>% filter(Code=='c')

for (i in seq_along(Tune_MPs)) {
  MP_name <- Tune_MPs[i]
  tt <- try(Tune(MP_name, Tuning_OMs, TuneTarget))
}


# ---- Trade-Offs from Tune Objects ----

fls <- list.files('Tuning_Objects', pattern='.tune')

MPs <- unlist(strsplit(fls, '.tune'))
MP_df <- data.frame(Name=MPs, MP=c(paste0(MPs, '_a'), paste0(MPs, '_b'), paste0(MPs, '_c')),
                    Target=rep(c(0.51,0.60, 0.70), each=length(MPs)))

tune_PM <- 'PGK_short'

df_list <- list()
for (i in 1:nrow(MP_df)) {
  fl <- paste0(MP_df$Name[i], '.tune')
  obj <- readRDS(file.path('Tuning_Objects', fl))
  targ <- MP_df$Target[i]
  tune_val <- obj %>% filter(Name==tune_PM, Target==targ) %>% mutate(t=abs(Value-targ)) %>%
    filter(t==min(t)) %>% distinct(test_vals)

  df <- obj %>% filter(test_vals==tune_val$test_vals, Target==targ) %>% distinct(PM=Name, Value, Target, caption)
  df <- df %>% mutate(Value=ifelse(PM=='PGK_short', Target, Value))
  df$MP <- MP_df$MP[i]
  df$Name <- MP_df$Name[i]
  df_list[[i]] <- df

}

DF <- do.call('rbind', df_list)
DF$Target <- factor(DF$Target)

DF$Name %>% unique()

# ---- LRP Filtering ----
Trade_Off <- function(DF, PM1, PM2, xline=NULL, yline=NULL, inc.leg=TRUE,
                      pt.size=2, xmax=NULL, xmin=NULL, ymax=NULL, ymin=NULL, lab.MPs=NULL) {

  if (is.null(xmin)) xmin <- 0
  if (is.null(xmax)) xmax <- 1
  if (is.null(ymin)) ymin <- 0
  if (is.null(ymax)) ymax <- 1

  DF2 <- DF %>% filter(PM %in% c(PM1, PM2)) %>%
    select(!caption) %>%
    tidyr::pivot_wider(., names_from = PM, values_from = Value)

  DF2 <- DF2 %>% rename(., PM1=all_of(PM1), PM2=all_of(PM2))

  captions <- DF %>% filter(PM %in% c(PM1, PM2)) %>%
    distinct(PM, caption) %>%
    tidyr::pivot_wider(., names_from = PM, values_from = caption) %>%
    rename(., x=all_of(PM1), y=all_of(PM2))

  p <- ggplot(DF2, aes(x=PM1, y=PM2)) +
    geom_point(size=pt.size, aes(shape=Target, color=MP)) +
    expand_limits(x=c(xmin,1), y=c(ymin,1)) +
    labs(x=captions$x, y=captions$y, shape='PGK_short', color='PGK_short') +
    theme_bw()

  if (!is.null(xline))
    p <- p + geom_vline(xintercept = xline, linetype=2, color='darkgray')
  if (!is.null(yline))
    p <- p + geom_hline(yintercept = yline, linetype=2, color='darkgray')

  if (!inc.leg)
    p <- p + guides(shape='none')
  p <- p + guides(color='none')
  if (!is.null(ymax))
    p <- p + ylim(c(ymin, ymax))
  if (!is.null(xmax))
    p <- p + xlim(c(xmin, xmax))

  if (!is.null(lab.MPs)) {
    DF3 <- DF2 %>% filter(MP %in% lab.MPs)
    p <- p + ggrepel::geom_text_repel(data=DF3, aes(label=MP, color=MP), show.legend = FALSE,
                                      max.overlaps=20)
  }


  p
}

failLRP <- DF %>% filter(PM=='LRP', Value>=0.15)

Trade_Off(DF, 'PGK_short', 'nLRP', xline=c(0.51,0.6,0.7), yline=c(0.85, 0.9, 0.95),
          lab.MPs = failLRP$MP)

ggsave('img/July_2023/PGK_short_nLRP.png', width=8, height=7)

passDF <- DF %>% filter(!MP %in% failLRP$MP)

# ---- PGK Filtering ----
fail_PGK <- passDF %>%
  filter(PM %in% c('PGK_short', 'PGK_med', 'PGK_long', 'PGK'), Value<0.51)

fail_PGK

passDF2 <- passDF %>% filter(!MP %in% fail_PGK$MP)


# ---- Calculated Dominated MPs ----


DomPMs <- data.frame(PM=c('PGK_short', 'PGK_med', 'PGK_long',
                       'AvTAC_short', 'AvTAC_med', 'AvTAC_long',
                       'nLRP', 'VarC'),
                  Greater=TRUE)

DomPMs$Greater[DomPMs$PM=='VarC'] <- FALSE

PM_grid <- data.frame(PM1=c('PGK_short', 'PGK_med', 'PGK_long',
                            'nLRP', 'nLRP', 'nLRP',
                            'VarC', 'VarC', 'VarC'),
                      PM2=rep(c('AvTAC_short', 'AvTAC_med', 'AvTAC_long'), 3),
                      MP=NA)

PM_grid

is.dominated <- function(x, y, greater=TRUE, rnd=2) {
  x$Value <- round(x$Value,rnd)
  y$Value <- round(y$Value,rnd)
  if (greater)
    sdf <- x %>% filter(Value>=y$Value)
  if (!greater)
    sdf <- x %>% filter(Value<=y$Value)
  sdf$MP
}

Calc_Dominated <- function(pms, PM_results) {
  PM1 <- pms$PM1
  PM2 <- pms$PM2
  greater <- rep(TRUE, 2)
  t1 <- DomPMs %>% filter(PM==PM1)
  greater[1] <- t1$Greater
  t2 <- DomPMs %>% filter(PM==PM2)
  greater[2] <- t2$Greater

  PMs <- c(PM1, PM2)
  df <- PM_results %>% filter(PM %in% PMs)
  MPs <- unique(df$MP)
  dom_MP_list <- list()
  for (mm in seq_along(MPs)) {
    tdf <- df %>% filter(MP==MPs[mm])
    dom_MP_list[[MPs[mm]]] <- list()
    for (pm in 1:2) {
      x <- df %>% filter(PM==PMs[pm])
      y <- tdf %>% filter(PM==PMs[pm])
      dom_MP <- is.dominated(x,y, greater=greater[pm])

      dom_MP_list[[MPs[mm]]][[PMs[[pm]]]] <- dom_MP[!dom_MP==MPs[mm]]
    }
    dom_MP_list[[MPs[mm]]] <- intersect(dom_MP_list[[MPs[mm]]][[1]], dom_MP_list[[MPs[mm]]][[2]] )
  }
  names(which(lapply(dom_MP_list, length)==0))
}

for (i in 1:nrow(PM_grid)) {
  PM_grid$MP[i] <- list(Calc_Dominated( PM_grid[i,], passDF2))
}

# Non-Dominated MPs
nondomMPs <- unlist(PM_grid$MP) %>% unique() %>% sort()

passDF2 %>% filter(MP %in% c('CE_c')) %>% mutate(Value=round(Value,2)) %>% select(PM, Value)
passDF2 %>% filter(MP %in% c('SPFox_a')) %>% mutate(Value=round(Value,2)) %>% select(PM, Value)

nonDomDF <- passDF2 %>% filter(MP %in% nondomMPs)


# ---- Trade-offs  ----

nonDomDF <- passDF2 %>% filter(MP %in% nondomMPs)

p1 <- Trade_Off(nonDomDF, 'nLRP', 'TAC1', inc.leg = FALSE,
                xline=c(0.85, 0.9, 0.95),
                xmin=0.85,
                ymax=12500)
p2 <- Trade_Off(nonDomDF, 'PGK_short', 'AvTAC_short', inc.leg = FALSE, xline=c(0.51,0.6,0.7),
                ymax=12500)

p3 <- Trade_Off(nonDomDF, 'PGK_med', 'AvTAC_med', inc.leg = FALSE, xline=c(0.51,0.6,0.7),
                ymax=12500)

p4 <- Trade_Off(nonDomDF, 'VarC', 'AvTAC_med', inc.leg = FALSE,
                xmax=0.25,
                xline=0.25,
                ymax=12500)


cowplot::plot_grid(p1, p2, p3, p4, ncol=2)
ggsave('img/July_2023/TradeOff1.png', width=8, height=7)


p1 <- Trade_Off(nonDomDF, 'nLRP', 'TAC1', inc.leg = FALSE,
                ymin=10000,
                xmin=0.95,
                ymax=12500,
                lab.MPs = nonDomDF$MP)

p2 <- Trade_Off(nonDomDF, 'PGK_short', 'AvTAC_short',
                inc.leg = FALSE, xline=c(0.51,0.6,0.7),
                ymin=10000,
                xmin=0.5,
                ymax=12500,
                lab.MPs = nonDomDF$MP)

p3 <- Trade_Off(nonDomDF, 'PGK_med', 'AvTAC_med',
                inc.leg = FALSE, xline=c(0.51,0.6,0.7),
                ymin=10000,
                xmin=0.5,
                ymax=12500,
                lab.MPs = nonDomDF$MP)

p4 <- Trade_Off(nonDomDF, 'VarC', 'AvTAC_med', inc.leg = FALSE,
                xmax=0.25,
                xline=0.25,
                ymin=10000,
                ymax=12500,
                lab.MPs = nonDomDF$MP)


cowplot::plot_grid(p1, p2, p3, p4, ncol=2)
ggsave('img/July_2023/TradeOff2.png', width=8, height=7)



# ---- Time-Series Plots -----

get_MSE_list <- function(MP) {
  Targ <- switch(strsplit(MP, '_')[[1]][2],  a=0.51, b=0.6, c=0.7)

  nm <- paste(strsplit(MP, '_')[[1]][1], 'PGK_short', Targ, sep="_")
  nms <- paste(nm, 1:5, sep='_')
  nms <- paste0(nms, '.mmse')

  for (i in seq_along(nms)) {
    if (file.exists(file.path('Tuning_Objects/tune_MMSE', nms[i]))) {
      mmse_list <- readRDS(file.path('Tuning_Objects/tune_MMSE', nms[i]))
    }
  }
  # add multHist objects
  for (om in 1:9) {
    hist <- paste0('MOM_00', om, '.hist')
    mmse_list[[om]]@multiHist <- readRDS(file.path('Hist_Objects', hist))
  }

  mmse_list
}

make_DF <- function(MP) {
  Targ <- switch(strsplit(MP, '_')[[1]][2],  a=0.51, b=0.6, c=0.7)

  nm <- paste(strsplit(MP, '_')[[1]][1], 'PGK_short', Targ, sep="_")
  nms <- paste(nm, 1:5, sep='_')
  nms <- paste0(nms, '.mmse')

  for (i in seq_along(nms)) {
    if (file.exists(file.path('Tuning_Objects/tune_MMSE', nms[i]))) {
      mmse_list <- readRDS(file.path('Tuning_Objects/tune_MMSE', nms[i]))
    }
    # add multHist objects
    # for (om in 1:9) {
    #   hist <- paste0('MOM_00', om, '.hist')
    #   mmse_list[[i]]@multiHist <- readRDS(file.path('Hist_Objects', hist))
    # }

  }

  MMSE <- combine_MMSE(mmse_list, 'name')
  mm <- which.min(abs(PGK_short(MMSE)@Mean -Targ))
  nsim <- MMSE@nsim
  nyears <- MMSE@proyears
  df <- data.frame(Sim=1:nsim,
                   Year=rep(2021:2053, each=nsim),
                   SB_SBMSY=as.vector(MMSE@SB_SBMSY[,1,mm,]),
                   F_FMSY=as.vector(MMSE@F_FMSY[,1,1,mm,]),
                   TAC=as.vector(apply(MMSE@TAC[,,1,mm,], c(1,3), sum)),
                   Landings=as.vector(apply(MMSE@Catch[,,1,mm,], c(1,3), sum)),
                   MP=MP)
  df$OM <- 1
  df$OM[df$Sim %in% 51:100] <- 2
  df$OM[df$Sim %in% 101:150] <- 3
  df$OM[df$Sim %in% 151:200] <- 4
  df$OM[df$Sim %in% 201:250] <- 5
  df$OM[df$Sim %in% 251:300] <- 6
  df$OM[df$Sim %in% 301:350] <- 7
  df$OM[df$Sim %in% 351:400] <- 8
  df$OM[df$Sim %in% 401:450] <- 9
  df
}

keep_MPs <- nonDomDF$MP %>% unique()

df_list <- list()
for (i in seq_along(keep_MPs)) {
  message(i)
  df_list[[i]] <- make_DF(keep_MPs[i])
}

DF <- do.call('rbind', df_list) %>%
  filter(Year>=2024) %>%
  tidyr::pivot_longer(., cols=c(SB_SBMSY, F_FMSY, TAC, Landings))


alpha <- 0.7
fill1 <- 'darkgrey'
fill2 <- 'lightgrey'

TimeSeries_plot <- function(DF, Var='SB_SBMSY', yline=NULL, byOM=FALSE) {

  if (byOM) {
    pDF <- DF %>% group_by(Year, MP, name, OM) %>%
      mutate(Median=median(value),
             Lower1=quantile(value, 0.1),
             Lower2=quantile(value, 0.25),
             Upper1=quantile(value, 0.9),
             Upper2=quantile(value, 0.75))
  } else {
    pDF <- DF %>% group_by(Year, MP, name) %>%
      mutate(Median=median(value),
             Lower1=quantile(value, 0.1),
             Lower2=quantile(value, 0.25),
             Upper1=quantile(value, 0.9),
             Upper2=quantile(value, 0.75))
  }


  p <- ggplot(pDF %>% filter(name==Var), aes(x=Year))

  if (byOM) {
    p <- p + facet_grid(OM~MP)
  } else {
    p <- p + facet_grid(~MP)
  }
  p <- p +
    geom_ribbon(aes(ymin=Lower1 , ymax=Upper1), fill=fill1, alpha=alpha) +
    geom_ribbon(aes(ymin=Lower2 , ymax=Upper2), fill=fill2, alpha=alpha) +
    geom_line(aes(y=Median)) +
    expand_limits(y=0) +
    theme_bw() +
    geom_hline(yintercept=yline, linetype=2) +
    labs(y=Var) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
  p
}

TimeSeries_plot(DF, 'SB_SBMSY', c(1, 0.4))
ggsave('img/July_2023/TS_SB_SBMSY.png', width=13, height=3)

TimeSeries_plot(DF, 'SB_SBMSY', c(1, 0.4), byOM=TRUE)
ggsave('img/July_2023/TS_SB_SBMSY-OM.png', width=13, height=9)

TimeSeries_plot(DF, 'F_FMSY', 1)
ggsave('img/July_2023/TS_F_FMSY.png', width=13, height=3)

meanC <- mean(tail(SWOData@Cat[1,], 5))
TimeSeries_plot(DF, 'TAC', meanC)
ggsave('img/July_2023/TS_TAC.png', width=13, height=3)

TimeSeries_plot(DF, 'TAC', meanC, byOM=TRUE)















# ---- Trade-offs between PGK and TAC ----
nonDomDF <- passDF2 %>% filter(MP %in% nondomMPs)

p1 <- Trade_Off(nonDomDF, 'PGK_short', 'AvTAC_short', inc.leg = FALSE, xline=c(0.51,0.6,0.7),
                ymax=12500)

p2 <- Trade_Off(nonDomDF, 'PGK_med', 'AvTAC_med', inc.leg = FALSE, xline=c(0.51,0.6,0.7),
                ymax=12500)

p3 <- Trade_Off(nonDomDF, 'PGK_long', 'AvTAC_long', inc.leg = FALSE, xline=c(0.51,0.6,0.7),
                ymax=12500)


cowplot::plot_grid(p1, p2, p3, ncol=3)
ggsave('img/July_2023/PGK_vs_TAC.png', width=12, height=4)


passDF2 <- passDF %>% filter(!MP %in%c(fail_med$MP, fail_long$MP))




# ---- Trade-offs between PGK and TAC ----

passDF %>% filter(PM=='PGK_short', Value<0.51)

p1 <- Trade_Off(passDF, 'PGK_short', 'AvTAC_short', inc.leg = FALSE, xline=c(0.51,0.6,0.7),
                ymax=12500)

fail_med <- passDF %>% filter(PM=='PGK_med', Value<0.51)
p2 <- Trade_Off(passDF, 'PGK_med', 'AvTAC_med', inc.leg = FALSE, xline=c(0.51,0.6,0.7),
                ymax=12500, lab.MPs = fail_med$MP)

fail_long <- passDF %>% filter(PM=='PGK_long', Value<0.51)
p3 <- Trade_Off(passDF, 'PGK_long', 'AvTAC_long', inc.leg = FALSE, xline=c(0.51,0.6,0.7),
                ymax=12500, lab.MPs = fail_long$MP)


cowplot::plot_grid(p1, p2, p3, ncol=3)
ggsave('img/July_2023/PGK_vs_TAC.png', width=12, height=4)


passDF2 <- passDF %>% filter(!MP %in%c(fail_med$MP, fail_long$MP))


# ---- Calculate Dominated re PGK and TAC ----


nondom_short <- Calc_Dominated('PGK_short', 'AvTAC_short', passDF2)
p1 <- Trade_Off(passDF2 %>% filter(MP %in% nondom_short), 'PGK_short', 'AvTAC_short', inc.leg = FALSE, xline=c(0.51,0.6,0.7),
                ymax=12500, lab.MPs = nondom_short)
p1

nondom_med <- Calc_Dominated('PGK_med', 'AvTAC_med', passDF2)
p2 <- Trade_Off(passDF2 %>% filter(MP %in% nondom_med), 'PGK_med', 'AvTAC_med', inc.leg = FALSE, xline=c(0.51,0.6,0.7),
                ymax=12500, lab.MPs = nondom_med)
p2

nondom_long <- Calc_Dominated('PGK_long', 'AvTAC_long', passDF2)
p3 <- Trade_Off(passDF2 %>% filter(MP %in% nondom_long), 'PGK_long', 'AvTAC_long', inc.leg = FALSE, xline=c(0.51,0.6,0.7),
                ymax=12500, lab.MPs = nondom_long)
p3

cowplot::plot_grid(p1, p2, p3, ncol=3)
ggsave('img/July_2023/dom_PGK_vs_TAC.png',  width=12, height=4)



keep_MPs <- c(nondom_short, nondom_med, nondom_long) %>% unique() %>% sort()
keep_MPs

# ---- Calculate Dominated re VarC and TAC ----
passDF3 <- passDF2 %>% filter(MP %in% keep_MPs)


p1 <- Trade_Off(passDF3, 'VarC', 'AvTAC_short', inc.leg = FALSE,
                xline=c(0.25), xmax=0.25,
                ymax=12500, lab.MPs = passDF3$MP)

p2 <- Trade_Off(passDF3, 'VarC', 'AvTAC_med', inc.leg = FALSE,
                xline=0.25, xmax=0.25,
                ymax=12500, lab.MPs = passDF3$MP)

p3 <- Trade_Off(passDF3 , 'VarC', 'AvTAC_long', inc.leg = FALSE,
                xline=0.25, xmax=0.25,
                ymax=12500, lab.MPs = passDF3$MP)

cowplot::plot_grid(p1, p2, p3, ncol=3)
ggsave('img/July_2023/VarC_vs_TAC.png', width=12, height=4)



nondom_short <- Calc_Dominated('VarC', 'AvTAC_short', passDF3, greater=c(FALSE, TRUE))
p1 <- Trade_Off(passDF3, 'VarC', 'AvTAC_short', inc.leg = FALSE,
                xline=c(0.25), xmax=0.25,
                ymax=12500, lab.MPs = nondom_short)


nondom_med <- Calc_Dominated('VarC', 'AvTAC_med', passDF3, greater=c(FALSE, TRUE))
p2 <- Trade_Off(passDF3, 'VarC', 'AvTAC_med', inc.leg = FALSE,
                xline=0.25, xmax=0.25,
                ymax=12500, lab.MPs = nondom_med)

nondom_long <- Calc_Dominated('VarC', 'AvTAC_long', passDF3, greater=c(FALSE, TRUE))
p3 <- Trade_Off(passDF3 , 'VarC', 'AvTAC_long', inc.leg = FALSE,
                xline=0.25, xmax=0.25,
                ymax=12500, lab.MPs = nondom_long)


cowplot::plot_grid(p1, p2, p3, ncol=3)
ggsave('img/July_2023/VarC_vs_TAC_dom.png', width=12, height=4)

keep_MPs2 <- c(nondom_short, nondom_med, nondom_long) %>% unique() %>% sort()

keep_MPs2

passDF4 <- passDF3 %>% filter(MP %in%keep_MPs2)

p1 <- Trade_Off(passDF4, 'PGK_short', 'TAC1', inc.leg = FALSE,
                xline=c(0.51, 0.6, 0.7),
                ymax=12500, lab.MPs = passDF4$MP)

p2 <- Trade_Off(passDF4, 'PGK_short', 'AvTAC_short', inc.leg = FALSE,
                xline=c(0.51, 0.6, 0.7),
                ymax=12500, lab.MPs = passDF4$MP)

p3 <- Trade_Off(passDF4, 'PGK_med', 'AvTAC_med', inc.leg = FALSE,
                xline=c(0.51, 0.6, 0.7),
                ymax=12500, lab.MPs = passDF4$MP)

p4 <- Trade_Off(passDF4, 'VarC', 'AvTAC_med', inc.leg = FALSE,
                xline=c(0.25), xmax=0.25,
                ymax=12500, lab.MPs = passDF4$MP)

cowplot::plot_grid(p1, p2, p3, p4, ncol=2)

ggsave('img/July_2023/Trade_Offs.png', width=8, height=6)

passDF4 %>% filter(PM=='PGK_long')
passDF4 %>% filter(PM=='TAC1')
passDF4 %>% filter(PM=='AvTAC_med')
passDF4 %>% filter(PM=='AvTAC_long')


# ---- Time-Series Plots -----

get_MSE_list <- function(MP) {
  Targ <- switch(strsplit(MP, '_')[[1]][2],  a=0.51, b=0.6, c=0.7)

  nm <- paste(strsplit(MP, '_')[[1]][1], 'PGK_short', Targ, sep="_")
  nms <- paste(nm, 1:5, sep='_')
  nms <- paste0(nms, '.mmse')

  for (i in seq_along(nms)) {
    if (file.exists(file.path('Tuning_Objects/tune_MMSE', nms[i]))) {
      mmse_list <- readRDS(file.path('Tuning_Objects/tune_MMSE', nms[i]))
    }
  }
  # add multHist objects
  for (om in 1:9) {
    hist <- paste0('MOM_00', om, '.hist')
    mmse_list[[om]]@multiHist <- readRDS(file.path('Hist_Objects', hist))
  }

  mmse_list
}

make_DF <- function(MP) {
  Targ <- switch(strsplit(MP, '_')[[1]][2],  a=0.51, b=0.6, c=0.7)

  nm <- paste(strsplit(MP, '_')[[1]][1], 'PGK_short', Targ, sep="_")
  nms <- paste(nm, 1:5, sep='_')
  nms <- paste0(nms, '.mmse')

  for (i in seq_along(nms)) {
    if (file.exists(file.path('Tuning_Objects/tune_MMSE', nms[i]))) {
      mmse_list <- readRDS(file.path('Tuning_Objects/tune_MMSE', nms[i]))
    }
    # add multHist objects
    # for (om in 1:9) {
    #   hist <- paste0('MOM_00', om, '.hist')
    #   mmse_list[[i]]@multiHist <- readRDS(file.path('Hist_Objects', hist))
    # }

  }

  MMSE <- combine_MMSE(mmse_list, 'name')
  mm <- which.min(abs(PGK_short(MMSE)@Mean -Targ))
  nsim <- MMSE@nsim
  nyears <- MMSE@proyears
  df <- data.frame(Sim=1:nsim,
                   Year=rep(2021:2053, each=nsim),
                   SB_SBMSY=as.vector(MMSE@SB_SBMSY[,1,mm,]),
                   F_FMSY=as.vector(MMSE@F_FMSY[,1,1,mm,]),
                   TAC=as.vector(apply(MMSE@TAC[,,1,mm,], c(1,3), sum)),
                   Landings=as.vector(apply(MMSE@Catch[,,1,mm,], c(1,3), sum)),
                   MP=MP)
  df$OM <- 1
  df$OM[df$Sim %in% 51:100] <- 2
  df$OM[df$Sim %in% 101:150] <- 3
  df$OM[df$Sim %in% 151:200] <- 4
  df$OM[df$Sim %in% 201:250] <- 5
  df$OM[df$Sim %in% 251:300] <- 6
  df$OM[df$Sim %in% 301:350] <- 7
  df$OM[df$Sim %in% 351:400] <- 8
  df$OM[df$Sim %in% 401:450] <- 9
  df
}

df_list <- list()
for (i in seq_along(keep_MPs2)) {
  message(i)
  df_list[[i]] <- make_DF(keep_MPs2[i])
}

DF <- do.call('rbind', df_list) %>%
  filter(Year>=2024) %>%
  tidyr::pivot_longer(., cols=c(SB_SBMSY, F_FMSY, TAC, Landings))


alpha <- 0.7
fill1 <- 'darkgrey'
fill2 <- 'lightgrey'

TimeSeries_plot <- function(DF, Var='SB_SBMSY', yline=NULL, byOM=FALSE) {

  if (byOM) {
    pDF <- DF %>% group_by(Year, MP, name, OM) %>%
      mutate(Median=median(value),
             Lower1=quantile(value, 0.1),
             Lower2=quantile(value, 0.25),
             Upper1=quantile(value, 0.9),
             Upper2=quantile(value, 0.75))
  } else {
    pDF <- DF %>% group_by(Year, MP, name) %>%
      mutate(Median=median(value),
             Lower1=quantile(value, 0.1),
             Lower2=quantile(value, 0.25),
             Upper1=quantile(value, 0.9),
             Upper2=quantile(value, 0.75))
  }


  p <- ggplot(pDF %>% filter(name==Var), aes(x=Year))

  if (byOM) {
    p <- p + facet_grid(OM~MP)
  } else {
    p <- p + facet_grid(~MP)
  }
  p <- p +
    geom_ribbon(aes(ymin=Lower1 , ymax=Upper1), fill=fill1, alpha=alpha) +
    geom_ribbon(aes(ymin=Lower2 , ymax=Upper2), fill=fill2, alpha=alpha) +
    geom_line(aes(y=Median)) +
    expand_limits(y=0) +
    theme_bw() +
    geom_hline(yintercept=yline, linetype=2) +
    labs(y='Median \n(10th, 90th, and 25th, 75th percentiles)') +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
  p
}

TimeSeries_plot(DF, 'SB_SBMSY', c(1, 0.4))
ggsave('img/July_2023/TS_SB_SBMSY.png', width=12, height=4)

TimeSeries_plot(DF, 'SB_SBMSY', c(1, 0.4), byOM=TRUE)
ggsave('img/July_2023/TS_SB_SBMSY-OM.png', width=9, height=9)

TimeSeries_plot(DF, 'F_FMSY', 1)
ggsave('img/July_2023/TS_F_FMSY.png', width=12, height=4)

meanC <- mean(tail(SWOData@Cat[1,], 5))
TimeSeries_plot(DF, 'TAC', meanC)
ggsave('img/July_2023/TS_TAC.png', width=12, height=4)

TimeSeries_plot(DF, 'TAC', meanC, byOM=TRUE)



## Why does F continue to decline for SPFox_a?

MSE_list <- get_MSE_list('SPFox_a')


om <- 5
All_Years <- get_Years(MSE_list[[om]])
All_Data <- MSE_list[[om]]@PPD[[1]][[1]][[2]]
Imp_Years <- seq(2024, by=3, to=2053)

x <- which.max(MSE_list[[om]]@SB_SBMSY[,1,2,33])

df_list <- list()
c_proj <- c(9729.000, 10770.000, 10770.000)
for (y in seq_along(Imp_Years)) {
  year <- Imp_Years[y]
  Data <- Trim_Data(All_Data, year-1)

#   Mod <- SPFox_a(x, Data)
  Mod <- SPFox3(x, Data)


  B_est <- Mod@Misc[[x]]@B
  Years <- as.numeric(names(B_est))


  TAC <-  Mod@TAC

  c_hist <- rowSums(MSE_list[[om]]@multiHist[[1]][[1]]@TSdata$Landings[x,,]) +
    rowSums(MSE_list[[om]]@multiHist[[2]][[1]]@TSdata$Landings[x,,])
  if (y>1) {
    c_proj <- c(c_proj, rep(TAC,3))
  } else {
    c_proj <- c(c_proj, TAC)
  }

  yy <- All_Years %>%  filter(Year<=(year))

  #   yy <- All_Years %>%  filter(Year<=(year+2))
  Catch_df <- data.frame(Year=yy$Year, TAC=c(c_hist, c_proj), Imp=year)

  df <- data.frame(Year=Years, B_est=B_est, Bthresh= Bthresh,  Imp=year,
                   B_BMSY_est=B_est/Bthresh,
                   F=Fmort)
  df <- left_join(Catch_df, df)



  df2 <- df %>% tidyr::pivot_longer(., cols=c(B_est, B_BMSY_est, TAC))
  df_list[[y]] <- df2

}

df <- do.call('rbind', df_list)
df$Imp <- factor(df$Imp)

ggplot(df) +
  facet_wrap(~name, scales='free_y') +
  expand_limits(y=0) +
  geom_line(aes(x=Year, y=value, color=Imp)) +
  theme_bw() +
  labs(color='TAC Year')

ggsave('img/July_2023/retrospective.png', width=12, height=4)







MSE_list <- get_MSE_list('SPFox2_b')


om <- 5
All_Years <- get_Years(MSE_list[[om]])
All_Data <- MSE_list[[om]]@PPD[[1]][[1]][[2]]
Imp_Years <- seq(2024, by=3, to=2053)

x <- which.max(MSE_list[[om]]@SB_SBMSY[,1,2,33])

df_list <- list()
for (y in seq_along(Imp_Years)) {
  year <- Imp_Years[y]
  Data <- Trim_Data(All_Data, year-1)

  Mod <- SPFox2_b(x, Data)

  B_est <- Mod@Misc[[x]]@B
  Years <- as.numeric(names(B_est))
  tunepar <- formals('SPFox_a')$tunepar
  Ftar <- tunepar * 0.1 #  Mod@FMSY
  Fmin <- 0.1 * Ftar #  * Mod@FMSY
  Bcurr <- B_est[length(B_est)]
  Bthresh=Mod@Misc[[x]]@BMSY
  if (Bcurr>=Bthresh) {
    Fmort <- Ftar
  } else if (Bcurr>Blim) {
    Fmort <- Mod@Misc[[x]]@FMSY * (-0.367 + 1.167*  Bcurr/Bthresh)
  } else {
    Fmort <- Fmin
  }

  c_hist <- rowSums(MSE_list[[om]]@multiHist[[1]][[1]]@TSdata$Landings[x,,]) +
    rowSums(MSE_list[[om]]@multiHist[[2]][[1]]@TSdata$Landings[x,,])
  c_proj <- colSums(MSE_list[[om]]@TAC[x,,1,1,])
  Catch_df <- data.frame(Year=All_Years$Year, TAC=c(c_hist, c_proj), Imp=year) %>%
    filter(Year<=year)

  df <- data.frame(Year=Years, B_est=B_est, Bthresh= Bthresh,  Imp=year,
                   B_BMSY_est=B_est/Bthresh,
                   F=Fmort)
  df <- left_join(Catch_df, df)



  df2 <- df %>% tidyr::pivot_longer(., cols=c(B_est, B_BMSY_est, TAC))
  df_list[[y]] <- df2

}

df <- do.call('rbind', df_list)
df$Imp <- factor(df$Imp)

ggplot(df) +
  facet_wrap(~name, scales='free_y') +
  expand_limits(y=0) +
  geom_line(aes(x=Year, y=value, color=Imp)) +
  theme_bw() +
  labs(color='TAC Year')

ggsave('img/July_2023/retrospective.png', width=12, height=4)



SWOData@steep
SWOData@Mort
SWOData@vbLinf <- 100
SWOData@vbK <- 0.1
SWOData@vbt0 <- 0
SWOData@wla <- 1E-5
SWOData@wlb <- 3
res_prior <- SP(Data = SWOData, Euler_Lotka = 100)




























Step_MP(MMSE, 2)

MMSE@F_FMSY[,1,1,mm,]
MMSE@TAC[,1,1,mm,]
MMSE@Catch[,1,1,mm,]


# Document and save tuned MPs
Tune_dir='Tuning_Objects'

fls <- list.files(Tune_dir, pattern='.tune')

MPs <- strsplit(fls, '.tune') %>% unlist()


get_location <- function(MP_name) {
  attrs <- attributes(body(MP_name))
  attrs$srcfile$filename
}

get_MP_locations <- function(MPs) {
  R_files <- list.files('R')
  for (fl in R_files)
    source(file.path('R', fl))
  sapply(1:length(MPs), function(x)
    get_location(MPs[x])
  )
}

MP_files <- get_MP_locations(MPs)

for (i in seq_along(MPs)) {
  Document_MP(MP_name=MPs[i], MP_file=MP_files[i], plot=TRUE)
}


# Run MSE
for (fl in CMP_files) source(file.path('CMPs', fl))
Hist.dir <- 'Hist_Objects'
MSE.dir <- 'MSE_Objects/optCMPs'
runMPs <- c('CE', 'CE25', 'CE2', 'CE2cr',
            'SPFox', 'SPFox25', 'SPFox2')

runMPs <- c(paste0(runMPs, '_a'), paste0(runMPs, '_b'), paste0(runMPs, '_c'))

for (i in 1:nrow(Ref_OMs)) {
  message(i, '/', nrow(Ref_OMs))
  OM_object <- Ref_OMs$OM.object[i]
  multiHist <- readRDS(file.path(Hist.dir, paste0(OM_object, '.hist')))

  MMSE <- ProjectMOM(multiHist, MPs=runMPs, silent=FALSE, dropHist=FALSE, checkMPs = FALSE)
  saveRDS(MMSE, file.path(MSE.dir, paste0(OM_object, '.mse')))
}


