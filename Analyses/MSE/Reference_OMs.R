library(SWOMSE)

out.dir <- 'Analyses/MSE'

MPs <- avail('MP', 'SWOMSE')

Ref_OMs <- OM_DF %>% filter(Class=='Reference')

# Load hist objects
histlist <- list()
for (i in seq_along(Ref_OMs$OM.object)) {
  histlist[[i]] <- readRDS(paste0('dev/', Ref_OMs$OM.object[i], '.mhist'))
}


# MSE
rerun <- FALSE
if (rerun) {
  mselist <- list()
  for (i in seq_along(Ref_OMs$OM.object)) {
    mse <- ProjectMOM(histlist[[i]], MPs=MPs)
    nm <- Ref_OMs$OM.object[i]
    saveRDS(mse, paste0('Analyses/MSE/', nm, '.mmse'))
    mselist[[i]] <- mse
  }
  saveRDS(mselist, 'Analyses/MSE/MSElist.mmse')
} else {
  mselist <- readRDS('Analyses/MSE/MSElist.mmse')
}

MSE_all <- combine_MMSE(mselist, 'Reference OMs')

PM_vals <- PM_table(MSE_all)

PMs <- c('PGK_med', 'AvTAC30')

p <- TradeOff(PM_vals, c('PGK_med', 'PGK_long'), vline=0.6, hline=0.6)
ggsave(file.path(out.dir, 'TradeOff1.png'), p, width=6, height=6)

p <- TradeOff(PM_vals, c('PGK_med', 'PGK_long'), vline=0.6, hline=0.6,
              ylim=0.51, xlim = 0.51)
ggsave(file.path(out.dir, 'TradeOff1a.png'), p, width=6, height=6)


p <- TradeOff(PM_vals, c('PGK_med', 'PGK_30'), vline=0.6, hline=0.6,
              ylim=0.51, xlim = 0.51)
p
ggsave(file.path(out.dir, 'TradeOff2.png'), p, width=6, height=6)

p <- TradeOff(PM_vals, c('PGK_med', 'nLRP_long'), xlim=0.51, ylim=0.85)
ggsave(file.path(out.dir, 'TradeOff2a.png'), p, width=6, height=6)


p <- TradeOff(PM_vals, c('AvTAC10', 'AvTAC30'))
ggsave(file.path(out.dir, 'TradeOff3.png'), p, width=6, height=6)

PM_vals2 <- PM_vals %>% filter(MP %in% c('IR1_a', 'IR2_a', 'CE_a', 'SP1_a', 'SP2_a'))
p <- TradeOff(PM_vals2, c('AvTAC10', 'AvTAC30'))
ggsave(file.path(out.dir, 'TradeOff4.png'), p, width=6, height=6)


PM_vals2 <- PM_vals %>% filter(MP %in% c('IR1_a', 'IR2_a', 'CE_a', 'SP1_a', 'SP2_a'))
p <- TradeOff(PM_vals2, c('VarC', 'AvTAC30'))
ggsave(file.path(out.dir, 'TradeOff5.png'), p, width=6, height=6)

PM_vals2 %>% filter(Name=='VarC')

tab <- PM_vals %>% filter(Name %in% c('PGK_med', 'PGK_long', 'PGK_30',
                               'nLRP_long', 'AvTAC10', 'AvTAC30')) %>%
  select(Name, MP, Value) %>%
  mutate(Value=round(Value,2)) %>%
  tidyr::pivot_wider(., names_from = Name, values_from=Value)

library(DT)
DT::datatable(tab)

DT::datatable(tab) %>% formatStyle(
  'PGK_med',
  backgroundColor = styleInterval(c(0.51), c('red', 'green')))%>%
  formatStyle(
  'PGK_long',
  backgroundColor = styleInterval(c(0.51), c('red', 'green'))) %>%
  formatStyle(
    'PGK_30',
    backgroundColor = styleInterval(c(0.51), c('red', 'green'))) %>%
  formatStyle(
    'nLRP_long',
    backgroundColor = styleInterval(c(0.85), c('red', 'green')))


p <- Catch_TS(MSE_all, mp=c('SP1_a', 'SP2_a', 'CE_a'))
ggsave(file.path(out.dir, 'Catch1.png'), p, width=8, height=3)

p <- SB_SBMSY_TS(MSE_all, fill='none', mp=c('SP1_a', 'SP2_a', 'CE_a'))
ggsave(file.path(out.dir, 'SB_SBMSY1.png'), p, width=8, height=3)

p <- F_FMSY_TS(MSE_all, fill='none', mp=c('SP1_a', 'SP2_a', 'CE_a'))
ggsave(file.path(out.dir, 'F_FMSY1.png'), p, width=8, height=3)


p <- SB_SBMSY_Box(MSE_all, mp=c('SP1_a', 'SP2_a', 'CE_a'))
ggsave(file.path(out.dir, 'SB_SBMSY_box.png'), p, width=8, height=3)

p <- F_FMSY_Box(MSE_all, mp=c('SP1_a', 'SP2_a', 'CE_a'))
ggsave(file.path(out.dir, 'F_FMSY_box.png'), p, width=8, height=3)




for (i in seq_along(Ref_OMs$OM.object)) {
  OM <- Ref_OMs$OM.object[i]
  p <- SB_SBMSY_TS(mselist[[i]], fill='none', mp=c('SP2_a'))
  nm <- paste0(OM, '_SB_SBMSY1.png')
  ggsave(file.path(out.dir, nm), p, width=4, height=3)
}

for (i in seq_along(Ref_OMs$OM.object)) {
  OM <- Ref_OMs$OM.object[i]
  p <- SB_SBMSY_TS(mselist[[i]], fill='none', mp=c('CE_a'))
  nm <- paste0(OM, '_SB_SBMSY2.png')
  ggsave(file.path(out.dir, nm), p, width=4, height=3)
}

p <- SB_SBMSY_Box(MSE_all, mp=c('SP1_a', 'SP2_a', 'CE_a'))
ggsave(file.path(out.dir, 'SB_SBMSY_box.png'), p, width=8, height=4)

p <- SB_SBMSY_Box(mselist[[i]], mp=c('SP1_a', 'SP2_a', 'CE_a'))
ggsave(file.path(out.dir, 'SB_SBMSY_box.png'), p, width=8, height=4)


DFlist <- list()
for (i in seq_along(Ref_OMs$OM.object)) {
  OM <- Ref_OMs$OM.object[i]
  DF <- MakePerformanceDF(mselist[[i]])
  DF$M <- Ref_OMs$M[i]
  DF$steepness <- Ref_OMs$steepness[i]
  DFlist[[i]] <- DF
}

DF <- do.call('rbind', DFlist) %>% filter(MP %in% 'SP2_a')

p <- ggplot(DF, aes(x=Year, color=Sim, y=Catch)) +
  facet_grid(steepness~M) +
  expand_limits(y=c(0,2)) +
  geom_line() +
  theme_bw() +
  scale_x_date(date_labels="%Y", breaks=breaks.vec)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(color='none') +
  labs(y='Landed Catch (t)')

nm <- 'RefOM_catch.png'
ggsave(file.path(out.dir, nm), p, width=8, height=5)


p <- ggplot(DF, aes(x=Year, color=Sim, y=SB_SBMSY)) +
  facet_grid(steepness~M) +
  expand_limits(y=c(0,2)) +
  geom_line() +
  theme_bw() +
  scale_x_date(date_labels="%Y", breaks=breaks.vec)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(color='none') +
  labs(y=expression(SB/SB[MSY]))

nm <- 'RefOM_sb_sbmsy.png'
ggsave(file.path(out.dir, nm), p, width=8, height=5)

DF$F_FMSY[DF$F_FMSY>2] <- 2
p <- ggplot(DF, aes(x=Year, color=Sim, y=F_FMSY)) +
  facet_grid(steepness~M) +
  expand_limits(y=c(0,2)) +
  geom_line() +
  theme_bw() +
  scale_x_date(date_labels="%Y", breaks=breaks.vec)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(color='none') +
  labs(y=expression(F/F[MSY]))

nm <- 'RefOM_f_fmsy.png'
ggsave(file.path(out.dir, nm), p, width=8, height=5)







## Why is h=0.9 more risky than h=0.75?

## track CE_a in action for one simulation for 2 OMs
library(MSEgraph)
i <- 5

MMSE <- mselist[[5]]
MMSE@multiHist <- histlist[[5]]
SB_SBMSY <- MMSE@SB_SBMSY[,1,1,]
ind <- which(SB_SBMSY[,10]<0.4)[1]
df1 <- make_plot(MMSE,14,Ref_OMs$OM.object[[5]])



MMSE <- mselist[[2]]
MMSE@multiHist <- histlist[[2]]
df2 <- make_plot(MMSE, 14, Ref_OMs$OM.object[[2]])

df1
df2

Ref_OMs$OM.object[c(2,5)]

PGK(mselist[[2]])@Mean[1]
PGK(mselist[[5]])@Mean[1]

LRP(mselist[[2]])@Mean[1]
LRP(mselist[[5]])@Mean[1]


make_plot <- function(MMSE, ind, mom) {

  Years <- get_Years(MMSE)
  Data <- MMSE@PPD[[1]][[1]][[1]]
  TAC <- apply(MMSE@TAC[,,1,1,], c(1,3),sum)

  B_hist <- rowSums(MMSE@multiHist[[1]][[1]]@TSdata$Biomass[ind,,]) +
    rowSums(MMSE@multiHist[[2]][[1]]@TSdata$Biomass[ind,,])
  B_proj <- colSums(MMSE@B[ind,,1,])

  C_hist <- rowSums(MMSE@multiHist[[1]][[1]]@TSdata$Landings[ind,,]) +
    rowSums(MMSE@multiHist[[2]][[1]]@TSdata$Landings[ind,,])
  C_proj <- colSums(MMSE@Catch[ind,,1,1,])

  B_all <- data.frame(Year=Years$Year, Period=Years$Period, B=c(B_hist, B_proj),
                      C=c(C_hist, C_proj))
  B_all$Index <- c(Data@Ind[ind,], NA)
  B_all$Catch <- c(Data@Cat[ind,], NA)

  B_all <- B_all %>% mutate(include=!is.na(Index))
  b_mean <- B_all %>% filter(include==TRUE, Period=='Historical') %>% summarise(mean=mean(B))
  B_all <- B_all %>% mutate(B_st=B/b_mean$mean)

  DF <- B_all %>% mutate(Biomass=B) %>% filter(Year>=2016)

  dflist <- list()
  years <- seq(2024, by=3, length.out=8)
  for (i in seq_along(years)) {
    yr <- years[i]

    DFa <- DF
    DFa$Catch[DFa$Year>=yr] <- NA
    # Biomass
    p1 <- ggplot(DFa  %>% filter(Year<=yr), aes(x=Year, y=Biomass)) +
      geom_line() +
      expand_limits(y=c(0, 1.2)) +
      theme_bw()

    # Index
    p2 <- ggplot(DFa  %>% filter(Year<=yr), aes(x=Year, y=Index)) +
      geom_line() +
      expand_limits(y=c(0, 1.5)) +
      theme_bw()

    # Catch
    p3 <- ggplot(DFa  %>% filter(Year<=yr), aes(x=Year, y=Catch)) +
      geom_line() +
      expand_limits(y=0) +
      theme_bw()

    nm <- paste(mom, yr, sep="_")
    nm <- paste0(nm, '.png')
    p <- cowplot::plot_grid(p1,p2,p3, nrow=3, align='v')
    ggsave(file.path(out.dir, 'CE_a', nm), p, width=6, height=9)


    # Apply MP
    Data_Lag <- 2
    # CE_a
    tunepar <- 0.868
    x <- ind
    yrs = c(5, 3)
    yr.ind <- which(Data@Year == Data@LHYear)
    hist.yrs <- (yr.ind - yrs[1] + 1):yr.ind
    histER <- mean(Data@Cat[x, hist.yrs])/mean(Data@Ind[x, hist.yrs])
    current_yr <- match(yr, Years$Year)
    current_yr_lag <- current_yr-Data_Lag
    nyears <- MMSE@nyears

    lastTAC <- sum(MMSE@TAC[x,,1,1,current_yr-1-nyears])

    recent_yrs <- (current_yr_lag - yrs[2] + 1):current_yr_lag
    curER <- mean(Data@Cat[x, recent_yrs])/mean(Data@Ind[x, recent_yrs])
    ER_ratio <- histER/curER
    TAC1 <- ER_ratio * tunepar * lastTAC
    TAC <- MaxChange(TAC1, lastTAC, mc=0.25)

    # mean historical exploitation
    histDF <- DF %>% filter(Year %in% Years$Year[hist.yrs]) %>% summarize(C=mean(Catch),
                                                                          I=mean(Index))

    p2a <- p2 + geom_line(data=data.frame(Year=Years$Year[hist.yrs], Index=histDF$I),
                          color='blue')

    p3a <- p3 + geom_line(data=data.frame(Year=Years$Year[hist.yrs], Catch=histDF$C),
                          color='blue')


    p <- cowplot::plot_grid(p1,p2a,p3a, nrow=3, align='v')
    nm <- paste(mom, yr, '1', sep="_")
    nm <- paste0(nm, '.png')
    ggsave(file.path(out.dir, 'CE_a', nm), p, width=6, height=9)

    # mean current exploitation
    currDF <- DF %>% filter(Year %in% Years$Year[recent_yrs]) %>% summarize(C=mean(Catch),
                                                                            I=mean(Index))

    p2b <- p2a + geom_line(data=data.frame(Year=Years$Year[recent_yrs], Index=currDF$I),
                           color='green')

    p3b <- p3a + geom_line(data=data.frame(Year=Years$Year[recent_yrs], Catch=currDF$C),
                           color='green')


    p <- cowplot::plot_grid(p1,p2b,p3b, nrow=3, align='v')
    nm <- paste(mom, yr, '2', sep="_")
    nm <- paste0(nm, '.png')
    ggsave(file.path(out.dir, 'CE_a', nm), p, width=6, height=9)


    # add new TAC
    DFb <- DFa %>% filter(Year==yr)
    DFb$Catch[DFb$Year==yr] <- TAC


    p3c <- p3b +
      geom_point(dat=DFb, size=4)

    p <- cowplot::plot_grid(p1,p2b,p3c, nrow=3, align='v')
    nm <- paste(mom, yr, '3', sep="_")
    nm <- paste0(nm, '.png')
    ggsave(file.path(out.dir, 'CE_a', nm), p, width=6, height=9)


    dflist[[i]] <- data.frame(Year=yr, MOM=mom, histER=histER, curER=curER, ER_ratio=ER_ratio,
                              lastTAC=lastTAC, TAC1=TAC1, TAC=TAC)
  }

  do.call('rbind', dflist)
}




MMSE <- mselist[[2]]
MMSE@multiHist <- histlist[[2]]
SB0 <- MMSE@multiHist[[1]][[1]]@Ref$ReferencePoints$SSB0[ind]

MMSE@SSB[ind,1,1,1:4]/SB0
MMSE@re


matplot(t(MMSE@multiHist$Female[[1]]@SampPars$Stock$Perr_y), type='l')



MMSE <- mselist[[5]]
MMSE@multiHist <- histlist[[5]]
matplot(t(MMSE@multiHist$Female[[1]]@SampPars$Stock$Perr_y), type='l')

