## Compare length comps
library(SWOMSE)
library(r4ss)
library(dplyr)

SSdir <- "G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/grid_2021/grid_May2021_shifted/1-M0.1_sigmaR0.2_steepness0.6_cpuelambda0.05_llq1_env-4_iter1"

replist <- SS_output(dir = SSdir, printstats = F, covar=T, cormax=0.70,
                     forecast=T,printhighcor=20, printlowcor=20, ncols=201)

OM <- SS2OM(replist)

OM@cpars$control$CAL <- 'removals'
OM@nsim <- 5
OM@cpars$Data <- SWOMSE::SWOData

Hist <- Simulate(OM)

plot(Hist@Data@CAL_mids, Hist@Data@CAL[1,61,]/sum(Hist@Data@CAL[1,61,]))
lines(OM@cpars$Data@CAL_mids, OM@cpars$Data@CAL[1,61,]/sum(OM@cpars$Data@CAL[1,61,]))


predLen <- replist$lendbase %>% group_by(Yr, Bin) %>%
  summarize(temp=sum(Exp), temp2=sum(Obs)) %>%
  group_by(Yr) %>% mutate(Pred=temp/sum(temp), Obs=temp2/sum(temp2))

tt <- predLen %>% filter(Yr==2015)
lines(tt$Bin, tt$Obs)


SimYear <- (OM@CurrentYr-OM@nyears+1):OM@CurrentYr
LenYr <- predLen$Yr %>% unique()

YrInd <- which(SimYear %in% LenYr)

Bins <- Hist@Data@CAL_bins
Mids <- Hist@Data@CAL_mids

predBins <- predLen$Bin %>% unique() %>% sort()
BinInd <- which(Bins %in% predBins)

sim <- 1
yr <- 40
SimLen <- Hist@Data@CAL[sim,YrInd,BinInd]

tt <- predLen %>% filter(Yr==LenYr[yr])

predBins <- tt$Bin %>% unique() %>% sort()
BinInd <- which(Bins %in% predBins)
SimLen <- Hist@Data@CAL[sim,YrInd[yr],]

plot(Mids, SimLen/sum(SimLen), type="l")
lines(tt$Bin+2.5, tt$Pred, col="blue")
lines(tt$Bin+2.5, tt$Obs, col="red")

lines(OM@cpars$Data@CAL_mids,OM@cpars$Data@CAL[1,yr,]/sum(OM@cpars$Data@CAL[1,yr,]), col="green")


