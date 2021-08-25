## Compare length comps
library(SWOMSE)
library(r4ss)
library(dplyr)

SSdir <- "G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/grid_2021/grid_May2021_shifted/1-M0.1_sigmaR0.2_steepness0.6_cpuelambda0.05_llq1_env-4_iter1"

replist <- SS_output(dir = SSdir, printstats = F, covar=T, cormax=0.70,
                     forecast=T,printhighcor=20, printlowcor=20, ncols=201)

OM <- SS2OM(replist)

OM@CAL_ESS <- c(2000, 2000)
apply(SWOMSE::SWOData@CAL[1,,], 1, sum)

OM@cpars$control$CAL <- 'removals'
OM@nsim <- 10

# OM@cpars$Data <- SWOMSE::SWOData

Hist <- Simulate(OM)

predLen <- replist$lendbase %>% group_by(Yr, Bin) %>%
  summarize(temp=sum(Exp), temp2=sum(Obs)) %>%
  group_by(Yr) %>% mutate(Pred=temp/sum(temp), Obs=temp2/sum(temp2))


CAL_years <- predLen$Yr %>% unique()
nyears <- Hist@Data@CAL %>% ncol()
OM_years <- (OM@CurrentYr-OM@nyears+1):OM@CurrentYr

# 9 most recent years
Yrs <- rev(CAL_years[seq(from=length(CAL_years), length.out=9, by=-1)])

png('Review/CAL_compare.png', width=6, height=6.2, units="in", res=400)

par(mfrow=c(3,3), oma=c(4,4,1,1), mar=c(1,1,1,1))
xlab <- 2013:2015
ylab <- c(2007, 2010, 2013)
ylim <- c(0, 0.1)
for (yr in Yrs) {
  yr_i <- match(yr, OM_years)
  matplot(Hist@Data@CAL_mids, t(Hist@Data@CAL[,yr_i,]/apply(Hist@Data@CAL[,yr_i,], 1, sum)),
          type="l", xlab="", ylab="", bty="n", axes=FALSE, main=yr, ylim=ylim)
  if (yr %in% xlab) {
    axis(side=1)
  } else {
    axis(side=1, label=FALSE)
  }
  if (yr %in% ylab) {
    axis(side=2, las=1)
  } else {
    axis(side=2, label=FALSE)
  }

  tt <- predLen %>% filter(Yr==yr)
  lines(tt$Bin, tt$Obs, lwd=3)
}
mtext(side=1, outer=TRUE, 'Length Class (cm)', line=2.5, cex=1.2)
mtext(side=2, outer=TRUE, 'Proportion', line=2.5, cex=1.2)

dev.off()
