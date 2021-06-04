# Combined Index

library(SWOMSE)
library(ggplot2)

OM <- OM_1
OM@nsim <- 5
OM@cpars$Data@Ind[1,67:68] <- OM@cpars$Data@Ind[1,66]

MSE <- runMSE(OM, MPs="Itarget1", extended = TRUE)

sim <- 3
VB <- rowSums(MSE@Hist@TSdata$VBiomass[sim,,])
VB <- c(VB, MSE@VB[sim,, 1:4])
VB <- VB[65:72]

ind <- MSE@PPD[[1]]@Ind[sim,65:72]


par(mfrow=c(1,2))
plot(VB, type="l")
plot(ind, type="l")

MSE@Hist@SampPars$Obs$Ierr_y[sim,65:72]


SBhist <- MSE@SSB_hist
SB <- MSE@SSB[,1,]

PPD <- MSE@PPD[[1]]
Year <- PPD@Year
Year[length(Year)+1] <- Year[length(Year)]+1
Index <- PPD@Ind
Index <- cbind(Index, rep(NA,OM@nsim))

DF <- data.frame(Year=rep(Year, each=OM@nsim),
                 Sim=rep(1:OM@nsim, length(Year)),
                 SB=c(as.vector(SBhist), as.vector(SB)),
                 Index=as.vector(Index),
                 Period=c(rep("Historical",OM@nyears*OM@nsim), rep("Projection",OM@proyears*OM@nsim)))
)


DF <- DF %>% group_by(Year) %>%
  mutate(med=median(Index, na.rm=TRUE),
         upper=quantile(Index,0.95, na.rm=TRUE),
         lower=quantile(Index,0.05, na.rm=TRUE))

DF_hist <- DF %>% ungroup() %>% filter(Period=="Historical", Sim==1) %>%
  mutate(SB=SB/mean(SB, na.rm=T), Index=Index/mean(Index, na.rm=T)) %>%
  tidyr::pivot_longer(cols=3:4)


ggplot(DF_hist, aes(x=Year, y=value, color=name)) +
  geom_line(size=1.2) +
  theme_bw() + labs(color="Legend")


ggplot(DF, aes(x=Year, y=SB, color=Period, group=Sim)) +
  geom_line(size=1.2) +
  theme_bw() + labs(color="Legend")

DF3 <- DF %>% distinct(Year, med, upper, lower, Period)

ggplot(DF3, aes(x=Year, y=med)) +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.1) +
  geom_line(size=1.2, aes(color=Period)) +
  theme_bw() + labs(color="Legend", y="Index") +
  coord_cartesian(ylim=c(50,475))




DF2 <- DF %>% tidyr::pivot_longer(cols=c(3,6))

ggplot(DF2) +
  geom_line(aes(x=Year, y=value, group=Sim, color=Period)) +
  facet_wrap(~name, scales="free")



ggplot(DF2) +
  geom_line(aes(x=Year, y=med, group=Sim, color=Period))

MSE@Hist@SampPars$Obs$Ind_Stat

