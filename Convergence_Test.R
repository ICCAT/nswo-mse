library(SWOMSE)

# Investigate Discard Mortality

MOM <- MOM_000
MOM@nsim <- 5

MOM@cpars$Female[[1]]$Fdisc_array1 <- MOM@cpars$Female[[1]]$Fdisc_array1 * 0
MOM@cpars$Male[[1]]$Fdisc_array1 <- MOM@cpars$Male[[1]]$Fdisc_array1 * 0

MOM@cpars$Female[[1]]$Fdisc_array2 <- MOM@cpars$Female[[1]]$Fdisc_array2 * 0
MOM@cpars$Male[[1]]$Fdisc_array2 <- MOM@cpars$Male[[1]]$Fdisc_array2 * 0

MOM@cpars$Female[[1]]$Fdisc <- MOM@cpars$Female[[1]]$Fdisc * 0
MOM@cpars$Male[[1]]$Fdisc <- MOM@cpars$Male[[1]]$Fdisc * 0



multiHist <- SimulateMOM(MOM)

C_sim <- rowSums(multiHist[[1]][[1]]@TSdata$Landings[1,,]) + rowSums(multiHist[[2]][[1]]@TSdata$Landings[1,,])

df <- data.frame(Year=SWOData@Year, Catch=SWOData@Cat[1,], C_sim=C_sim)

par(mfrow=c(1,2))
plot(df$Year, df$Catch, type='l')
lines(df$Year, df$C_sim, col='blue')

plot(multiHist$Female[[1]]@AtAge$F.Mortality[1,26,,1], type='l')
lines(multiHist$Female[[1]]@AtAge$Fret.Mortality[1,26,,1], col='blue')
