plot_HistRecDevs <- function() {
  Years <- (SWOM@CurrentYr-SWOM@nyears+1):SWOM@CurrentYr
  png("Figures/HistRecDevs.png")
  matplot(Years, t(SWOM@cpars$Perr_y[,SWOM@maxage:(SWOM@nyears+SWOM@maxage-1)]), type="l",
          xlab="Year", ylab="Recruitment Deviations", bty="l")
  dev.off()
}

plot_HistDep <- function() {
  png("Figures/HistDep.png")
  hist(SWOM@cpars$D, breaks=seq(0, 1, 0.025), col="gray", main="",
       xlab="Depletion")
  text(0.8, 20, "n=174")
  abline(v=SWOM@cpars$D[1], lty=2)
  dev.off()
}

plot_Hist <- function() {
  Hist <- DLMtool::runMSE(SWOM, Hist=TRUE, parallel=TRUE)
  png("Figures/Hist_SSB_Catch.png", units="in",
       width=6, height=3, res=300)
  par(mfrow=c(1,2), oma=c(1,1,0,0), mar=c(4,4,1,1))
  Years <- (SWOM@CurrentYr-SWOM@nyears+1):SWOM@CurrentYr
  matplot(Years, t(Hist@TSdata$SSB), type="l", bty="l",
          ylab="Spawning Biomass")
  matplot(Years, t(Hist@TSdata$Catch), type="l", bty="l",
          ylab="Catch", ylim=c(0, max(Hist@TSdata$Catch)))
  dev.off()
}
