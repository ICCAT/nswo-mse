MCC11 <- function(Data,
                  Data_Lag = 2,
                  Interval = 3,
                  tunepar = 0.756222283813747,
                  mc = NA, ...) {
  advice <- Advice()
  CurrentTS <- tail(Data@TimeSteps,1)
  if ((CurrentTS+1) %in% Catchdf$Year) {
    advice@TAC <-  Catchdf$Catch[match(CurrentTS, Catchdf$Year)]
    return(advice)
  }

  Initial_MP_Yr <- max(Catchdf$Year)+1
  ManagementTimeSteps <- seq(Initial_MP_Yr, by=Interval, length.out=50)

  if (!(CurrentTS+1) %in% ManagementTimeSteps) {
    advice@TAC <- tail(Data@TAC[!is.na(Data@TAC)],1) |> as.numeric()
    return(advice)
  }

  TACbase <- 12600 * tunepar

  CombinedIndex <- Data@Survey@Value[,8]

  Ibase <- mean(CombinedIndex[match(2017:2019, Data@TimeSteps)], na.rm=TRUE)

  # combined index averaged over last available 3 # years in time-series (y-4, y-3, y-2)
  Icurr <- mean(tail(CombinedIndex,3))

  Irat <- Icurr/Ibase

  fixed_low_TAC <- NULL  # initialize

  if (Irat>=1.85) {
    deltaTAC <- 1.85
  }
  if (Irat>=1.75 & Irat<1.85) {
    deltaTAC <- 1.75
  }
  if (Irat>=1.65 & Irat<1.75) {
    deltaTAC <- 1.65
  }
  if (Irat>=1.55 & Irat<1.65) {
    deltaTAC <- 1.55
  }
  if (Irat>=1.45 & Irat<1.55) {
    deltaTAC <- 1.45
  }
  if (Irat>=1.35 & Irat<1.45) {
    deltaTAC <- 1.35
  }
  if (Irat>=1.25 & Irat<1.35) {
    deltaTAC <- 1.25
  }
  if (Irat>=1.15 & Irat<1.25) {
    deltaTAC <- 1.15
  }
  if (Irat>=0.75 & Irat<1.15) {
    deltaTAC <- 1
  }
  if (Irat>=0.5 & Irat<0.75) {
    deltaTAC <- 0.75
  }
  if (Irat<0.5) {
    deltaTAC <- 0.5
  }

  advice@TAC <- TACbase * deltaTAC
  advice
}
class(MCC11) <- 'mp'

FullRetention <- function(Advice) {



  Advice@Retention@Pars <- list(RL50=0, RL50_95=0)
  Advice
}

MCC11_FR <- function(Data,...) {
  Advice <-  MCC11(Data, ...)
  CurrentTS <- tail(Data@TimeSteps,1)
  if ((CurrentTS+1) %in% Catchdf$Year)
    return(Advice)
  Advice |> FullRetention()
}
class(MCC11_FR) <- 'mp'
