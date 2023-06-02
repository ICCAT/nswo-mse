#' Constant Exploitation
#'
#' This MP aims to keep the exploitation rate at a constant level. The current relative
#' exploitation rate is calculated as the mean of the catches from 2016:2020 divided
#' by the mean Combined Index over this same period.
#'
#' @param x A position in the data object
#' @param Data An object of class `Data`
#' @param Data_Lag The number of years to lag the data
#' @param Interval The TAC update interval
#' @param tunepar Parameter used for tuning
#' @param mc The maximum fractional change in the TAC among years. NA to ignore
#' @param yrs numeric values length 2. The year index to calculate the historical mean catches and index,
#' and the years to smooth the estimate of current exploitation rate
#' @param ... Additional arguments (unused)
#'
#' @return An object of class `Rec` with the `TAC` slot populated
#'
CE <- function(x, Data, Data_Lag=1, Interval=3, tunepar=1, mc=0.25,
                yrs=c(5,3), ...) {
  Rec <- new('Rec')

  # Does TAC need to be updated? (or set a fixed catch if before Initial_MP_Yr)
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    Rec <- FixedTAC(Rec, Data) # use actual catches if they are available
    return(Rec)
  }

  # Lag Data
  Data <- Lag_Data(Data, Data_Lag)

  # Calculate Historical Relative Exploitation Rate
  yr.ind <- which(Data@Year==Data@LHYear)
  hist.yrs <- (yr.ind-yrs[1]+1):yr.ind
  histER <- mean(Data@Cat[x,hist.yrs])/mean(Data@Ind[x,hist.yrs])

  # Calculate Current Relative Exploitation Rate
  current_yr <- length(Data@Ind[x,])
  recent_yrs <- (current_yr-yrs[2]+1):current_yr
  curER <- mean(Data@Cat[x,recent_yrs])/mean(Data@Ind[x,recent_yrs])

  # Exploitation Rate Ratio
  ER_ratio <- histER/curER

  TAC <- ER_ratio * tunepar * Data@MPrec[x]

  # Maximum allowed change in TAC
  Rec@TAC <- MaxChange(TAC, Data@MPrec[x], mc)
  Rec
}

