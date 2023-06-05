#' Index Targeting 1
#'
#'
#' @param x A position in the data object
#' @param Data An object of class `Data`
#' @param Data_Lag The number of years to lag the data
#' @param Interval The TAC update interval
#' @param tunepar Parameter used for tuning
#' @param mc The maximum fractional change in the TAC among years. NA to ignore
#' @param yrs numeric vector length 2. The years to calculate the mean index. Ratio is
#' calculated as mean(ind[yrs[1]])/mean(ind[yrs[2]])
#' @param ... Additional arguments (unused)
#'
#' @return An object of class `Rec` with the `TAC` slot populated
#'
IT1 <- function(x, Data, Data_Lag=1, Interval=3, tunepar=1, mc=0.25, change=0.1,
                smooth_yr=3, ...) {
  Rec <- new('Rec')

  # Does TAC need to be updated? (or set a fixed catch if before Initial_MP_Yr)
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    Rec <- FixedTAC(Rec, Data) # use actual catches if they are available
    return(Rec)
  }

  # Lag Data
  Data <- Lag_Data(Data, Data_Lag)

  # Hist TAC
  TAC_base <- 13200
  TAC <- Data@MPrec[x]
  # Current Index Value
  current_yr <- length(Data@Ind[x,])
  CurI <- mean(Data@Ind[x,(current_yr-smooth_yr+1):current_yr])

  # Target Index Value
  TargI <-  mean(Data@Ind[x, 69:71])

  # Adjust TAC
  change <- change * tunepar
  if (CurI/TargI>1) {
    if (CurI>TargI*1.1) {
      TAC <-  TAC_base * (1+change)
    }
    if (CurI>TargI*1.3) {
      TAC <-TAC_base * (1+2*change)
    }

  }
  if (CurI/TargI<1) {
    if (CurI<TargI*0.9) {
      TAC <-  TAC_base * (1-change)
    }
    if (CurI<TargI*0.5) {
      TAC <-TAC_base * (1-2*change)
    }
  }

  # Maximum allowed change in TAC
  Rec@TAC <- MaxChange(TAC, Data@MPrec[x], mc)
  Rec
}

