#' Index Ratio 1
#'
#' This MP adjusts the TAC based on ratio of the mean index over the last 3 years (y-2):y
#' to the mean index over the 3 years before that (y-5):(y-3)
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
IR1 <- function(x, Data, Data_Lag=1, Interval=3, tunepar=1, mc=0.25,
                yrs=c(3,3), ...) {
  Rec <- new('Rec')

  # Does TAC need to be updated? (or set a fixed catch if before Initial_MP_Yr)
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    Rec <- FixedTAC(Rec, Data) # use actual catches if they are available
    return(Rec)
  }

  # Lag Data
  Data <- Lag_Data(Data, Data_Lag)

  # Calculate Index Ratio
  current_yr <- length(Data@Ind[x,])
  recent_yrs <- (current_yr-yrs[1]+1):current_yr
  previous_yrs <- (current_yr-yrs[2]-yrs[1]+1):(current_yr-yrs[1])

  Iratio <- mean(Data@Ind[x,recent_yrs], na.rm=TRUE)/mean(Data@Ind[x,previous_yrs], na.rm=TRUE)

  TACdelta <- Iratio * tunepar
  TAC <- TACdelta * Data@MPrec[x]

  # Maximum allowed change in TAC
  Rec@TAC <- MaxChange(TAC, Data@MPrec[x], mc)
  Rec
}

