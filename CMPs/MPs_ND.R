#' Constant Exploitation with Control Rule
#'
#' goal in this CMP would be toa have the catch remain as constant as possible and only increase if the
#' index rose substantially and only decrease if the index declined substantially. The base TAC (constant
#' catch) would be 12,600 as this CC TAC that would allow PGK60 and LRP15 (ideally we could get this
#' down) to be achieved.
#'
#' @param x A position in the data object
#' @param Data An object of class `Data`
#' @param Data_Lag The number of years to lag the data
#' @param Interval The TAC update interval
#' @param tunepar Parameter used for tuning
#' @param mc The maximum fractional change in the TAC among years. NA to ignore
#' @param ... Additional arguments (unused)
#'
#' @return An object of class `Rec` with the `TAC` slot populated
#'
MCC <- function(x, Data, Data_Lag=1, Interval=3, tunepar=1, mc=NA, ...) {
  Rec <- new('Rec')

  # Does TAC need to be updated? (or set a fixed catch if before Initial_MP_Yr)
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    Rec <- FixedTAC(Rec, Data) # use actual catches if they are available
    return(Rec)
  }

  # Lag Data
  Data <- Lag_Data(Data, Data_Lag)

  # Base TAC
  TACbase <- 12600 * tunepar

  # Combined Index averaged for 2018 - 2020
  Ibase <- mean(Data@Ind[x, match(2018:2020, Data@Year)], na.rm=TRUE)

  # combined index averaged over last available 3 # years in time-series (y-4, y-3, y-2)
  Icurr <- mean(tail(Data@Ind[x,],3))

  Irat <- Icurr/Ibase

  if (Irat>=1.25) {
    deltaTAC <- 1.2
  }
  if (Irat>=0.75 & Irat<1.25) {
    deltaTAC <- 1
  }
  if (Irat>=0.5 & Irat<0.75) {
    deltaTAC <- 0.75
  }
  if (Irat<0.5) {
    deltaTAC <- 0.5
  }

  TAC <- TACbase * deltaTAC
  # Maximum allowed change in TAC
  Rec@TAC <- MaxChange(TAC, Data@MPrec[x], mc)

  Rec
}

GSC <- function(x, Data, Data_Lag=1, Interval=3, tunepar=1, mc=NA, ...) {
  Rec <- new('Rec')

  # Does TAC need to be updated? (or set a fixed catch if before Initial_MP_Yr)
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    Rec <- FixedTAC(Rec, Data) # use actual catches if they are available
    return(Rec)
  }

  # Lag Data
  Data <- Lag_Data(Data, Data_Lag)


  # combined index averaged over last available 3 # years in time-series (y-4, y-3, y-2)
  Icurr <- mean(tail(Data@Ind[x,],3))

  # combined index averaged over 3-yr prior to Icurr
  Iprev <- mean(tail(Data@Ind[x,],6)[1:3])

  Irat <- Icurr/Iprev

  TAC_add <- 0
  if (Irat>=1.20) {
    deltaTAC <- 1.2
  }
  if (Irat>=1.05 & Irat <1.20) {
    deltaTAC <- 1
    TAC_add <- 1000
  }
  if (Irat>=0.95 & Irat <1.05) {
    deltaTAC <- 1
  }
  if (Irat>=0.8 & Irat <0.95) {
    deltaTAC <- 1
    TAC_add <- -1000
  }
  if (Irat<0.8) {
    deltaTAC <- 0.75
  }

  TAC <- Data@MPrec[x] * tunepar * deltaTAC + TAC_add
  # Maximum allowed change in TAC
  Rec@TAC <- MaxChange(TAC, Data@MPrec[x], mc)

  Rec
}

