


#' A Surplus Production model with a Harvest Control Rule that linearly reduces F
#'
#' The SP model assumes a Schaefer production model
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
SP1 <- function(x, Data, Data_Lag=1, Interval=3, tunepar=1, mc=0.25, ...) {

  Rec <- new('Rec')

  # Does TAC need to be updated? (or set a fixed catch if before Initial_MP_Yr)
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    Rec <- FixedTAC(Rec, Data) # use actual catches if they are available
    return(Rec)
  }

  # Lag Data
  Data <- Lag_Data(Data, Data_Lag)

  # apply SP assessment model
  Mod <- SAMtool::SP(x, Data)

  # harvest control rule
  # based on: https://www.iccat.int/Documents/Recs/compendiopdf-e/2017-04-e.pdf
  Bthresh <- Mod@BMSY
  Blim <- 0.4 * Bthresh
  Ftar <- 0.8 * tunepar * Mod@FMSY
  Fmin <- 0.1 * tunepar * Mod@FMSY
  Bcurr <- Mod@B[length(Mod@B)]

  if (Bcurr>=Bthresh) {
    Fmort <- Ftar
  } else if (Bcurr>Blim) {
    Fmort <- Ftar * (-0.367 + 1.167*  Bcurr/Bthresh)
  } else {
    Fmort <- Fmin
  }

  M <- 0.2 # assumed natural mortality
  Z <- M+Fmort
  TAC <-  Fmort/Z*(1-exp(-Z))*Bcurr

  # Maximum allowed change in TAC
  Rec@TAC <- MaxChange(TAC, Data@MPrec[x], mc)
  Rec
}

