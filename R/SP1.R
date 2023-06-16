


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

# ---- Tuned CMPs ----
#' @describeIn SP1 Tuned to PGK_6_10 = 0.6 across Reference OMs.
#' @export
SP1_a <- SP1
formals(SP1_a)$tunepar <- 3.11318681318681
class(SP1_a) <- "MP"


#' @describeIn SP1 Tuned to PGK_med = 0.6 across Reference OMs.
#' @export
SP1_b <- SP1
formals(SP1_b)$tunepar <- 3.38664343786295
class(SP1_b) <- "MP"


#' @describeIn SP1 Tuned to PGK_long = 0.6 across Reference OMs.
#' @export
SP1_c <- SP1
formals(SP1_c)$tunepar <- 3.61603053435114
class(SP1_c) <- "MP"


#' @describeIn SP1 Tuned to PGK_6_10 = 0.51 across Reference OMs.
#' @export
SP1_e <- SP1
formals(SP1_e)$tunepar <- 3.33004926108374
class(SP1_e) <- "MP"


#' @describeIn SP1 Tuned to LRP = 0.1 across Reference OMs.
#' @export
SP1_g <- SP1
formals(SP1_g)$tunepar <- 3.87857142857143
class(SP1_g) <- "MP"


#' @describeIn SP1 Tuned to LRP = 0.05 across Reference OMs.
#' @export
SP1_h <- SP1
formals(SP1_h)$tunepar <- 3.32857142857143
class(SP1_h) <- "MP"


