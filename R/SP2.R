
#' @describeIn SP1 Same as SP1, but uses the Fox production model
SP2 <- function(x, Data, Data_Lag=1, Interval=3, tunepar=1, mc=0.25, ...) {

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
  Mod <- SAMtool::SP_Fox(x, Data)

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
    Fmort <- Mod@FMSY * (-0.367 + 1.167*  Bcurr/Bthresh)
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
#' @describeIn SP2 Tuned to PGK_6_10 = 0.6 across Reference OMs.
#' @export
SP2_a <- SP2
formals(SP2_a)$tunepar <- 1.50034013605442
class(SP2_a) <- "MP"


#' @describeIn SP2 Tuned to PGK_med = 0.6 across Reference OMs.
#' @export
SP2_b <- SP2
formals(SP2_b)$tunepar <- 1.50936199722607
class(SP2_b) <- "MP"


#' @describeIn SP2 Tuned to PGK_long = 0.6 across Reference OMs.
#' @export
SP2_c <- SP2
formals(SP2_c)$tunepar <- 1.81645962732919
class(SP2_c) <- "MP"


#' @describeIn SP2 Tuned to LRP = 0.15 across Reference OMs.
#' @export
SP2_d <- SP2
formals(SP2_d)$tunepar <- 1.97168367346939
class(SP2_d) <- "MP"


#' @describeIn SP2 Tuned to PGK_6_10 = 0.51 across Reference OMs.
#' @export
SP2_e <- SP2
formals(SP2_e)$tunepar <- 1.57020308123249
class(SP2_e) <- "MP"


#' @describeIn SP2 Tuned to LRP = 0.1 across Reference OMs.
#' @export
SP2_g <- SP2
formals(SP2_g)$tunepar <- 1.83325330132053
class(SP2_g) <- "MP"


#' @describeIn SP2 Tuned to LRP = 0.05 across Reference OMs.
#' @export
SP2_h <- SP2
formals(SP2_h)$tunepar <- 1.63035714285714
class(SP2_h) <- "MP"


