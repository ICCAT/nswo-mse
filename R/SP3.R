
#' @describeIn SP1 Fox production model with
SP3 <- function(x, Data, Data_Lag=1, Interval=3, tunepar=1, mc=0.25, ...) {

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
  Fproxy <- 0.1
  Ftar <- tunepar * Fproxy
  Fmin <- 0.1 * Ftar
  Bcurr <- Mod@B[length(Mod@B)]

  if (Bcurr>=Bthresh) {
    Fmort <- Ftar
  } else if (Bcurr>Blim) {
    Fmort <- Mod@FMSY * (-0.367 + 1.167*  Bcurr/Bthresh)
  } else {
    Fmort <- Fmin
  }

  TAC <-  Fmort*Bcurr

  # Maximum allowed change in TAC
  Rec@TAC <- MaxChange(TAC, Data@MPrec[x], mc)
  Rec
}


# ---- Tuned CMPs ----
#' @describeIn SP3 Tuned to PGK_6_10 = 0.6 across Reference OMs.
#' @export
SP3_a <- SP3
formals(SP3_a)$tunepar <- 1.00732609275053
class(SP3_a) <- "MP"


#' @describeIn SP3 Tuned to PGK_med = 0.6 across Reference OMs.
#' @export
SP3_b <- SP3
formals(SP3_b)$tunepar <- 1.07142857142857
class(SP3_b) <- "MP"


#' @describeIn SP3 Tuned to PGK_6_10 = 0.51 across Reference OMs.
#' @export
SP3_e <- SP3
formals(SP3_e)$tunepar <- 1.05251409774436
class(SP3_e) <- "MP"


#' @describeIn SP3 Tuned to LRP = 0.05 across Reference OMs.
#' @export
SP3_h <- SP3
formals(SP3_h)$tunepar <- 1.05535714285714
class(SP3_h) <- "MP"


