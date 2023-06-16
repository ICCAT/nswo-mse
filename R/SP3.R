
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
formals(SP3_a)$tunepar <- 1.02930402930403
class(SP3_a) <- "MP"


#' @describeIn SP3 Tuned to PGK_med = 0.6 across Reference OMs.
#' @export
SP3_b <- SP3
formals(SP3_b)$tunepar <- 1.16587957497048
class(SP3_b) <- "MP"


#' @describeIn SP3 Tuned to PGK_long = 0.6 across Reference OMs.
#' @export
SP3_c <- SP3
formals(SP3_c)$tunepar <- 1.3936170212766
class(SP3_c) <- "MP"


#' @describeIn SP3 Tuned to LRP = 0.15 across Reference OMs.
#' @export
SP3_d <- SP3
formals(SP3_d)$tunepar <- 1.29591836734694
class(SP3_d) <- "MP"


#' @describeIn SP3 Tuned to PGK_6_10 = 0.51 across Reference OMs.
#' @export
SP3_e <- SP3
formals(SP3_e)$tunepar <- 1.10885372112917
class(SP3_e) <- "MP"


#' @describeIn SP3 Tuned to PGK_6_10 = 0.7 across Reference OMs.
#' @export
SP3_f <- SP3
formals(SP3_f)$tunepar <- 0.949722607489598
class(SP3_f) <- "MP"


#' @describeIn SP3 Tuned to LRP = 0.1 across Reference OMs.
#' @export
SP3_g <- SP3
formals(SP3_g)$tunepar <- 1.21243042671614
class(SP3_g) <- "MP"


#' @describeIn SP3 Tuned to LRP = 0.05 across Reference OMs.
#' @export
SP3_h <- SP3
formals(SP3_h)$tunepar <- 1.12894248608534
class(SP3_h) <- "MP"


