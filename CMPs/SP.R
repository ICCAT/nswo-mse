


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
SPS <- function(x, Data, Data_Lag=1, Interval=3, tunepar=1, mc=NA, ...) {

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
  Ftar <- tunepar * 0.1 #  Mod@FMSY
  Fmin <- 0.1 * Ftar #  * Mod@FMSY
  Bcurr <- Mod@B[length(Mod@B)]

  if (Bcurr>=Bthresh) {
    Fmort <- Ftar
  } else if (Bcurr>Blim) {
    Fmort <- Ftar * (-0.367 + 1.167*  Bcurr/Bthresh)
  } else {
    Fmort <- Fmin
  }

  TAC <-  Fmort*Bcurr

  # Maximum allowed change in TAC
  Rec@TAC <- MaxChange(TAC, Data@MPrec[x], mc)
  Rec
}


SPS2 <- function(x, Data, Data_Lag=1, Interval=3, tunepar=1, mc=NA, ...) {

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
  # assumed LH parameters for calculating r prior
  Data@vbLinf[x] <- 312
  Data@vbK[x] <- 0.0926
  Data@vbt0[x] <- -3.76
  Data@L50[x] <- 173.5
  Data@L95[x] <- 200
  Data@wla <- 4.45E-06
  Data@wlb <- 3.2
  Data@Mort[x] <- 0.2
  Data@steep[x] <- 0.88
  Data@CV_Mort <- 0.01
  Data@CV_steep <- 0.01

  Mod <- SAMtool::SP(x, Data, Euler_Lotka =100)

  # harvest control rule
  # based on: https://www.iccat.int/Documents/Recs/compendiopdf-e/2017-04-e.pdf
  Bthresh <- Mod@BMSY
  Blim <- 0.4 * Bthresh
  Ftar <- tunepar * 0.1 #  Mod@FMSY
  Fmin <- 0.1 * Ftar #  * Mod@FMSY
  Bcurr <- Mod@B[length(Mod@B)]

  if (Bcurr>=Bthresh) {
    Fmort <- Ftar
  } else if (Bcurr>Blim) {
    Fmort <- Ftar * (-0.367 + 1.167*  Bcurr/Bthresh)
  } else {
    Fmort <- Fmin
  }

  TAC <-  Fmort*Bcurr

  # Maximum allowed change in TAC
  Rec@TAC <- MaxChange(TAC, Data@MPrec[x], mc)
  Rec
}

SPS25 <-  SPS
formals(SPS25)$mc <- 0.25




# ---- Tuned CMPs ----
#' @describeIn SPS Tuned to PGK_short = 0.51 across Reference OMs.
#' @export
SPS_a <- SPS
formals(SPS_a)$tunepar <- 1.37701569353006
class(SPS_a) <- "MP"


#' @describeIn SPS Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
SPS_b <- SPS
formals(SPS_b)$tunepar <- 1.28434592227696
class(SPS_b) <- "MP"


#' @describeIn SPS Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
SPS_c <- SPS
formals(SPS_c)$tunepar <- 1.16717153302654
class(SPS_c) <- "MP"


#' @describeIn SPS25 Tuned to PGK_short = 0.51 across Reference OMs.
#' @export
SPS25_a <- SPS25
formals(SPS25_a)$tunepar <- 1.37769734590669
class(SPS25_a) <- "MP"


#' @describeIn SPS25 Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
SPS25_b <- SPS25
formals(SPS25_b)$tunepar <- 1.28547776726585
class(SPS25_b) <- "MP"


#' @describeIn SPS25 Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
SPS25_c <- SPS25
formals(SPS25_c)$tunepar <- 1.17681068083539
class(SPS25_c) <- "MP"


