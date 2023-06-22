
#' @describeIn SP Same as SP, but uses the Fox production model
SPFox <- function(x, Data, Data_Lag=1, Interval=3, tunepar=1, mc=NA, ...) {

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
  Ftar <- tunepar * 0.1 #  Mod@FMSY
  Fmin <- 0.1 * Ftar #  * Mod@FMSY
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


SPFox25 <-  SPFox
formals(SPFox25)$mc <- 0.25


# ---- Tuned CMPs ----
#' @describeIn SPFox Tuned to PGK_short = 0.51 across Reference OMs.
#' @export
SPFox_a <- SPFox
formals(SPFox_a)$tunepar <- 0.884312403656756
class(SPFox_a) <- "MP"


#' @describeIn SPFox Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
SPFox_b <- SPFox
formals(SPFox_b)$tunepar <- 0.821557128103843
class(SPFox_b) <- "MP"


#' @describeIn SPFox Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
SPFox_c <- SPFox
formals(SPFox_c)$tunepar <- 0.772372878653817
class(SPFox_c) <- "MP"


#' @describeIn SPFox25 Tuned to PGK_short = 0.51 across Reference OMs.
#' @export
SPFox25_a <- SPFox25
formals(SPFox25_a)$tunepar <- 0.86066429709785
class(SPFox25_a) <- "MP"


#' @describeIn SPFox25 Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
SPFox25_b <- SPFox25
formals(SPFox25_b)$tunepar <- 0.812346680601946
class(SPFox25_b) <- "MP"


#' @describeIn SPFox25 Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
SPFox25_c <- SPFox25
formals(SPFox25_c)$tunepar <- 0.765283097484389
class(SPFox25_c) <- "MP"


