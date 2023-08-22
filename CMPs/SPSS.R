
SPSS <- function(x, Data, Data_Lag=1, Interval=3, tunepar=1, mc=0.25, ...) {

  Rec <- new('Rec')

  # Does TAC need to be updated? (or set a fixed catch if before Initial_MP_Yr)
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    Rec <- FixedTAC(Rec, Data) # use actual catches if they are available
    return(Rec)
  }

  # Lag Data
  Data <- Lag_Data(Data, Data_Lag)

  # smooth combined index
  index <- smoothed_index <- Data@Ind[x,]
  smoothed <- stats::smooth(index[!is.na(index)])
  smoothed_index[!is.na(smoothed_index)] <- smoothed

  Data@Ind[x,] <- smoothed_index

  # apply SP_SS assessment model
  Mod <- SAMtool::SP_SS(x, Data, prior=list(r=c(0.21, 0.1)))

  # harvest control rule
  # based on: https://www.iccat.int/Documents/Recs/compendiopdf-e/2017-04-e.pdf
  Bthresh <- Mod@BMSY
  Blim <- 0.4 * Bthresh
  Ftar <- tunepar * 0.15 #  Mod@FMSY
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
  Rec@TAC <- TAC

  # Rec@Misc[[x]] <- Mod
  Rec@TAC <- MaxChange(TAC, Data@MPrec[x], mc)
  Rec
}
class(SPSS) <- 'MP'

# tested and made no difference
# SPSS2 <- function(x, Data, Data_Lag=1, Interval=3, tunepar=1, mc=0.25, ...) {
#
#   Rec <- new('Rec')
#
#   # Does TAC need to be updated? (or set a fixed catch if before Initial_MP_Yr)
#   if (SameTAC(Initial_MP_Yr, Interval, Data)) {
#     Rec@TAC <- Data@MPrec[x]
#     Rec <- FixedTAC(Rec, Data) # use actual catches if they are available
#     return(Rec)
#   }
#
#   # Lag Data
#   Data <- Lag_Data(Data, Data_Lag)
#
#   # smooth combined index
#   index <- smoothed_index <- Data@Ind[x,]
#   smoothed <- stats::smooth(index[!is.na(index)])
#   smoothed_index[!is.na(smoothed_index)] <- smoothed
#
#   Data@Ind[x,] <- smoothed_index
#
#   # apply SP_SS assessment model
#   Mod <- SAMtool::SP_SS(x, Data, prior=list(r=c(0.21, 0.1)))
#
#   # harvest control rule
#   # based on: https://www.iccat.int/Documents/Recs/compendiopdf-e/2017-04-e.pdf
#   Bthresh <- Mod@BMSY
#   Blim <- 0.5 * Bthresh
#   Ftar <- tunepar * 0.15 #  Mod@FMSY
#   Fmin <- 0.2 * Ftar #  * Mod@FMSY
#   Bcurr <- Mod@B[length(Mod@B)]
#
#   if (Bcurr>=Bthresh) {
#     Fmort <- Ftar
#   } else if (Bcurr>Blim) {
#     Fmort <- Ftar * (-0.367 + 1.167*  Bcurr/Bthresh)
#   } else {
#     Fmort <- Fmin
#   }
#
#   TAC <-  Fmort*Bcurr
#   Rec@TAC <- TAC
#
#   # Rec@Misc[[x]] <- Mod
#   Rec@TAC <- MaxChange(TAC, Data@MPrec[x], mc)
#   Rec
# }
# class(SPSS2) <- 'MP'

SPSSFox <- function(x, Data, Data_Lag=1, Interval=3, tunepar=1, mc=0.25, ...) {

  Rec <- new('Rec')

  # Does TAC need to be updated? (or set a fixed catch if before Initial_MP_Yr)
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    Rec <- FixedTAC(Rec, Data) # use actual catches if they are available
    return(Rec)
  }

  # Lag Data
  Data <- Lag_Data(Data, Data_Lag)

  # smooth combined index
  index <- smoothed_index <- Data@Ind[x,]
  smoothed <- stats::smooth(index[!is.na(index)])
  smoothed_index[!is.na(smoothed_index)] <- smoothed

  Data@Ind[x,] <- smoothed_index

  # apply SP_SS assessment model
  Mod <- SAMtool::SP_SS(x, Data, prior=list(r=c(0.21, 0.1)), start=list(n=1), fix_n=TRUE)

  # harvest control rule
  # based on: https://www.iccat.int/Documents/Recs/compendiopdf-e/2017-04-e.pdf
  Bthresh <- Mod@BMSY
  Blim <- 0.4 * Bthresh
  Ftar <- tunepar * 0.15 #  Mod@FMSY
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
  Rec@TAC <- TAC

  # Rec@Misc[[x]] <- Mod
  Rec@TAC <- MaxChange(TAC, Data@MPrec[x], mc)
  Rec
}
class(SPSSFox) <- 'MP'



# ---- Tuned CMPs ----
#' @describeIn SPSS Tuned to PGK_short = 0.51 across Reference OMs.
#' @export
SPSS_a <- SPSS
formals(SPSS_a)$tunepar <- 0.872384937238494
class(SPSS_a) <- "MP"


#' @describeIn SPSS Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
SPSS_b <- SPSS
formals(SPSS_b)$tunepar <- 0.728723404255319
class(SPSS_b) <- "MP"


#' @describeIn SPSS Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
SPSS_c <- SPSS
formals(SPSS_c)$tunepar <- 0.636950549450549
class(SPSS_c) <- "MP"


#' @describeIn SPSSFox Tuned to PGK_short = 0.51 across Reference OMs.
#' @export
SPSSFox_a <- SPSSFox
formals(SPSSFox_a)$tunepar <- 1.00261096605744
class(SPSSFox_a) <- "MP"


#' @describeIn SPSSFox Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
SPSSFox_b <- SPSSFox
formals(SPSSFox_b)$tunepar <- 0.948096976016684
class(SPSSFox_b) <- "MP"


#' @describeIn SPSSFox Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
SPSSFox_c <- SPSSFox
formals(SPSSFox_c)$tunepar <- 0.891204011893667
class(SPSSFox_c) <- "MP"


