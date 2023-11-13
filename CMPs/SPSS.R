
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



SPSSFox2 <- function(x, Data, Data_Lag=1, Interval=3, tunepar=1, mc=0.25, ...) {

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
  Rec@TAC <- MaxChange2(TAC, Data@MPrec[x], mc, Brel = Bcurr/Bthresh)
  Rec
}
class(SPSSFox2) <- 'MP'



# ---- Tuned CMPs ----
#' @describeIn SPSSFox Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
SPSSFox_b <- SPSSFox
formals(SPSSFox_b)$tunepar <- 0.891583873666597
class(SPSSFox_b) <- "MP"


#' @describeIn SPSSFox Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
SPSSFox_c <- SPSSFox
formals(SPSSFox_c)$tunepar <- 0.845612842501569
class(SPSSFox_c) <- "MP"


#' @describeIn SPSSFox Tuned to PGK_med = 0.6 across Reference OMs.
#' @export
SPSSFox_d <- SPSSFox
formals(SPSSFox_d)$tunepar <- 0.899164113355825
class(SPSSFox_d) <- "MP"


#' @describeIn SPSSFox Tuned to PGK_long = 0.6 across Reference OMs.
#' @export
SPSSFox_e <- SPSSFox
formals(SPSSFox_e)$tunepar <- 0.945269016697588
class(SPSSFox_e) <- "MP"


#' @describeIn SPSSFox2 Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
SPSSFox2_b <- SPSSFox2
formals(SPSSFox2_b)$tunepar <- 0.891657057444295
class(SPSSFox2_b) <- "MP"


#' @describeIn SPSSFox2 Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
SPSSFox2_c <- SPSSFox2
formals(SPSSFox2_c)$tunepar <- 0.845612842501569
class(SPSSFox2_c) <- "MP"


#' @describeIn SPSSFox2 Tuned to PGK_med = 0.6 across Reference OMs.
#' @export
SPSSFox2_d <- SPSSFox2
formals(SPSSFox2_d)$tunepar <- 0.90188998183886
class(SPSSFox2_d) <- "MP"


#' @describeIn SPSSFox2 Tuned to PGK_long = 0.6 across Reference OMs.
#' @export
SPSSFox2_e <- SPSSFox2
formals(SPSSFox2_e)$tunepar <- 0.958241665482627
class(SPSSFox2_e) <- "MP"


