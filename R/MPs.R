#' A simple index targeting MP
#'
#' @param x A position in the data object
#' @param Data An object of class `Data`
#' @param reps The number of replicates to include
#' @param yrsmth The number of recent years to average index over
#' @param mc The maximum fractional change in the TAC among years
#'
#' @return An object of class `Rec` with management recommendations
#' @export
#'
#' @examples
#'  Itarg1(1, SWOData)
Itarg1 <- function(x, Data, reps = 100, yrsmth = 5, mc = 0.05) {

  ind <- max(1, (length(Data@Year) - yrsmth + 1)):length(Data@Year)

  deltaI <- mean(Data@Ind[x, ind], na.rm=TRUE)/Data@Iref[x]

  if (deltaI < (1 - mc)) deltaI <- 1 - mc
  if (deltaI > (1 + mc)) deltaI <- 1 + mc
  Ind_CV <- mean(Data@CV_Ind[x,ind], na.rm=TRUE)
  if (is.na(Ind_CV)) Ind_CV <-  mean(Data@CV_Ind[x,], na.rm=TRUE)

  TAC <- Data@MPrec[x] * deltaI * trlnorm(reps, 1, Ind_CV)

  Rec <- new("Rec")
  Rec@TAC <- TACfilter(TAC)
  Rec
}
class(Itarg1) <- 'MP'

#' A second simple index targeting MP
#'
#' A management procedure that incrementally adjusts the TAC (starting from
#' reference level that is a fraction of mean recent catches)
#' to reach a target CPUE / relative abundance index.
#'
#' Based on the index target MP developed by Geromont and Butterworth (2014)
#' and evaluated in Carruthers et al. (2015).
#'
#' @param x A position in the data object
#' @param Data An object of class `Data`
#' @param reps The number of replicates to include
#' @param Imulti Parameter controlling how much larger target CPUE / index is compared with recent levels
#'
#' @return An object of class `Rec` with management recommendations
#' @export
#'
#' @examples
#' Itarg2(1, SWOData)
#'
#' @references Carruthers et al. 2015. Performance evaluation of simple
#' management procedures. ICES J. Mar Sci. 73, 464-482.
#'
#' Geromont, H.F., Butterworth, D.S. 2014. Generic management procedures for
#' data-poor fisheries; forecasting with few data. ICES J. Mar. Sci. 72, 251-261.
#' doi:10.1093/icesjms/fst232
#' @importFrom MSEtool TACfilter trlnorm
Itarg2 <- function(x, Data, reps = 100, Imulti = 1.5) {
  ind <- (length(Data@Year) - 4):length(Data@Year)  # recent 5 years
  ylast <- match(Data@LHYear[1], Data@Year)  #last historical year
  ind2 <- (ylast - 4):ylast  # historical 5 pre-projection years
  ind3 <- (ylast - 9):ylast  # historical 10 pre-projection years
  C_dat <- Data@Cat[x, ind2] # catch data from last 5 years
  C_bar <- mean(C_dat, na.rm = TRUE) # mean catch from last 5 years - ignoring CV

  Irecent <- mean(Data@Ind[x, ind], na.rm=TRUE) # mean index in last 5 years
  Iave <- mean(Data@Ind[x, ind3], na.rm=TRUE) # mean index in last 10 years

  Itarget <- Iave * Imulti
  I0 <- 0.8 * Iave
  if (Irecent > I0) {
    TAC <- 0.5 * C_bar * (1 + ((Irecent - I0)/(Itarget - I0)))
  } else {
    TAC <- 0.5 * C_bar * (Irecent/I0)^2
  }

  Rec <- new("Rec")
  Rec@TAC <-  TACfilter(TAC)
  Rec
}
class(Itarg2) <- 'MP'



#' A simple constant catch MP
#'
#' Future TACs are set to the most recent TAC
#'
#' @param x A position in the data object
#' @param Data An object of class `Data`
#' @param reps The number of replicates to include
#'
#' @return An object of class `Rec` with management recommendations
#' @export
#'
#' @examples
#' ConstC(1, SWOData)
ConstC <- function(x, Data, reps=100) {
  LastTAC <- Data@MPrec[x] # last TAC
  TAC <- rep(LastTAC, reps)
  TAC <- TACfilter(TAC)
  Rec <- new("Rec")
  Rec@TAC <- TAC
  Rec

}
class(ConstC) <- 'MP'

#' A simple constant catch MP using average historical catch
#'
#' Future TACs are set to the most average historical catch
#'
#' @param x A position in the data object
#' @param Data An object of class `Data`
#' @param reps The number of replicates to include
#'
#' @return An object of class `Rec` with management recommendations
#' @export
#'
#' @examples
#' AverageC(1, SWOData)
AverageC <- function(x, Data, reps=100) {

  ind <- which(Data@Year == Data@LHYear)
  histCatch <- Data@Cat[x, 1:ind]
  meanC <- mean(histCatch, na.rm=TRUE)
  TAC <- rlnorm(reps, log(meanC), 0.2)

  Rec <- new("Rec")
  Rec@TAC <- TAC
  Rec
}
class(AverageC) <- "MP"


