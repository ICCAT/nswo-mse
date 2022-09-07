
#' Swordfish Performance Metrics functions
#'
#' @param MMSEobj An object of class `MMSE`
#' @param Ref Reference point used in the performance metrics (e.g., 0.5BMSY)
#' @param Yrs Years the performance metric is calculated over
#'
#' @return An object of class `PM`
#' @name PMs
NULL

#' @describeIn PMs Probability Female Spawning Biomass is > 0.4SBMSY over first 10 years
#' @export
Safety_S <- function (MMSEobj = NULL, Ref = 0.4, Yrs = 10)  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Spawning Biomass relative to SBMSY"
  if (Ref != 1) {
    PMobj@Caption <- paste0("Prob. SB > ", Ref, " SBMSY (Years ",
                            Yrs[1], " - ", Yrs[2], ")")
  } else {
    PMobj@Caption <- paste0("Prob. SB > SBMSY (Years ",
                            Yrs[1], " - ", Yrs[2], ")")
  }
  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]]
  PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MMSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(Safety_S) <- 'PM'


#' @describeIn PMs Probability Female Spawning Biomass is > 0.4SBMSY over last 20 years
#' @export
Safety_M <- function (MMSEobj = NULL, Ref = 0.4, Yrs = c(11,30))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Spawning Biomass relative to SBMSY"
  if (Ref != 1) {
    PMobj@Caption <- paste0("Prob. SB > ", Ref, " SBMSY (Years ",
                            Yrs[1], " - ", Yrs[2], ")")
  }
  else {
    PMobj@Caption <- paste0("Prob. SB > SBMSY (Years ",
                            Yrs[1], " - ", Yrs[2], ")")
  }
  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1, , Yrs[1]:Yrs[2]]
  PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MMSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(Safety_M) <- 'PM'

#' @describeIn PMs Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) over first  10 years
#' @export
Status_S <- function (MMSEobj = NULL, Ref = 1, Yrs = 10)  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Green Zone of Kobe Space"
  PMobj@Caption <- paste0("Prob. Green Zone of Kobe Space (Years ",
                          Yrs[1], " - ", Yrs[2], ")")

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]] > 1 & MMSEobj@F_FMSY[, 1,1, , Yrs[1]:Yrs[2]] < 1
  PMobj@Prob <-calcProb(MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]] > 1 & MMSEobj@F_FMSY[, 1,1,, Yrs[1]:Yrs[2]] < 1, MMSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(Status_S) <- 'PM'

#' @describeIn PMs Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) over last  20 years
#' @export
Status_M <- function (MMSEobj = NULL, Ref = 1, Yrs =c(11, 30))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Green Zone of Kobe Space"
  PMobj@Caption <- paste0("Prob. Green Zone of Kobe Space (Years ",
                          Yrs[1], " - ", Yrs[2], ")")

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]] > 1 & MMSEobj@F_FMSY[, 1,1, , Yrs[1]:Yrs[2]] < 1
  PMobj@Prob <-calcProb(MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]] > 1 & MMSEobj@F_FMSY[, 1,1,, Yrs[1]:Yrs[2]] < 1, MMSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(Status_M) <- 'PM'

#' @describeIn PMs Average Annual Variability in Yield
#' @export
Stability <- function (MMSEobj = NULL, Ref = 1, Yrs =NULL)  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- paste0("Average Annual Variability in Yield (Years ",
                       Yrs[1], "-", Yrs[2], ")")
  PMobj@Caption <- paste0("Average Annual Variability in Yield (Years ",
                          Yrs[1], "-", Yrs[2], ")")
  y1 <- Yrs[1]:(Yrs[2] - 1)
  y2 <- (Yrs[1] + 1):Yrs[2]
  Total_Catch <- apply(MMSEobj@Catch, c(1,4,5), sum)
  if (MMSEobj@nMPs > 1) {
    AAVY <- apply(((((Total_Catch[, , y1] - Total_Catch[, , y2])/Total_Catch[, , y2])^2)^0.5), c(1, 2), mean)
  } else {
    AAVY <- array(apply(((((Total_Catch[, 1, y1] - Total_Catch[, 1, y2])/Total_Catch[, 1, y2])^2)^0.5), 1, mean))
  }
  AAVY[AAVY>1] <- 1
  PMobj@Stat <- AAVY
  PMobj@Ref <- Ref
  PMobj@Prob <- AAVY
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(Stability) <- 'PM'

#' @describeIn PMs Average Yield in first 10 years
#' @export
Yield_S <- function(MMSEobj=NULL, Ref=1, Yrs=10) {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- paste0("Mean Yield  (Years ", Yrs[1], "-", Yrs[2], ")")
  PMobj@Caption <- paste0("Mean Yield (Years ", Yrs[1], "-", Yrs[2], ")")

  Total_Catch <- apply(MMSEobj@Catch, c(1,4,5), sum)
  PMobj@Stat <- Total_Catch[,,Yrs[1]:Yrs[2]]
  PMobj@Ref <- Ref
  PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj) # no probability to calculate

  PMobj@Mean <- calcMean(PMobj@Prob) # calculate mean probability by MP
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(Yield_S) <- 'PM'

#' @describeIn PMs Average Yield in last 20 years
#' @export
Yield_M <- function(MSEobj=NULL, Ref=1, Yrs=c(11,30)) {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- paste0("Mean Yield  (Years ", Yrs[1], "-", Yrs[2], ")")
  PMobj@Caption <- paste0("Mean Yield (Years ", Yrs[1], "-", Yrs[2], ")")

  Total_Catch <- apply(MMSEobj@Catch, c(1,4,5), sum)
  PMobj@Stat <- Total_Catch[,,Yrs[1]:Yrs[2]]
  PMobj@Ref <- Ref
  PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj) # no probability to calculate

  PMobj@Mean <- calcMean(PMobj@Prob) # calculate mean probability by MP
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(Yield_M) <- 'PM'

