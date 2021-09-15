
#' Swordfish Performance Metrics functions
#'
#' @param MSEobj An object of class `MSE`
#' @param Ref Reference point used in the performance metrics (e.g., 0.5BMSY)
#' @param Yrs Years the performance metric is calculated over
#'
#' @return An object of class `PM`
#' @name PMs
NULL

#' @describeIn PMs Probability Spawning Biomass is > 0.4SBMSY over first 10 years
#' @export
Safety_S <- function (MSEobj = NULL, Ref = 0.4, Yrs = 10)  {
  Yrs <- ChkYrs(Yrs, MSEobj)
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
  PMobj@Stat <- MSEobj@SB_SBMSY[, , Yrs[1]:Yrs[2]]
  PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
}
class(Safety_S) <- 'PM'


#' @describeIn PMs Probability Spawning Biomass is > 0.4SBMSY over last 20 years
#' @export
Safety_M <- function (MSEobj = NULL, Ref = 0.4, Yrs = c(11,30))  {
  Yrs <- ChkYrs(Yrs, MSEobj)
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
  PMobj@Stat <- MSEobj@SB_SBMSY[, , Yrs[1]:Yrs[2]]
  PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
}
class(Safety_M) <- 'PM'

#' @describeIn PMs Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) over first  10 years
#' @export
Status_S <- function (MSEobj = NULL, Ref = 1, Yrs = 10)  {
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Green Zone of Kobe Space"
  PMobj@Caption <- paste0("Prob. Green Zone of Kobe Space (Years ",
                          Yrs[1], " - ", Yrs[2], ")")

  PMobj@Ref <- Ref
  PMobj@Stat <- MSEobj@SB_SBMSY[, , Yrs[1]:Yrs[2]] > 1 & MSEobj@F_FMSY[, , Yrs[1]:Yrs[2]] < 1
  PMobj@Prob <-calcProb(MSEobj@SB_SBMSY[, , Yrs[1]:Yrs[2]] > 1 & MSEobj@F_FMSY[, , Yrs[1]:Yrs[2]] < 1, MSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
}
class(Status_S) <- 'PM'

#' @describeIn PMs Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) over last  20 years
#' @export
Status_M <- function (MSEobj = NULL, Ref = 1, Yrs =c(11, 30))  {
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "Green Zone of Kobe Space"
  PMobj@Caption <- paste0("Prob. Green Zone of Kobe Space (Years ",
                          Yrs[1], " - ", Yrs[2], ")")

  PMobj@Ref <- Ref
  PMobj@Stat <- MSEobj@SB_SBMSY[, , Yrs[1]:Yrs[2]] > 1 & MSEobj@F_FMSY[, , Yrs[1]:Yrs[2]] < 1
  PMobj@Prob <-calcProb(MSEobj@SB_SBMSY[, , Yrs[1]:Yrs[2]] > 1 & MSEobj@F_FMSY[, , Yrs[1]:Yrs[2]] < 1, MSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
}
class(Status_M) <- 'PM'

#' @describeIn PMs Average Annual Variability in Yield
#' @export
Stability <- function (MSEobj = NULL, Ref = 1, Yrs =NULL)  {
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- paste0("Average Annual Variability in Yield (Years ",
                       Yrs[1], "-", Yrs[2], ")")
  PMobj@Caption <- paste0("Average Annual Variability in Yield (Years ",
                          Yrs[1], "-", Yrs[2], ")")
  y1 <- Yrs[1]:(Yrs[2] - 1)
  y2 <- (Yrs[1] + 1):Yrs[2]
  if (MSEobj@nMPs > 1) {
    AAVY <- apply(((((MSEobj@Catch[, , y1] - MSEobj@Catch[,
                                                          , y2])/MSEobj@Catch[, , y2])^2)^0.5), c(1, 2), mean)
  } else {
    AAVY <- array(apply(((((MSEobj@Catch[, 1, y1] - MSEobj@Catch[,
                                                                 1, y2])/MSEobj@Catch[, 1, y2])^2)^0.5), 1, mean))
  }
  AAVY[AAVY>1] <- 1
  PMobj@Stat <- AAVY
  PMobj@Ref <- Ref
  PMobj@Prob <- AAVY
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSEobj@MPs
  PMobj
}
class(Stability) <- 'PM'

#' @describeIn PMs Average Yield in first 10 years
#' @export
Yield_S <- function(MSEobj=NULL, Ref=1, Yrs=10) {
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- paste0("Mean Yield  (Years ", Yrs[1], "-", Yrs[2], ")")
  PMobj@Caption <- paste0("Mean Yield (Years ", Yrs[1], "-", Yrs[2], ")")

  RefYd <- array(MSEobj@OM$RefY, dim=dim(MSEobj@Catch[,,Yrs[1]:Yrs[2]]))

  PMobj@Stat <- MSEobj@Catch[,,Yrs[1]:Yrs[2]]
  PMobj@Ref <- Ref
  PMobj@Prob <- calcProb(PMobj@Stat, MSEobj) # no probability to calculate

  PMobj@Mean <- calcMean(PMobj@Prob) # calculate mean probability by MP
  PMobj@MPs <- MSEobj@MPs
  PMobj

}
class(Yield_S) <- 'PM'

#' @describeIn PMs Average Yield in last 20 years
#' @export
Yield_M <- function(MSEobj=NULL, Ref=1, Yrs=c(11,30)) {
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- paste0("Mean Yield  (Years ", Yrs[1], "-", Yrs[2], ")")
  PMobj@Caption <- paste0("Mean Yield (Years ", Yrs[1], "-", Yrs[2], ")")

  RefYd <- array(MSEobj@OM$RefY, dim=dim(MSEobj@Catch[,,Yrs[1]:Yrs[2]]))

  PMobj@Stat <- MSEobj@Catch[,,Yrs[1]:Yrs[2]]
  PMobj@Ref <- Ref
  PMobj@Prob <- calcProb(PMobj@Stat, MSEobj) # no probability to calculate

  PMobj@Mean <- calcMean(PMobj@Prob) # calculate mean probability by MP
  PMobj@MPs <- MSEobj@MPs
  PMobj

}
class(Yield_M) <- 'PM'

