
#' Swordfish Performance Metrics functions
#'
#' @param MMSEobj An object of class `MMSE`
#' @param Ref Reference point used in the performance metrics (e.g., 0.5BMSY)
#' @param Yrs Years the performance metric is calculated over
#'
#' @return An object of class `PM`
#' @name PMs
NULL

calcMedian <- function (Prob) {
  if ("matrix" %in% class(Prob))
    return(apply(Prob, 2, median, na.rm = TRUE))
  if ("numeric" %in% class(Prob))
    return(median(Prob, na.rm = TRUE))
}

calcMax <- function (Prob) {
  if ("matrix" %in% class(Prob))
    return(apply(Prob, 2, max, na.rm = TRUE))
  if ("numeric" %in% class(Prob))
    return(max(Prob, na.rm = TRUE))
}


firstChange <- function(vec) {
  ll <- length(vec)-1
  if (all(diff(vec)<1E-1))
    return(NA)
  for (i in 1:ll) {
    if (abs(vec[i]-vec[i+1]) > 0.1)
      break()
  }
  i
}

# ProjYears <- data.frame(Index1=1:33, Index2=-2:30, Year=2021:2053)


is_GK <- function(x, f, b) {
  nMP <- dim(f)[2]
  out <- (f[x,] < 1 & b[x,] > 1)
  out
}

## Status ----

#' @describeIn PMs Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) in Year 10 (2033)
#' @family Status
#' @export
PGK_10 <- function (MMSEobj = NULL, Ref = 1, Yrs = c(13,13))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "PKG_short: Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) in Year 10 (2029-2033)"
  PMobj@Caption <- "Prob. Green Zone of Kobe Space (2033)"

  PMobj@Ref <- Ref
  tt <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]] > 1 & MMSEobj@F_FMSY[, 1,1, , Yrs[1]:Yrs[2]] < 1
  if (is.null(dim(tt)))
    tt <- matrix(tt, nrow=MMSEobj@nsim, ncol=1)
  PMobj@Stat <- tt
  PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj

}
class(PGK_10) <- 'PM'



#' @describeIn PMs Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) in Years 6-10 (2029-2033)
#' @family Status
#' @export
PGK_med <- function (MMSEobj = NULL, Ref = 1, Yrs = c(9,13))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "PKG_short: Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) in Years 6-10 (2029-2033)"
  PMobj@Caption <- "Prob. Green Zone of Kobe Space (2029-2033)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]] > 1 & MMSEobj@F_FMSY[, 1,1, , Yrs[1]:Yrs[2]] < 1
  PMobj@Prob <- calcProb(MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]] > 1 & MMSEobj@F_FMSY[, 1,1,, Yrs[1]:Yrs[2]] < 1, MMSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(PGK_med) <- 'PM'


#' @describeIn PMs Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) in Years 11-30 (2034-2053)
#' @family Status
#' @export
PGK_long <- function (MMSEobj = NULL, Ref = 1, Yrs = c(14,33))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "PGK_long: Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) in Years 11-30 (2034-2053)"
  PMobj@Caption <- "Prob. Green Zone of Kobe Space (2034-2053)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]] > 1 & MMSEobj@F_FMSY[, 1,1, , Yrs[1]:Yrs[2]] < 1
  PMobj@Prob <- calcProb(MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]] > 1 & MMSEobj@F_FMSY[, 1,1,, Yrs[1]:Yrs[2]] < 1, MMSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(PGK_long) <- 'PM'


#' @describeIn PMs Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) over all years (2024-2053)
#' @family Status
#' @export
PGK <- function (MMSEobj = NULL, Ref = 1, Yrs = c(4,33))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "PGK: Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) over all years (2024-2053)"
  PMobj@Caption <- "Prob. Green Zone of Kobe Space (2024-2053)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]] > 1 & MMSEobj@F_FMSY[, 1,1, , Yrs[1]:Yrs[2]] < 1
  PMobj@Prob <- calcProb(MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]] > 1 & MMSEobj@F_FMSY[, 1,1,, Yrs[1]:Yrs[2]] < 1, MMSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(PGK) <- 'PM'


#' @describeIn PMs Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) in Year 30 (2053)
#' @family Status
#' @export
PGK_30 <- function (MMSEobj = NULL, Ref = 1, Yrs = c(33,33))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "PGK_30: Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY)  in Year 30 (2053)"
  PMobj@Caption <- "Prob. Green Zone of Kobe Space (2023)"

  PMobj@Ref <- Ref

  tt <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]] > 1 & MMSEobj@F_FMSY[, 1,1, , Yrs[1]:Yrs[2]] < 1
  if (is.null(dim(tt)))
    tt <- matrix(tt, nrow=MMSEobj@nsim, ncol=1)
  PMobj@Stat <- tt
  PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(PGK_30) <- 'PM'

#' @describeIn PMs Probability of Overfishing (F>FMSY) over all years (2024-2053)
#' @family Status
#' @export
POF <- function (MMSEobj = NULL, Ref = 1, Yrs = c(4,33))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "PNOF: Probability of Overfishing (F>FMSY) over all years (2024-2053)"
  PMobj@Caption <- "Prob. Overfishing (F>FMSY) (2024-2053)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@F_FMSY[, 1,1, , Yrs[1]:Yrs[2]] > 1
  PMobj@Prob <- calcProb(MMSEobj@F_FMSY[, 1,1,, Yrs[1]:Yrs[2]] > 1, MMSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(POF) <- 'PM'


#' @describeIn PMs Probability of Not Overfishing (F<FMSY) over all years (2024-2053)
#' @family Status
#' @export
PNOF <- function (MMSEobj = NULL, Ref = 1, Yrs = c(4,33))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "PNOF: Probability of Not Overfishing (F<FMSY) over all years (2024-2053)"
  PMobj@Caption <- "Prob. Not Overfishing (F<FMSY) (2024-2053)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@F_FMSY[, 1,1, , Yrs[1]:Yrs[2]] < 1
  PMobj@Prob <- calcProb(MMSEobj@F_FMSY[, 1,1,, Yrs[1]:Yrs[2]] < 1, MMSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(PNOF) <- 'PM'



## Safety ----

#' @describeIn PMs Probability of breaching the limit reference point (SSB<0.4SSB_MSY) in any of the first 10 years (2024-2033)
#' @family Safety
#' @export
LRP_short <- function (MMSEobj = NULL, Ref = 0.4, Yrs = c(4,13))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- "LRP_short: Probability of breaching the limit reference point (SSB<0.4SSB_MSY) in any of the first 10 years (2024-2033)"
  PMobj@Caption <- "Prob. SB < 0.4SBMSY (2024-2033)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]]

  PMobj@Prob <- calcProb(PMobj@Stat < PMobj@Ref, MMSEobj)
  Prob  <- array(as.logical(PMobj@Prob), dim=dim(PMobj@Prob))
  PMobj@Mean <- colSums(Prob)/nrow(Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(LRP_short) <- 'PM'


#' @describeIn PMs Probability of breaching the limit reference point (SSB<0.4SSB_MSY) in any of years 11-30 (2034-2053)
#' @family Safety
#' @export
LRP_long <- function (MMSEobj = NULL, Ref = 0.4, Yrs = c(14,33))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- "LRP_short: Probability of breaching the limit reference point (SSB<0.4SSB_MSY) n any of years 11-30 (2034-2053))"
  PMobj@Caption <- "Prob. SB < 0.4SBMSY (2034-2053)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]]

  PMobj@Prob <- calcProb(PMobj@Stat < PMobj@Ref, MMSEobj)
  Prob  <- array(as.logical(PMobj@Prob), dim=dim(PMobj@Prob))
  PMobj@Mean <- colSums(Prob)/nrow(Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(LRP_long) <- 'PM'

#' @describeIn PMs Probability of breaching the limit reference point (SSB<0.4SSB_MSY) in any year (2024-2053)
#' @family Safety
#' @export
LRP <- function (MMSEobj = NULL, Ref = 0.4, Yrs = c(4,33))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- "LRP_short: Probability of breaching the limit reference point (SSB<0.4SSB_MSY) over all years (2024-2053)"
  PMobj@Caption <- "Prob. SB < 0.4SBMSY (2024-2053)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]]

  PMobj@Prob <- calcProb(PMobj@Stat < PMobj@Ref, MMSEobj)
  Prob  <- array(as.logical(PMobj@Prob), dim=dim(PMobj@Prob))
  PMobj@Mean <- colSums(Prob)/nrow(Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(LRP) <- 'PM'



#' @describeIn PMs Probability of not breaching the limit reference point (SSB>0.4SSB_MSY) in any of the first 10 years (2024-2033)
#' @family Safety
#' @export
nLRP_short <- function (MMSEobj = NULL, Ref = 0.4, Yrs = c(4,13))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- "LRP_short: Probability of not breaching the limit reference point (SSB>0.4SSB_MSY) in any of the first 10 years (2024-2033)"
  PMobj@Caption <- "Prob. SB > 0.4SBMSY (2024-2033)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]]

  PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MMSEobj)
  Prob  <- array(PMobj@Prob==1, dim=dim(PMobj@Prob))
  PMobj@Mean <- colSums(Prob)/nrow(Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(nLRP_short) <- 'PM'


#' @describeIn PMs Probability of not breaching the limit reference point (SSB>0.4SSB_MSY) in any of years 11-30 (2034-2053)
#' @family Safety
#' @export
nLRP_long <- function (MMSEobj = NULL, Ref = 0.4, Yrs = c(14,33))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- "LRP_short: Probability ofnot  breaching the limit reference point (SSB>0.4SSB_MSY) n any of years 11-30 (2034-2053))"
  PMobj@Caption <- "Prob. SB > 0.4SBMSY (2034-2053)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]]

  PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MMSEobj)
  Prob  <- array(PMobj@Prob==1, dim=dim(PMobj@Prob))
  PMobj@Mean <- colSums(Prob)/nrow(Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(nLRP_long) <- 'PM'

#' @describeIn PMs Probability of not breaching the limit reference point (SSB>0.4SSB_MSY) in any year (2024-2053)
#' @family Safety
#' @export
nLRP <- function (MMSEobj = NULL, Ref = 0.4, Yrs = c(4,33))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- "LRP_short: Probability of not breaching the limit reference point (SSB>0.4SSB_MSY) over all years (2024-2053)"
  PMobj@Caption <- "Prob. SB > 0.4SBMSY (2024-2053)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]]

  PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MMSEobj)
  Prob  <- array(PMobj@Prob==1, dim=dim(PMobj@Prob))
  PMobj@Mean <- colSums(Prob)/nrow(Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(nLRP) <- 'PM'

## Yield ----

#' @describeIn PMs TAC in the First Implementation Year (2024)
#' @family Yield
#' @export
TAC1 <- function(MMSEobj=NULL, Ref=1, Yrs=c(4,4)) {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- 'C1: Median TAC in First Year'
  PMobj@Caption <- 'Median TAC in 2024'

  PMobj@Stat <- apply(MMSEobj@TAC[,,,,Yrs[1]:Yrs[2], drop=FALSE], c(1,4), sum)
  PMobj@Ref <- Ref
  PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj) # no probability to calculate

  PMobj@Mean <- calcMedian(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(TAC1) <- 'PM'



#' @describeIn PMs Median catches (t) over years 1-10 (2024-2033)
#' @family Yield
#' @export
AvC10 <- function(MMSEobj=NULL, Ref=NULL, Yrs=c(4,13)) {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)


  PMobj <- new("PMobj")
  PMobj@Name <- 'AvC10: Median catches (t) over years 1-10'
  PMobj@Caption <- 'Median catch (t) 2024 - 2033'

  PMobj@Stat <- apply(MMSEobj@TAC[,,,,Yrs[1]:Yrs[2], drop=FALSE], c(1,4), sum)
  PMobj@Ref <- 1
  PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj) # no probability to calculate

  PMobj@Mean <- calcMedian(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(AvC10) <- 'PM'


#' @describeIn PMs Median TAC (t) over years 1-10 (2024-2033)
#' @family Yield
#' @export
AvTAC10 <- function(MMSEobj=NULL, Ref=NULL, Yrs=c(4,13)) {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- 'AvC10: Median TAC (t) over years 1-10'
  PMobj@Caption <- 'Median TAC (t) 2024 - 2033'

  PMobj@Stat <- apply(MMSEobj@TAC[,,,,Yrs[1]:Yrs[2], drop=FALSE], c(1,4), sum)
  PMobj@Ref <- 1
  PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj) # no probability to calculate

  PMobj@Mean <- calcMedian(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(AvTAC10) <- 'PM'



#' @describeIn PMs Median catches (t) over years 11-30 (2034-2053)
#' @family Yield
#' @export
AvC30 <- function(MMSEobj=NULL, Ref=1, Yrs=c(14,33)) {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- 'AvC10: Median catches (t) over years 11-30'
  PMobj@Caption <- 'Median catch (t) 2034 - 2053'

  PMobj@Stat <- apply(MMSEobj@Catch[,,,,Yrs[1]:Yrs[2], drop=FALSE], c(1,4), sum)
  PMobj@Ref <- Ref
  PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj) # no probability to calculate

  PMobj@Mean <- calcMedian(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(AvC30) <- 'PM'

#' @describeIn PMs Median TAC (t) over years 11-30 (2034-2053)
#' @family Yield
#' @export
AvTAC30 <- function(MMSEobj=NULL, Ref=1, Yrs=c(14,33)) {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- 'AvC10: Median TAC (t) over years 11-30'
  PMobj@Caption <- 'Median TAC (t) 2034 - 2053'

  PMobj@Stat <- apply(MMSEobj@Catch[,,,,Yrs[1]:Yrs[2], drop=FALSE], c(1,4), sum)
  PMobj@Ref <- Ref
  PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj) # no probability to calculate

  PMobj@Mean <- calcMedian(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(AvTAC30) <- 'PM'



## Stability ----

#' @describeIn PMs Median variation in TAC (\%) between management cycles over all years
#' @family Stability
#' @export
VarC <- function (MMSEobj = NULL, Ref=1, Yrs=c(4,33))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- 'VarC: Median Variation in TAC (%) between management cycles'
  PMobj@Caption <- 'Median Variation in TAC (%) between management cycles'

  TAC <- apply(MMSEobj@TAC[,,,,Yrs[1]:Yrs[2], drop=FALSE], c(1,4,5), sum, na.rm=TRUE)

  # get management cycle
  interval <- min(apply(TAC[,1,], 1, firstChange), na.rm=TRUE)

  yrs <- seq_along(Yrs[1]:Yrs[2])
  change_yrs <- seq(1, by=interval, to=max(yrs))

  y1 <- change_yrs[1:(length(change_yrs)-1)]
  y2 <- change_yrs[2:length(change_yrs)]

  if (MMSEobj@nMPs > 1) {
    AAVY <- apply(((((TAC[, , y2] - TAC[, , y1])/TAC[, , y1])^2)^0.5), c(1, 2), median, na.rm=TRUE)
  } else {
    AAVY <- array(apply(((((TAC[, 1, y2] - TAC[, 1, y1])/TAC[, 1, y1])^2)^0.5), 1, median, na.rm=TRUE))
  }

  PMobj@Stat <- AAVY
  PMobj@Ref <- Ref
  PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj)
  PMobj@Mean <- calcMedian(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(VarC) <- 'PM'


#' @describeIn PMs Maximum variation in TAC (\%) between management cycles over all years
#' @family Stability
#' @export
MaxVarC <- function (MMSEobj = NULL, Ref=1, Yrs=c(4,33))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- 'MaxVarC: Maximum Variation in TAC (%) between management cycles'
  PMobj@Caption <- 'Maximum Variation in TAC (%) between management cycles'

  TAC <- apply(MMSEobj@TAC[,,,,Yrs[1]:Yrs[2], drop=FALSE], c(1,4,5), sum, na.rm=TRUE)

  # get management cycle
  interval <- min(apply(TAC[,1,], 1, firstChange), na.rm=TRUE)

  yrs <- seq_along(Yrs[1]:Yrs[2])
  change_yrs <- seq(1, by=interval, to=max(yrs))

  y1 <- change_yrs[1:(length(change_yrs)-1)]
  y2 <- change_yrs[2:length(change_yrs)]

  if (MMSEobj@nMPs > 1) {
    AAVY <- apply(((((TAC[, , y2] - TAC[, , y1])/TAC[, , y1])^2)^0.5), c(1, 2), max)
  } else {
    AAVY <- array(apply(((((TAC[, 1, y2] - TAC[, 1, y1])/TAC[, 1, y1])^2)^0.5), 1, max))
  }

  PMobj@Stat <- AAVY
  PMobj@Ref <- Ref
  PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj)
  PMobj@Mean <- calcMax(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(MaxVarC) <- 'PM'


#' Calculate Performance Metrics
#'
#' @param MMSE An object of class `MMSE`
#' @param PMs Optional. Names of `PM` functions to use. Otherwise all available PMs will be calculated.
#'
#' @return A `data.frame`
#' @export
#'
PM_table <- function(MMSE, PMs=NULL) {
  if (is.null(PMs))
    PMs <- PM_desc$Name

  PMlist <- list()
  for (i in seq_along(PMs)) {
    fun <- try(get(PMs[i]), silent = TRUE)
    if (inherits(fun, 'try-error'))
      stop(PMs[i], ' not a valid function of class `PM`')
    if (class(fun)!='PM')
      stop(PMs[i], ' not a valid function of class `PM`')

    PMlist[[PMs[i]]] <- fun
  }

  PM_Values <- list()
  for (i in seq_along(PMs)) {
    nm <- PMs[i]
    message('Calculating: ', nm)
    MPs <- MMSE@MPs[[1]]
    pm <- PMlist[[i]](MMSE)
    val <- pm@Mean
    name <- pm@Name
    caption <- pm@Caption

    PM_Values[[i]] <- data.frame(PM=nm, MP=MPs, Value=val, name=name,
                                 caption=caption)
  }

  do.call(rbind.data.frame, PM_Values) %>%
    rename(Name=PM)

}


#' Create a Trade-Off Plot
#'
#' @param PM_vals A data.frame generated by `PM_table`
#' @param PMs Character length 2. Names of `PM` functions to plot
#' @param xlim Optional. Numeric length 1. Minimum value for x
#' @param ylim Optional. Numeric length 1. Minimum value for y
#'
#' @return A `ggplot` object
#' @export
TradeOff <- function(PM_vals, PMs, xlim=NULL, ylim=NULL, vline=NULL,
                     hline=NULL) {

  df <- PM_vals %>% select(MP, Name, Value) %>%  filter(Name %in% PMs) %>%
    tidyr::pivot_wider(., names_from=Name, values_from = Value)

  captions <- paste0(PM_vals$Name[match(PMs, PM_vals$Name)], ': ', PM_vals$caption[match(PMs, PM_vals$Name)])

  p <- ggplot(df)

  if (!is.null(vline))
    p <- p + geom_vline(xintercept = vline, linetype=2, color='darkgray')
  if (!is.null(hline))
    p <- p + geom_hline(yintercept = hline, linetype=2, color='darkgray')

  # limits
  if (!is.null(xlim)) {
    xlimdata <- data.frame(x=c(-Inf, -Inf, xlim, xlim), y=c(-Inf, Inf, Inf, -Inf))
    p <- p +geom_polygon(data=xlimdata, aes(x=x, y=y), fill='red', alpha=0.1)
  }
  if (!is.null(ylim)) {
    ylimdata <- data.frame(x=c(-Inf, -Inf, Inf, Inf), y=c(-Inf, ylim, ylim, -Inf))
    p <- p +geom_polygon(data=ylimdata, aes(x=x, y=y), fill='red', alpha=0.1)
  }
  p <- p +
    geom_point(aes(x=.data[[PMs[1]]], y=.data[[PMs[2]]], color=MP), size=2) +
    expand_limits(x=c(0,1),
                  y=c(0,1)) +
    ggrepel::geom_text_repel(aes(x=.data[[PMs[1]]], y=.data[[PMs[2]]], label=MP), show.legend = FALSE) +
    theme_bw() +
    labs(x=captions[1], y=captions[2])

  p + guides(color='none')
}

