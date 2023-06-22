
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
  if (all(abs(diff(vec))<1E-1))
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

#' @describeIn PMs Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) in Years 1-10 (2024-2033)
#' @family Status
#' @export
PGK_short <- function (MMSEobj = NULL, Ref = 1, Yrs = c(4,13))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "PKG_short: Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) in Years 1-10 (2024-2033)"
  PMobj@Caption <- "Prob. Green Zone of Kobe Space (2024-2033)"

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
class(PGK_short) <- 'PM'


# #' @describeIn PMs Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) in Years 6-10 (2029-2033)
# #' @family Status
# #' @export
# PGK_6_10 <- function (MMSEobj = NULL, Ref = 1, Yrs = c(9,13))  {
#   if(!inherits(MMSEobj,'MMSE'))
#     stop('This PM method is designed for objects of class `MMSE`')
#   Yrs <- ChkYrs(Yrs, MMSEobj)
#   PMobj <- new("PMobj")
#   PMobj@Name <- "PKG_6_10: Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) in Years 6-10 (2029-2033)"
#   PMobj@Caption <- "Prob. Green Zone of Kobe Space (2029-2033)"
#
#   PMobj@Ref <- Ref
#   tt <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]] > 1 & MMSEobj@F_FMSY[, 1,1, , Yrs[1]:Yrs[2]] < 1
#   if (is.null(dim(tt)))
#     tt <- matrix(tt, nrow=MMSEobj@nsim, ncol=1)
#   PMobj@Stat <- tt
#   PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj)
#   PMobj@Mean <- calcMean(PMobj@Prob)
#   PMobj@MPs <- MMSEobj@MPs[[1]]
#   PMobj
#
# }
# class(PGK_6_10) <- 'PM'
#

#' @describeIn PMs Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) in Years 11-20 (2034-2043)
#' @family Status
#' @export
PGK_med <- function (MMSEobj = NULL, Ref = 1, Yrs = c(14,23))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "PKG_med: Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) in Years 11-20 (2034-2043)"
  PMobj@Caption <- "Prob. Green Zone of Kobe Space (2034-2043)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]] > 1 & MMSEobj@F_FMSY[, 1,1, , Yrs[1]:Yrs[2]] < 1
  PMobj@Prob <- calcProb(MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]] > 1 & MMSEobj@F_FMSY[, 1,1,, Yrs[1]:Yrs[2]] < 1, MMSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(PGK_med) <- 'PM'


#' @describeIn PMs Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) in Years 21-30 (2044-2053)
#' @family Status
#' @export
PGK_long <- function (MMSEobj = NULL, Ref = 1, Yrs = c(24,33))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "PGK_long: Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) in Years 21-30 (2044-2053)"
  PMobj@Caption <- "Prob. Green Zone of Kobe Space (2044-2053)"

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

#' @describeIn PMs Probability of not breaching the limit reference point (SSB>0.4SSB_MSY) in any of the first 10 years (2024-2033)
#' @family Safety
#' @export
nLRP_short <- function (MMSEobj = NULL, Ref = 0.4, Yrs = c(4,13))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- "nLRP_short: Probability of not breaching the limit reference point (SSB>0.4SSB_MSY) in any of the first 10 years (2024-2033)"
  PMobj@Caption <- "Prob. SB > 0.4SBMSY (2024-2033)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]]

  PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MMSEobj)
  Prob  <- array(as.logical(PMobj@Prob), dim=dim(PMobj@Prob))
  PMobj@Mean <- colSums(Prob)/nrow(Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(nLRP_short) <- 'PM'


#' @describeIn PMs Probability of breaching the limit reference point (SSB<0.4SSB_MSY) in any of years 11-20 (2034-2043)
#' @family Safety
#' @export
LRP_med <- function (MMSEobj = NULL, Ref = 0.4, Yrs = c(14,23))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- "LRP_short: Probability of breaching the limit reference point (SSB<0.4SSB_MSY) in any of years 11-20 (2034-2043)"
  PMobj@Caption <- "Prob. SB < 0.4SBMSY (2034-2043)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]]

  PMobj@Prob <- calcProb(PMobj@Stat < PMobj@Ref, MMSEobj)
  Prob  <- array(as.logical(PMobj@Prob), dim=dim(PMobj@Prob))
  PMobj@Mean <- colSums(Prob)/nrow(Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(LRP_med) <- 'PM'

#' @describeIn PMs Probability of not breaching the limit reference point (SSB>0.4SSB_MSY) in any of years 11-20 (2034-2043)
#' @family Safety
#' @export
nLRP_med <- function (MMSEobj = NULL, Ref = 0.4, Yrs = c(14,23))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- "nLRP_med: Probability of not breaching the limit reference point (SSB>0.4SSB_MSY) in any of years 11-20 (2034-2043)"
  PMobj@Caption <- "Prob. of not breaching LRP (2034-2043)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]]

  PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MMSEobj)
  Prob  <- array(as.logical(PMobj@Prob), dim=dim(PMobj@Prob))
  PMobj@Mean <- colSums(Prob)/nrow(Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(nLRP_med) <- 'PM'




#' @describeIn PMs Probability of breaching the limit reference point (SSB<0.4SSB_MSY) in any of years 21-30 (2044-2053)
#' @family Safety
#' @export
LRP_long <- function (MMSEobj = NULL, Ref = 0.4, Yrs = c(24,33))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- "LRP_long: Probability of breaching the limit reference point (SSB<0.4SSB_MSY) n any of years 21-30 (2044-2053))"
  PMobj@Caption <- "Prob. SB < 0.4SBMSY (2044-2053)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]]

  PMobj@Prob <- calcProb(PMobj@Stat < PMobj@Ref, MMSEobj)
  Prob  <- array(as.logical(PMobj@Prob), dim=dim(PMobj@Prob))
  PMobj@Mean <- colSums(Prob)/nrow(Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(LRP_long) <- 'PM'

#' @describeIn PMs Probability of not breaching the limit reference point (SSB>0.4SSB_MSY) in any of years 21-30 (2044-2053)
#' @family Safety
#' @export
nLRP_long <- function (MMSEobj = NULL, Ref = 0.4, Yrs = c(24,33))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- "nLRP_long: Probability of not breaching the limit reference point (SSB>0.4SSB_MSY) n any of years 21-30 (2044-2053))"
  PMobj@Caption <- "Prob. of not breaching LRP (2044-2053)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]]

  PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MMSEobj)
  Prob  <- array(as.logical(PMobj@Prob), dim=dim(PMobj@Prob))
  PMobj@Mean <- colSums(Prob)/nrow(Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(nLRP_long) <- 'PM'



#' @describeIn PMs Probability of breaching the limit reference point (SSB<0.4SSB_MSY) in any year (2024-2053)
#' @family Safety
#' @export
LRP <- function (MMSEobj = NULL, Ref = 0.4, Yrs = c(4,33))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- "LRP: Probability of breaching the limit reference point (SSB<0.4SSB_MSY) over all years (2024-2053)"
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

#' @describeIn PMs Probability of not breaching the limit reference point (SSB>0.4SSB_MSY) in any year (2024-2053)
#' @family Safety
#' @export
nLRP <- function (MMSEobj = NULL, Ref = 0.4, Yrs = c(4,33))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- "LRP: Probability of not breaching the limit reference point (SSB>0.4SSB_MSY) over all years (2024-2053)"
  PMobj@Caption <- "Prob. of not breaching LRP (2024-2053)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]]

  PMobj@Prob <- calcProb(PMobj@Stat < PMobj@Ref, MMSEobj)
  Prob  <- array(as.logical(PMobj@Prob), dim=dim(PMobj@Prob))
  PMobj@Mean <- 1-colSums(Prob)/nrow(Prob)
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


#' @describeIn PMs Median TAC (t) over years 1-10 (2024-2033)
#' @family Yield
#' @export
AvTAC_short <- function(MMSEobj=NULL, Ref=NULL, Yrs=c(4,13)) {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- 'AvC10: Median TAC (t) over years 1-10'
  PMobj@Caption <- 'Median TAC (t) 2024 - 2033'

  Stat_y <- apply(MMSEobj@TAC[,,,,Yrs[1]:Yrs[2], drop=FALSE], c(1,4,5), sum)

  PMobj@Stat <- apply(Stat_y, c(1,2), median)
  PMobj@Ref <- 1
  PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj) # no probability to calculate

  PMobj@Mean <- apply(Stat_y, 2, median)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(AvTAC_short) <- 'PM'

#' @describeIn PMs Median TAC relative to MSY over years 1-10 (2024-2033)
#' @family Yield
#' @export
rAvTAC_short <- function(MMSEobj=NULL, Ref=NULL, Yrs=c(4,13)) {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- 'AvC10: Median relative TAC over years 1-10'
  PMobj@Caption <- 'Median relative TAC 2024 - 2033'

  Stat_y <- apply(MMSEobj@TAC[,,,,Yrs[1]:Yrs[2], drop=FALSE], c(1,4,5), sum)

  MSYs <- apply(MMSEobj@RefPoint$ByYear$MSY, c(1,3,4), sum)
  nyears <- MMSEobj@nyears
  MSYs <- MSYs[,,nyears+Yrs[1]:Yrs[2]]

  PMobj@Stat <- apply(Stat_y/MSYs, c(1,2), median)
  PMobj@Ref <- 1
  PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj) # no probability to calculate

  PMobj@Mean <- apply(Stat_y/MSYs, 2, median)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(rAvTAC_short) <- 'PM'


#' @describeIn PMs Median TAC (t) over years 11-20 (2034-2043)
#' @family Yield
#' @export
AvTAC_med <- function(MMSEobj=NULL, Ref=1, Yrs=c(14,23)) {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- 'AvC10: Median TAC (t) over years 11-20'
  PMobj@Caption <- 'Median TAC (t) (2034 - 2043)'

  Stat_y <- apply(MMSEobj@TAC[,,,,Yrs[1]:Yrs[2], drop=FALSE], c(1,4,5), sum)
  PMobj@Stat <- apply(Stat_y, c(1,2), median)
  PMobj@Ref <- Ref
  PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj) # no probability to calculate

  PMobj@Mean <- apply(Stat_y, 2, median)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(AvTAC_med) <- 'PM'

#' @describeIn PMs Median TAC relative to MSY over years 11-20 (2034-2043)
#' @family Yield
#' @export
rAvTAC_med <- function(MMSEobj=NULL, Ref=1, Yrs=c(14,23)) {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- 'AvC10: Median relative TAC over years 11-20'
  PMobj@Caption <- 'Median relative TAC 2034 - 2043'

  Stat_y <- apply(MMSEobj@TAC[,,,,Yrs[1]:Yrs[2], drop=FALSE], c(1,4,5), sum)

  MSYs <- apply(MMSEobj@RefPoint$ByYear$MSY, c(1,3,4), sum)
  nyears <- MMSEobj@nyears
  MSYs <- MSYs[,,nyears+Yrs[1]:Yrs[2]]

  PMobj@Stat <- apply(Stat_y/MSYs, c(1,2), median)
  PMobj@Ref <- Ref
  PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj) # no probability to calculate

  PMobj@Mean <- apply(Stat_y/MSYs, 2, median)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(rAvTAC_med) <- 'PM'

#' @describeIn PMs Median TAC (t) over years 21-30 (2034-2053)
#' @family Yield
#' @export
AvTAC_long <- function(MMSEobj=NULL, Ref=1, Yrs=c(24,33)) {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- 'AvC10: Median TAC (t) over years 21-30'
  PMobj@Caption <- 'Median TAC (t) (2044 - 2053)'

  Stat_y <- apply(MMSEobj@TAC[,,,,Yrs[1]:Yrs[2], drop=FALSE], c(1,4,5), sum)
  PMobj@Stat <- apply(Stat_y, c(1,2), median)
  PMobj@Ref <- Ref
  PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj) # no probability to calculate

  PMobj@Mean <- apply(Stat_y, 2, median)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(AvTAC_long) <- 'PM'

#' @describeIn PMs Median TAC relative to MSY over years 21-30 (2034-2053)
#' @family Yield
#' @export
rAvTAC_long <- function(MMSEobj=NULL, Ref=1, Yrs=c(24,33)) {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- 'AvC10: Median relative TAC over years 21-30'
  PMobj@Caption <- 'Median relative TAC 2044 - 2053'

  Stat_y <- apply(MMSEobj@TAC[,,,,Yrs[1]:Yrs[2], drop=FALSE], c(1,4,5), sum)

  MSYs <- apply(MMSEobj@RefPoint$ByYear$MSY, c(1,3,4), sum)
  nyears <- MMSEobj@nyears
  MSYs <- MSYs[,,nyears+Yrs[1]:Yrs[2]]

  PMobj@Stat <- apply(Stat_y/MSYs, c(1,2), median)
  PMobj@Ref <- Ref
  PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj) # no probability to calculate

  PMobj@Mean <- apply(Stat_y/MSYs, 2, median)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(rAvTAC_long) <- 'PM'

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
  PMobj@Caption <- 'Median Variation in TAC (%)'

  TAC <- apply(MMSEobj@TAC[,,,,Yrs[1]:Yrs[2], drop=FALSE], c(1,4,5), sum, na.rm=TRUE)

  # get management cycle
  nMPs <- MMSEobj@nMPs
  interval <- rep(NA, nMPs)
  for (mm in 1:nMPs) {
    interval[mm] <- suppressWarnings(min(apply(TAC[,mm,], 1, firstChange), na.rm=TRUE))
  }

  AAVY <- array(0, dim=c(MMSEobj@nsim, nMPs))

  yrs <- seq_along(Yrs[1]:Yrs[2])
  for (mm in 1:nMPs) {
    change_yrs <- seq(1, by=interval[mm], to=max(yrs))

    y1 <- change_yrs[1:(length(change_yrs)-1)]
    y2 <- change_yrs[2:length(change_yrs)]
    if (interval[mm]==Inf) {
      AAVY[,mm] <- 0
    } else {
      AAVY[,mm] <- apply(((((TAC[, mm, y2] - TAC[, mm, y1])/TAC[,mm , y1])^2)^0.5), 1, median, na.rm=TRUE)
    }
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
  nMPs <- MMSEobj@nMPs
  interval <- rep(NA, nMPs)
  for (mm in 1:nMPs) {
    interval[mm] <- suppressWarnings(min(apply(TAC[,mm,], 1, firstChange), na.rm=TRUE))
  }

  AAVY <- array(0, dim=c(MMSEobj@nsim, nMPs))

  yrs <- seq_along(Yrs[1]:Yrs[2])
  for (mm in 1:nMPs) {
    change_yrs <- seq(1, by=interval[mm], to=max(yrs))

    y1 <- change_yrs[1:(length(change_yrs)-1)]
    y2 <- change_yrs[2:length(change_yrs)]
    if (interval[mm]==Inf) {
      AAVY[,mm] <- 0
    } else {
      AAVY[,mm] <- apply(((((TAC[, mm, y2] - TAC[, mm, y1])/TAC[,mm , y1])^2)^0.5), 1, max, na.rm=TRUE)
    }

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
#' @param msg Logical. Print messages?
#' @return A `data.frame`
#' @export
#'
PM_table <- function(MMSE, PMs=NULL, msg=TRUE) {
  if (is.null(PMs))
    PMs <- avail('PM', 'SWOMSE')

  PMlist <- list()
  for (i in seq_along(PMs)) {
    fun <- try(get(PMs[i]), silent = TRUE)
    if (inherits(fun, 'try-error'))
      stop(PMs[i], ' not a valid function of class `PM`')
    if (class(fun)!='PM')
      stop(PMs[i], ' not a valid function of class `PM`')

    PMlist[[i]] <- fun
  }


  PM_Values <- list()
  for (i in seq_along(PMs)) {
    nm <- PMs[i]
    if (msg) message('Calculating: ', nm)
    MPs <- MMSE@MPs[[1]]
    pm <- PMlist[[i]](MMSE)
    val <- pm@Mean
    name <- pm@Name
    caption <- pm@Caption

    PM_Values[[i]] <- data.frame(PM=nm, MP=MPs, Value=val, name=name,
                                 caption=caption)
  }

  df <- do.call(rbind.data.frame, PM_Values) %>%
    rename(Name=PM)

  df %>% filter(Name=='PGK_6_10')

  ######################

  df

}


#' Create a Trade-Off Plot
#'
#' @param MMSE An object of class `MMSE`
#' @param PMs Character length 2. Names of `PM` functions to plot
#' @param xlim Optional. Numeric length 1. Minimum value for x
#' @param ylim Optional. Numeric length 1. Minimum value for y
#' @param vline Optional. Numeric vector for vertical lines
#' @param hline Optional. Numeric vector for horizontal lines
#' @param quants Numeric vector length 2 of quantiles for error bars. Only shown
#' for TAC PMs. Ignored if NULL
#' @param inc.leg logical. Include the legend?
#'
#' @return A `ggplot` object
#' @export
TradeOff <- function(MMSE, PMs, xlim=NULL, ylim=NULL, vline=NULL,
                     hline=NULL, quants=c(0.1, 0.9), inc.leg=TRUE,
                     inc.labels=TRUE, pt.size=3, inc.line=FALSE,
                     subset=FALSE, ymax=1) {

  # Calculate PMs
  if (length(PMs)!=2)
    stop('PMs must be length 2 with PM functions')

  PMlist <- list()
  for (i in seq_along(PMs)) {
    fun <- get(PMs[i])
    PMlist[[i]] <- fun(MMSE)
  }

  # Make data.frame
  Captions <- list()
  PM_val <- list()
  for (i in seq_along(PMs)) {
    Captions[[i]] <- PMlist[[i]]@Caption
    PM_val[[i]] <- PMlist[[i]]@Mean
  }

  df <- data.frame(MP=MMSE@MPs[[1]], PM_val[[1]], PM_val[[2]])
  colnames(df)[2:3] <- c('x', 'y')


  # calculate quantiles
  if (!is.null(quants)) {
    quants_list <- list()

    for (i in seq_along(PMs)) {
      if (grepl('TAC', PMlist[[i]]@Name)) {
        quants_list[[i]] <- t(apply(PMlist[[i]]@Stat, 2, quantile, quants))

      } else {
        quants_list[[i]] <- matrix(NA, nrow=MMSE@nMPs, ncol=2)
        colnames(quants_list[[i]]) <-  paste0(quants*100, '%')
      }
    }

    quant_df <- data.frame(MP=MMSE@MPs[[1]], quants_list[[1]], quants_list[[2]])
    colnames(quant_df)[2:ncol(quant_df)] <- c(paste0('x', c('min', 'max')),
                                              paste0('y', c('min', 'max')))

    pdf <- left_join(df, quant_df, by='MP')
  } else {
    pdf <- df
  }


  # rename MPs
  get_Code <- function(MP) {
    if(!grepl("_", MP)) {
      MP <- MP
      Code <- NA
    } else {
      txt <- strsplit(MP, "_")[[1]]
      MP <- txt[1]
      Code <- txt[2]
    }
    data.frame(MP=MP, Code=Code)
  }

  mat <- lapply(1:nrow(pdf), function(x) get_Code(pdf$MP[x])) %>% do.call('rbind', .)
  pdf$MP <- mat[,1]
  pdf$Code <- mat[,2]
  pdf$Code[is.na(pdf$Code)] <- 'None'
  pdf <- left_join(pdf, TuneTargets, by='Code')
  pdf$Target[is.na(pdf$Target)] <- 'None'
  pdf$Target <- factor(pdf$Target)

  p <- ggplot(pdf)

  if (!is.null(vline))
    p <- p + geom_vline(xintercept = vline, linetype=2, color='darkgray')
  if (!is.null(hline))
    p <- p + geom_hline(yintercept = hline, linetype=2, color='darkgray')

  # limits
  if (!is.null(xlim)) {
    xlimdata <- data.frame(x=c(-Inf, -Inf, xlim, xlim), y=c(-Inf, Inf, Inf, -Inf))
    p <- p +geom_polygon(data=xlimdata, aes(x=x, y=y), fill='red', alpha=0.05)
  }
  if (!is.null(ylim)) {
    ylimdata <- data.frame(x=c(-Inf, -Inf, Inf, Inf), y=c(-Inf, ylim, ylim, -Inf))
    p <- p +geom_polygon(data=ylimdata, aes(x=x, y=y), fill='red', alpha=0.05)
  }

  p <- p +
    geom_point(aes(x=x, y=y, color=MP, shape=Target), size=pt.size)  +
    expand_limits(x=c(0,1), y=c(0, ymax)) +
    scale_shape_manual(values=15:19)





  if (inc.labels) {
    p <- p +
      ggrepel::geom_text_repel(aes(x=x, y=y, label=MP), show.legend = FALSE)
  }


  p <-  p + theme_bw() +
    labs(x=Captions[[1]], y=Captions[[2]],
         shape=unique(pdf$Metric[!is.na(pdf$Metric)])) +
    guides(color='none')

  if (!inc.leg | length(unique(pdf$Code))<2)
    p <-  p + guides(shape='none')

  # add quantiles
  if (!is.null(quants))
    p <- p + geom_errorbar(aes(x=x, ymin=ymin, ymax=ymax, color=MP), alpha=0.5)


  if (subset) {
    if (!is.null(xlim))
      p <- p + xlim(c(xlim*0.95, 1))
    if (!is.null(ylim))
      p <- p + ylim(c(ylim*0.95, 1))
  }

  if (inc.line) {
   p <- p + geom_line(aes(x=x, y=y, group=MP, color=MP))
  }
  p + theme(axis.title=element_text(size=15))
}

