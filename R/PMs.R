
is.dominated <- function(x, y, greater=TRUE, rnd=4) {
  x$Value <- round(x$Value,rnd)
  y$Value <- round(y$Value,rnd)
  if (greater)
    sdf <- x %>% filter(Value>y$Value)
  if (!greater)
    sdf <- x %>% filter(Value<y$Value)
  sdf$MP
}

#' Title
#'
#' @param pms
#' @param PM_results
#'
#' @return
#' @export
Calc_Dominated <- function(pms, PM_results) {
  PM1 <- pms$PM1
  PM2 <- pms$PM2
  greater <- rep(TRUE, 2)
  t1 <- DomPMs %>% filter(PM==PM1)
  greater[1] <- t1$Greater
  t2 <- DomPMs %>% filter(PM==PM2)
  greater[2] <- t2$Greater

  PMs <- c(PM1, PM2)
  df <- PM_results %>% filter(PM %in% PMs)
  MPs <- unique(df$MP)
  dom_MP_list <- list()
  for (mm in seq_along(MPs)) {
    tdf <- df %>% filter(MP==MPs[mm])
    dom_MP_list[[MPs[mm]]] <- list()
    for (pm in 1:2) {
      x <- df %>% filter(PM==PMs[pm])
      y <- tdf %>% filter(PM==PMs[pm])
      dom_MP <- is.dominated(x,y, greater=greater[pm])

      dom_MP_list[[MPs[mm]]][[PMs[[pm]]]] <- dom_MP[!dom_MP==MPs[mm]]
    }
    dom_MP_list[[MPs[mm]]] <- intersect(dom_MP_list[[MPs[mm]]][[1]], dom_MP_list[[MPs[mm]]][[2]] )
  }
  tt <-list(Non=names(which(lapply(dom_MP_list, length)==0)),
            Dom=names(which(lapply(dom_MP_list, length)!=0)))
  tt
}


#' Get Performance Metrics from Tuning Objects
#'
#' @param tune_dir
#' @param tune_PM
#'
#' @return
#' @export
get_tune_PM <- function(tune_dir='Tuning_Objects', tune_PM='PGK_short') {
  fls <- list.files(tune_dir, pattern='.tune')

  MPs <- unlist(strsplit(fls, '.tune'))
  MP_df <- data.frame(Name=MPs, MP=c(paste0(MPs, '_a'), paste0(MPs, '_b'), paste0(MPs, '_c')),
                      Target=rep(c(0.51,0.60, 0.70), each=length(MPs)))

  df_list <- list()
  for (i in 1:nrow(MP_df)) {
    fl <- paste0(MP_df$Name[i], '.tune')
    obj <- readRDS(file.path('Tuning_Objects', fl))
    targ <- MP_df$Target[i]
    tune_val <- obj %>% filter(Name==tune_PM, Target==targ) %>% mutate(t=abs(Value-targ)) %>%
      filter(t==min(t)) %>% distinct(test_vals)

    df <- obj %>% filter(test_vals==tune_val$test_vals, Target==targ) %>% distinct(PM=Name, Value, Target, caption)
    df <- df %>% mutate(Value=ifelse(PM=='PGK_short', Target, Value))
    df$MP <- MP_df$MP[i]
    df$Name <- MP_df$Name[i]
    df_list[[i]] <- df %>% filter(PM!='PMfun')

  }
  DF <- do.call('rbind', df_list)
  DF$Target <- factor(DF$Target)
  DF
}


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



is_GK <- function(x, f, b) {
  nMP <- dim(f)[2]
  out <- (f[x,] < 1 & b[x,] > 1)
  out
}

## Status ----

data.frame(Yr_Ind=1:32,
           Proj_Ind=seq(-1, by=1, length.out=32),
           year=seq(2023, by=1, length.out=32))

#' @describeIn PMs Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) in Years 1-10 (2025-2034)
#' @family Status
#' @export
PGK_short <- function (MMSEobj = NULL, Ref = 1, Yrs = c(3,12))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "PKG_short: Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) in Years 1-10 (2025-2034)"
  PMobj@Caption <- "Prob. Green Zone of Kobe Space (2025-2034)"

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

#' @describeIn PMs Probability of SB>SBMSY over all years
#' @family Status
#' @export
PBMSY <- function (MMSEobj = NULL, Ref = 1, Yrs = c(3,32))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "PBMSY_short: Probability of SB>SBMSY "
  PMobj@Caption <- "Prob. B>BMSY (2025-2054)"

  PMobj@Ref <- Ref
  tt <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]] > 1
  if (is.null(dim(tt)))
    tt <- matrix(tt, nrow=MMSEobj@nsim, ncol=1)
  PMobj@Stat <- tt
  PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj

}
class(PBMSY) <- 'PM'

#' @describeIn PMs Probability of SB>SBMSY in Years 1-10 (2025-2034)
#' @family Status
#' @export
PBMSY_short <- function (MMSEobj = NULL, Ref = 1, Yrs = c(3,12))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "PBMSY_short: Probability of SB>SBMSY in Years 1-10 (2025-2034)"
  PMobj@Caption <- "Prob. B>BMSY (2025-2034)"

  PMobj@Ref <- Ref
  tt <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]] > 1
  if (is.null(dim(tt)))
    tt <- matrix(tt, nrow=MMSEobj@nsim, ncol=1)
  PMobj@Stat <- tt
  PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj

}
class(PBMSY_short) <- 'PM'


#' @describeIn PMs Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) in Years 11-20 (2035-2044)
#' @family Status
#' @export
PGK_med <- function (MMSEobj = NULL, Ref = 1, Yrs = c(13,22))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "PKG_med: Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) in Years 11-20 (2035-2044)"
  PMobj@Caption <- "Prob. Green Zone of Kobe Space (2035-2044)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]] > 1 & MMSEobj@F_FMSY[, 1,1, , Yrs[1]:Yrs[2]] < 1
  PMobj@Prob <- calcProb(MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]] > 1 & MMSEobj@F_FMSY[, 1,1,, Yrs[1]:Yrs[2]] < 1, MMSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(PGK_med) <- 'PM'


#' @describeIn PMs Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) in Years 21-30 (2045-2054)
#' @family Status
#' @export
PGK_long <- function (MMSEobj = NULL, Ref = 1, Yrs = c(23,32))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "PGK_long: Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) in Years 21-30 (2045-2054)"
  PMobj@Caption <- "Prob. Green Zone of Kobe Space (2045-2054)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]] > 1 & MMSEobj@F_FMSY[, 1,1, , Yrs[1]:Yrs[2]] < 1
  PMobj@Prob <- calcProb(MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]] > 1 & MMSEobj@F_FMSY[, 1,1,, Yrs[1]:Yrs[2]] < 1, MMSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(PGK_long) <- 'PM'


#' @describeIn PMs Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) over all years (2025-2054)
#' @family Status
#' @export
PGK <- function (MMSEobj = NULL, Ref = 1, Yrs = c(3,32))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "PGK: Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) over all years (2025-2054)"
  PMobj@Caption <- "Prob. Green Zone of Kobe Space (2025-2054)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]] > 1 & MMSEobj@F_FMSY[, 1,1, , Yrs[1]:Yrs[2]] < 1
  PMobj@Prob <- calcProb(MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]] > 1 & MMSEobj@F_FMSY[, 1,1,, Yrs[1]:Yrs[2]] < 1, MMSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(PGK) <- 'PM'


#' @describeIn PMs Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY) in Year 30 (2054)
#' @family Status
#' @export
PGK_30 <- function (MMSEobj = NULL, Ref = 1, Yrs = c(32,32))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "PGK_30: Probability of being in Green Zone of Kobe Space (SB>SBMSY & F<FMSY)  in Year 30 (2054)"
  PMobj@Caption <- "Prob. Green Zone of Kobe Space (2054)"

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

#' @describeIn PMs Probability of Overfishing (F>FMSY) over all years (2025-2054)
#' @family Status
#' @export
POF <- function (MMSEobj = NULL, Ref = 1, Yrs = c(3,32))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "PNOF: Probability of Overfishing (F>FMSY) over all years (2025-2054)"
  PMobj@Caption <- "Prob. Overfishing (F>FMSY) (2025-2054)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@F_FMSY[, 1,1, , Yrs[1]:Yrs[2]] > 1
  PMobj@Prob <- calcProb(MMSEobj@F_FMSY[, 1,1,, Yrs[1]:Yrs[2]] > 1, MMSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(POF) <- 'PM'


#' @describeIn PMs Probability of Not Overfishing (F<FMSY) over all years (2025-2054)
#' @family Status
#' @export
PNOF <- function (MMSEobj = NULL, Ref = 1, Yrs = c(3,32))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- "PNOF: Probability of Not Overfishing (F<FMSY) over all years (2025-2054)"
  PMobj@Caption <- "Prob. Not Overfishing (F<FMSY) (2025-2054)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@F_FMSY[, 1,1, , Yrs[1]:Yrs[2]] < 1
  PMobj@Prob <- calcProb(MMSEobj@F_FMSY[, 1,1,, Yrs[1]:Yrs[2]] < 1, MMSEobj)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(PNOF) <- 'PM'



## Safety ----

#' @describeIn PMs Probability of breaching the limit reference point (SSB<0.4SSB_MSY) in any of the first 10 years (2025-2034)
#' @family Safety
#' @export
LRP_short <- function (MMSEobj = NULL, Ref = 0.4, Yrs = c(3,12))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- "LRP_short: Probability of breaching the limit reference point (SSB<0.4SSB_MSY) in any of the first 10 years (2025-2033)"
  PMobj@Caption <- "Prob. SB < 0.4SBMSY (2025-2034)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]]

  PMobj@Prob <- calcProb(PMobj@Stat < PMobj@Ref, MMSEobj)
  Prob  <- array(as.logical(PMobj@Prob), dim=dim(PMobj@Prob))
  PMobj@Mean <- colSums(Prob)/nrow(Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(LRP_short) <- 'PM'

#' @describeIn PMs Probability of not breaching the limit reference point (SSB>0.4SSB_MSY) in any of the first 10 years (2025-2034)
#' @family Safety
#' @export
nLRP_short <- function (MMSEobj = NULL, Ref = 0.4, Yrs = c(3,12))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- "nLRP_short: Probability of not breaching the limit reference point (SSB>0.4SSB_MSY) in any of the first 10 years (2025-2034)"
  PMobj@Caption <- "Prob. SB > 0.4SBMSY (2025-2034)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]]

  PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MMSEobj)
  Prob  <- array(as.logical(PMobj@Prob), dim=dim(PMobj@Prob))
  PMobj@Mean <- colSums(Prob)/nrow(Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(nLRP_short) <- 'PM'


#' @describeIn PMs Probability of breaching the limit reference point (SSB<0.4SSB_MSY) in any of years 11-20 (2035-2044)
#' @family Safety
#' @export
LRP_med <- function (MMSEobj = NULL, Ref = 0.4, Yrs = c(13,22))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- "LRP_short: Probability of breaching the limit reference point (SSB<0.4SSB_MSY) in any of years 11-20 (2035-2044)"
  PMobj@Caption <- "Prob. SB < 0.4SBMSY (2035-2044)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]]

  PMobj@Prob <- calcProb(PMobj@Stat < PMobj@Ref, MMSEobj)
  Prob  <- array(as.logical(PMobj@Prob), dim=dim(PMobj@Prob))
  PMobj@Mean <- colSums(Prob)/nrow(Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(LRP_med) <- 'PM'

#' @describeIn PMs Probability of not breaching the limit reference point (SSB>0.4SSB_MSY) in any of years 11-20 (2035-2044)
#' @family Safety
#' @export
nLRP_med <- function (MMSEobj = NULL, Ref = 0.4, Yrs = c(13,22))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- "nLRP_med: Probability of not breaching the limit reference point (SSB>0.4SSB_MSY) in any of years 11-20 (2035-2044)"
  PMobj@Caption <- "Prob. of not breaching LRP (2035-2044)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]]

  PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MMSEobj)
  Prob  <- array(as.logical(PMobj@Prob), dim=dim(PMobj@Prob))
  PMobj@Mean <- colSums(Prob)/nrow(Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(nLRP_med) <- 'PM'




#' @describeIn PMs Probability of breaching the limit reference point (SSB<0.4SSB_MSY) in any of years 21-30 (2045-2054)
#' @family Safety
#' @export
LRP_long <- function (MMSEobj = NULL, Ref = 0.4, Yrs = c(23,32))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- "LRP_long: Probability of breaching the limit reference point (SSB<0.4SSB_MSY) n any of years 21-30 (2045-2054))"
  PMobj@Caption <- "Prob. SB < 0.4SBMSY (2045-2054)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]]

  PMobj@Prob <- calcProb(PMobj@Stat < PMobj@Ref, MMSEobj)
  Prob  <- array(as.logical(PMobj@Prob), dim=dim(PMobj@Prob))
  PMobj@Mean <- colSums(Prob)/nrow(Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(LRP_long) <- 'PM'

#' @describeIn PMs Probability of not breaching the limit reference point (SSB>0.4SSB_MSY) in any of years 21-30 (2045-2054)
#' @family Safety
#' @export
nLRP_long <- function (MMSEobj = NULL, Ref = 0.4, Yrs = c(23,32))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- "nLRP_long: Probability of not breaching the limit reference point (SSB>0.4SSB_MSY) n any of years 21-30 (2045-2054))"
  PMobj@Caption <- "Prob. of not breaching LRP (2045-2054)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]]

  PMobj@Prob <- calcProb(PMobj@Stat > PMobj@Ref, MMSEobj)
  Prob  <- array(as.logical(PMobj@Prob), dim=dim(PMobj@Prob))
  PMobj@Mean <- colSums(Prob)/nrow(Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(nLRP_long) <- 'PM'



#' @describeIn PMs Probability of breaching the limit reference point (SSB<0.4SSB_MSY) in any year (2025-2054)
#' @family Safety
#' @export
LRP <- function (MMSEobj = NULL, Ref = 0.4, Yrs = c(3,32))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- "LRP: Probability of breaching the limit reference point (SSB<0.4SSB_MSY) over all years (2025-2054)"
  PMobj@Caption <- "Prob. SB < 0.4SBMSY (2025-2054)"

  PMobj@Ref <- Ref
  PMobj@Stat <- MMSEobj@SB_SBMSY[, 1,, Yrs[1]:Yrs[2]]

  PMobj@Prob <- calcProb(PMobj@Stat < PMobj@Ref, MMSEobj)
  Prob  <- array(as.logical(PMobj@Prob), dim=dim(PMobj@Prob))
  PMobj@Mean <- colSums(Prob)/nrow(Prob)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(LRP) <- 'PM'

#' @describeIn PMs Probability of not breaching the limit reference point (SSB>0.4SSB_MSY) in any year (2025-2054)
#' @family Safety
#' @export
nLRP <- function (MMSEobj = NULL, Ref = 0.4, Yrs = c(3,32))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- "LRP: Probability of not breaching the limit reference point (SSB>0.4SSB_MSY) over all years (2025-2054)"
  PMobj@Caption <- "Prob. of not breaching LRP (2025-2054)"

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

#' @describeIn PMs TAC in the First Implementation Year (2025)
#' @family Yield
#' @export
TAC1 <- function(MMSEobj=NULL, Ref=1, Yrs=c(3,3)) {
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


#' @describeIn PMs Median TAC (t) over years 1-10 (2025-2034)
#' @family Yield
#' @export
AvTAC_short <- function(MMSEobj=NULL, Ref=NULL, Yrs=c(3,12)) {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- 'Median TAC (t) over years 1-10'
  PMobj@Caption <- 'Median TAC (t) 2025 - 2034'

  Stat_y <- apply(MMSEobj@TAC[,,,,Yrs[1]:Yrs[2], drop=FALSE], c(1,4,5), sum)

  PMobj@Stat <- apply(Stat_y, c(1,2), median)
  PMobj@Ref <- 1
  PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj) # no probability to calculate

  PMobj@Mean <- apply(Stat_y, 2, median)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(AvTAC_short) <- 'PM'

#' @describeIn PMs Median TAC relative to MSY over years 1-10 (2025-2034)
#' @family Yield
#' @export
rAvTAC_short <- function(MMSEobj=NULL, Ref=NULL, Yrs=c(3,12)) {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- 'Median relative TAC over years 1-10'
  PMobj@Caption <- 'Median relative TAC 2025 - 2034'

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


#' @describeIn PMs Median TAC (t) over years 11-20 (2035-2044)
#' @family Yield
#' @export
AvTAC_med <- function(MMSEobj=NULL, Ref=1, Yrs=c(12,22)) {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- 'Median TAC (t) over years 11-20'
  PMobj@Caption <- 'Median TAC (t) (2035 - 2044)'

  Stat_y <- apply(MMSEobj@TAC[,,,,Yrs[1]:Yrs[2], drop=FALSE], c(1,4,5), sum)
  PMobj@Stat <- apply(Stat_y, c(1,2), median)
  PMobj@Ref <- Ref
  PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj) # no probability to calculate

  PMobj@Mean <- apply(Stat_y, 2, median)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(AvTAC_med) <- 'PM'

#' @describeIn PMs Median TAC relative to MSY over years 11-20 (2035-2044)
#' @family Yield
#' @export
rAvTAC_med <- function(MMSEobj=NULL, Ref=1, Yrs=c(13,22)) {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- 'Median relative TAC over years 11-20'
  PMobj@Caption <- 'Median relative TAC 2035 - 2044'

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

#' @describeIn PMs Median TAC (t) over years 21-30 (2045-2054)
#' @family Yield
#' @export
AvTAC_long <- function(MMSEobj=NULL, Ref=1, Yrs=c(23,32)) {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- 'Median TAC (t) over years 21-30'
  PMobj@Caption <- 'Median TAC (t) (2045 - 2054)'

  Stat_y <- apply(MMSEobj@TAC[,,,,Yrs[1]:Yrs[2], drop=FALSE], c(1,4,5), sum)
  PMobj@Stat <- apply(Stat_y, c(1,2), median)
  PMobj@Ref <- Ref
  PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj) # no probability to calculate

  PMobj@Mean <- apply(Stat_y, 2, median)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(AvTAC_long) <- 'PM'

#' @describeIn PMs Median TAC relative to MSY over years 21-30 (2045-2054)
#' @family Yield
#' @export
rAvTAC_long <- function(MMSEobj=NULL, Ref=1, Yrs=c(23,32)) {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- 'Median relative TAC over years 21-30'
  PMobj@Caption <- 'Median relative TAC 2045 - 2054'

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

#' @describeIn PMs Mean variation in TAC (\%) between management cycles over all years and simulations
#' @family Stability
#' @export
VarC <- function (MMSEobj = NULL, Ref=1, Yrs=c(3,32))  {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- 'VarC: Mean Variation in TAC (%) between management cycles'
  PMobj@Caption <- 'Mean Variation in TAC (%)'

  TAC <- apply(MMSEobj@TAC[,,,,Yrs[1]:Yrs[2], drop=FALSE], c(1,4,5), sum, na.rm=TRUE)

  # get management cycle
  nMPs <- MMSEobj@nMPs
  interval <- rep(NA, nMPs)
  for (mm in 1:nMPs) {
    interval[mm] <- suppressWarnings(min(apply(TAC[,mm,], 1, firstChange), na.rm=TRUE))
  }

  yrs <- seq_along(Yrs[1]:Yrs[2])
  change_yrs <- seq(1, by=interval[1], to=max(yrs))
  varC <- array(NA, dim=c(MMSEobj@nsim, length(change_yrs)-1, MMSEobj@nMPs))
  for (mm in 1:nMPs) {
    change_yrs <- seq(1, by=interval[mm], to=max(yrs))
    y1 <- change_yrs[1:(length(change_yrs)-1)]
    y2 <- change_yrs[2:length(change_yrs)]
    mat <- (((TAC[, mm, y2] - TAC[, mm, y1])/TAC[,mm , y1])^2)^0.5
    if (dim(mat)[2] < dim(varC)[2]) {
      dd <- dim(varC)[2] - dim(mat)[2]
      mat <- abind::abind(mat, matrix(NA, nrow=MMSEobj@nsim, ncol=dd))
    }

    if (interval[mm]==Inf) {
      varC[,,mm] <- 0
    } else {
      varC[,,mm] <- mat
    }
  }

  PMobj@Stat <- varC
  PMobj@Ref <- Ref
  PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj)
  PMobj@Mean <- apply(PMobj@Stat, 3, mean, na.rm=TRUE)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(VarC) <- 'PM'



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

  df <- do.call(rbind.data.frame, PM_Values)

  df$MP_name <- unlist(lapply(df$MP %>% strsplit(., '_'), '[[', 1))

  if(all(grepl('_', df$MP))) {
    code <- unlist(lapply(df$MP %>% strsplit(., '_'), '[[', 2))
  } else {
    code <- NA
  }

  df$Target <- TuneTargets$Target[match(code, TuneTargets$Code)]
  df$Target <- as.factor(df$Target)
  df$Name <- df$PM
  df

}

#' Create a Trade-Off Plot
#'
#' @param DF A data.frame from `get_tune_OM`
#' @param PMs Character length 2. Names of `PM` functions to plot
#' @param inc.leg logical. Include the legend?
#'
#' @return A `ggplot` object
#' @export
Trade_Off <- function(DF, PM1, PM2, xline=NULL,
                      yline=NULL, inc.leg=TRUE,
                      pt.size=2, xmax=NULL, xmin=NULL, ymax=NULL, ymin=NULL,
                      lab.MPs=NULL) {


  DF2 <- DF %>% filter(PM %in% c(PM1, PM2)) %>%
    dplyr::select(MP, MP_name, PM, Value, Target) %>%
    tidyr::pivot_wider(., names_from = PM, values_from = Value)

  DF2 <- DF2 %>% rename(., PM1=all_of(PM1), PM2=all_of(PM2))

  if (is.null(xmin)) xmin <- 0
  if (is.null(xmax)) xmax <- max(DF2$PM1)
  if (is.null(ymin)) ymin <- 0
  if (is.null(ymax)) ymax <- max(DF2$PM2)


  captions <- DF %>% filter(PM %in% c(PM1, PM2)) %>%
    distinct(PM, caption) %>%
    tidyr::pivot_wider(., names_from = PM, values_from = caption) %>%
    rename(., x=all_of(PM1), y=all_of(PM2))

  p <- ggplot(DF2, aes(x=PM1, y=PM2)) +
    geom_point(size=pt.size, aes(shape=Target, color=MP_name)) +
    expand_limits(x=c(xmin,1), y=c(ymin,1)) +
    labs(x=captions$x, y=captions$y, shape='PGK_short', color='PGK_short') +
    theme_bw()

  if (!is.null(xline))
    p <- p + geom_vline(xintercept = xline, linetype=2, color='darkgray')
  if (!is.null(yline))
    p <- p + geom_hline(yintercept = yline, linetype=2, color='darkgray')

  if (!inc.leg)
    p <- p + guides(shape='none')
  p <- p + guides(color='none')
  if (!is.null(ymax))
    p <- p + ylim(c(ymin, ymax))
  if (!is.null(xmax))
    p <- p + xlim(c(xmin, xmax))

  if (!is.null(lab.MPs)) {
    DF3 <- DF2 %>% filter(MP %in% lab.MPs)
    p <- p + ggrepel::geom_text_repel(data=DF3, aes(label=MP, color=MP_name), show.legend = FALSE,
                                      max.overlaps=20)
  }


  p
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


#' Process Results and Save to Disk
#'
#' @param dir
#'
#' @return
#' @export
process_results <- function(dir) {
  MSElist <- list()
  fls <- list.files(file.path('MSE_Objects', dir))

  for (i in seq_along(fls)) {
    om <- strsplit(fls[i], '.mse')[[1]][1]
    MSElist[[i]] <- readRDS(file.path('MSE_Objects', dir, fls[i]))
    if (class(MSElist[[i]]@multiHist[[1]]) =='character') {
      MSElist[[i]]@multiHist <- readRDS(file.path('Hist_Objects', dir, paste0(om, '.hist')))
    }
  }

  if (!dir.exists(file.path('Results', dir)))
    dir.create(file.path('Results', dir))

  MMSE <- combine_MMSE(MSElist, dir)

  # save PM results
  PerfDF <- calc_Performance(MMSE, dir)
  saveRDS(PerfDF, file.path('Results', dir, 'PM_values.rda'))

  # create and save time-series information
  TS_list <- list()
  for (i in seq_along(fls)) {
    OM <- strsplit(fls[i], '.mse')[[1]][1]
    MMSE <- MSElist[[i]]
    TS_DF <- get_TS_info(MMSE, OM)
    TS_DF$Class <- dir
    TS_list[[i]] <- TS_DF
  }
  TS_DF <- do.call('rbind', TS_list)
  saveRDS(TS_DF, file.path('Results', dir, 'TS_values.rda'))

}

#' Get Time-Series information
#'
#' @param MMSE
#'
#' @return
#' @export
get_TS_info <- function(MMSE, OM) {

  All_Years <- get_Years(MMSE)
  nsim <- MMSE@nsim
  MPs <- MMSE@MPs[[1]]
  nMPs <- MMSE@nMPs
  Hist_Years <- All_Years %>% filter(Period=='Historical')
  Projection_Years <- All_Years %>% filter(Period!='Historical')
  nyears <- length(Hist_Years$Year)
  pyears <- length(Projection_Years$Year)

  SB_SBMSY <- get_SSB(MMSE, OM) %>% filter(Stock=='Female') %>% filter(Period=='Projection')
  SB_SBMSY$SB_SBMSY <- SB_SBMSY$Value/MMSE@RefPoint$ByYear$SSBMSY[1,1,1,(nyears+1)]

  F_FMSY <- get_F(MMSE, OM) %>% filter(Stock=='Female') %>% filter(Period=='Projection')
  F_FMSY$F_FMSY <- F_FMSY$Value/MMSE@RefPoint$ByYear$FMSY[1,1,1,(nyears+1)]

  TAC <- apply(MMSE@TAC, c(1,4,5), sum)

  F_FMSY$TAC <- as.vector(TAC)

  SB_SBMSY <- SB_SBMSY %>% distinct(Year, Sim, Model, MP, SB_SBMSY)
  F_FMSY <- F_FMSY %>% distinct(Year, Sim, Model, MP, F_FMSY, TAC)

  left_join(SB_SBMSY,
            F_FMSY,
            by = join_by(Year, Sim, Model, MP))
}

#' Get Performance Metrics from MMSE Object
#'
#' @param MMSE
#'
#' @return
#' @export
calc_Performance <- function(MMSE, Class=NULL, PMs=NULL, msg=TRUE) {
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

  df <- do.call(rbind.data.frame, PM_Values)
  df$Class <- Class
  MP_names <- df$MP %>% strsplit(., '_')
  df$MP_names <- lapply(MP_names, '[[', 1) %>% unlist()
  df
}

addOMnumber <- function(df) {
  df$OM <- 1
  df$OM[df$Sim %in% 51:100] <- 2
  df$OM[df$Sim %in% 101:150] <- 3
  df$OM[df$Sim %in% 151:200] <- 4
  df$OM[df$Sim %in% 201:250] <- 5
  df$OM[df$Sim %in% 251:300] <- 6
  df$OM[df$Sim %in% 301:350] <- 7
  df$OM[df$Sim %in% 351:400] <- 8
  df$OM[df$Sim %in% 401:450] <- 9
  df
}


#' Process and Save Results
#'
#' @param MSE_DF A data.frame created b `m`
#' @return
#' @export
Process_MSE_Results <- function(PMs=NULL,
                                MSE.dir='MSE_Objects',
                                Results.dir='Results',
                                class=NULL) {

  MSE.files <- list.files(MSE.dir, pattern='.mse')

  make_DF <- function(MSE.files) {
    tt <- lapply(1:length(MSE.files), function(i) {
      txt <- strsplit(MSE.files[i], '-')[[1]]
      data.frame(OM=txt[1], MP_name=txt[2],
                 Class=strsplit(txt[3], '.mse')[[1]][1], file=MSE.files[i])
    })
    do.call('rbind', tt)
  }

  MSE_DF <- make_DF(MSE.files)
  classes <- unique(MSE_DF$Class)
  if (!is.null(class)) classes <- class

  for (cl in classes) {
    message("Processing Results for", cl)

    MSE_DF2 <- MSE_DF %>% filter(Class==cl)

    MPs <- MSE_DF2$MP %>% unique()
    PM_results_list <- list()
    TS_results_list <- list()
    VarC_results_list <- list()

    for (i in seq_along(MPs)) {
      df <- MSE_DF2 %>% filter(MP_name==MPs[i])
      MSElist <- list()
      for (j in 1:nrow(df)) {
        MSElist[[j]] <- readRDS(file.path(MSE.dir, df$file[j]))
      }

      MSE <- combine_MMSE(MSElist, cl)

      PM_results_list[[i]] <- PM_table(MSE, PMs=PMs, msg=FALSE)

      # Calc Var C statistics
      nsim <- MSE@nsim
      temp <- VarC(MSE)
      nmanyr <- dim(temp@Stat)[2]
      if (nmanyr==0) {
        vals <- 0
        nmanyr <- 1
      } else {
        vals <- as.vector(temp@Stat)
      }

      varCdf <- data.frame(Sim=1:nsim,
                           Management_Year=rep(1:nmanyr, each=nsim),
                           MP=rep(MSE@MPs[[1]], each=nmanyr*nsim),
                           Value=vals)

      VarC_results_list[[i]] <- varCdf


      nyears <- MSE@proyears
      ts_mp <- list()
      for (mm in 1:MSE@nMPs) {
        mp <- MSE@MPs[[1]][mm]
        ts_mp[[mm]] <- data.frame(Sim=1:nsim,
                                  Year=rep(2023:2054, each=nsim),
                                  SB_SBMSY=as.vector(MSE@SB_SBMSY[,1,mm,]),
                                  F_FMSY=as.vector(MSE@F_FMSY[,1,1,mm,]),
                                  TAC=as.vector(apply(MSE@TAC[,,1,mm,], c(1,3), sum)),
                                  Landings=as.vector(apply(MSE@Catch[,,1,mm,], c(1,3), sum)),
                                  MP=mp)
        # Add OM number
        ts_mp[[mm]] <- addOMnumber(ts_mp[[mm]])
        ts_mp[[mm]]$MP_name <- unlist(lapply(ts_mp[[mm]]$MP %>% strsplit(., '_'), '[[', 1))
      }
      TS_results_list[[i]] <- do.call('rbind', ts_mp)
    }

    PM_results <- do.call('rbind', PM_results_list)
    TS_results <- do.call('rbind', TS_results_list)
    VarC_results <- do.call('rbind', VarC_results_list)

    message("Saving results to:", file.path(Results.dir, cl))
    if(!dir.exists(file.path(Results.dir, cl)))
      dir.create(file.path(Results.dir, cl))
    saveRDS(PM_results, file.path(Results.dir, cl, 'PM_values.rda'))
    saveRDS(TS_results, file.path(Results.dir, cl, 'TS_values.rda'))
    saveRDS(VarC_results, file.path(Results.dir, cl, 'VarC_results.rda'))

  }


}


## MP Reports ----
make_table <- function(PM_results_mp, pms, rnd=2) {
  pm_table <- PM_results_mp %>% filter(PM %in% pms ) %>%
    mutate(Value=round(Value,rnd)) %>%
    select(PM, MP, Value)

  ind <- grepl('TAC', pm_table$PM)
  if (sum(ind)>0) {
    pm_table$Value[which(ind)] <- round(pm_table$Value[which(ind)],0)
    pm_table$Value[which(ind)] <- format(pm_table$Value[which(ind)], nsmall=0)
  } else {
    pm_table$Value <- format(signif(pm_table$Value,rnd), nsmall=2)
  }


  tbs <- lapply(split(pm_table, pm_table$MP), '[', -2)

  tibble(x = rep(-Inf, length(tbs)),
         y = rep(-Inf, length(tbs)),
         MP = levels(as.factor(pm_table$MP)),
         tbl = tbs)
}


# mp_TS_results <- TS_results |> filter(MP=='CE_b', Model=='Reference')
# mp_PM_results <- PM_results |> filter(MP=='CE_b', Model=='Reference')
# ll <- list(mp_TS_results, mp_PM_results)

# PM_results <- PM_results |> filter(MP=='CE_b', Model=='Reference')

#' Title
#'
#'
#' @return
#' @export
Time_Series_Plot <- function(ll, alpha=0.7, size.axis.title=16, size.axis.text=14,
                             size.strip.text=16, ymax=NULL) {

  Year_df <- data.frame(Year=2025:2054, Period='Short')
  Year_df$Period[Year_df$Year%in% 2035:2044] <- 'Medium'
  Year_df$Period[Year_df$Year%in% 2045:2054] <- 'Long'

  addtheme <- ggplot2::theme(axis.title = element_text(size=size.axis.title),
                          axis.text = element_text(size=size.axis.text),
                          strip.text=element_text(size=size.strip.text))
  df <- ll[[1]]
  PM_results_mp <- ll[[2]]

  fills=c('#373737', '#363639', '#CDCDCD')

  F_FMSYdf <-  df %>% filter(name=='F_FMSY')

  p1 <- ggplot(F_FMSYdf, aes(x=Year)) +
    facet_grid(~MP, scales='free') +
    geom_ribbon(aes(ymin=Lower , ymax=Upper, fill=fill), alpha=alpha) +
    # geom_line(aes(y=Median), linewidth=0.5) +
    geom_line(aes(y=Median)) +
    expand_limits(y=0) +
    geom_line(data=Year_df, aes(x=Year, y=1, col=Period), linetype=2, lwd=2)  +
    theme_bw() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_manual(values=fills) +
    guides(fill='none', color='none') +
    labs(y='F/FMSY') +
    geom_table(data = make_table(PM_results_mp, c('PNOF', 'PGK_short', 'PGK_med', 'PGK_long')),
               aes(x = x, y = y,label = tbl),
               hjust = 'inward', vjust = 'inward') +
    addtheme

  if (!is.null(ymax)) {
    max <- ymax |> dplyr::filter(name=='F_FMSY')
    p1 <- p1 + expand_limits(y=c(0,max$Max))
  }

  # B_BMSY
  B_BMSYdf <-df %>% filter(name=='SB_SBMSY')

  p2 <- ggplot(B_BMSYdf, aes(x=Year)) +
    facet_grid(~MP, scales='free') +
    geom_ribbon(aes(ymin=Lower , ymax=Upper, fill=fill), alpha=alpha) +
    geom_line(aes(y=Median)) +
    expand_limits(y=0) +
    theme_bw() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_manual(values=fills) +
    geom_line(data=Year_df, aes(x=Year, y=1, col=Period), linetype=2, lwd=2)  +
    geom_line(data=Year_df, aes(x=Year, y=0.4), linetype=3)  +
    guides(fill='none', color='none') +
    labs(y='SB/SBMSY') +
    geom_table(data = make_table(PM_results_mp, c('nLRP')),
               aes(x = x, y = y, label = tbl),
               hjust = 'inward', vjust = 'inward') +
    addtheme

  if (!is.null(ymax)) {
    max <- ymax |> dplyr::filter(name=='SB_SBMSY')
    p2 <- p2 +  expand_limits(y=c(0,max$Max))
  }

  # TAC

  TACdf <-df %>% filter(name=='TAC')

  if (!is.null(ymax)) {
    max <- ymax |> dplyr::filter(name=='TAC')
    max <- max$Max
  } else {
    max <- max(TACdf$Upper) * 1.05
  }

  p3 <- ggplot(TACdf, aes(x=Year)) +
    facet_grid(~MP, scales='free') +
    geom_ribbon(aes(ymin=Lower , ymax=Upper, fill=fill), alpha=alpha) +
    geom_line(aes(y=Median)) +
    expand_limits(y=c(0, max)) +
    theme_bw() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_manual(values=fills) +
    geom_line(data=Year_df, aes(x=Year, y=mean(tail(SWOData@Cat[1,], 5)), col=Period), linetype=2, lwd=2)  +
    guides(fill='none', color='none') +
    labs(y='TAC') +
    geom_table(data = make_table(PM_results_mp,
                                 c('TAC1', 'AvTAC_short', 'AvTAC_med', 'AvTAC_long',
                                   'VarC')),
               aes(x = x, y = y, label = tbl),
               hjust = 'inward', vjust = 'inward') +
    addtheme



  cowplot::plot_grid(p1,p2,p3, nrow=3, align='v')


}

MP_Report_class <- function(mp_name, cl) {
  PM_results <- readRDS(file.path('Results', cl, 'PM_values.rda'))
  TS_results <- readRDS(file.path('Results', cl, 'TS_values.rda'))

  TS_results_mp <- TS_results %>% filter(MP_name==mp_name)
  PM_results_mp <- PM_results %>% filter(MP_name==mp_name)

  results <- list(TS_results_mp, PM_results_mp)
  p <- Time_Series_Plot(results)

  cl_name <- strsplit(cl, "_")[[1]][1]

  title <- cowplot::ggdraw() + cowplot::draw_label(cl_name, fontface='bold')
  p <- cowplot::plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))
  nm <- paste(mp_name, cl, sep="_") %>% paste0(., '.png')
  ggsave(file.path('img/MP_Reports',nm), p, width=12, height=12)
}

Quilt <- function(PM_results, PMs=NULL) {
  library(DT)
  PM_results$Value <- round(PM_results$Value,2)
  tab <- PM_results %>% select(PM, MP, Value) %>% filter(PM %in% PMs)

  tab$PM <- factor(tab$PM, levels=PMs, ordered = TRUE)
  tab <- tab %>% group_by(PM) %>% arrange()
  tab <- tab  %>%
    tidyr::pivot_wider(., names_from = PM, values_from = Value)


  colfunc <- colorRampPalette(c("blue", "white"), alpha=TRUE)

  # Probability colors
  probs <- seq(0, 1.01, length.out=50)
  prob_colors <- rev(colfunc(length(probs)+1))
  rev_prob_colors <- rev(prob_colors)

  # TAC colors
  TAC_PMs <- PM_results$Name[grepl('TAC', PM_results$Name )] %>% unique()


  # Variability colors


  # Make table
  quilt <-  DT::datatable(tab, options = list(dom = 't', pageLength =20))

  for (i in 2:ncol(tab)) {
    pm <- colnames(tab)[i]

    if (grepl('TAC', pm)) {
      cuts <- seq(min(tab[,i]), max(tab[,i])*1.1, length.out=10)
      values <- rev(colfunc(length(cuts)+1))

    } else if (grepl('VarC', pm)) {
      # variability
      cuts <- seq(0, 1, length.out=10)
      values <- (colfunc(length(cuts)+1))

    } else {
      # probabilities
      cuts <- seq(0, 1.01, length.out=50)
      values <- rev(colfunc(length(cuts)+1))
    }
    quilt <- quilt %>%
      formatStyle(
        pm,
        backgroundColor = styleInterval(cuts=cuts,
                                        values=values)

      )

  }
  quilt
}

#' @describeIn Time_Series_Plot Another time series plot
#' @export
TimeSeriesPlot2 <- function(DF, PM_ResultsMP, alpha=0.7, size.axis.title=16, size.axis.text=14,
                            size.strip.text=16, text.size=6, includeOM=TRUE) {

  fills <- c('#373737', '#363639', '#CDCDCD')

  addtheme <- ggplot2::theme(axis.title = element_text(size=size.axis.title),
                             axis.text = element_text(size=size.axis.text),
                             strip.text=element_text(size=size.strip.text))
  n_mps <- DF$MP |> unique() |> length()
  each <- (DF$Year |> length()) / n_mps

  DF$plot <- rep(1:n_mps, each=each)
  DF$name[DF$name=='F_FMSY'] <- 'F/FMSY'
  DF$name[DF$name=='SB_SBMSY'] <- 'SB/SBMSY'

  Year_df <- data.frame(Year=2025:2054, Period='Short')
  Year_df$Period[Year_df$Year%in% 2035:2044] <- 'Medium'
  Year_df$Period[Year_df$Year%in% 2045:2054] <- 'Long'
  Year_df$name <- 'F/FMSY'
  Year_df$y <- 1

  Year_df2 <- Year_df
  Year_df2$name <- 'SB/SBMSY'
  Year_df2$y <- 1

  Year_df3 <- Year_df
  Year_df3$name <- 'TAC'
  Year_df3$y <- 10000

  Year_df <- bind_rows(Year_df, Year_df2, Year_df3)

  ref_table <- DF |> dplyr::filter(Year==2025, name=='F/FMSY') |>
    group_by(plot, MP, name) |>
    distinct(Model)


  PM_ResultsMP$plot <- rep(1:n_mps, each=20)

  table_list <- list()
  mps <- PM_ResultsMP |> group_by(plot) |>
    distinct(MP)

  for (p in 1:n_mps) {
    tab1 <- make_table(PM_ResultsMP |> filter(plot==p), pms= c('PNOF', 'PGK_short', 'PGK_med', 'PGK_long'))
    tab1$name <- 'F/FMSY'
    tab1$plot <- p

    tab2 <- make_table(PM_ResultsMP |> filter(plot==p), pms= c('nLRP'))
    tab2$name <- 'SB/SBMSY'
    tab2$plot <- p

    tab3 <- make_table(PM_ResultsMP |> filter(plot==p), pms= c('TAC1', 'AvTAC_short', 'AvTAC_med', 'AvTAC_long',
                                                               'VarC'))
    tab3$name <- 'TAC'
    tab3$plot <- p

    table_list[[p]] <- bind_rows(tab1, tab2, tab3)
  }
  tableDF <- do.call('rbind', table_list)

  mpnames <- c('1'=mps$MP[1],
               '2'=mps$MP[2],
               '3'=mps$MP[3],
               '4'=mps$MP[4],
               '5'=mps$MP[5],
               '6'=mps$MP[6],
               '7'=mps$MP[7],
               '8'=mps$MP[8],
               '9'=mps$MP[9],
               '10'=mps$MP[10]
               )

  p <- ggplot(DF, aes(x=Year)) +
    facet_grid(name~plot, scales='free',
               labeller = labeller(plot = mpnames)) +

    geom_ribbon(aes(ymin=Lower , ymax=Upper, fill=fill), alpha=alpha) +
    geom_line(aes(y=Median)) +
    expand_limits(y=0) +
    geom_line(data=Year_df, aes(x=Year, y=y, col=Period), linetype=2, lwd=2)  +
    theme_bw() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0.01)) +
    scale_fill_manual(values=fills) +
    guides(fill='none', color='none') +
    ggpp::geom_table(data = tableDF,
               aes(x = x, y = y,label = tbl),
               hjust = 'inward', vjust = 'inward') +
    addtheme

  if (includeOM) {
    p <- p +   geom_text(data=ref_table, aes(x=2025, y=Inf, label=ref_table$Model),
                         vjust=1.2, hjust=-0.1, size=text.size)
  }
  p
}

