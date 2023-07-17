#' Constant Exploitation with Control Rule
#'
#' This MP aims to keep the exploitation rate at a constant level. The current relative
#' exploitation rate is calculated as the mean of the catches from 2016:2023 divided
#' by the mean Combined Index over this same period.
#'
#' @param x A position in the data object
#' @param Data An object of class `Data`
#' @param Data_Lag The number of years to lag the data
#' @param Interval The TAC update interval
#' @param tunepar Parameter used for tuning
#' @param mc The maximum fractional change in the TAC among years. NA to ignore
#' @param yrs numeric values length 2. The year index to calculate the historical mean catches and index,
#' and the years to smooth the estimate of current exploitation rate
#' @param ... Additional arguments (unused)
#'
#' @return An object of class `Rec` with the `TAC` slot populated
#'
CE <- function(x, Data, Data_Lag=1, Interval=3, tunepar=1, mc=NA,
               yrs=c(5,3), ...) {
  Rec <- new('Rec')

  # Does TAC need to be updated? (or set a fixed catch if before Initial_MP_Yr)
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    Rec <- FixedTAC(Rec, Data) # use actual catches if they are available
    return(Rec)
  }

  # Lag Data
  Data <- Lag_Data(Data, Data_Lag)

  # Calculate Historical Relative Exploitation Rate
  yr.ind <- which(Data@Year==2020)
  hist.yrs <- (yr.ind-yrs[1]+1):yr.ind
  histER <- mean(Data@Cat[x,hist.yrs])/mean(Data@Ind[x,hist.yrs])

  # Calculate Current Relative Exploitation Rate
  current_yr <- length(Data@Ind[x,])
  recent_yrs <- (current_yr-yrs[2]+1):current_yr
  curER <- mean(Data@Cat[x,recent_yrs])/mean(Data@Ind[x,recent_yrs])

  # Control Rule

  histInd <- mean(Data@Ind[x,hist.yrs])
  curInd <- mean(Data@Ind[x,recent_yrs])

  ind_ratio <- curInd/histInd

  if (ind_ratio>=0.8) {
    targER <- histER
  } else if (ind_ratio> 0.5) {
    targER <- histER * ( -1.4+ 3 *ind_ratio)
  } else {
    targER <- 0.1 * histER
  }

  # Exploitation Rate Ratio
  ER_ratio <- targER/curER
  TAC <- ER_ratio * tunepar * Data@MPrec[x]

  # Maximum allowed change in TAC
  Rec@TAC <- MaxChange(TAC, Data@MPrec[x], mc)
  Rec@Misc[[x]] <- data.frame(targER, histER, curInd, histInd, ind_ratio, Data@Ind[x,])
  Rec
}

CE25 <-  CE
formals(CE25)$mc <- 0.25


CE2 <- function(x, Data, Data_Lag=1, Interval=3, tunepar=1, mc=0.25,
               yrs=c(5,3), ...) {
  Rec <- new('Rec')

  # Does TAC need to be updated? (or set a fixed catch if before Initial_MP_Yr)
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    Rec <- FixedTAC(Rec, Data) # use actual catches if they are available
    return(Rec)
  }

  # Lag Data
  Data <- Lag_Data(Data, Data_Lag)

  # Calculate Historical Relative Exploitation Rate
  Index <- Data@Ind[x,]
  Index_Smoothed <- stats::smooth.spline(Index[!is.na(Index)])

  Index <- c(rep(NA, sum(is.na(Data@Ind[x,]))), Index_Smoothed$y)

  yr.ind <- which(Data@Year==2020)
  hist.yrs <- (yr.ind-yrs[1]+1):yr.ind
  histER <- mean(Data@Cat[x,hist.yrs])/mean(Index[hist.yrs])

  # Calculate Current Relative Exploitation Rate
  current_yr <- length(Data@Ind[x,])
  recent_yrs <- (current_yr-yrs[2]+1):current_yr
  curER <- mean(Data@Cat[x,recent_yrs])/mean(Index[recent_yrs])


  histInd <- mean(Index[hist.yrs])
  curInd <- mean(Index[recent_yrs])

  ind_ratio <- curInd/histInd

  targER <- histER

  # Exploitation Rate Ratio
  ER_ratio <- targER/curER
  TAC <- ER_ratio * tunepar * Data@MPrec[x]

  # Maximum allowed change in TAC
  Rec@TAC <- MaxChange(TAC, Data@MPrec[x], mc)

  Rec
}


CE2cr <- function(x, Data, Data_Lag=1, Interval=3, tunepar=1, mc=0.25,
                yrs=c(5,3), ...) {
  Rec <- new('Rec')

  # Does TAC need to be updated? (or set a fixed catch if before Initial_MP_Yr)
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    Rec <- FixedTAC(Rec, Data) # use actual catches if they are available
    return(Rec)
  }

  # Lag Data
  Data <- Lag_Data(Data, Data_Lag)

  # Calculate Historical Relative Exploitation Rate
  Index <- Data@Ind[x,]
  Index_Smoothed <- stats::smooth.spline(Index[!is.na(Index)])

  Index <- c(rep(NA, sum(is.na(Data@Ind[x,]))), Index_Smoothed$y)

  yr.ind <- which(Data@Year==2020)
  hist.yrs <- (yr.ind-yrs[1]+1):yr.ind
  histER <- mean(Data@Cat[x,hist.yrs])/mean(Index[hist.yrs])

  # Calculate Current Relative Exploitation Rate
  current_yr <- length(Data@Ind[x,])
  recent_yrs <- (current_yr-yrs[2]+1):current_yr
  curER <- mean(Data@Cat[x,recent_yrs])/mean(Index[recent_yrs])

  # Control Rule
  histInd <- mean(Index[hist.yrs])
  curInd <- mean(Index[recent_yrs])

  ind_ratio <- curInd/histInd

  if (ind_ratio>=0.8) {
    targER <- histER
  } else if (ind_ratio> 0.5) {
    targER <- histER * ( -1.4+ 3 *ind_ratio)
  } else {
    targER <- 0.1 * histER
  }
  # Exploitation Rate Ratio
  ER_ratio <- targER/curER
  TAC <- ER_ratio * tunepar * Data@MPrec[x]

  # Maximum allowed change in TAC
  Rec@TAC <- MaxChange(TAC, Data@MPrec[x], mc)

  Rec
}



# ---- Tuned CMPs ----
#' @describeIn CE Tuned to PGK_short = 0.51 across Reference OMs.
#' @export
CE_a <- CE
formals(CE_a)$tunepar <- 0.926227678571429
class(CE_a) <- "MP"


#' @describeIn CE Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
CE_b <- CE
formals(CE_b)$tunepar <- 0.88056586270872
class(CE_b) <- "MP"


#' @describeIn CE Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
CE_c <- CE
formals(CE_c)$tunepar <- 0.833206915304691
class(CE_c) <- "MP"


#' @describeIn CE2 Tuned to PGK_short = 0.51 across Reference OMs.
#' @export
CE2_a <- CE2
formals(CE2_a)$tunepar <- 0.936695550825986
class(CE2_a) <- "MP"


#' @describeIn CE2 Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
CE2_b <- CE2
formals(CE2_b)$tunepar <- 0.887826873669742
class(CE2_b) <- "MP"


#' @describeIn CE2 Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
CE2_c <- CE2
formals(CE2_c)$tunepar <- 0.840275572954395
class(CE2_c) <- "MP"


#' @describeIn CE25 Tuned to PGK_short = 0.51 across Reference OMs.
#' @export
CE25_a <- CE25
formals(CE25_a)$tunepar <- 0.933753010540128
class(CE25_a) <- "MP"


#' @describeIn CE25 Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
CE25_b <- CE25
formals(CE25_b)$tunepar <- 0.886106710829493
class(CE25_b) <- "MP"


#' @describeIn CE25 Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
CE25_c <- CE25
formals(CE25_c)$tunepar <- 0.836688050747725
class(CE25_c) <- "MP"


#' @describeIn CE2cr Tuned to PGK_short = 0.51 across Reference OMs.
#' @export
CE2cr_a <- CE2cr
formals(CE2cr_a)$tunepar <- 0.936695550825986
class(CE2cr_a) <- "MP"


#' @describeIn CE2cr Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
CE2cr_b <- CE2cr
formals(CE2cr_b)$tunepar <- 0.887826873669742
class(CE2cr_b) <- "MP"


#' @describeIn CE2cr Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
CE2cr_c <- CE2cr
formals(CE2cr_c)$tunepar <- 0.840275572954395
class(CE2cr_c) <- "MP"


