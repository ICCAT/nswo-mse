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
CE <- function(x, Data, Data_Lag=2, Interval=3, tunepar=1, mc=0.25,
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

  # smooth combined index
  index <- smoothed_index <- Data@Ind[x,]
  smoothed <- stats::smooth(index[!is.na(index)])
  smoothed_index[!is.na(smoothed_index)] <- smoothed

  Data@Ind[x,] <- smoothed_index

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
  curInd <- mean(Data@Ind[x,recent_yrs], na.rm=TRUE)

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
  # Rec@Misc[[x]] <- data.frame(targER, histER, curInd, histInd, ind_ratio, Data@Ind[x,])
  Rec
}


CE2 <- function(x, Data, Data_Lag=2, Interval=3, tunepar=1, mc=0.25,
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

  # smooth combined index
  index <- smoothed_index <- Data@Ind[x,]

  smoothed <- stats::smooth(index[!is.na(index)])
  smoothed_index[!is.na(smoothed_index)] <- smoothed

  Data@Ind[x,] <- smoothed_index

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

  # calculate slope
  v <- Data@Ind[x,recent_yrs]
  y <- 1:3
  sl <- coef(lm(v~y))[2]

  if (ind_ratio>=0.8) {
    if (ind_ratio>1.2) {
      targER <- histER * ind_ratio
    } else {
      targER <- histER
    }
  } else if (ind_ratio> 0.5) {
    targER <- histER * ( -1.4+ 3 *ind_ratio)
  } else {
    targER <- 0.1 * histER
  }
  targER <- targER * exp(sl) * tunepar

  # Exploitation Rate Ratio
  ER_ratio <- targER/curER
  TAC <- ER_ratio * Data@MPrec[x]

  # Maximum allowed change in TAC
  Rec@TAC <- MaxChange(TAC, Data@MPrec[x], mc)
  # Rec@Misc[[x]] <- data.frame(targER, histER, curInd, histInd, ind_ratio, Data@Ind[x,])
  Rec
}


# ---- Tuned CMPs ----
#' @describeIn CE Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
CE_b <- CE
formals(CE_b)$tunepar <- 1.00858704137393
class(CE_b) <- "MP"


#' @describeIn CE Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
CE_c <- CE
formals(CE_c)$tunepar <- 0.95428130882294
class(CE_c) <- "MP"


#' @describeIn CE Tuned to PGK_med = 0.6 across Reference OMs.
#' @export
CE_d <- CE
formals(CE_d)$tunepar <- 1.06657420249653
class(CE_d) <- "MP"


#' @describeIn CE Tuned to PGK_long = 0.6 across Reference OMs.
#' @export
CE_e <- CE
formals(CE_e)$tunepar <- 1.12517293233083
class(CE_e) <- "MP"


