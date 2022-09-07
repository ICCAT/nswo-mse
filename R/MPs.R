
# Model-free CMPs ----


#' Targeting Model-Free Example MP
#'
#' A example index-targeting MP that iteratively adjusts the TAC
#' based on the ratio of recent to overall index.
#'
#' @param x A position in the data object
#' @param Data An object of class `Data`
#' @param Data_Lag The number of years to lag the data
#' @param Interval The TAC update interval
#' @param yrsmth The number of recent years to average index over
#' @param mc The maximum fractional change in the TAC among years
#' @param multi Multiplier for the index target from the average index
#' @param ... Additional arguments (unused)
#'
#' @return An object of class `Rec`
#' @export
ITarget_1 <- function(x, Data,
                      Data_Lag=2, Interval=3,
                      yrsmth = 5, mc = 0.2, multi=1,
                      ...) {
  # 1. Create a `Rec` (recommendation) object
  Rec <- new('Rec')

  # 2. TAC Imputation for initial projection years
  # Year when the MSE analysis is being conducted (2022)
  Current_Yr <- as.numeric(format(Sys.Date(), "%Y"))
  if (max(Data@Year) <Current_Yr) {
    # set the TAC to last recorded catch for years up to
    # and including Current_Yr (2022)
    Rec@TAC <- Data@MPrec[x]
    return(Rec)
  }

  # 3. MP Management Interval
  Imp_Years <- seq(Current_Yr+1, by=Interval, length.out=30)
  # Not an MP update year
  if (!(max(Data@Year)+1) %in% Imp_Years) {
    Rec@TAC <- Data@MPrec[x]
    return(Rec)
  }

  # 4. Lag the simulated data by `Data_Lag` years
  Data <- Lag_Data(Data, Data_Lag, x==1)

  # 5. Start of MP-specific Code
  # number of years of index data
  n_years <- length(Data@Ind[x,])

  # year index for `yrsmth` most recent years
  yr_ind <- max(1, n_years-yrsmth+1):n_years

  # index target - mean index x `multi`
  Ind_Target <- mean(Data@Ind[x, ], na.rm=TRUE) * multi

  # ratio of mean recent index to Ind_Target
  deltaI <- mean(Data@Ind[x, yr_ind], na.rm=TRUE)/Ind_Target

  # max/min change in TAC
  if (deltaI < (1 - mc)) deltaI <- 1 - mc
  if (deltaI > (1 + mc)) deltaI <- 1 + mc

  Rec@TAC <- Data@MPrec[x] * deltaI
  # 6. Return the `Rec` object
  Rec
}
# 7. Assign function to class `MP`
class(ITarget_1) <- 'MP'

#' Incremental Index Target MP
#'
#' A management procedure that incrementally adjusts the TAC
#' (starting from reference level that is a fraction of mean recent catches)
#' to reach a target CPUE / relative abundance index
#' See `?Itarget1` for more information.
#'
#' @param x A position in the data object
#' @param Data An object of class `Data`
#' @param Data_Lag The number of years to lag the data
#' @param Interval The TAC update interval
#' @param yrsmth The number of recent years to average index over
#' @param xx Parameter controlling the fraction of mean catch to start using in first year
#' @param Imulti Parameter controlling how much larger target CPUE / index is compared with recent levels
#' @param ... Additional arguments (unused)
#'
#' @return An object of class `Rec`
#' @export
ITarget_2 <- function(x, Data,
                      Data_Lag=2, Interval=3,
                      yrsmth = 5, xx = 0, Imulti=1.5,
                      ...) {
  Rec <- new('Rec')

  Current_Yr <- as.numeric(format(Sys.Date(), "%Y"))
  if (max(Data@Year) <Current_Yr) {
    Rec@TAC <- Data@MPrec[x]
    return(Rec)
  }

  Imp_Years <- seq(Current_Yr+1, by=Interval, length.out=30)
  if (!(max(Data@Year)+1) %in% Imp_Years) {
    Rec@TAC <- Data@MPrec[x]
    return(Rec)
  }

  Data <- Lag_Data(Data, Data_Lag, x==1)
  # modify Year slot so Itarget1 works
  Data@Year <- Data@Year[1:length(Data@Cat[1,])]
  Rec <- DLMtool::Itarget1(x, Data, yrsmth = yrsmth, xx=xx, Imulti=Imulti, ...)
  Rec
}
# 7. Assign function to class `MP`
class(ITarget_2) <- 'MP'


# Assessment-based CMPs ----

#' ASPIC-like Surplus Production Assessment Model with TAC = MSY
#'
#' An example model-based CMP that uses the `SAMtool::SP` function, which is similar
#' to ASPIC. It sets the TAC to the estimated MSY.
#'
#' @param x A position in the data object
#' @param Data An object of class `Data`
#' @param Data_Lag The number of years to lag the data
#' @param Interval The TAC update interval
#' @param ... Additional arguments (unused)
#'
#' @return An object of class `Rec`
#' @export
SP_1 <- function(x, Data, Data_Lag=2, Interval=3, ...) {
  Rec <- new('Rec')

  Current_Yr <- as.numeric(format(Sys.Date(), "%Y"))
  if (max(Data@Year) <Current_Yr) {
    Rec@TAC <- Data@MPrec[x]
    return(Rec)
  }

  Imp_Years <- seq(Current_Yr+1, by=Interval, length.out=30)
  if (!(max(Data@Year)+1) %in% Imp_Years) {
    Rec@TAC <- Data@MPrec[x]
    return(Rec)
  }

  Data <- Lag_Data(Data, Data_Lag, x==1)

  # apply assessment model
  Mod <- SAMtool::SP(x, Data, fix_dep=TRUE, start=list(dep=0.85))

  # harvest control rule - TAC = MSY
  Rec@TAC <- Mod@MSY
  Rec
}
class(SP_1) <- 'MP'

#' ASPIC-like Surplus Production Assessment Fox Model with TAC = MSY
#'
#' An example model-based CMP that uses the `SAMtool::SP_Fox` function, which fixes BMSY/K = 0.37, and is similar
#' to ASPIC. It sets the TAC to the estimated MSY.
#'
#' @param x A position in the data object
#' @param Data An object of class `Data`
#' @param Data_Lag The number of years to lag the data
#' @param Interval The TAC update interval
#' @param ... Additional arguments (unused)
#'
#' @return An object of class `Rec`
#' @export
#'
SP_Fox_1 <- function(x, Data, Data_Lag=2, Interval=3, ...) {
  Rec <- new('Rec')

  Current_Yr <- as.numeric(format(Sys.Date(), "%Y"))
  if (max(Data@Year) <Current_Yr) {
    Rec@TAC <- Data@MPrec[x]
    return(Rec)
  }

  Imp_Years <- seq(Current_Yr+1, by=Interval, length.out=30)
  if (!(max(Data@Year)+1) %in% Imp_Years) {
    Rec@TAC <- Data@MPrec[x]
    return(Rec)
  }

  Data <- Lag_Data(Data, Data_Lag, x==1)

  # apply assessment model
  Mod <- SAMtool::SP_Fox(x, Data, fix_dep=TRUE, start=list(dep=0.85))

  # harvest control rule - TAC = MSY
  Rec@TAC <- Mod@MSY
  Rec
}
class(SP_Fox_1) <- 'MP'


#' JABBA Assessmetn Model with TAC = MSY
#'
#' An example model-based CMP that uses the `JABBA` package to assess the stock.
#' It sets the TAC = estimated MSY.
#'
#' @param x A position in the data object
#' @param Data An object of class `Data`
#' @param Data_Lag The number of years to lag the data
#' @param Interval The TAC update interval
#' @param ... Additional arguments (unused)
#'
#' @return An object of class `Rec`
#' @export
#'
JABBA_1 <- function(x, Data, Data_Lag=2, Interval=3, ...) {

  # check for dependencies
  chk <- require(JABBA, quietly=TRUE)
  if (!chk) stop('package `JABBA` must be installed')

  chk <- require(rjags, quietly=TRUE)
  if (!chk) stop('package `rjags` must be installed')

  Rec <- new('Rec')

  Current_Yr <- as.numeric(format(Sys.Date(), "%Y"))
  if (max(Data@Year) <Current_Yr) {
    Rec@TAC <- Data@MPrec[x]
    return(Rec)
  }

  Imp_Years <- seq(Current_Yr+1, by=Interval, length.out=30)
  if (!(max(Data@Year)+1) %in% Imp_Years) {
    Rec@TAC <- Data@MPrec[x]
    return(Rec)
  }

  Data <- Lag_Data(Data, Data_Lag, x==1)

  # structure data to suit JABBA
  catch <- data.frame(Data@Year[1:length(Data@Cat[x,])], Data@Cat[x,])
  index <- data.frame(Data@Year[1:length(Data@Cat[x,])],Data@Ind[x,])
  index_se <- data.frame(Data@Year[1:length(Data@Cat[x,])], Data@CV_Ind[x,])
  index_se[,2] <- 0.23

  input <- JABBA::build_jabba(catch=catch,
                              cpue=index,
                              se=index_se,
                              catch.cv=0.01,
                              assessment="SWO",
                              scenario = "1",
                              model.type = "Schaefer",
                              sigma.est = FALSE,
                              fixed.obsE = 0.01,
                              r.prior = c(0.42, 0.4),
                              psi.dist='beta',
                              psi.prior=c(0.95, 0.05),
                              verbose=FALSE)

  # apply assessment model
  Mod <- JABBA::fit_jabba(input,quickmcmc=TRUE, verbose=FALSE)

  # harvest control rule - TAC = MSY
  Rec@TAC <- Mod$estimates[8,1]
  Rec
}
class(JABBA_1) <- 'MP'





