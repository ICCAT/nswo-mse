# Misc. Functions ----

#' Should the TAC remain unchanged?
#'
#' @param Initial_MP_Yr The calendar year when the MP will first be implemented
#' @param Interval The management interval where the TAC is updated
#' @param Data An object of class `Data`
#'
#' @return TRUE if the TAC should remain unchanged and FALSE otherwise
#' @export
#'
SameTAC <- function(Initial_MP_Yr, Interval, Data) {
  # Are CMP implemented yet?
  if (max(Data@Year) <(Initial_MP_Yr-1)) return(TRUE)

  # Is it an TAC update year?
  Imp_Years <- seq(Initial_MP_Yr, by=Interval, length.out=30)
  if (!(max(Data@Year)+1) %in% Imp_Years) return(TRUE)

  FALSE
}



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

  # 2. Check if TAC needs to be updated
  Initial_MP_Yr <- 2024
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    # Keep TAC unchanged from previous
    Rec@TAC <- Data@MPrec[x]
    return(Rec)
  }

  # 3. Lag the simulated data by `Data_Lag` years
  Data <- Lag_Data(Data, Data_Lag)

  # 4. Start of MP-specific Code
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
  # 5. Return the `Rec` object
  Rec
}
# 6. Assign function to class `MP`
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

  Initial_MP_Yr <- 2024
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    return(Rec)
  }

  Data <- Lag_Data(Data, Data_Lag)
  # modify Year slot so Itarget1 works
  Data@Year <- Data@Year[1:length(Data@Cat[1,])]
  Rec <- DLMtool::Itarget1(x, Data, yrsmth = yrsmth, xx=xx, Imulti=Imulti, ...)
  Rec
}
# 7. Assign function to class `MP`
class(ITarget_2) <- 'MP'


# openMSE Assessment-based CMPs ----

## Surplus production models ----

#' ASPIC-like Surplus Production Assessment Model
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
#' @describeIn SP_1 TAC set to estimated MSY
#' @export
SP_1 <- function(x, Data, Data_Lag=2, Interval=3, ...) {
  Rec <- new('Rec')

  Initial_MP_Yr <- 2024
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    return(Rec)
  }

  Data <- Lag_Data(Data, Data_Lag)

  # apply assessment model
  Mod <- SAMtool::SP(x, Data, fix_dep=TRUE, start=list(dep=0.85))

  # harvest control rule - TAC = MSY
  Rec@TAC <- Mod@MSY
  Rec
}
class(SP_1) <- 'MP'


#' @describeIn SP_1 Same as `SP_1` but calculates TAC by applying estimated F_MSY to estimated biomass
#' @export
SP_2 <- function(x, Data, Data_Lag=2, Interval=3, ...) {
  Rec <- new('Rec')

  Initial_MP_Yr <- 2024
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    return(Rec)
  }

  Data <- Lag_Data(Data, Data_Lag)

  # apply assessment model
  Mod <- SAMtool::SP(x, Data, fix_dep=TRUE, start=list(dep=0.85))

  # harvest control rule - apply estimated F_MSY to estimate biomass
  M <- 0.2 # assumed natural mortality
  Fmort <- Mod@FMSY
  Z <- M+Fmort
  VB <- as.numeric(Mod@VB[length(Mod@VB)])

  Rec@TAC <-  Fmort/Z*(1-exp(-Z))*VB
  Rec
}
class(SP_2) <- 'MP'

#' @describeIn SP_1 Same as `SP_1` but calculates TAC based on the harvest control rule proposed for North Atlantic Albacore
#' @export
SP_3 <- function(x, Data, Data_Lag=2, Interval=3, ...) {
  Rec <- new('Rec')

  Initial_MP_Yr <- 2024
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    return(Rec)
  }

  Data <- Lag_Data(Data, Data_Lag)

  # apply assessment model
  Mod <- SAMtool::SP(x, Data, fix_dep=TRUE, start=list(dep=0.85))

  # harvest control rule
  # based on: https://www.iccat.int/Documents/Recs/compendiopdf-e/2017-04-e.pdf
  Bthresh <- Mod@BMSY
  Blim <- 0.4 * Bthresh
  Ftar <- 0.8 * Mod@FMSY
  Fmin <- 0.1 * Mod@FMSY
  Bcurr <- Mod@B[length(Mod@B)]

  if (Bcurr>=Bthresh) {
    Fmort <- Ftar
  } else if (Bcurr>Blim) {
    Fmort <- Mod@FMSY * (-0.367 + 1.167*  Bcurr/Bthresh)
  } else {
    Fmort <- Fmin
  }

  M <- 0.2 # assumed natural mortality
  Z <- M+Fmort
  Rec@TAC <-  Fmort/Z*(1-exp(-Z))*Bcurr
  Rec
}
class(SP_3) <- 'MP'


## Surplus production Fox model ----
#' @describeIn SP_1 Same as `SP_1` but uses the Fox assumptions of the Fox model
#' @export
SP_Fox_1 <- function(x, Data, Data_Lag=2, Interval=3, ...) {
  Rec <- new('Rec')

  Initial_MP_Yr <- 2024
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    return(Rec)
  }

  Data <- Lag_Data(Data, Data_Lag)

  # apply assessment model
  Mod <- SAMtool::SP_Fox(x, Data, fix_dep=TRUE, start=list(dep=0.85))

  # harvest control rule - TAC = MSY
  Rec@TAC <- Mod@MSY
  Rec
}
class(SP_Fox_1) <- 'MP'


#' @describeIn SP_1 Same as `SP_Fox_1` but calculates TAC by applying estimated F_MSY to estimated biomass
#' @export
SP_Fox_2 <- function(x, Data, Data_Lag=2, Interval=3, ...) {
  Rec <- new('Rec')

  Initial_MP_Yr <- 2024
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    return(Rec)
  }

  Data <- Lag_Data(Data, Data_Lag)

  # apply assessment model
  Mod <- SAMtool::SP_Fox(x, Data, fix_dep=TRUE, start=list(dep=0.85))

  # harvest control rule - apply estimated F_MSY to estimate biomass
  M <- 0.2 # assumed natural mortality
  Fmort <- Mod@FMSY
  Z <- M+Fmort
  VB <- as.numeric(Mod@VB[length(Mod@VB)])

  Rec@TAC <-  Fmort/Z*(1-exp(-Z))*VB

  Rec
}
class(SP_Fox_2) <- 'MP'

#' @describeIn SP_1 Same as `SP_Fox_1` but calculates TAC based on the harvest control rule proposed for North Atlantic Albacore
#' @export
SP_Fox_3 <- function(x, Data, Data_Lag=2, Interval=3, ...) {
  Rec <- new('Rec')

  Initial_MP_Yr <- 2024
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    return(Rec)
  }

  Data <- Lag_Data(Data, Data_Lag)

  # apply assessment model
  Mod <- SAMtool::SP_Fox(x, Data, fix_dep=TRUE, start=list(dep=0.85))

  # harvest control rule
  # based on: https://www.iccat.int/Documents/Recs/compendiopdf-e/2017-04-e.pdf
  Bthresh <- Mod@BMSY
  Blim <- 0.4 * Bthresh
  Ftar <- 0.8 * Mod@FMSY
  Fmin <- 0.1 * Mod@FMSY
  Bcurr <- Mod@B[length(Mod@B)]

  if (Bcurr>=Bthresh) {
    Fmort <- Ftar
  } else if (Bcurr>Blim) {
    Fmort <- Mod@FMSY * (-0.367 + 1.167*  Bcurr/Bthresh)
  } else {
    Fmort <- Fmin
  }

  M <- 0.2 # assumed natural mortality
  Z <- M+Fmort
  Rec@TAC <-  Fmort/Z*(1-exp(-Z))*Bcurr

  Rec
}
class(SP_Fox_3) <- 'MP'




## State-Space Surplus production ----

#' @describeIn SP_1 State-space version of `SP_1` but calculates TAC by applying estimated F_MSY to estimated biomass
#' @export
SP_SS_1 <- function(x, Data, Data_Lag=2, Interval=3, ...) {
  Rec <- new('Rec')

  Initial_MP_Yr <- 2024
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    return(Rec)
  }

  Data <- Lag_Data(Data, Data_Lag)

  # apply assessment model
  Mod <- SAMtool::SP_SS(x, Data, fix_dep=TRUE, start=list(dep=0.85, sigma=0.2))

  # harvest control rule - TAC = MSY
  Rec@TAC <- Mod@MSY
  Rec
}
class(SP_SS_1) <- 'MP'

#' @describeIn SP_1 Same as `SP_SS_1` but
#' @export
SP_SS_2 <- function(x, Data, Data_Lag=2, Interval=3, ...) {
  Rec <- new('Rec')

  Initial_MP_Yr <- 2024
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    return(Rec)
  }

  Data <- Lag_Data(Data, Data_Lag)

  # apply assessment model
  Mod <- SAMtool::SP_SS(x, Data, fix_dep=TRUE, start=list(dep=0.85, sigma=0.2))

  # harvest control rule - apply estimated F_MSY to estimate biomass
  M <- 0.2 # assumed natural mortality
  Fmort <- Mod@FMSY
  Z <- M+Fmort
  VB <- as.numeric(Mod@VB[length(Mod@VB)])

  Rec@TAC <-  Fmort/Z*(1-exp(-Z))*VB
  Rec
}
class(SP_SS_2) <- 'MP'

#' @describeIn SP_1 Same as `SP_SS_1` but calculates TAC based on the harvest control rule proposed for North Atlantic Albacore
#' @export
SP_SS_3 <- function(x, Data, Data_Lag=2, Interval=3, ...) {
  Rec <- new('Rec')

  Initial_MP_Yr <- 2024
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    return(Rec)
  }

  Data <- Lag_Data(Data, Data_Lag)

  # apply assessment model
  Mod <- SAMtool::SP_Fox(x, Data, fix_dep=TRUE, start=list(dep=0.85))

  # harvest control rule
  # based on: https://www.iccat.int/Documents/Recs/compendiopdf-e/2017-04-e.pdf
  Bthresh <- Mod@BMSY
  Blim <- 0.4 * Bthresh
  Ftar <- 0.8 * Mod@FMSY
  Fmin <- 0.1 * Mod@FMSY
  Bcurr <- Mod@B[length(Mod@B)]

  if (Bcurr>=Bthresh) {
    Fmort <- Ftar
  } else if (Bcurr>Blim) {
    Fmort <- Mod@FMSY * (-0.367 + 1.167*  Bcurr/Bthresh)
  } else {
    Fmort <- Fmin
  }

  M <- 0.2 # assumed natural mortality
  Z <- M+Fmort
  Rec@TAC <-  Fmort/Z*(1-exp(-Z))*Bcurr
  Rec
}
class(SP_SS_3) <- 'MP'


# Assessments from other packages ----


#' SPiCT Assessment Model with TAC = MSY
#'
#' An example model-based CMP that uses the `MSEextra::spict` function as a wrapper
#' to the `spict` package to assess the stock.
#' It sets the TAC = estimated MSY.
#'
#' Note: this MP requires the `MSEextra` package to be installed. Run `MSEextra()` and
#' then `library(MSEextra)`
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
SPICT_1 <- function(x, Data, Data_Lag=2, Interval=3, ...) {

  chk <- require(MSEextra, quietly=TRUE)
  if (!chk) stop('package `MSEextra` must be installed. Use `MSEextra()`')

  Rec <- new('Rec')

  Initial_MP_Yr <- 2024
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    return(Rec)
  }

  Data <- Lag_Data(Data, Data_Lag)

  # apply assessment model
  Mod <- MSEextra::spict(x, Data, fix_dep=TRUE, start=list(dep=0.85))

  # harvest control rule - TAC = MSY
  Rec@TAC <- Mod@MSY
  Rec


}
class(SPICT_1) <- 'MP'


#' JABBA Assessment Model with TAC = MSY
#'
#' An example model-based CMP that uses the `JABBA` package to assess the stock.
#' It sets the TAC = estimated MSY.
#'
#' Note 1: this MP requires the `JABBA` and `rjags` packages to be installed, and
#' the JAGS software installed on your machine.
#'
#' Note 2: the JABBA model takes a long time to run in closed-loop, and sometimes
#' crashes. Recommend testing with a small number of simulations (`MOM@nsim <=5`)
#' first!
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

  Initial_MP_Yr <- 2024
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    return(Rec)
  }

  Data <- Lag_Data(Data, Data_Lag)

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





