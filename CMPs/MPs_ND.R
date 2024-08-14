#' Constant Exploitation with Control Rule
#'
#' goal in this CMP would be to have the catch remain as constant as possible and only increase if the
#' index rose substantially and only decrease if the index declined substantially. The base TAC (constant
#' catch) would be 12,600 as this CC TAC that would allow PGK60 and LRP15 (ideally we could get this LRP
#' down) to be achieved.
#'
#' @param x A position in the data object
#' @param Data An object of class `Data`
#' @param Data_Lag The number of years to lag the data
#' @param Interval The TAC update interval
#' @param tunepar Parameter used for tuning
#' @param mc The maximum fractional change in the TAC among years. NA to ignore
#' @param ... Additional arguments (unused)
#'
#' @return An object of class `Rec` with the `TAC` slot populated
#'
MCC9 <- function(x, Data, Data_Lag=2, Interval=3, tunepar=1, mc=NA, ...) {
  Rec <- new('Rec')

  # Does TAC need to be updated? (or set a fixed catch if before Initial_MP_Yr)
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    Rec <- FixedTAC(Rec, Data) # use actual catches if they are available
    return(Rec)
  }

  # Lag Data
  Data <- Lag_Data(Data, Data_Lag)

  # Base TAC
  TACbase <- 12600 * tunepar

  # Combined Index averaged for 2017 - 2019
  # MCC85 differs from MCC2 by using 2017-2019 instead of 2018-2020 as base years
  # and also adds a Fixed TAC of 4,000 for very low Irat values (below 0.5)
  # and it differs from MCC5 by adding more steps in the higher range of Irat

  Ibase <- mean(Data@Ind[x, match(2017:2019, Data@Year)], na.rm=TRUE)

  # combined index averaged over last available 3 # years in time-series (y-4, y-3, y-2)
  Icurr <- mean(tail(Data@Ind[x,],3))

  Irat <- Icurr/Ibase

  # in version 2 MCC changed the Irat up increase threshold to 1.20 from 1.25

  fixed_low_TAC <- NULL  # initialize

  if (Irat>=1.70) {
    deltaTAC <- 1.70
  }
  if (Irat>=1.60 & Irat<1.70) {
    deltaTAC <- 1.60
  }
  if (Irat>=1.50 & Irat<1.60) {
    deltaTAC <- 1.50
  }
  if (Irat>=1.40 & Irat<1.50) {
    deltaTAC <- 1.40
  }
  if (Irat>=1.30 & Irat<1.40) {
    deltaTAC <- 1.30
  }
  if (Irat>=1.20 & Irat<1.30) {
    deltaTAC <- 1.20
  }
  if (Irat>=0.75 & Irat<1.20) {
    deltaTAC <- 1
  }
  if (Irat>=0.5 & Irat<0.75) {
    deltaTAC <- 0.75
  }
  if (Irat<0.5) {
    # deltaTAC <- 0.5
    fixed_low_TAC <- 4000
  }

  if (is.null(fixed_low_TAC)) {
    # if fixed_low_TAC hasn't been assigned a value
    TAC <- TACbase * deltaTAC
  } else {
    TAC <- fixed_low_TAC
  }

  # Maximum allowed change in TAC
  Rec@TAC <- MaxChange(TAC, Data@MPrec[x], mc)

  Rec
}


MCC11 <- function(x, Data, Data_Lag=2, Interval=3, tunepar=1, mc=NA, ...) {
  Rec <- new('Rec')

  # Does TAC need to be updated? (or set a fixed catch if before Initial_MP_Yr)
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    Rec <- FixedTAC(Rec, Data) # use actual catches if they are available
    return(Rec)
  }

  # Lag Data
  Data <- Lag_Data(Data, Data_Lag)

  # Base TAC
  TACbase <- 12600 * tunepar

  # Combined Index averaged for 2017 - 2019
  # MCC7 differs from MCC2 by using 2017-2019 instead of 2018-2020 as base years
  # Also more deltaTAC modifiers were added for various Irat levels
  # it differs from MCC7 by adding even more steps for the higher Irat levels.
  # MCC97b now has 11 steps as one of the first up steps was removed from MCC97a
  # MCC97c maintains that removal and increased number of steps of MCC97b
  # MCC97c differs from MCC97b in that the smoother was removed

  Ibase <- mean(Data@Ind[x, match(2017:2019, Data@Year)], na.rm=TRUE)

  # combined index averaged over last available 3 # years in time-series (y-4, y-3, y-2)
  Icurr <- mean(tail(Data@Ind[x,],3))

  Irat <- Icurr/Ibase

  # MCC97 adds more upper steps for Irat values compared to MCC7

  fixed_low_TAC <- NULL  # initialize

  if (Irat>=1.85) {
    deltaTAC <- 1.85
  }
  if (Irat>=1.75 & Irat<1.85) {
    deltaTAC <- 1.75
  }
  if (Irat>=1.65 & Irat<1.75) {
    deltaTAC <- 1.65
  }
  if (Irat>=1.55 & Irat<1.65) {
    deltaTAC <- 1.55
  }
  if (Irat>=1.45 & Irat<1.55) {
    deltaTAC <- 1.45
  }
  if (Irat>=1.35 & Irat<1.45) {
    deltaTAC <- 1.35
  }
  if (Irat>=1.25 & Irat<1.35) {
    deltaTAC <- 1.25
  }
  if (Irat>=1.15 & Irat<1.25) {
    deltaTAC <- 1.15
  }
  if (Irat>=0.75 & Irat<1.15) {
    deltaTAC <- 1
  }
  if (Irat>=0.5 & Irat<0.75) {
    deltaTAC <- 0.75
  }
  if (Irat<0.5) {
    deltaTAC <- 0.5
  }

  TAC <- TACbase * deltaTAC
  # Maximum allowed change in TAC
  Rec@TAC <- MaxChange(TAC, Data@MPrec[x], mc)

  Rec
}

# ---- Tuned CMPs ----

#' @describeIn MCC9 Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
MCC9_b <- MCC9
formals(MCC9_b)$tunepar <- 0.748365885873977
class(MCC9_b) <- "MP"


#' @describeIn MCC9 Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
MCC9_c <- MCC9
formals(MCC9_c)$tunepar <- 0.720037451935572
class(MCC9_c) <- "MP"


#' @describeIn MCC9 Tuned to PGK_med = 0.6 across Reference OMs.
#' @export
MCC9_d <- MCC9
formals(MCC9_d)$tunepar <- 0.816209907433701
class(MCC9_d) <- "MP"


#' @describeIn MCC9 Tuned to PGK_long = 0.6 across Reference OMs.
#' @export
MCC9_e <- MCC9
formals(MCC9_e)$tunepar <- 0.840689429618001
class(MCC9_e) <- "MP"

#' @describeIn MCC11 Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
MCC11_b <- MCC11
formals(MCC11_b)$tunepar <- 0.756222283813747
class(MCC11_b) <- "MP"


#' @describeIn MCC11 Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
MCC11_c <- MCC11
formals(MCC11_c)$tunepar <- 0.731648818063811
class(MCC11_c) <- "MP"


#' @describeIn MCC11 Tuned to PGK_med = 0.6 across Reference OMs.
#' @export
MCC11_d <- MCC11
formals(MCC11_d)$tunepar <- 0.809388076093262
class(MCC11_d) <- "MP"


#' @describeIn MCC11 Tuned to PGK_long = 0.6 across Reference OMs.
#' @export
MCC11_e <- MCC11
formals(MCC11_e)$tunepar <- 0.837227644681061
class(MCC11_e) <- "MP"


