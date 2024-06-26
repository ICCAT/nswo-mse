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



MCC5 <- function(x, Data, Data_Lag=2, Interval=3, tunepar=1, mc=NA, ...) {
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
  # MCC5 differs from MCC2 by using 2017-2019 instead of 2018-2020 as base years
  # and also adds a Fixed TAC of 4,000 for very low Irat values (below 0.5)

  Ibase <- mean(Data@Ind[x, match(2017:2019, Data@Year)], na.rm=TRUE)

  # combined index averaged over last available 3 # years in time-series (y-4, y-3, y-2)
  Icurr <- mean(tail(Data@Ind[x,],3))

  Irat <- Icurr/Ibase

  # in version 2 MCC changed the Irat up increase threshold to 1.20 from 1.25

  fixed_low_TAC <- NULL  # initialize

  if (Irat>=1.20) {
    deltaTAC <- 1.2
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




MCC7 <- function(x, Data, Data_Lag=2, Interval=3, tunepar=1, mc=NA, ...) {
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
  # and also adds a smoother to Icurr and Ibase
  # Also more deltaTAC modifiers were addedfor various Irat levels

  # smooth combined index
  index <- smoothed_index <- Data@Ind[x,]
  smoothed <- stats::smooth(index[!is.na(index)], kind=)
  smoothed_index[!is.na(smoothed_index)] <- smoothed

  Data@Ind[x,] <- smoothed_index # replace the index values with smoothed values

  Ibase <- mean(Data@Ind[x, match(2017:2019, Data@Year)], na.rm=TRUE)

  # Add an index smoother to both Icurr and Ibase

  # combined index averaged over last available 3 # years in time-series (y-4, y-3, y-2)
  Icurr <- mean(tail(Data@Ind[x,],3))

  Irat <- Icurr/Ibase

  # in version 2 MCC changed the Irat up increase threshold to 1.20 from 1.25
  # In version 7 MCC has added more increases in TAC when Irat is higher by adding several
  # levels when Irat is above 1.15

  if (Irat>=1.35) {
    deltaTAC <- 1.35
  }
  if (Irat>=1.25 & Irat<1.35) {
    deltaTAC <- 1.25
  }
  if (Irat>=1.20 & Irat<1.25) {
    deltaTAC <- 1.20
  }
  if (Irat>=1.15 & Irat<1.20) {
    deltaTAC <- 1.10
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


MCC85 <- function(x, Data, Data_Lag=2, Interval=3, tunepar=1, mc=NA, ...) {
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

MCC97 <- function(x, Data, Data_Lag=2, Interval=3, tunepar=1, mc=NA, ...) {
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
  # and also adds a smoother to Icurr and Ibase
  # Also more deltaTAC modifiers were added for various Irat levels
  # it differs from MCC7 by adding even more steps for the higher Irat levels.
  # now it has 10 steps

  # smooth combined index
  index <- smoothed_index <- Data@Ind[x,]
  smoothed <- stats::smooth(index[!is.na(index)], kind=)
  smoothed_index[!is.na(smoothed_index)] <- smoothed

  Data@Ind[x,] <- smoothed_index # replace the index values with smoothed values

  Ibase <- mean(Data@Ind[x, match(2017:2019, Data@Year)], na.rm=TRUE)

  # Add an index smoother to both Icurr and Ibase

  # combined index averaged over last available 3 # years in time-series (y-4, y-3, y-2)
  Icurr <- mean(tail(Data@Ind[x,],3))

  Irat <- Icurr/Ibase

  # in version 2 MCC changed the Irat up increase threshold to 1.20 from 1.25
  # In version 7 MCC has added more increases in TAC when Irat is higher by adding several
  # levels when Irat is above 1.15

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
  if (Irat>=1.20 & Irat<1.25) {
    deltaTAC <- 1.20
  }
  if (Irat>=1.15 & Irat<1.20) {
    deltaTAC <- 1.10
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
#' @describeIn MCC5 Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
MCC5_b <- MCC5
formals(MCC5_b)$tunepar <- 0.860732587877038# 0.869592831042283
class(MCC5_b) <- "MP"


#' @describeIn MCC5 Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
MCC5_c <- MCC5
formals(MCC5_c)$tunepar <- 0.833008192840919
class(MCC5_c) <- "MP"


#' @describeIn MCC5 Tuned to PGK_med = 0.6 across Reference OMs.
#' @export
MCC5_d <- MCC5
formals(MCC5_d)$tunepar <- 0.854893984366685
class(MCC5_d) <- "MP"


#' @describeIn MCC5 Tuned to PGK_long = 0.6 across Reference OMs.
#' @export
MCC5_e <- MCC5
formals(MCC5_e)$tunepar <- 0.860732587877038
class(MCC5_e) <- "MP"


#' @describeIn MCC7 Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
MCC7_b <- MCC7
formals(MCC7_b)$tunepar <- 0.790940555357638
class(MCC7_b) <- "MP"


#' @describeIn MCC7 Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
MCC7_c <- MCC7
formals(MCC7_c)$tunepar <- 0.758061767879461
class(MCC7_c) <- "MP"


#' @describeIn MCC7 Tuned to PGK_med = 0.6 across Reference OMs.
#' @export
MCC7_d <- MCC7
formals(MCC7_d)$tunepar <- 0.801124845883973
class(MCC7_d) <- "MP"


#' @describeIn MCC7 Tuned to PGK_long = 0.6 across Reference OMs.
#' @export
MCC7_e <- MCC7
formals(MCC7_e)$tunepar <- 0.818304110551717
class(MCC7_e) <- "MP"


#' @describeIn MCC85 Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
MCC85_b <- MCC85
formals(MCC85_b)$tunepar <- 0.701946328856892
class(MCC85_b) <- "MP"


#' @describeIn MCC85 Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
MCC85_c <- MCC85
formals(MCC85_c)$tunepar <- 0.667250275522346
class(MCC85_c) <- "MP"


#' @describeIn MCC85 Tuned to PGK_med = 0.6 across Reference OMs.
#' @export
MCC85_d <- MCC85
formals(MCC85_d)$tunepar <- 0.755673913269389
class(MCC85_d) <- "MP"


#' @describeIn MCC85 Tuned to PGK_long = 0.6 across Reference OMs.
#' @export
MCC85_e <- MCC85
formals(MCC85_e)$tunepar <- 0.774632636202341
class(MCC85_e) <- "MP"


#' @describeIn MCC97 Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
MCC97_b <- MCC97
formals(MCC97_b)$tunepar <- 0.716428765771338
class(MCC97_b) <- "MP"


#' @describeIn MCC97 Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
MCC97_c <- MCC97
formals(MCC97_c)$tunepar <- 0.682911779252167
class(MCC97_c) <- "MP"


#' @describeIn MCC97 Tuned to PGK_med = 0.6 across Reference OMs.
#' @export
MCC97_d <- MCC97
formals(MCC97_d)$tunepar <- 0.761342860692875
class(MCC97_d) <- "MP"


#' @describeIn MCC97 Tuned to PGK_long = 0.6 across Reference OMs.
#' @export
MCC97_e <- MCC97
formals(MCC97_e)$tunepar <- 0.78357744210698
class(MCC97_e) <- "MP"


