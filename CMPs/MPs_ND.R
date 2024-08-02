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


MCC85a <- function(x, Data, Data_Lag=2, Interval=3, tunepar=1, mc=NA, ...) {
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

MCC85b <- function(x, Data, Data_Lag=2, Interval=3, tunepar=1, mc=NA, ...) {
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
  # MCC85b adds two more upper steps for Irat values 1.70-1.80 and 1.80-1.90

  fixed_low_TAC <- NULL  # initialize

  if (Irat>=1.90) {
    deltaTAC <- 1.90
  }
  if (Irat>=1.80 & Irat<1.90) {
    deltaTAC <- 1.80
  }
  if (Irat>=1.70 & Irat<1.80) {
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


MCC97a <- function(x, Data, Data_Lag=2, Interval=3, tunepar=1, mc=NA, ...) {
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
  # MCC97a now has 12 steps after adding upper steps on to MCC7
  # MCC97a originally tried 10 steps but needed more to avoid being on the top step right away

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

MCC97b <- function(x, Data, Data_Lag=2, Interval=3, tunepar=1, mc=NA, ...) {
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
  # MCC97b now has 11 steps as one of the first up steps was removed from MCC97a
  # MCC97a originally tried 10 steps but needed more to avoid being on the top step right away


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
  # MCC97b has modifications to the first up step
  # the first up step now goes Irat 1.15 to 1.25 and has a deltaTAC of 1.15

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

MCC97c <- function(x, Data, Data_Lag=2, Interval=3, tunepar=1, mc=NA, ...) {
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
#' @describeIn MCC5 Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
MCC5_b <- MCC5
formals(MCC5_b)$tunepar <- 0.909053789347635 # 0.93518504110836
class(MCC5_b) <- "MP"


#' @describeIn MCC5 Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
MCC5_c <- MCC5
formals(MCC5_c)$tunepar <- 0.912138649369481
class(MCC5_c) <- "MP"


#' @describeIn MCC5 Tuned to PGK_med = 0.6 across Reference OMs.
#' @export
MCC5_d <- MCC5
formals(MCC5_d)$tunepar <- 0.912121559336078
class(MCC5_d) <- "MP"


#' @describeIn MCC5 Tuned to PGK_long = 0.6 across Reference OMs.
#' @export
MCC5_e <- MCC5
formals(MCC5_e)$tunepar <- 0.909053789347635
class(MCC5_e) <- "MP"


#' @describeIn MCC7 Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
MCC7_b <- MCC7
formals(MCC7_b)$tunepar <- 0.848197803454192
class(MCC7_b) <- "MP"


#' @describeIn MCC7 Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
MCC7_c <- MCC7
formals(MCC7_c)$tunepar <- 0.825228531069442
class(MCC7_c) <- "MP"


#' @describeIn MCC7 Tuned to PGK_med = 0.6 across Reference OMs.
#' @export
MCC7_d <- MCC7
formals(MCC7_d)$tunepar <- 0.858852481337904
class(MCC7_d) <- "MP"


#' @describeIn MCC7 Tuned to PGK_long = 0.6 across Reference OMs.
#' @export
MCC7_e <- MCC7
formals(MCC7_e)$tunepar <- 0.870842035607629
class(MCC7_e) <- "MP"


#' @describeIn MCC85 Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
MCC85_b <- MCC85
formals(MCC85_b)$tunepar <- 0.748365885873977
class(MCC85_b) <- "MP"


#' @describeIn MCC85 Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
MCC85_c <- MCC85
formals(MCC85_c)$tunepar <- 0.720037451935572
class(MCC85_c) <- "MP"


#' @describeIn MCC85 Tuned to PGK_med = 0.6 across Reference OMs.
#' @export
MCC85_d <- MCC85
formals(MCC85_d)$tunepar <- 0.816209907433701
class(MCC85_d) <- "MP"


#' @describeIn MCC85 Tuned to PGK_long = 0.6 across Reference OMs.
#' @export
MCC85_e <- MCC85
formals(MCC85_e)$tunepar <- 0.840689429618001
class(MCC85_e) <- "MP"




#' @describeIn MCC97 Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
MCC97_b <- MCC97
formals(MCC97_b)$tunepar <- 0.763751899023205
class(MCC97_b) <- "MP"


#' @describeIn MCC97 Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
MCC97_c <- MCC97
formals(MCC97_c)$tunepar <- 0.738198212076526
class(MCC97_c) <- "MP"


#' @describeIn MCC97 Tuned to PGK_med = 0.6 across Reference OMs.
#' @export
MCC97_d <- MCC97
formals(MCC97_d)$tunepar <- 0.825207022942215
class(MCC97_d) <- "MP"


#' @describeIn MCC97 Tuned to PGK_long = 0.6 across Reference OMs.
#' @export
MCC97_e <- MCC97
formals(MCC97_e)$tunepar <- 0.851371996610092
class(MCC97_e) <- "MP"


#' @describeIn MCC85a Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
MCC85a_b <- MCC85a
formals(MCC85a_b)$tunepar <- 0.748365885873977
class(MCC85a_b) <- "MP"


#' @describeIn MCC85a Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
MCC85a_c <- MCC85a
formals(MCC85a_c)$tunepar <- 0.720037451935572
class(MCC85a_c) <- "MP"


#' @describeIn MCC85a Tuned to PGK_med = 0.6 across Reference OMs.
#' @export
MCC85a_d <- MCC85a
formals(MCC85a_d)$tunepar <- 0.816209907433701
class(MCC85a_d) <- "MP"


#' @describeIn MCC85a Tuned to PGK_long = 0.6 across Reference OMs.
#' @export
MCC85a_e <- MCC85a
formals(MCC85a_e)$tunepar <- 0.840689429618001
class(MCC85a_e) <- "MP"


#' @describeIn MCC85b Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
MCC85b_b <- MCC85b
formals(MCC85b_b)$tunepar <- 0.74672448556677
class(MCC85b_b) <- "MP"


#' @describeIn MCC85b Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
MCC85b_c <- MCC85b
formals(MCC85b_c)$tunepar <- 0.71825508374241
class(MCC85b_c) <- "MP"


#' @describeIn MCC85b Tuned to PGK_med = 0.6 across Reference OMs.
#' @export
MCC85b_d <- MCC85b
formals(MCC85b_d)$tunepar <- 0.81417618727282
class(MCC85b_d) <- "MP"


#' @describeIn MCC85b Tuned to PGK_long = 0.6 across Reference OMs.
#' @export
MCC85b_e <- MCC85b
formals(MCC85b_e)$tunepar <- 0.840733458830091
class(MCC85b_e) <- "MP"


#' @describeIn MCC97a Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
MCC97a_b <- MCC97a
formals(MCC97a_b)$tunepar <- 0.763751899023205
class(MCC97a_b) <- "MP"


#' @describeIn MCC97a Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
MCC97a_c <- MCC97a
formals(MCC97a_c)$tunepar <- 0.738198212076526
class(MCC97a_c) <- "MP"


#' @describeIn MCC97a Tuned to PGK_med = 0.6 across Reference OMs.
#' @export
MCC97a_d <- MCC97a
formals(MCC97a_d)$tunepar <- 0.825207022942215
class(MCC97a_d) <- "MP"


#' @describeIn MCC97a Tuned to PGK_long = 0.6 across Reference OMs.
#' @export
MCC97a_e <- MCC97a
formals(MCC97a_e)$tunepar <- 0.851371996610092
class(MCC97a_e) <- "MP"

#' @describeIn MCC97b Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
MCC97b_b <- MCC97b
formals(MCC97b_b)$tunepar <- 0.764519101618623
class(MCC97b_b) <- "MP"


#' @describeIn MCC97b Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
MCC97b_c <- MCC97b
formals(MCC97b_c)$tunepar <- 0.73814917113238
class(MCC97b_c) <- "MP"


#' @describeIn MCC97b Tuned to PGK_med = 0.6 across Reference OMs.
#' @export
MCC97b_d <- MCC97b
formals(MCC97b_d)$tunepar <- 0.824290691886511
class(MCC97b_d) <- "MP"


#' @describeIn MCC97b Tuned to PGK_long = 0.6 across Reference OMs.
#' @export
MCC97b_e <- MCC97b
formals(MCC97b_e)$tunepar <- 0.850005530667552
class(MCC97b_e) <- "MP"

#' @describeIn MCC97c Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
MCC97c_b <- MCC97c
formals(MCC97c_b)$tunepar <- 0.756222283813747
class(MCC97c_b) <- "MP"


#' @describeIn MCC97c Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
MCC97c_c <- MCC97c
formals(MCC97c_c)$tunepar <- 0.731648818063811
class(MCC97c_c) <- "MP"


#' @describeIn MCC97c Tuned to PGK_med = 0.6 across Reference OMs.
#' @export
MCC97c_d <- MCC97c
formals(MCC97c_d)$tunepar <- 0.809388076093262
class(MCC97c_d) <- "MP"


#' @describeIn MCC97c Tuned to PGK_long = 0.6 across Reference OMs.
#' @export
MCC97c_e <- MCC97c
formals(MCC97c_e)$tunepar <- 0.837227644681061
class(MCC97c_e) <- "MP"


