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

MCC2 <- function(x, Data, Data_Lag=1, Interval=3, tunepar=1, mc=NA, ...) {
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

  # Combined Index averaged for 2018 - 2020
  Ibase <- mean(Data@Ind[x, match(2018:2020, Data@Year)], na.rm=TRUE)

  # combined index averaged over last available 3 # years in time-series (y-4, y-3, y-2)
  Icurr <- mean(tail(Data@Ind[x,],3))

  Irat <- Icurr/Ibase

  # in version 2 changed the Irat up increase threshold to 1.20 from 1.25

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
    deltaTAC <- 0.5
  }

  # look at a fixed TAC for really low Irat values

  TAC <- TACbase * deltaTAC
  # Maximum allowed change in TAC
  Rec@TAC <- MaxChange(TAC, Data@MPrec[x], mc)

  Rec
}



MCC3 <- function(x, Data, Data_Lag=1, Interval=3, tunepar=1, mc=NA, ...) {
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
  # MCC3 differs from MCC2 by using 2017-2019 instead of 2018-2020 as base years
  Ibase <- mean(Data@Ind[x, match(2017:2019, Data@Year)], na.rm=TRUE)

  # look at a fixed TAC for really low Irat values
  # think about a smoother

  # combined index averaged over last available 3 # years in time-series (y-4, y-3, y-2)
  Icurr <- mean(tail(Data@Ind[x,],3))

  Irat <- Icurr/Ibase

  # in version 2 changed the Irat up increase threshold to 1.20 from 1.25

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
    deltaTAC <- 0.5
  }

  TAC <- TACbase * deltaTAC
  # Maximum allowed change in TAC
  Rec@TAC <- MaxChange(TAC, Data@MPrec[x], mc)

  Rec
}



MCC4 <- function(x, Data, Data_Lag=1, Interval=3, tunepar=1, mc=NA, ...) {
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
  # MCC4 differs from MCC2 by using 2017-2019 instead of 2018-2020 as base years
  # and also adds a smoother to Icurr and Ibase

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
    deltaTAC <- 0.5
  }

  TAC <- TACbase * deltaTAC
  # Maximum allowed change in TAC
  Rec@TAC <- MaxChange(TAC, Data@MPrec[x], mc)

  Rec
}



MCC5 <- function(x, Data, Data_Lag=1, Interval=3, tunepar=1, mc=NA, ...) {
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
  # and also adds a Fixed TAC of 5,000 for very low Irat values (below 0.5)

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
    fixed_low_TAC <- 5000
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



GSC2 <- function(x, Data, Data_Lag=1, Interval=3, tunepar=1, mc=NA, ...) {
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
  TACbase <- 13200 * tunepar

  # combined index averaged over last available 3 # years in time-series (y-4, y-3, y-2)
  Icurr <- mean(tail(Data@Ind[x,],3))

  # combined index averaged over 3-yr prior to Icurr
  # updated in version 2 to change this to a set 3-yr historical period in version 1 it was a 3yr moving period just before Icurr
  # Iprev <- mean(tail(Data@Ind[x,],6)[1:3])
  Ibase <- mean(Data@Ind[x, match(2018:2020, Data@Year)], na.rm=TRUE)

  # Irat <- Icurr/Iprev
  Irat <- Icurr/Ibase

  # version 1 had the no TAC change range going from 0.95 to 1.05
  # V2 has this going from 0.90 to 1.10 also changed lowest value to 0.75 from 0.80
  # V2 also has another range of Irat below 0.75

  TAC_add <- 0
  if (Irat>=1.20) {
    deltaTAC <- 1.2
  }
  if (Irat>=1.10 & Irat <1.20) {
    deltaTAC <- 1
    TAC_add <- 1000
  }
  if (Irat>=0.90 & Irat <1.10) {
    deltaTAC <- 1
  }
  if (Irat>=0.75 & Irat <0.90) {
    deltaTAC <- 1
    TAC_add <- -1000
  }
  if (Irat>=0.50 & Irat <0.75) {
    deltaTAC <- 0.625
  }
  if (Irat<0.50) {
    deltaTAC <- 0.50
  }

  # TAC <- Data@MPrec[x] * tunepar * deltaTAC + TAC_add
  # in version 1 tuning parameter was as above using previous TAC
  # this caused a gradually declining TAC though as tuning paramter dragged it down
  # Version 2 uses the base TAC idea to set a base TAC and then have rules modify that

  TAC <- TACbase * deltaTAC + TAC_add
  # Maximum allowed change in TAC
  Rec@TAC <- MaxChange(TAC, Data@MPrec[x], mc)

  Rec
}

