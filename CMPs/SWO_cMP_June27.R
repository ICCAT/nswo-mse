#' Index Ratio 1
#'
#' This MP adjusts the TAC based on ratio of the mean index over the last 3 years (y-2):y
#' to the mean index over the 3 years before that (y-5):(y-3)
#'
#' @param x A position in the data object
#' @param Data An object of class `Data`
#' @param Data_Lag The number of years to lag the data
#' @param Interval The TAC update interval
#' @param tunepar Parameter used for tuning
#' @param mc The maximum fractional change in the TAC among years. NA to ignore
#' @param yrs numeric vector length 2. The years to calculate the mean index. Ratio is
#' calculated as mean(ind[yrs[1]])/mean(ind[yrs[2]])
#' @param ... Additional arguments (unused)
#'
#' @return An object of class `Rec` with the `TAC` slot populated
#'
#Data = MSE2$Data SWOData RefMSEList[[1]]$Data
SP2 <- function(x, Data, Index_ID=1, Data_Lag=2, Interval=3, tunepar=1, mc=0.25,
                useCombined = FALSE,
                yrsmth = 3, ...) {
  Rec <- new('Rec')

  # Load R package forecast
  library(forecast)

  # Does TAC need to be updated? (or set a fixed catch if before Initial_MP_Yr)
  Initial_MP_Yr <- 2024
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    Rec <- FixedTAC(Rec, Data) # use actual catches if they are available
    return(Rec)
  }

  # Lag Data
  Data <- Lag_Data(Data, Data_Lag)

  # Calculate Index Ratio
  # number of years of index data
  # Historical data starts in 1950 and ends in 2020 (71 years)
  n_years <- length(Data@AddInd[x,Index_ID,])

  # the year index for `yrsmth` most recent years
  yr_ind <- 2020+((max(1, n_years-yrsmth+1):n_years)-71)

  # the year index for target years (2016 to 2020)
  yr_ind_tar <- 2015:2020 #67:71

 #for(x in 1:50){
  if(useCombined==FALSE){
    # get the index and fit and exponential smoothing state space model
    Index_raw = ts(Data@AddInd[x,Index_ID,],start=1950,
                   end=2020+(n_years-71), frequency = 1)
    Index_raw = na.interp(Index_raw)
    mymodel = auto.arima(Index_raw)
    myforecast <- forecast(mymodel, level=c(50), h=3)

    # index target
    Ind_Target <- mean(myforecast$mean, na.rm=TRUE)
    if(Ind_Target<min(myforecast$x[1:71], na.rm=TRUE)) {
      Ind_Target = min(myforecast$x[1:71], na.rm=TRUE)
    }
    # ratio of mean recent index to Ind_Target
    deltaI <- (mean(myforecast$x[1:71], na.rm=TRUE)/Ind_Target)
    #print(deltaI)
  }else{
    # get the index and fit and exponential smoothing state space model
    Index_raw = ts(Data@Ind[x,],start=1950,
                   end=2020+(n_years-71), frequency = 1)
    Index_raw = na.interp(Index_raw)
    mymodel = auto.arima(Index_raw)
    myforecast <- forecast(mymodel, level=c(50), h=3)

    # index target
    Ind_Target <- mean(myforecast$mean, na.rm=TRUE)
    if(Ind_Target<min(myforecast$x[1:71], na.rm=TRUE)) {
      Ind_Target = min(myforecast$x[1:71], na.rm=TRUE)
    }
    # ratio of mean recent index to Ind_Target
    deltaI <- (mean(myforecast$x[1:71], na.rm=TRUE)/Ind_Target)
    #print(deltaI)
  }
  #}

  if(is.na(deltaI)) deltaI = 1

  deltaI = exp(log(deltaI)*.1)

  # max/min change in TAC
  if (deltaI < (1 - mc)) deltaI <- 1 - mc
  if (deltaI > (1 + mc)) deltaI <- 1 + mc

  Rec@TAC <- Data@MPrec[x] * deltaI * tunepar
  # 5. Return the `Rec` object
  Rec
}
class(SP2) <- 'MP'
########################################################################
SP4 <- function(x, Data, Index_ID=1, Data_Lag=2, Interval=3, tunepar=0.892, mc=0.25,
                useCombined = FALSE,
                yrsmth = 3, ...) {
  Rec <- new('Rec')

  # Load R package forecast
  library(forecast)

  # Does TAC need to be updated? (or set a fixed catch if before Initial_MP_Yr)
  Initial_MP_Yr <- 2024
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    Rec <- FixedTAC(Rec, Data) # use actual catches if they are available
    return(Rec)
  }

  # Lag Data
  Data <- Lag_Data(Data, Data_Lag)

  # Calculate Index Ratio
  # number of years of index data
  # Historical data starts in 1950 and ends in 2020 (71 years)
  n_years <- length(Data@AddInd[x,Index_ID,])

  # the year index for `yrsmth` most recent years
  yr_ind <- 2020+((max(1, n_years-yrsmth+1):n_years)-71)

  # the year index for target years (2016 to 2020)
  yr_ind_tar <- 2015:2020 #67:71

  #for(x in 1:50){
  if(useCombined==FALSE){
    # get the index and fit and exponential smoothing state space model
    Index_raw = ts(Data@AddInd[x,Index_ID,],start=1950,
                   end=2020+(n_years-71), frequency = 1)
    Index_raw = na.interp(Index_raw)
    Index_raw = ets(y=Index_raw, damped=T, alpha=.2)$fitted
    mymodel = auto.arima(Index_raw)
    myforecast <- forecast(mymodel, level=c(50), h=3)

    # index target
    Ind_Cur <- mean(myforecast$mean, na.rm=TRUE)
    if(Ind_Cur<min(myforecast$x[1:71], na.rm=TRUE)) {
      Ind_Cur = min(myforecast$x[1:71], na.rm=TRUE)
    }
    # ratio of mean recent index to 1950 to 2020 series mean
    deltaI <- (Ind_Cur/mean(myforecast$x[1:71], na.rm=TRUE))
    #print(deltaI)
  }else{
    # get the index and fit and exponential smoothing state space model
    Index_raw = ts(Data@Ind[x,],start=1950,
                   end=2020+(n_years-71), frequency = 1)
    Index_raw = na.interp(Index_raw)
    Index_raw = ets(y=Index_raw, damped=T, alpha=.2)$fitted
    mymodel = auto.arima(Index_raw)
    myforecast <- forecast(mymodel, level=c(50), h=3)

    # index target
    Ind_Cur <- mean(myforecast$mean, na.rm=TRUE)
    if(Ind_Cur<min(myforecast$x[1:71], na.rm=TRUE)) {
      Ind_Cur = min(myforecast$x[1:71], na.rm=TRUE)
    }
    # ratio of mean recent index to 1950 to 2020 series mean
    deltaI <- (Ind_Cur/mean(myforecast$x[1:71], na.rm=TRUE))
    #print(deltaI)
  }
  #}

  if(is.na(deltaI)) deltaI = 1

  deltaI = exp(log(deltaI)*.1)

  # max/min change in TAC
  if (deltaI < (1 - mc)) deltaI <- 1 - mc
  if (deltaI > (1 + mc)) deltaI <- 1 + mc

  Rec@TAC <- Data@MPrec[x] * deltaI * tunepar
  # 5. Return the `Rec` object
  Rec
}
class(SP4) <- 'MP'
###################################################
C1320 <- function(x, Data, Index_ID=1, Data_Lag=2, Interval=3, tunepar=1, mc=0.25,
                useCombined = FALSE,
                yrsmth = 3, ...) {
  Rec <- new('Rec')

  # Does TAC need to be updated? (or set a fixed catch if before Initial_MP_Yr)
  Initial_MP_Yr <- 2024
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    Rec <- FixedTAC(Rec, Data) # use actual catches if they are available
    return(Rec)
  }

  # Lag Data
  Data <- Lag_Data(Data, Data_Lag)

  Rec@TAC <- 12650 * tunepar
  # 5. Return the `Rec` object
  Rec
}
class(C1320) <- 'MP'

###############################################
FX1 <- function(x, Data, Index_ID=c(1,2,3,4,5,6,7),
                Index_CV=c(.275,.322,.342,.258,.219,.205,.307),
                Data_Lag=2, Interval=3,
                PC = c(.8,.9,1,1.05,1.1),
                yrsmth = 3, mc = 0.25, tunepar=1,
                ...){
  Rec <- new('Rec')

  # Load R package forecast
  library(forecast)

  # Does TAC need to be updated? (or set a fixed catch if before Initial_MP_Yr)
  Initial_MP_Yr <- 2024
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    Rec <- FixedTAC(Rec, Data) # use actual catches if they are available
    return(Rec)
  }

  # Lag Data
  Data <- Lag_Data(Data, Data_Lag)

  # Calculate Index Ratio
  # number of years of index data
  # Historical data starts in 1950 and ends in 2002 (71 years)
  n_years <- dim(Data@AddInd[x,Index_ID,])[2]

  # the year index for `yrsmth` most recent years
  yr_ind <- 2020+((max(1, n_years-yrsmth+1):n_years)-71)

  # the year index for target years (2016 to 2020)
  yr_ind_tar <- 2015:2020 #67:71

  # get the target quantiles of the indices and current values
  Index_year = apply(Data@AddInd[x,Index_ID,],1,
                     function(x) time(ets(y=na.interp(ts(x,start=1950,
                     end=2020+(n_years-71), frequency = 1)),
                     damped=T, alpha=.2)$fitted))

  Index_TarQ = apply(Data@AddInd[x,Index_ID,Index_year[,1]%in%1950:2020],1,
                    function(x) quantile(sort(x),c(.2,.4,.6,.8),na.rm=T))
  Index_CurV = apply(Data@AddInd[x,Index_ID,],1,
                     function(x) ts(x,start=1950,end=2020+(n_years-71),
                      frequency = 1))


  # ratio of mean recent index to Ind_Target
  Index_CurV = Index_CurV[Index_year[,1]%in%yr_ind,]
  Index_CurV = apply(t(t(Index_CurV)/(1)),2,
                  function(x) mean(x, na.rm=T))
  Score = 1+apply(Index_TarQ<matrix(Index_CurV,ncol=length(Index_CurV),
                                    nrow=4,byrow=T),2,"sum")
  # ratio of mean recent index to Ind_Target
  deltaI <- mean(PC[Score],na.rm=T) * tunepar
  if(is.na(deltaI)|deltaI<0) deltaI = 1


  # max/min change in TAC
  if (deltaI <= (1 - mc)) deltaI <- 1 - mc
  if (deltaI > (1 + mc)) deltaI <- 1 + mc

  if(n_years<75){
    Rec@TAC <- Data@MPrec[x] * deltaI
  }else{Rec@TAC <- Data@Cat[x,n_years] * deltaI}
  # 5. Return the `Rec` object
  Rec
}
# 6. Assign function to class `MP`
class(FX1) <- 'MP'
############
FX2 <- function(x, Data, Index_ID=c(1,2,3,4,5,6,7),
                Index_CV=c(.275,.322,.342,.258,.219,.205,.307),
                Data_Lag=2, Interval=3,
                PC = c(.8,.90,1,1.025,1.05),
                yrsmth = 3, mc = 0.25, tunepar=1,
                ...){
  Rec <- new('Rec')

  # Load R package forecast
  library(forecast)

  # Does TAC need to be updated? (or set a fixed catch if before Initial_MP_Yr)
  Initial_MP_Yr <- 2024
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    Rec <- FixedTAC(Rec, Data) # use actual catches if they are available
    return(Rec)
  }

  # Lag Data
  Data <- Lag_Data(Data, Data_Lag)

  # Calculate Index Ratio
  # number of years of index data
  # Historical data starts in 1950 and ends in 2002 (71 years)
  n_years <- dim(Data@AddInd[x,Index_ID,])[2]

  # the year index for `yrsmth` most recent years
  yr_ind <- 2020+((max(1, n_years-yrsmth+1):n_years)-71)

  # the year index for target years (2016 to 2020)
  yr_ind_tar <- 2015:2020 #67:71

  # get the target quantiles of the indices and current values
  Index_year = apply(Data@AddInd[x,Index_ID,],1,
                     function(x) time(ets(y=na.interp(ts(x,start=1950,
                                                         end=2020+(n_years-71), frequency = 1)),
                                          damped=T, alpha=.2)$fitted))

  Index_TarQ = apply(Data@AddInd[x,Index_ID,Index_year[,1]%in%1950:max(Index_year[,1])],1,
                     function(x) quantile(sort(x),c(.2,.4,.6,.8),na.rm=T))
  Index_CurV = apply(Data@AddInd[x,Index_ID,],1,
                     function(x) ts(x,start=1950,end=2020+(n_years-71),
                                    frequency = 1))


  # ratio of mean recent index to Ind_Target
  Index_CurV = Index_CurV[Index_year[,1]%in%yr_ind,]
  Index_CurV = apply(t(t(Index_CurV)/(1)),2,
                     function(x) mean(x, na.rm=T))
  Score = 1+apply(Index_TarQ<matrix(Index_CurV,ncol=length(Index_CurV),
                                    nrow=4,byrow=T),2,"sum")
  # ratio of mean recent index to Ind_Target
  deltaI <- mean(PC[Score],na.rm=T)
  if(is.na(deltaI)|deltaI<0) deltaI = 1


  # max/min change in TAC
  if (deltaI <= (1 - mc)) deltaI <- 1 - mc
  if (deltaI > (1 + mc)) deltaI <- 1 + mc

  Rec@TAC <- (12500*tunepar) * deltaI

  # 5. Return the `Rec` object
  Rec
}
# 6. Assign function to class `MP`
class(FX2) <- 'MP'

##TUNINGS###
FZ1_b <- FX1
formals(FZ1_b)$tunepar <- 1.028
class(FZ1_b) <- "MP"#'

FZ2_b <- FX2
formals(FZ2_b)$tunepar <- 1.029
class(FZ2_b) <- "MP"#'

# ---- Tuned CMPs ----
#' @describeIn C1320 Tuned to PGK_short = 0.51 across Reference OMs.
#' @export
C1320_a <- C1320
formals(C1320_a)$tunepar <- 0.94962891987717
class(C1320_a) <- "MP"


#' @describeIn C1320 Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
C1320_b <- C1320
formals(C1320_b)$tunepar <- 0.91638901197777
class(C1320_b) <- "MP"


#' @describeIn C1320 Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
C1320_c <- C1320
formals(C1320_c)$tunepar <- 0.877126672732743
class(C1320_c) <- "MP"


#' @describeIn FX1 Tuned to PGK_short = 0.51 across Reference OMs.
#' @export
FX1_a <- FX1
formals(FX1_a)$tunepar <- 1.04085116936232
class(FX1_a) <- "MP"


#' @describeIn FX1 Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
FX1_b <- FX1
formals(FX1_b)$tunepar <- 1.01885662754416
class(FX1_b) <- "MP"


#' @describeIn FX1 Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
FX1_c <- FX1
formals(FX1_c)$tunepar <- 0.994816100079258
class(FX1_c) <- "MP"


FX2_a <- FX2
formals(FX2_a)$tunepar <- 0.972 # PGK_short 0.51 tuning
class(FX2_a) <- "MP"#'

FX2_b <- FX2
formals(FX2_b)$tunepar <- 0.939 # PGK_short 0.6 tuning
class(FX2_b) <- "MP"#'

FX2_c <- FX2
formals(FX2_c)$tunepar <- 0.902 # PGK_short 0.7 tuning
class(FX2_c) <- "MP"#'


#' @describeIn SP2 Tuned to PGK_short = 0.51 across Reference OMs.
#' @export
SP2_a <- SP2
formals(SP2_a)$tunepar <- 1.07988076899994
class(SP2_a) <- "MP"


#' @describeIn SP2 Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
SP2_b <- SP2
formals(SP2_b)$tunepar <- 1.05456111606014
class(SP2_b) <- "MP"


#' @describeIn SP2 Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
SP2_c <- SP2
formals(SP2_c)$tunepar <- 1.02864897701625
class(SP2_c) <- "MP"


#' @describeIn SP4 Tuned to PGK_short = 0.51 across Reference OMs.
#' @export
SP4_a <- SP4
formals(SP4_a)$tunepar <- 1.04579842695447
class(SP4_a) <- "MP"


#' @describeIn SP4 Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
SP4_b <- SP4
formals(SP4_b)$tunepar <- 1.02626250031216
class(SP4_b) <- "MP"


#' @describeIn SP4 Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
SP4_c <- SP4
formals(SP4_c)$tunepar <- 1.00188655458282
class(SP4_c) <- "MP"


