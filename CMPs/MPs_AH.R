
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
SP1 <- function(x, Data, Index_ID=1, Data_Lag=2, Interval=3, tunepar=1, mc=0.25,
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
  # Historical data starts in 1950 and ends in 2002 (71 years)
  n_years <- length(Data@AddInd[x,Index_ID,])

  # the year index for `yrsmth` most recent years
  yr_ind <- 2020+((max(1, n_years-yrsmth+1):n_years)-71)

  # the year index for target years (2016 to 2020)
  yr_ind_tar <- 2015:2020 #67:71


  if(useCombined==FALSE){
    # get the index and fit and exponential smoothing state space model
    Index_raw = ts(Data@AddInd[x,Index_ID,],start=1950,
                   end=2020+(n_years-71), frequency = 1)
    Index_raw = tsclean(Index_raw)
    Index_fitted = ets(y=Index_raw, damped=F, alpha=.2)$fitted
    Index_fitted[Index_fitted<0] <- 1E-2
    Index_year = time(ets(y=Index_raw, damped=F, alpha=.2)$fitted)

    # index target
    Ind_Target <- mean(Index_fitted[Index_year%in%yr_ind_tar], na.rm=TRUE)
    # ratio of mean recent index to Ind_Target
    deltaI <- (mean(Index_fitted[Index_year%in%yr_ind], na.rm=TRUE)/Ind_Target)* tunepar
  }else{# get the index and fit and exponential smoothing state space model
    Index_raw = ts(Data@Ind[x,],start=1950,
                   end=2020+(n_years-71), frequency = 1)
    Index_raw = tsclean(Index_raw)
    Index_fitted = ets(y=Index_raw, damped=F, alpha=.2)$fitted
    Index_fitted[Index_fitted<0] <- 1E-2
    Index_year = time(ets(y=Index_raw, damped=F, alpha=.2)$fitted)

    # index target
    Ind_Target <- mean(Index_fitted[Index_year%in%yr_ind_tar], na.rm=TRUE)
    # ratio of mean recent index to Ind_Target
    deltaI <- (mean(Index_fitted[Index_year%in%yr_ind], na.rm=TRUE)/Ind_Target)* tunepar

  }
  deltaI = exp(log(deltaI)*.1)
  if (!is.finite(deltaI))  deltaI <- 1

  # max/min change in TAC
  if (deltaI < (1 - mc)) deltaI <- 1 - mc
  if (deltaI > (1 + mc)) deltaI <- 1 + mc

  Rec@TAC <- Data@MPrec[x] * deltaI
  # 5. Return the `Rec` object
  Rec
}
class(SP1) <- 'MP'

# Create MP functions for each Fleet
Fleets = c("CA1","JP1","CT1","MO1","US1","PO1")
for(i in Fleets) {
  assign(paste0(i),SP1)
}
formals(CA1)$Index_ID = 2;    class(CA1) <- 'MP'
formals(JP1)$Index_ID = 3;    class(JP1) <- 'MP'
formals(CT1)$Index_ID = 4;    class(CT1) <- 'MP'
formals(MO1)$Index_ID = 5;    class(MO1) <- 'MP'
formals(US1)$Index_ID = 6;    class(US1) <- 'MP'
formals(PO1)$Index_ID = 7;    class(PO1) <- 'MP'

CI1 = SP1
formals(CI1)$useCombined = TRUE; class(CI1) <- 'MP'

######################################################################
# MPs based on similar performing indices
######################################################################

EA1 <- function(x, Data, Index_ID=c(1,5,7),
                Index_CV=c(.275,.219,.307),
                Data_Lag=2, Interval=3,
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

  # get the indices and fit and exponential smoothing state space model
  Index_fitted = apply(Data@AddInd[x,Index_ID,],1,
                       function(x) ets(y=tsclean(ts(x,start=1950,
                                                    end=2020+(n_years-71), frequency = 1)),
                                       damped=F, alpha=.2)$fitted)
  Index_fitted[Index_fitted<0] <- 1E-2
  Index_year = apply(Data@AddInd[x,Index_ID,],1,
                     function(x) time(ets(y=tsclean(ts(x,start=1950,
                                                       end=2020+(n_years-71), frequency = 1)),
                                          damped=F, alpha=.2)$fitted))

  # index target
  Ind_Target = Index_fitted[Index_year[,1]%in%yr_ind_tar,]
  Ind_Target = mean(apply(t(t(Ind_Target)/(Index_CV)),2,
                          function(x) mean(x, na.rm=T)),
                    na.rm=T) * tunepar

  # ratio of mean recent index to Ind_Target
  Ind_cur = Index_fitted[Index_year[,1]%in%yr_ind,]
  Index_cur = mean(apply(t(t(Ind_cur)/(Index_CV)),2,
                         function(x) mean(x, na.rm=T)), na.rm=T)

  # ratio of mean recent index to Ind_Target
  deltaI <- Index_cur/Ind_Target
  deltaI = exp(log(deltaI)*.1)
  if (!is.finite(deltaI))  deltaI <- 1

  # max/min change in TAC
  if (deltaI < (1 - mc)) deltaI <- 1 - mc
  if (deltaI > (1 + mc)) deltaI <- 1 + mc

  Rec@TAC <- Data@MPrec[x] * deltaI
  # 5. Return the `Rec` object
  Rec
}
# 6. Assign function to class `MP`
class(EA1) <- 'MP'

WA1 <- function(x, Data, Index_ID=c(2,3,4,6),
                Index_CV=c(.322,.342,.258,.205),
                Data_Lag=2, Interval=3,
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
  n_years <- length(Data@AddInd[x,Index_ID,])

  # the year index for `yrsmth` most recent years
  yr_ind <- 2020+((max(1, n_years-yrsmth+1):n_years)-71)

  # the year index for target years (2016 to 2020)
  yr_ind_tar <- 2015:2020 #67:71

  # get the indices and fit and exponential smoothing state space model
  Index_fitted = apply(Data@AddInd[x,Index_ID,],1,
                       function(x) ets(y=tsclean(ts(x,start=1950,
                                                    end=2020+(n_years-71), frequency = 1)),
                                       damped=F, alpha=.2)$fitted)
  Index_fitted[Index_fitted<0] <- 1E-2
  Index_year = apply(Data@AddInd[x,Index_ID,],1,
                     function(x) time(ets(y=tsclean(ts(x,start=1950,
                                                       end=2020+(n_years-71), frequency = 1)),
                                          damped=F, alpha=.2)$fitted))

  # index target
  Ind_Target = Index_fitted[Index_year[,1]%in%yr_ind_tar,]
  Ind_Target = mean(apply(t(t(Ind_Target)/(Index_CV)),2,
                          function(x) mean(x, na.rm=T)),
                    na.rm=T) * tunepar

  # ratio of mean recent index to Ind_Target
  Ind_cur = Index_fitted[Index_year[,1]%in%yr_ind,]
  Index_cur = mean(apply(t(t(Ind_cur)/(Index_CV)),2,
                         function(x) mean(x, na.rm=T)), na.rm=T)

  # ratio of mean recent index to Ind_Target
  deltaI <- Index_cur/Ind_Target
  deltaI = exp(log(deltaI)*.1)
  if (!is.finite(deltaI))  deltaI <- 1

  # max/min change in TAC
  if (deltaI < (1 - mc)) deltaI <- 1 - mc
  if (deltaI > (1 + mc)) deltaI <- 1 + mc

  Rec@TAC <- Data@MPrec[x] * deltaI
  # 5. Return the `Rec` object
  Rec
}
# 6. Assign function to class `MP`
class(WA1) <- 'MP'


AT1 <- function(x, Data, Index_ID=c(1,2,3,4,5,6,7),
                Index_CV=c(.275,.322,.342,.258,.219,.205,.307),
                Data_Lag=2, Interval=3,
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
  n_years <- length(Data@AddInd[x,Index_ID,])

  # the year index for `yrsmth` most recent years
  yr_ind <- 2020+((max(1, n_years-yrsmth+1):n_years)-71)

  # the year index for target years (2016 to 2020)
  yr_ind_tar <- 2015:2020 #67:71

  # get the indices and fit and exponential smoothing state space model
  Index_fitted = apply(Data@AddInd[x,Index_ID,],1,
                       function(x) ets(y=tsclean(ts(x,start=1950,
                                                    end=2020+(n_years-71), frequency = 1)),
                                       damped=F, alpha=.2)$fitted)
  Index_fitted[Index_fitted<0] <- 1E-2
  Index_year = apply(Data@AddInd[x,Index_ID,],1,
                     function(x) time(ets(y=tsclean(ts(x,start=1950,
                                                       end=2020+(n_years-71), frequency = 1)),
                                          damped=F, alpha=.2)$fitted))

  # index target
  Ind_Target = Index_fitted[Index_year[,1]%in%yr_ind_tar,]
  Ind_Target = mean(apply(t(t(Ind_Target)/(Index_CV)),2,
                          function(x) mean(x, na.rm=T)),
                    na.rm=T) * tunepar

  # ratio of mean recent index to Ind_Target
  Ind_cur = Index_fitted[Index_year[,1]%in%yr_ind,]
  Index_cur = mean(apply(t(t(Ind_cur)/(Index_CV)),2,
                         function(x) mean(x, na.rm=T)), na.rm=T)

  # ratio of mean recent index to Ind_Target
  deltaI <- Index_cur/Ind_Target
  deltaI = exp(log(deltaI)*.1)
  if (!is.finite(deltaI))  deltaI <- 1

  # max/min change in TAC
  if (deltaI < (1 - mc)) deltaI <- 1 - mc
  if (deltaI > (1 + mc)) deltaI <- 1 + mc

  #z = .01*exp(.125*(max(Index_year)-2019))+.1#.1


  Rec@TAC <- Data@MPrec[x] * deltaI
  # 5. Return the `Rec` object
  Rec
}
# 6. Assign function to class `MP`
class(AT1) <- 'MP'


# ---- Tuned CMPs ----

#' @export
CI1_a <- CI1
formals(CI1_a)$tunepar <- 0.840855262756301
class(CI1_a) <- "MP"


#' @describeIn CI1 Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
CI1_b <- CI1
formals(CI1_b)$tunepar <- 0.696637722132472
class(CI1_b) <- "MP"


#' @describeIn CI1 Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
CI1_c <- CI1
formals(CI1_c)$tunepar <- 0.507645531938066
class(CI1_c) <- "MP"
#' @describeIn EA1 Tuned to PGK_short = 0.51 across Reference OMs.
#' @export
EA1_a <- EA1
formals(EA1_a)$tunepar <- 1.24676724137931
class(EA1_a) <- "MP"


#' @describeIn EA1 Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
EA1_b <- EA1
formals(EA1_b)$tunepar <- 1.51231527093596
class(EA1_b) <- "MP"


#' @describeIn EA1 Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
EA1_c <- EA1
formals(EA1_c)$tunepar <- 2.1
class(EA1_c) <- "MP"


#' @describeIn WA1 Tuned to PGK_short = 0.51 across Reference OMs.
#' @export
WA1_a <- WA1
formals(WA1_a)$tunepar <- 1.15951655303543
class(WA1_a) <- "MP"


#' @describeIn WA1 Tuned to PGK_short = 0.6 across Reference OMs.
#' @export
WA1_b <- WA1
formals(WA1_b)$tunepar <- 1.40134257577028
class(WA1_b) <- "MP"


#' @describeIn WA1 Tuned to PGK_short = 0.7 across Reference OMs.
#' @export
WA1_c <- WA1
formals(WA1_c)$tunepar <- 1.93022650749923
class(WA1_c) <- "MP"


