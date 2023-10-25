
# Investigate for select CMPs and OM:
# - impact of minimum TAC change of 200t
# - impact of removing size limit
# - different management cycles

library(SWOMSE)

# MOM = MOM_000 - 2022 assessment

multiHist <- SimulateMOM(MOM_000)


# ---- Minimum TAC change of 200t vs default ----

# CMPs

CE <- function(x, Data, Data_Lag=1, Interval=3, tunepar=0.964931283484743, mc=0.25,
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
class(CE) <- 'MP'

CE_mc <- function(x, Data, Data_Lag=1, Interval=3, tunepar=0.964931283484743, mc=0.25,
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

  if (abs(Data@MPrec[x] - Rec@TAC)<200)
    Rec@TAC <- Data@MPrec[x]

  Rec
}
class(CE_mc) <- 'MP'

FX4 <- function(x, Data, Data_Lag=1, Interval=3,
                PC = c(.75,.8,.85,.90,.95,1,1.025,1.05,1.075,1.1,1.125),
                Pcentile = c(.1,.2,.3,.4,.45,.55,.6,.7,.8,.9),
                R30=TRUE,
                yrsmth = 3, mc = 0.25, tunepar= 0.985972762145045,
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
  n_years <- length(Data@AddInd[x,2,])

  # the year index for `yrsmth` most recent years
  yr_ind <- 2020+((max(1, n_years-yrsmth+1):n_years)-71)

  # the year index for the most recent 30 years
  yr_ind_tar <- 2020+((max(1, n_years- 30+1):n_years)-71)

  # get the target quantiles of the indices and current values
  Index_year = time(ets(y=na.interp(ts(Data@Ind[x,],start=1950,
                                       end=2020+(n_years-71), frequency = 1)),
                        damped=T, alpha=.2)$fitted)
  if(R30 == TRUE) {Tyear = yr_ind_tar
  }else{Tyear = 1950:max(Index_year)}
  Index_TarQ = quantile(sort(stats::runmed(Data@Ind[x,Index_year%in%Tyear],3)),
                        Pcentile,na.rm=T)
  Index_CurV = ts(stats::runmed(Data@Ind[x,],3),start=1950,end=2020+(n_years-71),
                  frequency = 1)

  # ratio of mean recent index to Ind_Target
  Index_CurV = Index_CurV[Index_year%in%yr_ind]
  Index_CurV = mean(t(t(Index_CurV)/(1)), na.rm=T)
  Score = 1+ sum(Index_TarQ<Index_CurV)

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
class(FX4) <- 'MP'

FX4_mc <- function(x, Data, Data_Lag=1, Interval=3,
                PC = c(.75,.8,.85,.90,.95,1,1.025,1.05,1.075,1.1,1.125),
                Pcentile = c(.1,.2,.3,.4,.45,.55,.6,.7,.8,.9),
                R30=TRUE,
                yrsmth = 3, mc = 0.25, tunepar= 0.985972762145045,
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
  n_years <- length(Data@AddInd[x,2,])

  # the year index for `yrsmth` most recent years
  yr_ind <- 2020+((max(1, n_years-yrsmth+1):n_years)-71)

  # the year index for the most recent 30 years
  yr_ind_tar <- 2020+((max(1, n_years- 30+1):n_years)-71)

  # get the target quantiles of the indices and current values
  Index_year = time(ets(y=na.interp(ts(Data@Ind[x,],start=1950,
                                       end=2020+(n_years-71), frequency = 1)),
                        damped=T, alpha=.2)$fitted)
  if(R30 == TRUE) {Tyear = yr_ind_tar
  }else{Tyear = 1950:max(Index_year)}
  Index_TarQ = quantile(sort(stats::runmed(Data@Ind[x,Index_year%in%Tyear],3)),
                        Pcentile,na.rm=T)
  Index_CurV = ts(stats::runmed(Data@Ind[x,],3),start=1950,end=2020+(n_years-71),
                  frequency = 1)

  # ratio of mean recent index to Ind_Target
  Index_CurV = Index_CurV[Index_year%in%yr_ind]
  Index_CurV = mean(t(t(Index_CurV)/(1)), na.rm=T)
  Score = 1+ sum(Index_TarQ<Index_CurV)

  # ratio of mean recent index to Ind_Target
  deltaI <- mean(PC[Score],na.rm=T)
  if(is.na(deltaI)|deltaI<0) deltaI = 1


  # max/min change in TAC
  if (deltaI <= (1 - mc)) deltaI <- 1 - mc
  if (deltaI > (1 + mc)) deltaI <- 1 + mc

  Rec@TAC <- (12500*tunepar) * deltaI

  if (abs(Data@MPrec[x] - Rec@TAC)<200)
    Rec@TAC <- Data@MPrec[x]

  # 5. Return the `Rec` object
  Rec
}
# 6. Assign function to class `MP`
class(FX4_mc) <- 'MP'


MCC7 <- function(x, Data, Data_Lag=1, Interval=3, tunepar=0.834354466598188, mc=NA, ...) {
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
class(MCC7) <- 'MP'

MCC7_mc <- function(x, Data, Data_Lag=1, Interval=3, tunepar=0.834354466598188, mc=NA, ...) {
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

  if (abs(Data@MPrec[x] - Rec@TAC)<200)
    Rec@TAC <- Data@MPrec[x]

  Rec
}
class(MCC7_mc) <- 'MP'



MMSE <- ProjectMOM(multiHist, MPs=c('CE', 'CE_mc',
                                    'FX4', 'FX4_mc',
                                    'MCC7', 'MCC7_mc'))

saveRDS(MMSE, 'Additional_MSEs/MinTACChange.rda')


# ---- Different Management Cycles ----

# 1, 2, 3, and 4 yr management cycle
# Group only wants to do 3 and 4

CE_1 <- CE
formals(CE_1)$Interval <- 1
class(CE_1) <- 'MP'

CE_2 <- CE
formals(CE_2)$Interval <- 2
class(CE_2) <- 'MP'

CE_3 <- CE
formals(CE_3)$Interval <- 3
class(CE_3) <- 'MP'

CE_4 <- CE
formals(CE_4)$Interval <- 4
class(CE_4) <- 'MP'


FX4_1 <- FX4
formals(FX4_1)$Interval <- 1
class(FX4_1) <- 'MP'

FX4_2 <- FX4
formals(FX4_2)$Interval <- 2
class(FX4_2) <- 'MP'

FX4_3 <- FX4
formals(FX4_3)$Interval <- 3
class(FX4_3) <- 'MP'

FX4_4 <- FX4
formals(FX4_4)$Interval <- 4
class(FX4_4) <- 'MP'


MCC7_1 <- MCC7
formals(MCC7_1)$Interval <- 1
class(MCC7_1) <- 'MP'

MCC7_2 <- MCC7
formals(MCC7_2)$Interval <- 2
class(MCC7_2) <- 'MP'

MCC7_3 <- MCC7
formals(MCC7_3)$Interval <- 3
class(MCC7_3) <- 'MP'

MCC7_4 <- MCC7
formals(MCC7_4)$Interval <- 4
class(MCC7_4) <- 'MP'


MMSE_mngcycle <- ProjectMOM(multiHist, MPs=c('CE_3', 'CE_4',
                                           'FX4_3', 'FX4_4',
                                           'MCC7_3', 'MCC7_4'))

saveRDS(MMSE_mngcycle, 'Additional_MSEs/DiffMngCycle.rda')



# ---- Process results -----

MMSE <- readRDS('Additional_MSEs/MinTACChange.rda')

mp_names <- strsplit(MMSE@MPs[[1]], '_mc') %>% unlist()
type <- rep('None', length(MMSE@MPs[[1]]))
type[grepl('_mc', MMSE@MPs[[1]])] <- '200 t'


df <- data.frame(MP=mp_names,
                 `Minimum TAC Change`=type,
                 PGK=round(PGK(MMSE)@Mean,2),
                 `Mean Landings`=round(apply(apply(MMSE@Catch, c(1,4,5), sum), 2, mean),0),
                 VarC=round(VarC(MMSE)@Mean,2))

df

write.csv(df, 'dev/MinTACChange.csv')

saveRDS(df,'inst/shiny_apps/SWOMSE/data/MinTACChange.rda')


ggplot(df, aes(x=PGK, y=Mean.Landings, color=MP, shape=Minimum.TAC.Change)) +
  geom_point() +
  theme_bw() +
  expand_limits(y=0, x=0) +
  labs(x='Prob. Green Kobe', y='Mean Landings (t)', shape='Minimum TAC change')

tacs <- apply(MMSE@TAC, c(1,4,5), sum)



MMSE <- readRDS('Additional_MSEs/DiffMngCycle.rda')

df <- data.frame(MP=unlist(lapply(strsplit(MMSE@MPs[[1]], '_'), '[[', 1)),
                 Interval=unlist(lapply(strsplit(MMSE@MPs[[1]], '_'), '[[', 2)),
                 PGK=round(PGK(MMSE)@Mean,2),
                 `Mean Landings`=round(apply(apply(MMSE@Catch, c(1,4,5), sum), 2, mean),0),
                 VarC=round(VarC(MMSE)@Mean,2))


write.csv(df, 'dev/DiffMngCycle.csv')
saveRDS(df,'inst/shiny_apps/SWOMSE/data/DiffMngCycle.rda')

ggplot(df, aes(x=PGK, y=Mean.Landings, color=MP, shape=Interval)) +
  geom_point() +
  theme_bw() +
  expand_limits(y=0, x=0) +
  labs(x='Prob. Green Kobe', y='Mean Landings (t)', shape='Management Interval')






