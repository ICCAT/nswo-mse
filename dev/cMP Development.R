devtools::load_all()

library(SWOMSE)

# Empirical (model-free CMPs) ----

Average_Catch <- function(x, Data, Data_Lag=2, Interval=3, ...) {

  # Create a Rec (recommendation) object
  Rec <- new('Rec')

  # Year when the MSE analysis is being conducted (2022)
  Current_Yr <- as.numeric(format(Sys.Date(), "%Y"))

  # MP Implementation Interval
  Imp_Years <- seq(Current_Yr+1, by=Interval, length.out=30)

  # Catch Imputation for initial projection years
  if (max(Data@Year) <=Current_Yr) {
    # set the TAC to last recorded catch for years up to
    # and including Current_Yr (2022)
    Rec@TAC <- Data@MPrec[x]
    return(Rec)
  }

  # Not an MP update year
  if (!(max(Data@Year)+1) %in% Imp_Years) {
    # return an empty Rec object (TAC remains unchanged)
    return(Rec)
  }

  # Lag the simulated data by `Data_Lag` years
  Data <- Lag_Data(Data, Data_Lag, x==1)

  # --- Start of MP-specific Code ---
  # Calculate the average historical catch
  yr_ind <- min(length(Data@Cat[x,]), match(Data@LHYear,Data@Year))
  Avg_Catch <- mean(Data@Cat[x,1:yr_ind], na.rm=TRUE)

  # Set TAC to average catch
  Rec@TAC <- Avg_Catch

  # return Rec object
  Rec
}
class(Average_Catch) <- 'MP'



ITarget_1 <- function(x, Data,
                      Data_Lag=2, Interval=3,
                      yrsmth = 5, mc = 0.2, multi=1,
                      ...) {

  # Create a Rec (recommendation) object
  Rec <- new('Rec')

  # Year when the MSE analysis is being conducted (2022)
  Current_Yr <- as.numeric(format(Sys.Date(), "%Y"))

  # MP Implementation Interval
  Imp_Years <- seq(Current_Yr+1, by=Interval, length.out=30)

  # Catch Imputation for initial projection years
  if (max(Data@Year)<Current_Yr) {
    # set the TAC to last recorded catch for years up to
    # and including Current_Yr (2022)
    Rec@TAC <- Data@MPrec[x]
    return(Rec)
  }

  # Not an MP update year
  if (!(max(Data@Year)+1) %in% Imp_Years) {
    # keep TAC unchanged from previous
    Rec@TAC <- Data@MPrec[x]
    return(Rec)
  }

  # Lag the simulated data by `Data_Lag` years
  Data <- Lag_Data(Data, Data_Lag, x==1)

  # --- Start of MP-specific Code ---
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

  TAC <- Data@MPrec[x] * deltaI

  Rec <- new("Rec")
  Rec@TAC <- TACfilter(TAC)
  Rec
}
class(ITarget_1) <- 'MP'



# Assessment-based CMPs ----


# Test the new CMPs ----

MOM <- MOM_000
MOM@nsim <- 3

MOM@cpars$Female[[1]]$M_ageArray %>% dim()
MOM@cpars$Female[[1]]$Cobs_y %>% dim()


fl <- tempfile()
fl
saveRDS(MOM, fl)



OM <- testOM
Hist <- Simulate(OM)
Hist@Data@Year <- rev(seq(2022, by=-1, length.out=50))

OM@CurrentYr <- 2022
Hist@Data@LHYear <- 2022
OM@cpars <- list(Data=Hist@Data)
Hist <- Simulate(OM)

MSE <- Project(Hist, MPs='ITarget_1')

MSE@PPD[[1]]@Year

MSE@TAC[1,1,]
MSE@Catch[1,1,]


multiHist <- SimulateMOM(MOM)

MMSE <- ProjectMOM(multiHist, MPs=c('ITarget_1'))

mm <- 1
Years <- MMSE@PPD[[1]][[1]][[1]]@Year
Years <- Years[Years>2020]
cbind(c(Years, 1),
      colSums(MMSE@TAC[1,,1,mm,]),
      colSums(MMSE@Catch[1,,1,mm,]),
      colSums(MMSE@Removals[1,,1,mm,]))

plot(Years, colSums(MMSE@TAC[1,,1,mm,]), type='b')

SWOData@MPrec



fl <- tempfile()
fl
saveRDS(MOM, fl)




Data <- SWOData
x<-1
Data_Lag=2
Interval=3

Average_Catch(1, SWOData)










# check assumptions for these MPs

run1 <- SP(1, Data)
run2 <- SP_Fox(1,Data)

run1@B_BMSY
run1@FMSY

run2@B_BMSY
run2@FMSY




## JABBA
library(JABBA)

Sys.setenv(JAGS_HOME="C:/Program Files/JAGS/JAGS-4.3.1")

library(rjags)

# requires JAGS installed on machine
x <- 1
catch <- data.frame(Data@Year, Data@Cat[x,])
index <- data.frame(Data@Year,Data@Ind[x,])
index_se <- data.frame(Data@Year, Data@CV_Ind[x,])
index_se[,2] <- 0.23

input <- build_jabba(catch=catch,
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
                      psi.prior=c(0.95, 0.05))

fit1 <- fit_jabba(input,quickmcmc=TRUE)


fit1$estimates
fit1$estimates
Fs <- fit1$timeseries[,1,2]
F_FMSY <- fit1$timeseries[,1,4]
Fs/F_FMSY # FMSY

fit1$timeseries[,1,3] # B/BMSY

jbplot_summary(fit1)






