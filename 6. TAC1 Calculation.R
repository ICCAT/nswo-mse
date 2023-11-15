
# ====================================================================================
# === Calculate TAC1 for CMPs ========================================================
# ====================================================================================

# NATL SWO MSE
# T Carruthers
# 11 Nov '23

# --- load library and MPs ------------------------------------------------------

# devtools::install_github("ICCAT/nswo-mse")# latest version has maxchange2
library(SWOMSE)
setwd("C:/GitHub/nswo-mse")
source("CMPs/CE.R")
source("CMPs/SPSS.R")
source("CMPs/MPs_ND.R")

# --- run an MSE to get the correctly formatted data object ---------------------

getDataMP = function(x,Data,...){ saveRDS(Data,"TAC1/Data.rds"); stop()} ; class(getDataMP) = "MP"
MOM <- get("MOM_001")
MSE = multiMSE(MOM, "getDataMP",checkMPs=F) # will produce an error
PPD = readRDS("TAC1/Data.rds") # MSE@PPD[[1]][[1]][[1]]
cbind(PPD@Year,PPD@Ind[1,])

# --- compare the index data ----------------------------------------------------

data = read.csv("TAC1/SWOForTom.csv")
catch = read.csv("TAC1/catch.csv")
plot(data$Year,data$CombinedIndex,type="l",xlab="Year",ylab="Combined Index") # new data
lines(PPD@Year,PPD@Ind[1,],type='l',col="red") # old data
legend('topright',legend=c("old","new"),text.col=c("red","black"),bty="n")

# --- assuming this is ok, make a data object with a single simulation and only
# historical years and overwrite the data for 2021 and 2022 ---------------------

TAC2023 = 10349 # !!! this is the 2022 TAC !!!!
newyears = 2021:2022
newdat = c(data$CombinedIndex[data$Year %in% newyears],NA) # data are lagged
newse = c(data$se[data$Year %in% newyears],NA) # data are lagged
newcv = newse/newdat # !!! is this right?  is the cv the se/mean as you have reported it?
newcat = c(catch$Catch[catch$Year %in% newyears],NA)       # data are lagged
Data = PPD # make a data object from the posterior predicted data
Data@Year = 1950:2023
Data@Ind = matrix(c(PPD@Ind[1,,drop=F],newdat),nrow = 1)
Data@CV_Ind = matrix(c(PPD@CV_Ind[1,,drop=F],newcv),nrow = 1)
Data@Cat = matrix(c(PPD@Cat[1,,drop=F],newcat),nrow = 1)
Data@MPrec = TAC2023

# --- calculate TAC1 -------------------------------------------------

MPs = c("CE_b","MCC5_b","MCC5_c", "MCC7_b","MCC7_c","SPSSFox_b","SPSSFox2_b")
CE_b(1,Data)
MCC5_b(1,Data)
MCC7_b(x=1,Data)
SPSSFox_b(1,Data)
SPSSFox2_b(1,Data)

TACs = sapply(MPs,function(FF,x,Data)do.call(FF,list(x,Data)),Data=Data,x=1)
write.csv(sapply(TACs,function(x)x@TAC),"TAC1/TACs.csv")



# === END OF SCRIPT ===================================================================


