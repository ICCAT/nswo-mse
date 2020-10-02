library(SWOMSE)


# devtools::install_github("r4ss/r4ss", build_vignettes = TRUE, force=TRUE)

# -------- Build Package Data ----------

# ---- Add SS OMs as Data ----

# OM.root <- 'G:/My Drive/Projects/Projects_2020/ICCAT_Swordfish/OMs/SS/2018_GRID'
OM.root <- 'G:/My Drive/Projects/Projects_2020/ICCAT_Swordfish/OMs/Grid_2020'
OMbase.dir <-  file.path(OM.root, "Michael_March2020/NSWO_MSE_SS3_Base_v2")
OMgrid.dir <- file.path(OM.root, "grid_2020")
OMgrid.dirs <- list.dirs(OMgrid.dir, recursive = FALSE)

ord <- lapply(strsplit(OMgrid.dirs, 'iter'), '[[', 2) %>% as.numeric() %>% order()
OMgrid.dirs <- OMgrid.dirs[ord]

library(r4ss); library(dplyr)

# -- Import objects from SS OMs and save in 'OM_objects' --
# -- Only need to run this if SS OMs in OM.dirs are updated --
# saveBaseCaseObjects(OMbase.dir)
# saveGridObjects(OMgrid.dirs)


# -- Import temporary objects and save as data objects --
# SS OM Overview
OMs_DF <- readRDS('OM_objects/OM_DF.rda')
usethis::use_data(OMs_DF, overwrite = TRUE)

basecase_DF <- readRDS('OM_objects/basecase_DF.rda')
usethis::use_data(basecase_DF, overwrite = TRUE)

# Likelihood List
Like_List <- readRDS('OM_objects/LHlist.rda')
usethis::use_data(Like_List, overwrite = TRUE)

basecase_LH <- readRDS('OM_objects/basecase_LH.rda')
usethis::use_data(basecase_LH, overwrite = TRUE)

# Fleet Information
DataList <- readRDS('OM_objects/DataList.rda')
data <- DataList[[1]] # same dat for all OMs
fleet.names <- data$fleetnames
fleet.index <- seq_along(fleet.names)
Fleet_DF <- data.frame(Code=fleet.names,
                      Name=c('Spain',
                             'US',
                             'Canada - Early',
                             'Canada - Late',
                             'Japan - Early',
                             'Japan - Mid',
                             'Japan - Late',
                             'Portugal',
                             'Chinese-Taipai',
                             'Morocco',
                             'Other',
                             'Age-1 Survey',
                             'Age-2 Survey',
                             'Age-3 Survey',
                             'Age-4 Survey',
                             'Age-5+ Survey'),
                      index=fleet.index)
Fleet_DF$Code <- factor(Fleet_DF$Code, levels=Fleet_DF$Code, ordered = TRUE)
Fleet_DF$Name <- factor(Fleet_DF$Name, levels=Fleet_DF$Name, ordered = TRUE)
Fleet_DF <- Fleet_DF %>% dplyr::select(index, Name, Code)
usethis::use_data(Fleet_DF, overwrite = TRUE)

# Data Summary
# Catch Data
CatchDat <- data$catch %>% tidyr::pivot_longer(1:data$Nfleet, 'Fleet', values_to="Catch") %>%
  dplyr::rename(Code=Fleet) %>%
  dplyr::select(year, Code, Catch)
CatchDat$Code <- factor(CatchDat$Code, levels=levels(Fleet_DF$Code), ordered = TRUE)
CatchDat <- left_join(CatchDat, Fleet_DF, by="Code")


# CPUE Data
CPUEDat <- data$CPUE %>% dplyr::filter(year>0) %>%
  dplyr::rename(CPUE_Obs=obs, CPUE_SE_log=se_log) %>%
  dplyr::select(year, index, CPUE_Obs, CPUE_SE_log) %>%
  dplyr::mutate(llq=1)
CPUEDat <- dplyr::left_join(CPUEDat, Fleet_DF, by="index")

# data2 <- DataList[[73]] # CPUE with adjustment for increasing q
# CPUEDat2 <- data2$CPUE %>% dplyr::filter(year>0) %>%
#   dplyr::rename(CPUE_Obs=obs, CPUE_SE_log=se_log) %>%
#   dplyr::select(year, index, CPUE_Obs, CPUE_SE_log) %>%
#   dplyr::mutate(llq=1.01)
# CPUEDat2 <- dplyr::left_join(CPUEDat2, Fleet_DF, by="index")
#
# CPUEDat <- dplyr::bind_rows(CPUEDat, CPUEDat2)

# Length Comp Data
LenDat <- data$lencomp %>% dplyr::filter(Yr>0) %>%
  dplyr::rename(year=Yr, index=FltSvy)
cols <- names(LenDat)[7:length(names(LenDat))] # convert to long
LenDat <- LenDat %>% tidyr::pivot_longer(all_of(cols), 'Length.Class', values_to="Length.Comp") %>%
  dplyr::select(year, index, Length.Class, Length.Comp, Nsamp) %>%
  dplyr::rename(Length.Nsamp=Nsamp)
LenDat <- left_join(LenDat, Fleet_DF, by="index")

# Mean Weight Data
WghtDat <- data$meanbodywt %>% dplyr::filter(Year>0) %>%
  dplyr::rename(year=Year, index=Type, Mean.Weight=Value, Weight.CV=CV) %>%
  dplyr::select(year, index, Mean.Weight, Weight.CV)
WghtDat <- left_join(WghtDat, Fleet_DF, by="index")

# Combine all data
AllDat <- dplyr::full_join(CatchDat, CPUEDat, by = c("year", "Code", "Name", "index"))
AllDat <- dplyr::full_join(AllDat, LenDat, by = c("year", "Code", "Name", "index"))
AllDat <- dplyr::full_join(AllDat, WghtDat, by =  c("year", "Code", "Name", "index"))
AllDat <- dplyr::select(AllDat, year, index, Code, Name, Catch, CPUE_Obs,
                        CPUE_SE_log, Length.Class, Length.Comp, Length.Nsamp,
                        Mean.Weight, Weight.CV)
AllDat$year2 <- as.Date(paste(AllDat$year, 1, 1, sep = "-"))

Data_DF <- AllDat
usethis::use_data(Data_DF, overwrite = TRUE)

# CPUE Fit Data
RepList <- readRDS('OM_objects/RepList.rda')

CPUE_List <- list()

for (OM.n in unique(OMs_DF$n)) {
  CPUE_List[[OM.n]] <- RepList[[OM.n]]$cpue %>%
    dplyr::rename(index=Fleet, year=Yr) %>%
    select(year, index, Vuln_bio, Obs, Exp, Calc_Q, Eff_Q)
}
usethis::use_data(CPUE_List, overwrite = TRUE)


basecase_replist <- readRDS('OM_objects/basecase_replist.rda')
basecase_CPUE <- basecase_replist$cpue %>%
  dplyr::rename(index=Fleet, year=Yr) %>%
  select(year, index, Vuln_bio, Obs, Exp, Calc_Q, Eff_Q)
usethis::use_data(basecase_CPUE, overwrite = TRUE)

# Time-series Biology
TSBio_List <- list()
for (OM.n in OMs_DF$n) {

  TSbio_dat <- RepList[[OM.n]]$recruit %>%
    dplyr::select(year=Yr, SSB=SpawnBio, Exp_Rec=exp_recr, Exp_Rec_biasadj=bias_adjusted,
                  Obs_Rec=pred_recr, dev=dev, era, biasadj)
  TSbio_dat$SB0 <- RepList[[OM.n]]$SBzero
  R0 <- RepList[[OM.n]]$timeseries %>% filter(Era=="VIRG") %>% select(R0=Recruit_0)
  TSbio_dat$R0 <- R0$R0
  TSbio_dat$Depletion <- TSbio_dat$SSB/TSbio_dat$SB0
  TSbio_dat$SS_Depletion <- RepList[[OM.n]]$current_depletion

  derived_quants <- RepList[[OM.n]]$derived_quants
  FMSY <- derived_quants$Value[derived_quants$Label=='Fstd_MSY']
  Kobe <- RepList[[OM.n]]$Kobe %>% dplyr::rename(year=Year)
  TSbio_dat <- dplyr::full_join(TSbio_dat, Kobe,by="year")
  TSbio_dat$F <- TSbio_dat$F.Fmsy*FMSY
  TSBio_List[[OM.n]] <- TSbio_dat
}
usethis::use_data(TSBio_List, overwrite = TRUE)


TSbio_dat <- basecase_replist$recruit %>%
  dplyr::select(year=Yr, SSB=SpawnBio, Exp_Rec=exp_recr, Exp_Rec_biasadj=bias_adjusted,
                Obs_Rec=pred_recr, dev=dev, era, biasadj)
TSbio_dat$SB0 <- basecase_replist$SBzero
R0 <- basecase_replist$timeseries %>% filter(Era=="VIRG") %>% select(R0=Recruit_0)
TSbio_dat$R0 <- R0$R0
TSbio_dat$Depletion <- TSbio_dat$SSB/TSbio_dat$SB0
TSbio_dat$SS_Depletion <- basecase_replist$current_depletion

derived_quants <- basecase_replist$derived_quants
FMSY <- derived_quants$Value[derived_quants$Label=='Fstd_MSY']
Kobe <- basecase_replist$Kobe %>% dplyr::rename(year=Year)
TSbio_dat <- dplyr::full_join(TSbio_dat, Kobe,by="year")
TSbio_dat$F <- TSbio_dat$F.Fmsy*FMSY

basecase_TSbio <- TSbio_dat
usethis::use_data(basecase_TSbio, overwrite = TRUE)


# Reference Points
tempList <- list()
for (OM.n in OMs_DF$n) {
  derived <- RepList[[OM.n]]$derived_quants
  derived <- derived %>% dplyr::filter(Label %in% c('SSB_Virgin', 'SSB_MSY', 'Recr_Unfished', 'Fstd_MSY', 'TotYield_MSY'))

  TSbio_dat <- TSBio_List[[OM.n]]
  conv <- !TSbio_dat$SS_Depletion %>% is.na() %>% all()
  if (conv) {
    years <- min(TSbio_dat$year):TSbio_dat$year[which(TSbio_dat$Depletion == TSbio_dat$SS_Depletion)]
    SSB_DF <- TSbio_dat %>% filter(year %in% years)
    finalSB <- SSB_DF %>% dplyr::filter(year==max(year))
  } else{
    finalSB <- data.frame(F.Fmsy=NA, B.Bmsy=NA, Depletion=NA)
  }

  tempList[[OM.n]] <- data.frame(n=OM.n,
                                 MSY=derived$Value[derived$Label=='TotYield_MSY'],
                                 SBMSY=derived$Value[derived$Label=='SSB_MSY'],
                                 FMSY=derived$Value[derived$Label=='Fstd_MSY'],
                                 F_FMSY=finalSB$F.Fmsy,
                                 SB_SBMSY=finalSB$B.Bmsy,
                                 SBMSY_SB0=derived$Value[derived$Label=='SSB_MSY']/derived$Value[derived$Label=='SSB_Virgin'],
                                 Depletion=finalSB$Depletion)
}
RefPoint_DF <- do.call('rbind', tempList)
usethis::use_data(RefPoint_DF, overwrite = TRUE)


derived <- basecase_replist$derived_quants
derived <- derived %>% dplyr::filter(Label %in% c('SSB_Virgin', 'SSB_MSY', 'Recr_Unfished', 'Fstd_MSY', 'TotYield_MSY'))

TSbio_dat <- basecase_TSbio
conv <- !TSbio_dat$SS_Depletion %>% is.na() %>% all()
if (conv) {
  years <- min(TSbio_dat$year):TSbio_dat$year[which(TSbio_dat$Depletion == TSbio_dat$SS_Depletion)]
  SSB_DF <- TSbio_dat %>% filter(year %in% years)
  finalSB <- SSB_DF %>% dplyr::filter(year==max(year))
} else{
  finalSB <- data.frame(F.Fmsy=NA, B.Bmsy=NA, Depletion=NA)
}
basecase_RefPoint <- data.frame(n='base_case',
                                MSY=derived$Value[derived$Label=='TotYield_MSY'],
                                SBMSY=derived$Value[derived$Label=='SSB_MSY'],
                                FMSY=derived$Value[derived$Label=='Fstd_MSY'],
                                F_FMSY=finalSB$F.Fmsy,
                                SB_SBMSY=finalSB$B.Bmsy,
                                SBMSY_SB0=derived$Value[derived$Label=='SSB_MSY']/derived$Value[derived$Label=='SSB_Virgin'],
                                Depletion=finalSB$Depletion)

usethis::use_data(basecase_RefPoint, overwrite = TRUE)

# Selectivity data
Select_List <- list()
for (OM.n in OMs_DF$n) {
  SizeSelect <- RepList[[OM.n]]$sizeselex %>% dplyr::rename(index=Fleet, year=Yr)

  SizeSelect <- SizeSelect %>% tidyr::pivot_longer(6:ncol(SizeSelect),
                                                   'Length.Class', values_to="Select")
  SizeSelect <- left_join(SizeSelect, Fleet_DF, by="index")
  SizeSelect$Length.Class <- as.numeric(SizeSelect$Length.Class)
  SizeSelect <- SizeSelect %>% filter(Factor=='Lsel',
                                      Sex==1,
                                      year==max(RepList[[OM.n]]$endyr),
                                      Name %in% Fleet_DF$Name[1:11])
  Select_List[[OM.n]] <- SizeSelect
}
usethis::use_data(Select_List, overwrite = TRUE)

SizeSelect <- basecase_replist$sizeselex %>% dplyr::rename(index=Fleet, year=Yr)
SizeSelect <- SizeSelect %>% tidyr::pivot_longer(6:ncol(SizeSelect),
                                                 'Length.Class', values_to="Select")
SizeSelect <- left_join(SizeSelect, Fleet_DF, by="index")
SizeSelect$Length.Class <- as.numeric(SizeSelect$Length.Class)
SizeSelect <- SizeSelect %>% filter(Factor=='Lsel',
                                    Sex==1,
                                    year==max(basecase_replist$endyr),
                                    Name %in% Fleet_DF$Name[1:11])
basecase_Select <- SizeSelect
usethis::use_data(basecase_Select, overwrite = TRUE)



# Catch data and fit
Catch_List <- list()
for (OM.n in OMs_DF$n) {
  catch_dat <- RepList[[OM.n]]$catch %>%
    dplyr::rename(index=Fleet, year=Yr) %>%
    dplyr::select(index, year, Obs, Exp, se)
  catch_dat <- dplyr::left_join(catch_dat, Fleet_DF, by="index")
  Catch_List[[OM.n]] <- catch_dat
}
usethis::use_data(Catch_List, overwrite = TRUE)

catch_dat <- basecase_replist$catch %>%
  dplyr::rename(index=Fleet, year=Yr) %>%
  dplyr::select(index, year, Obs, Exp, se)
catch_dat <- dplyr::left_join(catch_dat, Fleet_DF, by="index")
basecase_catch <- catch_dat
usethis::use_data(basecase_catch, overwrite = TRUE)

# Length data and fit
LenDat_List <- list()
for (OM.n in OMs_DF$n) {
  predLen <- RepList[[OM.n]]$lendbase %>% dplyr::rename(year=Yr, index=Fleet) %>%
    dplyr::select(year, index, Sex, Bin, Obs, Exp, Used, effN, N)
  predLen <- dplyr::left_join(predLen, Fleet_DF, by="index")
  LenDat_List[[OM.n]] <- predLen
}
usethis::use_data(LenDat_List, overwrite = TRUE)

predLen <- basecase_replist$lendbase %>% dplyr::rename(year=Yr, index=Fleet) %>%
  dplyr::select(year, index, Sex, Bin, Obs, Exp, Used, effN, N)
predLen <- dplyr::left_join(predLen, Fleet_DF, by="index")

basecase_LenDat <- predLen
usethis::use_data(basecase_LenDat, overwrite = TRUE)



nbound <- NA
templist <- list()
for (i in seq_along(RepList)) {
  tt <- RepList[[i]]$estimated_non_dev_parameters
  nbound[i] <- sum((tt$Value<0.99*tt$Min) | (tt$Value>0.99*tt$Max))
  if (nbound[i] > 0) {
    ind <- which((tt$Value<0.99*tt$Min) | (tt$Value>0.99*tt$Max))
    templist[[i]] <- data.frame(OM=i,
                                Parameter=rownames(tt[ind,]),
                                Value= tt$Value[ind],
                                Min=tt$Min[ind],
                                Max=tt$Max[ind])
  }
}
DF_bound <- do.call('rbind', templist)
usethis::use_data(DF_bound, overwrite = TRUE)

# --- Combined Index ----
CombinedIndex <- read.csv('inst/Combined_Index.csv')
names(CombinedIndex) <- c("Year", "CPUE", "CV")
CombinedIndex <- CombinedIndex %>% dplyr::filter(Year>=1975) # ignore the first few years with NAs

CombinedIndex$st.CPUE <- CombinedIndex$CPUE/mean(CombinedIndex$CPUE, na.rm=TRUE)


yrs <- range(CombinedIndex$Year)

CombinedIndexList <- list()
for (i in seq_along(RepList)) {
  replist <- RepList[[i]]
  dat <- replist$timeseries %>% dplyr::filter(Yr>=yrs[1], Yr<=yrs[2])
  st.bio <- dat$Bio_all # need to account for selectivity?
  st.bio <- st.bio/mean(st.bio, na.rm=TRUE)

  temp <- OMs_DF %>% dplyr::filter(n==i)
  CombinedIndexList[[i]] <- CombinedIndex
  CombinedIndexList[[i]]$st.bio <- st.bio
  CombinedIndexList[[i]]$OM <- i
  CombinedIndexList[[i]]$env <- temp$env
  CombinedIndexList[[i]]$llq <- temp$llq

}
CombinedIndex <- do.call('rbind', CombinedIndexList)

usethis::use_data(CombinedIndex, overwrite = TRUE)



document_data()














