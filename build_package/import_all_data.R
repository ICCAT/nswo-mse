
# ---- Install latest version of r4ss ----
# devtools::install_github("r4ss/r4ss", build_vignettes = TRUE, force=TRUE)

library(r4ss); library(dplyr);
library(MSEtool);
library(usethis); library(SWOMSE)

# -------- Build Package Data ----------

# - OM Documentation -
RoxygenFile <- 'Roxygen_OMs.r'
if (file.exists(file.path('R/', RoxygenFile)))
  file.remove(file.path('R/', RoxygenFile)) # delete
file.create(file.path('R/', RoxygenFile)) # make empty file

cat("# This file is automatically built by build_package/import_OMs.r\n",
    "# Don't edit by hand!\n",
    "# \n\n", sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))


# ---- Add SWO_Data ----
SWOData <- MSEtool::XL2Data("inst/SWO_Data")
usethis::use_data(SWOData, overwrite = TRUE)

cat("#' @name SWOData",
    "\n#' @docType data",
    "\n#' @title North Atlantic Swordfish Data",
    "\n#' @description North Atlantic Swordfish Data",
    "\n#'  ",
    '\nNULL',
    "\n\n\n",
    sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))


# ---- Add SS OMs as Data ----
nsim <- 20

OM.root <- 'G:/My Drive/1_Projects/North_Atlantic_Swordfish/OMs/Grid_2020'
OMbase.dir <-  file.path(OM.root, "Michael_March2020/NSWO_MSE_SS3_Base_v2")
OMgrid.dir <- file.path(OM.root, "grid_2020")
OMgrid.dirs <- list.dirs(OMgrid.dir, recursive = FALSE)

ord <- lapply(strsplit(OMgrid.dirs, 'iter'), '[[', 2) %>% as.numeric() %>% order()
OMgrid.dirs <- OMgrid.dirs[ord]

# ---- Import SS OMs ----

cat("#' @name SWO-OMs",
    "\n#' @docType data",
    "\n#' @title Operating models",
    "\n#' @description North Atlantic Swordfish Operating Models",
    "\n#'  ",
    "\n#' `SWOM` is a single OM combining all SS OMs including `OM_base_case` and",
    "\n#' all SS OMs there were imported into the package (`OM_1`, `OM_2`, ..., etc)",
    '\nNULL',
    "\n\n\n",
    sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))

docOM <- function(OMname) {
  cat("#' @rdname SWO-OMs \n", "'", OMname, "'\n",
      sep="", append=TRUE,
      file=file.path('R/', RoxygenFile)
  )
}


# - Base Case -
OM_base_case <- SWOMSE::SWO_SS2OM(OMbase.dir, nsim=nsim)
OM_base_case@Name <- 'Base Case'
OM_base_case@cpars$Data <- SWOData
usethis::use_data(OM_base_case, overwrite = TRUE)

docOM('OM_base_case')

# - Loop Over Uncertainty Grid -
options(warn=2)
for (i in seq_along(OMgrid.dirs)) {
  OM <- SWO_SS2OM(OMgrid.dirs[i], nsim=nsim)
  OM@cpars$Data <- SWOData

  SS.dir <- OMgrid.dirs[i]

  # M
  txt <- strsplit(SS.dir, '-M')[[1]][2]
  M <- strsplit(txt, '_')[[1]][1] %>% as.numeric()

  # sigmaR
  txt <- strsplit(SS.dir, '_sigmaR')[[1]][2]
  sigmaR <- strsplit(txt, '_')[[1]][1] %>% as.numeric()

  # Steep
  txt <- strsplit(SS.dir, 'steepness')[[1]][2]
  h <- strsplit(txt, '_cpue')[[1]][1] %>% as.numeric()

  # CV
  txt <- strsplit(SS.dir, 'cpuecv')[[1]][2]
  CV <- strsplit(txt, '_ess')[[1]][1] %>% as.numeric()

  # ESS
  txt <- strsplit(SS.dir, '_ess')[[1]][2]
  ESS <- strsplit(txt, '_')[[1]][1] %>% as.numeric()

  # Q
  txt <- strsplit(SS.dir, 'llq')[[1]][2]
  q <- strsplit(txt, '_env')[[1]][1] %>% as.numeric()

  # ENV
  txt <- strsplit(SS.dir, 'env')[[1]][2]
  env <- strsplit(txt, '_')[[1]][1] %>% as.numeric()
  if (env <1) {
    env <- 0
  } else {
    env <- 1
  }

  OM@Name <- paste('M', M,
                   'sigmaR', sigmaR,
                   'h', h,
                   'CV', CV,
                   'ESS', ESS,
                   'Q', q,
                   'ENV', env,
                   sep='-')

  name <- paste0('OM_', i)
  assign(name, OM)

  do.call("use_data", list(as.name(name), overwrite = TRUE))
  docOM(name)
}


# ---- Add additional Data ----

OMs_DF <- readRDS(paste0(OM.root,'/OM_objects/OM_DF.rda'))
usethis::use_data(OMs_DF, overwrite = TRUE)

cat("#' @name OMs_DF",
    "\n#' @docType data",
    "\n#' @title Overview of OM Parameters",
    "\n#' @description A dataframe of OM Parameters",
    "\n#'  ",
    '\n "OMs_DF"',
    "\n\n\n",
    sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))


basecase_DF <- readRDS(paste0(OM.root,'/OM_objects/basecase_DF.rda'))
usethis::use_data(basecase_DF, overwrite = TRUE)

cat("#' @name basecase_DF",
    "\n#' @docType data",
    "\n#' @title Overview of Base Case OM Parameters",
    "\n#' @description A dataframe of OM Parameters",
    "\n#'  ",
    '\n "basecase_DF"',
    "\n\n\n",
    sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))


# Likelihood List
Like_List <- readRDS(paste0(OM.root,'/OM_objects/LHlist.rda'))
usethis::use_data(Like_List, overwrite = TRUE)

cat("#' @name Like_List",
    "\n#' @docType data",
    "\n#' @title OM Likelihoods",
    "\n#' @description A list of likelihood values for each OM",
    "\n#'  ",
    '\n "Like_List"',
    "\n\n\n",
    sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))

basecase_LH <- readRDS(paste0(OM.root,'/OM_objects/basecase_LH.rda'))
usethis::use_data(basecase_LH, overwrite = TRUE)

cat("#' @name basecase_LH",
    "\n#' @docType data",
    "\n#' @title Base Case OM Likelihoods",
    "\n#' @description A list of likelihood values for base case OM",
    "\n#'  ",
    '\n "basecase_LH"',
    "\n\n\n",
    sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))

# Fleet Information
DataList <- readRDS(paste0(OM.root,'/OM_objects/DataList.rda'))
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

cat("#' @name Fleet_DF",
    "\n#' @docType data",
    "\n#' @title Fleet Information",
    "\n#' @description A dataframe of information on fleet names",
    "\n#'  ",
    '\n "Fleet_DF"',
    "\n\n\n",
    sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))


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


cat("#' @name Data_DF",
    "\n#' @docType data",
    "\n#' @title Data Information",
    "\n#' @description A dataframe of fishery data",
    "\n#'  ",
    '\n "Data_DF"',
    "\n\n\n",
    sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))


# CPUE Fit Data
RepList <- readRDS(paste0(OM.root,'/OM_objects/RepList.rda'))

CPUE_List <- list()

for (OM.n in unique(OMs_DF$n)) {
  CPUE_List[[OM.n]] <- RepList[[OM.n]]$cpue %>%
    dplyr::rename(index=Fleet, year=Yr) %>%
    select(year, index, Vuln_bio, Obs, Exp, Calc_Q, Eff_Q)
}
usethis::use_data(CPUE_List, overwrite = TRUE)

cat("#' @name CPUE_List",
    "\n#' @docType data",
    "\n#' @title CPUE Fitting Information",
    "\n#' @description A list of fitted CPUE indices for each OM",
    "\n#'  ",
    '\n "CPUE_List"',
    "\n\n\n",
    sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))

basecase_replist <- readRDS(paste0(OM.root,'/OM_objects/basecase_replist.rda'))
basecase_CPUE <- basecase_replist$cpue %>%
  dplyr::rename(index=Fleet, year=Yr) %>%
  select(year, index, Vuln_bio, Obs, Exp, Calc_Q, Eff_Q)
usethis::use_data(basecase_CPUE, overwrite = TRUE)

cat("#' @name basecase_CPUE",
    "\n#' @docType data",
    "\n#' @title CPUE Fitting Information for base case OM",
    "\n#' @description Fitted CPUE indices for base case OM",
    "\n#'  ",
    '\n "basecase_CPUE"',
    "\n\n\n",
    sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))


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

cat("#' @name TSBio_List",
    "\n#' @docType data",
    "\n#' @title Time-Series Data of Predicted Spawning Biomass and Recruitment",
    "\n#' @description A list of predicted spawning biomass and recruitment for each OM",
    "\n#'  ",
    '\n "TSBio_List"',
    "\n\n\n",
    sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))





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

cat("#' @name basecase_TSbio",
    "\n#' @docType data",
    "\n#' @title Time-Series Data of Predicted Spawning Biomass and Recruitment",
    "\n#' @description A list of predicted spawning biomass and recruitment for base case OM",
    "\n#'  ",
    '\n "basecase_TSbio"',
    "\n\n\n",
    sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))


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

cat("#' @name RefPoint_DF",
    "\n#' @docType data",
    "\n#' @title Dataframe of Reference Points for each OM",
    "\n#' @description None converged OMs have Depletion=NA",
    "\n#'  ",
    '\n "RefPoint_DF"',
    "\n\n\n",
    sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))


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

cat("#' @name basecase_RefPoint",
    "\n#' @docType data",
    "\n#' @title Dataframe of Reference Points for base case OM",
    "\n#' @description None converged OMs have Depletion=NA",
    "\n#'  ",
    '\n "basecase_RefPoint"',
    "\n\n\n",
    sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))


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

cat("#' @name Select_List",
    "\n#' @docType data",
    "\n#' @title List of estimated selectivity-at-size for each OM",
    "\n#' @description ",
    "\n#'  ",
    '\n "Select_List"',
    "\n\n\n",
    sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))

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

cat("#' @name basecase_Select",
    "\n#' @docType data",
    "\n#' @title List of estimated selectivity-at-size for base case OM",
    "\n#' @description ",
    "\n#'  ",
    '\n "basecase_Select"',
    "\n\n\n",
    sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))

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

cat("#' @name Catch_List",
    "\n#' @docType data",
    "\n#' @title List of observed and predicted catch for each OM",
    "\n#' @description ",
    "\n#'  ",
    '\n "Catch_List"',
    "\n\n\n",
    sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))

catch_dat <- basecase_replist$catch %>%
  dplyr::rename(index=Fleet, year=Yr) %>%
  dplyr::select(index, year, Obs, Exp, se)
catch_dat <- dplyr::left_join(catch_dat, Fleet_DF, by="index")
basecase_catch <- catch_dat
usethis::use_data(basecase_catch, overwrite = TRUE)

cat("#' @name basecase_catch",
    "\n#' @docType data",
    "\n#' @title List of observed and predicted catch for base case OM",
    "\n#' @description ",
    "\n#'  ",
    '\n "basecase_catch"',
    "\n\n\n",
    sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))

# Length data and fit
LenDat_List <- list()
for (OM.n in OMs_DF$n) {
  predLen <- RepList[[OM.n]]$lendbase %>% dplyr::rename(year=Yr, index=Fleet) %>%
    dplyr::select(year, index, Sex, Bin, Obs, Exp, Used, effN)
  predLen <- dplyr::left_join(predLen, Fleet_DF, by="index")
  LenDat_List[[OM.n]] <- predLen
}
usethis::use_data(LenDat_List, overwrite = TRUE)

cat("#' @name LenDat_List",
    "\n#' @docType data",
    "\n#' @title List of observed and predicted length data for each OM",
    "\n#' @description ",
    "\n#'  ",
    '\n "LenDat_List"',
    "\n\n\n",
    sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))

predLen <- basecase_replist$lendbase %>% dplyr::rename(year=Yr, index=Fleet) %>%
  dplyr::select(year, index, Sex, Bin, Obs, Exp, Used, effN)
predLen <- dplyr::left_join(predLen, Fleet_DF, by="index")

basecase_LenDat <- predLen
usethis::use_data(basecase_LenDat, overwrite = TRUE)


cat("#' @name basecase_LenDat",
    "\n#' @docType data",
    "\n#' @title List of observed and predicted length data for base case OM",
    "\n#' @description ",
    "\n#'  ",
    '\n "basecase_LenDat"',
    "\n\n\n",
    sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))


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


#
#
#
# # ---- Compare Simulation with SS3 ----
#
#
# i <- 5
# SLSSdir <- OMgrid.dirs[i]
#
# replist <- r4ss::SS_output(dir=SLSSdir)
#
# # Compare SS OM with OMtool::Simulate
# OM <- get(paste0('OM_', i))
# Hist <- Simulate(OM)
#
# mainyrs <- replist$startyr:replist$endyr
# OM_years <- mainyrs
# maxage <- OM@maxage
#
#
# # ---- Catch ----
# SS_Catch <- replist$catch %>% dplyr::filter(Yr%in%mainyrs)
# SS_Catch <- SS_Catch %>% dplyr::group_by(Yr) %>%
#   dplyr::summarise(C=sum(Exp))
# SimCatch <- apply(Hist@TSdata$Landings, c(1,2), sum)
# ylim_SSB <- c(0, 1.1 * max(c(SS_Catch$C, SimCatch)))
# matplot(OM_years, t(SimCatch),
#         xlab = mainyrs, ylab = "Total Catch",
#         ylim = ylim_SSB, pch = 1, col = "black", typ = "o")
# lines(mainyrs, SS_Catch$C, col = "red", lwd = 3)
# abline(h = 0, col = "grey")
#
#
# # ----- Compare_SS_OM ----
# # add to OMtool
#
# Compare_SS_OM <- function(Hist, replist) {
#   if (class(Hist) != 'Hist')
#   stop('Must provide object of class `Hist` (use `Simulate`)')
#   if (class(replist) != 'list')
#     stop('replist must be class `list` (use `r4ss:SS_output`)')
#   if (is.null(replist$endyr))
#     warning("replist doesn't appear to be output from `r4ss::SS_output`")
#
#   season_as_years <- FALSE
#   if(replist$nseasons == 1 && replist$seasduration < 1) {
#     message("Season-as-years detected in SS model. There is one season in the year with duration of ",
#             replist$seasduration, "year.")
#     season_as_years <- TRUE
#     nseas <- 1/replist$seasduration
#     message("MSEtool operating model is an annual model. Since the SS model is seasonal, we need to aggregate over seasons.\n")
#   } else {
#     nseas <- replist$nseasons
#     if(nseas > 1) {
#       message("MSEtool operating model is an annual model. Since the SS model is seasonal, we need to aggregate over seasons.\n")
#     }
#   }
#
#   mainyrs <- replist$startyr:replist$endyr
#
#   if(season_as_years) {
#     OM_years <- seas1_yind_full$assess_year[seas1_yind]
#     year_lab <- "Stock Synthesis Year"
#   } else {
#     OM_years <- mainyrs
#     year_lab <- "Year"
#   }
#
#   # ---- Numbers-at-age ----
#   SS_at_age <- replist$natage %>%
#     dplyr::filter(Yr %in% mainyrs, `Beg/Mid` =="B")
#   ages <- 0:(OM@maxage)
#   ind <- colnames(SS_at_age)[colnames(SS_at_age) %in% ages]
#   SS_at_age <- SS_at_age %>% tidyr::pivot_longer(cols=ind,
#                                                  names_to="Age",
#                                                  values_to="N")
#   SS_at_age$Age <- as.numeric(SS_at_age$Age)
#
#   SS_at_age <- SS_at_age %>% dplyr::group_by(Age, Yr) %>%
#     dplyr::summarise(N=sum(N))
#
#   nyears <- length(mainyrs)
#   nages <- length(ages)
#
#   # check each year
#   yrind <- 68
#   SS <- SS_at_age %>% dplyr::filter(Yr==mainyrs[yrind])
#   Sim <- apply(Hist@AtAge$Number[,,yrind,], c(1,2), sum)
#
#   plot(ages, SS$N, type="b", col="blue", pch=16)
#   matplot(ages, t(Sim), type='l', add=TRUE)
#
#   SS_TSdata <- replist$timeseries %>%
#     dplyr::filter(Yr %in% mainyrs)
#
#   tt = replist$timeseries%>%
#     dplyr::filter(Yr %in% mainyrs) %>%
#     select(Yr, SpawnBio)
#
#   replist$current_depletion
#   replist$SBzero
#   tt$SpawnBio[length(tt$SpawnBio)]/tt$SpawnBio[1]
#
#   # ---- Total Biomass -----
#   SimBiomass <- apply(Hist@TSdata$Biomass, c(1,2), sum)
#   ylim_SSB <- c(0, 1.1 * max(c(SS_TSdata$Bio_all, SimBiomass)))
#   matplot(OM_years, t(SimBiomass),
#           xlab = year_lab, ylab = "Total Biomass",
#           ylim = ylim_SSB, pch = 1, col = "black", typ = "o")
#   lines(mainyrs, SS_TSdata$Bio_all, col = "red", lwd = 3)
#   abline(h = 0, col = "grey")
#
#   # ---- Spawning Biomass ----
#   SimSB <- apply(Hist@TSdata$SBiomass, c(1,2), sum)
#   ylim_SSB <- c(0, 1) # .1 * max(c(SS_TSdata$SpawnBio, SimSB)))
#   matplot(OM_years, t(SimSB/SimSB[,1]),
#           xlab = year_lab, ylab = "Spawning Biomass",
#           ylim = ylim_SSB, pch = 1, col = "black", typ = "o")
#   lines(mainyrs, SS_TSdata$SpawnBio/SS_TSdata$SpawnBio[1], col = "red", lwd = 3)
#   abline(h = 0, col = "grey")
#
#   # ---- Catch ----
#   SS_Catch <- replist$catch %>% dplyr::filter(Yr%in%mainyrs)
#   SS_Catch <- SS_Catch %>% dplyr::group_by(Yr) %>%
#     dplyr::summarise(C=sum(Exp))
#   SimCatch <- apply(Hist@TSdata$Landings, c(1,2), sum)
#   ylim_SSB <- c(0, 1.1 * max(c(SS_Catch$C, SimCatch)))
#   matplot(OM_years, t(SimCatch),
#           xlab = year_lab, ylab = "Total Catch",
#           ylim = ylim_SSB, pch = 1, col = "black", typ = "o")
#   lines(mainyrs, SS_Catch$C, col = "red", lwd = 3)
#   abline(h = 0, col = "grey")
#
#
# }
#
#
#
#
#
#
#
#
#
#
#
#
