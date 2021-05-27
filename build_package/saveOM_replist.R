
# Import SS3 OMs and save as RepList in file.path(OM.root, '/OM_objects/RepList.rda'))

# ---- Install latest version of r4ss ----
# devtools::install_github("r4ss/r4ss", build_vignettes = TRUE, force=TRUE)

library(r4ss); library(dplyr); library(tidyr); library(purrr)

OM.root <- 'G:/My Drive/1_Projects/North_Atlantic_Swordfish/OMs/grid_2021'
OMgrid.dir <- file.path(OM.root, "grid_May2021_shifted")
OMgrid.dirs <- list.dirs(OMgrid.dir, recursive = FALSE)

ord <- lapply(strsplit(OMgrid.dirs, 'iter'), '[[', 2) %>% as.numeric() %>% order()
OMgrid.dirs <- OMgrid.dirs[ord]

# Based on the 2017 assessment - does not include discard mortality and retention curve
OMbase.dir <-  file.path(OM.root, "Michael_March2020/NSWO_MSE_SS3_Base_v2")

# ---- Save OM replist ----
RepList <- list()

for (i in seq_along(OMgrid.dirs)) {
  message(i)
  SS.dir <- OMgrid.dirs[i]
  RepList[[i]] <- suppressWarnings(r4ss::SS_output(SS.dir, verbose = FALSE,
                                              hidewarn = TRUE,
                                              printstats=FALSE))
}
saveRDS(RepList, paste0(OM.root, '/OM_objects/RepList.rda'))


# ---- Save Data List ----
DataList <- list()

for (i in seq_along(OMgrid.dirs)) {
  message(i)
  SS.dir <- OMgrid.dirs[i]
  DataList[[i]] <- r4ss::SS_readdat(file.path(SS.dir, 'SWO.dat'),
                                    version='3.30',
                                    verbose = FALSE)
}
saveRDS(DataList, paste0(OM.root, '/OM_objects/DataList.rda'))

# ---- Save Likelihood List ----
if (!exists('RepList')) RepList <- readRDS(paste0(OM.root, '/OM_objects/RepList.rda'))

LHlist <- list()
for (i in seq_along(RepList)) {
  replist <- RepList[[i]]
  LHlist[[i]] <- list(replist$likelihoods_used %>% t() %>% data.frame(),
                      replist$likelihoods_by_fleet)

}
saveRDS(LHlist, paste0(OM.root, '/OM_objects/LHlist.rda'))


# ---- Save CPUE List ----
CPUE_List <- list()

for (i in seq_along(RepList)) {
  CPUE_List[[i]] <- RepList[[i]]$cpue %>%
    dplyr::rename(index=Fleet, year=Yr) %>%
    select(year, index, Vuln_bio, Obs, Exp, Calc_Q, Eff_Q)
}
saveRDS(CPUE_List, paste0(OM.root, '/OM_objects/CPUE_List.rda'))

# ---- Time-Series - Biology ----
TSBio_List <- list()
for (i in seq_along(RepList)) {
  TSbio_dat <- RepList[[i]]$recruit %>%
    dplyr::select(year=Yr, SSB=SpawnBio, Exp_Rec=exp_recr, Exp_Rec_biasadj=bias_adjusted,
                  Obs_Rec=pred_recr, dev=dev)
  TSbio_dat$SB0 <- RepList[[i]]$SBzero
  R0 <- RepList[[i]]$timeseries %>% filter(Era=="VIRG") %>% select(R0=Recruit_0)
  TSbio_dat$R0 <- R0$R0
  TSbio_dat$Depletion <- TSbio_dat$SSB/TSbio_dat$SB0
  TSbio_dat$SS_Depletion <- RepList[[i]]$current_depletion

  derived_quants <- RepList[[i]]$derived_quants
  FMSY <- derived_quants$Value[derived_quants$Label=='annF_MSY']
  Kobe <- RepList[[i]]$Kobe %>% dplyr::rename(year=Yr)
  TSbio_dat <- dplyr::full_join(TSbio_dat, Kobe,by="year")
  TSbio_dat$F <- TSbio_dat$F.Fmsy*FMSY
  TSBio_List[[i]] <- TSbio_dat
}

saveRDS(TSBio_List, paste0(OM.root, '/OM_objects/TSBio_List.rda'))


# ---- Reference Points -----
tempList <- list()
for (i in seq_along(RepList)) {
  derived <- RepList[[i]]$derived_quants
  derived %>% dplyr::filter(Label %in% c('SSB_Virgin', 'SSB_MSY', 'Recr_Virgin',
                                         'SPR_MSY','annF_MSY', 'Dead_Catch_MSY',
                                         'Ret_Catch_MSY'))

  TSbio_dat <- TSBio_List[[i]]
  conv <- !TSbio_dat$SS_Depletion %>% is.na() %>% all()
  if (conv) {
    TSbio_dat$year
    years <- min(TSbio_dat$year):(RepList[[i]]$endyr+1)
    SSB_DF <- TSbio_dat %>% filter(year %in% years)
    finalSB <- SSB_DF %>% dplyr::filter(year==max(year))
  } else{
    finalSB <- data.frame(F.Fmsy=NA, B.Bmsy=NA, Depletion=NA)
  }

  tempList[[i]] <- data.frame(i=i,
                              MSY_d=derived$Value[derived$Label=='Dead_Catch_MSY'],
                              MSY_r=derived$Value[derived$Label=='Ret_Catch_MSY'],
                              SBMSY=derived$Value[derived$Label=='SSB_MSY'],
                              FMSY=derived$Value[derived$Label=='annF_MSY'],
                              F_FMSY=finalSB$F.Fmsy,
                              SB_SBMSY=finalSB$B.Bmsy,
                              SBMSY_SB0=derived$Value[derived$Label=='SSB_MSY']/derived$Value[derived$Label=='SSB_Virgin'],
                              Depletion=finalSB$Depletion)

}
RefPoint_DF <- do.call('rbind', tempList)
saveRDS(RefPoint_DF, paste0(OM.root, '/OM_objects/RefPoint_DF.rda'))

# --- Fleet DF ----
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

# ---- Selectivity and Retention ----
# Selectivity data
Select_List <- list()

for (i in seq_along(RepList)) {
  SizeSelect <- RepList[[i]]$sizeselex %>% dplyr::rename(index=Fleet, year=Yr)
  SizeSelect <- SizeSelect %>% tidyr::pivot_longer(6:ncol(SizeSelect),
                                                   'Length.Class', values_to="Prob")
  SizeSelect <- left_join(SizeSelect, Fleet_DF, by="index")
  SizeSelect$Length.Class <- as.numeric(SizeSelect$Length.Class)


  SizeSelect <- SizeSelect %>% filter(Sex==1,
                                      Factor%in%c('Ret', 'Lsel'),
                                      year==max(RepList[[i]]$endyr),
                                      Name %in% Fleet_DF$Name[1:11])
  SizeSelect$Factor[SizeSelect$Factor=='Lsel'] <- 'Selected'
  SizeSelect$Factor[SizeSelect$Factor=='Ret'] <- 'Retained'
  Select_List[[i]] <- SizeSelect
}
saveRDS(Select_List, paste0(OM.root, '/OM_objects/Select_List.rda'))


# ---- Catch data and fit ----
Catch_List <- list()
for (i in seq_along(RepList)) {
  catch_dat <- RepList[[i]]$catch %>%
    dplyr::rename(index=Fleet, year=Yr) %>%
    dplyr::select(index, year, Obs, Exp, se)
  catch_dat <- dplyr::left_join(catch_dat, Fleet_DF, by="index")
  Catch_List[[i]] <- catch_dat
}
saveRDS(Catch_List, paste0(OM.root, '/OM_objects/Catch_List.rda'))

# ---- Length data and fit ----
LenDat_List <- list()
for (i in seq_along(RepList)) {
  predLen <- RepList[[i]]$lendbase %>% dplyr::rename(year=Yr, index=Fleet) %>%
    dplyr::select(year, index, Sex, Bin, Obs, Exp, Used, effN)
  predLen <- dplyr::left_join(predLen, Fleet_DF, by="index")
  LenDat_List[[i]] <- predLen
}
saveRDS(LenDat_List, paste0(OM.root, '/OM_objects/LenDat_List.rda'))

