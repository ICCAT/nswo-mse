
# Import SS3 OMs and save as RepList in file.path(OM.root, '/OM_objects/RepList.rda'))

# ---- Install latest version of r4ss ----
# devtools::install_github("r4ss/r4ss", build_vignettes = TRUE, force=TRUE)

library(r4ss); library(dplyr); library(tidyr); library(purrr);
library(ggplot2)

OM.root <- 'G:/My Drive/1_Projects/North_Atlantic_Swordfish/OMs'
OM.object <- file.path(OM.root, 'OM_objects')
OMgrid.dir <- file.path(OM.root, "grid_2022")
OMgrid.dirs <- list.dirs(OMgrid.dir, recursive = TRUE)

# ---- Save OM replist ----

for (i in seq_along(OMgrid.dirs)) {
  dir <- OMgrid.dirs[i]
  if (!any(grepl('Report.sso',list.files(dir)))) {
    # top level dir
    print(dir)
  } else {
    # import replist
    out.dir <- file.path(OM.object, 'Replists')
    if (!dir.exists(out.dir))  dir.create(out.dir)
    out.dir <- file.path(OM.object, 'Replists', basename(dir))
    if (!dir.exists(out.dir))  dir.create(out.dir)
    replist <- suppressWarnings(r4ss::SS_output(dir, verbose = FALSE,
                                                hidewarn = TRUE,
                                                printstats=FALSE))

    saveRDS(replist, file.path(out.dir, 'replist.rda'))
  }
}

# ---- Save Data List ----

# Only use Data from Base Case Model
SS.dir <- file.path(OMgrid.dir, '000_base_case')
SWO_Data <- r4ss::SS_readdat(file.path(SS.dir, 'SWOv5.dat'),
                                  version='3.30',
                                  verbose = FALSE)

saveRDS(SWO_Data, paste0(OM.root, '/OM_objects/DataList.rda'))

# --- Fleet DF ----
data <- readRDS(paste0(OM.root,'/OM_objects/DataList.rda'))
fleet.names <- data$fleetnames
fleet.index <- seq_along(fleet.names)
Fleet_DF <- data.frame(Code=fleet.names,
                       Name=c('EU-Spain longline (LL)',
                              'USA LL',
                              'Canada LL',
                              'Japan LL - Early',
                              'Japan LL - Late',
                              'EU-Portugal LL',
                              'Chinese-Taipai LL - Early',
                              'Chinese-Taipai LL - Late',
                              'Morocco LL',
                              'Canada/USA Harpoon',
                              'Other - LL by the other CPCs, and all other gears except HP',
                              'USA Survey',
                              'EU-Portugal Survey',
                              'Age-1 Survey',
                              'Age-2 Survey',
                              'Age-3 Survey',
                              'Age-4 Survey',
                              'Age-5+ Survey',
                              'Combined Index'),
                       index=fleet.index)
Fleet_DF$Code <- factor(Fleet_DF$Code, levels=Fleet_DF$Code, ordered = TRUE)
Fleet_DF$Name <- factor(Fleet_DF$Name, levels=Fleet_DF$Name, ordered = TRUE)
Fleet_DF <- Fleet_DF %>% dplyr::select(index, Name, Code)

saveRDS(Fleet_DF, paste0(OM.root, '/OM_objects/Fleet_DF.rda'))

# ---- Save Likelihood List ----

rep.dirs <- list.dirs(file.path(OM.object, 'Replists'), recursive = FALSE)

for (i in seq_along(rep.dirs)) {
  replist <- readRDS(file.path(rep.dirs[i], 'replist.rda'))
  likelihoods <- list(replist$likelihoods_used %>% t() %>% data.frame(),
                      replist$likelihoods_by_fleet)

  dir.create(file.path(OM.object, 'Likelihoods', basename(rep.dirs[i])))
  saveRDS(likelihoods, file.path(OM.object, 'Likelihoods', basename(rep.dirs[i]), 'likelihoods.rda'))
}



# ---- Save CPUE List ----

for (i in seq_along(rep.dirs)) {
  replist <- readRDS(file.path(rep.dirs[i], 'replist.rda'))
  cpues <- replist$cpue %>%
    dplyr::rename(index=Fleet, year=Yr) %>%
    select(year, index, Vuln_bio, Obs, Exp, Calc_Q, Eff_Q)

  dir.create(file.path(OM.object, 'CPUEs', basename(rep.dirs[i])))
  saveRDS(cpues, file.path(OM.object, 'CPUEs', basename(rep.dirs[i]), 'cpues.rda'))
}


# ---- Time-Series - Biology ----

for (i in seq_along(rep.dirs)) {
  replist <- readRDS(file.path(rep.dirs[i], 'replist.rda'))

  TSbio_dat <- replist$recruit %>%
    dplyr::select(year=Yr, SSB=SpawnBio, Exp_Rec=exp_recr, Exp_Rec_biasadj=bias_adjusted,
                  Obs_Rec=pred_recr, dev=dev)
  TSbio_dat$SB0 <-replist$SBzero
  R0 <- replist$timeseries %>% filter(Era=="VIRG") %>% select(R0=Recruit_0)
  TSbio_dat$R0 <- R0$R0
  TSbio_dat$Depletion <- TSbio_dat$SSB/TSbio_dat$SB0
  TSbio_dat$SS_Depletion <- replist$current_depletion

  derived_quants <- replist$derived_quants
  FMSY <- derived_quants$Value[derived_quants$Label=='annF_MSY']
  Kobe <- replist$Kobe %>% dplyr::rename(year=Yr)
  TSbio_dat <- dplyr::full_join(TSbio_dat, Kobe,by="year")
  TSbio_dat$F <- TSbio_dat$F.Fmsy*FMSY

  dir.create(file.path(OM.object, 'Timeseries', basename(rep.dirs[i])))
  saveRDS(TSbio_dat, file.path(OM.object, 'Timeseries', basename(rep.dirs[i]), 'timeseries.rda'))
}

# ---- Reference Points -----

for (i in seq_along(rep.dirs)) {
  replist <- readRDS(file.path(rep.dirs[i], 'replist.rda'))

  derived <- replist$derived_quants

  derived %>% dplyr::filter(Label %in% c('SSB_Virgin', 'SSB_MSY', 'Recr_Virgin',
                                         'SPR_MSY','annF_MSY', 'Dead_Catch_MSY',
                                         'Ret_Catch_MSY'))

  TSbio_dat <- readRDS(file.path(OM.object, 'Timeseries', basename(rep.dirs[i]), 'timeseries.rda'))

  conv <- !TSbio_dat$SS_Depletion %>% is.na() %>% all()
  if (conv) {
    TSbio_dat$year
    years <- min(TSbio_dat$year):(replist$endyr+1)
    SSB_DF <- TSbio_dat %>% filter(year %in% years)
    finalSB <- SSB_DF %>% dplyr::filter(year==max(year))
  } else{
    finalSB <- data.frame(F.Fmsy=NA, B.Bmsy=NA, Depletion=NA)
  }

  df <- data.frame(
                   MSY_d=derived$Value[derived$Label=='Dead_Catch_MSY'],
                   MSY_r=derived$Value[derived$Label=='Ret_Catch_MSY'],
                   SBMSY=derived$Value[derived$Label=='SSB_MSY'],
                   FMSY=derived$Value[derived$Label=='annF_MSY'],
                   F_FMSY=finalSB$F.Fmsy,
                   SB_SBMSY=finalSB$B.Bmsy,
                   SBMSY_SB0=derived$Value[derived$Label=='SSB_MSY']/derived$Value[derived$Label=='SSB_Virgin'],
                   Depletion=finalSB$Depletion)



  dir.create(file.path(OM.object, 'Ref_Points', basename(rep.dirs[i])))
  saveRDS(df, file.path(OM.object, 'Ref_Points', basename(rep.dirs[i]), 'ref_points.rda'))
}

# ---- Selectivity and Retention ----


for (i in seq_along(rep.dirs)) {
  replist <- readRDS(file.path(rep.dirs[i], 'replist.rda'))

  SizeSelect <- replist$sizeselex %>% dplyr::rename(index=Fleet, year=Yr)
  SizeSelect <- SizeSelect %>% tidyr::pivot_longer(6:ncol(SizeSelect),
                                                   'Length.Class', values_to="Prob")
  SizeSelect <- left_join(SizeSelect, Fleet_DF, by="index")
  SizeSelect$Length.Class <- as.numeric(SizeSelect$Length.Class)

  SizeSelect <- SizeSelect %>% filter(Sex==1,
                                      Factor%in%c('Ret', 'Lsel'),
                                      year%in%c(1950, 2020),
                                      Name %in% Fleet_DF$Name[1:11])
  SizeSelect$Factor[SizeSelect$Factor=='Lsel'] <- 'Selected'
  SizeSelect$Factor[SizeSelect$Factor=='Ret'] <- 'Retained'

  dir.create(file.path(OM.object, 'Select_Retain', basename(rep.dirs[i])))
  saveRDS(SizeSelect, file.path(OM.object, 'Select_Retain', basename(rep.dirs[i]), 'select.rda'))
}


# ---- Catch data and fit ----

for (i in seq_along(rep.dirs)) {
  replist <- readRDS(file.path(rep.dirs[i], 'replist.rda'))

  catch_dat <- replist$catch %>%
    dplyr::rename(index=Fleet, year=Yr) %>%
    dplyr::select(index, year, Obs, Exp, se)
  catch_dat <- dplyr::left_join(catch_dat, Fleet_DF, by="index")


  dir.create(file.path(OM.object, 'Catch', basename(rep.dirs[i])))
  saveRDS(catch_dat, file.path(OM.object, 'Catch', basename(rep.dirs[i]), 'catch.rda'))
}


# ---- Length data and fit ----

for (i in seq_along(rep.dirs)) {
  replist <- readRDS(file.path(rep.dirs[i], 'replist.rda'))

  predLen <- replist$lendbase %>% dplyr::rename(year=Yr, index=Fleet) %>%
    dplyr::select(year, index, Sex, Bin, Obs, Exp, Used, effN)
  predLen <- dplyr::left_join(predLen, Fleet_DF, by="index")


  dir.create(file.path(OM.object, 'CAL', basename(rep.dirs[i])))
  saveRDS(predLen, file.path(OM.object, 'CAL', basename(rep.dirs[i]), 'cal.rda'))
}




