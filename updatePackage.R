library(SWOMSE)



# devtools::install_github("r4ss/r4ss", build_vignettes = TRUE, force=TRUE)

# -------- Build Package Data ----------

# ---- Add SS OMs as Data ----

OM.root <- '../../../Google Drive/Projects/Projects_2020/ICCAT_Swordfish/OMs/SS/2018_GRID'
OMgrid.dir <- file.path(OM.root, "grid")
OM.dirs <- list.dirs(OMgrid.dir, recursive = FALSE)

library(r4ss)

# -- Import objects from SS OMs and save in 'OM_objects' --
# -- Only need to run this if SS OMs in OM.dirs are updated --
# saveTempObject(OM.dirs)

# -- Import temporary objects and save as data objects --
# SS OM Overview
OMs_DF <- readRDS('OM_objects/OM_DF.rda')
usethis::use_data(OMs_DF, overwrite = TRUE)

# Likelihood List
Like_List <- readRDS('OM_objects/LHlist.rda')
usethis::use_data(Like_List, overwrite = TRUE)

# Fleet Information
DataList <- readRDS('OM_objects/DataList.rda')
data <- DataList[[1]] # same dat for all OMs
fleet.names <- data[[1]]$fleetnames
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
  dplyr::select(year, index, CPUE_Obs, CPUE_SE_log)
CPUEDat <- dplyr::left_join(CPUEDat, Fleet_DF, by="index")

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
for (OM.n in OMs_DF$n) {
  CPUE_List[[OM.n]] <- RepList[[OM.n]]$cpue %>%
    dplyr::rename(index=Fleet, year=Yr) %>%
    select(year, index, Vuln_bio, Obs, Exp, Calc_Q, Eff_Q)
}
usethis::use_data(CPUE_List, overwrite = TRUE)

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


document_data()

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

# Fits to Catches




head(Data_DF)

# Fits to Size Comps


predLen <- RepList[[OM.n]]$lendbase %>% dplyr::rename(year=Yr, index=Fleet) %>%
  dplyr::select(year, index, Sex, Bin, Obs, Exp, Used, effN, N)
predLen <- dplyr::left_join(predLen, Fleet_DF, by="index")

# Plot residuals over time
tt <- predLen %>% filter(Name=="Spain")
t2 <- fit.diagnostics(tt)

t2$color <- 1
t2$color[t2$res < 0] <- 2
t2$color <- as.factor(t2$color)

ggplot(t2, aes(x=year, y=Bin)) +
  geom_point(aes(size=Obs^0.5*res, color=color))

# To do - color residual plots for length comps


tt$res <- log(tt$Obs) - log(tt$Exp)

predLen %>% group_by(Name)


# Plot aggregated fits
tt <- predLen %>% group_by(Name, Bin) %>% summarize(Obs=sum(Obs), Exp=sum(Exp))


ggplot(tt, aes(x=Bin)) +
  facet_wrap(~Name, scale="free_y") +
  geom_bar(aes(y=Obs), stat='identity') +
  geom_line(aes(y=Exp), size=1)



dat <- Data_DF %>% filter(Code=="SPN_1") %>% select(year, Length.Class, Length.Comp)
dat <- dat[!is.na(dat$Length.Comp),]









# Fits to Length Comps

OMs_DF %>% filter(M==0.2, sigmaR==0.6, h==0.75, cpue_cv==0.6, L_ESS==2,
                  llq==1.01, env==FALSE)
OM.n <- 4









TL <- 1
TR <- 2
BL <- 3
BR <- 4

xloc <- 0.4
yloc <- 0.1
maxV <- max(ab)
textDF <- data.frame(x=c(xloc, maxV-xloc, xloc, maxV-xloc),
                     y=c(maxV-yloc, maxV-yloc, yloc, yloc),
                     label=paste0(c(TL, TR, BL, BR), "%"),
                     color=1, val=2,
                     stringsAsFactors = FALSE)





head(SSB_DF)

P <- ggplot(DF) +
  geom_tile(na.rm=TRUE, aes(x=x, y=y, fill=color, alpha=val)) +
  scale_fill_manual(values=values,na.value = 'white') +
  theme_classic() +



head(SizeSelect)

SizeSelect %>% filter(Name=="Spain", Factor=='Lsel', Sex==1, year%in%c(1950))
SizeSelect %>% filter(Name=="Spain", Factor=='Lsel', Sex==2, year%in%c(1950))






cols <- names(LenDat)[7:length(names(LenDat))] # convert to long
LenDat <- LenDat %>% tidyr::pivot_longer(all_of(cols), 'Length.Class', values_to="Length.Comp") %>%




tt <- RepList[[OM.n]]$exploitation





# ---- Reference Points ----




input <- 'RMD_Dev/OM_Report.RMD'
RMDfileout <- 'RMD_Dev/OM_Report.html'

datList <- list(OMinfo=OMselect,
                data=DataList[[OMselect$n]],
                likelihood=LHlist[[OMselect$n]],
                cpue=cpue,
                SpRecDF=SpRecDF)

rmarkdown::render(input, params=list(datList))

utils::browseURL(RMDfileout)


RepList[[OMselect$n]]$catch







