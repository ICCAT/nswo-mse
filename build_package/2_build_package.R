# Import OMs and Data and add to SWOMSE package

nsim <- 50 # number of simulations per OM
proyears <- 33 # number of projection years

OM.root <- 'G:/My Drive/1_Projects/North_Atlantic_Swordfish/OMs'
OMgrid.dir <- file.path(OM.root,'grid_2022')
OMgrid.dirs <- list.dirs(OMgrid.dir, recursive = TRUE)

OMgrid.dirs <- OMgrid.dirs[!grepl('other_not_currently_used', OMgrid.dirs)]

SSData <- r4ss::SS_readdat(file.path(OMgrid.dir, '000_base_case/SWOv5.dat'))

# ---- Install latest version of r4ss ----
# devtools::install_github("r4ss/r4ss", build_vignettes = TRUE, force=TRUE)

library(r4ss); library(dplyr); library(tidyr)
library(MSEtool); library(usethis); library(purrr);
library(ggplot2)

# ---- Delete existing data ----
fls <- list.files('data')
file.remove(file.path('data', fls))

# ----- Documentation ----
RoxygenFile <- 'Roxygen.r'
if (file.exists(file.path('R/', RoxygenFile)))
  file.remove(file.path('R/', RoxygenFile)) # delete
file.create(file.path('R/', RoxygenFile)) # make empty file

cat("# This file is automatically built by build_package/build_package.r\n",
    "# Don't edit by hand!\n",
    "# \n\n", sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))

# ---- Save Fleet Information ----
Fleet_DF <- readRDS(paste0(OM.root, '/OM_objects/Fleet_DF.rda'))
usethis::use_data(Fleet_DF, overwrite = TRUE)

cat("\n#' @name Fleet_DF",
    "\n#' @docType data",
    "\n#' @title Fleet Information",
    "\n#' @description A dataframe of information on fleet names",
    "\n#'  ",
    '\n "Fleet_DF"',
    "\n\n\n",
    sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))


# ---- Add SWO_Data ----
SWOData <- MSEtool::XL2Data("inst/SWO_Data")

cat("#' @name SWOData",
    "\n#' @docType data",
    "\n#' @title North Atlantic Swordfish Data",
    "\n#' @description North Atlantic Swordfish Data",
    "\n#'  ",
    '\nNULL',
    "\n\n\n",
    sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))

# ---- Create OM Data Frame -----

title_case <- function(name) {
  tt <- stringr::str_split(name, ' ')
  chk <- lapply(tt, stringr::str_detect, "[[:upper:]]")[[1]]
  for (i in seq_along(chk)) {
    if (!chk[i])
      if (nchar(tt[[1]][i])>1)
        tt[[1]][i] <- stringr::str_to_title(tt[[1]][i])
  }
  paste(tt[[1]], collapse = ' ')
}

get_OM_details <- function(dir,  OMgrid.dir) {

  if (!any(grepl('Report.sso',list.files(dir)))) {

  } else {
    text <- dir %>% basename() %>% strsplit(.,'_')
    text <- text[[1]]
    OM.num <- text[1]

    if (grepl('base_case', dir)) {

      df <- data.frame(M=0.2,
                       sigmaR=0.2,
                       steepness=0.88,
                       cpuelambda=TRUE,
                       llq=1,
                       env=7,
                       Class='Base Case'
                       )
    } else {
      text <- text[-1]
      text[6] <- gsub('-5', 0, text[6])
      text[6] <- gsub('5', 1, text[6])

      df <- data.frame(text) %>% tidyr::separate(text, into = c("Parameter", "Value"),
                                                 sep = "(?<=[A-Za-z])(?=[0-9])")
      df <- df %>% tidyr::pivot_wider(., names_from = Parameter, values_from = Value)
    }

    if (grepl('Reference', dir)) {
      df$Class <- 'Reference'
    }
    if (grepl('Robustness', dir)) {
      dir2 <- strsplit(dir, 'Robustness/')[[1]][2]
      name <- strsplit(dir2, '/')[[1]][1]
      name <- sub("_", '. ', name)
      name <- sub("_", ' ', name)
      name <- title_case(name)
      df$Class <- name
    }

    df$OM.num <- OM.num
    df$env[df$env=='7'] <- '1'
    df$cpuelambda[df$cpuelambda==1] <- TRUE
    df$cpuelambda[df$cpuelambda==20] <- FALSE
    df$OM.object <- paste0('MOM_', df$OM.num)
    df$dir <- strsplit(dir, OMgrid.dir)[[1]][2]

    return(df)
  }
}

OM_list <- lapply(OMgrid.dirs, get_OM_details,  OMgrid.dir= OMgrid.dir)
OM_list[sapply(OM_list, is.null)] <- NULL
OM_DF <- do.call('rbind',OM_list)

OM_DF <- OM_DF %>% mutate(across(c(1:3, 5:6), as.numeric))
OM_DF <- OM_DF %>% rename('Include CAL'=cpuelambda)

OM_DF$Class <- factor(OM_DF$Class,
                      levels=unique(OM_DF$Class),
                      ordered = TRUE)

OM_DF$`Include CAL` <- as.logical(OM_DF$`Include CAL`)

OM_DF <- OM_DF %>% relocate(OM.object)

# ---- Import OMs ----

docOM <- function(OMname) {
  cat("#' @rdname SWO-OMs \n", "'", OMname, "'\n",
      sep="", append=TRUE,
      file=file.path('R/', RoxygenFile)
  )
}

importOM <- function(i, nsim, proyears, OM_DF, SWOData) {
  message(i)
  SS.dir <- file.path(OMgrid.dir, OM_DF$dir[i])

  # import MOM
  replist <- r4ss::SS_output(SS.dir)

  MOM <- SS2MOM(replist, nsim=nsim, proyears = proyears, interval = 1)

  # get fleet-specific indices
  nyears <- MOM@Fleets[[1]][[1]]@nyears
  Data <- SWOData
  n.stock <- length(MOM@Stocks)
  n.fleet <- length(MOM@cpars[[1]])
  n.survey.fleet <- nrow(MOM@cpars[[1]][[1]]$Data@AddInd[1,,])

  Data@AddInd <- array(NA, dim=c(1, n.fleet+n.survey.fleet, nyears))
  Data@CV_AddInd <- Data@AddInd
  n_age <- MOM@Stocks[[1]]@maxage+1
  Data@AddIndV <- array(NA, dim=c(1, n.fleet+n.survey.fleet, n_age))
  Data@AddIunits <- rep(1,n.fleet+n.survey.fleet) # to be updated

  fleet.ind.units <- c(1,0,0,0,0,1,0,0,1,0,0) # from SWOv5.dat
  # add fleet indices
  for (fl in 1:(n.fleet+n.survey.fleet)) {
    if (fl <= n.fleet) {
      ind <- MOM@cpars[[1]][[fl]]$Data@VInd[1,]
      cv_ind <- MOM@cpars[[1]][[fl]]$Data@CV_VInd[1,]
      units <- fleet.ind.units[fl]
      vul <- MOM@cpars[[1]][[fl]]$V[1,,nyears]
    } else {
      fl2 <- fl-n.fleet
      ind <- MOM@cpars[[1]][[1]]$Data@AddInd[1,fl2,]
      cv_ind <- MOM@cpars[[1]][[1]]$Data@CV_AddInd[1,fl2,]
      units <- MOM@cpars[[1]][[1]]$Data@AddIunits[fl2]
      vul <- MOM@cpars[[1]][[1]]$Data@AddIndV[1,fl2,]
    }
    if (length(cv_ind)<nyears)
      cv_ind <- rep(cv_ind[1], nyears)
    Data@AddInd[1,fl, ] <- ind
    Data@CV_AddInd[1,fl,] <- cv_ind
    Data@AddIunits[fl] <- units
    Data@AddIndV[1,fl,] <- vul
  }

  # name the dimensions
  dimnames(Data@AddInd)[[1]] <- 1
  dimnames(Data@AddInd)[[2]] <- Fleet_DF$Code
  dimnames(Data@AddInd)[[3]] <- SWOData@Year

  # Calculate vulnerability schedules for fleet-specific indices
  nstock <- 2
  nage <- MOM@Stocks[[1]]@maxage + 1
  nind <- n.fleet+n.survey.fleet
  AddIV <- array(NA, dim=c(nstock, nage, nind, nyears+proyears))

  age_select <- replist$ageselex
  fleet_names <- replist$FleetNames
  fleet_index <- seq_along(fleet_names)
  fleet_index_list <- list()
  for (fl in fleet_index) {
    df <- age_select %>% filter(Fleet==fl, Factor=='Asel2') %>%
      tidyr::pivot_longer(., cols=8:ncol(age_select), names_to = 'Age',
                          values_to='Select') %>%
      select(Fleet_id=Fleet, Year=Yr, Sex, Age, Select) %>%
      filter(Year>=replist$startyr)
    for (s in unique(df$Sex)) {
      temp <-  df %>% filter(Sex==s)
      AddIV[s,,fl,1:nyears] <- temp$Select
      AddIV[s,,fl,(nyears+1):(nyears+proyears)] <- AddIV[s,,fl,nyears]
    }
  }

  # drop the combined index (already in Data@Ind)
  Data@AddInd <- Data@AddInd[,-19,, drop=FALSE]
  Data@CV_AddInd <- Data@CV_AddInd[,-19,, drop=FALSE]
  Data@AddIunits <- Data@AddIunits[-19]
  Data@AddIndV <- Data@AddIndV[,-19,, drop=FALSE]
  AddIV <- AddIV[,,-19, ,drop=FALSE]

  # drop empty indices
  max.vals <- suppressWarnings(apply(Data@AddInd[1,,], 1, max, na.rm=TRUE))
  ind <- which(!is.finite(max.vals))
  Data@AddInd <- Data@AddInd[,-ind,, drop=FALSE]
  Data@CV_AddInd <- Data@CV_AddInd[,-ind,, drop=FALSE]
  Data@AddIunits <- Data@AddIunits[-ind]
  Data@AddIndV <- Data@AddIndV[,-ind,, drop=FALSE]
  AddIV <- AddIV[,,-ind, ,drop=FALSE]

  # drop early indices
  ind <- which(rowSums(Data@AddInd[1,,60:71], na.rm=T)==0)
  Data@AddInd <- Data@AddInd[,-ind,, drop=FALSE]
  Data@CV_AddInd <- Data@CV_AddInd[,-ind,, drop=FALSE]
  Data@AddIunits <- Data@AddIunits[-ind]
  Data@AddIndV <- Data@AddIndV[,-ind,, drop=FALSE]
  AddIV <- AddIV[,,-ind, ,drop=FALSE]

  # drop age-specific indices
  ind <- which(grepl('Age-', dimnames(Data@AddInd)[[2]]))
  Data@AddInd <- Data@AddInd[,-ind,, drop=FALSE]
  Data@CV_AddInd <- Data@CV_AddInd[,-ind,, drop=FALSE]
  Data@AddIunits <- Data@AddIunits[-ind]
  Data@AddIndV <- Data@AddIndV[,-ind,, drop=FALSE]
  AddIV <- AddIV[,,-ind, ,drop=FALSE]

  # Add catch-at-length data
  CAL_fleets <- 3 # use CAN LL CAL data - longest with logistic selectivity
  nsamp <- 200 # assumed annual sample size

  len_dat <- SSData$lencomp %>% filter(FltSvy==CAL_fleets)
  len_dat <- len_dat[,c(1, 3, 7:ncol(len_dat))]
  len_dat <- len_dat %>% tidyr::pivot_longer(., cols=3:ncol(len_dat))
  len_dat$Length <- readr::parse_number(len_dat$name)

  len_dat <- len_dat %>% select(Year=Yr, Fleet=FltSvy, Value=value, Length=Length) %>%
    group_by(Year, Fleet, Length) %>%
    summarise(Value=sum(Value)) %>%
    group_by(Year, Fleet) %>%
    mutate(Value=Value/sum(Value))

  Data@CAL_mids <- MOM@cpars$Female$CAN_3$CAL_binsmid
  nbins <- length(Data@CAL_mids)
  by <-  Data@CAL_mids[2] -  Data@CAL_mids[1]

  len_dat$Len_Mid <- len_dat$Length+0.5*by
  Data@CAL_bins <- MOM@cpars$Female$CAN_3$CAL_bins

  Data@CAL <- array(NA, dim=c(1,length(Data@Year), nbins))
  for (yy in seq_along(Data@Year)) {
    len_dat_y <- len_dat %>% filter(Year==Data@Year[yy])
    if (nrow(len_dat_y)>0) {
      # match bins
      ind <- match(len_dat_y$Len_Mid, Data@CAL_mids)
      Data@CAL[1,yy,] <- 0
      Data@CAL[1,yy,ind] <- len_dat_y$Value * nsamp
    }
  }

  Data@Vuln_CAL <- matrix(MOM@cpars$Female$CAN_3$retL[1,,nyears], nrow=1)

  # aggregate all fleets into one
  MOM_com <- MOM %>% MSEtool::MOM_agg_fleets(.)

  MOM_com@proyears <- proyears

  # add data to female and male stocks
  for (p in 1:n.stock) {
    MOM_com@cpars[[p]][[1]]$Data <- Data
    MOM_com@cpars[[p]][[1]]$AddIbeta <- matrix(1, nrow=nsim, ncol=dim(Data@AddInd)[2])
    MOM_com@cpars[[p]][[1]]$I_beta <- rep(1, nsim)
    MOM_com@cpars[[p]][[1]]$Cobs_y <- matrix(1, nrow=nsim, ncol=nyears+MOM_com@proyears)

    # Add CAL bins to cpars
    MOM_com@cpars[[p]][[1]]$CAL_bins <-  Data@CAL_bins
    MOM_com@cpars[[p]][[1]]$CAL_binsmid <- Data@CAL_mids

    # Add additional index vulnerability schedules
    v_sched <- replicate(nsim,AddIV[p,,,])
    v_sched <- aperm(v_sched, c(4,1:3))
    MOM_com@cpars[[p]][[1]]$AddIV <- v_sched
  }

  # Drop Obs and Imp for other fleets
  for (p in 1:n.stock) {
    drop.fleet <- 2:(n.fleet+n.survey.fleet)
    MOM_com@Obs[[p]] <- MOM_com@Obs[[p]][-drop.fleet]
    MOM_com@Imps[[p]] <- MOM_com@Imps[[p]][-drop.fleet]
  }

  # assign name
  vals <- OM_DF[i,]
  vals$OM.num <- vals$dir <- vals$OM.object <- NULL
  vals[7] <- as.character(vals[7][1,1])
  name <- paste(names(vals), vals, collapse = ' ', sep=':')

  MOM_com@Name <- name
  n.fleet <- length(MOM_com@cpars[[1]])

  # map real data across stocks - data is not sex-specific
  MOM_com@cpars[[1]][[1]]$Real.Data.Map <- matrix(1, nrow=n.fleet, ncol=n.stock)

  # Combined Index - years to calculate deviations
  years <- replist$startyr:replist$endyr
  ind_yrs <- 1999:2020
  MOM_com@cpars[[1]][[1]]$Ind_Yrs <-match(ind_yrs, years)

  name <- OM_DF$OM.object[i]

  assign(name, MOM_com)

  do.call("use_data", list(as.name(name), overwrite = TRUE))
  docOM(name)
}


cat("#' @name SWO-OMs",
    "\n#' @docType data",
    "\n#' @title Operating models",
    "\n#' @description North Atlantic Swordfish Operating Models",
    "\n#'  ",
    "\n#' `MOM_000` is the base case assessment. The rest of the OMs are from the ",
    "\n#' OM uncertainty grid.",
    "\n#' ",
    "\n#' All OMs are class `MOM` and contain 2 stocks (female and male) and 1 aggregated fleet",
    '\nNULL',
    "\n\n\n",
    sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))


purrr::map(1:nrow(OM_DF), importOM, nsim, proyears,
           OM_DF, SWOData)

usethis::use_data(OM_DF, overwrite = TRUE)

cat("\n#' @name OM_DF",
    "\n#' @docType data",
    "\n#' @title North Atlantic Swordfish OM Data-Frame",
    "\n#' @description Summary of the North Atlantic Swordfish OM Parameters",
    "\n#'  ",
    '\nNULL',
    "\n\n\n",
    sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))



# ---- Make OM Table -----

OM_desc <- read.csv(file.path(OMgrid.dir, 'OM_Description.csv'))
OM_desc$OM.objects <- NA

# add MOM objects
for (i in 1:nrow(OM_desc)) {
  cl <- OM_desc$Class[i]
  df <- OM_DF %>% dplyr::filter(Class==cl)
  OM_desc$OM.objects[i] <- paste(df$OM.object, collapse=', ')
}

OM_desc$Class <- factor(OM_desc$Class, levels=unique(OM_desc$Class), ordered = TRUE)



# ---- Add Additional OMs (modifications of those in OM_DF) ----
#
# ## R5. Increasing q
#
# df <- OM_DF %>% filter(Class == 'R4. Increase q')
#
# df$OM.object <- paste(df$OM.object, '(inc q in projections)')
# df$dir <- ''
# df$Class <- 'R5. Increasing q'
# df$OM.num <- ''
#
# OM_DF <- bind_rows(OM_DF, df)
#
#
# ## R6. Implementation Error
# df <- OM_DF %>% filter(Class == 'Reference')
#
# add_overages <- function(MOM, overage=1.1, OM_DF) {
#   #load(paste0('data/', MOM, '.rda'))
#   #obj <- get(MOM)
#   #obj@Imps$Female[[1]]@TACFrac <- c(1.10, 1.10)
#   #obj@Imps$Male[[1]]@TACFrac <- c(1.10, 1.10)
#   nm <- paste0(MOM, '_overage')
#  # assign(nm, obj)
#
#   df <- OM_DF %>% filter(OM.object==MOM)
#   df$OM.object <- nm
#   df$dir <- ''
#   df$Class <- 'R6. Implementation Error'
#   df$OM.num <- ''
#   OM_DF <- bind_rows(OM_DF, df)
#
#   #do.call("use_data", list(as.name(nm), overwrite = TRUE))
#   OM_DF
# }

# for (i in 1:nrow(df)) {
#   MOM <- df$OM.object[i]
#   OM_DF <- add_overages(MOM, overage=1.1, OM_DF)
# }

## R7. Climate Change - Recruitment

### Scenarios

# base_case <- rep(1, proyears)
# decreasing <- seq(from=1, to=0.8, length.out=proyears)
# increasing <- seq(from=1, to=1.2, length.out=proyears)
# more_variable <- cbind(decreasing, increasing)
#
# pro_Years <- 2021:(2021+proyears-1)
#
# df1 <- data.frame(Scenario='Base Case', Values=base_case, Year=pro_Years)
# df2 <- data.frame(Scenario='Decreasing Trend', Values=decreasing, Year=pro_Years)
# df3 <- data.frame(Scenario='Increasing Trend', Values=increasing, Year=pro_Years)
# df4 <- data.frame(Scenario='Increased Variability', Values=c(decreasing, increasing), Year=pro_Years)
#
# rec_df <- bind_rows(df1, df2, df3, df4)
# rec_df$Scenario <- factor(rec_df$Scenario, levels=unique(rec_df$Scenario), ordered = TRUE)
#
# p1 <- ggplot(rec_df, aes(x=Year, y=Values)) +
#   facet_wrap(~Scenario, nrow=2) +
#   geom_line() +
#   geom_hline(yintercept = 1, linetype=2, color='darkgray') +
#   theme_bw() +
#   labs(x='Projection Years', y='Mean Trend Recruitment Deviations')
#
# ggsave('img/R7_Recruitment_Scenarios.png', p1, width=6, height=6)

modify_recruit_devs <- function(MOM, rec_df, scenario='Decreasing Trend', trend=decreasing, OM_DF) {
  load(paste0('data/', MOM, '.rda'))
  obj <- get(MOM)

  nyears <- obj@Fleets$Female[[1]]@nyears
  p.ind <- nyears + obj@Stocks$Female@maxage + 1
  p.ind <- p.ind:(p.ind+proyears-1)

  if (!is.null(ncol(trend))) {
    trend2 <- matrix(1, nrow=nsim, ncol=proyears)
    for (i in 1:nsim) {


      trend2[i,] <- runif(proyears, trend[,1], trend[,2])
    }
  } else {
    trend2 <- t(replicate(nsim, trend))
  }


  obj@cpars$Female[[1]]$Perr_y[,p.ind] * trend2
  obj@cpars$Male[[1]]$Perr_y[,p.ind] * trend2

  nm <- paste(MOM, scenario, sep='_')
  assign(nm, obj)

  df <- OM_DF %>% filter(OM.object==MOM)
  df$OM.object <- nm
  df$dir <- ''
  df$Class <- 'R7. Climate Change - Recruitment'
  df$OM.num <- ''
  OM_DF <- bind_rows(OM_DF, df)

  do.call("use_data", list(as.name(nm), overwrite = TRUE))
  OM_DF
}
#
# df <- OM_DF %>% filter(Class == 'Reference')
#
# for (i in 1:nrow(df)) {
#   MOM <- df$OM.object[i]
#   OM_DF <- modify_recruit_devs(MOM, rec_df, scenario='Decreasing Trend', trend=decreasing, OM_DF)
#   OM_DF <- modify_recruit_devs(MOM, rec_df, scenario='Increasing Trend', trend=increasing, OM_DF)
#   OM_DF <- modify_recruit_devs(MOM, rec_df, scenario='Increased Variability', trend=more_variable, OM_DF)
#
# }

## R8. Size Limit
# df <- OM_DF %>% filter(Class == 'Reference')
#
# df$OM.object <- paste(df$OM.object, '(modified size limit in CMP)')
# df$dir <- ''
# df$Class <- 'R8. Size Limit'
# df$OM.num <- ''
#
# OM_DF <- bind_rows(OM_DF, df)


## R9. Alternative Management Cycles
# df <- OM_DF %>% filter(Class == 'Reference')
#
# df$OM.object <- paste(df$OM.object, '(modified in projections)')
# df$dir <- ''
# df$Class <- 'R9. Alternative Management Cycles'
# df$OM.num <- ''
#
# OM_DF <- bind_rows(OM_DF, df)
#

# ---- Update OM_DF to include Additional OMs ----

# usethis::use_data(OM_DF, overwrite = TRUE)

cat("\n#' @name OM_DF",
    "\n#' @docType data",
    "\n#' @title North Atlantic Swordfish OM Data-Frame",
    "\n#' @description Summary of the North Atlantic Swordfish OM Parameters",
    "\n#'  ",
    '\nNULL',
    "\n\n\n",
    sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))


# ---- Update OM table ----

OM_desc <- read.csv(file.path(OMgrid.dir, 'OM_Description.csv'))
OM_desc$OM.objects <- NA

# add MOM objects
for (i in 1:nrow(OM_desc)) {
  cl <- OM_desc$Class[i]
  df <- OM_DF %>% dplyr::filter(Class==cl)
  OM_desc$OM.objects[i] <- paste(df$OM.object, collapse=', ')
}

OM_desc$Class <- factor(OM_desc$Class, levels=unique(OM_desc$Class), ordered = TRUE)


usethis::use_data(OM_desc, overwrite = TRUE)


# ---- Catches for Initial Projection Years ----

catchdf <- data.frame(Year=SWOData@Year, Catch=SWOData@Cat[1,])
catchdf <- bind_rows(catchdf, data.frame(Year=2021, Catch=9729))
mean_catch <- catchdf %>% tail(10) %>% summarise(mean=round(mean(Catch),0))

Catchdf <- data.frame(Year=c(2021, 2022, 2023),
                 Catch=c(9729, mean_catch$mean, mean_catch$mean),
                 Details=c('Reported Catch',
                           'Assumed Catch',
                           'Assumed Catch'))

usethis::use_data(Catchdf, overwrite = TRUE)

# ---- Update SWO_Data with AddInd -----
load('data/MOM_000.rda')

SWOData <- MOM_000@cpars[[1]][[1]]$Data
SWOData@MPrec <- Catchdf$Catch[3]

usethis::use_data(SWOData, overwrite = TRUE)

# ----- Index Dataframe ----

ind_names <- dimnames(SWOData@AddInd[1,,])[[1]]
l <- strsplit(ind_names, '_')
ind_names2 <- sapply(l,"[[",1)

Index_Code <- data.frame(Code=c('Comb', ind_names2),
                         Index=c('Ind', ind_names),
                         Description=c('Combined Index',
                                       'EU-Spain Longline (LL)',
                                       'Canada LL',
                                       'Japan LL',
                                       'Chinese Taipei LL',
                                       'Morocco LL',
                                       'USA LL',
                                       'EU-Portugal LL'))


usethis::use_data(Index_Code, overwrite = TRUE)


Initial_MP_Yr <- 2024
usethis::use_data(Initial_MP_Yr, overwrite = TRUE)


# ----- PM Table -----

PM_desc <- read.csv(file.path('build_package', 'PM_Description.csv'))

colnames(PM_desc)[4] <- trimws(gsub('\\.', ' ', colnames(PM_desc)[4]))

usethis::use_data(PM_desc, overwrite = TRUE)


# ----- Tuning Codes ----

TuneTargets <- read.csv('build_package/Tuning_Target_Codes.csv')

usethis::use_data(TuneTargets, overwrite = TRUE)

