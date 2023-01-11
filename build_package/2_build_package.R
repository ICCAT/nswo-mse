# Import OMs and Data and add to SWOMSE package


nsim <- 48 # number of simulations per OM
proyears <- 30 # number of projection years

OM.root <- 'G:/My Drive/1_Projects/North_Atlantic_Swordfish/OMs'
OMgrid.dir <- file.path(OM.root,'grid_2022')
OMgrid.dirs <- list.dirs(OMgrid.dir, recursive = TRUE)

SSData <- r4ss::SS_readdat(file.path(OMgrid.dir, '000_base_case/SWOv5.dat'))

# ---- Install latest version of r4ss ----
# devtools::install_github("r4ss/r4ss", build_vignettes = TRUE, force=TRUE)

library(r4ss); library(dplyr); library(tidyr)
library(MSEtool); library(usethis); library(purrr)

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
  MOM <- SS2MOM(SS.dir, nsim=nsim, proyears = proyears, interval = 1)

  # get fleet-specific indices
  nyears <- MOM@Fleets[[1]][[1]]@nyears
  Data <- SWOData
  n.stock <- length(MOM@Stocks)
  n.fleet <- length(MOM@cpars[[1]])
  n.survey.fleet <- nrow(MOM@cpars[[1]][[1]]$Data@AddInd[1,,])

  Data@AddInd <- array(NA, dim=c(1, n.fleet+n.survey.fleet, nyears))
  Data@CV_AddInd <- Data@AddInd
  n_age <- MOM@Stocks[[1]]@maxage+1
  Data@AddIndV <-   array(NA, dim=c(1, n.fleet+n.survey.fleet, n_age))
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

  # drop the combined index (already in Data@Ind)
  Data@AddInd <- Data@AddInd[,-19,, drop=FALSE]
  Data@CV_AddInd <- Data@CV_AddInd[,-19,, drop=FALSE]
  Data@AddIunits <- Data@AddIunits[-19]
  Data@AddIndV <- Data@AddIndV[,-19,, drop=FALSE]

  # drop empty indices
  max.vals <- suppressWarnings(apply(Data@AddInd[1,,], 1, max, na.rm=TRUE))
  ind <- which(!is.finite(max.vals))
  Data@AddInd <- Data@AddInd[,-ind,, drop=FALSE]
  Data@CV_AddInd <- Data@CV_AddInd[,-ind,, drop=FALSE]
  Data@AddIunits <- Data@AddIunits[-ind]
  Data@AddIndV <- Data@AddIndV[,-ind,, drop=FALSE]

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

  MOM@cpars$Female$CAN_3$retL[1,,nyears] %>% plot()
  MOM@cpars$Female$CAN_3$SLarray[1,,nyears] %>% lines()

  MOM@cpars$Female$CAN_3$V[1,,nyears] %>% plot()
  MOM@cpars$Female$CAN_3$retA[1,,nyears] %>% lines()

  # aggregate all fleets into one
  MOM <- MOM %>% MSEtool::MOM_agg_fleets(.)

  MOM@proyears <- proyears
  # add data to female and male stocks
  for (p in 1:n.stock) {
    MOM@cpars[[p]][[1]]$Data <- Data
    MOM@cpars[[p]][[1]]$AddIbeta <- matrix(1, nrow=nsim, ncol=dim(Data@AddInd)[2])
    MOM@cpars[[p]][[1]]$I_beta <- rep(1, nsim)
    MOM@cpars[[p]][[1]]$Cobs_y <- matrix(1, nrow=nsim, ncol=nyears+MOM@proyears)

    # Add CAL bins to cpars
    MOM@cpars[[p]][[1]]$CAL_bins <-  Data@CAL_bins
    MOM@cpars[[p]][[1]]$CAL_binsmid <- Data@CAL_mids
  }

  # Drop Obs and Imp for other fleets
  for (p in 1:n.stock) {
    drop.fleet <- 2:(n.fleet+n.survey.fleet)
    MOM@Obs[[p]] <- MOM@Obs[[p]][-drop.fleet]
    MOM@Imps[[p]] <- MOM@Imps[[p]][-drop.fleet]
  }

  # assign name
  vals <- OM_DF[i,]
  vals$OM.num <- vals$dir <- vals$OM.object <- NULL
  vals[7] <- as.character(vals[7][1,1])
  name <- paste(names(vals), vals, collapse = ' ', sep=':')

  MOM@Name <- name
  n.fleet <- length(MOM@cpars[[1]])

  # map real data across stocks - data is not sex-specific
  MOM@cpars[[1]][[1]]$Real.Data.Map <- matrix(1, nrow=n.fleet, ncol=n.stock)

  name <- OM_DF$OM.object[i]
  assign(name, MOM)

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


# ---- Update SWO_Data with AddInd -----
load('data/MOM_000.rda')

SWOData <- MOM_000@cpars[[1]][[1]]$Data

usethis::use_data(SWOData, overwrite = TRUE)

