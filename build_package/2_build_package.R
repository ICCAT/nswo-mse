# Import OMs and Data and add to SWOMSE package


nsim <- 48 # number of simulations per OM
proyears <- 30 # number of projection years

OM.root <- 'G:/My Drive/1_Projects/North_Atlantic_Swordfish/OMs'
OMgrid.dir <- file.path(OM.root,'grid_2022')
OMgrid.dirs <- list.dirs(OMgrid.dir, recursive = FALSE)

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
get_OM_details <- function(dir) {

  text <- dir %>% basename() %>% strsplit(.,'_')
  text <- text[[1]]
  OM.num <- text[1]

  if (grepl('base_case', dir)) {

    df <- data.frame(M=0.2,
                     sigmaR=0.2,
                     steepness=0.88,
                     cpuelambda=1,
                     llq=1,
                     env=7)
  } else {
    text <- text[-1]
    text[6] <- gsub('-5', 0, text[6])
    text[6] <- gsub('5', 1, text[6])

    df <- data.frame(text) %>% tidyr::separate(text, into = c("Parameter", "Value"),
                                               sep = "(?<=[A-Za-z])(?=[0-9])")
    df <- df %>% tidyr::pivot_wider(., names_from = Parameter, values_from = Value)
  }

  df$OM.num <- OM.num
  df$dir <- dir %>% basename()

  df
}

OM_DF <- lapply(OMgrid.dirs, get_OM_details) %>% do.call('rbind',.)
OM_DF$env[OM_DF$env=='7'] <- '1'
OM_DF <- OM_DF %>% mutate_at(1:6, as.numeric)

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


importOM <- function(i, OMgrid.dirs, nsim, proyears, OM_DF, SWOData) {
  message(i)
  SS.dir <- OMgrid.dirs[i]

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
  Data@AddIunits <- rep(1,n.fleet+n.survey.fleet)

  fleet.ind.units <- c(1,0,0,0,0,1,0,0,1,0,0) # from SWOv5.dat
  # add fleet indices
  for (fl in 1:(n.fleet+n.survey.fleet)) {
    if (fl <= n.fleet) {
      ind <- MOM@cpars[[1]][[fl]]$Data@VInd[1,]
      cv_ind <- MOM@cpars[[1]][[fl]]$Data@CV_VInd[1,]
      units <- fleet.ind.units[fl]
    } else {
      fl2 <- fl-n.fleet
      ind <- MOM@cpars[[1]][[1]]$Data@AddInd[1,fl2,]
      cv_ind <- MOM@cpars[[1]][[1]]$Data@CV_AddInd[1,fl2,]
      units <- MOM@cpars[[1]][[1]]$Data@AddIunits[fl2]
    }
    if (length(cv_ind)<nyears)
      cv_ind <- rep(cv_ind[1], nyears)
    Data@AddInd[1,fl, ] <- ind
    Data@CV_AddInd[1,fl,] <- cv_ind
    Data@AddIunits[fl] <- units

  }
  # name the dimensions
  dimnames(Data@AddInd)[[1]] <- 1
  dimnames(Data@AddInd)[[2]] <- Fleet_DF$Code
  dimnames(Data@AddInd)[[3]] <- SWOData@Year

  # drop the combined index (already in Data@Ind)
  Data@AddInd <- Data@AddInd[,-19,, drop=FALSE]
  Data@CV_AddInd <- Data@CV_AddInd[,-19,, drop=FALSE]
  Data@AddIunits <- Data@AddIunits[-19]

  # drop empty indices
  max.vals <- suppressWarnings(apply(Data@AddInd[1,,], 1, max, na.rm=TRUE))
  ind <- which(!is.finite(max.vals))
  Data@AddInd <- Data@AddInd[,-ind,, drop=FALSE]
  Data@CV_AddInd <- Data@CV_AddInd[,-ind,, drop=FALSE]
  Data@AddIunits <- Data@AddIunits[-ind]

  # aggregate all fleets into one
  MOM <- MOM %>% MSEtool::MOM_agg_fleets(.)

  MOM@proyears <- proyears
  # add data to female and male stocks
  for (p in 1:n.stock) {
    MOM@cpars[[p]][[1]]$Data <- Data
    MOM@cpars[[p]][[1]]$AddIbeta <- matrix(1, nrow=nsim, ncol=dim(Data@AddInd)[2])
    MOM@cpars[[p]][[1]]$I_beta <- rep(1, nsim)
    MOM@cpars[[p]][[1]]$Cobs_y <- matrix(1, nrow=nsim, ncol=nyears+MOM@proyears)
  }

  i_num <- as.character(i)
  if (nchar(i_num)==1) i_num <- paste0('00', i_num)
  if (nchar(i_num)==2) i_num <- paste0('0', i_num)



  if (grepl('base_case', SS.dir)) {
    name <- 'SWO 2022 Base Case'
  } else {
    vals <- OM_DF %>% dplyr::filter(OM.num==i_num)
    vals$OM.num <- vals$dir <- NULL
    name <- paste(names(vals), vals, collapse = ' ', sep=':')

  }

  MOM@Name <- name
  n.fleet <- length(MOM@cpars[[1]])

  # map real data across stocks - data is not sex-specific
  MOM@cpars[[1]][[1]]$Real.Data.Map <- matrix(1, nrow=n.fleet, ncol=n.stock)

  OM.num <- i-1  # base case is 0
  OM.num <- as.character(OM.num)
  if (nchar(OM.num)==1) OM.num <- paste0('00',OM.num)
  if (nchar(OM.num)==2) OM.num <- paste0('0',OM.num)
  name <- paste0('MOM_', OM.num)
  assign(name, MOM)

  do.call("use_data", list(as.name(name), overwrite = TRUE))
  docOM(name)

}


cat("#' @name SWO-OMs",
    "\n#' @docType data",
    "\n#' @title Operating models",
    "\n#' @description North Atlantic Swordfish Operating Models",
    "\n#'  ",
    "\n#' `MOM_1` is the base case assessment. The rest of the OMs are from the ",
    "\n#' OM uncertainty grid.",
    "\n#' ",
    "\n#' All OMs are class `MOM` and contain 2 stocks (female and male) and 1 aggregated fleet",
    '\nNULL',
    "\n\n\n",
    sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))


purrr::map(seq_along(OMgrid.dirs), importOM, OMgrid.dirs, nsim, proyears,
           OM_DF, SWOData)

# prob <- NULL
# for (i in 1:length(OMgrid.dirs)) {
#   tt <- try(importOM(i, OMgrid.dirs, nsim))
#   if (class(try)=='try-error') {
#     prob <- c(prob, i)
#   }
# }
#
# prob


# ---- Update SWO_Data with AddInd -----
load('data/MOM_000.rda')

SWOData <- MOM_000@cpars[[1]][[1]]$Data

usethis::use_data(SWOData, overwrite = TRUE)

