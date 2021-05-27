# Import OMs and Data and add to SWOMSE package

# ---- Install latest version of r4ss ----
# devtools::install_github("r4ss/r4ss", build_vignettes = TRUE, force=TRUE)

library(r4ss); library(dplyr); library(tidyr)
library(MSEtool); library(usethis); library(purrr)

# ---- Add SWO_Data ----
SWOData <- MSEtool::XL2Data("inst/SWO_Data")
usethis::use_data(SWOData, overwrite = TRUE)

# - Documentation -
RoxygenFile <- 'Roxygen.r'
if (file.exists(file.path('R/', RoxygenFile)))
  file.remove(file.path('R/', RoxygenFile)) # delete
file.create(file.path('R/', RoxygenFile)) # make empty file

cat("# This file is automatically built by build_package/build_package.r\n",
    "# Don't edit by hand!\n",
    "# \n\n", sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))

cat("#' @name SWOData",
    "\n#' @docType data",
    "\n#' @title North Atlantic Swordfish Data",
    "\n#' @description North Atlantic Swordfish Data",
    "\n#'  ",
    '\nNULL',
    "\n\n\n",
    sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))


# ---- Import OMs ----

nsim <- 48 # number of simulations per OM

OM.root <- 'G:/My Drive/1_Projects/North_Atlantic_Swordfish/OMs/grid_2021'
OMgrid.dir <- file.path(OM.root, "grid_May2021_shifted")
OMgrid.dirs <- list.dirs(OMgrid.dir, recursive = FALSE)

ord <- lapply(strsplit(OMgrid.dirs, 'iter'), '[[', 2) %>% as.numeric() %>% order()
OMgrid.dirs <- OMgrid.dirs[ord]

# Based on the 2017 assessment - does not include discard mortality and retention curve
OMbase.dir <-  file.path(OM.root, "Michael_March2020/NSWO_MSE_SS3_Base_v2")

importOM <- function(i, OMgrid.dirs, nsim) {
  message(i)
  SS.dir <- OMgrid.dirs[i]
  OM <- MSEtool::SS2OM(SS.dir, nsim=nsim)
  OM@cpars$Data <- SWOData

  # M
  txt <- strsplit(SS.dir, '-M')[[1]][2]
  M <- strsplit(txt, '_')[[1]][1] %>% as.numeric()

  # sigmaR
  txt <- strsplit(SS.dir, '_sigmaR')[[1]][2]
  sigmaR <- strsplit(txt, '_')[[1]][1] %>% as.numeric()

  # Steep
  txt <- strsplit(SS.dir, 'steepness')[[1]][2]
  h <- strsplit(txt, '_cpue')[[1]][1] %>% as.numeric()

  # CPUE lambda
  txt <- strsplit(SS.dir, 'cpuelambda')[[1]][2]
  lambda <- strsplit(txt, '_llq')[[1]][1] %>% as.numeric()

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
                   'CPUE', lambda,
                   'Q', q,
                   'ENV', env,
                   sep='-')

  nyears <- OM@nyears
  proyears <- OM@proyears
  Len_age <- OM@cpars$Len_age
  maxage <- OM@maxage

  name <- paste0('OM_', i)
  assign(name, OM)

  do.call("use_data", list(as.name(name), overwrite = TRUE))
  docOM(name)

}

docOM <- function(OMname) {
  cat("#' @rdname SWO-OMs \n", "'", OMname, "'\n",
      sep="", append=TRUE,
      file=file.path('R/', RoxygenFile)
  )
}

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



prob <- NULL
for (i in 1:length(OMgrid.dirs)) {
# for (i in 1:2) {
  tt <- try(importOM(i, OMgrid.dirs, nsim))
  if (class(try)=='try-error') {
    prob <- c(prob, i)
  }
}

prob


# purrr::map(seq_along(OMgrid.dirs), importOM, OMgrid.dirs, nsim)

# ---- Create OM Data-Frame ----

writeOMDF <- function(i, OMgrid.dirs, RepList) {
  message(i)
  replist <- RepList[[i]]
  SS.dir <- OMgrid.dirs[i]

  if (i =='base_case') {
    data <- r4ss::SS_readdat(file.path(SS.dir, 'data.ss_new'), version='3.24',
                             verbose = FALSE)
  } else {
    data <- r4ss::SS_readdat(file.path(SS.dir, 'SWO.dat'), version='3.30',
                             verbose = FALSE)
  }

  # M
  txt <- strsplit(SS.dir, '-M')[[1]][2]
  M <- strsplit(txt, '_')[[1]][1] %>% as.numeric()

  # sigmaR
  txt <- strsplit(SS.dir, '_sigmaR')[[1]][2]
  sigmaR <- strsplit(txt, '_')[[1]][1] %>% as.numeric()

  # Steep
  txt <- strsplit(SS.dir, 'steepness')[[1]][2]
  h <- strsplit(txt, '_cpue')[[1]][1] %>% as.numeric()

  # cpue cv
  cpue_cv <- data$CPUE %>% filter(year>0, year<= replist$endyr) %>%
    group_by(index, year) %>% summarise(se_log=unique(se_log), .groups = 'keep') %>%
    dplyr::ungroup() %>%
    dplyr::select(se_log) %>% dplyr::distinct()

  if (nrow(cpue_cv)>1) {
    rng <- range(cpue_cv$se_log)
    cpue_cv <- cpue_cv %>% dplyr::filter(se_log %in% rng) %>% dplyr::distinct()
  }

  # ess
  L_ESS <- data$lencomp$Nsamp %>% unique()

  # CPUE lambda
  txt <- strsplit(SS.dir, 'cpuelambda')[[1]][2]
  lambda <- strsplit(txt, '_llq')[[1]][1] %>% as.numeric()

  # Q
  txt <- strsplit(SS.dir, 'llq')[[1]][2]
  llq <- strsplit(txt, '_env')[[1]][1] %>% as.numeric()

  # ENV
  txt <- strsplit(SS.dir, 'env')[[1]][2]
  env <- strsplit(txt, '_')[[1]][1] %>% as.numeric()
  if (env <1) {
    env <- 0
  } else {
    env <- 1
  }

  # converge
  log_det_hessian <- replist$log_det_hessian
  if (is.null(log_det_hessian)) log_det_hessian <- NA
  data.frame(i=i, M=M, sigmaR=sigmaR, h=h, cpue_cv=cpue_cv,lambda=lambda,
             L_ESS=L_ESS, llq=llq, env=env, dir=basename(SS.dir),
             log_det_hessian=log_det_hessian)
}

RepList <- readRDS(file.path(OM.root, '/OM_objects/RepList.rda'))

OM_DF <- purrr::map_df(seq_along(OMgrid.dirs), writeOMDF, OMgrid.dirs, RepList)
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

# ---- Save Fleet Information ----
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

cat("\n#' @name Fleet_DF",
    "\n#' @docType data",
    "\n#' @title Fleet Information",
    "\n#' @description A dataframe of information on fleet names",
    "\n#'  ",
    '\n "Fleet_DF"',
    "\n\n\n",
    sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))

