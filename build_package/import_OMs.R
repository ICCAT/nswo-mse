
# ---- Install latest version of r4ss ----
# devtools::install_github("r4ss/r4ss", build_vignettes = TRUE, force=TRUE)

library(r4ss); library(dplyr);
library(OMtool); library(SWOMSE);
library(usethis)

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
SWOData <- DLMtool::XL2Data("inst/SWO_Data")
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
OM_base_case <- SWO_SS2OM(OMbase.dir, nsim=nsim)
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
#     message("DLMtool operating model is an annual model. Since the SS model is seasonal, we need to aggregate over seasons.\n")
#   } else {
#     nseas <- replist$nseasons
#     if(nseas > 1) {
#       message("DLMtool operating model is an annual model. Since the SS model is seasonal, we need to aggregate over seasons.\n")
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
