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
nsim <- 48

OM.root <- 'G:/My Drive/1_Projects/North_Atlantic_Swordfish/OMs/grid_2021'
OMgrid.dir <- file.path(OM.root, "grid_Apr2021")
OMgrid.dirs <- list.dirs(OMgrid.dir, recursive = FALSE)

ord <- lapply(strsplit(OMgrid.dirs, 'iter'), '[[', 2) %>% as.numeric() %>% order()
OMgrid.dirs <- OMgrid.dirs[ord]

OMbase.dir <-  file.path(OM.root, "Michael_March2020/NSWO_MSE_SS3_Base_v2")

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

# Compare 2 OMs

OM1a <- SWOMSE::SWO_SS2OM(SSdir=OMgrid.dirs[1], nsim=nsim)
OM1b <- MSEtool::SS2OM(OMgrid.dirs[1], nsim=nsim)

# compare
MSEtool::plot_SS2OM(OM1a, OMgrid.dirs[1], filename='old', dir=getwd())
MSEtool::plot_SS2OM(OM1b, OMgrid.dirs[1], filename='new', dir=getwd())





# - Base Case -
OM_base_case <- SWOMSE::SWO_SS2OM(SSdir=OMbase.dir, nsim=nsim)
OM_1 <- MSEtool::SS2OM(OMbase.dir, nsim=nsim)






OM_1@Fdisc <- c(0.88, 0.88)

OM_1@cpars$V



Hist <- Simulate(OM_1)

replist <- MSEtool:::SS_import(OMbase.dir)


year_lab <- "Year"
OM_years <- replist$startyr:replist$endyr
C_SS <- cbind(replist$timeseries[, 1:4], rowSums(replist$timeseries[, startsWith(names(replist$timeseries), "dead(B):")], na.rm = TRUE))
C_SS <- aggregate(C_SS[, 5], list(Year = C_SS$Yr), sum)
C_SS <- C_SS[C_SS$Year %in% OM_years, 2]

C_OM <- apply(Hist@AtAge$Removals, c(1, 3), sum)

ylim_cat <- c(0, 1.1 * max(C_SS, C_OM))
matplot(OM_years, t(C_OM), xlab = year_lab, ylab = "Removals", ylim = ylim_cat, pch = 1, col = "black", typ = "o")
lines(OM_years, C_SS, col = "red", lwd = 3)
abline(h = 0, col = "grey")

C_OM_1 <- apply(Hist@AtAge$Removals, c(1, 3), sum)

C_OM_2 <- apply(Hist@AtAge$Landings, c(1, 3), sum)



C_OM_1[1,]
C_OM_2[1,]














