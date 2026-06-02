library(MSEtool)

OM.root <- 'G:/My Drive/1_Projects/North_Atlantic_Swordfish/OMs'
OM.object <- file.path(OM.root, 'OM_objects')
OMgrid.dir <- file.path(OM.root, "2024_OMs")
OMgrid.dirs <- list.dirs(OMgrid.dir, recursive = TRUE)


# Base Case
SSDir <- OMgrid.dirs[7]
OM <- ImportSS(SSDir)


