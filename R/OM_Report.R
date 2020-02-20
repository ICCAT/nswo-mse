root <- '../../../Google Drive/Projects/Projects_2020/ICCAT_Swordfish/OMs/SS/2018_GRID'

OMdir <- file.path(root, "base_case")

# devtools::install_github("r4ss/r4ss", build_vignettes = TRUE, force=TRUE)
library(r4ss)
vignette('r4ss-intro-vignette', package='r4ss')

replist <- r4ss::SS_output(OMdir)


SS_plots(replist)






