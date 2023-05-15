
OM.root <- 'G:/My Drive/1_Projects/North_Atlantic_Swordfish/OMs'
OMgrid.dir <- file.path(OM.root,'grid_2022')
PM_desc <- read.csv(file.path(OMgrid.dir, 'PM_Description.csv'))


colnames(PM_desc)[4] <- trimws(gsub('\\.', ' ', colnames(PM_desc)[4]))

usethis::use_data(PM_desc, overwrite = TRUE)


