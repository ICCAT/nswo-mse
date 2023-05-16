

PM_desc <- read.csv(file.path('dev', 'PM_Description.csv'))

colnames(PM_desc)[4] <- trimws(gsub('\\.', ' ', colnames(PM_desc)[4]))

usethis::use_data(PM_desc, overwrite = TRUE)


