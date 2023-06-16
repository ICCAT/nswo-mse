library(SWOMSE)

MSE.dir <- 'MSE_Objects'
Hist.dir <- 'Hist_Objects'

MPs <- avail('MP', 'SWOMSE')

# MPs tuned to PGK_6_10 = 60%
MPs <- MPs[grepl('_a', MPs)]

Ref_OMs <- OM_DF %>% filter(Class=='Reference')

for (i in 1:nrow(Ref_OMs)) {
  message(i, '/', nrow(Ref_OMs))
  OM_object <- Ref_OMs$OM.object[i]
  multiHist <- readRDS(file.path(Hist.dir, paste0(OM_object, '.hist')))
  MMSE <- ProjectMOM(multiHist, MPs=MPs, silent=TRUE)
  saveRDS(MMSE, file.path(MSE.dir, paste0(OM_object, '.mse')))
}

