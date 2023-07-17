library(SWOMSE)

MSE.dir <- 'MSE_Objects'
Hist.dir <- 'Hist_Objects'

fls <- list.files('CMPs')
for (fl in fls)
  source(file.path('CMPs', fl))

MPs <- avail('MP', 'SWOMSE')

# MPs tuned to PGK_6_10 = 60%
TuneTargets
MPs_a <- MPs[grepl('_a', MPs)]
MPs_b <- MPs[grepl('_b', MPs)]
MPs_c <- MPs[grepl('_c', MPs)]

MPs_all <- c(MPs_a, MPs_b, MPs_c)

# ---- Constant Catch MPs ----
CC15000 <- function(x, Data, Interval=3, TAC=15000, ...) {

  Rec <- new('Rec')
  Rec@TAC <- TAC
  Rec
}
class(CC15000) <- 'MP'

CC10000 <- CC15000
formals(CC10000)$TAC <- 10000
class(CC10000) <- 'MP'


MPs_all <- c('NFref', 'CC15000', 'CC10000',  MPs_all)


Ref_OMs <- OM_DF %>% filter(Class=='Reference')

for (i in 1:nrow(Ref_OMs)) {
  message(i, '/', nrow(Ref_OMs))
  OM_object <- Ref_OMs$OM.object[i]
  multiHist <- readRDS(file.path(Hist.dir, paste0(OM_object, '.hist')))
  MMSE <- ProjectMOM(multiHist, MPs=MPs_all, silent=FALSE, dropHist=FALSE, checkMPs = FALSE)
  saveRDS(MMSE, file.path(MSE.dir, paste0(OM_object, '.mse')))
}


