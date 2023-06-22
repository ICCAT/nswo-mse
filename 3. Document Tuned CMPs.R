library(SWOMSE)

Tune_dir='Tuning_Objects'

CMP_files <- list.files('CMPs')
for (fl in CMP_files) source(file.path('CMPs', fl))

TuneTargets

fls <- list.files(Tune_dir, pattern='.tune')

MPs <- strsplit(fls, '.tune') %>% unlist()


get_location <- function(MP_name) {
  attrs <- attributes(body(MP_name))
  attrs$srcfile$filename
}

get_MP_locations <- function(MPs) {
  R_files <- list.files('R')
  for (fl in R_files)
    source(file.path('R', fl))
  sapply(1:length(MPs), function(x)
    get_location(MPs[x])
  )
}


MPs
MP_files <- get_MP_locations(MPs)



for (i in seq_along(MPs)) {
  Document_MP(MP_name=MPs[i], MP_file=MP_files[i], plot=TRUE)
}

#
# multiHist <- readRDS('Hist_Objects/MOM_001.hist')
# testMSE <- ProjectMOM(multiHist, MPs=c('SPS_a', "SPFox_a"))
#
#
# PGK_short(testMSE)
# PGK_med(testMSE)
# PGK_long(testMSE)
# TAC1(testMSE)
# AvTAC_short(testMSE)
# AvTAC_med(testMSE)
# AvTAC_long(testMSE)
#
# TS_plot(testMSE, relY=FALSE)
#

















