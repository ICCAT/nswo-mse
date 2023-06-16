library(SWOMSE)

Tune_dir='Tuning_Objects'

TuneTargets

fls <- list.files(Tune_dir, pattern='.tune')

MPs <- strsplit(fls, '.tune') %>% unlist()

for (i in seq_along(MPs)) {
  Document_MP(MP_name=MPs[i], plot=TRUE)
}





















