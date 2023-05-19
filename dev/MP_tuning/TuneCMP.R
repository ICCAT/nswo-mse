library(SWOMSE)


## Run Historical Spool Up and Save to Disk
Tuning_OMs <- OM_DF %>% filter(Class=='Reference')

rerun <- FALSE
if (rerun) {
  HistList <- list()
  for (i in 1:nrow(Tuning_OMs)) {
    message(i, '/', nrow(Tuning_OMs))
    MOM <- get(Tuning_OMs$OM.object[i])
    Hist <- SimulateMOM(MOM, silent=TRUE)
    nm <- paste0(Tuning_OMs$OM.object[i], '.mhist')
    saveRDS(Hist, file.path('dev', nm))
  }
}


# Tuning Targets
TuneTarget <- read.csv('dev/MP_tuning/Tuning_Target_Codes.csv')
TuneTarget

# Directory where the multiHist objects are saved
histdir <- 'dev'
# Directory where tuning objects are saved
tune.dir <- "dev/MP_tuning/Tuning_Objects"

MPnames <- c('IR1')


# -------------- Code a: PGK_med = 0.6 -----------------------------------------

tt <- 1
tuneinfo <- TuneTarget[tt,]
tuneMetric <- tuneinfo$Metric
tunetarg <- tuneinfo$Target
tunecode <- tuneinfo$Code

for (MPname in MPnames) {
setup()
run <- try(TuneCMP(MOM_objects=Tuning_OMs$OM.object,
                   histdir=histdir,
                   MPname=MPname,
                   tuneMetric=tuneMetric,
                   tunetarg=tunetarg,
                   step=0.8,
                   maxit=10,
                   tol=0.001,
                   rnd=3,
                   silent=TRUE,
                   parallel=TRUE,
                   savedir=tune.dir,
                   start_vals = c(0.7, 0.8, 0.9)))


}



# -------------- Code a: PGK_long = 0.6 -----------------------------------------

tt <- 2
tuneinfo <- TuneTarget[tt,]
tuneMetric <- tuneinfo$Metric
tunetarg <- tuneinfo$Target
tunecode <- tuneinfo$Code

for (MPname in MPnames) {

  setup()
  run <- try(TuneCMP(MOM_objects=Tuning_OMs$OM.object,
                     histdir=histdir,
                     MPname=MPname,
                     tuneMetric=tuneMetric,
                     tunetarg=tunetarg,
                     step=0.8,
                     maxit=10,
                     tol=0.001,
                     rnd=3,
                     silent=TRUE,
                     parallel=TRUE,
                     savedir=tune.dir,
                     start_vals=c(1, 1.005, 1.007)))

}

# -------------- Code a: PGK_30 = 0.6 -----------------------------------------

tt <- 3
tuneinfo <- TuneTarget[tt,]
tuneMetric <- tuneinfo$Metric
tunetarg <- tuneinfo$Target
tunecode <- tuneinfo$Code

for (MPname in MPnames) {

  setup()
  run <- try(TuneCMP(MOM_objects=Tuning_OMs$OM.object,
                     histdir=histdir,
                     MPname=MPname,
                     tuneMetric=tuneMetric,
                     tunetarg=tunetarg,
                     step=0.8,
                     maxit=10,
                     tol=0.001,
                     rnd=3,
                     silent=TRUE,
                     parallel=TRUE,
                     savedir=tune.dir,
                     start_vals=c(0.95, 0.96, 0.97)))

}



### ---- After Tuning ----
## Write CMP Functions to Package ----
library(SWOMSE)

tune.dir <- 'dev/MP_tuning/Tuning_Objects'

r.files <- list.files('R')

document_MP('CE', tune.dir)
document_MP('IR2', tune.dir)
document_MP('SP1', tune.dir)
document_MP('SP2', tune.dir)



