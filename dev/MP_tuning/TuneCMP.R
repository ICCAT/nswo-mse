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
Tuning_OMs <- Tuning_OMs[1:6,]
Tuning_OMs$OM.object <- paste0('MOM_', c(133, 134, 135, 139, 140, 141))

tt <- 1
tuneinfo <- TuneTarget[tt,]
tuneMetric <- tuneinfo$Metric
tunetarg <- tuneinfo$Target
tunecode <- tuneinfo$Code

## Tune
parallel <- TRUE
MOM_objects <- Tuning_OMs$OM.object

MPname <- 'IR1'

MP <- try(get(MPname), silent=TRUE)

if (class(MP)=='try-error')
  stop(MPname, ' not found. Have you sourced ', MPname, '.R?')

n.OM <- length(MOM_objects)

if (parallel)
  if(!snowfall::sfIsRunning())
    setup()

MSEtool:::message('\nTuning', MPname)
MSEtool:::message_info('Tuning over', n.OM, 'OM(s):', paste0(MOM_objects, collapse=', '))
MSEtool:::message_info('Tuning', tuneMetric, 'to target value:', tunetarg)

if (is.null(histdir))
  stop('Must specify location of the multiHist objects corresponding to `MOM_objects`')

histFiles <- list.files(histdir, pattern='.mhist')
if (any(!MOM_objects %in% tools::file_path_sans_ext(histFiles))) {
  stop("'.mhist' files not found for all `MOM_objects` in directory: ", file.path(getwd(), histdir))
}

tunefun <- get(tuneMetric)
tunefunc <- function(MMSE) {
  tt <- tunefun(MMSE)
  tt@Mean
}

# ---- Tuning MPs ----

MP1 <- function(x,Data,tunepar, ...) MP(x,Data,tunepar=par1, ...)
MP2 <- function(x,Data,tunepar, ...) MP(x,Data,tunepar=par2, ...)
MP3 <- function(x,Data,tunepar, ...) MP(x,Data,tunepar=par3, ...)
MP4 <- function(x,Data,tunepar, ...) MP(x,Data,tunepar=par4, ...)
MP5 <- function(x,Data,tunepar, ...) MP(x,Data,tunepar=par5, ...)
MP6 <- function(x,Data,tunepar, ...) MP(x,Data,tunepar=par6, ...)
class(MP1) <- class(MP2) <- class(MP3) <- class(MP4) <- class(MP5) <-
  class(MP6) <- "MP"

assign("MP1", MP1, envir=globalenv())
assign("MP2", MP2, envir=globalenv())
assign("MP3", MP3, envir=globalenv())
assign("MP4", MP4, envir=globalenv())
assign("MP5", MP5, envir=globalenv())
assign("MP6", MP6, envir=globalenv())


# --- export MPs to cluster ----
if (parallel) {
  MSEtool:::message_info('Exporting MPs to cluster')
  snowfall::sfExport(list=c('MP', 'MP1', 'MP2', 'MP3', 'MP4', 'MP5', 'MP6', 'MOM_objects', 'histdir'))
  sfLibrary("SWOMSE", character.only = TRUE, verbose=FALSE)
}


runTuning_6 <- function(om, MOM_objects) {
  multiHist <- readRDS(file.path(histdir, paste0(MOM_objects[om], '.mhist')))
  ProjectMOM(multiHist, MPs=c("MP1","MP2","MP3", 'MP4', 'MP5', 'MP6'),
           silent=TRUE,parallel = TRUE, checkMPs=FALSE)

}


# ---- Initial Search - wide range of parameters ----
if (is.null(test_vals))
  test_vals <- seq(0.5, 1.2, length.out=6)

assign("par1", test_vals[1], envir=globalenv())
assign("par2", test_vals[2], envir=globalenv())
assign("par3", test_vals[3], envir=globalenv())
assign("par4", test_vals[4], envir=globalenv())
assign("par5", test_vals[5], envir=globalenv())
assign("par6", test_vals[6], envir=globalenv())


if (parallel) {
  snowfall::sfExport(list=c('par1', 'par2', 'par3', 'par4', 'par5', 'par6'))
}

st <- Sys.time()
if (parallel) {
  MMSEList <- sfLapply(1:n.OM, runTuning_6, MOM_objects=MOM_objects)
} else {
  MMSEList <- lapply(1:n.OM, runTuning_6, MOM_objects=MOM_objects)
}

elapse1 <- Sys.time() - st
MSEtool:::message_info(paste0(round(as.numeric(elapse1,2, units = "mins"), 2), " Minutes"))


MMSE <- combine_MMSE(MMSEList, 'name')
tune_vals <- tunefunc(MMSE)

df <- data.frame(MP=MPname, Metric=tuneMetric, TunePar=test_vals, TuneVal=tune_vals)

plot(df$TunePar, df$TuneVal, type='b')


  plot_tune <- function(tune_pars, tune_vals, trial=NULL) {
    plot(tune_pars,tune_vals,pch=19,xlab="Tuning Parameter",ylab="Target Variable")
    lines(tune_pars,tune_vals,col="grey")
    abline(h=tunetarg, lty=2)
    if (!is.null(trial)) {
      abline(v=trial,col='blue', lty=3)
    }
  }









  runTuning_3 <- function(om, histdir, MOM_objects) {
    multiHist <- readRDS(file.path(histdir, paste0(MOM_objects[om], '.mhist')))
    ProjectMOM(multiHist, MPs=c("MP1","MP2","MP3"),
               silent=TRUE,parallel = TRUE, checkMPs=FALSE)

  }

  # ---- Initial Search - wide range of parameters ----
  dif <- 10
  if (is.null(start_vals)) {
    if (is.null(test_vals))
      test_vals <- c(0.25, 0.5, 1, 1.25, 3, 5)

    MSEtool:::message_info('Initial Parameter Search with 20 simulations with tuning values:', paste(test_vals, collapse=', '))

    assign("par1", test_vals[1], envir=globalenv())
    assign("par2", test_vals[2], envir=globalenv())
    assign("par3", test_vals[3], envir=globalenv())
    assign("par4", test_vals[4], envir=globalenv())
    assign("par5", test_vals[5], envir=globalenv())
    assign("par6", test_vals[6], envir=globalenv())

    if (parallel) {
      snowfall::sfExport(list=c('par1', 'par2', 'par3', 'par4', 'par5', 'par6'))
    }

    st <- Sys.time()
    if (parallel) {
      MMSEList <- sfLapply(1:n.OM, runTuning_6, MOM_objects=MOM_objects)
    } else {
      MMSEList <- lapply(1:n.OM, runTuning_6, MOM_objects=MOM_objects)
    }

    elapse1 <- Sys.time() - st
    MSEtool:::message_info(paste0(round(as.numeric(elapse1,2, units = "mins"), 2), " Minutes"))

    # Check
    chk_class <- lapply(MMSEList, class)
    if (!all(chk_class=='MMSE')) {
      MSEtool:::message_warn('Did not return an MMSE object for each MOM. Stopping and returning the list of output from `multiMSE`')
      return(MMSEList)
    }

    MMSE <- combine_MMSE(MMSEList, 'name')
    tune_vals <- tunefunc(MMSE)

    val_ind <- 1:6
    if (6 %in% which(tune_vals < tunetarg)) {
      val_ind <- c(min(which(tune_vals < tunetarg)), max(which(tune_vals > tunetarg)))
    }
    if (1 %in% which(tune_vals < tunetarg)) {
      val_ind <- c(max(which(tune_vals < tunetarg)), min(which(tune_vals > tunetarg)))
    }

    x <- tune_vals[val_ind]
    y <- test_vals[val_ind]

    proposed <- try(suppressWarnings(approx(x,y,xout=tunetarg)$y[[1]]), silent=TRUE)

    if (inherits(proposed, 'try-error'))
      proposed <- NULL

    trial <- NULL
    if (!is.null(proposed)) {
      rng <- proposed*0.5*step
      trial <- c(proposed-rng, proposed+rng)
      trial[trial<min(y)]<- min(y) * 1.05
      trial[trial>max(y)]<- max(y) * 0.95
      trial <- c(trial[1], proposed, trial[2])
    }

    plot_tune(test_vals, tune_vals, trial)

    if (all(sign(tune_vals-tunetarg) ==-1)) {
      MSEtool:::message_warn(tuneMetric, 'below', tunetarg, 'for all tuning values. Stopping and returning the list of output from `multiMSE`')
      return(MMSEList)
    }
    if (all(sign(tune_vals-tunetarg) ==1)) {
      MSEtool:::message_warn(tuneMetric, 'above', tunetarg, 'for all tuning values. Stopping and returning the list of output from `multiMSE`')
      return(MMSEList)
    }

    dif <- abs(proposed-mean(test_vals))

    MSEtool:::message_info(paste0('Tuning Parameters: ', paste(round(test_vals,rnd), collapse=" ")))
    MSEtool:::message_info(paste0('Tuning Target: ', paste(round(tune_vals,rnd), collapse=" ")))
    MSEtool:::message_info('Proposed Tuning Parameter:',round(proposed,rnd))
    MSEtool:::message_info('Difference from Previous', round(dif,rnd), paste0('(tolerance: ', tol, ')') )
    MSEtool:::message_info(paste0('New Tuning Parameters: ', paste(round(trial,rnd), collapse=" ")))


  }

  # ---- Narrow Search - 3 parameters ----
  if (!is.null(start_vals)) trial <- start_vals
  vals_array <- array(NA, c(maxit,3))
  vals_array[1,] <- trial
  i_vals <- rep(NA, maxit)
  i_vals[1] <- mean(trial)

  out_MSE <- NULL
  i <- 0
  dfList <- list()
  elapse <- list()
  msg <- vector('character', maxit)
  while(i<=maxit & dif>tol){
    i <- i+1
    if (i>maxit)
      break()
    MSEtool:::message_info('Iteration', i, 'of', maxit)
    assign("par1", vals_array[i,1], envir=globalenv())
    assign("par2", vals_array[i,2], envir=globalenv())
    assign("par3", vals_array[i,3], envir=globalenv())

    if (parallel) {
      snowfall::sfExport(list=c('par1', 'par2', 'par3'))
    }

    MSEtool:::message_info(paste0('Tuning over ', n.OM, ' operating models'))
    st <- Sys.time()
    if (parallel) {
      MMSEList <- sfLapply(1:n.OM, runTuning_3, histdir=histdir, MOM_objects=MOM_objects)
    } else {
      MMSEList <- lapply(1:n.OM, runTuning_3, histdir=histdir, MOM_objects=MOM_objects)
    }
    elapse1 <- Sys.time() - st
    MSEtool:::message_info(paste0(round(as.numeric(elapse1,2, units = "mins"), 2), " Minutes"))
    elapse[[i]] <- elapse1

    chk_class <- lapply(MMSEList, class)
    if (!all(chk_class=='MMSE')) {
      msg[i] <- "Did not produce MSE objects for all OMs"
      out_MSE <- MMSEList
      break()
    }

    # make df
    dflist <- list()
    for (om in 1:n.OM) {
      dflist[[om]] <- data.frame(i=i,
                                 OM=om,
                                 TunePar=vals_array[i,],
                                 TuneVal=tunefunc(MMSEList[[om]])
      )
    }
    df <- do.call('rbind', dflist)
    MMSE <- combine_MMSE(MMSEList, 'name')

    tune_vals <- tunefunc(MMSE)

    x <- tune_vals
    y <- vals_array[i,]
    diff_vals <- diff(tune_vals)
    if (diff_vals[1]==0) {
      x <- x[2:3]
      y <- y[2:3]
    }
    if (diff_vals[2]==0) {
      x <- x[1:2]
      y <- y[1:2]
    }
    proposed <- try(suppressWarnings(approx(x,y,xout=tunetarg)$y[[1]]), silent=TRUE)

    if (is.na(proposed))
      proposed <- NULL

    if (inherits(proposed, 'try-error'))
      proposed <- NULL

    trial <- NULL
    if (!is.null(proposed)) {
      rng <- abs(vals_array[i,1]-vals_array[i,2])*0.5*step
      trial <- c(proposed-rng, proposed+rng)

      span_vals <- try(c(max(which(tune_vals - tunetarg>0)),
                         min(which(tune_vals - tunetarg<0))))

      if (all(is.finite(span_vals))) {
        ind <- which(trial>y[span_vals[2]])
        if (length(ind)>0)
          trial[ind] <- y[span_vals[2]]

        ind <- which(trial<y[span_vals[1]])
        if (length(ind)>0)
          trial[ind] <- y[span_vals[1]]
      }
      trial <- c(trial[1], mean(trial), trial[2])
    }

    plot_tune(vals_array[i,], tune_vals, trial)

    if (all(tune_vals > tunetarg)) {
      rng <- abs(vals_array[i,1]-vals_array[i,3])*0.5*step
      # all above
      ii <- which.min(tune_vals - tunetarg)
      if (ii==3) {
        trial <- c(vals_array[i,ii]+0.01*vals_array[i,ii], vals_array[i,ii]+2*rng)
      }
      if (ii==1) {
        trial <- c(vals_array[i,ii]-2*rng, vals_array[i,ii]+0.01*vals_array[i,ii])
      }
      if (ii==2) {
        trial <- c(vals_array[i,ii]-rng, vals_array[i,ii]+rng)
      }

    } else if (all(tune_vals < tunetarg)) {
      # all below
      rng <- abs(vals_array[i,1]-vals_array[i,3])*0.5*step
      ii <- which.max(tune_vals - tunetarg)
      if (ii==3) {
        trial <- c(vals_array[i,ii]+0.01*vals_array[i,ii], vals_array[i,ii]+2*rng)
      }
      if (ii==1) {
        trial <- c(vals_array[i,ii]-2*rng, vals_array[i,ii]+0.01*vals_array[i,ii])
      }
      if (ii==2) {
        trial <- c(vals_array[i,ii]-rng, vals_array[i,ii]+rng)
      }

    }

    if (!is.null(trial) & length(trial)==3) {
      i_vals[i+1] <- trial[2]
      if (i < maxit)
        vals_array[i+1,]<- trial
    } else {
      i_vals[i+1] <-  i_vals[i]
    }

    dif <- abs(i_vals[i+1]-i_vals[i])

    legend('topleft',legend=paste0('Diff = ', round(dif,5)),text.col='red',bty='n')

    MSEtool:::message_info(paste0('Tuning Parameters: ', paste(round(vals_array[i,],rnd), collapse=" ")))
    MSEtool:::message_info(paste0('Tuning Target: ', paste(round(tune_vals,rnd), collapse=" ")))
    MSEtool:::message_info('Proposed Tuning Parameter:',round(i_vals[i+1],rnd))
    MSEtool:::message_info('Difference from Previous', round(dif,rnd), paste0('(tolerance: ', tol, ')') )
    if (i < maxit & dif>tol) {
      MSEtool:::message_info(paste0('New Tuning Parameters: ', paste(round(vals_array[i+1,],rnd), collapse=" ")))
    } else {
      val <- i_vals[!is.na(i_vals)]
      val <- val[length(val)]
      MSEtool:::message_info(paste0('Final Tuning Parameter: ', paste(round(val,rnd), collapse=" ")))
    }

    dfList[[i]] <- df
  }
  cat('\n\n')

  df <- do.call('rbind', dfList)
  out <- list()
  tune_val <- i_vals[!is.na(i_vals)]
  out$tune_val <- tune_val[length(tune_val)]
  out$i_vals <- i_vals
  out$df <- df
  out$tunetarg <- tunetarg
  out$tuneMetric <- tuneMetric
  out$MSEtool.version <- packageVersion('MSEtool')
  out$SWOMSE.version <- packageVersion('SWOMSE')
  out$Date <- Sys.time()
  out$elapse <- elapse
  out$MP_name <- MPname
  out$msg <- msg
  out$date <- Sys.time()
  out$out_MSE <- out_MSE

  if (!is.null(savedir)) {
    nm <- paste(MPname, tuneMetric, tunetarg, sep='_')
    saveRDS(out, file.path(savedir, paste0(nm, '.rda')))
  }

  out
}







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



