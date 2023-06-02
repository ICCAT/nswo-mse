
#' Scope the Performance of a CMP across different tuning values
#'
#' Calculates performance metrics for a range of tuning values. Uses the Reference
#' OMs with 20 simulations
#'
#' @param MP_name Name of an `MP` function
#' @param Tuning_OMs Names of the `MOM` objects to tune over
#' @param TuneTargets A data.frame of tuning metrics and targets
#' @param Hist_dir Directory where `.hist` objects are saved
#' @param Tune_dir Directory where scoping results will be saved
#' @param test_vals Optional test values to use
#' @param parallel Logical. Run in parallel? (recommended)
#'
#' @return An invisible data.frame
#' @export
Scope <- function(MP_name, Tuning_OMs, TuneTargets, Hist_dir='Hist_Objects',
                  Tune_dir='Tuning_Objects',
                  test_vals=NULL,
                  parallel=TRUE) {

  MP <- try(get(MP_name), silent=TRUE)
  if (class(MP)=='try-error')
    stop(MP_name, ' not found. Have you sourced ', MP_name, '.R?')

  n.OM <- length(Tuning_OMs)

  if (parallel)
    if(!snowfall::sfIsRunning())
      setup()

  if (is.null(Hist_dir))
    stop('Must specify location of the multiHist objects corresponding to `MOM_objects`')

  histFiles <- list.files(Hist_dir, pattern='.hist')
  if (any(!Tuning_OMs %in% tools::file_path_sans_ext(histFiles))) {
    stop("'.hist' files not found for all `Tuning_OMs` in directory: ", file.path(getwd(), Hist_dir))
  }

  # ---- Performance Metrics ----
  PMs <- unique(TuneTargets$Metric)
  PMlist <- lapply(PMs, get)

  # ---- Tuning MPs ----
  MP1 <- function(x,Data,tunepar, ...) MP(x,Data,tunepar=par1, ...)
  MP2 <- function(x,Data,tunepar, ...) MP(x,Data,tunepar=par2, ...)
  MP3 <- function(x,Data,tunepar, ...) MP(x,Data,tunepar=par3, ...)
  MP4 <- function(x,Data,tunepar, ...) MP(x,Data,tunepar=par4, ...)
  MP5 <- function(x,Data,tunepar, ...) MP(x,Data,tunepar=par5, ...)
  MP6 <- function(x,Data,tunepar, ...) MP(x,Data,tunepar=par6, ...)
  MP7 <- function(x,Data,tunepar, ...) MP(x,Data,tunepar=par7, ...)
  MP8 <- function(x,Data,tunepar, ...) MP(x,Data,tunepar=par8, ...)


  class(MP1) <- class(MP2) <- class(MP3) <- class(MP4) <- class(MP5) <-
    class(MP6) <- class(MP7) <- class(MP8) <-  "MP"

  assign("MP1", MP1, envir=globalenv())
  assign("MP2", MP2, envir=globalenv())
  assign("MP3", MP3, envir=globalenv())
  assign("MP4", MP4, envir=globalenv())
  assign("MP5", MP5, envir=globalenv())
  assign("MP6", MP6, envir=globalenv())
  assign("MP7", MP7, envir=globalenv())
  assign("MP8", MP8, envir=globalenv())


  # --- export MPs to cluster ----
  if (parallel) {
    MSEtool:::message_info('Exporting MPs to cluster')
    snowfall::sfExport(list=c('MP', 'MP1', 'MP2', 'MP3', 'MP4', 'MP5', 'MP6',
                              'MP7', 'MP8', 'Tuning_OMs', 'Hist_dir'))
    sfLibrary("SWOMSE", character.only = TRUE, verbose=FALSE)
  }

  runTuning_8 <- function(om, Tuning_OMs) {
    MOM <- get(Tuning_OMs[om])
    MOM@nsim <- 20
    multiHist <- SimulateMOM(MOM, silent=T, parallel=FALSE)
    ProjectMOM(multiHist, MPs=c("MP1","MP2","MP3", 'MP4', 'MP5', 'MP6', 'MP7', 'MP8'),
               silent=TRUE,parallel = FALSE, checkMPs=FALSE)

  }

  # --- Scoping Values ----
  if (is.null(test_vals))
    test_vals <- seq(0.5, 1.5, length.out=8)

  if (length(test_vals)!=8)
    stop('`test_vals` must be length 8')

  assign("par1", test_vals[1], envir=globalenv())
  assign("par2", test_vals[2], envir=globalenv())
  assign("par3", test_vals[3], envir=globalenv())
  assign("par4", test_vals[4], envir=globalenv())
  assign("par5", test_vals[5], envir=globalenv())
  assign("par6", test_vals[6], envir=globalenv())
  assign("par7", test_vals[7], envir=globalenv())
  assign("par8", test_vals[8], envir=globalenv())


  if (parallel) {
    snowfall::sfExport(list=c('par1', 'par2', 'par3', 'par4', 'par5', 'par6',
                              'par7', 'par8'))
  }

  # --- Run closed-loop ---
  MSEtool:::message('\nScoping', MP_name, 'across', n.OM, 'OM(s):', paste0(Tuning_OMs, collapse=', '))

  st <- Sys.time()
  if (parallel) {
    MMSEList <- sfLapply(1:n.OM, runTuning_8, Tuning_OMs=Tuning_OMs)
  } else {
    MMSEList <- lapply(1:n.OM, runTuning_8, Tuning_OMs=Tuning_OMs)
  }

  elapse1 <- Sys.time() - st
  MSEtool:::message_info(paste0(round(as.numeric(elapse1,2, units = "mins"), 2), " Minutes"))

  MMSE <- combine_MMSE(MMSEList, 'name')

  PM_val_list <- list()
  for (i in 1:length(PMlist)) {
    val <- PMlist[[i]](MMSE)
    PM_val_list[[i]] <- data.frame(MP=MP_name, PM=PMs[i], tune_val=test_vals, PM_value=val@Mean)

  }

  df <- do.call('rbind', PM_val_list)
  df$Date <- Sys.time()
  df$elapse <- paste0(round(as.numeric(elapse1,2, units = "mins"), 2), " Minutes")

  nm <- paste0(MP_name, '.scopedf')
  saveRDS(df, file.path(Tune_dir, nm))
  invisible(df)
}



#' Tune a Candidate Management Procedure with 1 Tuning Parameter
#'
#' @param MOM_objects Character vector. Names of the `MOM` objects to tune over
#' @param histdir The file path to the directory where the multiHist objects are saved. The files in `histdir` must be `multiHist` objects with
#' names corresponding to the `MOM_objects` and file extension `.mhist`
#' @param MPname Character. Name of the CMP to tune
#' @param tuneMetric Name of a PM function
#' @param tunetarg The target value for `tuneMetric` output
#' @param step Step size for new proposed tuning values
#' @param maxit Maximum number of iterations
#' @param tol Stopping tolerance
#' @param rnd Numeric value for rounding displayed numbers
#' @param silent Logical. Hide the output while running closed-loop simulations?
#' @param parallel Run in parallel?
#' @param savedir Optional. Directory path to save the MP tuning object
#' @param start_vals. Optional. Numeric vector length 3. Increasing values. Initial tuning parameters.
#' @param test_vals Optional. Numeric vector length 6. Increasing values. Wider range initial tuning parameters.
#' @export
TuneCMP <- function(MOM_objects=NULL,
                    histdir=NULL,
                    MPname=NULL,
                    tuneMetric=NULL,
                    tunetarg=1,
                    step=0.8,
                    maxit=10,
                    tol=0.001,
                    rnd=3,
                    silent=TRUE,
                    parallel=TRUE,
                    savedir=NULL,
                    start_vals=NULL,
                    test_vals=NULL){

  MP <- try(get(MPname), silent=TRUE)

  if (class(MP)=='try-error')
    stop(MPname, ' not found. Have you sourced ', MPname, '.R?')

  n.OM <- length(MOM_objects)

  if (parallel)
    if(!snowfall::sfIsRunning())
      setup()

  MSEtool:::message('\nTuning', MPname)
  MSEtool:::message_info('Tuning over', n.OM, 'OM(s):', paste0(MOM_objects, collapse=', '))

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

  plot_tune <- function(tune_pars, tune_vals, trial=NULL) {
    plot(tune_pars,tune_vals,pch=19,xlab="Tuning Parameter",ylab="Target Variable")
    lines(tune_pars,tune_vals,col="grey")
    abline(h=tunetarg, lty=2)
    if (!is.null(trial)) {
      abline(v=trial,col='blue', lty=3)
    }
  }

  MSEtool:::message_info('Tuning', tuneMetric, 'to target value:', tunetarg)

  # ---- Tuning MPs ----

  MP1 <- function(x,Data,tunepar, ...) MP(x,Data,tunepar=par1, ...)
  MP2 <- function(x,Data,tunepar, ...) MP(x,Data,tunepar=par2, ...)
  MP3 <- function(x,Data,tunepar, ...) MP(x,Data,tunepar=par3, ...)
  MP4 <- function(x,Data,tunepar, ...) MP(x,Data,tunepar=par4, ...)
  MP5 <- function(x,Data,tunepar, ...) MP(x,Data,tunepar=par5, ...)
  MP6 <- function(x,Data,tunepar, ...) MP(x,Data,tunepar=par6, ...)
  class(MP1) <- class(MP2) <- class(MP3) <- class(MP4) <- class(MP5) <- class(MP6)<- "MP"

  assign("MP1", MP1, envir=globalenv())
  assign("MP2", MP2, envir=globalenv())
  assign("MP3", MP3, envir=globalenv())
  assign("MP4", MP4, envir=globalenv())
  assign("MP5", MP5, envir=globalenv())
  assign("MP6", MP6, envir=globalenv())

  # --- export MPs to cluster ----
  if (parallel) {
    MSEtool:::message_info('Exporting MPs to cluster')
    snowfall::sfExport(list=c('MP', 'MP1', 'MP2', 'MP3', 'MP4', 'MP5', 'MP6', 'MOM_objects'))
    sfLibrary("SWOMSE", character.only = TRUE, verbose=FALSE)
  }

  runTuning_6 <- function(om, MOM_objects) {
    mom <- get(MOM_objects[om])
    mom <- SubCpars(mom, sims=1:20)
    multiMSE(mom, MPs=c("MP1","MP2","MP3", 'MP4', 'MP5', 'MP6'),
               silent=TRUE,parallel = TRUE, checkMPs=FALSE)

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


write_rmd <- function(i, df) {
  tune_info <- paste('Tuned to', df$metric[i], '=', df$target[i], 'across Reference OMs.')
  # maxchange <- df$mc[i]
  # if (!is.na(maxchange)) {
  #   maxchangeinfo <- paste0('With a maximum absolute change of ', maxchange*100, '% in TAC between management cycles.')
  # } else {
  #   maxchangeinfo <- 'With no constraint on maximum absolute change in TAC between management cycles.'
  # }

  if (df$Code[i] == 'NA') return(NULL)

  MP <- strsplit(df$Name,'_')[[1]][1]

  oxygen_text <- ''
  oxygen_text[1] <-  paste("#' @describeIn", MP, tune_info)
  oxygen_text[2] <-  paste("#' @export")
  MP_text <- paste(df$Name[i], '<-', MP)

  arg_text <- ''
  arg_text[1] <- paste0('formals(', df$Name[i], ')$tunepar <- ', df$tunepar[i])
  # arg_text[2] <- paste0('formals(', df$Name[i], ')$mc <- ', df$mc[i])

  class_text <- paste0('class(', df$Name[i], ') <- "MP"')

  c(oxygen_text, MP_text, arg_text, class_text, '\n')
}


#' Title
#'
#' @param fl
#' @param tune.dir
#'
#' @return
#' @export
get_MP_info <- function(fl, tune.dir) {
  obj <- readRDS(file.path(tune.dir, fl))
  # TuneTarget <- read.csv(file.path('dev/MP_tuning', 'Tuning_Target_Codes.csv'))

  tt <- TuneTarget %>% filter(Target%in%obj$tunetarg,
                              Metric %in% obj$tuneMetric
                            )
  Code <- tt$Code
  if (length(Code)<1) Code <- 'NA'

  MP_family <- (obj$MP_name %>% strsplit(., '_'))[[1]][1]
  data.frame(Code=Code,
             Name=paste(obj$MP_name, Code, sep="_"),
             Family=MP_family,
             target=obj$tunetarg,
             metric=obj$tuneMetric,
             tunepar=obj$tune_val,
             file=fl)
}


#' Document and Export Tuned CMPs
#'
#' @param MP The name of the tuned CMP. The CMP function must be in an R script in 'R' directory with the file name
#' `MP.R`
#' @param tune.dir Directory where the tuning parameter objects (created with `TuneCMP`) are saved.
#'
#' @return Writes the tuned CMPs to `MP.R`
#' @export
document_MP <- function(MP, tune.dir) {
  MP.tune.files <- list.files(tune.dir, pattern=MP, full.names = FALSE)

  if(length(MP.tune.files)<1)
    stop('No tuning objects found in ', tune.dir, ' for MP: ', MP)

  MP.file <- paste0('R/', MP, '.R')
  if(!file.exists(MP.file))
    stop(MP.file, ' does not exist')

  df <- lapply(MP.tune.files, get_MP_info, tune.dir=tune.dir) %>% do.call('rbind', .) %>% arrange(Code)
  n <- nrow(df)
  MSEtool:::message(n, 'tuned CMPs found for', MP)
  MSEtool:::message_info('Writing tuned CMPs to', MP.file)

  txt <- readLines(MP.file)
  begin.tune <- which(grepl('# ---- Tuned CMPs ----', txt))

  if (length(begin.tune)<1) {
    # no tuned MPs exist
    tune_txt <- '\n# ---- Tuned CMPs ----'
    cat(tune_txt,file=MP.file, sep="\n", append = TRUE)
  } else {
    # delete all tuned MPs
    txt[begin.tune:length(txt)] <- ''
    last.line <- max(which(nchar(txt)!=0))
    txt <- txt[1:last.line]
    cat(txt,file=MP.file, sep="\n", append = FALSE)
    tune_txt <- '\n# ---- Tuned CMPs ----'
    cat(tune_txt,file=MP.file, sep="\n", append = TRUE)
  }

  txt <- readLines(MP.file)
  begin.tune <- which(grepl('# ---- Tuned CMPs ----', txt))

  txt.list <- lapply(1:nrow(df), write_rmd, df=df)

  for (i in seq_along(txt.list)) {
    cat(txt.list[[i]],file=MP.file, sep="\n", append = TRUE)
  }
}

