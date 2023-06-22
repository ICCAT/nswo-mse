
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
Scope <- function(MP_name, Tuning_OMs, TuneTargets,
                  Hist_dir='Hist_Objects',
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
    stop("'.hist' files not found for all `Tuning_OMs` in directory: ", file.path( Hist_dir))
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

  # save scoping MMSE list
  nm <- paste0(MP_name, '.mmse')
  if (!dir.exists(file.path(Tune_dir, 'scope_MMSE'))) {
    dir.create(file.path(Tune_dir, 'scope_MMSE'))
  }
  saveRDS(MMSEList, file.path(Tune_dir, 'scope_MMSE', nm))


  MMSE <- combine_MMSE(MMSEList, 'name')

  PM_val_list <- list()
  for (i in 1:length(PMlist)) {
    val <- PMlist[[i]](MMSE)
    PM_val_list[[i]] <- data.frame(MP=MP_name, PM=PMs[i], tune_val=test_vals, PM_value=val@Mean)

  }

  df <- do.call('rbind', PM_val_list)
  df$Date <- Sys.time()
  df$elapse <- paste0(round(as.numeric(elapse1,2, units = "mins"), 2), " Minutes")

  nm <- paste0(MP_name, '.scope')
  saveRDS(df, file.path(Tune_dir, nm))
  invisible(df)
}


#' Plot the results of scoping for an MP
#'
#' @param MP Name of the MP
#' @param Tune_dir Directory where scoping results are saved
#'
#' @return invisible list with ggplot object and data.frame
#' @export
Plot_Scope <- function(MP, Tune_dir='Tuning_Objects') {
  fls <- list.files(Tune_dir, pattern='.scope')

  if (!paste0(MP, '.scope') %in% fls) {
    stop(paste0(MP, '.scope'), ' not found in ', Tune_dir )
  } else {
    fl <- fls[fls==paste0(MP, '.scope')]
  }

  df <- readRDS(file.path(Tune_dir, fl))
  df$PM <- factor(df$PM, ordered = TRUE, levels=unique(df$PM))

  p <- ggplot(df, aes(x=tune_val, y=PM_value)) +
    facet_grid(MP~PM) +
    geom_point() +
    geom_line() +
    expand_limits(y=c(0,1)) +
    theme_bw()

  print(p)

  out <- list()
  out$df <- df
  out$p <- p
  invisible(out)

}


#' Tune an MP
#'
#' @param MP_name Name of an `MP` function
#' @param Tuning_OMs Names of the `MOM` objects to tune over
#' @param TuneTarget A data.frame of tuning metric and target
#' @param Hist_dir Directory where `.hist` objects are saved
#' @param Tune_dir Directory where scoping results will be saved
#' @param test_vals Optional test values to use
#' @param parallel Logical. Run in parallel? (recommended)
#' @param maxit maximum number of iterations
#' @param step step size
#' @param tol tolerance level
#' @param rnd rounding value
#'
#' @return saves results to `Tune_dir` and insivibly returns data.frame
#' @export
Tune <- function(MP_name, Tuning_OMs, TuneTarget,
                 Hist_dir='Hist_Objects',
                 Tune_dir='Tuning_Objects',
                 test_vals=NULL,
                 parallel=TRUE,
                 maxit=10,
                 step=0.8,
                 tol=0.001,
                 rnd=3) {

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
    stop("'.hist' files not found for all `Tuning_OMs` in directory: ", file.path(Hist_dir))
  }

  # ---- Performance Metric ----
  code <- TuneTarget$Code
  PMmetric <- TuneTarget$Metric
  PMfun <- get(TuneTarget$Metric)
  Target <- TuneTarget$Target
  allPMs <- avail('PM', 'SWOMSE', msg=F)

  # ---- Tuning MPs ----
  MP1 <- function(x,Data,tunepar, ...) MP(x,Data,tunepar=par1, ...)
  MP2 <- function(x,Data,tunepar, ...) MP(x,Data,tunepar=par2, ...)
  MP3 <- function(x,Data,tunepar, ...) MP(x,Data,tunepar=par3, ...)

  class(MP1) <- class(MP2) <- class(MP3) <-  "MP"

  assign("MP1", MP1, envir=globalenv())
  assign("MP2", MP2, envir=globalenv())
  assign("MP3", MP3, envir=globalenv())

  # --- export MPs to cluster ----
  if (parallel) {
    MSEtool:::message_info('Exporting MPs to cluster')
    snowfall::sfExport(list=c('MP', 'MP1', 'MP2', 'MP3', 'Tuning_OMs', 'Hist_dir', 'histFiles'))
    sfLibrary("SWOMSE", character.only = TRUE, verbose=FALSE)
  }

  runTuning_3 <- function(om, Hist_dir, histFiles) {
    multiHist <- readRDS(file.path(Hist_dir, histFiles[om]))
    ProjectMOM(multiHist, MPs=c("MP1","MP2","MP3"),
               silent=TRUE,parallel = FALSE, checkMPs=FALSE)

  }

  plot_tuning <- function(df) {
    p <- ggplot(df, aes(x=test_vals, y=Value)) +
      geom_point(size=2) +
      geom_line() +
      expand_limits(y=c(0,1)) +
      theme_bw() +
      geom_hline(aes(yintercept = Target), linetype=2)

    if (!any(is.na(df$proposed)))
      p <- p + geom_vline(aes(xintercept = proposed), linetype=2, color='blue')

    p + labs(x='Tuning Value', y='PM Value', title=PMmetric, subtitle=MP_name)

  }

  # --- Begin Tuning ----
  scope_files <- list.files(Tune_dir, pattern='.scope')
  nms <- unlist(strsplit(scope_files, '.scope'))
  scope <- MP_name %in% nms
  if (!scope & is.null(test_vals)) {
    stop('Scoping results not found for ', MP_name, '. Run `Scope` or provide `test_vals` argument')
  }

  if (is.null(test_vals)) {
    scope <- readRDS(file.path(Tune_dir, paste0(MP_name, '.scope')))
    scope <- scope %>% filter(PM==PMmetric)

    test_vals <- scope$tune_val
    PM_vals <- scope$PM_value
    # check for existing tune object
    if(file.exists(file.path(Tune_dir, paste0(MP_name, '.tune')))) {
      tune_obj <- readRDS(file.path(Tune_dir, paste0(MP_name, '.tune')))
      tune_obj <- tune_obj %>% filter(Name==PMmetric)

      scopedf <- data.frame(test_vals=test_vals, test_vals2=round(test_vals,2), PM_vals=PM_vals, type='Scope')
      tunedf <- data.frame(test_vals=tune_obj$test_vals, test_vals2=round(tune_obj$test_vals,2), PM_vals=tune_obj$Value, type='Tune')

      ii <- which(scopedf$test_vals2 %in% tunedf$test_vals2)
      if (length(ii)>0) {
        scopedf <-scopedf[-ii,]
      }
      temp_df <- bind_rows(scopedf, tunedf)
      test_vals <- temp_df$test_vals
      PM_vals <- temp_df$PM_vals
    }
    test_vals <- test_vals[order(PM_vals)]
    PM_vals <- PM_vals[order(PM_vals)]

    trial <- rep(0, 3)
    trial[1] <- test_vals[min(which(PM_vals > Target))]
    trial[3] <- test_vals[max(which(PM_vals < Target))]
    trial[2] <- mean(trial[c(1,3)])
    trial <- sort(trial)
  } else {
    trial <- test_vals
  }

  MSEtool:::message('\nTuning', MP_name, 'across', n.OM, 'OM(s):', paste0(Tuning_OMs, collapse=', '))
  MSEtool:::message('Tuning for', PMmetric, '=', Target)

  vals_array <- array(NA, c(maxit,3))
  vals_array[1,] <- trial
  i_vals <- rep(NA, maxit)
  i_vals[1] <- mean(trial)

  dif <- 10
  i <- 0

  results_list <- list()
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

    st <- Sys.time()
    if (parallel) {
      MMSEList <- sfLapply(1:n.OM, runTuning_3, Hist_dir=Hist_dir, histFiles=histFiles)
    } else {
      MMSEList <- lapply(1:n.OM, runTuning_3, Hist_dir=Hist_dir, histFiles=histFiles)
    }
    elapse1 <- Sys.time() - st
    MSEtool:::message_info(paste0(round(as.numeric(elapse1,2, units = "mins"), 2), " Minutes"))

    nm <- paste(MP_name, PMmetric, Target, i, sep="_")
    nm <- paste0(nm, '.mmse')
    if (!dir.exists(file.path(Tune_dir, 'tune_MMSE'))) {
      dir.create(file.path(Tune_dir, 'tune_MMSE'))
    }
    saveRDS(MMSEList, file.path(Tune_dir, 'tune_MMSE', nm))

    MMSE <- combine_MMSE(MMSEList, 'name')

    # Calculate PMs
    PM_results <- PM_table(MMSE,msg=FALSE)
    PM_results$test_vals <- vals_array[i,]
    PM_results$i <- i
    PM_results$elapse <- elapse1
    PM_results$MP <- MP_name
    PM_results$Target <- Target
    PM_results$PMmetric <- PMmetric
    results_list[[i]] <- PM_results

    # write out results_list
    results_df <- do.call('rbind', results_list)

    if (file.exists(file.path(Tune_dir, paste0(MP_name, '.tune')))) {
      load_results <- readRDS(file.path(Tune_dir, paste0(MP_name, '.tune')))
      results_df <- bind_rows(results_df, load_results)
    }
    saveRDS(results_df, file.path(Tune_dir, paste0(MP_name, '.tune')))

    # Tuning PM
    df <- PM_results %>% filter(Name == PMmetric)
    df$target <- Target

    tune_vals <- df$Value
    test_vals <- df$test_vals

    get_proposed <- function(tune_vals, test_vals, target, step) {
      x <- tune_vals
      y <- test_vals
      diff_vals <- diff(tune_vals)
      if (diff_vals[1]==0) {
        x <- x[2:3]
        y <- y[2:3]
      }
      if (diff_vals[2]==0) {
        x <- x[1:2]
        y <- y[1:2]
      }

      proposed <- try(suppressWarnings(approx(x, y, xout=Target)$y[[1]]), silent=TRUE)
      if (class(proposed)=='try-error') proposed <- NA

      rng <- abs(vals_array[i,1]-vals_array[i,2])*0.5*step
      trial <- c(proposed-rng, proposed+rng)

      trial[trial < min(y)] <- min(y)
      trial[trial > max(y)] <- max(y)

      proposed <- c(trial[1], proposed, trial[2])

    }

    proposed <- get_proposed(tune_vals, test_vals, target, step)

    df$proposed <- proposed

    p <- plot_tuning(df)
    print(p)

    # check if any values at target
    chk <- which(abs(tune_vals - Target) < 0.01)

    if (length(chk)>0) {
      if (length(chk)>1) {
        chk <- which.min(abs(tune_vals - Target))
      }
      i_vals[i+1] <- test_vals[chk]
      dif <- tol * 0.99
    } else {
      if (i < maxit)
        vals_array[i+1,]<- proposed
      i_vals[i+1] <- proposed[2]
      dif <- abs(i_vals[i+1]-i_vals[i])
    }

    if (is.na(dif)) {
      break()
      # which.min(abs(tune_vals-Target))



    }

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
  }

  invisible(do.call('rbind', results_list))

}


get_tune_val <- function(df, PM='PGK_6_10', Target=0.6) {
  df <- df %>% filter(Name==PM) %>% arrange(test_vals) %>%
    distinct(Value, test_vals, MP)

  MP <- unique(df$MP)

  ind <- which.min(abs(df$Value - Target))

  if (df$Value[ind] == Target) {
    proposed <- df$test_vals[ind]
    ind2 <- ind+1


  } else {
    if (df$Value[ind] > Target) {
      tt <- df %>% filter(Value<Target)
      ind2 <- which.min(abs(tt$Value - Target))
      val2 <- tt$test_vals[ind2]
      ind2 <- which(df$test_vals==val2)
    }
    if (df$Value[ind] < Target) {
      tt <- df %>% filter(Value>Target)
      ind2 <- which.min(abs(tt$Value - Target))
      val2 <- tt$test_vals[ind2]
      ind2 <- which(df$test_vals==val2)
    }

    y <- df$test_vals[c(ind, ind2)]
    x <- df$Value[c(ind, ind2)]
    proposed <- try(suppressWarnings(approx(x, y, xout=Target)$y[[1]]), silent=TRUE)

  }

  if (class(proposed)!='try-error') {
    pdf <- bind_rows(df[c(ind, ind2),], data.frame(test_vals=proposed, Value=Target))
  } else {
    pdf <- df
  }

  pdf$PM <- PM
  pdf$Target <- Target
  pdf$MP <- MP
  pdf
}

#' Plot the results of tuning for an MP
#'
#' @param MP_name Name of the MP

#' @param Tune_dir Directory where scoping results are saved
#' @param plot Logical. Show the plot?
#'
#' @return list ggplot object and data.frame
#' @export
Plot_Tune <- function(MP_name, Tune_dir='Tuning_Objects', plot=TRUE) {

  nm <- paste0(MP_name, '.tune')
  fls <- list.files(Tune_dir, pattern='.tune')

  if (!nm %in% fls)
    stop(nm, ' not found in ', Tune_dir)
  df <- readRDS(file.path(Tune_dir, nm))
  MP_family <- (MP_name %>% strsplit(., '_'))[[1]][1]

  # loop over TuneTargets
  df_out <- list()
  df_out2 <- list()
  for (j in 1:nrow(TuneTargets)) {
    df_out[[j]] <- get_tune_val(df, PM=TuneTargets$Metric[j], Target=TuneTargets$Target[j])
    tuned <- df_out[[j]] %>% filter(round(Value,2)==Target)
    if (nrow(tuned)>0) {
      ind <- which.min(abs(tuned$Value-tuned$Target))
      tune_val <- tuned$test_vals[ind]

      df_out2[[j]] <- data.frame(Code= TuneTargets$Code[j],
                                 Name=paste(MP_name, TuneTargets$Code[j], sep="_"),
                                 Family=MP_family,
                                 target=TuneTargets$Target[j],
                                 metric=TuneTargets$Metric[j],
                                 tunepar=tune_val)
    }

  }

  pdf <- do.call('rbind', df_out)
  df <- do.call('rbind', df_out2)

  p <- ggplot(pdf, aes(x=test_vals, y=Value)) +
    facet_grid(~PM, scales='free') +
    geom_line() +
    expand_limits(y=c(0,1)) +
    geom_hline(aes(yintercept=Target), linetype=2) +
    theme_bw()

  if (plot) {
    print(p)
  }
  out <- list()
  out$p <- p
  out$df <- df
  out
}



#' Document an MP
#'
#' @param MP_name Name of an `MP` function
#' @param MP_file Name of the R script containing the MP code (including path). If NULL, searches for
#' `R/MP_name.r`
#' @param Tune_dir Directory where scoping results are saved
#' @param plot logical. Show the plot?
#' @return Nothing
#' @export
Document_MP <- function(MP_name, MP_file=NULL,
                        Tune_dir='Tuning_Objects', plot=TRUE) {
  tdf <- Plot_Tune(MP_name, plot=plot)
  df_mp <- tdf$df
  n <- nrow(df_mp)
  MSEtool:::message(n, 'tuned CMPs found for', MP_name)


  if (is.null(MP_file) | is.na(MP_file)) {
    MP_file <- paste0('CMPs/', MP_name, '.R')
  }

  MSEtool:::message_info('Writing tuned CMPs to', MP_file)

  txt <- readLines(MP_file)
  begin.tune <- which(grepl('# ---- Tuned CMPs ----', txt))

  if (length(begin.tune)<1) {
    # no tuned MPs exist
    tune_txt <- '\n# ---- Tuned CMPs ----'
    cat(tune_txt,file=MP_file, sep="\n", append = TRUE)
  } else {
    # delete already tuned MPs
    ind <- which(grepl(paste0('@describeIn ',MP_name), txt))
    if (any(ind>0)) {
      ind2 <- max(ind)+5
      last.line <- max(which(nchar(txt)!=0))

      if (ind2>=last.line) {
        txt <- txt[1:(ind[1]-1)]
      } else {
        txt2a <- txt[1:(ind[1]-1)]
        txt2b <- txt[(ind2+1):last.line]
        txt <- c(txt2a, txt2b)
      }
      cat(txt,file=MP_file, sep="\n", append = FALSE)
    }
  }

  txt <- readLines(MP_file)

  txt.list <- lapply(1:nrow(df_mp), write_rmd, df=df_mp)

  for (i in seq_along(txt.list)) {
    cat(txt.list[[i]],file=MP_file, sep="\n", append = TRUE)
  }

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
