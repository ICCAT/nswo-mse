library(SWOMSE)

# Tuning OMs (Reference OMs)
Tuning_OMs <- OM_DF %>% filter(Class=='Reference')
Tuning_OMs <- Tuning_OMs$OM.object

# Tuning Targets
TuneTargets <- read.csv('dev/MP_tuning/Tuning_Target_Codes.csv')
TuneTargets

# --- Scope CMP Performance over Range of Tuning Values ----

Scope_MPs <- 'CE_un'

for (i in seq_along(Scope_MPs)) {
  MP_name <- Scope_MPs[i]
  Scope(MP_name, Tuning_OMs, TuneTargets)
}


Plot_Scope()

Plot_Scope <- function(MP=NULL, Tune_dir='Tuning_Objects') {
  fls <- list.files(Tune_dir, pattern='.scope')

  out <- list()
  for (i in seq_along(fls)) {
    out[[i]] <- readRDS(file.path(Tune_dir, fls[i]))
  }
  df <- do.call('rbind', out)
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


# ---- Tune an MP ----

MP_name <- 'IR2'
TuneTarget <- TuneTargets %>% filter(Code=='a')

tt <- Tune(MP_name, Tuning_OMs, TuneTarget)

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
    stop("'.hist' files not found for all `Tuning_OMs` in directory: ", file.path(getwd(), Hist_dir))
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
  scope_files <- list.files(Tune_dir, pattern='.scopedf')
  nms <- unlist(strsplit(scope_files, '.scopedf'))
  scope <- MP_name %in% nms
  if (!scope & is.null(test_vals)) {
    stop('Scoping results not found for ', MP_name, '. Run `Scope` or provide `test_vals` argument')
  }

  if (is.null(test_vals)) {
    scope <- readRDS(file.path(Tune_dir, paste0(MP_name, '.scopedf')))
    scope <- scope %>% filter(PM==PMmetric)

    test_vals <- scope$tune_val
    PM_vals <- scope$PM_value
    # check for existing tune object
    if(file.exists(file.path(Tune_dir, paste0(MP_name, '.tune')))) {
      tune_obj <- readRDS(file.path(Tune_dir, paste0(MP_name, '.tune')))
      tune_obj <- tune_obj %>% filter(Name==PMmetric)

      test_vals <- c(test_vals, tune_obj$test_vals)
      PM_vals <- c(PM_vals, tune_obj$Value)
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

  MSEtool:::message('\nScoping', MP_name, 'across', n.OM, 'OM(s):', paste0(Tuning_OMs, collapse=', '))
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

    MMSE <- combine_MMSE(MMSEList, 'name')

    # Calculate PMs
    PM_results <- PM_table(MMSE,msg=FALSE)
    PM_results$test_vals <- vals_array[i,]
    PM_results$i <- i
    PM_results$elapse <- elapse1
    PM_results$MP <- MP_name

    results_list[[i]] <- PM_results

    # write out results_list
    results_df <- do.call('rbind', results_list)
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

    plot_tuning(df)

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
    }

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














