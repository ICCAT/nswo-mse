message <- MSEtool:::message


#' Access the SWOMSE User Manual
#'
#' Opens the `SWOMSE` User Manual in a browser window
#' @return Nothing.
#' @export
#'
SWO_userguide <- function() {
  utils::browseURL('docs/UserManual/User_Manual.html')
}


#' Trim Data object
#'
#' @param Data An object of class `Data`
#' @param year The year to trim the data to
#'
#' @return  An object of class `Data`
#' @export
#'
Trim_Data <- function(Data, year) {
  Year_ind <- which(Data@Year<=year)
  Data@Year <- Data@Year[Year_ind]
  Data@Ind <- Data@Ind[,Year_ind]
  Data@AddInd <- Data@AddInd[,,Year_ind]
  Data@Cat <- Data@Cat[,Year_ind]
  Data@MPrec <- Data@Cat[, max(Year_ind)]
  Data

}


#' Source CMP Scripts
#'
#' @param CMPdir
#'
#' @return
#' @export
source_CMPs <- function(CMPdir='CMPs') {
  CMP_files <- list.files(CMPdir)
  for (fl in CMP_files) source(file.path(CMPdir, fl))

}

#' Scope and Tune an MP to the Tuning Targets
#'
#' Wrapper for `Tune` and `Scope`
#' @param MP_name
#' @param Tuning_OMs
#' @param Hist_dir
#' @param Tune_dir
#' @param test_vals
#' @param parallel
#'
#' @return
#' @export
#'
Tune_MP <- function(MP_name, Tuning_OMs,
                    Hist_dir = "Hist_Objects/Reference",
                    Tune_dir = "Tuning_Objects",
                    test_vals = NULL,
                    parallel = TRUE) {

  # Scope
  if (MP_name %in% c('EA1')) {
    tt <- try(Scope(MP_name, Tuning_OMs, test_vals=seq(0.1,1, length.out=8)))
  } else {
    tt <- try(Scope(MP_name, Tuning_OMs))
  }

  try(Plot_Scope(MP_name))

  # Tune
  for (i in 1:nrow(TuneTargets)) {
    TuneTarget <- TuneTargets[i,]
    tt <- try(Tune(MP_name, Tuning_OMs, TuneTarget))
  }
}



#' Get Location of CMP script
#'
#' @return
#' @export
get_MP_locations <- function(MPs) {
  R_files <- list.files('R')
  for (fl in R_files)
    source(file.path('R', fl))
  sapply(1:length(MPs), function(x)
    get_location(MPs[x])
  )
}

get_location <- function(MP_name) {
  attrs <- attributes(body(MP_name))
  attrs$srcfile$filename
}

#' Get Names of Tuned MPs
#'
#' @return
#' @export
get_tune_MPs <- function(MP_name) {
  MPs <- paste(MP_name, TuneTargets$Code, sep='_')

  for (i in 1:length(MPs)) {
    tt <- try(get(MPs[i]), silent=TRUE)
    if (class(tt)=='try-error')
      MPs[i] <- NA
  }
  MPs[!is.na(MPs)]
}
