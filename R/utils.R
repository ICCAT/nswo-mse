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

