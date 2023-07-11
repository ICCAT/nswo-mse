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
