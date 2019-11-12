

#' Average Yield in the first 10 Years
#'
#'See `?LTY` for more information.
#'
#' @param MSEobj An object of class `MSE`
#' @param Ref Reference point for calculating the performance metric.
#' @param Yrs Numeric vector of length 2 with year indices to summarize performance.
#' If NULL, the performance is summarized over all projection years.
#'
#' @return An object of class `PMobj`
#' @export
#'
#' @examples
#' \dontrun{
#' MSE <- runMSE()
#' STC(MSE)
#' }
STC <- function(MSEobj=NULL, Ref=1, Yrs=10) {
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- paste0("Mean Short-Term Yield \n(Years ", Yrs[1], "-", Yrs[2], ")")
  PMobj@Caption <- paste0("Mean Short-Term Yield \n(Years ", Yrs[1], "-", Yrs[2], ")")

  PMobj@Stat <- MSEobj@C[,,Yrs[1]:Yrs[2]]
  PMobj@Ref <- Ref
  PMobj@Prob <- calcProb(PMobj@Stat, MSEobj) # no probability to calculate

  PMobj@Mean <- calcMean(PMobj@Prob) # calculate mean probability by MP
  PMobj@MPs <- MSEobj@MPs
  PMobj
}
class(STC) <- "PM"



#' Average Yield in the last 10 Years
#'
#'See `?LTY` for more information.
#'
#' @param MSEobj An object of class `MSE`
#' @param Ref Reference point for calculating the performance metric.
#' @param Yrs Numeric vector of length 2 with year indices to summarize performance.
#' If NULL, the performance is summarized over all projection years.
#'
#' @return An object of class `PMobj`
#' @export
#'
#' @examples
#' \dontrun{
#' MSE <- runMSE()
#' LTC(MSE)
#' }
LTC <- function(MSEobj=NULL, Ref=1, Yrs=-10) {
  Yrs <- ChkYrs(Yrs, MSEobj)
  PMobj <- new("PMobj")
  PMobj@Name <- paste0("Mean Long-Term Yield \n(Years ", Yrs[1], "-", Yrs[2], ")")
  PMobj@Caption <- paste0("Mean Long-Term Yield \n(Years ", Yrs[1], "-", Yrs[2], ")")

  PMobj@Stat <- MSEobj@C[,,Yrs[1]:Yrs[2]]
  PMobj@Ref <- Ref
  PMobj@Prob <- calcProb(PMobj@Stat, MSEobj) # no probability to calculate

  PMobj@Mean <- calcMean(PMobj@Prob) # calculate mean probability by MP
  PMobj@MPs <- MSEobj@MPs
  PMobj
}
class(LTC) <- "PM"
