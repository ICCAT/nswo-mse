#' Run the SWO MSE Shiny Application
#'
#' \code{Shiny} runs the SWO MSE Shiny Application
#' @references Modified from Deal Attali's code: \url{http://deanattali.com/2015/04/21/r-package-shiny-app/}
#' @export
Shiny <- function() {
  appDir <- system.file("shiny_apps", 'SWOMSE', package = "SWOMSE")
  shiny::runApp(appDir, display.mode = "normal",launch.browser = TRUE)
}
