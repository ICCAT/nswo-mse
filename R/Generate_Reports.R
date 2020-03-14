#' Generate OM Summary Report
#'
#' Creates a HTML report with tables and figures summarizing the key properties
#' of the OMs from the base case and uncertainty grid.
#'
#' @param openFile Logical. Open the report in the browser?
#' @param quiet Logical. Suppress printing of the pandoc command line
#' @param dev Logical. Use the development version of RMD file?
#'
#' @return Nothing
#' @export
#'
OM_Summary_Report <- function(openFile=TRUE, quiet=TRUE, dev=FALSE) {

  input <- 'Rmd/OMs_Summary.RMD'
  if (dev) input <- file.path('inst', input)
  if (!dev) input <- file.path(system.file(package = "SWOMSE"), input)
  message('Generating OM Summary Report')
  rmarkdown::render(input, output_format = 'html_document', output_dir=getwd(), quiet=quiet)

  if (openFile) utils::browseURL('OMs_Summary.html')
}


#' Generate Diagnostic Report for specific OM
#'
#' @param OM Name of the OM; either 'base_case' or 1 - 288
#' @param openFile Logical. Open the report in the browser?
#' @param quiet Logical. Suppress printing of the pandoc command line
#' @param dev Logical. Use the development version of RMD file?
#'
#' @return Nothing
#' @export
#'
OM_Diagnostic_Report <- function(OM, openFile=TRUE, quiet=TRUE, dev=FALSE) {
  OM <- as.character(OM)
  OM.ns <- SWOMSE::OMs_DF$n
  if (OM =="base_case") {
    OM.n <- OM
  } else {
    if (OM %in% OM.ns) {
      tt <- SWOMSE::OMs_DF %>% filter(n==OM)
      if (!tt$conv)
        stop("OM ", OM, " did not converge. No diagnostic report can be generated")
    } else {
      stop("OM must be either 'base_case' or '", OM.ns[1], "' to '", OM.ns[length(OM.ns)], "'")
    }
  }


  input <- 'Rmd/OM_Report.RMD'
  if (dev) input <- file.path('inst', input)
  if (!dev) input <- file.path(system.file(package = "SWOMSE"), input)
  output_file <- paste0('OM-', OM, '-Report', '.html')
  message('Generating OM Diagnostic Report for OM ', OM)
  rmarkdown::render(input, params=list(OM.n=OM),
                    output_format = 'html_document', output_file=output_file, output_dir=getwd(), quiet=quiet)

  if (openFile) utils::browseURL(output_file)
}
