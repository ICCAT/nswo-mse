
library(SWOMSE)

OM_Diagnostic_Report <- function(i, openFile=TRUE, quiet=TRUE) {

  diag.dir <- 'docs/Reports/OM_Diagnostics'
  input <- file.path(diag.dir, 'OM_Diagnostics.Rmd')
  output_file <- paste0('OM-', i, '-Report', '.html')
  rmarkdown::render(input, params=list(om.num=i),
                    output_format = 'html_document', output_file=output_file, output_dir=diag.dir, quiet=quiet)
  if (openFile) utils::browseURL(file.path(diag.dir,output_file))
}

for (om.num in OM_DF$OM.num) {
  message(om.num)
  OM_Diagnostic_Report(om.num, FALSE)
}

