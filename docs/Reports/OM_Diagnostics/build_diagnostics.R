# Create the OM Diagnostic Reports

OM_Diagnostic_Report <- function(i, openFile=TRUE, quiet=TRUE) {

  diag.dir <- 'docs/Reports/OM_Diagnostics'
  input <- file.path(diag.dir, 'OM_Diagnostics.Rmd')
  output_file <- paste0('OM-', i, '-Report', '.html')
  rmarkdown::render(input, params=list(OM.n=i),
                    output_format = 'html_document', output_file=output_file, output_dir=diag.dir, quiet=quiet)
  if (openFile) utils::browseURL(output_file)
}

for (i in 40:nrow(SWOMSE::OM_DF)) {
  message(i)
  OM_Diagnostic_Report(i, FALSE)
}



