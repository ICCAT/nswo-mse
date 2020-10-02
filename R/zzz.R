.onLoad <- function(libname, pkgname) {
  pck <- require("OMtool") # temporary name for the updated DLMtool package
  if (!pck) {
    devtools::install_github('DLMtool/OMtool')
    library('OMtool')
  }
  # avoid clashes with existing packages
  if("MSEtool" %in% (.packages())){
    detach("package:MSEtool", unload=TRUE)
  }
  if("DLMtool" %in% (.packages())){
    detach("package:DLMtool", unload=TRUE)
  }

  invisible()
}
