#' Swordfish MSE package user manual
#'
#' @return Opens the user guide in browser
#' @export
#'
#' @examples
#' \dontrun{
#' SWO_userguide()
#' }
SWO_userguide <- function() {
  browseURL(file.path(system.file(package="SWOMSE"), "userguide.html"))
}


getclass <- function(x, classy) {
  return(any(class(get(x)) == classy)) # inherits(get(x), classy) - this gives a problem since we now inherit Stock etc in OM
}

#' What objects of this class are available
#'
#' Finds objects of the specified class in the global environment and `SWOMSE` package,
#' or from `SWOMSE`, `DLMtool`, and `MSEtool` packages.
#'
#' @param classy A class of object (character string, e.g. 'Fleet')
#' @param package Character. Either 'SWOMSE' to return objects from
#' package `SWOMSE` and global environment (default) or 'all' to also returns
#' objects from the `MSEtool` and `DLMtool` packages.
#'
#' @return A character string of available objects of the specified class
#' @export
#'
#' @examples
#' avail("OM")
avail <- function (classy, package=c("SWOMSE", 'all'))  {
  temp <- try(class(classy), silent = TRUE)
  if (class(temp) == "try-error")
    classy <- deparse(substitute(classy))
  if (temp == "function")
    classy <- deparse(substitute(classy))
  package <- match.arg(package)

  temp <- ls("package:SWOMSE")[vapply(ls("package:SWOMSE"),
                                      getclass, logical(1), classy = classy)]

  nums <- as.numeric(stringr::str_extract_all(temp, "[0-9]+"))
  nums[is.na(nums)] <- sum(is.na(nums)):1
  temp <- temp[order(nums)]

  temp_globalenv <- ls(envir = .GlobalEnv)[vapply(ls(envir = .GlobalEnv),
                                                  getclass, logical(1), classy = classy)]
  temp <- c(temp, temp_globalenv)

  if (package=="all") {
    temp <- c(temp, ls("package:MSEtool")[vapply(ls("package:MSEtool"),
                                         getclass, logical(1), classy = classy)])
      temp_DLMtool <- try(DLMtool::avail(classy), silent = TRUE)
      if (!inherits(temp_DLMtool, "try-error"))
        temp <- unique(c(temp, temp_DLMtool))

  }

  if (length(temp) < 1)
    stop("No objects of class '", classy, "' found",
         call. = FALSE)
  return(temp)
}
