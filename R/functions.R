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

  }

  if (length(temp) < 1)
    stop("No objects of class '", classy, "' found",
         call. = FALSE)
  return(temp)
}

addLabels <- function(p, labelR=NULL, labelT=NULL, font.size=14, plot=FALSE) {

  z <- ggplot2::ggplotGrob(p)

  if (!is.null(labelR)) {
    # Get the positions of the strips in the gtable: t = top, l = left, ..
    posR <- subset(z$layout, grepl("strip-r", name), select = t:r)
    # Add a new column to the right of current right strips,
    # and a new row on top of current top strips
    width <- z$widths[max(posR$r)]    # width of current right strips
    z <- gtable::gtable_add_cols(z, width, max(posR$r))
    # Construct the new strip grobs
    stripR <- grid::gTree(name = "Strip_right", children = grid::gList(
      grid::rectGrob(gp = grid::gpar(col = NA, fill = "white")),
      grid::textGrob(labelR, rot = -90, gp = grid::gpar(fontsize = font.size, col = "grey10"))))
    # Position the grobs in the gtable
    z <- gtable::gtable_add_grob(z, stripR, t = min(posR$t), l = max(posR$r)+1, b = max(posR$b), name = "strip-right")

    # Add small gaps between strips
    z <- gtable::gtable_add_cols(z, grid::unit(1/5, "line"), max(posR$r))
  }
  if (!is.null(labelT)) {
    posT <- subset(z$layout, grepl("strip-t", name), select = t:r)
    height <- z$heights[min(posT$t)]  # height of current top strips
    z <- gtable::gtable_add_rows(z, height, min(posT$t)-1)

    stripT <- grid::gTree(name = "Strip_top", children = grid::gList(
      grid::rectGrob(gp = grid::gpar(col = NA, fill = "white")),
      grid::textGrob(labelT, gp = grid::gpar(fontsize = font.size, col = "grey10"))))
    z <- gtable::gtable_add_grob(z, stripT, t = min(posT$t), l = min(posT$l), r = max(posT$r), name = "strip-top")
    z <- gtable::gtable_add_rows(z, grid::unit(1/5, "line"), min(posT$t))
  }

  if (plot) grid.draw(z)
  return(invisible(z))
}
