library(SWOMSE)
library(dplyr)


# Generate Table of OM Axes of Uncertainty

makeDF <- function(grid.dir) {

  save.dir <- 'inst/shiny_apps/SWOMSE/data/'
  ss.dirs <- list.dirs(grid.dir, recursive = FALSE)

  # MSE files
  dir.create(file.path(save.dir, basename(grid.dir)))

  outlist <- list();
  for (i in 1:length(ss.dirs)) {
    message(i, '/', length(ss.dirs))
    ss.dir <- file.path(ss.dirs[i])

    MSE <- readRDS(file.path(ss.dir, 'MSE.rda'))
    MSE@PPD <- list()
    MSE@Hist <- new('Hist')
    MSE@CB_hist <- MSE@FM_hist <- MSE@SSB_hist <- matrix()
    MSE@RefPoint <-  MSE@BioEco <- list()


    dir.create(file.path(save.dir, basename(grid.dir), i))
    saveRDS(MSE, paste0(file.path(save.dir, basename(grid.dir), i), '/MSE.rda'))

    vals <- as.numeric(strsplit(MSE@Name, '-')[[1]][seq(2, by=2, length=6)])

    outlist[[i]] <- data.frame(i=i,
                               M=vals[1],
                               sigmaR=vals[2],
                               h=vals[3],
                               lambda=vals[4],
                               incQ=vals[5],
                               incE=vals[6])
  }

  DF <- do.call('rbind', outlist)
  saveRDS(DF, paste0(file.path(save.dir, basename(grid.dir)), '/DF.rda'))

}

# # Run function to save parameter assumption table
# # Grid Dir - Original Size Comp
# OMgrid <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/grid_2021/grid_May2021/'
#
# makeDF(OMgrid)
#
# # Grid Dir - Adjusted Size Comp
# OMgrid_adjusted <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/grid_2021/grid_May2021_shifted/'
#
# makeDF(OMgrid_adjusted)
#
# # Make overall OM table
# DF1 <- readRDS('inst/shiny_apps/SWOMSE/data/grid_May2021/DF.rda')
# DF2 <- readRDS('inst/shiny_apps/SWOMSE/data/grid_May2021_shifted/DF.rda')
#
# DF1$Size <- 'Original'
# DF1$Name <- paste0('OM_',DF1$i)
#
# DF2$Size <- 'Adjusted'
# DF2$Name <- NA
#
# for (i in 1:nrow(DF2)) {
#   vals <- as.numeric(DF1[i,2:7])
#   subDF <- DF2 %>% filter(M==vals[1],
#                           sigmaR==vals[2],
#                           h==vals[3],
#                           lambda==vals[4],
#                           incQ==vals[5],
#                           incE==vals[6])
#   nm <- DF1$Name[i]
#   DF2$Name[subDF$i] <- paste0(nm, '_adj')
# }
#
# DF_all <- dplyr::bind_rows(DF1, DF2)
# DF_all <- DF_all %>% arrange(Name)

# regexp <- "[[:digit:]]+"
# numbers <- as.numeric(stringr::str_extract(DF_all$Name, regexp))
#
# DF_all <- DF_all[order(numbers), ]
# DF_all$Name <- factor(DF_all$Name, level=DF_all$Name, ordered = TRUE)

# saveRDS(DF_all, 'inst/shiny_apps/SWOMSE/data/OMTable.rds')



# DF_all <- OM_Axes<- readRDS('inst/shiny_apps/SWOMSE/data/OMTable.rds')
OM_Axes <- readRDS('data/OMTable.rds')
OM_Axes$incE <- as.logical(OM_Axes$incE)

OM_Axes$incQ[OM_Axes$incQ == 1] <- FALSE
OM_Axes$incQ[OM_Axes$incQ>1] <- TRUE
OM_Axes$incQ <- as.logical(OM_Axes$incQ)

OMnames <- OM_Axes$Name

PMnames <- avail("PM", 'SWOMSE')

joinMSE2 <- function (MSEobjs = NULL) {
  if (class(MSEobjs) != "list")
    stop("MSEobjs must be a list")
  if (length(MSEobjs) < 2)
    stop("MSEobjs list doesn't contain multiple MSE objects")
  lapply(MSEobjs, checkMSE)
  if (!all(unlist(lapply(MSEobjs, class)) == "MSE"))
    stop("Objects in list must all be class `MSE`")
  MPNames <- lapply(MSEobjs, getElement, name = "MPs")
  allsame <- length(unique(lapply(MPNames, unique))) == 1
  if (!allsame) {
    mpnames <- unlist(MPNames)
    npack <- length(MSEobjs)
    tab <- table(mpnames)
    ind <- tab == npack
    commonMPs <- names(tab)[ind]
    if (length(commonMPs) < 1)
      stop("No common MPs in MSE objects", call. = FALSE)
    MSEobjs <- lapply(MSEobjs, Sub, MPs = commonMPs)
    message("MPs not in all MSE objects:")
    message(paste(names(tab)[!ind], ""))
    message("Dropped from final MSE object.")
  }
  Nobjs <- length(MSEobjs)
  for (X in 1:Nobjs) {
    tt <- MSEobjs[[X]]
    assign(paste0("obj", X), tt)
    if (X > 1) {
      tt <- MSEobjs[[X]]
      tt2 <- MSEobjs[[X - 1]]
      if (!all(slotNames(tt) == slotNames(tt2)))
        stop("The MSE objects don't have the same slots")
      if (any(tt@MPs != tt2@MPs))
        stop("MPs must be the same for all MSE objects")
    }
  }
  chkmat <- matrix(NA, nrow = Nobjs, ncol = 2)
  nms <- NULL
  for (X in 1:Nobjs) {
    tt <- get(paste0("obj", X))
    chkmat[X, ] <- c(tt@nyears, tt@proyears)
    if (X > 1)
      if (!any(grepl(tt@Name, nms)))
        stop("MSE objects have different names")
    nms <- append(nms, tt@Name)
  }
  chk <- all(colSums(chkmat) == chkmat[1, ] * Nobjs)
  if (!chk)
    stop("The MSE objects have different number of nyears or proyears")
  Allobjs <- mget(paste0("obj", 1:Nobjs))
  Name <- Allobjs[[1]]@Name
  nyears <- Allobjs[[1]]@nyears
  proyears <- Allobjs[[1]]@proyears
  nMPs <- length(MPNames[[1]])
  MPs <- MPNames[[1]]
  nsim <- sum(sapply(Allobjs, slot, name = "nsim"))
  OM <- do.call("rbind", lapply(Allobjs, slot, name = "OM"))
  Obs <- do.call("rbind", lapply(Allobjs, slot, name = "Obs"))
  SB_SBMSY <- abind::abind(lapply(Allobjs, slot, name = "SB_SBMSY"),
                           along = 1)
  F_FMSY <- abind::abind(lapply(Allobjs, slot, name = "F_FMSY"),
                         along = 1)
  N <- abind::abind(lapply(Allobjs, slot, name = "N"), along = 1)
  B <- abind::abind(lapply(Allobjs, slot, name = "B"), along = 1)
  SSB <- abind::abind(lapply(Allobjs, slot, name = "SSB"),
                      along = 1)
  VB <- abind::abind(lapply(Allobjs, slot, name = "VB"), along = 1)
  FM <- abind::abind(lapply(Allobjs, slot, name = "FM"), along = 1)
  SPR <- list()
  Catch <- abind::abind(lapply(Allobjs, slot, name = "Catch"),
                        along = 1)
  Removals <- abind::abind(lapply(Allobjs, slot, name = "Removals"),
                           along = 1)
  Effort <- abind::abind(lapply(Allobjs, slot, name = "Effort"),
                         along = 1)
  TAC <- abind::abind(lapply(Allobjs, slot, name = "TAC"),
                      along = 1)
  TAE <- abind::abind(lapply(Allobjs, slot, name = "TAE"),
                      along = 1)
  BioEco <- list()
  BioEco_List <- lapply(Allobjs, slot, name = "BioEco")
  nms <- names(Allobjs[[1]]@BioEco)
  for (nm in nms) {
    temp <- list()
    for (obj in 1:length(Allobjs)) {
      temp[[obj]] <- BioEco_List[[obj]][[nm]]
    }
    BioEco[[nm]] <- abind::abind(temp, along = 1)
  }
  RefPoint <- list()
  RefPoint_List <- lapply(Allobjs, slot, name = "RefPoint")
  nms <- names(Allobjs[[1]]@RefPoint)
  for (nm in nms) {
    temp <- list()
    for (obj in 1:length(Allobjs)) {
      temp[[obj]] <- RefPoint_List[[obj]][[nm]]
    }
    if (class(temp[[1]]) == "list") {
      nms2 <- names(temp[[1]])
      for (j in seq_along(nms2)) {
        temp2 <- list()
        for (k in 1:length(temp)) {
          temp2[[k]] <- temp[[k]][[j]]
        }
        nm2 <- nms2[j]
        RefPoint[[nm]][[nm2]] <- abind::abind(temp2,
                                              along = 1)
      }
    }
    else {
      RefPoint[[nm]] <- abind::abind(temp, along = 1)
    }
  }
  CB_hist <- abind::abind(lapply(Allobjs, slot, name = "CB_hist"),
                          along = 1)
  FM_hist <- abind::abind(lapply(Allobjs, slot, name = "FM_hist"),
                          along = 1)
  SSB_hist <- abind::abind(lapply(Allobjs, slot, name = "SSB_hist"),
                           along = 1)

  PPD <- list()
  Hist <- new('Hist')

  Misc <- list()
  MSE <- new("MSE", Name, nyears, proyears, nMPs, MPs, nsim,
             OM, Obs, SB_SBMSY, F_FMSY, N, B, SSB, VB, FM, SPR, Catch,
             Removals, Effort, TAC, TAE, BioEco, RefPoint, CB_hist,
             FM_hist, SSB_hist, Hist, PPD, Misc)
  MSE
}


#' Generic Trade-Plot Function
#'
#' @param MSEobj An object of class `MSE` or `MMSE` or a list of these objects
#' @param ... Names of Performance Metrics (PMs), or other arguments to `TradePlot`. First PM is recycled if number of PMs is not even
#' @param Lims A numeric vector of acceptable risk/minimum probability thresholds. Recycled if not equal to number of PMs.
#' @param Title Optional title for each plot. Character vector of `length(PMs)`/2. Recycled.
#' @param Labels Optional named list specifying new labels for MPs. For example: `Labels = list(AvC="Average Catch", CC1="Constant Catch")`
#' @param Satisficed Logical. Show only the MPs that meet minimum acceptable thresholds (specified in `Lims`)
#' @param Show Character. Show the plots ('plots'), results table ('table'), 'both' (default), or invisibly return objects only ('none')
#' @param point.size Numeric. Size of the MP points
#' @param lab.size Numeric. Size of MP label. Set to NULL to remove MP labels.
#' @param axis.title.size Numeric. Size of axis titles
#' @param axis.text.size Numeric. Size of axis text
#' @param legend Logical. Include legend?
#' @param legend.title.size Numeric. Size of legend title text
#' @param position Character. Position of legend - 'right' or 'bottom'
#' @param cols Optional character vector of colors for the legend (MP Types) or if `cols` is a character vector of length `MSEobj@nMPs`,
#' then the MP labels are colored (no color legend).
#' @param fill Character. Color of the fill
#' @param alpha Numeric. Transparency of fill
#' @param PMlist Optional list of PM names. Overrides any supplied in ... above
#' @param Refs An optional named list (matching the PM names) with numeric values to override the default `Ref` values. See examples.
#' @param Yrs An optional named list (matching the PM names) with numeric values to override the default `Yrs` values. See examples.


#' @author A. Hordyk
#' @return Invisibly returns a list with summary table of MP performance and
#' the ggplot objects for the plots
#' @export
#'
TradePlot_2 <- function(MSEobj, ..., Lims=c(0.2, 0.2, 0.8, 0.8),
                      Title=NULL,
                      Labels=NULL,
                      Satisficed=FALSE,
                      Show='both',
                      point.size=2,
                      lab.size=4,
                      axis.title.size=12,
                      axis.text.size=10,
                      legend=TRUE,
                      legend.title.size=12,
                      position = c("right", "bottom"),
                      cols=NULL,
                      fill="gray80",
                      alpha=0.4,
                      PMlist=NULL,
                      Refs=NULL,
                      Yrs=NULL
) {
  if (class(MSEobj) == 'list') {
    cls <- unlist(lapply(MSEobj, class))
    if (length(unique(cls))>1)
      stop('Objects in list must all be class `MSE` or `MMSE`')
  } else {
    if (class(MSEobj) != 'MSE' & class(MSEobj) !='MMSE')
      stop("Object must be class `MSE` or class `MMSE`", call.=FALSE)
  }

  if (class(MSEobj)=='MMSE') legend <- FALSE
  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    stop("Package \"ggrepel\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (is.null(PMlist)) {
    PMlist <- unlist(list(...))
  } else {
    PMlist <- unlist(PMlist)
  }
  position <- match.arg(position)

  if(length(PMlist) == 0) PMlist <- c("STY", "LTY", "P10", "AAVY")
  if (class(PMlist) != 'character') stop("Must provide names of PM methods")
  # check

  # for (X in seq_along(PMlist))
  #   if (!class(PMlist[X]) =="PM") stop(PMlist[X], " is not a valid PM method")
  if (length(PMlist)<2) stop("Must provided more than 1 PM method")

  if (is.null(cols)) {
    cols <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")
  }

  if (class(MSEobj)!='list') {
    if (length(cols) == MSEobj@nMPs) {
      Col <- 'MP'
    } else {
      Col <- 'Class'
    }
  } else {
    Col <- 'OM'
  }

  nPMs <- length(PMlist)
  if (nPMs %% 2 != 0) {
    message("Odd number of PMs. Recycling first PM")
    PMlist <- c(PMlist, PMlist[1])
    nPMs <- length(PMlist)
  }
  if (length(Lims) < nPMs) {
    message("Recycling limits")
    Lims <- rep(Lims,10)[1:nPMs]
  }
  if (length(Lims) > nPMs) {
    Lims <- Lims[1:nPMs]
  }

  if (class(MSEobj)!='list') {
    runPM <- vector("list", length(PMlist))
    for (X in 1:length(PMlist)) {
      ref <- Refs[[PMlist[X]]]
      yrs <- Yrs[[PMlist[X]]]
      if (is.null(ref)) {
        if (is.null(yrs)) {
          runPM[[X]] <- eval(call(PMlist[X], MSEobj))
        } else {
          runPM[[X]] <- eval(call(PMlist[X], MSEobj, Yrs=yrs))
        }

      } else {
        if (is.null(yrs)) {
          runPM[[X]] <- eval(call(PMlist[X], MSEobj, Ref=ref))
        } else {
          runPM[[X]] <- eval(call(PMlist[X], MSEobj, Ref=ref, Yrs=yrs))
        }
      }
    }
  } else {
    runPM <- vector("list", length(PMlist))
    OM_PMs <- vector("list", length(MSEobj))
    for (om in 1:length(MSEobj)) {
      for (X in 1:length(PMlist)) {
        ref <- Refs[[PMlist[X]]]
        yrs <- Yrs[[PMlist[X]]]
        if (is.null(ref)) {
          if (is.null(yrs)) {
            runPM[[X]] <- eval(call(PMlist[X], MSEobj[[om]]))
          } else {
            runPM[[X]] <- eval(call(PMlist[X], MSEobj[[om]], Yrs=yrs))
          }

        } else {
          if (is.null(yrs)) {
            runPM[[X]] <- eval(call(PMlist[X], MSEobj[[om]], Ref=ref))
          } else {
            runPM[[X]] <- eval(call(PMlist[X], MSEobj[[om]], Ref=ref, Yrs=yrs))
          }
        }
      }
      OM_PMs[[om]] <- runPM
    }
  }

  nplots <- nPMs/2
  n.col <- ceiling(sqrt(nplots))
  n.row <- ceiling(nplots/n.col)

  m <- matrix(1:(n.col*n.row), ncol=n.col, nrow=n.row, byrow=FALSE)
  xmin <- xmax <- ymin <- ymax <- x <- y <- Class <- label <- fontface <- NULL
  plots <- listout <- list()

  xInd <- seq(1, by=2, length.out=nplots)
  yInd <- xInd + 1

  if (!(is.null(Title))) Title <- rep(Title, nplots)[1:nplots]

  for (pp in 1:nplots) {

    if (class(MSEobj)=='list') {
      dflist <- list(); xrect <- yrect <- list()
      for (om in 1:length(MSEobj)) {
        yPM <- PMlist[yInd[pp]]
        yvals <- OM_PMs[[om]][[match(yPM, PMlist)]]@Mean
        ycap <-  OM_PMs[[om]][[match(yPM, PMlist)]]@Caption
        yname <-  OM_PMs[[om]][[match(yPM, PMlist)]]@Name
        yline <- Lims[match(yPM, PMlist)]

        xPM <- PMlist[xInd[pp]]
        xvals <- OM_PMs[[om]][[match(xPM, PMlist)]]@Mean
        xcap <-  OM_PMs[[om]][[match(xPM, PMlist)]]@Caption
        xname <-  OM_PMs[[om]][[match(xPM, PMlist)]]@Name
        xline <- Lims[match(xPM, PMlist)]

        xlim <- c(0, max(max(xvals, 1)))
        ylim <- c(0, max(max(yvals, 1)))

        xrect[[om]] <- data.frame(xmin=0, xmax=xline, ymin=0, ymax=max(ylim))
        yrect[[om]] <- data.frame(xmin=0, xmax=max(xlim), ymin=0, ymax=yline)

        if(legend) {
          MPType <- MPtype(MSEobj[[om]]@MPs)
          Class <- MPType[match(MSEobj[[om]]@MPs, MPType[,1]),2]
          OM <- om
        } else {
          Class <- rep('', MSEobj[[om]]@nMPs)
          OM <- ''
        }

        if (class(MSEobj[[om]]) =='MSE') {
          labels <- MSEobj[[om]]@MPs
          if (class(Labels) == "list") {
            repnames <- names(Labels)
            invalid <- repnames[!repnames %in% labels]
            if (length(invalid >0)) {
              warning("Labels: ", paste(invalid, collapse=", "), " are not MPs in MSE")
              Labels[invalid] <- NULL
              repnames <- names(Labels)
            }
            labels[labels %in% repnames] <- Labels %>% unlist()
          }
        } else {
          labels <- runPM[[1]]@MPs

        }


        dflist[[om]] <- data.frame(x=xvals, y=yvals, label=labels, Class=Class,
                         pass=xvals>xline & yvals>yline, fontface="plain",
                         xPM=xPM, yPM=yPM, OM=om)
      }
      df <- do.call('rbind', dflist)
      df$OM <- as.factor(df$OM)

      xrect <- data.frame(xmin=0, xmax=max(unlist(lapply(xrect, '[[', 'xmax'))),
                          ymin=0, ymax=max(unlist(lapply(xrect, '[[', 'ymax'))))

      yrect <- data.frame(xmin=0, xmax=max(unlist(lapply(yrect, '[[', 'xmax'))),
                          ymin=0, ymax=max(unlist(lapply(yrect, '[[', 'ymax'))))
    } else {
      yPM <- PMlist[yInd[pp]]
      yvals <- runPM[[match(yPM, PMlist)]]@Mean
      ycap <-  runPM[[match(yPM, PMlist)]]@Caption
      yname <-  runPM[[match(yPM, PMlist)]]@Name
      yline <- Lims[match(yPM, PMlist)]

      xPM <- PMlist[xInd[pp]]
      xvals <- runPM[[match(xPM, PMlist)]]@Mean
      xcap <-  runPM[[match(xPM, PMlist)]]@Caption
      xname <-  runPM[[match(xPM, PMlist)]]@Name
      xline <- Lims[match(xPM, PMlist)]

      xlim <- c(0, max(max(xvals, 1)))
      ylim <- c(0, max(max(yvals, 1)))

      xrect <- data.frame(xmin=0, xmax=xline, ymin=0, ymax=max(ylim))
      yrect <- data.frame(xmin=0, xmax=max(xlim), ymin=0, ymax=yline)

      if(legend) {
        MPType <- MPtype(MSEobj@MPs)
        Class <- MPType[match(MSEobj@MPs, MPType[,1]),2]
      } else {
        Class <- rep('', MSEobj@nMPs)
      }

      if (class(MSEobj) =='MSE') {
        labels <- MSEobj@MPs
        if (class(Labels) == "list") {
          repnames <- names(Labels)
          invalid <- repnames[!repnames %in% labels]
          if (length(invalid >0)) {
            warning("Labels: ", paste(invalid, collapse=", "), " are not MPs in MSE")
            Labels[invalid] <- NULL
            repnames <- names(Labels)
          }
          labels[labels %in% repnames] <- Labels %>% unlist()
        }
      } else {
        labels <- runPM[[1]]@MPs

      }
      df <- data.frame(x=xvals, y=yvals, label=labels, Class=Class,
                     pass=xvals>xline & yvals>yline, fontface="plain",
                     xPM=xPM, yPM=yPM)
    }
    df$fontface <- as.character(df$fontface)
    df$fontface[!df$pass] <- "italic"
    df$fontface <- factor(df$fontface)

    listout[[pp]] <- df


    if (Satisficed) {
      xlim <- c(xline, 1)
      ylim <- c(yline, 1)
      plots[[pp]] <- ggplot2::ggplot()
    } else {
      plots[[pp]] <- ggplot2::ggplot() +
        ggplot2::geom_rect(data=xrect, ggplot2::aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=fill, alpha=alpha) +
        ggplot2::geom_rect(data=yrect, ggplot2::aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=fill, alpha=alpha)
    }

    if (Col == "Class") {
      plots[[pp]] <-  plots[[pp]] +
        ggplot2::geom_point(data=df, ggplot2::aes(x, y, shape=Class, color=Class), size=point.size, na.rm=TRUE)
      if (!is.null(lab.size))
        plots[[pp]] <-  plots[[pp]] +
          ggrepel::geom_text_repel(data=df, ggplot2::aes(x, y, color=Class, label=label,
                                                         fontface = fontface),
                                   show.legend=FALSE, size=lab.size, na.rm=TRUE)
    } else if (Col == "MP") {
      plots[[pp]] <-  plots[[pp]] +
        ggplot2::geom_point(data=df, ggplot2::aes(x, y, shape=Class, color=label), size=point.size, na.rm=TRUE)
      if (!is.null(lab.size))
        plots[[pp]] <-  plots[[pp]] +
          ggrepel::geom_text_repel(data=df, ggplot2::aes(x, y, color=label, label=label,
                                                         fontface = fontface),
                                   show.legend=FALSE, size=lab.size, na.rm=TRUE)
    } else if (Col == 'OM') {
      plots[[pp]] <-  plots[[pp]] +
        ggplot2::geom_point(data=df, ggplot2::aes(x, y, shape=OM, color=label), size=point.size, na.rm=TRUE)
      if (!is.null(lab.size))
        plots[[pp]] <-  plots[[pp]] +
          ggrepel::geom_text_repel(data=df, ggplot2::aes(x, y, color=label, label=label,
                                                         fontface = fontface),
                                   show.legend=FALSE, size=lab.size, na.rm=TRUE, max.overlaps = Inf)
    }
    if (Col == 'OM') {
      plots[[pp]] <-  plots[[pp]] +
        ggplot2::xlab(xcap) + ggplot2::ylab(ycap) +
        ggplot2::xlim(xlim) + ggplot2::ylim(c(0, xrect$ymax)) +
        ggplot2::theme_classic() +
        ggplot2::theme(axis.title = ggplot2::element_text(size=axis.title.size),
                       axis.text = ggplot2::element_text(size=axis.text.size),
                       legend.text=ggplot2::element_text(size=legend.title.size),
                       legend.title = ggplot2::element_text(size=legend.title.size)) +
        ggplot2::labs(shape= "OM Set") +
        ggplot2::guides(color='none')
    } else {
      plots[[pp]] <-  plots[[pp]] +
        ggplot2::xlab(xcap) + ggplot2::ylab(ycap) +
        ggplot2::xlim(xlim) + ggplot2::ylim(ylim) +
        ggplot2::theme_classic() +
        ggplot2::theme(axis.title = ggplot2::element_text(size=axis.title.size),
                       axis.text = ggplot2::element_text(size=axis.text.size),
                       legend.text=ggplot2::element_text(size=legend.title.size),
                       legend.title = ggplot2::element_text(size=legend.title.size)) +
        ggplot2::labs(shape= "MP Type", color="MP Type")
    }

    if (Col == "Class") {
      plots[[pp]] <-  plots[[pp]] +  ggplot2::scale_colour_manual(values=cols)
    } else if (Col == "MP") {
      plots[[pp]] <-  plots[[pp]] +  ggplot2::scale_colour_manual(values=cols) +  ggplot2::guides(color='none')
    }

    if (!is.null(Title))
      plots[[pp]] <-  plots[[pp]] + ggplot2::labs(title=Title[pp])

    if (legend==FALSE)
      plots[[pp]] <-  plots[[pp]] + ggplot2::theme(legend.position="none")

  }

  out <- do.call("rbind", listout)
  tab <- table(out$label, out$pass)
  passall <- rownames(tab)[tab[,ncol(tab)] == nplots]
  if (class(MSEobj)=='MSE') {
    Results <- summary(MSEobj, PMlist, silent=TRUE, Refs=Refs)
    Results$Satisificed <- FALSE
    Results$Satisificed[match(passall, Results$MP)] <- TRUE
    Results <- Results[,unique(colnames(Results))]
  } else {
    Results <- 'Summary table of results only available for objects of class `MSE`'
  }

  out <- list(Results=Results, Plots=plots)
  if (Show == "plots") {
    join_plots(plots, n.col, n.row,  position = position, legend=legend)
  } else if (Show == "table") {
    print(Results)
  } else if (Show == "none") {
    return(invisible(out))
  } else {
    join_plots(plots, n.col, n.row,  position = position, legend=legend)
    if (class(MSEobj) =='MSE') print(Results)
  }
  invisible(out)
}
