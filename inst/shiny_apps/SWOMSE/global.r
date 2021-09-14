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
# saveRDS(DF_all, 'inst/shiny_apps/SWOMSE/data/OMTable.rds')



# OM_Axes <- readRDS('inst/shiny_apps/SWOMSE/data/OMTable.rds')
OM_Axes <- readRDS('data/OMTable.rds')
OM_Axes$incE <- as.logical(OM_Axes$incE)

OM_Axes$incQ[OM_Axes$incQ == 1] <- FALSE
OM_Axes$incQ[OM_Axes$incQ>1] <- TRUE
OM_Axes$incQ <- as.logical(OM_Axes$incQ)

OMnames <- OM_Axes$Name

PMnames <- avail("PM")

joinMSE2 <- function (MSEobjs = NULL)
{
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
