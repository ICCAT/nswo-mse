
# These functions are used internally in the package to import SS OMs
# and assign as data objects in the SWOMSE R package

# ImportAll() # import all SS OMs and save as .rda data files
# BuildOM() # join all imported SS OMs into a single OM
# BuildData(redoOM=FALSE, redoMSE=FALSE) # add Data file to data directory
# document_data() # add help files for all Data files


#' Document the Data files
#'
#' @return nothing
#' @export
#' @importFrom stringr str_extract_all
#' @keywords internal
document_data <- function() {
  RoxygenFile <- "Roxy_DataObjects.r" # name of R script with roxygen
  file.remove(file.path('R/', RoxygenFile)) # delete
  file.create(file.path('R/', RoxygenFile)) # make empty file

  cat("# This file is automatically built by document_data in buildPackage.R\n",
      "# Don't edit by hand!\n",
      "# \n\n", sep="", append=TRUE,
      file=file.path('R/', RoxygenFile))

  datfiles <- list.files("data")
  OMdat <- datfiles[grepl("OM", datfiles)]
  nums <- as.numeric(stringr::str_extract_all(OMdat, "[0-9]+"))
  nums[is.na(nums)] <- sum(is.na(nums)):1
  OMdat <- OMdat[order(nums)]
  nums2 <- sort(nums)
  # OM Data
  cat("#' @name SWO-OMs",
      "\n#' @docType data",
      "\n#' @title Operating models",
      "\n#' @description Swordfish operating models.",
      "\n#'  ",
      "\n#' `SWOM` is a single OM combining all SS OMs including `OM_base_case` and",
      "\n#' all SS OMs there were imported into the package (`OM_19`, `OM_20`, ..., `OM_288`)",
      '\nNULL',
      "\n\n\n",
      sep="", append=TRUE,
      file=file.path('R/', RoxygenFile))

  for (x in seq_along(OMdat)) {
    Name <- strsplit(OMdat[x], ".rda")[[1]]
    cat("#' @rdname SWO-OMs", '\n"', Name,  '"\n\n\n', sep="", append=TRUE,
        file=file.path('R/', RoxygenFile))
  }


  cat("#' @name SWO_Data",
      "\n#' @docType data",
      "\n#' @title Operating models",
      "\n#' @description Swordfish Data Object",
      "\n#'  ",
      '\n "SWOData"',
      "\n\n\n",
      sep="", append=TRUE,
      file=file.path('R/', RoxygenFile))

  cat("#' @name SWO_MSE",
      "\n#' @docType data",
      "\n#' @title Example Swordish MSE",
      "\n#' @description Example Swordish MSE Object",
      "\n#'  ",
      '\n "SWO_MSE"',
      "\n\n\n",
      sep="", append=TRUE,
      file=file.path('R/', RoxygenFile))

  cat("#' @name OMs_DF",
      "\n#' @docType data",
      "\n#' @title Overview of OM Parameters",
      "\n#' @description A dataframe of OM Parameters",
      "\n#'  ",
      '\n "OMs_DF"',
      "\n\n\n",
      sep="", append=TRUE,
      file=file.path('R/', RoxygenFile))

  cat("#' @name Like_List",
      "\n#' @docType data",
      "\n#' @title OM Likelihoods",
      "\n#' @description A list of likelihood values for each OM",
      "\n#'  ",
      '\n "Like_List"',
      "\n\n\n",
      sep="", append=TRUE,
      file=file.path('R/', RoxygenFile))

  cat("#' @name Fleet_DF",
      "\n#' @docType data",
      "\n#' @title Fleet Information",
      "\n#' @description A dataframe of information on fleet names",
      "\n#'  ",
      '\n "Fleet_DF"',
      "\n\n\n",
      sep="", append=TRUE,
      file=file.path('R/', RoxygenFile))

  cat("#' @name Data_DF",
      "\n#' @docType data",
      "\n#' @title Data Information",
      "\n#' @description A dataframe of fishery data",
      "\n#'  ",
      '\n "Data_DF"',
      "\n\n\n",
      sep="", append=TRUE,
      file=file.path('R/', RoxygenFile))

  cat("#' @name CPUE_List",
      "\n#' @docType data",
      "\n#' @title CPUE Fitting Information",
      "\n#' @description A list of fitted CPUE indices for each OM",
      "\n#'  ",
      '\n "CPUE_List"',
      "\n\n\n",
      sep="", append=TRUE,
      file=file.path('R/', RoxygenFile))

  cat("#' @name TSBio_List",
      "\n#' @docType data",
      "\n#' @title Time-Series Data of Predicted Spawning Biomass and Recruitment",
      "\n#' @description A list of predicted spawning biomass and recruitment for each OM",
      "\n#'  ",
      '\n "TSBio_List"',
      "\n\n\n",
      sep="", append=TRUE,
      file=file.path('R/', RoxygenFile))

  cat("#' @name RefPoint_DF",
      "\n#' @docType data",
      "\n#' @title Dataframe of Reference Points for each OM",
      "\n#' @description None converged OMs have Depletion=NA",
      "\n#'  ",
      '\n "RefPoint_DF"',
      "\n\n\n",
      sep="", append=TRUE,
      file=file.path('R/', RoxygenFile))

  cat("#' @name Select_List",
      "\n#' @docType data",
      "\n#' @title List of estimated selectivity-at-size for each OM",
      "\n#' @description ",
      "\n#'  ",
      '\n "Select_List"',
      "\n\n\n",
      sep="", append=TRUE,
      file=file.path('R/', RoxygenFile))

  cat("#' @name Catch_List",
      "\n#' @docType data",
      "\n#' @title List of observed and predicted catch for each OM",
      "\n#' @description ",
      "\n#'  ",
      '\n "Catch_List"',
      "\n\n\n",
      sep="", append=TRUE,
      file=file.path('R/', RoxygenFile))

}

# AverageC <- function(x, Data, reps) {
#   Rec <- new("Rec")
#   Rec@TAC <- rep(mean(Data@Cat[x,], na.rm = TRUE), reps)
#   Rec
# }
# class(AverageC) <- "MP"

#' Add the SWO Data Excel file and OM Doc file to the package
#'
#' @param redoPlotOM Logical. Re-build `plot(SWOM)`?
#' @param redoMSE Logical. Re-build MSE object?
#' @return nothing
#' @export
#' @keywords internal
#' @importFrom dplyr %>% filter summarise mutate group_by select summarize
#' @importFrom usethis use_data
BuildData <- function(redoPlotOM=FALSE, redoMSE=FALSE) {
  SWOData <- DLMtool::XL2Data("inst/SWO_Data")
  usethis::use_data(SWOData, overwrite = TRUE)

  # copy user manual to doc folder
  file.copy("doc/userguide.html", "inst/userguide.html", overwrite =TRUE)

  if (redoPlotOM)
    plot(SWOM, output_dir="inst", open=FALSE)

  if (redoMSE) {
    MPs <- c("AverageC", "ConstC", "Itarg1", "Itarg2",
             "FMSYref", "SP_4010") # the MPs that will be run
    SWOM@cpars$Data <- SWOData # for clarity, re-add SWOData to the OM

    SWO_MSE <- runMSE(SWOM, MPs=MPs, parallel = TRUE)
    usethis::use_data(SWO_MSE, overwrite = TRUE)
  }


}


Names <- c(".", "Age", 'OM_base_case', "Gender", "Len_Mid", "Wt_Mid",
           "N", "M", "seas1_aind", "SpawnBio", 'SWOM', 'SWOData', "Year", "Z")
utils::globalVariables(Names)


#' Import a SS OM and save to package data directory
#'
#' @param SSdir File path to SS output files
#' @param Gen Gen Genders to import. Numeric. `Gen=1` for first gender (usually female),
#'  `Gen=2` for second gender (usually male), etc. `Gen=1:2` (default) uses average from both genders
#' @param overwrite Logical. Overwrite existing files?
#'
#' @return nothing
#' @keywords internal
#'
ImportSSOM <- function(SSdir, Gen=1:2, overwrite=TRUE) {
  try <- try(SWO_SS2OM(SSdir=SSdir, nsim=2, Gen=Gen,
                       Obs = DLMtool::Perfect_Info,
                       Imp = DLMtool::Perfect_Imp,
                       proyears=30), silent=TRUE)
  if (class(try) == "OM") {
    name <- basename(SSdir)
    try@Name <- name
    name <- strsplit(name, split = "-", perl = TRUE)[[1]][1]
    name <- paste0("OM_", name)
    assign(name, try)
    save(list=name, file=file.path("data", paste0(name, '.rda')))
  }
}




#' Loop over directories and import SS OMs
#'
#' @param rootdir root directory containing sub-directories with SS output files
#' @param Gen Gen Genders to import. Numeric. `Gen=1` for first gender (usually female),
#'  `Gen=2` for second gender (usually male), etc. `Gen=1:2` (default) uses average from both genders
#' @param overwrite Logical. Overwrite existing files?
#'
#' @return nothing
#' @export
#' @keywords internal
ImportAll <- function(rootdir='C:/Users/Adrian/Dropbox/SWO_MSE/OMs/SS/2018_GRID',
                      Gen=1:2, overwrite=TRUE) {
  SSdirs <- list.dirs(rootdir)
  # Loop over dirs and import SS OMs if possible
  for (i in 1:length(SSdirs)) {
    message(i, "/", length(SSdirs))
    ImportSSOM(SSdir=SSdirs[i], Gen=Gen, overwrite = overwrite)
  }
}


#' Join all SS OMs into a single OM
#'
#' @export
#' @keywords internal
#'
BuildOM <- function() {
  fls <- list.files("data", pattern="OM_")

  nums <- strsplit(fls, split = "OM_") %>%
    lapply(., "[", 2) %>% unlist() %>%
    strsplit(., split = ".rda") %>% unlist() %>%
    as.numeric()

  names <- strsplit(fls, split = ".rda") %>% unlist()
  names <- names[order(nums)]
  names <- names[!names =="OM_base_case"]
  names <- c("OM_base_case", names)

  OMlist <- list()
  for (i in 1:length(names)) {
    OMlist[[i]] <- get(names[i])
  }

  baseOM <- OM_base_case
  SWOM <- baseOM

  SWOM@nsim <- length(fls)
  cnames <- names(baseOM@cpars)
  SWOM@cpars <- list()

  for (nm in cnames) {
    temp <- extract(OMlist, nm)
    check(temp, OMlist, nm)
    SWOM@cpars[[nm]] <- temp
  }

  SWOM@cpars$M_ageArray <- array(NA, dim=c(SWOM@nsim, SWOM@maxage, SWOM@nyears+SWOM@proyears))
  SWOM@cpars$D <- SWOM@cpars$AC <-  rep(NA, SWOM@nsim)
  SWOM@cpars$R0 <- rep(0, SWOM@nsim)
  for (i in 1:length(OMlist)) {
    SWOM@cpars$M_ageArray[i,,] <- matrix(OMlist[[i]]@M, nrow=SWOM@maxage, ncol=SWOM@nyears+SWOM@proyears)
    SWOM@cpars$D[i] <- OMlist[[i]]@D[1]
    SWOM@cpars$AC[i] <- OMlist[[i]]@AC[1]
    SWOM@cpars$R0[i] <- OMlist[[i]]@R0[1]
  }

  SWOM@M <- SWOM@M2 <- SWOM@h <- SWOM@Perr <- SWOM@AC <- SWOM@D <-SWOM@R0 <- c(0,0)
  SWOM@Esd <- SWOM@qcv <- SWOM@L5 <- SWOM@LFS <- SWOM@Vmaxlen <- c(0,0)
  SWOM@EffLower <- c(0,0)
  SWOM@EffUpper <- c(0,0)

  SWOM <- DLMtool::Replace(SWOM, DLMtool::Perfect_Info)
  SWOM <- DLMtool::Replace(SWOM, DLMtool::Perfect_Imp)
  SWOM@Source <- names
  SWOM@Name <- "SWO All OMs"
  SWOM@interval <- 1
  usethis::use_data(SWOM, overwrite = TRUE)
}










extract <- function(OMlist, cpar) {
  list <- lapply(lapply(OMlist, slot, name="cpars"), "[[", cpar)
  if (class(list[[1]])=="array") {
    fun <- function(i, list) list[[i]][1,,]
    list <- lapply(1:length(list), fun, list=list)
    arr <- array(as.numeric(unlist(list)),
                 dim=c(OMlist[[1]]@maxage,
                       OMlist[[1]]@nyears+OMlist[[1]]@proyears,
                       length(OMlist)))
    arr <- aperm(arr, c(3,1,2))
    return(arr)
  }
  if (class(list[[1]])=="matrix") {
    fun <- function(i, list) list[[i]][1,]
    list <- lapply(1:length(list), fun, list=list)
    if (cpar=="Perr_y") {
      dim <- c(OMlist[[1]]@maxage + OMlist[[1]]@nyears+OMlist[[1]]@proyears-1, length(OMlist))
    } else {
      dim <- c(OMlist[[1]]@nyears, length(OMlist))
    }
    arr <- array(as.numeric(unlist(list)), dim=dim)
    arr <- aperm(arr, c(2,1))
    return(arr)
  }
  if (class(list[[1]])=="numeric") {
    arr <- lapply(list, '[[', 1) %>% unlist()
    return(arr)
  }

}

check <- function(array, OMlist, cpar) {
  for (i in 1:length(OMlist)) {
    if (class(array) == "array") {
      p <- prod(OMlist[[i]]@cpars[[cpar]][1,,] == array[i,,])
    }
    if (class(array) == "numeric") {
      p <- prod(OMlist[[i]]@cpars[[cpar]][1] == array[i])
    }
    if (class(array) == "matrix") {
      p <- prod(OMlist[[i]]@cpars[[cpar]][1,] == array[i,])
    }
    if (p !=1) stop()
  }
}



#' Import SS objects and save to root
#'
#' @param OM.dirs
#'
#' @return Nothing
#' @export
#'
#' @keywords internal
saveTempObject <- function(OM.dirs) {
  DFlist <- DataList <- RepList <- LHlist <- list()
  for (i in seq_along(OM.dirs)) {
    message(i, '/', length(OM.dirs))
    replist <- suppressWarnings(r4ss::SS_output(OM.dirs[i], verbose = FALSE,
                                                hidewarn = TRUE,
                                                printstats=FALSE))
    data <- r4ss::SS_readdat(file.path(OM.dirs[i], 'data.ss_new'), version='3.24',
                             verbose = FALSE)
    # Natural mortality
    tt <- replist$M_at_age %>% filter(Year <= replist$endyr)
    M <- unique(tt[,4])
    M <- M[!is.na(M)]

    # sigma R
    sigmaR <- replist$sigma_R_in

    # steepness
    h <- replist$parameters[grepl("steep", rownames(replist$parameters)), ]$Value

    # cpue cv
    cpue_cv <- data$CPUE$se_log %>% unique()

    # ess
    L_ESS <- data$lencomp$Nsamp %>% unique()

    # llq
    txt <- strsplit(OM.dirs[i], 'llq')[[1]][2]
    llq <- strsplit(txt, '_env')[[1]][1] %>% as.numeric()

    # env
    est_pars <- replist$estimated_non_dev_parameters
    nms <- rownames(est_pars)
    env <- FALSE
    ind <- which(grepl('Q_envlink', rownames(est_pars)))
    if (length(ind)>0) {
      flts_env <- gsub('Q_envlink_[[:digit:]]+_', '', nms[ind])
      env <- TRUE
    }

    # converge
    log_det_hessian <- replist$log_det_hessian
    if (log_det_hessian>0) conv <- TRUE
    if (log_det_hessian==-1) conv <- FALSE

    LHlist[[i]] <- list(replist$likelihoods_used %>% t() %>% data.frame(),
                        replist$likelihoods_by_fleet)

    DFlist[[i]] <- data.frame(M=M, sigmaR=sigmaR, h=h, cpue_cv=cpue_cv,
                              L_ESS=L_ESS, llq=llq, env=env, dir=basename(OM.dirs[i]),
                              n=i, conv=conv)

    RepList[[i]] <- replist
    DataList[[i]] <- data

  }
  DF <- do.call('rbind', DFlist)
  saveRDS(DF, 'OM_objects/OM_DF.rda')
  saveRDS(LHlist, 'OM_objects/LHlist.rda')
  saveRDS(DataList, 'OM_objects/DataList.rda')
  saveRDS(RepList, 'OM_objects/RepList.rda')
}



# Compare <- function(OM, replist) {
#   OM@nsim <- 2
#   Hist <- runMSE(OM, Hist=TRUE, silent=TRUE, control=control)
#
#   simNAA <- Hist@AtAge$Nage[1,,]
#
#   natage <- replist$natage_annual_2_with_fishery  %>%
#     group_by(Year) %>%
#     tidyr::gather(., "Age", "N", 4:ncol(replist$natage_annual_2_with_fishery)) %>%
#     mutate(Age=as.numeric(Age)) %>%
#     group_by(Year, Age) %>%
#     filter(Age>0) %>%
#     summarise(N=sum(N))
#
#   ages <- 1:OM@maxage
#
#   # Compare N-at-age
#   yrs <- c(1, OM@nyears)
#   Years <- (OM@CurrentYr-OM@nyears+1):OM@CurrentYr
#
#   par(mfrow=c(2,3))
#   for (yr in yrs) {
#     Ns <- natage %>% filter(Year %in% Years[yr], Age>0)
#     plot(Ns$Age, Ns$N, xlab="Age", ylab="N", main=paste('Year = ', yr), bty="n")
#     lines(ages, simNAA[,yr])
#   }
#
#
#   # Compare Total N
#   Ns <- natage %>% group_by(Year) %>% summarise(N=sum(N)) %>%
#     filter(Year %in% Years)
#   simN <- apply(simNAA, 2, sum)
#   plot(Ns, xlab="Year", ylab="Total N",
#        ylim=c(0, max(max(Ns$N), max(simN))), bty="n")
#
#   lines(Years, simN)
#
#
#   # Compare Total Biomass
#   bio <- replist$timeseries %>% group_by(Yr) %>% summarise(bio=sum(Bio_all)) %>%
#     filter(Yr %in% Years)
#   simbio <-Hist@TSdata$B
#   plot(bio, xlab="Year", ylab="Total Biomass",
#        ylim=c(0, max(max(bio$bio), max(simbio))), bty="n")
#   matplot(Years, t(simbio), type="l", add=TRUE)
#
#   # Compare Spawning Biomass
#   spbio <- replist$timeseries %>% group_by(Yr) %>% summarise(bio=sum(SpawnBio)) %>%
#     filter(Yr %in% Years)
#   spbio$bio <- spbio$bio/spbio$bio[1]
#   simsbbio <-Hist@TSdata$SSB[1,]
#   simsbbio <- simsbbio/simsbbio[1]
#   plot(spbio, xlab="Year", ylab="Spawning Depletion",
#        ylim=c(0, max(max(spbio$bio), max(simsbbio))), bty="n")
#   lines(Years, simsbbio, type="l")
#   abline(h=replist$current_depletion, lty=3)
#
#
#   # Compare Total Catch
#   timeseries <- replist$catch
#   catch <- timeseries %>% group_by(Yr) %>% summarise(catch=sum(Exp)) %>%
#     filter(Yr %in% Years)
#   catch_obs <- timeseries %>% group_by(Yr) %>% summarise(catch=sum(Obs)) %>%
#     filter(Yr %in% Years)
#   simcatch <- Hist@TSdata$Catch
#
#   plot(catch, xlab="Year", ylab="Total Catch",
#        ylim=c(0, max(max(catch), max(simcatch))), bty="n")
#   # lines(Years, catch_obs$catch, col='red')
#
#   matplot(Years, t(simcatch), type="l", add=TRUE)
#
#   # Compare Vulnerable Numbers
#
#
# }
