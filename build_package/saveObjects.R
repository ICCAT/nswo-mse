library(dplyr)

readSS <- function(SS.dir, i='base_case') {

  replist <- suppressWarnings(r4ss::SS_output(SS.dir, verbose = FALSE,
                                              hidewarn = TRUE,
                                              printstats=FALSE))

  if (i =='base_case') {
    data <- r4ss::SS_readdat(file.path(SS.dir, 'data.ss_new'), version='3.24',
                             verbose = FALSE)
  } else {
    data <- r4ss::SS_readdat(file.path(SS.dir, 'data.ss_new'), version='3.30',
                             verbose = FALSE)
  }


  # Natural mortality
  if (i =='base_case') {
    tt <- replist$M_at_age %>% filter(Year <= replist$endyr)
  } else {
    tt <- replist$M_at_age %>% filter(Yr <= replist$endyr)
  }

  M <- unique(tt[,4])
  M <- M[!is.na(M)]

  # sigma R
  sigmaR <- replist$sigma_R_in

  # steepness
  h <- replist$parameters[grepl("steep", rownames(replist$parameters)), ]$Value

  # cpue cv
  cpue_cv <- data$CPUE %>% filter(year>0, year<= replist$endyr) %>%
    group_by(index, year) %>% summarise(se_log=unique(se_log)) %>%
    dplyr::ungroup() %>%
    dplyr::select(se_log) %>% dplyr::distinct()

  if (nrow(cpue_cv)>1) {
    rng <- range(cpue_cv$se_log)
    cpue_cv <- cpue_cv %>% dplyr::filter(se_log %in% rng) %>% dplyr::distinct()
  }

  # cpue lambda
  txt <- strsplit(SS.dir, 'cpuelambda')[[1]][2]
  lambda <- strsplit(txt, '_llq')[[1]][1] %>% as.numeric()

  # ess
  L_ESS <- data$lencomp$Nsamp %>% unique()

  # llq
  txt <- strsplit(SS.dir, 'llq')[[1]][2]
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

  return(list(LH=list(replist$likelihoods_used %>% t() %>% data.frame(),
                      replist$likelihoods_by_fleet),
              DF=data.frame(M=M, sigmaR=sigmaR, h=h, cpue_cv=cpue_cv,lambda=lambda,
                            L_ESS=L_ESS, llq=llq, env=env, dir=basename(SS.dir),
                            n=i, conv=conv),
              replist=replist,
              data=data))
}

saveGridObjects <- function(OM.dirs) {
  DFlist <- DataList <- RepList <- LHlist <- list()

  for (i in seq_along(OM.dirs)) {
    message(i, '/', length(OM.dirs))

    run <- readSS(OM.dirs[i],i=i)
    DFlist[[i]] <- run$DF
    DataList[[i]] <- run$data
    RepList[[i]] <- run$replist
    LHlist[[i]] <- run$LH
  }
  DF <- do.call('rbind', DFlist)
  saveRDS(DF, paste0(OM.root, '/OM_objects/OM_DF.rda'))
  saveRDS(LHlist, paste0(OM.root, '/OM_objects/LHlist.rda'))
  saveRDS(DataList, paste0(OM.root, '/OM_objects/DataList.rda'))
  saveRDS(RepList, paste0(OM.root, '/OM_objects/RepList.rda'))
}

saveBaseCaseObjects <- function(OMbase.dir) {

  base.case <- readSS(OMbase.dir)
  saveRDS(base.case$DF, paste0(OM.root, '/OM_objects/basecase_DF.rda'))
  saveRDS(base.case$LH, paste0(OM.root, '/OM_objects/basecase_LH.rda'))
  saveRDS(base.case$data, paste0(OM.root, '/OM_objects/basecase_data.rda'))
  saveRDS(base.case$replist, paste0(OM.root, '/OM_objects/basecase_replist.rda'))
}




OM.root <- 'G:/My Drive/1_Projects/North_Atlantic_Swordfish/OMs/grid_2021'
OMgrid.dir <- file.path(OM.root, "grid_Apr2021")
OMgrid.dirs <- list.dirs(OMgrid.dir, recursive = FALSE)

ord <- lapply(strsplit(OMgrid.dirs, 'iter'), '[[', 2) %>% as.numeric() %>% order()
OMgrid.dirs <- OMgrid.dirs[ord]


saveGridObjects(OMgrid.dirs)


OMbase.dir <-  file.path(OM.root, "Michael_March2020/NSWO_MSE_SS3_Base_v2")
saveBaseCaseObjects(OMbase.dir)

