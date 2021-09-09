
saveOM <- function(OM.dir, nsim=48, proyears=30) {
  replist <- r4ss::SS_output(OM.dir)
  MOM <- SS2MOM(replist, nsim=nsim, proyears=proyears)
  OM <- SSMOM2OM(MOM, SSdir=replist)
  OM@cpars$Data <- SWOMSE::SWOData
  OM@cpars$control$CAL <- 'removals'

  # M
  txt <- strsplit(OM.dir, '-M')[[1]][2]
  M <- strsplit(txt, '_')[[1]][1] %>% as.numeric()

  # sigmaR
  txt <- strsplit(OM.dir, '_sigmaR')[[1]][2]
  sigmaR <- strsplit(txt, '_')[[1]][1] %>% as.numeric()

  # Steep
  txt <- strsplit(OM.dir, 'steepness')[[1]][2]
  h <- strsplit(txt, '_cpue')[[1]][1] %>% as.numeric()

  # CPUE lambda
  txt <- strsplit(OM.dir, 'cpuelambda')[[1]][2]
  lambda <- strsplit(txt, '_llq')[[1]][1] %>% as.numeric()

  # Q
  txt <- strsplit(OM.dir, 'llq')[[1]][2]
  q <- strsplit(txt, '_env')[[1]][1] %>% as.numeric()

  # ENV
  txt <- strsplit(OM.dir, 'env')[[1]][2]
  env <- strsplit(txt, '_')[[1]][1] %>% as.numeric()
  if (env <1) {
    env <- 0
  } else {
    env <- 1
  }
  OM@Name <- paste('M', M,
                   'sigmaR', sigmaR,
                   'h', h,
                   'CPUE', lambda,
                   'Q', q,
                   'ENV', env,
                   sep='-')
  MOM@Name <- OM@Name


  saveRDS(OM, file.path(OM.dir, 'OM.rda'))
  saveRDS(MOM, file.path(OM.dir, 'MOM.rda'))
}

saveOMs <- function(grid.dir, nsim=48, proyears=30) {
  dirs <- list.dirs(grid.dir, recursive = FALSE)
  ndirs <- length(dirs)
  setup()
  sfExport('saveOM')
  sfLibrary("SWOMSE", character.only = TRUE, verbose=FALSE)
  snowfall::sfSapply(1:ndirs, function(i)
    saveOM(dirs[i], nsim=nsim, proyears=proyears)
    )
}

# saveOMs('G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/grid_2021/grid_May2021')
# saveOMs('G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/grid_2021/grid_May2021_shifted')


