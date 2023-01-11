
library(dplyr)
library(r4ss)

# don't re-run as the models have now been re-conditioned using
# the Francis iterative re-weighting procedure
#
# This script does not do that and will overwrite those results!!

stop()

OM.grid.dir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/grid_2022'

# ---- Base Case - 2022 Assessment ----
# 2022 NSWO Assessment Base Case Model:
source.dir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/Base_v5'

base.dir <- file.path(OM.grid.dir, '000_base_case')
files <- c('ss.par', 'forecast.ss', 'starter.ss', 'control.ss_new', 'SWOv5.dat', 'ss3.exe')

if (!dir.exists(base.dir))
    dir.create(base.dir)
file.copy(file.path(source.dir, files), file.path(base.dir, files), overwrite = TRUE)


# Modify forecast.ss so MSY ref points are calculated
forecast <- r4ss::SS_readforecast(file.path(base.dir, 'forecast.ss'))
forecast$MSY <- 2
r4ss::SS_writeforecast(forecast, base.dir, overwrite = TRUE)


# ---- Condition OM Grid ----
scenarios <- list(
  # M
  M=c(0.1,0.2,0.3),
  # sigmaR
  sigmaR=c(0.2, 0.6),
  # steepness
  steepness=c(0.6, 0.75, 0.9),
  #CPUE lambda
  cpuelambda= c(0.05,1,20),
  # CPUE LL q
  llq=c(1, 1.01),
  #environmental effects
  env=c(-5,7)
) # }}}

grid <- expand.grid(scenarios,stringsAsFactors=FALSE)


# ---- Create grid SS3 folders ----
create_grid_folders <- function(i, grid, OM.grid.dir, base.dir) {

  # read source files
  dat <- r4ss::SS_readdat_3.30(file.path(base.dir, 'SWOv5.dat'), verbose = FALSE)
  ctl <- r4ss::SS_readctl_3.30(file.path(base.dir, 'control.ss_new'), datlist=dat, verbose = FALSE)

  # M
  ind <- grepl('NatM', rownames(ctl$MG_parms)) %>% which()
  ctl$MG_parms[ind[1], c("INIT", "PRIOR")] <- grid$M[i]
  ctl$MG_parms[ind[2], c("INIT", "PRIOR")] <- grid$M[i]

  # sigmaR
  ctl$SR_parms['SR_sigmaR',c("INIT", "PRIOR")] <- grid$sigmaR[i]

  # steepness
  ctl$SR_parms["SR_BH_steep",c('INIT', 'PRIOR')] <- grid$steepness[i]

  # cpuelambda
  ind <- grepl('Surv', rownames(ctl$lambdas)) %>% which()
  lambdas <- ctl$lambdas[ind,"value"]
  lambdas[lambdas==1] <- grid$cpuelambda[i]
  ctl$lambdas[ind,"value"] <- lambdas

  # llq
  dat$CPUE <- dat$CPUE %>%
    group_by(index) %>%
    mutate(n.yr=length(obs)) %>%
    mutate(obs=obs/grid$llq[i]^(0:(unique(n.yr)-1)))
  dat$CPUE$n.yr <- NULL
  dat$CPUE <- as.data.frame(dat$CPUE)

  # env
  vals <-  ctl$Q_parms_tv[, 'PHASE']
  vals[vals>0] <- grid$env[i]
  ctl$Q_parms_tv[, 'PHASE'] <- vals

  # adjust bounds on R0
  ctl$SR_parms[1,'LO'] <- 5
  ctl$SR_parms[1,'HI'] <- 9

  # create directory
  i_char <- as.character(i)
  if (nchar(i_char)==1) i_char <- paste0('00', i_char)
  if (nchar(i_char)==2) i_char <- paste0('0', i_char)

  nm <- paste0(names(grid), grid[i,], collapse="_")
  dir <- paste(i_char, nm, sep="_")
  dir.create(file.path(OM.grid.dir, dir))

  # modify starter.ss
  starter <- r4ss::SS_readstarter(file.path(base.dir, 'starter.ss'),verbose = FALSE)
  starter$ctlfile <- 'control.ss'
  starter$datfile <- 'SWOdat.dat' # indices will differ based on llq
  starter$init_values_src <- 0 # use control file for initial values
  starter$run_display_detail <- 0
  starter$cumreport <- 0

  # write the modified files
  SS_writectl_3.30(ctl, file.path(OM.grid.dir, dir, 'control.ss'), verbose=F)
  SS_writedat_3.30(dat, file.path(OM.grid.dir, dir, 'SWOdat.dat'), verbose=F)
  SS_writestarter(starter, file.path(OM.grid.dir, dir), verbose=F)

  # copy over other files
  file.copy(file.path(base.dir, "forecast.ss"),
            file.path(OM.grid.dir, dir, "forecast.ss"))

  file.copy(file.path(base.dir, "ss3.exe"),
            file.path(OM.grid.dir, dir, "ss3.exe"))

  invisible(file.path(OM.grid.dir, dir))
}

sapply(1:nrow(grid), create_grid_folders,
       grid=grid, OM.grid.dir=OM.grid.dir, base.dir=base.dir)

# --- Run the models ----
library(snowfall)
cpus <- parallel::detectCores(logical=FALSE)
snowfall::sfInit(parallel=TRUE,cpus=cpus)
sfLibrary("dplyr", character.only = TRUE, verbose=FALSE)


run_ss_grid <- function(dir) {
  setwd(dir)
  system2('ss3.exe', stdout = FALSE, stderr = FALSE)
}

OM.dirs <- list.dirs(OM.grid.dir, recursive = FALSE)

# Split into groups (system seems to start to lag if I run the entire set in one go)

OM.dir.list <- split(OM.dirs, ceiling(seq_along(OM.dirs)/cpus))

for (i in 1:length(OM.dir.list)) {
  message(i,'/', length(OM.dir.list))
  sfClusterApplyLB(OM.dir.list[[i]], run_ss_grid)

}

# --- Clean up ----

for (d in OM.dirs) {
  if (file.exists(file.path(d, 'ss3.exe')))
    file.remove(file.path(d, 'ss3.exe'))
}

