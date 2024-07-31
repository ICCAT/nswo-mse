
library(dplyr)
library(r4ss)

# --- Run the Reference Models ----
OM.grid.dir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/2024_OMs/Reference'
ss3_exec <- 'ss3_opt.exe'
ss3_dir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/2024_OMs'
#
# library(snowfall)
# cpus <- parallel::detectCores(logical=FALSE)
# snowfall::sfInit(parallel=TRUE,cpus=cpus)
# sfLibrary("dplyr", character.only = TRUE, verbose=FALSE)
# snowfall::sfExport(list=c('ss3_dir', 'ss3_exec'))

run_ss_grid <- function(dir) {
  file.copy(file.path(ss3_dir, ss3_exec),
            file.path(dir, ss3_exec))
  setwd(dir)
  system2(ss3_exec, stdout = FALSE, stderr = FALSE)
  file.remove(file.path(dir, ss3_exec))
}

OM.dirs <- list.dirs(OM.grid.dir, recursive = FALSE)

for (i in 1:length(OM.dirs)) {
  message(i,'/', length(OM.dirs))
  run_ss_grid(OM.dirs[i])
}

## UNPAUSE G DRIVE ###

# ---- Robustness OMs ----

## R1 - Catchability increase 1%
source_dir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/2024_OMs/Reference/005_M0.2_sigmaR0.2_steepness0.80_cpuelambda1_llq1_env7'
dest_dir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/2024_OMs/Robustness/R1_Increasing_q/010_M0.2_sigmaR0.2_steepness0.80_cpuelambda1_llq1.01_env7'

# copy over files
source_files <- list.files(source_dir)

file.copy(file.path(source_dir, source_files),
          file.path(dest_dir, source_files),
          overwrite = TRUE
)


# modify data CPUE
data_file <- 'SWOv8.dat'
data_source <- r4ss::SS_readdat(file.path(dest_dir, data_file))

mod_cpue <- function(obs) {
  n <- length(obs)-1
  obs/1.01^(0:n)
}

cpue_mod <- data_source$CPUE |> dplyr::group_by(index) |>
  dplyr::mutate(obs=mod_cpue(obs))

data_source$CPUE <- cpue_mod

r4ss::SS_writedat(data_source, file.path(dest_dir, data_file), overwrite = TRUE)

run_ss_grid(dest_dir)

## R1a - Catchability increase 2%

source_dir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/2024_OMs/Reference/005_M0.2_sigmaR0.2_steepness0.80_cpuelambda1_llq1_env7'
dest_dir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/2024_OMs/Robustness/R1a_Increasing_q2/011_M0.2_sigmaR0.2_steepness0.80_cpuelambda1_llq1.02_env7'

# copy over files
source_files <- list.files(source_dir)

file.copy(file.path(source_dir, source_files),
          file.path(dest_dir, source_files),
          overwrite = TRUE
)


# modify data CPUE
data_file <- 'SWOv8.dat'
data_source <- r4ss::SS_readdat(file.path(source_dir, data_file))

data_mod <- data_source

mod_cpue <- function(obs) {
  n <- length(obs)-1
  obs/1.02^(0:n)
}

cpue_mod <- data_source$CPUE |> dplyr::group_by(index) |>
  dplyr::mutate(obs=mod_cpue(obs))

data_mod$CPUE <- cpue_mod
data_mod$CPUE <- as.data.frame(data_mod$CPUE )

r4ss::SS_writedat(data_mod, file.path(dest_dir, data_file), overwrite = TRUE)

run_ss_grid(dest_dir)

# check new SB/SBMSY in terminal year
replist <- r4ss::SS_output(dest_dir)
replist_2 <- r4ss::SS_output('G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/2024_OMs/Robustness/R1_Increasing_q/010_M0.2_sigmaR0.2_steepness0.80_cpuelambda1_llq1.01_env7')

sb_msy <- replist$derived_quants |> dplyr::filter(Label=='SSB_MSY')
plot(replist$timeseries$SpawnBio/sb_msy$Value, type='l', ylim=c(0,5))
abline(h=1, lty=4)
abline(h=0.4, lty=4)

sb_msy <- replist_2$derived_quants |> dplyr::filter(Label=='SSB_MSY')

lines(replist_2$timeseries$SpawnBio/sb_msy$Value, col='blue')
