
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

source_dir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/2024_OMs/Reference/005_M0.2_sigmaR0.2_steepness0.80_cpuelambda1_llq1_env7'
dest_dir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/2024_OMs/Robustness/010_M0.2_sigmaR0.2_steepness0.80_cpuelambda1_llq1.01_env7'

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


