
library(dplyr)
library(r4ss)

OM.grid.dir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/grid_2022'

Ref.dir <- file.path(OM.grid.dir, 'Reference')
Rob.dir <- file.path(OM.grid.dir, 'Robustness/R1_increasing_q')

# Robustness OM - Decided MSE Technical Team August 3 2023
Rob_OM <- OM_DF %>% filter(M==0.2, steepness==0.8, Class=='R1. Increasing q')

# Copy over SS files
fls <- list.files(file.path(OM.grid.dir,Rob_OM$dir))

dir.create( file.path(Rob.dir,basename(Rob_OM$dir)))

for (fl in fls)
  file.copy(file.path(OM.grid.dir,Rob_OM$dir, fl), file.path(Rob.dir,basename(Rob_OM$dir), fl),
            overwrite = TRUE)


# Modify indices - 1% annual increase in catchability
Data <- r4ss::SS_readdat_3.30(file.path(Rob.dir,basename(Rob_OM$dir), 'SWOv5.dat'))

increase_q <- function(val, inc=0.01) {
  val/ (1+inc)^(0:(length(val)-1))

}

Data$CPUE <- Data$CPUE %>% group_by(index) %>%
  mutate(obs=increase_q(obs))

Data$CPUE <- as.data.frame(Data$CPUE)

# Write Data
r4ss::SS_writedat_3.30(Data, file.path(Rob.dir,basename(Rob_OM$dir), 'SWOv5.dat'), overwrite = TRUE)


# Run SS3
setwd(file.path(Rob.dir, basename(Rob_OM$dir)))
system2('ss3.exe', stdout = FALSE, stderr = FALSE)

# Iterative re-weighting
replist <- r4ss::SS_output(dir=file.path(Rob.dir, basename(Rob_OM$dir)))
r4ss::SS_tune_comps(replist, option = "Francis")














# Modify indices - 1% annual increase in catchability
Data <- r4ss::SS_readdat_3.30('G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/dev/SWOv5.dat')

increase_q <- function(val, inc=0.1) {
  val/ (1+inc)^(0:(length(val)-1))

}

Data$CPUE <- Data$CPUE %>% group_by(index) %>%
  mutate(obs=increase_q(obs))

Data$CPUE <- as.data.frame(Data$CPUE)

# Write Data
r4ss::SS_writedat_3.30(Data, file.path(Rob.dir,basename(Rob_OM$dir), 'SWOv5.dat'), overwrite = TRUE)


