
library(dplyr)
library(ggplot2)

OM.root <- 'G:/My Drive/1_Projects/North_Atlantic_Swordfish/OMs/Grid_2020'
OMgrid.dir <- file.path(OM.root, "grid_2020")

dirs <- list.dirs(OMgrid.dir, recursive = FALSE)

QincF <- dirs[grepl('llq1_', dirs)]
QincT <- dirs[grepl('llq1.01_', dirs)]

i <- 5 # select one data file (shouldn't matter which one)

# Default CPUE
data <- r4ss::SS_readdat(file.path(QincF[i], 'SWO.dat'), version='3.24',
                         verbose = FALSE)

fleet_info <- data.frame(index=1:length(data$fleetnames), Fleet=data$fleetnames)

DF <- left_join(data$CPUE, fleet_info, by="index")
DF$Qinc <- FALSE


# 1% increase in catchability
data2 <- r4ss::SS_readdat(file.path(QincT[i], 'SWO.dat'), version='3.24',
                         verbose = FALSE)

DF2 <- left_join(data2$CPUE, fleet_info, by="index")
DF2$Qinc <- TRUE

DF_all <- rbind(DF, DF2)
DF_all <- DF_all %>% dplyr::filter(year>0)
DF_all$Fleet <- factor(DF_all$Fleet,
                       levels=data$fleetnames,
                       ordered = TRUE)

ggplot(DF_all, aes(x=year, y=obs, linetype=Qinc)) +
  facet_wrap(~Fleet, scales="free", ncol=4) +
  geom_line() +
  expand_limits(y=0) +
  labs(x="Year", y="Observed CPUE Index",
       linetype='Assummed 1% Annual Increase in Catchability (q)') +
  theme_bw()


DF_all %>% dplyr::filter(Fleet=="Age-1")

