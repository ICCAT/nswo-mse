
library(Slick)


MSElist <- readRDS('dev/PM_dev.mse')

nOM <- length(MSElist)

nMPs <- MSElist[[1]]@nMPs
nsim <- MSElist[[1]]@nsim
nyears <- MSElist[[1]]@nyears
proyears <- MSElist[[1]]@proyears

avail('PM', 'SWOMSE')

# OM Design ----

Factor_Labels <- c('Natural Mortality', 'Steepness')
nfactor<- length(Factor_Labels)
n <- nOM/nfactor

Labels <- list(c('M=0.1', 'M=0.2', 'M=0.3'),
               c('h=0.69', 'h=0.80', 'h=0.88'))

Codes <- Labels
Description <- list(c('Lowest M value', 'Middle M value', 'Highest M value'),
                    c('Lowest h value', 'Middle h value', 'Highest h value'))


Design <- data.frame(M=c(rep(1:n, each=n)), h=rep(1:n))


# Create Slick Object
SLICKobj <- NewSlick(nPerf=list(nD=10,nS=10,nP=3), # The number of deterministic (nD), stochastic (nS) and projected performance metrics (nP)
                     nMPs=nMPs,      # The number of management procedures
                     nsim=nsim,     # The number of simulations per operating model and MP
                     nProjYr=proyears,  # The number of MSE projection years
                     nStateVar=3, # The number of state variables (e.g. spawning stock biomass)
                     nHistYr=nyears,  # The number of historical years
                     Design=Design # The operating model design grid
)


# Determine Performance Metrics ----

## Deterministic ----

Det_Metrics <- read.csv('Slick_objects/Det_Metrics.csv')

SLICKobj$Perf$Det$Labels <- Det_Metrics$Label
SLICKobj$Perf$Det$Codes <- Det_Metrics$Code
SLICKobj$Perf$Det$Description <- Det_Metrics$Description

# PGK_short
for (i in 1:nOM) {
  SLICKobj$Perf$Det$Values[i,,1] <- PGK_short(MSElist[[i]])@Mean
}

# PGK_med
for (i in 1:nOM) {
  SLICKobj$Perf$Det$Values[i,,2] <- PGK_med(MSElist[[i]])@Mean
}

# PGK_long
for (i in 1:nOM) {
  SLICKobj$Perf$Det$Values[i,,3] <- PGK_long(MSElist[[i]])@Mean
}

# nLRP_short
for (i in 1:nOM) {
  SLICKobj$Perf$Det$Values[i,,4] <- nLRP_short(MSElist[[i]])@Mean
}

# nLRP_med
for (i in 1:nOM) {
  SLICKobj$Perf$Det$Values[i,,5] <- nLRP_med(MSElist[[i]])@Mean
}

# nLRP_long
for (i in 1:nOM) {
  SLICKobj$Perf$Det$Values[i,,6] <- nLRP_long(MSElist[[i]])@Mean
}

# relAvTAC_short
for (i in 1:nOM) {
  pm <- AvTAC_short(MSElist[[i]])
  SLICKobj$Perf$Det$Values[i,,7] <- pm@Mean/max(pm@Mean)
}

# relAvTAC_med
for (i in 1:nOM) {
  pm <- AvTAC_med(MSElist[[i]])
  SLICKobj$Perf$Det$Values[i,,8] <- pm@Mean/max(pm@Mean)
}

# relAvTAC_long
for (i in 1:nOM) {
  pm <- AvTAC_long(MSElist[[i]])
  SLICKobj$Perf$Det$Values[i,,9] <- pm@Mean/max(pm@Mean)
}

# Var25
for (i in 1:nOM) {
  pm <- VarC(MSElist[[i]])
  SLICKobj$Perf$Det$Values[i,,10] <- apply(pm@Stat < 0.25, 2, mean)
}

saveRDS(SLICKobj, 'Slick_objects/test.slick')

SLICKobj$Perf$Det$RefPoints
SLICKobj$Perf$Det$RefNames


# relAvTAC_short
# relAvTAC_med
# relAvTAC_long
# Var25



## Stochastic ----

## Projection ----


## State Variables ----

# SSB/SSBMSY
# F/FMSY
# Yield

# OM Details ----
SLICKobj$OM$Design
SLICKobj$OM$Factor_Labels
SLICKobj$OM$Design












# Deterministic


# Stochastic

# Projection

# StateVar

# Management Procecudures

# Text

# Misc




