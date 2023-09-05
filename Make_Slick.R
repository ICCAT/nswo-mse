
library(Slick)
library(SWOMSE)
library(colorspace)


# Reference OMs
Ref_OMs <- OM_DF %>% filter(Class=='Reference')

# subset MPs

MPs_keep <- c('CC10000', 'EA1_a', 'SPFox_a', 'SPS_a',
              'WA1_a', 'CI1_a', 'CE_a', 'CE25_a',
              'SPS25_a', 'SPS_b', 'SPS25_b', 'SPS_c',
              'SPS25_c')

# MSElist <- list()
# for (i in 1:nrow(Ref_OMs)) {
#   mmse <-  readRDS(file.path('MSE_Objects', paste0(Ref_OMs$OM.object[i], '.mse')))
#   MSElist[[i]] <- Sub_MMSE(mmse, MPs=MPs_keep)
# }

plot(rowSums(MSElist[[1]]@B[,,1,32]),
MSElist[[1]]@PPD[[1]][[1]][[1]]@Abun )

nOM <- length(MSElist)

nMPs <- MSElist[[1]]@nMPs
nsim <- MSElist[[1]]@nsim
nyears <- MSElist[[1]]@nyears
proyears <- 30 # MSElist[[1]]@proyears


# OM Design ----

Factor_Labels <- c('Natural Mortality', 'Steepness')
nfactor<- length(Factor_Labels)
n <- nOM/nfactor

Labels <- list(c('M=0.1', 'M=0.2', 'M=0.3'),
               c('h=0.69', 'h=0.80', 'h=0.88'))

Codes <- Labels
Description <- list(c('Lowest M value', 'Middle M value', 'Highest M value'),
                    c('Lowest h value', 'Middle h value', 'Highest h value'))


dd <- expand.grid(h=1:3, M=1:3)

Design <- data.frame(M=dd$M, h=dd$h)

colnames(Design) <- c("Natural Mortality", "Steepness")

# Create Slick Object
nD <- 8
nS <- 7
nP <- 2
nSV <- 3

SLICKobj <- NewSlick(nPerf=list(nD=nD,nS=nS,nP=nP), # The number of deterministic (nD), stochastic (nS) and projected performance metrics (nP)
                     nMPs=nMPs,      # The number of management procedures
                     nsim=nsim,     # The number of simulations per operating model and MP
                     nProjYr=proyears,  # The number of MSE projection years
                     nStateVar=nSV, # The number of state variables (e.g. spawning stock biomass)
                     nHistYr=nyears,  # The number of historical years
                     Design=Design # The operating model design grid
)

SLICKobj$name <- 'North Atlantic Swordfish MSE'
SLICKobj$Text$Title <- 'North Atlantic Swordfish MSE'
SLICKobj$Text$Sub_title <- 'Prelimanary Results'
SLICKobj$Text$Introduction[[1]] <- "These prelimanary results show the performance of the a set of Candidate Management Procedures (CMPs) that have been developed for the North Atlantic Swordfish fishery."

SLICKobj$Text$Introduction[[2]] <- "<strong>Note:</strong>These results were posted with permission from the International Commission for the Conservation of Atlantic Tunas (ICCAT) for the purpose of demonstrating the features of Slick. The North Atlantic Swordfish MSE process in still ongoing. The operating models, candidate management procedures, and performance metrics shown here are for demonstration purposes only and are subject to change as the MSE process contiunes. The results presented here do not necessarily reflect the point of view of ICCAT or other funders and in no ways anticipate ICCAT future policy in this area."


SLICKobj$Misc$Author <- 'NSWO MSE Technical Team'
SLICKobj$Misc$Contact <- "<a href='mailto:adrian@bluematterscience.com'>adrian@bluematterscience.com</a>"
SLICKobj$Misc$Date <- 'June 2023'
SLICKobj$Misc$Institution <- ''


SLICKobj$Misc$Cols$MP <- colorspace::diverge_hcl(n=nMPs)


## Factor info
SLICKobj$OM$Design
SLICKobj$OM$Factor_Labels <- c('Natural Mortality', 'Steepness')
SLICKobj$OM$Description[[1]] <- c('Natural Mortality of 0.1', 'Natural Mortality of 0.2', 'Natural Mortality of 0.3')
SLICKobj$OM$Description[[2]] <- c('Beverton-Holt stock recruitment steepness (resilience) of 0.69',
                                  'Beverton-Holt stock recruitment steepness (resilience) of 0.80',
                                  'Beverton-Holt stock recruitment steepness (resilience) of 0.88')
SLICKobj$OM$Codes[[1]] <- c('0.1', '0.2', '0.3')
SLICKobj$OM$Codes[[2]] <- c('0.69', '0.80', '0.88')

SLICKobj$OM$Labels[[1]] <- c('M=0.1', 'M=0.2', 'M=0.3')
SLICKobj$OM$Labels[[2]] <- c('h=0.69', 'h=0.80', 'h=0.88')


## MP Info

MPs_keep <- c('CC10000', 'EA1_a', 'SPFox_a', 'SPS_a',
              'WA1_a', 'CI1_a', 'CE_a', 'CE25_a',
              'SPS25_a', 'SPS_b', 'SPS25_b', 'SPS_c',
              'SPS25_c')


SLICKobj$MP$Codes <- MPs_keep
SLICKobj$MP$Labels <- MPs_keep

SLICKobj$MP$Description <- c('Constant TAC at 10,000 t',
                             'Index ratio method using the SP, MO, and PO indices, smoothed and scaled by the inverse variance before averaging. Tuned to PGK_short = 0.51',
                             'Fox Surplus Production with a HCR. Tuned to PGK_short=0.51',
                             'Schaefer Surplus Production with a HCR. Tuned to PGK_short=0.51',
                             'Index ratio method using the CA, US, CT, and JP indices, smoothed and scaled by the inverse variance before averaging. Tuned to PGK_short = 0.51',
                             'Index ratio method using the Combined Index, smoothed and scaled by the inverse variance before averaging. Tuned to PGK_short = 0.51',
                             'Constant Exploitation Rate. Tuned to PGK_short = 0.51',
                             'Constant Exploitation Rate with a maximum absolute change in TAC of 25%. Tuned to PGK_short = 0.51',
                             'Schaefer Surplus Production with a HCR with a maximum absolute change in TAC of 25%. Tuned to PGK_short=0.51',
                             'Schaefer Surplus Production with a HCR. Tuned to PGK_short=0.60',
                             'Schaefer Surplus Production with a HCR with a maximum absolute change in TAC of 25%. Tuned to PGK_short=0.60',
                             'Schaefer Surplus Production with a HCR. Tuned to PGK_short=0.70',
                             'Schaefer Surplus Production with a HCR with a maximum absolute change in TAC of 25%. Tuned to PGK_short=0.70'
)


# Determine Performance Metrics ----

## Deterministic ----

SLICKobj$Perf$Det$Codes <- c("PGK_short", "PGK_med", 'PGK_long',
                              'TAC1', "AvTAC_short", 'AvTAC_med', 'AvTAC_long',
                              "VarTAC25")

SLICKobj$Perf$Det$Labels <- SLICKobj$Perf$Det$Codes


SLICKobj$Perf$Det$Description <- c('Prob. Green Zone of Kobe Space (2024-2033)',
                                   'Prob. Green Zone of Kobe Space (2034-2043)',
                                   'Prob. Green Zone of Kobe Space (2044-2053)',
                                   'Median TAC relative to the highest TAC in 2024. ',
                                   'Median TAC relative to the highest TAC in 2024-2033',
                                   'Median TAC relative to the highest TAC in any simulation in 2034-2043 ',
                                   'Median TAC relative to the highest TAC in any simulation in 2044-2053',
                                   'Prob. average absolute change in TAC <25%')

# PGK_short
for (i in 1:nOM) {
  SLICKobj$Perf$Det$Values[i,,1] <- PGK_short(MSElist[[i]])@Mean * 100
}

# PGK_med
for (i in 1:nOM) {
  SLICKobj$Perf$Det$Values[i,,2] <- PGK_med(MSElist[[i]])@Mean  * 100
}

# PGK_long
for (i in 1:nOM) {
  SLICKobj$Perf$Det$Values[i,,3] <- PGK_long(MSElist[[i]])@Mean * 100
}

# TAC1
for (i in 1:nOM) {
  PM <- TAC1(MSElist[[i]])
  PM@Mean <- PM@Mean/max(PM@Mean)
  SLICKobj$Perf$Det$Values[i,,4] <-  PM@Mean * 100
}

# AvTAC_short
for (i in 1:nOM) {
  PM <- AvTAC_short(MSElist[[i]])
  PM@Mean <- PM@Mean/max(PM@Mean)
  SLICKobj$Perf$Det$Values[i,,5] <-  PM@Mean * 100
}

# AvTAC_med
for (i in 1:nOM) {
  PM <- AvTAC_med(MSElist[[i]])
  PM@Mean <- PM@Mean/max(PM@Mean)
  SLICKobj$Perf$Det$Values[i,,6] <-  PM@Mean * 100
}

# AvTAC_long
for (i in 1:nOM) {
  PM <- AvTAC_long(MSElist[[i]])
  PM@Mean <- PM@Mean/max(PM@Mean)
  SLICKobj$Perf$Det$Values[i,,7] <-  PM@Mean * 100
}

# Var25
for (i in 1:nOM) {
  pm <- VarC(MSElist[[i]])
  SLICKobj$Perf$Det$Values[i,,8] <- apply(pm@Stat < 0.25, 2, mean) * 100
}


# SLICKobj$Perf$Det$RefPoints <- c(0.51, 0.51, 0.51,
#                                  NA, NA, NA, NA,
#                                  NA)
#
# SLICKobj$Perf$Det$RefNames

## Stochastic ----

SLICKobj$Perf$Stoch$Codes <- c("FGK_short", "FGK_med", 'FGK_long',
                                'TAC1', "AvTAC_short", 'AvTAC_med', 'AvTAC_long')
SLICKobj$Perf$Stoch$Labels <- SLICKobj$Perf$Stoch$Codes


SLICKobj$Perf$Stoch$Description <- c('Fraction of years in  Green Zone of Kobe Space (2024-2033)',
                                   'Fraction of years in  Green Zone of Kobe Space (2034-2043)',
                                   'Fraction of years in  Green Zone of Kobe Space (2044-2053)',
                                   'TAC (t) (2024)',
                                   'TAC (t) (2024-2033)',
                                   'TAC (t) (2034-2043)',
                                   'TAC (t) (2044-2053)'
                                   )



# PGK_short
for (i in 1:nOM) {
  PM <- PGK_short(MSElist[[i]])
  SLICKobj$Perf$Stoch$Values[,i,,1] <- apply(PM@Stat, c(1,2), mean)  * 100
}

# PGK_med
for (i in 1:nOM) {
  PM <- PGK_med(MSElist[[i]])
  SLICKobj$Perf$Stoch$Values[,i,,2] <- apply(PM@Stat, c(1,2), mean) * 100
}

# PGK_long
for (i in 1:nOM) {
  PM <- PGK_long(MSElist[[i]])
  SLICKobj$Perf$Stoch$Values[,i,,3] <- apply(PM@Stat, c(1,2), mean) * 100
}

# TAC1
for (i in 1:nOM) {
  PM <- TAC1(MSElist[[i]])
  SLICKobj$Perf$Stoch$Values[,i,,4] <- apply(PM@Stat, c(1,2), mean)
}

# AvTAC_short
for (i in 1:nOM) {
  PM <- AvTAC_short(MSElist[[i]])
  SLICKobj$Perf$Stoch$Values[,i,,5] <- apply(PM@Stat, c(1,2), mean)
}

# AvTAC_med
for (i in 1:nOM) {
  PM <- AvTAC_med(MSElist[[i]])
  SLICKobj$Perf$Stoch$Values[,i,,6] <- apply(PM@Stat, c(1,2), mean)
}

# AvTAC_long
for (i in 1:nOM) {
  PM <- AvTAC_long(MSElist[[i]])
  SLICKobj$Perf$Stoch$Values[,i,,7] <- apply(PM@Stat, c(1,2), mean)
}


# # Var25
# for (i in 1:nOM) {
#   PM <- VarC(MSElist[[i]])
#   SLICKobj$Perf$Stoch$Values[,i,,8] <- PM@Stat * 100
# }


## Projection ----

SLICKobj$Perf$Proj$Codes <- c('SB/SBMSY',
                               'F/FMSY')

SLICKobj$Perf$Proj$Labels <- SLICKobj$Perf$Proj$Codes
SLICKobj$Perf$Proj$Description <- c('Spawning Biomass relative to SB<sub>MSY</sub>',
                                    'Fishing mortality relative to F<sub>MSY</sub>')

year_df <- data.frame(i=-2:30, Year=2021:2053)
year_df$i2 <- 1:nrow(year_df)
p.years <- year_df$i2[year_df$Year>=2024]
# SB/SBMSY
for (i in 1:nOM) {
  SLICKobj$Perf$Proj$Values[,i,,1,] <- MSElist[[i]]@SB_SBMSY[,1,,p.years]
}

# F/MSY
for (i in 1:nOM) {
  SLICKobj$Perf$Proj$Values[,i,,2,] <- MSElist[[i]]@F_FMSY[,1,1,,p.years]
}

# # TAC (t)
# for (i in 1:nOM) {
#   SLICKobj$Perf$Proj$Values[,i,,3,] <- apply(MSElist[[i]]@TAC[,,1,,p.years], c(1,3,4), sum)
# }
#
# # TAC (relative)
# for (i in 1:nOM) {
#   TAC <- apply(MSElist[[i]]@TAC[,,1,,p.years], c(1,3,4), sum)
#   msys <- apply(MSElist[[i]]@RefPoint$ByYear$MSY[,,,nyears+p.years], c(1,3,4), sum)
#
#   SLICKobj$Perf$Proj$Values[,i,,4,] <- TAC/msys
# }


SLICKobj$Perf$Proj$Times <- 2024:2053
SLICKobj$Perf$Proj$RefPoints <- list()
SLICKobj$Perf$Proj$RefPoints[[1]] <- c(1, 0.4)
SLICKobj$Perf$Proj$RefPoints[[2]] <- 1
SLICKobj$Perf$Proj$RefNames[[1]] <- c("SBMSY", 'BLim')
SLICKobj$Perf$Proj$RefNames[[2]] <- 'FMSY'


## State Variables ----

SLICKobj$StateVar$Codes <- c('SB/SBMSY', 'F/FMSY', 'TAC (t)')
SLICKobj$StateVar$Labels <- SLICKobj$StateVar$Codes
SLICKobj$StateVar$Description <- c('Spawning Biomass relative to SB<sub>MSY</sub>',
                                   'Fishing mortality relative to F<sub>MSY</sub>',
                                   'Total allowable catch (TAC; t)')

totyears <- length(1950:2053)
SLICKobj$StateVar$Values <- array(NA, dim=c(nsim, nOM, nMPs, nSV, totyears))

# SB/SBMSY
for (i in 1:nOM) {
  for (mm in 1:nMPs) {
    histSB <- apply(MSElist[[i]]@multiHist[[1]][[1]]@TSdata$SBiomass, 1:2, sum)
    projSB <- MSElist[[i]]@SSB[,1,mm,]
    SB <- abind::abind(histSB, projSB, along=2)
    SB_SBMSY <- SB/MSElist[[i]]@RefPoint$ByYear$SSBMSY[,1,mm,]
    SLICKobj$StateVar$Values[,i,mm,1,] <- SB_SBMSY
  }
}

# F/MSY
for (i in 1:nOM) {
  for (mm in 1:nMPs) {
    histF <- apply(MSElist[[i]]@multiHist[[1]][[1]]@TSdata$Find, 1:2, sum)
    projF <- MSElist[[i]]@FM[,1,1,mm,]
    Fs <- abind::abind(histF, projF, along=2)
    F_FMSY <- Fs/MSElist[[i]]@RefPoint$ByYear$FMSY[,1,mm,]
    SLICKobj$StateVar$Values[,i,mm,2,] <- F_FMSY
  }
}

# TAC (t)
for (i in 1:nOM) {
  for (mm in 1:nMPs) {
    histTAC <- apply(MSElist[[i]]@multiHist[[1]][[1]]@TSdata$Landings, 1:2, sum) +
      apply(MSElist[[i]]@multiHist[[2]][[1]]@TSdata$Landings, 1:2, sum)
    projTAC <- apply(MSElist[[i]]@TAC[,,1,mm,], c(1,3), sum)
    TACs <- abind::abind(histTAC, projTAC, along=2)
    SLICKobj$StateVar$Values[,i,mm,3,] <- TACs
  }
}


# # TAC (relative)
# for (i in 1:nOM) {
#   MSYs <- apply(MSElist[[i]]@RefPoint$ByYear$MSY, c(1,3,4), sum)
#   for (mm in 1:nMPs) {
#     SLICKobj$StateVar$Values[,i,mm,4,] <-  SLICKobj$StateVar$Values[,i,mm,3,]/MSYs[,mm,]
#   }
# }

SLICKobj$StateVar$TimeNow <- 2023
SLICKobj$StateVar$Times <- 1950:2053

SLICKobj$StateVar$RefPoints <- list(
  c(1, 0.4),
  c(1),
  NA
)
SLICKobj$StateVar$RefNames <- list(
  c('SBMSY', 'Blim'),
  c('FMSY'),
  NA
)




saveRDS(SLICKobj, 'Slick_objects/NSWO.slick')

#
saveRDS(SLICKobj, 'C:/Users/User/Documents/GitHub/Slick/inst/shiny_apps/Slick/data/case_studies/NSWO.slick')




