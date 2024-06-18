
library(Slick)
library(SWOMSE)

slick <- Slick()
Title(slick) <- 'June 2024 NSWO Results'
Author(slick) <- 'NSWO Technical Team'
Email(slick) <- "[adrian@bluematterscience.com](mailto:adrian@bluematterscience.com)"
Introduction(slick) <- '

Preliminary results for discussion with the NSWO Technical Team

<strong>Note:</strong>These results were posted with permission from the International Commission for the Conservation of Atlantic Tunas (ICCAT) for the purpose of demonstrating the features of Slick. The North Atlantic Swordfish MSE process in still ongoing. The operating models, candidate management procedures, and performance metrics shown here are for demonstration purposes only and are subject to change as the MSE process contiunes. The results presented here do not necessarily reflect the point of view of ICCAT or other funders and in no ways anticipate ICCAT future policy in this area.

'




# MPs ----
mp_names <- c('CE_b', 'CE_c',
              'MCC5_b', 'MCC5_c',
              'MCC7_b', 'MCC7_c',
              'MCC85_b', 'MCC85_c',
              'MCC97_b', 'MCC97_c',
              'SPSSFox_b', 'SPSSFox_c',
              'SPSSFox2_b', 'SPSSFox2_c')

mps <- MPs()
Code(mps) <- mp_names
tunings <- c('PGK_short 60%', 'PGK_short 70%')

Label(mps) <- c(paste('Constant Exploitation Rate', tunings),
                paste('Mostly Constant Catch 5', tunings),
                paste('Mostly Constant Catch 7', tunings),
                paste('Mostly Constant Catch 85', tunings),
                paste('Mostly Constant Catch 97', tunings),
                paste('Surplus Production Model', tunings),
                paste('Surplus Production Model v2', tunings)
)

Description(mps) <- c('Aims to maintain a constant exploitation rate (ER) at mean of 2016 - 2020. A HCR linearly reduces the ER if the current index is between 50% - 80% of the mean index from this same time period. If the current index is less than 50% of the mean historical index, exploitation rate  is set to 10% of the mean historical ER. TAC is constrained to change no more than 25% between management cycles. Tuned to 60% PGK_short.',
                      'Same as previous but tuned to 70% PGK_short',
                      'The goal of the MCC methods is to keep the TAC as constant as possible. The TAC changes by discrete steps according to the ratio the recent index (mean of three most recent years) to the historical target index (mean of 2017 - 2019). Tuned to 60% PGK_short.',
                      'Same as previous but tuned to 70% PGK_short',
                      'Same as MCC5 but different stepped changes in TAC. Tuned to 60% PGK_short.',
                      'Same as previous but tuned to 70% PGK_short',
                      'Same as MCC5 but different stepped changes in TAC. Tuned to 60% PGK_short.',
                      'Same as previous but tuned to 70% PGK_short',
                      'Same as MCC5 but different stepped changes in TAC. Tuned to 60% PGK_short.',
                      'Same as previous but tuned to 70% PGK_short',
                      "A state-space surplus production model is fit to the index and catch data. A constant F (Ftarget) is set (determined as a tuning parameter) and the TAC calculated by applying the F to the model's estimate of abundance. A linear harvest control rule reduces Ftarget to 0.1Ftarget if estimated B/BMSY < BMSY. If estimated B/BMSY < 0.4BMSY, Ftarget set to 0.1Ftarget. TAC is constrained to change no more than 25% between management cycles. Tuned to 60% PGK_short.",
                      'Same as previous but tuned to 70% PGK_short',
                      'Same as SPSSFox, except there is no constraint on reduction in TAC if estimated B/BMSY < 1. Tuned to 60% PGK_short.',
                      'Same as previous but tuned to 70% PGK_short'
                      )


Preset(mps) <- list('PGK 60'=which(grepl('_b',Code(mps))),
                    'PGK 70'=which(grepl('_c',Code(mps))))
MPs(slick) <- mps

Check(mps)




# OMs ----
oms <- OMs()

Factors(oms) <- data.frame(Factor=c(rep('M',3),
                                    rep('h', 3),
                                    rep('Set',6)),
                           Level=c(0.1, 0.2, 0.3,
                                   0.69, 0.8, 0.88,
                                   'Reference',
                                   'R1',
                                   'R2',
                                   'R3a',
                                   'R3b',
                                   'R4'),
                           Description=c('Natural Mortality = 0.1',
                                         'Natural Mortality = 0.2',
                                         'Natural Mortality = 0.3',
                                         'Steepness = 0.69',
                                         'Steepness = 0.80',
                                         'Steepness = 0.88',
                                         'Reference OMs',
                                         'Robustness 1: Assumed 1 percent annual increase catchability (q), that is not accounted for in the standardization of the indices of abundance (historical and projection)',
                                         'Robustness 2: Same as R1, except only in the historical period',
                                         'Robustness 3a: Cyclical pattern in recruitment deviations in projection period; a proxy for impact of climate change on stock productivity',
                                         'Lower than expected recruitment deviations for first 15 years of projection period; a proxy for impact of climate change on stock productivity',
                                         'Illegal, unreported, or unregulated catches')
)

Design(oms) <- data.frame(M=c(rep(c(0.1,0.2,0.3), each=3), rep(0.2, 5)),
                          h=c(rep(c(0.69,0.8,0.88),3), rep(0.8,5)),
                          Set=c(rep('Reference', 9),
                                'R1', 'R2', 'R3a', 'R3b', 'R4'))


Preset(oms) <- list('Reference'=list(1:3, 1:3, 1),
                    'R0'=list(2, 2, 1),
                    'R1'=list(2, 2, 2),
                    'R2'=list(2, 2, 3),
                    'R3a'=list(2, 2, 4),
                    'R3b'=list(2, 2, 5),
                    'R4'=list(2, 2, 6))

Check(oms)

OMs(slick) <- oms




# Time Series ----

timeseries <- Timeseries()

Code(timeseries) <- c('SB/SBMSY', 'F/FMSY', 'Catch', 'Index')
Label(timeseries) <- c('SB/SBMSY', 'F/FMSY', 'Catch', 'Index')
Description(timeseries) <- c('Spawning biomass relative to SBMSY',
                             'Fishing mortality relative to FMSY',
                             'Historical catch and projected catch (t)',
                             'Combined Index')



years <- c(rev(seq(MOM_001@Fleets[[1]][[1]]@CurrentYr, by=-1, length.out=MOM_001@Fleets[[1]][[1]]@nyears)),
           seq(MOM_001@Fleets[[1]][[1]]@CurrentYr+1, by=1, length.out=MOM_001@proyears))

Time(timeseries) <- years
TimeNow(timeseries) <- 2024
Value(timeseries) <- array(NA, dim=c(MOM_000@nsim,
                                     nrow(oms@Design),
                                     length(mps@Code),
                                     length(Code(timeseries)),
                                     length(years))
)


MSE_dir <- 'MSE_objects'
mse_files <- list.files(MSE_dir, pattern='.mse')
mp_codes <- unlist(lapply(strsplit(Code(mps), '_'), '[[', 1)) |> unique()

mp_index <- data.frame(MP=rep(1:length(mp_codes), each=2),
                       MP_index=seq_along(Code(mps)))


populate_TS <- function(result_files, timeseries, omnums) {
  for (mm in seq_along(mp_codes)) {
    mp_files <- result_files[grepl(paste0('\\<', mp_codes[mm], '\\>'), result_files)]
    for (i in seq_along(mp_files)) {
      mse <- readRDS(file.path('MSE_objects', mp_files[i]))
      mm_ind <- mp_index %>% dplyr::filter(MP%in%mm)

      # SB/SBMSY
      histSB <- apply(mse@multiHist$Female[[1]]@TSdata$SBiomass, 1:2, sum)
      histSB <- replicate(length(mm_ind$MP_index), histSB)|> aperm(perm=c(1,3,2))
      projSB <- mse@SSB[,1,,]
      sb <- abind::abind(histSB, projSB, along=3)
      sb_sbmsy <- sb/mse@RefPoint$ByYear$SSBMSY[,1,,]
      Value(timeseries)[,omnums[i],mm_ind$MP_index, 1, ] <- sb_sbmsy


      # F/FMSY
      Fs <- get_F(mse)
      histF <- Fs %>% dplyr::filter(Period=='Historical', Stock=='Female')

      FS <- array(NA, dim=c(mse@nsim, mse@nMPs, mse@nyears+mse@proyears))
      FS[,,1:mse@nyears] <-  replicate(mse@nMPs, matrix(histF$Value, mse@nsim, mse@nyears)) |>
        aperm(perm=c(1,3,2))
      FS[,,(mse@nyears+1):(mse@nyears+mse@proyears)] <- mse@FM[,1,1,,]
      f_fmsy <- FS/mse@RefPoint$ByYear$FMSY[,1,,]
      Value(timeseries)[,omnums[i],mm_ind$MP_index, 2, ] <- f_fmsy

      # Catch
      rem <- get_Removals(mse)
      histC <- rem %>% dplyr::filter(Period=='Historical') |>
        dplyr::group_by(Year, Sim) |>
        dplyr::summarise(Catch=sum(Value))

      Catch <- array(NA, dim=c(mse@nsim, mse@nMPs, mse@nyears+mse@proyears))
      Catch[,,1:mse@nyears] <-  replicate(mse@nMPs, matrix(histC$Catch, mse@nsim, mse@nyears)) |>
        aperm(perm=c(1,3,2))
      Catch[,,(mse@nyears+1):(mse@nyears+mse@proyears)] <- apply(mse@Removals, c(1,4,5), sum)
      Value(timeseries)[,omnums[i],mm_ind$MP_index, 3, ] <- Catch

      # Index
      for (j in seq_along(mm_ind$MP_index)) {
        Value(timeseries)[,omnums[i],mm_ind$MP_index[j], 4, 1:(mse@nyears+mse@proyears-1)] <- mse@PPD[[1]][[1]][[j]]@Ind
      }
    }
  }
  timeseries
}

# Reference OMs
result_files <- mse_files[grepl('-Reference', mse_files)]
timeseries <- populate_TS(result_files, timeseries, omnums=1:9)

result_files <- mse_files[grepl('-R1', mse_files)]
timeseries <- populate_TS(result_files, timeseries, omnums=10)

result_files <- mse_files[grepl('-R2', mse_files)]
timeseries <- populate_TS(result_files, timeseries, omnums=11)

result_files <- mse_files[grepl('-R3a', mse_files)]
timeseries <- populate_TS(result_files, timeseries, omnums=12)

result_files <- mse_files[grepl('-R3b', mse_files)]
timeseries <- populate_TS(result_files, timeseries, omnums=13)

result_files <- mse_files[grepl('-R4', mse_files)]
timeseries <- populate_TS(result_files, timeseries, omnums=14)

Target(timeseries) <- c(1, NA, NA, NA)
Limit (timeseries) <- c(0.4, NA, NA, NA)


Preset(timeseries)

Timeseries(slick) <- timeseries


# ---- Quilt -----
quilt <- Quilt()
Code(quilt) <- c('AvTAC_long', 'AvTAC_med', 'AvTAC_short',
                 'nLRP', 'PGK', 'PGK_med', 'PGK_short',
                 'PNOF', 'VarC')

Label(quilt) <- Code(quilt)

mse_files <- list.files(MSE_dir, pattern='.mse')
mse <- readRDS(file.path(MSE_dir, mse_files[1]))

# get captions
desc <- rep('', length(Code(quilt)))
for (p in seq_along(Code(quilt))) {
  desc[p] <- get(Code(quilt)[p])(mse)@Caption
}

Description(quilt) <- desc

Value(quilt) <- array(NA, dim=c(nrow(Design(oms)), length(Code(mps)), length(Code(quilt))))



mse_files <- list.files(MSE_dir, pattern='.mse')
mp_codes <- unlist(lapply(strsplit(Code(mps), '_'), '[[', 1)) |> unique()


populate_Quilt <- function(result_files, quilt, omnums) {
  for (mm in seq_along(mp_codes)) {
    mp_files <- result_files[grepl(paste0('\\<', mp_codes[mm], '\\>'), result_files)]
    for (i in seq_along(mp_files)) {

      mse <- readRDS(file.path('MSE_objects', mp_files[i]))
      mm_ind <- mp_index %>% dplyr::filter(MP%in%mm)
      pms <- Code(quilt)
      for (p in seq_along(pms)) {
        Value(quilt)[omnums[i],mm_ind$MP_index,p] <- get(pms[p])(mse)@Mean
      }
    }
  }
  quilt
}

# Reference
result_files <- mse_files[grepl('-Reference', mse_files)]
quilt <- populate_Quilt(result_files, quilt, 1:9)

# R1
result_files <- mse_files[grepl('-R1', mse_files)]
quilt <- populate_Quilt(result_files, quilt, 10)

# R2
result_files <- mse_files[grepl('-R2', mse_files)]
quilt <- populate_Quilt(result_files, quilt, 11)


# R3a
result_files <- mse_files[grepl('-R3a', mse_files)]
quilt <- populate_Quilt(result_files, quilt, 12)


# R3b
result_files <- mse_files[grepl('-R3b', mse_files)]
quilt <- populate_Quilt(result_files, quilt, 13)

# R4
result_files <- mse_files[grepl('-R4', mse_files)]
quilt <- populate_Quilt(result_files, quilt, 14)

Color(quilt) <- c('darkblue','white' )
Quilt(slick) <- quilt



# ---- Kobe ----
kobe <- Slick::Kobe()
Code(kobe) <- c('SB/SBMSY', 'F/FMSY')
Label(kobe) <- Code(kobe)
Description(kobe) <- c('SB/SBMSY', 'F/FMSY')
Time(kobe) <- 2025:2054
Value(kobe) <- array(NA, dim=c(mse@nsim, nrow(Design(oms)), length(Code(mps)),
                               length(Code(kobe)), 30))


populateKobe <- function(result_files, kobe, omnums) {
  for (mm in seq_along(mp_codes)) {
    mp_files <- result_files[grepl(paste0('\\<', mp_codes[mm], '\\>'), result_files)]
    for (i in seq_along(mp_files)) {
      mse <- readRDS(file.path('MSE_objects', mp_files[i]))

      mm_ind <- mp_index %>% dplyr::filter(MP%in%mm)
      Value(kobe)[,omnums[i], mm_ind$MP_index, 1,] <- mse@SB_SBMSY[,1,,5:34]
      Value(kobe)[,omnums[i], mm_ind$MP_index, 2,] <- mse@F_FMSY[,1,1,,5:34]

    }
  }
  kobe
}

# Reference
result_files <- mse_files[grepl('-Reference', mse_files)]
kobe <- populateKobe(result_files, kobe, 1:9)

# R1
result_files <- mse_files[grepl('-R1', mse_files)]
kobe <- populateKobe(result_files, kobe, 10)

# R2
result_files <- mse_files[grepl('-R2', mse_files)]
kobe <- populateKobe(result_files, kobe, 11)


# R3a
result_files <- mse_files[grepl('-R3a', mse_files)]
kobe <- populateKobe(result_files, kobe, 12)


# R3b
result_files <- mse_files[grepl('-R3b', mse_files)]
kobe <- populateKobe(result_files, kobe, 13)

# R4
result_files <- mse_files[grepl('-R4', mse_files)]
kobe <- populateKobe(result_files, kobe, 14)

Kobe(slick) <- kobe



# save Slick ----

saveRDS(slick, 'NSWO.slick')
saveRDS(slick, '../Slick/inst/NSWO.rda')


#
#
# # Reference OMs
# Ref_OMs <- OM_DF %>% filter(Class=='Reference')
#
# # subset MPs
#
# MPs_keep <- c('CC10000', 'EA1_a', 'SPFox_a', 'SPS_a',
#               'WA1_a', 'CI1_a', 'CE_a', 'CE25_a',
#               'SPS25_a', 'SPS_b', 'SPS25_b', 'SPS_c',
#               'SPS25_c')
#
# # MSElist <- list()
# # for (i in 1:nrow(Ref_OMs)) {
# #   mmse <-  readRDS(file.path('MSE_Objects', paste0(Ref_OMs$OM.object[i], '.mse')))
# #   MSElist[[i]] <- Sub_MMSE(mmse, MPs=MPs_keep)
# # }
#
# plot(rowSums(MSElist[[1]]@B[,,1,32]),
# MSElist[[1]]@PPD[[1]][[1]][[1]]@Abun )
#
# nOM <- length(MSElist)
#
# nMPs <- MSElist[[1]]@nMPs
# nsim <- MSElist[[1]]@nsim
# nyears <- MSElist[[1]]@nyears
# proyears <- 30 # MSElist[[1]]@proyears
#
#
# # OM Design ----
#
# Factor_Labels <- c('Natural Mortality', 'Steepness')
# nfactor<- length(Factor_Labels)
# n <- nOM/nfactor
#
# Labels <- list(c('M=0.1', 'M=0.2', 'M=0.3'),
#                c('h=0.69', 'h=0.80', 'h=0.88'))
#
# Codes <- Labels
# Description <- list(c('Lowest M value', 'Middle M value', 'Highest M value'),
#                     c('Lowest h value', 'Middle h value', 'Highest h value'))
#
#
# dd <- expand.grid(h=1:3, M=1:3)
#
# Design <- data.frame(M=dd$M, h=dd$h)
#
# colnames(Design) <- c("Natural Mortality", "Steepness")
#
# # Create Slick Object
# nD <- 8
# nS <- 7
# nP <- 2
# nSV <- 3
#
# SLICKobj <- NewSlick(nPerf=list(nD=nD,nS=nS,nP=nP), # The number of deterministic (nD), stochastic (nS) and projected performance metrics (nP)
#                      nMPs=nMPs,      # The number of management procedures
#                      nsim=nsim,     # The number of simulations per operating model and MP
#                      nProjYr=proyears,  # The number of MSE projection years
#                      nStateVar=nSV, # The number of state variables (e.g. spawning stock biomass)
#                      nHistYr=nyears,  # The number of historical years
#                      Design=Design # The operating model design grid
# )
#
# SLICKobj$name <- 'North Atlantic Swordfish MSE'
# SLICKobj$Text$Title <- 'North Atlantic Swordfish MSE'
# SLICKobj$Text$Sub_title <- 'Prelimanary Results'
# SLICKobj$Text$Introduction[[1]] <- "These prelimanary results show the performance of the a set of Candidate Management Procedures (CMPs) that have been developed for the North Atlantic Swordfish fishery."
#
# SLICKobj$Text$Introduction[[2]] <- "<strong>Note:</strong>These results were posted with permission from the International Commission for the Conservation of Atlantic Tunas (ICCAT) for the purpose of demonstrating the features of Slick. The North Atlantic Swordfish MSE process in still ongoing. The operating models, candidate management procedures, and performance metrics shown here are for demonstration purposes only and are subject to change as the MSE process contiunes. The results presented here do not necessarily reflect the point of view of ICCAT or other funders and in no ways anticipate ICCAT future policy in this area."
#
#
# SLICKobj$Misc$Author <- 'NSWO MSE Technical Team'
# SLICKobj$Misc$Contact <- "<a href='mailto:adrian@bluematterscience.com'>adrian@bluematterscience.com</a>"
# SLICKobj$Misc$Date <- 'June 2023'
# SLICKobj$Misc$Institution <- ''
#
#
# SLICKobj$Misc$Cols$MP <- colorspace::diverge_hcl(n=nMPs)
#
#
# ## Factor info
# SLICKobj$OM$Design
# SLICKobj$OM$Factor_Labels <- c('Natural Mortality', 'Steepness')
# SLICKobj$OM$Description[[1]] <- c('Natural Mortality of 0.1', 'Natural Mortality of 0.2', 'Natural Mortality of 0.3')
# SLICKobj$OM$Description[[2]] <- c('Beverton-Holt stock recruitment steepness (resilience) of 0.69',
#                                   'Beverton-Holt stock recruitment steepness (resilience) of 0.80',
#                                   'Beverton-Holt stock recruitment steepness (resilience) of 0.88')
# SLICKobj$OM$Codes[[1]] <- c('0.1', '0.2', '0.3')
# SLICKobj$OM$Codes[[2]] <- c('0.69', '0.80', '0.88')
#
# SLICKobj$OM$Labels[[1]] <- c('M=0.1', 'M=0.2', 'M=0.3')
# SLICKobj$OM$Labels[[2]] <- c('h=0.69', 'h=0.80', 'h=0.88')
#
#
# ## MP Info
#
# MPs_keep <- c('CC10000', 'EA1_a', 'SPFox_a', 'SPS_a',
#               'WA1_a', 'CI1_a', 'CE_a', 'CE25_a',
#               'SPS25_a', 'SPS_b', 'SPS25_b', 'SPS_c',
#               'SPS25_c')
#
#
# SLICKobj$MP$Codes <- MPs_keep
# SLICKobj$MP$Labels <- MPs_keep
#
# SLICKobj$MP$Description <- c('Constant TAC at 10,000 t',
#                              'Index ratio method using the SP, MO, and PO indices, smoothed and scaled by the inverse variance before averaging. Tuned to PGK_short = 0.51',
#                              'Fox Surplus Production with a HCR. Tuned to PGK_short=0.51',
#                              'Schaefer Surplus Production with a HCR. Tuned to PGK_short=0.51',
#                              'Index ratio method using the CA, US, CT, and JP indices, smoothed and scaled by the inverse variance before averaging. Tuned to PGK_short = 0.51',
#                              'Index ratio method using the Combined Index, smoothed and scaled by the inverse variance before averaging. Tuned to PGK_short = 0.51',
#                              'Constant Exploitation Rate. Tuned to PGK_short = 0.51',
#                              'Constant Exploitation Rate with a maximum absolute change in TAC of 25%. Tuned to PGK_short = 0.51',
#                              'Schaefer Surplus Production with a HCR with a maximum absolute change in TAC of 25%. Tuned to PGK_short=0.51',
#                              'Schaefer Surplus Production with a HCR. Tuned to PGK_short=0.60',
#                              'Schaefer Surplus Production with a HCR with a maximum absolute change in TAC of 25%. Tuned to PGK_short=0.60',
#                              'Schaefer Surplus Production with a HCR. Tuned to PGK_short=0.70',
#                              'Schaefer Surplus Production with a HCR with a maximum absolute change in TAC of 25%. Tuned to PGK_short=0.70'
# )
#
#
# # Determine Performance Metrics ----
#
# ## Deterministic ----
#
# SLICKobj$Perf$Det$Codes <- c("PGK_short", "PGK_med", 'PGK_long',
#                               'TAC1', "AvTAC_short", 'AvTAC_med', 'AvTAC_long',
#                               "VarTAC25")
#
# SLICKobj$Perf$Det$Labels <- SLICKobj$Perf$Det$Codes
#
#
# SLICKobj$Perf$Det$Description <- c('Prob. Green Zone of Kobe Space (2024-2033)',
#                                    'Prob. Green Zone of Kobe Space (2034-2043)',
#                                    'Prob. Green Zone of Kobe Space (2044-2053)',
#                                    'Median TAC relative to the highest TAC in 2024. ',
#                                    'Median TAC relative to the highest TAC in 2024-2033',
#                                    'Median TAC relative to the highest TAC in any simulation in 2034-2043 ',
#                                    'Median TAC relative to the highest TAC in any simulation in 2044-2053',
#                                    'Prob. average absolute change in TAC <25%')
#
# # PGK_short
# for (i in 1:nOM) {
#   SLICKobj$Perf$Det$Values[i,,1] <- PGK_short(MSElist[[i]])@Mean * 100
# }
#
# # PGK_med
# for (i in 1:nOM) {
#   SLICKobj$Perf$Det$Values[i,,2] <- PGK_med(MSElist[[i]])@Mean  * 100
# }
#
# # PGK_long
# for (i in 1:nOM) {
#   SLICKobj$Perf$Det$Values[i,,3] <- PGK_long(MSElist[[i]])@Mean * 100
# }
#
# # TAC1
# for (i in 1:nOM) {
#   PM <- TAC1(MSElist[[i]])
#   PM@Mean <- PM@Mean/max(PM@Mean)
#   SLICKobj$Perf$Det$Values[i,,4] <-  PM@Mean * 100
# }
#
# # AvTAC_short
# for (i in 1:nOM) {
#   PM <- AvTAC_short(MSElist[[i]])
#   PM@Mean <- PM@Mean/max(PM@Mean)
#   SLICKobj$Perf$Det$Values[i,,5] <-  PM@Mean * 100
# }
#
# # AvTAC_med
# for (i in 1:nOM) {
#   PM <- AvTAC_med(MSElist[[i]])
#   PM@Mean <- PM@Mean/max(PM@Mean)
#   SLICKobj$Perf$Det$Values[i,,6] <-  PM@Mean * 100
# }
#
# # AvTAC_long
# for (i in 1:nOM) {
#   PM <- AvTAC_long(MSElist[[i]])
#   PM@Mean <- PM@Mean/max(PM@Mean)
#   SLICKobj$Perf$Det$Values[i,,7] <-  PM@Mean * 100
# }
#
# # Var25
# for (i in 1:nOM) {
#   pm <- VarC(MSElist[[i]])
#   SLICKobj$Perf$Det$Values[i,,8] <- apply(pm@Stat < 0.25, 2, mean) * 100
# }
#
#
# # SLICKobj$Perf$Det$RefPoints <- c(0.51, 0.51, 0.51,
# #                                  NA, NA, NA, NA,
# #                                  NA)
# #
# # SLICKobj$Perf$Det$RefNames
#
# ## Stochastic ----
#
# SLICKobj$Perf$Stoch$Codes <- c("FGK_short", "FGK_med", 'FGK_long',
#                                 'TAC1', "AvTAC_short", 'AvTAC_med', 'AvTAC_long')
# SLICKobj$Perf$Stoch$Labels <- SLICKobj$Perf$Stoch$Codes
#
#
# SLICKobj$Perf$Stoch$Description <- c('Fraction of years in  Green Zone of Kobe Space (2024-2033)',
#                                    'Fraction of years in  Green Zone of Kobe Space (2034-2043)',
#                                    'Fraction of years in  Green Zone of Kobe Space (2044-2053)',
#                                    'TAC (t) (2024)',
#                                    'TAC (t) (2024-2033)',
#                                    'TAC (t) (2034-2043)',
#                                    'TAC (t) (2044-2053)'
#                                    )
#
#
#
# # PGK_short
# for (i in 1:nOM) {
#   PM <- PGK_short(MSElist[[i]])
#   SLICKobj$Perf$Stoch$Values[,i,,1] <- apply(PM@Stat, c(1,2), mean)  * 100
# }
#
# # PGK_med
# for (i in 1:nOM) {
#   PM <- PGK_med(MSElist[[i]])
#   SLICKobj$Perf$Stoch$Values[,i,,2] <- apply(PM@Stat, c(1,2), mean) * 100
# }
#
# # PGK_long
# for (i in 1:nOM) {
#   PM <- PGK_long(MSElist[[i]])
#   SLICKobj$Perf$Stoch$Values[,i,,3] <- apply(PM@Stat, c(1,2), mean) * 100
# }
#
# # TAC1
# for (i in 1:nOM) {
#   PM <- TAC1(MSElist[[i]])
#   SLICKobj$Perf$Stoch$Values[,i,,4] <- apply(PM@Stat, c(1,2), mean)
# }
#
# # AvTAC_short
# for (i in 1:nOM) {
#   PM <- AvTAC_short(MSElist[[i]])
#   SLICKobj$Perf$Stoch$Values[,i,,5] <- apply(PM@Stat, c(1,2), mean)
# }
#
# # AvTAC_med
# for (i in 1:nOM) {
#   PM <- AvTAC_med(MSElist[[i]])
#   SLICKobj$Perf$Stoch$Values[,i,,6] <- apply(PM@Stat, c(1,2), mean)
# }
#
# # AvTAC_long
# for (i in 1:nOM) {
#   PM <- AvTAC_long(MSElist[[i]])
#   SLICKobj$Perf$Stoch$Values[,i,,7] <- apply(PM@Stat, c(1,2), mean)
# }
#
#
# # # Var25
# # for (i in 1:nOM) {
# #   PM <- VarC(MSElist[[i]])
# #   SLICKobj$Perf$Stoch$Values[,i,,8] <- PM@Stat * 100
# # }
#
#
# ## Projection ----
#
# SLICKobj$Perf$Proj$Codes <- c('SB/SBMSY',
#                                'F/FMSY')
#
# SLICKobj$Perf$Proj$Labels <- SLICKobj$Perf$Proj$Codes
# SLICKobj$Perf$Proj$Description <- c('Spawning Biomass relative to SB<sub>MSY</sub>',
#                                     'Fishing mortality relative to F<sub>MSY</sub>')
#
# year_df <- data.frame(i=-2:30, Year=2021:2053)
# year_df$i2 <- 1:nrow(year_df)
# p.years <- year_df$i2[year_df$Year>=2024]
# # SB/SBMSY
# for (i in 1:nOM) {
#   SLICKobj$Perf$Proj$Values[,i,,1,] <- MSElist[[i]]@SB_SBMSY[,1,,p.years]
# }
#
# # F/MSY
# for (i in 1:nOM) {
#   SLICKobj$Perf$Proj$Values[,i,,2,] <- MSElist[[i]]@F_FMSY[,1,1,,p.years]
# }
#
# # # TAC (t)
# # for (i in 1:nOM) {
# #   SLICKobj$Perf$Proj$Values[,i,,3,] <- apply(MSElist[[i]]@TAC[,,1,,p.years], c(1,3,4), sum)
# # }
# #
# # # TAC (relative)
# # for (i in 1:nOM) {
# #   TAC <- apply(MSElist[[i]]@TAC[,,1,,p.years], c(1,3,4), sum)
# #   msys <- apply(MSElist[[i]]@RefPoint$ByYear$MSY[,,,nyears+p.years], c(1,3,4), sum)
# #
# #   SLICKobj$Perf$Proj$Values[,i,,4,] <- TAC/msys
# # }
#
#
# SLICKobj$Perf$Proj$Times <- 2024:2053
# SLICKobj$Perf$Proj$RefPoints <- list()
# SLICKobj$Perf$Proj$RefPoints[[1]] <- c(1, 0.4)
# SLICKobj$Perf$Proj$RefPoints[[2]] <- 1
# SLICKobj$Perf$Proj$RefNames[[1]] <- c("SBMSY", 'BLim')
# SLICKobj$Perf$Proj$RefNames[[2]] <- 'FMSY'
#
#
# ## State Variables ----
#
# SLICKobj$StateVar$Codes <- c('SB/SBMSY', 'F/FMSY', 'TAC (t)')
# SLICKobj$StateVar$Labels <- SLICKobj$StateVar$Codes
# SLICKobj$StateVar$Description <- c('Spawning Biomass relative to SB<sub>MSY</sub>',
#                                    'Fishing mortality relative to F<sub>MSY</sub>',
#                                    'Total allowable catch (TAC; t)')
#
# totyears <- length(1950:2053)
# SLICKobj$StateVar$Values <- array(NA, dim=c(nsim, nOM, nMPs, nSV, totyears))
#
# # SB/SBMSY
# for (i in 1:nOM) {
#   for (mm in 1:nMPs) {
#     histSB <- apply(MSElist[[i]]@multiHist[[1]][[1]]@TSdata$SBiomass, 1:2, sum)
#     projSB <- MSElist[[i]]@SSB[,1,mm,]
#     SB <- abind::abind(histSB, projSB, along=2)
#     SB_SBMSY <- SB/MSElist[[i]]@RefPoint$ByYear$SSBMSY[,1,mm,]
#     SLICKobj$StateVar$Values[,i,mm,1,] <- SB_SBMSY
#   }
# }
#
# # F/MSY
# for (i in 1:nOM) {
#   for (mm in 1:nMPs) {
#     histF <- apply(MSElist[[i]]@multiHist[[1]][[1]]@TSdata$Find, 1:2, sum)
#     projF <- MSElist[[i]]@FM[,1,1,mm,]
#     Fs <- abind::abind(histF, projF, along=2)
#     F_FMSY <- Fs/MSElist[[i]]@RefPoint$ByYear$FMSY[,1,mm,]
#     SLICKobj$StateVar$Values[,i,mm,2,] <- F_FMSY
#   }
# }
#
# # TAC (t)
# for (i in 1:nOM) {
#   for (mm in 1:nMPs) {
#     histTAC <- apply(MSElist[[i]]@multiHist[[1]][[1]]@TSdata$Landings, 1:2, sum) +
#       apply(MSElist[[i]]@multiHist[[2]][[1]]@TSdata$Landings, 1:2, sum)
#     projTAC <- apply(MSElist[[i]]@TAC[,,1,mm,], c(1,3), sum)
#     TACs <- abind::abind(histTAC, projTAC, along=2)
#     SLICKobj$StateVar$Values[,i,mm,3,] <- TACs
#   }
# }
#
#
# # # TAC (relative)
# # for (i in 1:nOM) {
# #   MSYs <- apply(MSElist[[i]]@RefPoint$ByYear$MSY, c(1,3,4), sum)
# #   for (mm in 1:nMPs) {
# #     SLICKobj$StateVar$Values[,i,mm,4,] <-  SLICKobj$StateVar$Values[,i,mm,3,]/MSYs[,mm,]
# #   }
# # }
#
# SLICKobj$StateVar$TimeNow <- 2023
# SLICKobj$StateVar$Times <- 1950:2053
#
# SLICKobj$StateVar$RefPoints <- list(
#   c(1, 0.4),
#   c(1),
#   NA
# )
# SLICKobj$StateVar$RefNames <- list(
#   c('SBMSY', 'Blim'),
#   c('FMSY'),
#   NA
# )
#
#
#
#
# saveRDS(SLICKobj, 'Slick_objects/NSWO.slick')
#
# #
# saveRDS(SLICKobj, 'C:/Users/User/Documents/GitHub/Slick/inst/shiny_apps/Slick/data/case_studies/NSWO.slick')




