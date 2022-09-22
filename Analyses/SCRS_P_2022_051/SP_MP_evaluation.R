library(SWOMSE)

# Reference OMs ----
## Simulate Historical Fisheries ----

OMs <- OM_DF %>% filter(Class=="Reference")

for (i in 1:nrow(OMs)) {
  message(i, '/', nrow(OMs))
  name <- OMs$OM.object[i]
  MOM <- get(name)
  multiHist <- try(SimulateMOM(MOM, parallel = TRUE))
  saveRDS(multiHist, paste0('results/Reference/', name, '.hist'))
  if(exists('multiHist')) rm(multiHist)
}

## Closed Loop Projections ----
MPs <- c('SP_1', 'SP_2', 'SP_3',
         'SP_Fox_1', 'SP_Fox_2', 'SP_Fox_3',
         'SP_SS_1', 'SP_SS_2', 'SP_SS_3')

for (i in 1:nrow(OMs)) {
  message(i, '/', nrow(OMs))
  name <- OMs$OM.object[i]
  MOM <- get(name)
  multiHist <- readRDS(paste0('results/Reference/', name, '.hist'))

  MMSE <- try(ProjectMOM(multiHist, MPs=MPs, parallel = TRUE))
  saveRDS(MMSE, paste0('results/Reference/', name, '.mmse'))
  if(exists('MMSE')) rm(MMSE)
}

# R3 OMs ----
## Simulate Historical Fisheries ----
OMs <- OM_DF %>% filter(Class=="R3. increase q")

for (i in 1:nrow(OMs)) {
  message(i, '/', nrow(OMs))
  name <- OMs$OM.object[i]
  MOM <- get(name)
  multiHist <- try(SimulateMOM(MOM, parallel = TRUE))
  saveRDS(multiHist, paste0('results/Robustness/R3_increase_q/', name, '.hist'))
  if(exists('multiHist')) rm(multiHist)
}

## Closed Loop Projections ----
MPs <- c('SP_1', 'SP_2', 'SP_3',
         'SP_Fox_1', 'SP_Fox_2', 'SP_Fox_3',
         'SP_SS_1', 'SP_SS_2', 'SP_SS_3')

for (i in 1:nrow(OMs)) {
  message(i, '/', nrow(OMs))
  name <- OMs$OM.object[i]
  MOM <- get(name)
  multiHist <- readRDS(paste0('results/Robustness/R3_increase_q/', name, '.hist'))

  MMSE <- try(ProjectMOM(multiHist, MPs=MPs, parallel = TRUE))
  saveRDS(MMSE, paste0('results/Robustness/R3_increase_q/', name, '.mmse'))
  if(exists('MMSE')) rm(MMSE)
}

