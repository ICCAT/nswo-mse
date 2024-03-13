library(dplyr)
library(DT)
library(ggplot2)
library(ggrepel)
library(readxl)
library(shinyBS)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyjs)
library(shinyalert)
library(cowplot)
library(tidyr)
library(SWOMSE)
library(ggpp)


for (fl in list.files("./source")) source(file.path("./source", fl))


PM_results <- readRDS('./data/PM_results.rda')
TS_results <- readRDS('./data/summary_TS_results.rda')
kobe_results <- readRDS('./data/kobe_results.rda')
Violin_results <- readRDS('./data/Violin_results.rda')

DiffMngCycle <- readRDS('./data/DiffMngCycle.rda')
MinTACChange <- readRDS('./data/MinTACChange.rda')

OMnames <- unique(PM_results$Model)
OMnames <- factor(OMnames, levels=c('Reference', 'R0', 'R1', 'R2', 'R3a', 'R3b', 'R4'), ordered = TRUE)

metadf <- data.frame(OMnames=OMnames) %>% arrange(OMnames)

MPnames <- PM_results$MP_name %>% unique()
allMPs <-   PM_results$MP %>% unique()

#short_list_mps <- c('CE', 'SPSSFox','SPSSFox2', 'MCC5', 'MCC7')
#short_list_mps <- paste(short_list_mps, rep(c('b', 'c'), each=length(short_list_mps)), sep='_')

short_list_mps <- c('CE_b',"SPSSFox_b","SPSSFox2_b","MCC5_b","MCC5_c","MCC7_b","MCC7_c")

# Trade-Off Options
allPMs <- unique(PM_results$PM) %>% sort()

tradeoffPMs <- data.frame(TO=1:4,
                          x=c('PGK_short', 'PGK_med', 'nLRP', 'VarC'),
                          y=c('AvTAC_short', 'AvTAC_med', 'AvTAC_med', 'AvTAC_med'))



# Quilt plot options
quiltPMs <- c('AvTAC_short', 'AvTAC_med', 'AvTAC_long', #'TAC1',
              'nLRP', 'PNOF', 'PGK_short', 'PGK_med', 'PGK', 'VarC')



# Initial state
pPM_results <- PM_results
pTS_results <- TS_results


fail_MPs <- PM_results %>% filter(Model=='Reference', PM=='LRP', Value>0.15) %>%
      distinct(MP)

pPM_results <- pPM_results %>% filter(!MP %in% fail_MPs$MP)
pTS_results <- pTS_results %>% filter(!MP %in% fail_MPs$MP)


# PGK med
fail_MPs_PGK <- PM_results %>% filter(Model=='Reference', PM=='PGK_med', Value<0.50) %>%
  distinct(MP)



pPM_results <- pPM_results %>% filter(!MP %in% fail_MPs_PGK$MP)
pTS_results <- pTS_results %>% filter(!MP %in% fail_MPs_PGK$MP)

  # MPs
pPM_results <- pPM_results %>% filter(MP %in% short_list_mps)
pTS_results <- pTS_results %>% filter(MP %in% short_list_mps)

pkobe_results <- kobe_results %>% filter(MP %in% unique(pPM_results$MP))
pViolin_results <- Violin_results %>% filter(MP %in% unique(pPM_results$MP))

#short_list_mps <- c('CE', 'SPSSFox','SPSSFox2', 'MCC5', 'MCC7')
#short_list_mps <- paste(short_list_mps, rep(c('b', 'c'), each=length(short_list_mps)), sep='_')
short_list_mps <- c('CE_b',"SPSSFox_b","SPSSFox2_b","MCC5_b","MCC5_c","MCC7_b","MCC7_c")

short_list_mps <- list(default=short_list_mps)
short_list <- short_list_mps



### CMP Project ####

fls <- list.files('../../../CMPs')
print(fls)

for (fl in fls) source(file.path('../../../CMPs', fl))


MCC5_60 <- MCC5_b


MCC5_70 <- MCC5_c


MCC7_60 <- MCC7_b

MCC7_70 <- MCC7_c

CE_60 <- CE_c

CMPs <- c('MCC5_60',
          'MCC5_70',
          'MCC7_60',
          'MCC7_70',
          'CE_60')


Data <- SWOMSE::SWOData

data <- data.frame(Year=Data@Year,
                   Catch=Data@Cat[1,],
                   Index=Data@Ind[1,],
                   Type='Reported',
                   Period='Historical')


## add extra years
data <- rbind(data,
              data.frame(Year=2021:2023,
                         Catch=Catchdf$Catch,
                         Index=NA,
                         Type=c('Reported', 'Assumed', 'Assumed'),
                         Period='Historical'
              ))

# update index
dat = read.csv("../../../TAC1/SWOForTom.csv")

data$Index[data$Year %in% dat$Year] <- dat$CombinedIndex
data$Index[data$Year ==2023] <-data$Index[data$Year ==2022]

# data <- data %>% dplyr::filter(is.na(Index) ==FALSE)


