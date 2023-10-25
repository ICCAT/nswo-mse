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

short_list_mps <- c('CE', 'SPSSFox','SPSSFox2', 'MCC5', 'MCC7')
short_list_mps <- paste(short_list_mps, rep(c('b', 'c'), each=length(short_list_mps)), sep='_')


# Trade-Off Options
allPMs <- unique(PM_results$PM) %>% sort()

tradeoffPMs <- data.frame(TO=1:4,
                          x=c('PGK_short', 'PGK_med', 'nLRP', 'VarC'),
                          y=c('AvTAC_short', 'AvTAC_med', 'AvTAC_med', 'AvTAC_med'))



# Quilt plot options
quiltPMs <- c('AvTAC_short', 'AvTAC_med', 'AvTAC_long', 'TAC1',
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

short_list_mps <- c('CE', 'SPSSFox','SPSSFox2', 'MCC5', 'MCC7')
short_list_mps <- paste(short_list_mps, rep(c('b', 'c'), each=length(short_list_mps)), sep='_')

short_list_mps <- list(default=short_list_mps)
short_list <- short_list_mps
