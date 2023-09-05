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
TS_results <- readRDS('./data/TS_results.rda')
Violin_results <- readRDS('./data/Violin_results.rda')

# PM_results <- readRDS('C:/Users/User/Documents/GitHub/nswo-mse/inst/shiny_apps/SWOMSE/data/PM_results.rda')
# TS_results <- readRDS('C:/Users/User/Documents/GitHub/nswo-mse/inst/shiny_apps/SWOMSE/data/TS_results.rda')

# PM_results %>% filter(Model=='R3b') %>% distinct(MP_name)
# PM_results %>% filter(Model=='R3a') %>% distinct(MP_name)
#
# TS_results %>% filter(Model=='R3a') %>% distinct(MP_name)
# TS_results %>% filter(Model=='R3b') %>% distinct(MP_name)
#

OMnames <- unique(PM_results$Model)
OMnames <- factor(OMnames, levels=c('Reference', 'R0', 'R1', 'R2', 'R3a', 'R3b', 'R4'), ordered = TRUE)

metadf <- data.frame(OMnames=OMnames) %>% arrange(OMnames)

MPnames <- PM_results$MP_name %>% unique()
allMPs <-   PM_results$MP %>% unique()


# Trade-Off Options
allPMs <- unique(PM_results$PM) %>% sort()

tradeoffPMs <- data.frame(TO=1:4,
                          x=c('PGK_short', 'PGK_med', 'nLRP', 'VarC'),
                          y=c('AvTAC_short', 'AvTAC_med', 'AvTAC_med', 'AvTAC_med'))



# Quilt plot options
quiltPMs <- c('AvTAC_med', 'AvTAC_short', 'TAC1', 'PGK_med', 'VarC', 'PNOF')
