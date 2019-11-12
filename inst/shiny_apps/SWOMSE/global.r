library(SWOMSE)
library(dplyr)



# Generate Table of OM Assumptions
SWOM@Source

# Base Case
# TODO: Check assumptions/parameters for base case
makeDF <- function(grid.dir) {
  DFbc <- data.frame(Num=0,
                     Name="OM_base_case",
                     M=unique(OM_base_case@M),
                     sigmaR=0.2,
                     steepness=0.88,
                     cpuecv=0.3,
                     ess=2,
                     llq=1,
                     env=-4,
                     stringsAsFactors = FALSE
  )

  # uncertainty grid

  ss.dirs <- list.dirs(grid.dir)
  count <- 0; outlist <- list()
  for (i in 1:length(ss.dirs)) {
    ss.dir <- file.path(grid.dir, ss.dirs[i])

    tdf <- stringr::str_extract_all(basename(ss.dir),"\\(?[0-9,.]+\\)?")[[1]]
    tdf <- matrix(as.numeric(tdf), nrow=1, ncol=9) %>% data.frame(stringsAsFactors = FALSE)
    colnames(tdf) <- c("Num", 'M', "sigmaR", 'steepness', 'cpuecv', 'ess', 'llq', 'env', 'num2')
    tdf <- tdf %>% select('Num', 'M', "sigmaR", 'steepness', 'cpuecv', 'ess', 'llq', 'env')

    is.neg <- stringr::str_extract(basename(ss.dir), 'env-')
    if (!is.na(is.neg)) tdf$env <- -tdf$env

    OMNums <- stringr::str_extract_all(SWOM@Source, "\\(?[0-9,.]+\\)?") %>% unlist() %>%
      as.numeric()

    if (tdf$Num %in% OMNums) {
      tdf$Name <- paste0('OM_', tdf$Num)
      count <- count+1
      outlist[[count]] <- tdf
    }
  }
  DFuncert <- do.call('rbind', outlist)

  DF <- bind_rows(DFbc, DFuncert)
  DF <- arrange(DF, Num)

  saveRDS(DF, "inst/shiny_apps/SWOMSE/data/OMTable.rds")
}
# Run function to save parameter assumption table
# makeDF(grid.dir='C:/Users/Adrian/Dropbox/SWO_MSE/OMs/SS/2018_GRID/grid')

OMAssumption <- readRDS('data/OMTable.rds')
OMAssumption$env[OMAssumption$env<0] <- FALSE
OMAssumption$env[OMAssumption$env>0] <- TRUE
OMAssumption$env <- as.logical(OMAssumption$env)

OMAssumption$llq[OMAssumption$llq == 1] <- FALSE
OMAssumption$llq[OMAssumption$llq>1] <- TRUE
OMAssumption$llq <- as.logical(OMAssumption$llq)

OMnames <- OMAssumption$Name

PMnames <- avail("PM", 'all')
