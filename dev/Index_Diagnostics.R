
library(SWOMSE)
library(ggplot2)
library(MSEgraph)

RefOMs <- OM_DF %>% filter(Class=='Reference')

## Historical Simulations - 10 Sims ##
multiHistList <- list()

for (i in 1:nrow(RefOMs)) {
  multiHistList[[i]] <- readRDS(paste0('dev/RefOM_Hist/', RefOMs$OM.object[i], '.mhist'))
}

## Testing MPs ##
NF <- function(x, Data, ...) {
  rec <- new('Rec')
  rec@TAC <- 10
  rec
}
class(NF) <- 'MP'

CC_13200 <- function(x, Data, ...) {
  rec <- new('Rec')
  rec@TAC <- 13200
  rec
}
class(CC_13200) <- 'MP'

CC_9900 <- function(x, Data, ...) {
  rec <- new('Rec')
  rec@TAC <- 9900
  rec
}
class(CC_9900) <- 'MP'


MPs <- c('NF', 'CC_13200', 'CC_9900', 'FMSYref', 'FMSYref50', 'SP1_a', 'IR1_a')

rerun <- FALSE
## Forward Projections ##
if (rerun) {
  MSElist <- list()
  for (i in 1:nrow(RefOMs)) {
    MSElist[[i]] <- ProjectMOM( multiHistList[[i]], MPs=MPs)
  }
  saveRDS(MSElist, 'dev/refOM_Hist/testMPs.mmse')
} else {
  MSElist <- readRDS('dev/refOM_Hist/testMPs.mmse')
}


## Add Historical Object

for (i in 1:nrow(RefOMs)) {
  multiHist <- readRDS(paste0('dev/RefOM_Hist/', RefOMs$OM.object[i], '.mhist'))
  MSElist[[i]]@multiHist <- multiHist
}



## ----------- Index Diagnostics ---------------------------------------------##


# ---- Plot Historical Biomass and Indices ----

## Combined Index
for (om in 1:length(RefOMs$OM.object)) {
  MMSE <- MSElist[[om]]

  p <- Compare_Index(MMSE, plot=FALSE)
  DF <- p$DF
  DF$OM <- RefOMs$OM.object[om]
  sims <- unique(DF$Sim)
  mps <- unique(DF$MP)[1]
  DF <- DF %>% filter(Period=='Historical', Sim==min(sims), MP==mps)

  p <- ggplot(DF, aes(x=Year)) +
    geom_line(aes(y=BSt)) +
    geom_line(aes(y=ISt), color='blue') +
    theme_bw() +
    labs(y='Standardized Biomass/Index') +
    guides(color='none')


  DF$log_I_err <- log(DF$I_err)
  DF <- DF %>% mutate(negative=ifelse(log_I_err<0, TRUE, FALSE))

  p2 <- ggplot(DF, aes(x=Year, fill=negative)) +
    geom_bar(aes(y=log_I_err), stat='identity') +
    guides(fill='none') +
    labs(y='Log deviations') +
    theme_bw()

  p3 <- ggplot(DF, aes(x=Year)) +
    geom_line(aes(y=I_err)) +
    guides(fill='none') +
    labs(y='Deviations') +
    geom_hline(yintercept = 1, linetype=2) +
    theme_bw()

  pout <- cowplot::plot_grid(p, p2, p3, nrow=1,
                             labels = c('a)', 'b)', 'c)'))

  nm <- paste0(RefOMs$OM.object[om], '_hist_index')

  ggsave(paste0('dev/Index_Diagnostic/RefOMs/', nm, '.png'), pout,
         width=9, height=3)

}


## Other Indices
all_indices <- names(AddIndList())

indices <- c('SPN_1', "CAN_3", 'JPN_LATE_5', 'CHT_LATE_8', 'MOR_9',
             'US_Survey_12', 'PORT_Survey_13')

for (om in 1:length(RefOMs$OM.object)) {
  MMSE <- MSElist[[om]]
  for (index in indices) {
    p <- Compare_Additional_Index(MMSE, index)
    DF <- p$DF
    DF$OM <- RefOMs$OM.object[om]
    sims <- unique(DF$Sim)
    mps <- unique(DF$MP)[1]
    DF <- DF %>% filter(Period=='Historical', Sim==min(sims), MP==mps)

    p <- ggplot(DF, aes(x=Year)) +
      geom_line(aes(y=BSt)) +
      geom_line(aes(y=ISt), color='blue') +
      theme_bw() +
      labs(y='Standardized Biomass/Index') +
      guides(color='none')

    DF$log_I_err <- log(DF$I_err)
    DF <- DF %>% mutate(negative=ifelse(log_I_err<0, TRUE, FALSE))

    p2 <- ggplot(DF, aes(x=Year, fill=negative)) +
      geom_bar(aes(y=log_I_err), stat='identity') +
      guides(fill='none') +
      labs(y='Log deviations') +
      theme_bw()

    p3 <- ggplot(DF, aes(x=Year)) +
      geom_line(aes(y=I_err)) +
      guides(fill='none') +
      labs(y='Deviations') +
      geom_hline(yintercept = 1, linetype=2) +
      theme_bw()

    pout <- cowplot::plot_grid(p, p2, p3, nrow=1,
                               labels = c('a)', 'b)', 'c)'))

    nm <- paste0(RefOMs$OM.object[om], '_', index, '_hist_index')

    ggsave(paste0('dev/Index_Diagnostic/RefOMs/', nm, '.png'), pout,
           width=9, height=3)
  }
}


## Plot Projection Indices ----

## Combined Index
for (om in 1:length(RefOMs$OM.object)) {
  MMSE <- MSElist[[om]]

  p <- Compare_Index(MMSE, plot=FALSE)

  pout <- p$p + labs(title='') +
    guides(color='none')

  nm <- paste0(RefOMs$OM.object[om], '_proj_index')

  ggsave(paste0('dev/Index_Diagnostic/RefOMs/', nm, '.png'), pout,
         width=12, height=18)
}




## Other Indices
for (om in 1:length(RefOMs$OM.object)) {
  MMSE <- MSElist[[om]]
  for (index in indices) {
    p <- Compare_Additional_Index(MMSE, index,plot=FALSE)
    pout <- p$p + labs(title='') +
      guides(color='none')

    nm <- paste0(RefOMs$OM.object[om], '_', index, '_proj_index')

    ggsave(paste0('dev/Index_Diagnostic/RefOMs/', nm, '.png'), pout,
           width=12, height=18)
  }
}




## ---- Render Markdown ----
library(rmarkdown)
for (om in 1:length(RefOMs$OM.object)) {
  message(om)
  output_file <- paste0(RefOMs$OM.object[om], '.html')
  render("dev/Index_Diagnostic/Index_Diagnostic_Report.Rmd",
            output_file=output_file,
            params=list(MOM=RefOMs$OM.object[om]))
}


