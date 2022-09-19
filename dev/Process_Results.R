library(SWOMSE)

# ---- Functions to Add to Package ----


# ---- Projection Plots
MMSE <- readRDS('results/Reference/MOM_130.mmse')
MMSE <- readRDS('results/Reference/MOM_131.mmse')
MMSE <- readRDS('results/Reference/MOM_132.mmse')

Proj_plot(MMSE)
Proj_plot(MMSE, 'Catch')

Proj_plot(MMSE, 'F_FMSY')


# ---- Performance Metrics

PMs <- avail('PM', 'SWOMSE')

summary(MMSE, PMs)

# ---- Trade-Off Plots

cols <- c('red', 'blue', 'green')
cols <- rep(cols, each=3)
TradePlot(MMSE,
          'Safety_M', 'Yield_M',
          'Status_M', 'Yield_M',
          'Stability', 'Yield_M',
          'Yield_S', 'Yield_M',
          Lims=c(0.95,0,
                 0.95, 0,
                 0,0,
                 0,0),
          cols=cols)

# check Kobe results for SP_SS_1



# ---- Kobe Plots by MP

Kobe <- function(MMSE) {
  TS_DF_1 <- get_TS_results(MMSE, 'SB_SBMSY')
  TS_DF_2 <- get_TS_results(MMSE, 'F_FMSY')

  DF <- dplyr::bind_rows(TS_DF_1, TS_DF_2)

  DF <- DF %>% tidyr::pivot_wider(., names_from = 'Name', values_from = 'Value')

  ggplot(DF, aes(x=F_FMSY, y=SB_SBMSY, group=Sim)) +
    facet_wrap(~MP) +
    geom_line()
}






combine_MMSE <- function(MMSE_list) {

}


mmse_files <- list.files('results/Reference/', '.mmse')

i <- 1

MMSE <- readRDS(file.path('results/Reference/',  mmse_files[i]))



get_OM_details(MMSE)





MMSE@Misc$MOM@Name
MMSE@Name
OM_DF %>% head()



MMSEobj <- MMSE

MMSEobj<- list()
MMSEobj[[1]] <- readRDS(file.path('results/Reference/', mmse_files[1]))
MMSEobj[[2]] <- readRDS(file.path('results/Reference/', mmse_files[2]))
MMSEobj[[3]] <- readRDS(file.path('results/Reference/', mmse_files[4]))

# Projections ----

pSB_SBMSY(MMSE)
pSB_SBMSY(MMSEobj)

pSB_SBMSY <- function(MMSEobj) {
  if (inherits(MMSEobj, 'list')) {
    # combine together
    nMMSE <- length(MMSEobj)
    if(!all(unlist(lapply(MMSEobj, inherits, 'MMSE'))))
      stop('All elements in list must be class `MMSE`')
    MMSE <- MMSEobj[[1]]
    for (i in 2:nMMSE) {
      MMSE@SB_SBMSY <- abind::abind( MMSE@SB_SBMSY, MMSEobj[[i]]@SB_SBMSY, along=1)
    }
    MMSEobj <- MMSE
  }

  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')

  SB_SBMSY <- MMSEobj@SB_SBMSY[, 1,, ]
  median <- apply(SB_SBMSY, 2:3, median)
  upper <- apply(SB_SBMSY, 2:3, quantile, 0.95)
  lower <- apply(SB_SBMSY, 2:3, quantile, 0.05)
  MPs <- MMSEobj@MPs[[1]]
  nMPs <- length(MPs)
  fst.proj.yr <- MMSEobj@PPD[[1]][[1]][[1]]@LHYear+1
  lst.proj.yr <- fst.proj.yr+MMSEobj@proyears-1
  Years <- fst.proj.yr:lst.proj.yr
  df <- data.frame(MP=MPs,
                   Year=rep(Years, each=nMPs),
                   Median=as.numeric(median),
                   Upper=as.numeric(upper),
                   Lower=as.numeric(lower))
  df$MP <- factor(df$MP, levels=MPs, ordered = TRUE)

  ggplot(df, aes(x=Year)) +
    facet_grid(~MP) +
    expand_limits(y=0) +
    geom_hline(yintercept = 1, linetype=2) +
    geom_hline(yintercept = 0.5, linetype=3) +
    geom_ribbon(aes(ymin=Lower, ymax=Upper, fill=MP), alpha=0.6) +
    geom_line(aes(y=Median, color=MP), size=1.2) +
    theme_classic() +
    labs(y='SB/SB_MSY') +
    guides(fill='none', color='none')

}



# Kobe Plot ----
