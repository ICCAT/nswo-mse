library(SWOMSE)

# Reference OMs ----

MMSE.dir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/Results/Reference'
fls <- list.files(MMSE.dir, pattern='.mmse')
img.dir <- 'img/MP_Performance/SCRS_P_2022_051'

MMSElist <- list()
for (i in seq_along(fls)) {
  nm <- strsplit(fls[i], '.mmse')[[1]][1]
  MMSElist[[nm]] <- readRDS(file.path(MMSE.dir, fls[i]))
}


combine_MMSE <- function(MMSElist, name) {
  classess <- lapply(MMSElist, class) %>% unlist()
  if (!all(classess %in% 'MMSE'))
    stop('MMSElist must be a list of `MMSE` objects')

  MMSE_out <- MMSElist[[1]]
  MMSE_out@nsim <- lapply(MMSElist, slot, 'nsim') %>% unlist() %>% sum()

  slots <- c('SB_SBMSY', 'F_FMSY', 'N', 'B', 'SSB', 'VB', 'Catch', 'Removals')
  for (sl in slots) {
    vals <- lapply(MMSElist, slot, sl)
    slot(MMSE_out, sl) <- do.call(abind:::abind, c(vals, along = 1))
  }
  MMSE_out@Name <- name
  MMSE_out
}



MMSE <- combine_MMSE(MMSElist, 'Reference')

p1 <- Proj_plot(MMSE)
ggsave(file.path(img.dir, 'Reference_proj_B_BMSY.png'),p1, width=8, height=6)

## Safety ----
Safety_S(MMSE)
Safety_M(MMSE)

## Status ----
Status_S(MMSE)
Status_M(MMSE)


apply(tt@Prob, 2, median)
apply(tt@Prob, 2, mean)

apply(MMSE@SB_SBMSY[,1,,1:10], 2, median)

Yield_S(MMSE)
Yield_M(MMSE)

apply(obj$Perf$Det$Values[1:9,,3], 2, median)
tt <- Status_S(MMSElist[[1]])

apply(tt@Prob, 2, mean)

## Stability ----
Stability(MMSE)


## Short-term and Long-term Yield ----
# drop _1 MPs
MMSE2 <- MMSE
mps <- MMSE2@MPs[[1]]
ind <- !(grepl('_1', mps))
keep.mps <- mps[ind]

p2 <- Proj_plot(MMSE, 'Catch', MPs=keep.mps, ncol=2)
ggsave(file.path(img.dir, 'Reference_proj_Catch.png'),p2, width=8, height=6)

MMSE2@MPs[[1]] <- keep.mps
MMSE2@Catch <- MMSE2@Catch[,,,ind,, drop=FALSE]
MMSE2@nMPs <- length(MMSE2@MPs[[1]])

library(RColorBrewer)
cols <- RColorBrewer::brewer.pal(6,'Dark2')

p3 <- TradePlot(MMSE2,
          'Yield_S', 'Yield_M',
          Lims=c(0,0),
          cols=cols)
ggsave(file.path(img.dir, 'Reference_T_Catch.png'),p3[[2]][[1]], width=5, height=5)



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




Proj_plot(MMSE)
Proj_plot(MMSE, 'Catch')

Proj_plot(MMSE, 'F_FMSY')


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


Yrs <- MMSElist[[1]]@PPD[[1]][[1]][[1]]@Year
Years <- 2021:2050
cbind(Years,MMSElist[[1]]@TAC[1,1,1,1,])
