library(MSEgraph)
library(dplyr)
library(SWOMSE)

RefOMs <- OM_DF %>% filter(Class=='Reference')

Index_select_dir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/OM_objects/Index_Selectivity'

out.dir <- 'Analyses/SCRS_P_2023_064'
source(file.path(out.dir, 'functions.r'))



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


MPs <- c('NF', 'CC_13200', 'FMSYref')

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


for (i in 1:nrow(RefOMs)) {
  multiHist <- readRDS(paste0('dev/RefOM_Hist/', RefOMs$OM.object[i], '.mhist'))
  MSElist[[i]]@multiHist <- multiHist
}


OM_DF %>% filter(Class=='Reference')

## Combined Index -----
## Historical Fits - Calculate Observation Error ##
statsList <- list()
for (i in 1:nrow(RefOMs)) {
  MMSE <- MSElist[[i]]
  p <- Hist_CI_plot(MMSE, MOM=RefOMs$OM.object[i])

  tt <-  p$stats
  tt$Model <- RefOMs$OM.object[i]
  statsList[[i]] <- tt

  nm <- paste0(RefOMs$OM.object[i], 'CI_hist.png')
  ggsave(file.path(out.dir, nm), p$p, width=8, height=6)
}

stats <- do.call('rbind', statsList)


## Projection Plots
MMSE <- MSElist[[1]]
p <- Compare_Index(MMSE, plot=FALSE, MPs=MPs, nsims=10)

Resids <- p$DF

ggplot(Resids, aes(x=Year, y=I_err, color=Period)) +
  facet_wrap(~Sim, nrow=2) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw() +
  geom_hline(yintercept = 1, linetype=2) +
  labs(y='Residual')
ggsave(file.path(out.dir, 'MOM_133_residuals.png'), width=12, height=5)


Resids <- p$DF %>% filter(Period=='Projection')

ggplot(Resids, aes(x=Year, y=I_err)) +
  facet_wrap(~Sim, nrow=2) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw() +
  geom_hline(yintercept = 1, linetype=2) +
  labs(y='Residual')

ggsave(file.path(out.dir, 'MOM_133_residuals2.png'), width=12, height=5)

for (i in 1:nrow(RefOMs)) {
  MMSE <- MSElist[[i]]
  p <- Compare_Index(MMSE, plot=FALSE, MPs=MPs, nsims=3)
  nm <- paste0(RefOMs$OM.object[i], 'CI_proj.png')

  ggsave(file.path(out.dir, nm), p$p, width=8, height=6)
}

for (i in 1:nrow(RefOMs)) {
  MMSE <- MSElist[[i]]
  p <- Compare_Index(MMSE, plot=FALSE, MPs=MPs, nsims=3, Years=2021:2053)
  p <- p$p + guides(color='none')
  nm <- paste0(RefOMs$OM.object[i], 'CI_proj2.png')

  ggsave(file.path(out.dir, nm), p, width=8, height=6)
}



dflist <- list()
for (i in 1:nrow(RefOMs)) {
  MMSE <- MSElist[[i]]
  p <- Compare_Index(MMSE, plot=FALSE, MPs=MPs, nsims=9)

  DF <- p$DF
  DF <- DF %>% mutate(res=ISt/BSt,
                      log_I_err=log(res))
  DF$OM <- RefOMs$OM.object[i]
  dflist[[i]] <- DF
}

DF <- do.call('rbind', dflist)
DF$Period <- factor(DF$Period)

ggplot(DF %>% filter(MP=='NF', Period=='Projection'), aes(x=Year, y=res, color=Period)) +
  facet_grid(Sim~OM) +
  geom_line() +
  theme_bw() +
  scale_color_discrete(drop=FALSE) +
  geom_hline(yintercept = 1, linetype=2) +
  labs(y='Residuals')

ggsave(file.path(out.dir, 'Proj_Resid_RefOMs.png'), width=12, height=4)


# ---- Additional Indices ----
AddIndList
indices <- c('SPN_1', "CAN_3", 'JPN_LATE_5', 'CHT_LATE_8', 'MOR_9',
             'US_Survey_12', 'PORT_Survey_13')

AddInd <- SWOMSE::MOM_000@cpars[[1]][[1]]$Data@AddInd

ni <- dim(AddInd)[2]
yrs <- dimnames(AddInd)[[3]]
nms <- dimnames(AddInd)[[2]]
outlist <- list()
for (i in 1:ni) {
  outlist[[i]] <- data.frame(Year=yrs, Index=nms[i], Value=AddInd[1,i,])
}
df <- do.call('rbind', outlist)
df$Year <- as.numeric(df$Year)
df$Index <- factor(df$Index, levels=unique(df$Index), ordered = TRUE)
df <- df%>% filter(Index %in% indices)

p <- ggplot(df, aes(x=Year, y=Value)) +
  facet_wrap(~Index) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw()

ggsave(file.path(out.dir, 'Individual_Indices.png'), p, width=8, height=6)




stats_list <- list()
for (i in 1:nrow(RefOMs)) {
  MMSE <- MSElist[[i]]
  MOM <- RefOMs$OM.object[i]
  stats_list_ind <- list()
  for (ind in seq_along(indices)) {
    index <- indices[ind]
    ll <- Compare_Additional_Index_Hist(MMSE, index, MOM, plot=FALSE)
    nm <- paste0(MOM, '_', index, '.png')
    ggsave(file.path(out.dir, 'Additional_Indices', nm), ll$p, width=8, height=5)
    stats_list_ind[[ind]] <- ll$stats
  }

  ll$stats$units <- ll$units
  stats_list[[i]] <- do.call('rbind', stats_list_ind)

}

add_ind_stats <- do.call('rbind', stats_list)
saveRDS(add_ind_stats, file.path(out.dir, 'Additional_Indices', 'add_ind_stats.rda'))

tt <- add_ind_stats %>% filter(OM=='MOM_133') %>%
  mutate(SD=round(sd,2), AC=round(ac,2)) %>%
  select(OM, Index, SD, AC)

tt <- add_ind_stats %>% group_by(OM, Index) %>%
  mutate(SD=round(sd,2), AC=round(ac,2)) %>%
  select(OM, Index, SD, AC)

DT::datatable(tt)


## ---- Render Markdown ----
library(rmarkdown)
output_file <- 'Additional_Index_Diagnostics.html'
render(file.path(out.dir,"Index_Diagnostic_Report.Rmd"), output_file=output_file)





## Next
# make plots for each index
# make summary table for each index
# show values for all Reference OMs.







