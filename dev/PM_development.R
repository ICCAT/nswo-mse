library(SWOMSE)
library(ggplot2)
library(cowplot)
library(lubridate)

RefOMs <- OM_DF %>% filter(Class=='Reference')


# Run Historical Simulations
rerun_hist <- FALSE
if (rerun_hist) {
  histlist <- list()
  for (i in 1:nrow(RefOMs)) {
    MOM <- get(RefOMs$OM.object[i])
    MOM@nsim <- nsim <-  10
    histlist[[i]] <- SimulateMOM(MOM)
  }
  saveRDS(histlist, 'dev/PM_dev.hist')

} else {
  histlist <- readRDS('dev/PM_dev.hist')
}

# Run Forward Projections

MP1 <- IR1

MP2 <- IR1

rerun_proj <- FALSE
mps <- c("MP1", "MP2")
if (rerun_proj) {
  mselist <- list()
  for (i in 1:nrow(RefOMs)) {
    mselist[[i]] <- ProjectMOM(histlist[[i]], MPs=mps)
  }
  saveRDS(mselist, 'dev/PM_dev.mse')
} else {
  mselist <- readRDS('dev/PM_dev.mse')
}

MSE_all <- combine_MMSE(mselist, 'Reference OMs')



MSE_1 <- mselist[[1]]
MSE_2 <- mselist[[2]]
MSE_3 <- mselist[[3]]
MSE_4 <- mselist[[4]]
MSE_5 <- mselist[[5]]
MSE_6 <- mselist[[6]]

PMlist <- list()
## Status ----
width <- 9
height <- 12


mps <- c("IR1_a", "IR1_g")
MSEobj <- MSE_2

### PGK_short - Year = 2024 - 2033 ----

mpind <- which(MSEobj@MPs[[1]] %in% mps)

year_range <- 2024:2033

p1 <- F_FMSY_TS(MSEobj, year_range, mp=mps)
p2 <- SB_SBMSY_TS(MSEobj, year_range, mp=mps)

p <- cowplot::plot_grid(p1 + theme(legend.position="none"),
                        p2+ theme(legend.position="none"),
                        nrow=2)
legend <- get_legend(
  # create some space to the left of the legend
  p1 + theme(legend.box.margin = margin(0, 0, 0, 12))
)
p <- plot_grid(p, legend, Kobe(MSEobj, year_range, mp=mps)+ coord_equal(clip = 'off'),
          rel_widths = c(3, .4))
p

ggsave('img/PMs/PGK_short.png', p, width=width, height=height)

pp <- PGK_short(MSEobj)
PMlist$PGK_short <- pp@Mean[mpind]

### PGK_6_10 - Year = 2029 - 2033 ----

mpind <- which(MSEobj@MPs[[1]] %in% mps)

year_range <- 2029:2033

p1 <- F_FMSY_TS(MSEobj, year_range, mp=mps)
p2 <- SB_SBMSY_TS(MSEobj, year_range, mp=mps)

p <- cowplot::plot_grid(p1 + theme(legend.position="none"),
                        p2+ theme(legend.position="none"),
                        nrow=2)
legend <- get_legend(
  # create some space to the left of the legend
  p1 + theme(legend.box.margin = margin(0, 0, 0, 12))
)
p <- plot_grid(p, legend, Kobe(MSEobj, year_range, mp=mps)+ coord_equal(clip = 'off'),
               rel_widths = c(3, .4))
p

ggsave('img/PMs/PGK_6_10.png', p, width=width, height=height)

pp <- PGK_6_10(MSEobj)
PMlist$PGK_6_10 <- pp@Mean[mpind]


### PGK_med - Years 2034 -- 2043 ----
year_range <- 2034:2043

mpind <- which(MSEobj@MPs[[1]] %in% mps)


p1 <-F_FMSY_TS(MSEobj, year_range, mp=mps)
p2 <- SB_SBMSY_TS(MSEobj, year_range, mp=mps)

p <- cowplot:::plot_grid(p1 + theme(legend.position="none"),
                         p2+ theme(legend.position="none"),
                         nrow=2)

legend <- get_legend(
  # create some space to the left of the legend
  p1 + theme(legend.box.margin = margin(0, 0, 0, 12))
)
p <- plot_grid(p, legend, Kobe(MSEobj, year_range, mp=mps)+ coord_equal(clip = 'off'),
          rel_widths = c(3, .4))

ggsave('img/PMs/PGK_med.png', p, width=width, height=height)

pp <- PGK_med(MSEobj)
PMlist$PGK_med <- pp@Mean[mpind]


### PGK_long - Years 2044-- 2053 ----
year_range <- 2044:2053

mpind <- which(MSEobj@MPs[[1]] %in% mps)


p1 <-F_FMSY_TS(MSEobj,year_range, mp=mps)
p2 <- SB_SBMSY_TS(MSEobj, year_range, mp=mps)

p <- cowplot:::plot_grid(p1 + theme(legend.position="none"),
                         p2+ theme(legend.position="none"),
                         nrow=2)

legend <- get_legend(
  # create some space to the left of the legend
  p1 + theme(legend.box.margin = margin(0, 0, 0, 12))
)
p <- plot_grid(p, legend, Kobe(MSEobj, year_range, mp=mps)+ coord_equal(clip = 'off'),
          rel_widths = c(3, .4))
p

ggsave('img/PMs/PGK_long.png', p, width=width, height=height)

pp <- PGK_long(MSEobj)
PMlist$PGK_long <- pp@Mean[mpind]


### PGK - Years 2024-- 2053 ----
year_range <- 2024:2053

mpind <- which(MSEobj@MPs[[1]] %in% mps)


p1 <-F_FMSY_TS(MSEobj,year_range, mp=mps)
p2 <- SB_SBMSY_TS(MSEobj, year_range, mp=mps)

p <- cowplot:::plot_grid(p1 + theme(legend.position="none"),
                         p2+ theme(legend.position="none"),
                         nrow=2)

legend <- get_legend(
  # create some space to the left of the legend
  p1 + theme(legend.box.margin = margin(0, 0, 0, 12))
)
p <- plot_grid(p, legend, Kobe(MSEobj, year_range, mp=mps)+ coord_equal(clip = 'off'),
          rel_widths = c(3, .4))
p

ggsave('img/PMs/PGK.png', p, width=width, height=height)

pp <- PGK(MSEobj)
PMlist$PGK <- pp@Mean[mpind]


### PGK_30 - Years 2053 ----

year_range <- 2053

mpind <- which(MSEobj@MPs[[1]] %in% mps)


p1 <-F_FMSY_TS(MSEobj,year_range, mp=mps)
p2 <- SB_SBMSY_TS(MSEobj, year_range, mp=mps)

p <- cowplot:::plot_grid(p1 + theme(legend.position="none"),
                         p2+ theme(legend.position="none"),
                         nrow=2)

legend <- get_legend(
  # create some space to the left of the legend
  p1 + theme(legend.box.margin = margin(0, 0, 0, 12))
)
p <- plot_grid(p, legend, Kobe(MSEobj, year_range, mp=mps)+ coord_equal(clip = 'off'),
          rel_widths = c(3, .4))
p

ggsave('img/PMs/PGK_30.png', p,width=width, height=height)

pp <- PGK_30(MSEobj)
PMlist$PGK_30 <- pp@Mean[mpind]


### PNOF - Years 2024:2053 ----
year_range <- 2024:2053

mpind <- which(MSEobj@MPs[[1]] %in% mps)

p1 <- F_FMSY_TS(MSEobj,year_range, fill='OF', mp=mps)
p1

p <- plot_grid(p1, Kobe(MSEobj, year_range, fill='OF', mp=mps)+ coord_equal(clip = 'off'),
          nrow=2, align='h', axis='l')
p

ggsave('img/PMs/POF.png', p, width=width, height=8)

pp <- POF(MSEobj)
PMlist$POF <- pp@Mean[mpind]


## Safety ----

### LRP_short - Year = 2024:2033 ----
year_range <- 2024:2033

mpind <- which(MSEobj@MPs[[1]] %in% mps)

p1 <- SB_SBMSY_TS(MSEobj, year_range, fill='LRP', ref=0.4, mp=mps)

p <- plot_grid(p1, Kobe(MSEobj, year_range, fill='LRP', mp=mps)+ coord_equal(clip = 'off'),
          nrow=2)
p
ggsave('img/PMs/LRP_short.png', p, width=width, height=8)

pp <- LRP_short(MSEobj)
PMlist$LRP_short <- pp@Mean[mpind]

### LRP_med - Year = 2034:2043 ----
year_range <- 2034:2043

mpind <- which(MSEobj@MPs[[1]] %in% mps)

p1 <- SB_SBMSY_TS(MSEobj, year_range, fill='LRP', ref=0.4, mp=mps)

p <-  plot_grid(p1, Kobe(MSEobj, year_range, fill='LRP', mp=mps)+ coord_equal(clip = 'off'),
                nrow=2)
p
ggsave('img/PMs/LRP_med.png', p, width=width, height=8)

pp <- LRP_med(MSEobj)
PMlist$LRP_med <- pp@Mean[mpind]


### LRP_long - Year = 2044:2053 ----
year_range <- 2044:2053

mpind <- which(MSEobj@MPs[[1]] %in% mps)

p1 <- SB_SBMSY_TS(MSEobj, year_range, fill='LRP', ref=0.4, mp=mps)

p <-  plot_grid(p1, Kobe(MSEobj, year_range, fill='LRP', mp=mps)+ coord_equal(clip = 'off'),
          nrow=2)
p
ggsave('img/PMs/LRP_long.png', p, width=width, height=8)

pp <- LRP_long(MSEobj)
PMlist$LRP_long <- pp@Mean[mpind]


### LRP - Year = 2024:2053
year_range <- 2024:2053

mpind <- which(MSEobj@MPs[[1]] %in% mps)

p1 <- SB_SBMSY_TS(MSEobj, year_range, fill='LRP', ref=0.4, mp=mps)

p <- plot_grid(p1, Kobe(MSEobj, year_range, fill='LRP', mp=mps)+ coord_equal(clip = 'off'),
          nrow=2)
p
ggsave('img/PMs/LRP.png', p, width=width, height=height)

pp <- LRP(MSEobj)
PMlist$LRP <- pp@Mean[mpind]


## Yield ----

### C1 - Year = 2024 ----
year_range <- 2024

pp <- TAC1(MSEobj)
PMlist$TAC1 <- pp@Mean[mpind]

p <- TAC_TS(MSEobj, year_range, mp=mps, hline=PMlist$TAC1)
p
ggsave('img/PMs/C1.png', p, width=width, height=4)


### AvTAC_short ----
year_range <- 2024:2033

pp <- AvTAC_short(MSEobj)
PMlist$AvTAC_short <- pp@Mean[mpind]

p <- TAC_TS(MSEobj, year_range, mp=mps, hline=PMlist$AvTAC_short)
p

ggsave('img/PMs/AvTAC_short.png', p,width=width, height=4)

### AvTAC_med ----
year_range <- 2034:2043

pp <- AvTAC_med(MSEobj)
PMlist$AvTAC_med <- pp@Mean[mpind]

p <- TAC_TS(MSEobj, year_range, mp=mps, hline=PMlist$AvTAC_med)
p

ggsave('img/PMs/AvTAC_med.png', p,width=width, height=4)


### AvTAC_long ----
year_range <- 2044:2053

pp <- AvTAC_long(MSEobj)
PMlist$AvTAC_long <- pp@Mean[mpind]


p <- TAC_TS(MSEobj, year_range, mp=mps, hline=PMlist$AvTAC_long)

ggsave('img/PMs/AvTAC_long.png', p, width=width, height=4)


## Stability ----

### VarC ----

p <- Var_TS(MSEobj, mp=mps)
p

ggsave('img/PMs/VarC.png', p, width=width, height=4)

pp <- VarC(MSEobj)
PMlist$VarC <- pp@Mean[mpind]
pp <- MaxVarC(MSEobj)
PMlist$MaxVarC <- pp@Mean[mpind]

PMlist$MPs <- mps

saveRDS(PMlist, 'dev/PMlist.rda')


## Example Trade-off ----

PM_vals <- PM_table(MSEobj)


PMs <- c('PGK_med', 'AvTAC30')

p1 <- TradeOff(PM_vals, c('PGK_10', 'PGK_long'), xlim=0.51, ylim=0.51)

p2 <- TradeOff(PM_vals, c('nLRP_short', 'nLRP_long'), xlim=0.85, ylim=0.85)

p3 <- TradeOff(PM_vals, c('AvTAC10', 'AvTAC30'))

p4 <- TradeOff(PM_vals, c('VarC', 'AvTAC30'))

prow <- cowplot::plot_grid(p1+ theme(legend.position="none"),
                   p2+ theme(legend.position="none"),
                   p3+ theme(legend.position="none"),
                   p4+ theme(legend.position="none"), nrow=2)

legend <- get_legend(
  # create some space to the left of the legend
  p1 + theme(legend.box.margin = margin(0, 0, 0, 12))
)

p <- plot_grid(prow, legend, rel_widths = c(3, .4))

ggsave('img/PMs/TradeOff.png', p, width=12, height=10)







