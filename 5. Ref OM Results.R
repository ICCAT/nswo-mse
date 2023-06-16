library(SWOMSE)

MSE.dir <- 'MSE_Objects'

Ref_OMs <- OM_DF %>% filter(Class=='Reference')

MSElist <- list()
for (i in 1:nrow(Ref_OMs)) {
  nm <- paste0(Ref_OMs$OM.object[i], '.mse')
  if (file.exists(file.path(MSE.dir, nm))) {
    MSElist[[i]] <- readRDS(file.path(MSE.dir, nm))
  }
}

MSE_all <- combine_MMSE(MSElist, 'Reference OMs')

MPs <- MMSE@MPs[[1]]

# MPs <- MSE_all@MPs[[1]][grepl('_a', MSE_all@MPs[[1]])]
#

# Keep mc 0.25
dropMPs <- c("CE2_a", "CE3_a", "CE4_a", "CE5_a", "CEcr2_a", "CEcr3_a", "CEcr4_a", "CEcr5_a")
MPs <- MPs[!MPs %in% dropMPs]

MMSE <- Sub_MMSE(MSE_all, MPs=MPs)
MMSE@nMPs
MMSE@MPs[[1]]

p1 <- TradeOff(MMSE, PMs=c('PGK_6_10', 'rAvTAC_med'), quants=NULL, vline=0.6)
p1

VarC_Violin(MMSE)

TS_plot(MMSE)

MMSE@MPs[[1]][c(1,2,5,6,7,8,9,10,11,15,16)] %>% length()
sim <- 3
tac <- apply(MMSE@TAC[sim,,1,c(1,2,5,6,7,8,9,10,11,15,16),], 2:3, sum)
matplot(t(tac), type='l')

MaxVarC(MMSE)

MMSE@MPs[[1]]
Data <- MMSE@PPD[[1]][[1]][[5]]

Tab <- PM_table(MMSE)


Quilt <- function(Tab, PMs=NULL) {


}

# PGK_short

p1 <- TradeOff(MMSE, PMs=c('PGK_short', 'AvTAC_med'), xlim=0.51, vline=c(0.6, 0.7))
p2 <- TradeOff(MMSE, PMs=c('nLRP', 'AvTAC_med'), xlim=0.85, vline=c(0.9, 0.95))

cowplot::plot_grid(p1, p2, labels=c('a)', 'b)'))
ggsave('img/June_2023/TradeOff_quants_absTAC.png', width=12, height=6)


p1 <- TradeOff(MMSE, PMs=c('PGK_short', 'AvTAC_med'), xlim=0.51, vline=c(0.6, 0.7), quants=NULL)
p2 <- TradeOff(MMSE, PMs=c('nLRP', 'AvTAC_med'), xlim=0.85, vline=c(0.9, 0.95), quants=NULL)

cowplot::plot_grid(p1, p2, labels=c('a)', 'b)'))
ggsave('img/June_2023/TradeOff_no_quants_absTAC.png', width=12, height=6)


p1 <- TradeOff(MMSE, PMs=c('PGK_short', 'rAvTAC_med'), xlim=0.51, vline=c(0.6, 0.7))
p2 <- TradeOff(MMSE, PMs=c('nLRP', 'rAvTAC_med'), xlim=0.85, vline=c(0.9, 0.95))

cowplot::plot_grid(p1, p2, labels=c('a)', 'b)'))
ggsave('img/June_2023/TradeOff_quants.png', width=12, height=6)


p1 <- TradeOff(MMSE, PMs=c('PGK_short', 'rAvTAC_med'), xlim=0.51, vline=c(0.6, 0.7), quants=NULL)
p2 <- TradeOff(MMSE, PMs=c('nLRP', 'rAvTAC_med'), xlim=0.85, vline=c(0.9, 0.95), quants=NULL)

cowplot::plot_grid(p1, p2, labels=c('a)', 'b)'))
ggsave('img/June_2023/TradeOff_no_quants.png', width=12, height=6)


# PGK_med

p1 <- TradeOff(MMSE, PMs=c('PGK_med', 'AvTAC_med'), xlim=0.51, vline=c(0.6, 0.7))
p2 <- TradeOff(MMSE, PMs=c('nLRP', 'AvTAC_med'), xlim=0.85, vline=c(0.9, 0.95))

cowplot::plot_grid(p1, p2, labels=c('a)', 'b)'))
ggsave('img/June_2023/TradeOff_med_quants_absTAC.png', width=12, height=6)


p1 <- TradeOff(MMSE, PMs=c('PGK_med', 'AvTAC_med'), xlim=0.51, vline=c(0.6, 0.7), quants=NULL)
p2 <- TradeOff(MMSE, PMs=c('nLRP', 'AvTAC_med'), xlim=0.85, vline=c(0.9, 0.95), quants=NULL)

cowplot::plot_grid(p1, p2, labels=c('a)', 'b)'))
ggsave('img/June_2023/TradeOff_med_no_quants_absTAC.png', width=12, height=6)


p1 <- TradeOff(MMSE, PMs=c('PGK_med', 'rAvTAC_med'), xlim=0.51, vline=c(0.6, 0.7))
p2 <- TradeOff(MMSE, PMs=c('nLRP', 'rAvTAC_med'), xlim=0.85, vline=c(0.9, 0.95))

cowplot::plot_grid(p1, p2, labels=c('a)', 'b)'))
ggsave('img/June_2023/TradeOff_med_quants.png', width=12, height=6)


p1 <- TradeOff(MMSE, PMs=c('PGK_med', 'rAvTAC_med'), xlim=0.51, vline=c(0.6, 0.7), quants=NULL)
p2 <- TradeOff(MMSE, PMs=c('nLRP', 'rAvTAC_med'), xlim=0.85, vline=c(0.9, 0.95), quants=NULL)

cowplot::plot_grid(p1, p2, labels=c('a)', 'b)'))
ggsave('img/June_2023/TradeOff_med_no_quants.png', width=12, height=6)


# PGK_long
p1 <- TradeOff(MMSE, PMs=c('PGK_long', 'AvTAC_med'), xlim=0.51, vline=c(0.6, 0.7))
p2 <- TradeOff(MMSE, PMs=c('nLRP', 'AvTAC_med'), xlim=0.85, vline=c(0.9, 0.95))

cowplot::plot_grid(p1, p2, labels=c('a)', 'b)'))
ggsave('img/June_2023/TradeOff_long_quants_absTAC.png', width=12, height=6)


p1 <- TradeOff(MMSE, PMs=c('PGK_long', 'AvTAC_med'), xlim=0.51, vline=c(0.6, 0.7), quants=NULL)
p2 <- TradeOff(MMSE, PMs=c('nLRP', 'AvTAC_med'), xlim=0.85, vline=c(0.9, 0.95), quants=NULL)

cowplot::plot_grid(p1, p2, labels=c('a)', 'b)'))
ggsave('img/June_2023/TradeOff_long_no_quants_absTAC.png', width=12, height=6)


p1 <- TradeOff(MMSE, PMs=c('PGK_long', 'rAvTAC_med'), xlim=0.51, vline=c(0.6, 0.7))
p2 <- TradeOff(MMSE, PMs=c('nLRP', 'rAvTAC_med'), xlim=0.85, vline=c(0.9, 0.95))

cowplot::plot_grid(p1, p2, labels=c('a)', 'b)'))
ggsave('img/June_2023/TradeOff_long_quants.png', width=12, height=6)


p1 <- TradeOff(MMSE, PMs=c('PGK_long', 'rAvTAC_med'), xlim=0.51, vline=c(0.6, 0.7), quants=NULL)
p2 <- TradeOff(MMSE, PMs=c('nLRP', 'rAvTAC_med'), xlim=0.85, vline=c(0.9, 0.95), quants=NULL)

cowplot::plot_grid(p1, p2, labels=c('a)', 'b)'))
ggsave('img/June_2023/TradeOff_long_no_quants.png', width=12, height=6)



Kobe_Time(MMSE)
ggsave('img/June_2023/Kobe_Time.png', width=12, height=6)

TS_plot(MMSE)
ggsave('img/June_2023/TS.png', width=12, height=6)

VarC_Violin(MMSE)
ggsave('img/June_2023/VarC.png', width=6, height=6)

# Tuning Trade-offs
codes <- TuneTargets %>% filter(Metric=='PGK_6_10')

codes <- paste0('_',codes$Code)

MPs_a <- MSE_all@MPs[[1]][grepl(codes[1], MSE_all@MPs[[1]])]
MPs_b <- MSE_all@MPs[[1]][grepl(codes[2], MSE_all@MPs[[1]])]
MPs_c <- MSE_all@MPs[[1]][grepl(codes[3], MSE_all@MPs[[1]])]

MMSE_2 <- Sub_MMSE(MSE_all, MPs=c(MPs_a, MPs_b, MPs_c))

TradeOff(MMSE_2, PMs=c('PGK_short', 'rAvTAC_med'), xlim=0.51, vline=c(0.6, 0.7), quants=NULL)

TradeOff(MMSE_2, PMs=c('PGK_med', 'rAvTAC_med'), xlim=0.51, vline=c(0.6, 0.7), quants=NULL)
ggsave('img/June_2023/TO_Tuning.png', width=8, height=6)

TradeOff(MMSE_2, PMs=c('PGK_long', 'AvTAC_med'), xlim=0.51, vline=c(0.6, 0.7), quants=NULL)



MMSE@MPs

which(MSE_all@MPs[[1]]=='SP3_a')
Data <- MSE_all@PPD[[1]][[1]][[28]]




MMSE <- MSE_all
PMs <- c('PGK_6_10', 'rAvTAC_med')

# make projection plots ##
# with quantiles
SB_SBMSY_TS(MMSE)

MMSE@nsim

LRP(MMSE)

# check LRP calculations
# fix in PM functions - by simulation





PM_df <- PM_table(MSE_all)

PM_df_6_10 <- PM_df %>% filter(grepl('_a', PM_df$MP)==TRUE)

TradeOff(PM_df_6_10, PMs=c('PGK_6_10', 'rAvTAC_med'), xlim=0.51)
TradeOff(PM_df_6_10, PMs=c('nLRP', 'rAvTAC_med'), xlim=0.85)

TradeOff(PM_df_6_10, PMs=c('PGK_6_10', 'rAvTAC_med'))

SB_SBMSY_TS(MSE_all)





