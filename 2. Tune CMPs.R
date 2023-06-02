library(SWOMSE)


## --- Setup ---

# Need to run 1. Simulate OMs.R first to simulate the historical fishery
# dynamics for the Reference OMs and save the multiHist objects to
# 'Hist_Objects' folder in the root directory


# Tuning OMs (Reference OMs)
Tuning_OMs <- OM_DF %>% filter(Class=='Reference')
Tuning_OMs <- Tuning_OMs$OM.object

# Tuning Targets
TuneTargets <- read.csv('Tuning_Objects/Tuning_Target_Codes.csv')
TuneTargets

# --- Scope CMP Performance over Range of Tuning Values ----

# 1. Source the MP functions
# 2. Loop over MPs and conduct scoping

Scope_MPs <- c('CE_un', 'IR1', 'IR2', 'SP1', 'SP2', 'SP3')

for (i in seq_along(Scope_MPs)) {
  MP_name <- Scope_MPs[i]
  Scope(MP_name, Tuning_OMs, TuneTargets)
  Plot_Scope(MP_name)
}







# ---- Tune an MP to a specific PM Target ----

MP_name <- 'IR2'
TuneTarget <- TuneTargets %>% filter(Code=='a')

tt <- Tune(MP_name, Tuning_OMs, TuneTarget)


obj <- readRDS('Tuning_Objects/IR2.tune')
obj$Name %>% unique()

plotTO <- function(obj, PM1, PM2) {
  df1 <- obj %>% filter(Name==PM1)
  df2 <- obj %>% filter(Name==PM2)

  df1$x <- df1$Value
  df2$y <- df2$Value

  df <- bind_cols(df1, df2)
  df$test_vals <- df$test_vals...15
  df$test_vals <- factor(df$test_vals)
  ggplot(df, aes(x=x, y=y, group=1)) +
    geom_point(aes(color=test_vals), size=4) +
    geom_line() +
    expand_limits(x=c(0,1), y=c(0,1)) +
    theme_bw() +
    labs(x=PM1, y=PM2) +
    guides(color='none')
}


p1 <- plotTO(obj, 'PGK_6_10', 'LRP')
p2 <- plotTO(obj, 'PGK_med', 'LRP')
p3 <- plotTO(obj, 'PGK_long', 'LRP')
p4 <- plotTO(obj, 'PGK_30', 'LRP')

p <- cowplot::plot_grid(p1, p2, p3,p4)
ggsave('img/Tuning_Tradeoffs/PGK_LRP.png', p)

p1 <- plotTO(obj, 'PGK_6_10', 'AvTAC_long')
p2 <- plotTO(obj, 'PGK_med', 'AvTAC_long')
p3 <- plotTO(obj, 'PGK_long', 'AvTAC_long')
p4 <- plotTO(obj, 'PGK_30', 'AvTAC_long')

p <- cowplot::plot_grid(p1, p2, p3,p4)
ggsave('img/Tuning_Tradeoffs/PGK_AvTAC_long.png', p)


df %>% tidyr::pivot_wider(., names_from=Name, values_from = Value)

TradeOff(obj, PMs=c('PGK_6_10', 'AvTAC_med'))


tt <- obj %>% filter(Name %in% c('PGK_6_10', 'LRP'))

tt %>% filter(Name=='LRP')
tt %>% filter(Name=='PGK_6_10')




Ref_OMs <- OM_DF %>% filter(Class=='Reference')

IR2_a <- IR2
IR2_b <- IR2

formals(IR2_a)$tunepar <- 0.9816327
formals(IR2_b)$tunepar <- 1.0714286

class(IR2_a) <- class(IR2_b) <- 'MP'

MPs <- c('IR2_a', 'IR2_b')
MSElist <- list()
for (i in 1:9) {
  mom <- Ref_OMs$OM.object[i]
  multiHist <- readRDS(file.path('Hist_Objects', paste0(mom, '.hist')))
  MSElist[[i]] <- ProjectMOM(multiHist, MPs=MPs)
}

MSE_all <- combine_MMSE(MSElist, 'name')


PGK_6_10(MSE_all)
LRP(MSE_all)
AvTAC_long(MSE_all)

Catch_TS(MSE_all)
SB_SBMSY_TS(MSE_all)

