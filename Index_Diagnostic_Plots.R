# ---- Index Diagnostic Plots ----

library(SWOMSE)

# MOM <- MOM_000
# MOM@nsim <- 6
# MPs <- c('Islope1', 'Itarget1', 'NFref')
# MMSE <- multiMSE(MOM, MPs=MPs, dropHist = FALSE)
# saveRDS(MMSE, 'MSE_Objects/Index_Testing/MMSE.mse')
#

MMSE <- readRDS('MSE_Objects/Index_Testing/MMSE.mse')

pp <- Index_Diagnostic(MMSE, 'CI')

ggsave('img/Index_Diagnostics/MOM_000_1.png', pp[[1]], width=12, height=6)
ggsave('img/Index_Diagnostics/MOM_000_2.png', pp[[2]], width=12, height=8)



