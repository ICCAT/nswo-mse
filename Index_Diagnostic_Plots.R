# ---- Index Diagnostic Plots ----

library(SWOMSE)

# MOM <- MOM_000
# MOM@nsim <- 6
# MPs <- c('Islope1', 'Itarget1', 'NFref')
# MMSE <- multiMSE(MOM, MPs=MPs, dropHist = FALSE)
# saveRDS(MMSE, 'MSE_Objects/Index_Testing/MMSE.mse')
#

MMSE <- readRDS('MSE_Objects/Index_Testing/MMSE.mse')

Index_Diagnostic(MMSE, 'CI')



