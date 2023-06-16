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

Scope_MPs <- c('CEcr', 'CEcr2', 'CEcr3', 'CEcr4', 'CEcr5')

for (i in seq_along(Scope_MPs)) {
  MP_name <- Scope_MPs[i]
  Scope(MP_name, Tuning_OMs, TuneTargets)
  Plot_Scope(MP_name)
}


# ---- Tune an MP to a specific PM Target ----

Tune_MPs <-c('CEcr', 'CEcr2', 'CEcr3', 'CEcr4', 'CEcr5')

TuneTarget <- TuneTargets %>% filter(Code=='a')

for (i in seq_along(Tune_MPs)) {
  MP_name <- Scope_MPs[i]
  Tune(MP_name, Tuning_OMs, TuneTarget)
}














