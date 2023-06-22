library(SWOMSE)


## --- Setup ---

# Need to run 1. Simulate OMs.R first to simulate the historical fishery
# dynamics for the Reference OMs and save the multiHist objects to
# 'Hist_Objects' folder in the root directory


# Tuning OMs (Reference OMs)
Tuning_OMs <- OM_DF %>% filter(Class=='Reference')
Tuning_OMs <- Tuning_OMs$OM.object

# Tuning Targets
TuneTargets

MPs <- avail('MP', 'SWOMSE')

# --- Scope CMP Performance over Range of Tuning Values ----

# 1. Source the MP functions
# 2. Loop over MPs and conduct scoping

# ------------------- CE Method -----------------------------------------------
Tune_MPs <- c('CE', 'CE25')

for (i in seq_along(Tune_MPs)) {
  MP_name <- Tune_MPs[i]
  Scope(MP_name, Tuning_OMs, TuneTargets)
  Plot_Scope(MP_name)
}

TuneTarget <- TuneTargets %>% filter(Code=='a')

for (i in seq_along(Tune_MPs)) {
  MP_name <- Tune_MPs[i]
  Tune(MP_name, Tuning_OMs, TuneTarget, test_vals =c(0.75, 1, 1.25))
}

TuneTarget <- TuneTargets %>% filter(Code=='b')

for (i in seq_along(Tune_MPs)) {
  MP_name <- Tune_MPs[i]
  Tune(MP_name, Tuning_OMs, TuneTarget, test_vals =c(0.9, 0.95, 1))
}

TuneTarget <- TuneTargets %>% filter(Code=='c')

for (i in seq_along(Tune_MPs)) {
  MP_name <- Tune_MPs[i]
  Tune(MP_name, Tuning_OMs, TuneTarget, test_vals =c(0.75, 0.85,0.95))
}

# --------------------SPS MPs ---------------------------------------------------


Tune_MPs <- c('SPS', 'SPS25')

for (i in seq_along(Tune_MPs)) {
  MP_name <- Tune_MPs[i]
  Scope(MP_name, Tuning_OMs, TuneTargets)
  Plot_Scope(MP_name)
}


TuneTarget <- TuneTargets %>% filter(Code=='a')

for (i in seq_along(Tune_MPs)) {
  MP_name <- Tune_MPs[i]
  Tune(MP_name, Tuning_OMs, TuneTarget, test_vals =c(1.5, 2, 2.5))
}


TuneTarget <- TuneTargets %>% filter(Code=='b')

for (i in seq_along(Tune_MPs)) {
  MP_name <- Tune_MPs[i]
  Tune(MP_name, Tuning_OMs, TuneTarget, test_vals =c(1.6, 1.7, 1.8))
}

TuneTarget <- TuneTargets %>% filter(Code=='c')

for (i in seq_along(Tune_MPs)) {
  MP_name <- Tune_MPs[i]
  Tune(MP_name, Tuning_OMs, TuneTarget, test_vals =c(1.4, 1.5, 1.6))
}


# --------------------SPFox MPs ---------------------------------------------------

Tune_MPs <- c('SPFox', 'SPFox25')


TuneTarget <- TuneTargets %>% filter(Code=='a')

for (i in seq_along(Tune_MPs)) {
  MP_name <- Tune_MPs[i]
  Tune(MP_name, Tuning_OMs, TuneTarget, test_vals=c(0.75, 1, 1.25))
}


TuneTarget <- TuneTargets %>% filter(Code=='b')

for (i in seq_along(Tune_MPs)) {
  MP_name <- Tune_MPs[i]
  Tune(MP_name, Tuning_OMs, TuneTarget, test_vals =c(0.8, 0.85, 1))
}

TuneTarget <- TuneTargets %>% filter(Code=='c')

for (i in seq_along(Tune_MPs)) {
  MP_name <- Tune_MPs[i]
  Tune(MP_name, Tuning_OMs, TuneTarget, test_vals =c(0.7, 0.8, 0.9))
}



# --------------------AH MPs ---------------------------------------------------


Tune_MPs <- c('EA1', 'WA1')


TuneTarget <- TuneTargets %>% filter(Code=='a')

for (i in seq_along(Tune_MPs)) {
  MP_name <- Tune_MPs[i]
  Tune(MP_name, Tuning_OMs, TuneTarget, test_vals=c(0.75, 1, 1.25))
}


Tune_MPs <- c('CI1', 'EA1', 'WA1')

TuneTarget <- TuneTargets %>% filter(Code=='b')

for (i in seq_along(Tune_MPs)) {
  MP_name <- Tune_MPs[i]
  Tune(MP_name, Tuning_OMs, TuneTarget, test_vals =c(0.5, 1, 1.5))
}

Tune_MPs <- c('EA1', 'WA1')

TuneTarget <- TuneTargets %>% filter(Code=='c')

for (i in seq_along(Tune_MPs)) {
  MP_name <- Tune_MPs[i]
  Tune(MP_name, Tuning_OMs, TuneTarget, test_vals =c(1.5, 2, 2.1))
}





# ---- Tune an MP to a specific PM Target ----

AH_CMPs <- c('AT1')

Tune_MPs <- c(Scope_MPs_1, Scope_MPs_2, )

TuneTarget <- TuneTargets %>% filter(Code=='a')

for (i in seq_along(Tune_MPs)) {
  MP_name <- Tune_MPs[i]
  Tune(MP_name, Tuning_OMs, TuneTarget, test_vals=c(0.5, 1, 1.5))
}



TuneTarget <- TuneTargets %>% filter(Code=='b')

for (i in seq_along(Tune_MPs)) {
  MP_name <- Tune_MPs[i]
  Tune(MP_name, Tuning_OMs, TuneTarget)
}



TuneTarget <- TuneTargets %>% filter(Code=='c')

for (i in seq_along(Tune_MPs)) {
  MP_name <- Tune_MPs[i]
  Tune(MP_name, Tuning_OMs, TuneTarget)
}


