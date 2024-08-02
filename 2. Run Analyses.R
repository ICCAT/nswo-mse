library(SWOMSE)

# setwd("C:/Users/tcarruth/Documents/GitHub/nswo-mse")
# setwd("C:/GitHub/nswo-mse")

# Source CMPs
source_CMPs()

get_MP_names <- function() {
  tt <- lsf.str(envir=.GlobalEnv)
  df_list <- list()
  for (i in seq_along(tt)) {
    MP <- tt[i]
    is.MP <- !is.null(formals(MP)$Data)
    code <- strsplit(MP,'_')[[1]][2]
    df_list[[i]] <- data.frame(MP=MP, is.MP=is.MP, code=code)

  }
  df <- do.call('rbind', df_list)
  df <- df %>% dplyr::filter(is.na(code)==TRUE, is.MP==TRUE)
  df$MP
}

# Define Reference OMs
Refs_OMs <- OM_DF %>% filter(Class=='Reference')
Refs_OMs <- Refs_OMs$OM.object


All_MPs <- get_MP_names() %>% sort()
Test_MPs <- c('MCC85a', 'MCC85b', 'MCC97a', 'MCC97b', 'MCC97c') #

TuneTargets$Metric <- 'PGK_short'


# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
TuneTargets = TuneTargets[2:3,]
TuneTargets = dplyr::bind_rows(TuneTargets,
                    data.frame(Code=c("d","e"),
                               Metric = c("PGK_med","PGK_long"),
                               Target = c(0.60,0.60)))
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

if (!dir.exists('Tuning_Objects'))
  dir.create('Tuning_Objects')

# ---- Scoping and Tuning ----
for (MP_name in Test_MPs) {
  Tune_MP(MP_name,
          Tuning_OMs=Refs_OMs,
          TuneTargets)

  # Create tuned CMPs
  Document_MP(MP_name=MP_name, MP_file=get_MP_locations(MP_name), plot=TRUE)

}



# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# There is now a hack in here whereby the b tunings are manually
# (copy past in the .r files) set to the lowest tuning
# value of the b, d, and e tuning such that PGK is at least 60% for all
# three time periods (annoying I know)
# was not necessary last update with teh new index
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# ---- Run MSE for Reference OMs -----
# Source new tuned CMPs
source_CMPs()
TuneTargets=TuneTargets[TuneTargets$Code %in% c("b","c"),]

Test_MPs <- c('MCC5', 'MCC85a', 'MCC85b', 'MCC97a', 'MCC97b', 'MCC97c') #

if (!dir.exists('MSE_Objects'))
  dir.create('MSE_Objects')

for (MP_name in Test_MPs) {

  MPs <- get_tune_MPs(MP_name)

  for (i in seq_along(Refs_OMs)) {

    # load hist
    om <- paste0(Refs_OMs[i], '.hist')
    hist <- readRDS(file.path('Hist_Objects/Reference', om))

    # run mse
    mmse <- ProjectMOM(hist, MPs)

    # save MSE
    nm <- paste0(Refs_OMs[i], '-', MP_name, '-Reference', '.mse')
    saveRDS(mmse, file=file.path('MSE_Objects', nm))
  }

}


# ---- Run MSE for Robustness Tests ----

## R1. Increasing Catchability  - Historical & Projection ----
hist <- readRDS(file.path('Hist_Objects/R1_Increasing_q', 'MOM_010.hist'))

for (MP_name in Test_MPs) {
  MPs <- get_tune_MPs(MP_name)
  mmse <- ProjectMOM(hist, MPs)

  # save MSE
  nm <- paste0('MOM_010', '-', MP_name, '-R1_Increasing_q', '.mse')
  saveRDS(mmse, file=file.path('MSE_Objects', nm))

}


## R1a. Increasing Catchability  - Historical & Projection ----
hist <- readRDS(file.path('Hist_Objects/R1a_Increasing_q', 'MOM_011.hist'))

for (MP_name in Test_MPs) {
  MPs <- get_tune_MPs(MP_name)
  mmse <- ProjectMOM(hist, MPs)

  # save MSE
  nm <- paste0('MOM_011', '-', MP_name, '-R1a_Increasing_q', '.mse')
  saveRDS(mmse, file=file.path('MSE_Objects', nm))

}




## R2. Increasing Catchability  - Historical Only ----
hist <- readRDS(file.path('Hist_Objects/R2', 'MOM_010.hist'))

for (MP_name in Test_MPs) {
  MPs <- get_tune_MPs(MP_name)
  mmse <- ProjectMOM(hist, MPs)

  # save MSE
  nm <- paste0('MOM_010', '-', MP_name, '-R2_Increasing_q', '.mse')
  saveRDS(mmse, file=file.path('MSE_Objects', nm))
}

## R2a. Increasing Catchability  - Historical Only ----
hist <- readRDS(file.path('Hist_Objects/R2a', 'MOM_011.hist'))

for (MP_name in Test_MPs) {
  MPs <- get_tune_MPs(MP_name)
  mmse <- ProjectMOM(hist, MPs)

  # save MSE
  nm <- paste0('MOM_011', '-', MP_name, '-R2a_Increasing_q', '.mse')
  saveRDS(mmse, file=file.path('MSE_Objects', nm))
}




## R3. Climate Change - Increased Recruitment Variability ----


### R3a
hist <- readRDS(file.path('Hist_Objects/R3a', 'MOM_005.hist'))

for (MP_name in Test_MPs) {
  MPs <- get_tune_MPs(MP_name)
  if (!length(MPs))
    next()
  mmse <- ProjectMOM(hist, MPs)

  # save MSE
  nm <- paste0('MOM_005', '-', MP_name, '-R3a_CC', '.mse')
  saveRDS(mmse, file=file.path('MSE_Objects', nm))
}

### R3b

hist <- readRDS(file.path('Hist_Objects/R3b', 'MOM_005.hist'))

for (MP_name in Test_MPs) {
  MPs <- get_tune_MPs(MP_name)
  if (!length(MPs))
    next()
  mmse <- ProjectMOM(hist, MPs)

  # save MSE
  nm <- paste0('MOM_005', '-', MP_name, '-R3b_CC', '.mse')
  saveRDS(mmse, file=file.path('MSE_Objects', nm))
}


## R4. Implementation Error - 10% Overages in Unreported Catch ----

hist <- readRDS(file.path('Hist_Objects/R4', 'MOM_005.hist'))

for (MP_name in Test_MPs) {
  MPs <- get_tune_MPs(MP_name)
  mmse <- ProjectMOM(hist, MPs)

  # save MSE
  nm <- paste0('MOM_005', '-', MP_name, '-R4_Imp', '.mse')
  saveRDS(mmse, file=file.path('MSE_Objects', nm))
}

## R0. Reference ----

hist <- readRDS(file.path('Hist_Objects/Reference', 'MOM_005.hist'))

for (MP_name in Test_MPs) {
  MPs <- get_tune_MPs(MP_name)
  mmse <- ProjectMOM(hist, MPs)

  # save MSE
  nm <- paste0('MOM_005', '-', MP_name, '-R0_Ref', '.mse')
  saveRDS(mmse, file=file.path('MSE_Objects', nm))
}






