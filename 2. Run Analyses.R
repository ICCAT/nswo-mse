library(SWOMSE)

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


All_MPs <- get_MP_names() %>% sort()

Test_MPs <- c('CE', 'GSC2', 'MCC3', 'SPSS', 'SPSSFox')

# Define Reference OMs
Refs_OMs <- OM_DF %>% filter(Class=='Reference')
Refs_OMs <- Refs_OMs$OM.object


for (MP_name in Test_MPs) {

  # Scoping and Tuning
  Tune_MP(MP_name, Refs_OMs)

  # Create tuned CMPs
  Document_MP(MP_name=MP_name, MP_file=get_MP_locations(MP_name), plot=TRUE)

  # Source new tuned CMPs
  source_CMPs()

  # ---- Run MSE for Reference OMs -----
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

  # ---- Run MSE for Robustness Tests ----

  ## R1. Increasing Catchability
  hist <- readRDS(file.path('Hist_Objects/R1_Increasing_q', 'MOM_010.hist'))

  mmse <- ProjectMOM(hist, MPs)

  # save MSE
  nm <- paste0('MOM_010', '-', MP_name, '-R1_Increasing_q', '.mse')
  saveRDS(mmse, file=file.path('MSE_Objects', nm))
}








