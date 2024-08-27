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
Test_MPs <- c('CE', 'SPSSFox', 'SPSSFox2') #


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



## R2. Increasing Catchability  - Historical Only ----
hist <- readRDS(file.path('Hist_Objects/R2', 'MOM_010.hist'))

for (MP_name in Test_MPs) {
  MPs <- get_tune_MPs(MP_name)
  mmse <- ProjectMOM(hist, MPs)

  # save MSE
  nm <- paste0('MOM_010', '-', MP_name, '-R2_Increasing_q', '.mse')
  saveRDS(mmse, file=file.path('MSE_Objects', nm))
}

## R3. Increasing Catchability  - Historical Only ----
hist <- readRDS(file.path('Hist_Objects/R2a', 'MOM_011.hist'))

for (MP_name in Test_MPs) {
  MPs <- get_tune_MPs(MP_name)
  mmse <- ProjectMOM(hist, MPs)

  # save MSE
  nm <- paste0('MOM_011', '-', MP_name, '-R2a_Increasing_q', '.mse')
  saveRDS(mmse, file=file.path('MSE_Objects', nm))
}




## Climate Change - Increased Recruitment Variability ----

### R4
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

### R5

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


## R6. Implementation Error - 10% Overages in Unreported Catch ----

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



## R7. Impact of some fleets not updating indices on time -----

library(SWOMSE)

library(dplyr)

Hist <- readRDS('Hist_Objects/Reference/MOM_005.hist')

xlfile <- 'DataGapTest.xlsx'

sheets <- readxl::excel_sheets(xlfile)
df_list <- list()
for (i in seq_along(sheets)) {
  df_list[[i]] <- readxl::read_excel(xlfile, sheet=sheets[i]) |>
    dplyr::select(Year=YearC, Index=response)
  df_list[[i]]$Model <- sheets[i]
}


DF <- do.call('rbind', df_list) |>
  dplyr::mutate(Mean=mean(Index[Model=='Base'])) |>
  mutate(Index=Index/Mean) |> group_by(Year) |>
  dplyr::mutate(Dev=Index/Index[Model=='Base'],
                logDev=log(Dev))

p1 <- ggplot(DF |> dplyr::filter(Year>2017), aes(x=Year, y=Index, color=Model, linetype=Model)) +
  expand_limits(y=0) +
  geom_line() +
  theme_bw()

p2 <- ggplot(DF |> dplyr::filter(Year>2017), aes(x=Year, y=logDev, color=Model, linetype=Model)) +
  geom_line() +
  theme_bw() +
  expand_limits(y=c(-0.25, 0.25)) +
  labs(y='Deviation from Base')

library(patchwork)
p <- p1 /(p2+guides(color='none', linetype='none'))
p

ggsave('img/R7/index.png', height=4, width=5)





DF <- do.call('rbind', df_list) |>
  dplyr::mutate(Mean=mean(Index[Model=='Base'])) |>
  mutate(Index=Index/Mean) |> group_by(Year) |>
  dplyr::mutate(Dev=Index/Index[Model=='Base'],
                logDev=log(Dev)) |>
  dplyr::filter(Model!='Base') |>
  dplyr::filter(Year>=2020, Model=='Spain')

sd <- sd(DF$logDev)
mu <-  -0.5 * sd^2

dd <- dim(Hist[[1]][[1]]@TSdata$Find)
nsim <- dd[1]
nyears <- dd[2]
proyears <- Hist[[1]][[1]]@Misc$MOM@proyears

Ierr_y <- Hist[[1]][[1]]@SampPars$Obs$Ierr_y[,(nyears+1):(nyears+proyears)]

interval <- 3

devs <- exp(rnorm(nsim*proyears, mu, sd)) |> matrix(nrow=nsim, ncol=proyears)

for (sim in 1:nsim) {
  ntimes <- sample(1:4, 1)
  years <- sample(seq(2025, to=2054, by=interval), ntimes) |> sort()

  yr_df <- data.frame(ind=1:proyears, Year=seq(2023, by=1, length.out=32))
  ind <- match(years, yr_df$Year)

  post <- Ierr_y[sim,]
  for (j in 1:ntimes) {
    index <- seq(ind[j], by=1, length.out=3)
    post[index] <- post[index] * devs[sim, index]
  }
  Ierr_y[sim,] <- post
}

Hist[[1]][[1]]@SampPars$Obs$Ierr_y[,(nyears+1):(nyears+proyears)] <- Ierr_y


# Run Projections
source_CMPs()
Test_MPs <- c('CE', 'MCC9', 'MCC11', 'SPSSFox', 'SPSSFox2')


for (MP_name in Test_MPs) {

  MPs <- get_tune_MPs(MP_name)

  # run mse
  mmse <- ProjectMOM(Hist, MPs)

  # save MSE
  nm <- paste0('MOM_005', '-', MP_name, '-R7', '.mse')
  saveRDS(mmse, file=file.path('MSE_Objects', nm))


}



