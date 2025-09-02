library(SWOMSE)

indices <- list.files('ECP/Indices', full.names = TRUE)

hist_objects <- list.files('Hist_Objects/Reference', full.names = TRUE)
nyears <- length(SWOData@Ind[1,])
proyears <- MOM_001@proyears
nsim <- MOM_001@nsim

ind_yrs <- 1999:2022 # years to calculate index error
yrs_index <- match(ind_yrs, SWOData@Year)

yrs <- seq(SWOData@Year[1], by=1, length.out=nyears+proyears)
update_yrs <- seq(2025, by=3, to=max(yrs))
update <- rep(0, length(yrs))
update[yrs %in% update_yrs] <- 1

UpdateDF <- data.frame(Ind=seq_along(yrs),
                       Year=yrs,
                       Update=update)

UpdateDF_Proj <- UpdateDF[(nyears+1):nrow(UpdateDF),]
UpdateDF_Proj$Index <- 1:nrow(UpdateDF_Proj)


UpdateErrorYears <- 79:105 # starts in 2028
UpdateErrorProjYears <- 6:32

# load MP
source("CMPs/MPs_ND.R")

for (i in seq_along(hist_objects)) {
  Hist <- readRDS(hist_objects[i])

  B <- apply(Hist[[1]][[1]]@TSdata$Biomass[1,,] + Hist[[2]][[1]]@TSdata$Biomass[1,,], 1, sum)

  for (j in seq_along(indices)) {
    index <- readRDS(indices[j])
    index <- index$response/mean(index$response)

    Resids <- MSEtool:::Calc_Residuals(B[yrs_index], index[yrs_index], 1)
    Stats <- MSEtool:::Calc_Stats(Resids$res)
    Stats <- data.frame(AC=replicate(nsim, Stats$AC),
                        SD=replicate(nsim, Stats$SD),
                        lst.err=replicate(nsim, Stats$lst.err)
    )

    Resid_Proj <- MSEtool:::Gen_Residuals(Stats, nsim, proyears)

    Hist$Female$`Fleet 1`@SampPars$Obs$Ierr_y[,UpdateErrorYears] <- Resid_Proj[,UpdateErrorProjYears]
    Hist$Male$`Fleet 1`@SampPars$Obs$Ierr_y[,UpdateErrorYears] <- Resid_Proj[,UpdateErrorProjYears]

    MSE <- ProjectMOM(Hist, 'MCC11_b')

    om_name <- gsub('.hist', '', basename(hist_objects[i]))

    nm <- gsub(".rda", "", basename(indices[j]))
    nm <- paste0(om_name, '_Dropped_',nm, '.rda')
    name <- file.path('ECP/MSE_Objects', nm)
    saveRDS(MSE, name)
  }
}

## Run Base Case
for (i in seq_along(hist_objects)) {
  Hist <- readRDS(hist_objects[i])
  MSE <- ProjectMOM(Hist, 'MCC11_b')

  om_name <- gsub('.hist', '', basename(hist_objects[i]))

  nm <- paste0(om_name, '_Base.rda')
  name <- file.path('ECP/MSE_Objects', nm)
  saveRDS(MSE, name)

}



