library(SWOMSE)

Refs_OMs <- OM_DF %>% filter(Class=='Reference')
Refs_OMs <- Refs_OMs$OM.object

if (!dir.exists('MSE_Objects'))
  dir.create('MSE_Objects')

# Run MCC11_b
MPs <- 'MCC11_b'

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




MSEList <- list()

for (i in 1:9) {
  nm <- paste0(Refs_OMs[i], '-', 'MCC11_b', '-Reference', '.mse')
  MSEList[[i]] <- readRDS(file.path('MSE_Objects', nm))
}

IndexList <- list()
for (i in 1:9) {
  IndexList[[i]] <- MSEList[[i]]@PPD[[1]][[1]][[1]]@Ind[,71:78]
}

# 2023 Simulated Index
Index2020_2027 <- do.call('rbind', IndexList)

data.frame(Year=2020:2027,
           Lower=apply(Index2020_2027, 2, quantile, 0.025) |> round(2),
           Mean=apply(Index2020_2027, 2, mean) |> round(2),
           Upper=apply(Index2020_2027, 2, quantile, 0.975) |> round(2)
)

