library(SWOMSE)

Refs_OMs <- OM_DF %>% filter(Class=='Reference')
Refs_OMs <- Refs_OMs$OM.object

if (!dir.exists('MSE_Objects'))
  dir.create('MSE_Objects')


source('CMPs/MPs_ND.R')

# Run MCC11_b
MP_name <- 'MCC11_b'

Catchdf$Catch[3:4] <- c(12215, 10997) # Update catches re Kyle email 18/9/2025

for (i in seq_along(Refs_OMs)) {

  # load hist
  om <- paste0(Refs_OMs[i], '.hist')
  hist <- readRDS(file.path('Hist_Objects/Reference', om))

  # run mse
  mmse <- ProjectMOM(hist, MP_name)

  # save MSE
  nm <- paste0(Refs_OMs[i], '-', MP_name, '-Reference', '.mse')
  saveRDS(mmse, file=file.path('Update_MSE_Objects', nm))
}


MSEList <- list()

for (i in 1:9) {
  nm <- paste0(Refs_OMs[i], '-', 'MCC11_b', '-Reference', '.mse')
  MSEList[[i]] <- readRDS(file.path('Update_MSE_Objects', nm))
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

dimnames(Index2020_2027) <- list(Sim=1:nrow(Index2020_2027),
                                 Year=2020:2027)

SimIndexDF <- array2DF(Index2020_2027)
SimIndexDF$Sim <- as.numeric(SimIndexDF$Sim)
SimIndexDF$Year <- as.numeric(SimIndexDF$Year)

# Observed Index
Index <- read.csv('NSWOCombinedIndex2025.csv') |>
  dplyr::mutate(Ind=ifelse(YearC<=2022, TRUE, FALSE)) |>
  dplyr::mutate(Mean=mean(response[Ind==TRUE])) |>
  dplyr::mutate(StIndex=response/Mean)


densalator = function(vec){
  if (prod(vec == mean(vec))) {
    return(NA)
  }
  if(all(vec>0,na.rm=T)){
    dens = density(log(vec),na.rm=T)

    dx = 0.0001
    dexpx = exp(dens$x+dx)-exp(dens$x)
    dexpx_dx = dexpx/dx
    dens$y = dens$y/dexpx_dx
    dens$x = exp(dens$x)
  }else{
    dens = density(vec,na.rm=T)
  }
  dens
}

qs <- SimIndexDF |> dplyr::group_by(Year) |>
  dplyr::summarise(Lower=quantile(Value,0.001),
                   Upper=quantile(Value, 0.999)
)

png('img/IndexDensity.png', width=6, height = 4, units='in', res=400)

par(mfrow=c(1,1), mar=c(1,1,1,1), oma=c(3,3,1,1))
plot(c(min(SimIndexDF$Year),max(SimIndexDF$Year)+1),
     c(min(qs$Lower), max(qs$Upper)),
     col='white',xlab="",ylab="")

Yrs <- unique(SimIndexDF$Year)
axis(side=1, labels=Yrs, at=Yrs)
mtext(side=1, outer=TRUE, 'Year', line=1, font=2)
mtext(side=2, outer=TRUE, 'Index', line=1, font=2)

for (i in seq_along(Yrs)) {
  vec <- SimIndexDF |> dplyr::filter(Year==Yrs[i]) |> dplyr::pull(Value)
  lower <- quantile(vec, 0.025)
  upper <- quantile(vec, 0.975)
  dens <- densalator(vec)

  if (!all(is.na(dens))) {
    ys = 0.95*(dens$y / max(dens$y,na.rm=T))
    polygon(Yrs[i]+ys,dens$x,col='#0000ff60',border=NA)
    lines(range(Yrs[i]+ys), rep(lower, 2), lty=2)
    lines(range(Yrs[i]+ys), rep(upper, 2), lty=2)
  }

  Obs <- Index |> dplyr::filter(YearC==Yrs[i]) |> dplyr::pull(StIndex)
  if (length(Obs)>0)
    lines(Yrs[i]+c(0,0.95),rep(Obs,2),lwd=3)
}
legend('topleft', cex=1,legend = c("Predicted","Observed"),text.col=c("#0000ff60",'black'),text.font=2,bty='n')
dev.off()


#
# library(ggridges)
#
# SimIndexDF$Value
#
# ggplot(SimIndexDF, aes(x=Year, y=Value, width=Value, group=Year)) +
#   geom_vridgeline()
#
#
# d <- data.frame(y = rep(1:5, 3), x = c(rep(0, 5), rep(1, 5), rep(3, 5)),
#                 width = c(0, 1, 3, 4, 0, 1, 2, 3, 5, 4, 0, 5, 4, 4, 1))
# ggplot(d, aes(x, y, width = width, group = x)) + geom_vridgeline(fill="lightblue")
