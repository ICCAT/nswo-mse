library(SWOMSE)

# Observed Index
index_exist <- data.frame(Year = SWOData@Year, Index = SWOData@Ind[1,])
index_exist |> dplyr::filter(Year <= 2022) |> dplyr::summarise(mean(Index, na.rm = T))


Index <- read.csv('CombinedIndex_Updates/NSWOCombinedIndex2026.csv') |>
  dplyr::mutate(Ind=ifelse(YearC<=2022, TRUE, FALSE)) |>
  dplyr::mutate(Mean=mean(response[Ind==TRUE])) |>
  dplyr::mutate(StIndex=response/Mean)


# https://www.iccat.int/Documents/Recs/compendiopdf-e/2024-10-e.pd
theta <- 0.7562
TAC_base <- 12600 * theta

Ibase <- Index |>
  dplyr::filter(YearC %in% 2017:2019) |>
  dplyr::summarise(Mean = mean(StIndex)) |>
  dplyr::pull(Mean)

Ibase # 0.6826465

# mean 2022 - 2024
Icurr <- Index |> tail(3) |> dplyr::summarise(ind = mean(StIndex)) |> dplyr::pull(ind)
Icurr # 1.402953
Iratio <- Icurr/Ibase
Iratio # 2.055167

if (Iratio>=1.85) {
  deltaTAC <- 1.85
}
if (Iratio>=1.75 & Iratio<1.85) {
  deltaTAC <- 1.75
}
if (Iratio>=1.65 & Iratio<1.75) {
  deltaTAC <- 1.65
}
if (Iratio>=1.55 & Iratio<1.65) {
  deltaTAC <- 1.55
}
if (Iratio>=1.45 & Iratio<1.55) {
  deltaTAC <- 1.45
}
if (Iratio>=1.35 & Iratio<1.45) {
  deltaTAC <- 1.35
}
if (Iratio>=1.25 & Iratio<1.35) {
  deltaTAC <- 1.25
}
if (Iratio>=1.15 & Iratio<1.25) {
  deltaTAC <- 1.15
}
if (Iratio>=0.75 & Iratio<1.15) {
  deltaTAC <- 1
}
if (Iratio>=0.5 & Iratio<0.75) {
  deltaTAC <- 0.75
}
if (Iratio<0.5) {
  deltaTAC <- 0.5
}

deltaTAC * TAC_base # 17627.02





Refs_OMs <- OM_DF %>% filter(Class=='Reference')
Refs_OMs <- Refs_OMs$OM.object

if (!dir.exists('MSE_Objects'))
  dir.create('MSE_Objects')





source('CMPs/MPs_ND.R')

# Decisions: 26 August 2026 call with Kyle G.
# V1: 2025 -> catches set by MP
# V2: 2025 - 2028 catches are mean from 2022 - 2024: 11,137 t

MeanCatch <- function(x, Data, Data_Lag = 2, Interval = 3, ...) {
  Rec <- new("Rec")
  if (SameTAC(Initial_MP_Yr, Interval, Data)) {
    Rec@TAC <- Data@MPrec[x]
    Rec <- FixedTAC(Rec, Data)
    return(Rec)
  }

  Rec@TAC <- 11137
  Rec
}
class(MeanCatch) <- 'MP'

MPs <- c('MCC11_b', 'MeanCatch')

# Real 2023 and 2024 catches
Catchdf$Catch[3:4] <- c(12215, 10921) # Update catches from call with Kyle 26 August 2026

# for (i in seq_along(Refs_OMs)) {
#
#   # load hist
#   om <- paste0(Refs_OMs[i], '.hist')
#   hist <- readRDS(file.path('Hist_Objects/Reference', om))
#
#   # V1
#   mmse_v1 <- ProjectMOM(hist, MPs = MPs[1])
#   nm <- paste0(Refs_OMs[i], '-MCC.mse')
#   saveRDS(mmse_v1, file=file.path('Update_MSE_Objects/2026', nm))
#
#   # V2
#   mmse_v2 <- ProjectMOM(hist, MPs = MPs[2])
#   nm <- paste0(Refs_OMs[i], '-MeanCatch.mse')
#   saveRDS(mmse_v2, file=file.path('Update_MSE_Objects/2026', nm))
#
# }


MSEList_V1 <- list()
MSEList_V2 <- list()

for (i in 1:9) {
  nm <- paste0(Refs_OMs[i], '-MCC.mse')
  MSEList_V1[[i]] <- readRDS(file.path('Update_MSE_Objects/2026', nm))

  nm <- paste0(Refs_OMs[i], '-MeanCatch.mse')
  MSEList_V2[[i]] <- readRDS(file.path('Update_MSE_Objects/2026', nm))
}

MSEList_V1[[1]]@TAC[1,,1,1,1:6] |> apply(2,sum)
# [1] 12215.00 10921.00 14769.02 14769.02 14769.02
MSEList_V2[[1]]@TAC[1,,1,1,1:6] |> apply(2,sum)
# [1] 12215.00 10921.00 11137 11137 11137

11137/14769.02

years <- 2021:2052
ssb_df <- data.frame(Year = years,
                     V1 = MSEList_V1[[1]]@SSB[1,,1,] |> apply(2, sum),
                     V2 = MSEList_V2[[1]]@SSB[1,,1,] |> apply(2, sum)
) |> round(0)


plot(ssb_df$Year, ssb_df$V1, type='l', col = 'black')
lines(ssb_df$Year, ssb_df$V2, col = 'blue')

ssb_df$Rel <- ssb_df$V2/ssb_df$V1
ssb_df

IndexList_V1 <- list()
IndexList_V2 <- list()
for (i in 1:9) {
  IndexList_V1[[i]] <- MSEList_V1[[i]]@PPD[[1]][[1]][[1]]@Ind[,71:79]
  IndexList_V2[[i]] <- MSEList_V2[[i]]@PPD[[1]][[1]][[1]]@Ind[,71:79]
}

# 2024 Simulated Index
Index2020_2028_V1 <- do.call('rbind', IndexList_V1)
Index2020_2028_V2 <- do.call('rbind', IndexList_V2)





obs <- Index |> dplyr::filter(YearC %in% 2020: 2024) |>
  dplyr::pull(StIndex) |> round(2)

V1_DF <- data.frame(Year=2020:2028,
           Lower=apply(Index2020_2028_V1, 2, quantile, 0.025) |> round(2),
           Mean=apply(Index2020_2028_V1, 2, mean) |> round(2),
           Upper=apply(Index2020_2028_V1, 2, quantile, 0.975) |> round(2),
           Observed = c(obs, rep(NA,4))
)

V2_DF <- data.frame(Year=2020:2028,
           Lower=apply(Index2020_2028_V2, 2, quantile, 0.025) |> round(2),
           Mean=apply(Index2020_2028_V2, 2, mean) |> round(2),
           Upper=apply(Index2020_2028_V2, 2, quantile, 0.975) |> round(2),
           Observed = c(obs, rep(NA,4))
)

V1_DF

V2_DF

# ---- Calculate percentiles ----
hist(Index2020_2028_V1[,5]/mean(Index2020_2028_V1[,5]), breaks = 30,
     main ='', xlab = 'Simulated 2024 Index Data Point (normalized)')
abline(v=Index$StIndex[62]/mean(Index2020_2028_V1[,5]), lwd = 2)

sum(Index2020_2028_V1[,5] <= Index$StIndex[62])

mean((Index2020_2028_V1[,5] <= Index$StIndex[62])) * 100
100 -(mean((Index2020_2028_V1[,5] <= Index$StIndex[62])) * 100)

Data <- new('Data')
Data@Year <- MSEList_V1[[1]]@PPD[[1]][[1]][[1]]@Year
Data@Ind  <- matrix(MSEList_V1[[1]]@PPD[[1]][[1]][[1]]@Ind[1,], nrow = 1)
Data@Ind[match(Index$YearC, Data@Year)] <- Index$StIndex

Data@Ind  <- matrix(Data@Ind[,Data@Year <= 2024], nrow = 1)
Data@Year <- Data@Year[Data@Year <= 2024]

MCC11_b(1, Data)


cbind(Data@Year, Data@Ind[1,])


dimnames(Index2020_2028_V1) <- list(Sim=1:nrow(Index2020_2028_V1),
                                 Year=2020:2028)

dimnames(Index2020_2028_V2) <- list(Sim=1:nrow(Index2020_2028_V2),
                                    Year=2020:2028)

SimIndexDF_V1 <- array2DF(Index2020_2028_V1)
SimIndexDF_V1$Sim <- as.numeric(SimIndexDF_V1$Sim)
SimIndexDF_V1$Year <- as.numeric(SimIndexDF_V1$Year)

SimIndexDF_V2 <- array2DF(Index2020_2028_V2)
SimIndexDF_V2$Sim <- as.numeric(SimIndexDF_V2$Sim)
SimIndexDF_V2$Year <- as.numeric(SimIndexDF_V2$Year)




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

qs_V1 <- SimIndexDF_V1 |> dplyr::group_by(Year) |>
  dplyr::summarise(Lower=quantile(Value,0.001),
                   Upper=quantile(Value, 0.999)
  )

qs_V2 <- SimIndexDF_V2 |> dplyr::group_by(Year) |>
  dplyr::summarise(Lower=quantile(Value,0.001),
                   Upper=quantile(Value, 0.999)
  )

png('img/IndexDensity_2026.png', width=6, height = 4, units='in', res=400)

par(mfrow=c(1,1), mar=c(1,1,1,1), oma=c(3,3,1,1))
plot(c(min(SimIndexDF_V1$Year),max(SimIndexDF_V1$Year)+1),
     c(min(qs_V1$Lower), max(qs_V1$Upper)),
     col='white',xlab="",ylab="")

Yrs <- unique(SimIndexDF_V1$Year)
axis(side=1, labels=Yrs, at=Yrs)
mtext(side=1, outer=TRUE, 'Year', line=1, font=2)
mtext(side=2, outer=TRUE, 'Index', line=1, font=2)

alpha <- 50
col1 <- '#0000ff60'
col2 <- '#A3E2A3'
lwd <- 2

for (i in seq_along(Yrs)) {
  vec_v1 <- SimIndexDF_V1 |> dplyr::filter(Year==Yrs[i]) |> dplyr::pull(Value)
  lower_v1 <- quantile(vec_v1, 0.025)
  upper_v1 <- quantile(vec_v1, 0.975)
  dens_v1 <- densalator(vec_v1)

  vec_v2 <- SimIndexDF_V2 |> dplyr::filter(Year==Yrs[i]) |> dplyr::pull(Value)
  lower_v2 <- quantile(vec_v2, 0.025)
  upper_v2 <- quantile(vec_v2, 0.975)
  dens_v2 <- densalator(vec_v2)


  if (!all(is.na(dens_v1))) {
    ys = 0.95*(dens_v1$y / max(dens_v1$y,na.rm=T))
    polygon(Yrs[i]+ys,dens_v1$x,col=makeTransparent(col1, alpha),border=NA )
    lines(range(Yrs[i]+ys), rep(lower_v1, 2), lty=2, col =col1, lwd =lwd)
    lines(range(Yrs[i]+ys), rep(upper_v1, 2), lty=2, col =col1, lwd =lwd)
  }

  if (!all(is.na(dens_v2))) {
    ys = 0.95*(dens_v2$y / max(dens_v2$y,na.rm=T))
    polygon(Yrs[i]+ys,dens_v2$x,col=makeTransparent(col2, alpha), border=NA)
    lines(range(Yrs[i]+ys), rep(lower_v2, 2), lty=2, col = col2, lwd =lwd)
    lines(range(Yrs[i]+ys), rep(upper_v2, 2), lty=2, col = col2, lwd =lwd)
  }



  Obs <- Index |> dplyr::filter(YearC==Yrs[i]) |> dplyr::pull(StIndex)
  if (length(Obs)>0)
    lines(Yrs[i]+c(0,0.95),rep(Obs,2),lwd=3)
}
legend('topleft', cex=1,legend = c("Predicted V1", "Predicted V2", "Observed"),
       text.col=c(col1, col2, 'black'),
       text.font=2,bty='n')

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
