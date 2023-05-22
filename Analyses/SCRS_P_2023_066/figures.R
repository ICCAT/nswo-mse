
obj <- readRDS('dev/MP_tuning/Tuning_Objects/IR1_PGK_med_0.6.rda')
out.dir <- 'C:/Users/User/Documents/GitHub/nswo-mse/Analyses/SCRS_P_2023_066'

tt <- obj$df %>% filter(i==max(i))
tt[seq(2, by=3, to=17),]

mselist <- readRDS('Analyses/MSE/MSElist.mmse')
MSE_all <- combine_MMSE(mselist, 'Ref')

# PGK_med
year_range <- 2029:2033

p1 <- Kobe(mselist[[1]], mp='IR1_a', year_range = year_range)
p2 <- Kobe(mselist[[2]], mp='IR1_a', year_range = year_range)
p3 <- Kobe(mselist[[3]], mp='IR1_a', year_range = year_range)
p4 <- Kobe(mselist[[4]], mp='IR1_a', year_range = year_range)
p5 <- Kobe(mselist[[5]], mp='IR1_a', year_range = year_range)
p6 <- Kobe(mselist[[6]], mp='IR1_a', year_range = year_range)

p <- cowplot::plot_grid(p1,p2,p3,p4,p5,p6, nrow=2)

ggsave(file.path(out.dir, 'RefOMs.png'), p, width=9, height=6)

p7 <- Kobe(MSE_all, mp='IR1_a', year_range = year_range)

ggsave(file.path(out.dir, 'RefOMs2.png'), p7, width=7, height=6)

p1 <- SB_SBMSY_TS(MSE_all, mp='IR1_a', year_range = year_range)
p2<- F_FMSY_TS(MSE_all, mp='IR1_a', year_range = year_range)
p <- cowplot::plot_grid(p1,p2, nrow=2)
ggsave(file.path(out.dir, 'RefOMs3.png'), p, width=9, height=8)


# Demonstrate CMPS

# SP1_a
MMSE <- mselist[[1]]
MMSE@MPs[[1]]
AllData <- MMSE@PPD[[1]][[1]][[10]]

pyears <- 2021:2053
yr <- 2024


ind <- which(AllData@Year == yr-1)
Data <- AllData
Data@Cat <- Data@Cat[,1:ind]
Data@Ind <- Data@Ind[,1:ind]
Data@Year <- Data@Year[1:ind]

x <- 1
Data_Lag = 1
Interval = 3
tunepar = 2.404
mc = 0.25
Data <- Lag_Data(Data, Data_Lag)

yy <- match(yr-1, pyears)
LastTAC <- sum(MMSE@TAC[x,,1,10,yy])

Mod <- SAMtool::SP(x, Data)
Bthresh <- Mod@BMSY
Blim <- 0.4 * Bthresh
Ftar <- 0.8 * tunepar * Mod@FMSY
Fmin <- 0.1 * tunepar * Mod@FMSY
Bcurr <- Mod@B[length(Mod@B)]
if (Bcurr >= Bthresh) {
  Fmort <- Ftar
}else if (Bcurr > Blim) {
  Fmort <- Ftar * (-0.367 + 1.167 * Bcurr/Bthresh)
}else {
  Fmort <- Fmin
}
M <- 0.2
Z <- M + Fmort
TAC1 <- Fmort/Z * (1 - exp(-Z)) * Bcurr
TAC <- MaxChange(TAC1, LastTAC, mc)

## IR1_a
AllData <- MMSE@PPD[[1]][[1]][[7]]
Years <- get_Years(MMSE)
df <- data.frame(Years %>% filter(Year<=2052), Index=AllData@Ind[1,],
                 Catch=AllData@Cat[1,])
df <- df %>% filter(Year<=2023)
df$Catch[length(df$Catch)] <-NA

ggplot(df , aes(x=Year, y=Index)) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw()

df2 <- df
df$Index[length(df$Index)] <-NA
ggplot(df, aes(x=Year, y=Index)) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw()

y <- length(df$Year)-1
yy <- match(yr-1, pyears)
LastTAC <- sum(MMSE@TAC[1,,1,10,yy])

p1 <- ggplot(df, aes(x=Year, y=Index)) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw() +
  geom_line(data =data.frame(Year=2020:2022, Index=mean(df$Index[(y-2):y])), color='blue') +
  geom_line(data =data.frame(Year=2017:2019, Index=mean(df$Index[(y-2):(y-3)])), color='red')

ratio <- mean(df$Index[(y-2):y])/mean(df$Index[(y-5):(y-3)])

catch <- ratio * 0.791  * LastTAC

p2 <- ggplot(df, aes(x=Year, y=Catch)) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw() +
  geom_point(data=data.frame(Year=2024, Catch=catch), size=3)

p <- cowplot::plot_grid(p1,p2, nrow=2)
ggsave(file.path(out.dir, 'IR1_a.png'), p, width=9, height=8)



## IR2_a
AllData <- MMSE@PPD[[1]][[1]][[4]]
Years <- get_Years(MMSE)
df <- data.frame(Years %>% filter(Year<=2052), Index=AllData@Ind[1,],
                 Catch=AllData@Cat[1,])
df <- df %>% filter(Year<=2023)
df$Catch[length(df$Catch)] <-NA

ggplot(df , aes(x=Year, y=Index)) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw()

df2 <- df
df$Index[length(df$Index)] <-NA
ggplot(df, aes(x=Year, y=Index)) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw()

y <- length(df$Year)-1
yy <- match(yr-1, pyears)
LastTAC <- sum(MMSE@TAC[1,,1,10,yy])

p1 <- ggplot(df, aes(x=Year, y=Index)) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw() +
geom_line(data =data.frame(Year=2021:2022, Index=mean(df$Index[(y-1):y])), color='blue') +
  geom_line(data =data.frame(Year=2019:2020, Index=mean(df$Index[(y-3):(y-2)])), color='red')


ratio <- mean(df$Index[(y-1):y])/mean(df$Index[(y-3):(y-2)])

catch <- ratio * 1.214 * LastTAC
catch2 <- MaxChange(catch, LastTAC, 0.25)

p2 <- ggplot(df, aes(x=Year, y=Catch)) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw() +
  geom_point(data=data.frame(Year=2024, Catch=catch2), size=3)

p <- cowplot::plot_grid(p1,p2, nrow=2)

ggsave(file.path(out.dir, 'IR2_a.png'), p, width=9, height=8)


