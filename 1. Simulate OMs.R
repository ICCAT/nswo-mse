library(SWOMSE)



# ---- Reference OMs ----
Ref_OMs <- OM_DF %>% filter(Class=='Reference')

if (!dir.exists('Hist_Objects'))
  dir.create('Hist_Objects')

if (!dir.exists('Hist_Objects/Reference'))
  dir.create('Hist_Objects/Reference')

MOM_Objects <- Ref_OMs$OM.object

for (i in seq_along(MOM_Objects)) {
  MOM <- get(MOM_Objects[i])
  multiHist <- SimulateMOM(MOM, parallel = FALSE, silent=TRUE)

  nm <- paste0(MOM_Objects[i], '.hist')
  saveRDS(multiHist, file.path('Hist_Objects/Reference', nm))
}

# ---- R1. Increasing Catchability ----

R1_OMs <- OM_DF %>% filter(Class=='R1. Increasing q')

if (!dir.exists('Hist_Objects/R1_Increasing_q'))
  dir.create('Hist_Objects/R1_Increasing_q')

MOM_Objects <- R1_OMs$OM.object
MOM <- get(MOM_Objects)
multiHist <- SimulateMOM(MOM, parallel = FALSE, silent=TRUE)

# Increase indices in projections by 1% per year
increase_mat <- t(matrix(1.01^(0:(MOM@proyears-1)), nrow=MOM@proyears, ncol=50, byrow=F))

# Combined Index
dd <- dim(multiHist$Female$`Fleet 1`@SampPars$Obs$Ierr_y)
yr_ind <- (MOM@Fleets[[1]][[1]]@nyears+1):dd[2]
multiHist$Female$`Fleet 1`@SampPars$Obs$Ierr_y[,yr_ind] <- multiHist$Female$`Fleet 1`@SampPars$Obs$Ierr_y[,yr_ind] * increase_mat

# Individual Indices
increase_mat2 <- replicate(7,increase_mat)
increase_mat2 <- aperm(increase_mat2, c(1,3,2))
multiHist$Female$`Fleet 1`@SampPars$Obs$AddIerr[,, yr_ind] <- multiHist$Female$`Fleet 1`@SampPars$Obs$AddIerr[,, yr_ind] *increase_mat2

nm <- paste0(MOM_Objects, '.hist')
saveRDS(multiHist, file.path('Hist_Objects/R1_Increasing_q', nm))


# Plot of Reference and R1 SB_SBMSY and F_FMSY

Ref_OM <- OM_DF %>% filter(M==0.2, steepness==0.8, Class=='Reference')
Rob_OM <- OM_DF %>% filter(M==0.2, steepness==0.8, Class=='R1. Increasing q')

Ref_Hist <- readRDS(file.path('Hist_Objects/Reference', paste0(Ref_OM$OM.object, '.hist')))
Rob_Hist <- readRDS(file.path('Hist_Objects/R1_Increasing_q', paste0(Rob_OM$OM.object, '.hist')))

Ref_DF_SB <- get_SSB(Ref_Hist) %>% filter(Sim==1, Stock=='Female')
Ref_DF_F <- get_F(Ref_Hist) %>% filter(Sim==1, Stock=='Female')
Ref_DF_SB$SB_SBMSY <- Ref_DF_SB$Value/Ref_Hist$Female$`Fleet 1`@Ref$ReferencePoints$SSBMSY[1]
Ref_DF_F$F_FMSY <- Ref_DF_F$Value/Ref_Hist$Female$`Fleet 1`@Ref$ReferencePoints$FMSY[1]

Ref_DF <- left_join(Ref_DF_SB %>% select(Year, SB_SBMSY),
                    Ref_DF_F %>% select(Year, F_FMSY) )
Ref_DF$Model <- 'Reference'


Rob_DF_SB <- get_SSB(Rob_Hist) %>% filter(Sim==1, Stock=='Female')
Rob_DF_F <- get_F(Rob_Hist) %>% filter(Sim==1, Stock=='Female')

Rob_DF_SB$SB_SBMSY <- Rob_DF_SB$Value/Rob_Hist$Female$`Fleet 1`@Ref$ReferencePoints$SSBMSY[1]
Rob_DF_F$F_FMSY <- Rob_DF_F$Value/Rob_Hist$Female$`Fleet 1`@Ref$ReferencePoints$FMSY[1]

Rob_DF <- left_join(Rob_DF_SB %>% select(Year, SB_SBMSY),
                    Rob_DF_F %>% select(Year, F_FMSY) )
Rob_DF$Model <- 'R1'


DF <- bind_rows(Ref_DF, Rob_DF) %>%
  tidyr::pivot_longer(., cols=c(SB_SBMSY, F_FMSY))
DF$Model[DF$Model=='Reference'] <- 'R0'
DF$Model <- factor(DF$Model, levels=c('R0', 'R1', ordered=TRUE))

ggplot(DF, aes(x=Year, y=value, color=Model)) +
  facet_wrap(~name, scales='free_y') +
  expand_limits(y=0) +
  geom_line(linewidth=1) +
  geom_hline(yintercept=1, linetype=2) +
  theme_bw()

ggsave('img/R1_Increasing_q/Compare.png', width=8, height=3)

# ---- Plot Indices and Biomass ----

mmse <- ProjectMOM(Rob_Hist, MPs='FMSYref')

Biomass <- get_Biomass(mmse) %>% filter(Sim==1)
get_Index <- function(MMSE) {
  nsim <- MMSE@nsim
  nstocks <- MMSE@nstocks
  nfleets <- MMSE@nfleets
  Years <- get_Years(MMSE)
  Years <- Years$Year[1:(nrow(Years)-1)]

  MPs <- MMSE@MPs[[1]]
  nMPs <- length(MPs)

  df_list <- list()
  for (mm in 1:nMPs) {
    df_list[[mm]] <- data.frame(Sim=1:nsim,
                                Year=rep(Years, each=nsim),
                                Index=as.vector(MMSE@PPD[[1]][[1]][[mm]]@Ind),
                                MP=MPs[mm])
  }
  do.call('rbind',df_list )
}

CI <- get_Index(mmse) %>% filter(Sim==1)

Biomass <- Biomass %>% group_by(Year) %>% summarise(Value=sum(Value))

Biomass$include <- FALSE
Biomass$include[Biomass$Year %in% 1999:2020] <- TRUE
b_mean <- mean(Biomass$Value[Biomass$include==TRUE])
Biomass$stB <- Biomass$Value/b_mean

CI$StIndex <- CI$Index/mean(CI$Index[CI$Year %in% 1999:2020])

df <- left_join(Biomass, CI)

ggplot(df, aes(x=Year)) +
  geom_line(aes(y=stB), color='blue') +
  geom_line(aes(y=StIndex), color='red') +
  expand_limits(y=0) +
  labs(y='Combined Index') +
  theme_bw()
ggsave('img/R1_Increasing_q/Index.png', width=4, height=3)


# R2 ----
R1_OMs <- OM_DF %>% filter(Class=='R1. Increasing q')

MOM_Objects <- R1_OMs$OM.object
MOM <- get(MOM_Objects)
multiHist <- SimulateMOM(MOM, parallel = FALSE, silent=TRUE)

if (!dir.exists('Hist_Objects/R2'))
  dir.create('Hist_Objects/R2')

saveRDS(multiHist, file.path('Hist_Objects/R2', 'MOM_010.hist'))


# R3a and R3b ----
R3_OM <- OM_DF %>% filter(Class=='Reference', M==0.2, steepness==0.8)


MOM_Objects <- R3_OM$OM.object
MOM <- get(MOM_Objects)
multiHist <- SimulateMOM(MOM, parallel = FALSE, silent=TRUE)

series <- read.csv('inst/R3_series.csv') # series <- read.csv('C:/Users/tcarruth/Documents/GitHub/nswo-mse/inst/R3_series.csv')


inflate <- 2

R3a <- c(exp(series$R3a * inflate),1)
R3b <- c(exp(series$R3b * inflate),1)

rec_devs <- data.frame(Year=2021:2054, R0=1, R3a=R3a, R3b=R3b) %>%
  tidyr::pivot_longer(., cols=c(R0, R3a, R3b))
rec_devs$name <- factor(rec_devs$name, levels=c('R0', 'R3a', 'R3b'), ordered = TRUE)

ggplot(rec_devs, aes(x=Year, y=value)) +
  facet_wrap(~name) +
  geom_line() + theme_bw() +
  expand_limits(y=0) +
  geom_hline(yintercept = 1, linetype=3) +
  labs(y='Recruitment Deviation')

ggsave('img/R3/RecDevs_mean.png', width=6, height=2.5)

# R3a
# increased recruitment variability
dd <- dim(multiHist$Female$`Fleet 1`@SampPars$Stock$Perr_y)
pind <- (dd[2]+1 - MOM@proyears):dd[2]
devs <- multiHist$Male$`Fleet 1`@SampPars$Stock$Perr_y[,pind]
new_devs_R3a <- new_devs_R3b <- devs # existing rec devs

for (y in 1:MOM@proyears) {
  new_devs_R3a[,y] <- new_devs_R3a[,y] * R3a[y]
  new_devs_R3b[,y] <- new_devs_R3b[,y] * R3b[y]
}

multiHist_R3a <- multiHist
multiHist_R3a$Female$`Fleet 1`@SampPars$Stock$Perr_y[,pind] <- new_devs_R3a
multiHist_R3a$Male$`Fleet 1`@SampPars$Stock$Perr_y[,pind] <- new_devs_R3a

multiHist_R3b <- multiHist
multiHist_R3b$Female$`Fleet 1`@SampPars$Stock$Perr_y[,pind] <- new_devs_R3b
multiHist_R3b$Male$`Fleet 1`@SampPars$Stock$Perr_y[,pind] <- new_devs_R3b

if (!dir.exists('Hist_Objects/R3a'))
  dir.create('Hist_Objects/R3a')
saveRDS(multiHist_R3a, file.path('Hist_Objects/R3a', 'MOM_005.hist'))

if (!dir.exists('Hist_Objects/R3b'))
  dir.create('Hist_Objects/R3b')
saveRDS(multiHist_R3b, file.path('Hist_Objects/R3b', 'MOM_005.hist'))



df1 <-data.frame(Sim=1:MOM@nsim,
                 Year=rep(2021:2054, each=MOM@nsim),
                 Rec_Dev=as.vector(devs),
                 Model='Reference')

df2 <-data.frame(Sim=1:MOM@nsim,
                 Year=rep(2021:2054, each=MOM@nsim),
                 Rec_Dev=as.vector(new_devs_R3a),
                 Model='R3a')

df3 <-data.frame(Sim=1:MOM@nsim,
                 Year=rep(2021:2054, each=MOM@nsim),
                 Rec_Dev=as.vector(new_devs_R3b),
                 Model='R3b')

df <- bind_rows(df1, df2, df3)

sims <- sample(1:MOM@nsim, 3)
pdf <- df %>% filter(Sim%in%sims)
pdf$Sim <- factor(pdf$Sim)
pdf$Model[pdf$Model=='Reference'] <- 'R0'
pdf$Model <- factor(pdf$Model, levels=c('R0', 'R3a', 'R3b'), ordered = TRUE)
pdf$Sim <- 1:3
ggplot(pdf, aes(x=Year, y=Rec_Dev, linetype=Model, color=Model)) +
  facet_grid(~Sim) +
  geom_line() +
  theme_bw() +
  geom_hline(yintercept = 1, linetype=3) +
  labs(y='Recruitment Deviations') +
  scale_color_manual(values=c('black', 'red', 'blue'))


ggsave('img/R3/RecDevs.png', width=6, height=2.5)


# R4 ----
multiHist <- readRDS(file.path('Hist_Objects/Reference', 'MOM_005.hist'))

multiHist_mod <- multiHist
multiHist_mod$Female$`Fleet 1`@SampPars$Imp$TAC_y[] <- 1.1
multiHist_mod$Male$`Fleet 1`@SampPars$Imp$TAC_y[] <- 1.1

multiHist_mod$Female$`Fleet 1`@SampPars$Obs$Cobs_y[] <- 1/1.1
multiHist_mod$Male$`Fleet 1`@SampPars$Obs$Cobs_y[] <- 1/1.1

if (!dir.exists('Hist_Objects/R4'))
  dir.create('Hist_Objects/R4')

saveRDS(multiHist_mod, file.path('Hist_Objects/R4', 'MOM_005.hist'))



