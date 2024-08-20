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
increase_mat <- t(matrix(1.01^(0:(MOM@proyears-1)), nrow=MOM@proyears, ncol=MOM@nsim, byrow=F))

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


# # ---- R1a. Increasing Catchability 2% ----
#
# R1a_OMs <- OM_DF %>% filter(Class=='R1a. Increasing Q2')
#
# if (!dir.exists('Hist_Objects/R1a_Increasing_q'))
#   dir.create('Hist_Objects/R1a_Increasing_q')
#
# MOM_Objects <- R1a_OMs$OM.object
# MOM <- get(MOM_Objects)
# multiHist <- SimulateMOM(MOM, parallel = FALSE, silent=TRUE)
#
# # Increase indices in projections by 1% per year
# increase_mat <- t(matrix(1.01^(0:(MOM@proyears-1)), nrow=MOM@proyears, ncol=MOM@nsim, byrow=F))
#
# # Combined Index
# dd <- dim(multiHist$Female$`Fleet 1`@SampPars$Obs$Ierr_y)
# yr_ind <- (MOM@Fleets[[1]][[1]]@nyears+1):dd[2]
# multiHist$Female$`Fleet 1`@SampPars$Obs$Ierr_y[,yr_ind] <- multiHist$Female$`Fleet 1`@SampPars$Obs$Ierr_y[,yr_ind] * increase_mat
#
# # Individual Indices
# increase_mat2 <- replicate(7,increase_mat)
# increase_mat2 <- aperm(increase_mat2, c(1,3,2))
# multiHist$Female$`Fleet 1`@SampPars$Obs$AddIerr[,, yr_ind] <- multiHist$Female$`Fleet 1`@SampPars$Obs$AddIerr[,, yr_ind] *increase_mat2
#
# nm <- paste0(MOM_Objects, '.hist')
# saveRDS(multiHist, file.path('Hist_Objects/R1a_Increasing_q', nm))



# R2 ---- Historical Only
R1_OMs <- OM_DF %>% filter(Class=='R1. Increasing q')

MOM_Objects <- R1_OMs$OM.object
MOM <- get(MOM_Objects)
multiHist <- SimulateMOM(MOM, parallel = FALSE, silent=TRUE)

if (!dir.exists('Hist_Objects/R2'))
  dir.create('Hist_Objects/R2')

saveRDS(multiHist, file.path('Hist_Objects/R2', 'MOM_010.hist'))

# R2a ---- (new R3)

R1_OMs <- OM_DF %>% filter(Class=='R1a. Increasing Q2')

MOM_Objects <- R1_OMs$OM.object
MOM <- get(MOM_Objects)
multiHist <- SimulateMOM(MOM, parallel = FALSE, silent=TRUE)

if (!dir.exists('Hist_Objects/R2a'))
  dir.create('Hist_Objects/R2a')

saveRDS(multiHist, file.path('Hist_Objects/R2a', 'MOM_011.hist'))



# Plot of Reference and R1 & R1a SB_SBMSY and F_FMSY ----

Ref_OM <- OM_DF %>% filter(M==0.2, steepness==0.8, Class=='Reference')
Rob_OM <- OM_DF %>% filter(M==0.2, steepness==0.8, Class=='R1. Increasing q')
Rob_OM_a <- OM_DF %>% filter(M==0.2, steepness==0.8, Class=='R1a. Increasing Q2')

Ref_Hist <- readRDS(file.path('Hist_Objects/Reference', paste0(Ref_OM$OM.object, '.hist')))
Rob_Hist <- readRDS(file.path('Hist_Objects/R1_Increasing_q', paste0(Rob_OM$OM.object, '.hist')))
Rob_Hist_a <- readRDS(file.path('Hist_Objects/R1a_Increasing_q', paste0(Rob_OM_a$OM.object, '.hist')))

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


Rob_DF_SB_a <- get_SSB(Rob_Hist_a) %>% filter(Sim==1, Stock=='Female')
Rob_DF_F_a <- get_F(Rob_Hist_a) %>% filter(Sim==1, Stock=='Female')

Rob_DF_SB_a$SB_SBMSY <- Rob_DF_SB_a$Value/Rob_Hist$Female$`Fleet 1`@Ref$ReferencePoints$SSBMSY[1]
Rob_DF_F_a$F_FMSY <- Rob_DF_F_a$Value/Rob_Hist$Female$`Fleet 1`@Ref$ReferencePoints$FMSY[1]

Rob_DF_a <- left_join(Rob_DF_SB_a %>% select(Year, SB_SBMSY),
                      Rob_DF_F_a %>% select(Year, F_FMSY) )
Rob_DF_a$Model <- 'R3'


DF <- bind_rows(Ref_DF, Rob_DF, Rob_DF_a) %>%
  tidyr::pivot_longer(., cols=c(SB_SBMSY, F_FMSY))
DF$Model[DF$Model=='Reference'] <- 'R0'
DF$Model <- factor(DF$Model, levels=c('R0', 'R1', 'R3', ordered=TRUE))

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

st_yrs <- 1999:2022

CI$StIndex <- CI$Index/mean(CI$Index[CI$Year %in% st_yrs])


Biomass$include <- FALSE
Biomass$include[Biomass$Year %in% st_yrs] <- TRUE
b_mean <- mean(Biomass$Value[Biomass$include==TRUE])
Biomass$stB <- Biomass$Value/b_mean


df <- left_join(Biomass, CI)

ggplot(df, aes(x=Year)) +
  geom_line(aes(y=stB), color='blue') +
  geom_line(aes(y=StIndex), color='red') +
  expand_limits(y=0) +
  labs(y='Combined Index') +
  theme_bw()

ggsave('img/R1_Increasing_q/Index.png', width=4, height=3)


# R3a and R3b ---- (new R4 and R5)
R3_OM <- OM_DF %>% filter(Class=='Reference', M==0.2, steepness==0.8)

series <- read.csv('inst/R3_series.csv') # series <- read.csv('C:/Users/tcarruth/Documents/GitHub/nswo-mse/inst/R3_series.csv')
series <- series[1:MOM@proyears,]

inflate <- 2

R3a <- c(exp(series$R3a * inflate))
R3b <- c(exp(series$R3b * inflate))

rec_devs <- data.frame(Year=2023:2054, R0=1, R4=R3a, R5=R3b) %>%
  tidyr::pivot_longer(., cols=c(R0, R4, R5))
rec_devs$name <- factor(rec_devs$name, levels=c('R0', 'R4', 'R5'), ordered = TRUE)

ggplot(rec_devs, aes(x=Year, y=value)) +
  facet_wrap(~name) +
  geom_line() + theme_bw() +
  expand_limits(y=0) +
  geom_hline(yintercept = 1, linetype=3) +
  labs(y='Recruitment Deviation')

ggsave('img/R3/RecDevs_mean.png', width=6, height=2.5)



MOM_Objects <- R3_OM$OM.object
MOM <- get(MOM_Objects)
multiHist <- SimulateMOM(MOM, parallel = FALSE, silent=TRUE)


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
                 Year=rep(2023:2054, each=MOM@nsim),
                 Rec_Dev=as.vector(devs),
                 Model='Reference')

df2 <-data.frame(Sim=1:MOM@nsim,
                 Year=rep(2023:2054, each=MOM@nsim),
                 Rec_Dev=as.vector(new_devs_R3a),
                 Model='R4')

df3 <-data.frame(Sim=1:MOM@nsim,
                 Year=rep(2023:2054, each=MOM@nsim),
                 Rec_Dev=as.vector(new_devs_R3b),
                 Model='R5')

df <- bind_rows(df1, df2, df3)

sims <- sample(1:MOM@nsim, 3)
pdf <- df %>% filter(Sim%in%sims)
pdf$Sim <- factor(pdf$Sim)
pdf$Model[pdf$Model=='Reference'] <- 'R0'
pdf$Model <- factor(pdf$Model, levels=c('R0', 'R4', 'R5'), ordered = TRUE)
pdf$Sim <- 1:3
ggplot(pdf, aes(x=Year, y=Rec_Dev, linetype=Model, color=Model)) +
  facet_grid(~Sim) +
  geom_line() +
  theme_bw() +
  geom_hline(yintercept = 1, linetype=3) +
  labs(y='Recruitment Deviations') +
  scale_color_manual(values=c('black', 'red', 'blue'))


ggsave('img/R3/RecDevs.png', width=6, height=2.5)


# R4 ---- new R6
multiHist <- readRDS(file.path('Hist_Objects/Reference', 'MOM_005.hist'))

multiHist_mod <- multiHist
multiHist_mod$Female$`Fleet 1`@SampPars$Imp$TAC_y[] <- 1.1
multiHist_mod$Male$`Fleet 1`@SampPars$Imp$TAC_y[] <- 1.1

multiHist_mod$Female$`Fleet 1`@SampPars$Obs$Cobs_y[] <- 1/1.1
multiHist_mod$Male$`Fleet 1`@SampPars$Obs$Cobs_y[] <- 1/1.1

if (!dir.exists('Hist_Objects/R4'))
  dir.create('Hist_Objects/R4')

saveRDS(multiHist_mod, file.path('Hist_Objects/R4', 'MOM_005.hist'))



