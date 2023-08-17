library(SWOMSE)

# ---- Reference OMs ----
Ref_OMs <- OM_DF %>% filter(Class=='Reference')

if (!dir.exists('Hist_Objects'))
  dir.create('Hist_Objects')

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
multiHist$Female$`Fleet 1`@SampPars$Obs$Ierr_y[,72:104] <- multiHist$Female$`Fleet 1`@SampPars$Obs$Ierr_y[,72:104] * increase_mat

# Individual Indices
increase_mat2 <- replicate(7,increase_mat)
increase_mat2 <- aperm(increase_mat2, c(1,3,2))
multiHist$Female$`Fleet 1`@SampPars$Obs$AddIerr[,, 72:104] <- multiHist$Female$`Fleet 1`@SampPars$Obs$AddIerr[,, 72:104] *increase_mat2

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
Rob_DF$Model <- 'R1. Increasing q'


DF <- bind_rows(Ref_DF, Rob_DF) %>%
  tidyr::pivot_longer(., cols=c(SB_SBMSY, F_FMSY))
DF$Model <- factor(DF$Model, levels=c('Reference', 'R1. Increasing q', ordered=TRUE))

ggplot(DF, aes(x=Year, y=value, color=Model)) +
  facet_wrap(~name, scales='free_y') +
  expand_limits(y=0) +
  geom_line(linewidth=1) +
  geom_hline(yintercept=1, linetype=2) +
  theme_bw()

ggsave('img/R1_Increasing_q/Compare.png', width=8, height=3)

# ---- Plot Indices and Biomass ----

mmse <- ProjectMOM(multiHist, MPs='FMSYref')

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

