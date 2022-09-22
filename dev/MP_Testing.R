



ggplot(res_DF %>% filter(Name!='SS3'), aes(x=Year, y=B_BMSY, color=Name)) +
  expand_limits(y=c(0,1)) +
  facet_wrap(~Name) +
  theme_bw() +
  geom_line()

ggplot(res_DF %>% filter(Name!='SS3'), aes(x=Year, y=B_BMSY, color=Name)) +
  expand_limits(y=c(0,1)) +
  theme_bw() +
  geom_line() +
  geom_line(data=res_DF %>% filter(Name=='SS3'), color='black', linetype=2) +
  ylab('SB/SBMSY')

ggplot(res_DF %>% filter(Name!='SS3'), aes(x=Year, y=B/1000, color=Name)) +
  expand_limits(y=0) +
  theme_bw() +
  geom_line() +
  geom_line(data=res_DF %>% filter(Name=='SS3'), color='black', linetype=2) +
  ylab('Vulnerable biomass (1000 t)')









# Add extreme OMs
TSBio_List <- readRDS(file.path(obj.dir, 'TSBio_List.rda'))

TSBio_List[[1]] %>% head()

replist$timeseries %>% head()
replist$natage_annual_2_with_fishery %>% head()
265643
228969

library(r4ss)
SSdir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/grid_2022/000_base_case'
replist <- r4ss::SS_output(SSdir)
SS_SB <- replist$timeseries %>% filter(Yr%in%1950:2020) %>% select(Year=Yr, SB=SpawnBio)
SS_B_BMSY <- replist$Kobe %>% select(Year=Yr, B_BMSY=B.Bmsy)
SS_SBMSY <- mean(SS_SB$SB / SS_B_BMSY$B_BMSY)

replist$cpue %>% filter(Fleet_name == 'SPN_1')

SSplotIndices(replist, subplots = 8)


MOM <- MOM_000
MOM@nsim <- 2
multiHist <- SimulateMOM(MOM, parallel = FALSE)
SB <- rowSums(multiHist$Female[[1]]@TSdata$SBiomass[1,,])
SB_SBMSY <- SB/SS_SBMSY
SS_VB <- rowSums(multiHist$Female[[1]]@TSdata$VBiomass[1,,]) +
  rowSums(multiHist$Male[[1]]@TSdata$VBiomass[1,,])

SS_df <- data.frame(Name='SS3', Year=1950:2020,
                    B_BMSY=SB_SBMSY,
                    B=SS_VB)

res_DF <- bind_rows(res_DF, SS_df)
res_DF$Name <- factor(res_DF$Name, levels=unique(res_DF$Name), ordered = TRUE)

ggplot(res_DF, aes(x=Year, y=B_BMSY, color=Name)) +
  expand_limits(y=c(0,1)) +
  facet_wrap(~Name) +
  theme_bw() +
  geom_line()


ggplot(res_DF %>% filter(Name!='SS3'), aes(x=Year, y=B_BMSY, color=Name)) +
  expand_limits(y=c(0,1)) +
  facet_wrap(~Name) +
  theme_bw() +
  geom_line()

ggplot(res_DF %>% filter(Name!='SS3'), aes(x=Year, y=B_BMSY, color=Name)) +
  expand_limits(y=c(0,1)) +
  theme_bw() +
  geom_line() +
  geom_line(data=res_DF %>% filter(Name=='SS3'), color='black', linetype=2) +
  ylab('SB/SBMSY')

ggplot(res_DF %>% filter(Name!='SS3'), aes(x=Year, y=B/1000, color=Name)) +
  expand_limits(y=0) +
  theme_bw() +
  geom_line() +
  geom_line(data=res_DF %>% filter(Name=='SS3'), color='black', linetype=2) +
  ylab('Vulnerable biomass (1000 t)')

SS_SB %>%  filter(Year>2000)
res_DF %>% filter(Name=='SS3', Year>2000)






OM.root <- "G:/My Drive/1_Projects/North_Atlantic_Swordfish/OMs"
TSbio_dat <- readRDS(paste0(OM.root, '/OM_objects/TSBio_List.rda'))[[1]]
RefPoint_DF <- readRDS(paste0(OM.root, '/OM_objects/RefPoint_DF.rda'))
ref_points <- RefPoint_DF %>% filter(i==1) %>% select(-one_of('i'))
TSbio_dat$SBMSY <- ref_points$SBMSY

years <- min(TSbio_dat$year):TSbio_dat$year[which(TSbio_dat$Depletion == TSbio_dat$SS_Depletion)]
SSB_DF <- TSbio_dat %>% filter(year %in% years)

df <- data.frame(Name='SS3', Year=1950:2020, B_BMSY=SSB_DF$B.Bmsy)
res_DF <- bind_rows(res_DF, df)

library(ggplot2)
ggplot(res_DF, aes(x=Year, y=B_BMSY, color=Name)) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw()


ggplot(res_DF %>% dplyr::filter(Name %in% c('SP', 'SP_SS', 'spict')),
       aes(x=Year, y=B_BMSY, color=Name)) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw()


ggplot(res_DF %>% dplyr::filter(Name %in% c('spict', 'SP')),
       aes(x=Year, y=B_BMSY, color=Name)) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw()







