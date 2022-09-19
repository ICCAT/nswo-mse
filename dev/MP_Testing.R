library(SWOMSE)

img.dir <- 'img/SP_assess'
obj.dir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/OM_objects'

# Apply different Assessment models to SWOData ----

# openMSE assessment models
SP_assess <- SP(1, SWOData)
SP_Fox_assess <- mod2 <- SP_Fox(1, SWOData)
SP_Fox_assess@Model <- 'SP_Fox'
SP_SS_assess <- SP_SS(1, SWOData)

# JABBA
catch <- data.frame(SWOData@Year, SWOData@Cat[1,])
index <- data.frame(SWOData@Year,SWOData@Ind[1,])
index_se <- data.frame(SWOData@Year, SWOData@CV_Ind[1,])
index_se[,2] <- 0.23

input <- JABBA::build_jabba(catch=catch,
                            cpue=index,
                            se=index_se,
                            catch.cv=0.01,
                            assessment="SWO",
                            scenario = "1",
                            model.type = "Schaefer",
                            sigma.est = FALSE,
                            fixed.obsE = 0.01,
                            r.prior = c(0.42, 0.4),
                            psi.dist='beta',
                            psi.prior=c(0.95, 0.05),
                            verbose=FALSE)

# apply assessment model
JABBA_assess <- JABBA::fit_jabba(input,quickmcmc=TRUE, verbose=FALSE)
JABBA_assess$timeseries[1,,3]

# SPICT
library(MSEextra)

SPICT_assess <- MSEextra::spict(1, SWOData)


Extract_Results <- function(mod) {
  Name <- mod@Model
  B <- mod@B
  B_BMSY <- mod@B_BMSY
  Year <- as.numeric(names(B) )
  data.frame(Name, Year, B_BMSY=B_BMSY, B=B)
}

res_List <- lapply(list(SP_assess, SP_Fox_assess, SP_SS_assess, SPICT_assess), Extract_Results)

res_DF <- do.call('rbind', res_List)

# add JABBA
jabba_df <- data.frame(Name='JABBA', Year=1950:2020,
                 B_BMSY=as.numeric(JABBA_assess$timeseries[,1,3]),
                 B=as.numeric(JABBA_assess$timeseries[,1,1]))
res_DF <- bind_rows(res_DF, jabba_df)


## Plot Assessments  ----
res_DF$Name <- factor(res_DF$Name, levels=unique(res_DF$Name), ordered = TRUE)
nms <- levels(res_DF$Name)

ymax <- res_DF$B_BMSY %>% max() %>% ceiling()

cols <- RColorBrewer::brewer.pal(5, 'Set1')
for (i in 1:length(nms)) {
  nm <- nms[1:i]
  p <- ggplot(res_DF %>% filter(Name %in% nm),
         aes(x=Year, y=B_BMSY, color=Name)) +
    expand_limits(y=c(0,ymax)) +
    theme_bw() +
    geom_line() +
    ylab(expression(SB/SB[MSY])) +
    scale_color_manual(values = cols[1:i])
  image.name <- paste0('SB_SBMSY_', i, '.png')
  ggsave(file.path(img.dir, image.name))
}



ggplot(res_DF, aes(x=Year, y=B_BMSY, color=Name)) +
  expand_limits(y=c(0,1)) +
  theme_bw() +
  geom_line() +
  ylab(expression(SB/SB[MSY]))

ggplot(res_DF, aes(x=Year, y=B_BMSY, color=Name)) +
  expand_limits(y=c(0,1)) +
  facet_wrap(~Name) +
  theme_bw() +
  geom_line() +
  ylab(expression(SB/SB[MSY]))


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



OMs <- OM_DF %>% filter(sigmaR==0.6, cpuelambda==1, llq==1, env==1)

OMs$OM.name <- paste0('MOM_', OMs$OM.num)

# Simulate Historical Fisheries ----
for (i in 1:nrow(OMs)) {
  message(i, '/', nrow(OMs))
  name <- OMs$OM.name[i]
  MOM <- get(name)
  multiHist <- try(SimulateMOM(MOM, parallel = TRUE))
  saveRDS(multiHist, paste0('results/Reference/', name, '.hist'))
  if(exists('multiHist')) rm(multiHist)
}

# Closed Loop Projections ----
MPs <- c('SP_1', 'SP_2', 'SP_3',
         'SP_Fox_1', 'SP_Fox_2', 'SP_Fox_3',
         'SP_SS_1', 'SP_SS_2', 'SP_SS_3')

for (i in 1:nrow(OMs)) {
  message(i, '/', nrow(OMs))
  name <- OMs$OM.name[i]
  MOM <- get(name)
  multiHist <- readRDS(paste0('results/Reference/', name, '.hist'))

  MMSE <- try(ProjectMOM(multiHist, MPs=MPs, parallel = TRUE))
  saveRDS(MMSE, paste0('results/Reference/', name, '.mmse'))
  if(exists('MMSE')) rm(MMSE)
}





# Select OMs
OMs <- OM_DF %>% filter(sigmaR==0.6, cpuelambda==1, llq==1, env==1)

OMs$OM.name <- paste0('MOM_', OMs$OM.num)

MPs <-  c("ITarget_1", "ITarget_2", "SP_1", "SP_Fox_1")

MOM <- MOM_001

st <- Sys.time()
multiHist1 <- SimulateMOM(MOM, parallel = FALSE)
elapse1 <- Sys.time() - st

setup()
st <- Sys.time()
multiHist2 <- SimulateMOM(MOM, parallel = TRUE)
elapse2 <- Sys.time() - st

elapse1
elapse2

st <- Sys.time()
multiHist1 <- ProjectMOM(multiHist1, MPs=MPs,parallel = FALSE)
elapse3 <- Sys.time() - st

setup()
sfLibrary("SWOMSE", character.only = TRUE, verbose=FALSE)
st <- Sys.time()
multiHist2 <- ProjectMOM(multiHist1, MPs=MPs, parallel = TRUE)
elapse4 <- Sys.time() - st

elapse3
elapse4


sfExport()






# 1. Update PMs

