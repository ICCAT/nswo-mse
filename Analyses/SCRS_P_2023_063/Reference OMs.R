library(SWOMSE)
library(ggplot2)

OM.root <- 'G:/My Drive/1_Projects/North_Atlantic_Swordfish/OMs'
out.dir <- 'Analyses/SCRS_P_2023_063'

getTS <- function(i, om_df) {
  dir <- basename(om_df$dir[i])
  TSbio <- readRDS(file.path(OM.root, '/OM_objects/Timeseries', dir, 'timeseries.rda'))
  df <- om_df[i,]
  df <- df %>% select(-dir)
  cbind(df, TSbio)
}

om_df <- OM_DF %>% filter(Class %in% c('Reference', 'R1. Lower Steepness'))
TSBio_List <- lapply(1:nrow(om_df), getTS, om_df=om_df)
TSBio <- do.call('rbind', TSBio_List)
TSBio$M <- factor(TSBio$M)
TSBio$steepness <- factor(TSBio$steepness)

ggplot(TSBio, aes(year, y=B.Bmsy, color=steepness)) +
  facet_wrap(~M) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw() +
  geom_hline(yintercept = 1, linetype=3) +
  labs(x="Year", y=expression(SB/SB[MSY]),
       color='Steepness (h)')

ggsave(file.path(out.dir, 'Ref_OMs.png'), width=9, height=3)

# BH Steepness

h2CR <- function(h) {
  (4*h)/(1-h)
}

CR2h <- function(CR) {
  CR/(CR+4)
}

BH_SRR <- function(R0, h, SB, SBpR) {
  (4*R0*h*SB)/(SBpR*R0*(1-h)+(5*h-1)*SB)
}

# Plot steepness BH
SB0 <- 1
R0 <- 1
SBpR <- SB0/R0
SB <- seq(0, SB0, length.out=1000)
h_vals <- c(0.6, 0.75, 0.9)
out_list <- list()
for (i in seq_along(h_vals)) {
  h <- h_vals[i]
  out_list[[i]] <- data.frame(SB=SB, Rec=BH_SRR(R0, h, SB, SBpR), h=h)
}

df <- do.call('rbind', out_list)
df$h <- factor(df$h)
df$linetype <-  1
df$linetype[df$h==0.6] <- 3
df$linetype <- factor(df$linetype)

ggplot(df, aes(x=SB, y=Rec, color=h, linetype=linetype)) +
  geom_line(linewidth=1) +
  labs(x='Relative Spawning Biomass', y='Relative Recruitment', color='Steepness (h)') +
  geom_vline(xintercept = 0.2, linetype=2, color='lightgray') +
  theme_bw() +
  guides(linetype='none')
ggsave(file.path(out.dir, 'BH_SRR.png'), width=6, height=4)


TSBio2 <-TSBio %>% filter(steepness!=0.6)

ggplot(TSBio2, aes(year, y=B.Bmsy, color=steepness)) +
  facet_wrap(~M) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw() +
  geom_hline(yintercept = 1, linetype=3) +
  labs(x="Year", y=expression(SB/SB[MSY]),
       color='Steepness (h)')

ggsave(file.path(out.dir, 'Ref_OMs2.png'), width=9, height=3)

# Plot steepness BH
SB0 <- 1
R0 <- 1
SBpR <- SB0/R0
SB <- seq(0, SB0, length.out=1000)
h_vals <- c(0.7, 0.75, 0.9)
out_list <- list()
for (i in seq_along(h_vals)) {
  h <- h_vals[i]
  out_list[[i]] <- data.frame(SB=SB, Rec=BH_SRR(R0, h, SB, SBpR), h=h)
}

df <- do.call('rbind', out_list)
df$h <- factor(df$h)
df$linetype <-  1
df$linetype[df$h==0.6] <- 3
df$linetype <- factor(df$linetype)

ggplot(df, aes(x=SB, y=Rec, color=h, linetype=linetype)) +
  geom_line(linewidth=1) +
  labs(x='Relative Spawning Biomass', y='Relative Recruitment', color='Steepness (h)') +
  geom_vline(xintercept = 0.2, linetype=2, color='lightgray') +
  theme_bw() +
  guides(linetype='none')
ggsave(file.path(out.dir, 'BH_SRR2.png'), width=6, height=4)


## Read in TS for h=0.7 and h=0.82

dirs <- list.dirs(file.path(OM.root, 'grid_2022'), recursive = FALSE)
dirs <- dirs[grepl('00', dirs)]
dirs <- dirs[!grepl('000', dirs)]

replistList <- list()
for (i in seq_along(dirs)) {
  replistList[[i]] <- r4ss::SS_output(dirs[i])
}

get_M_h <- function(df, ..) {
  dd <- df$dir %>% unique()
  txt <- strsplit(dd, '_M')[[1]][2]
  M <- strsplit(txt, '_sigmaR')[[1]][1]
  h <- strsplit(txt, '_steepness')[[1]][2]
  h <- strsplit(h, '_cpue')[[1]][1]

  df$M <- as.numeric(M)
  df$steepness <- as.numeric(h)
  df

}

TSbio_datlist <- list()
for (i in seq_along(dirs)) {
  replist <- replistList[[i]]

  TSbio_dat <- replist$recruit %>%
    dplyr::select(year=Yr, SSB=SpawnBio, Exp_Rec=exp_recr, Exp_Rec_biasadj=bias_adjusted,
                  Obs_Rec=pred_recr, dev=dev)
  TSbio_dat$SB0 <-replist$SBzero
  R0 <- replist$timeseries %>% filter(Era=="VIRG") %>% select(R0=Recruit_0)
  TSbio_dat$R0 <- R0$R0
  TSbio_dat$Depletion <- TSbio_dat$SSB/TSbio_dat$SB0
  TSbio_dat$SS_Depletion <- replist$current_depletion

  derived_quants <- replist$derived_quants
  FMSY <- derived_quants$Value[derived_quants$Label=='annF_MSY']
  Kobe <- replist$Kobe %>% dplyr::rename(year=Yr)
  TSbio_dat <- dplyr::full_join(TSbio_dat, Kobe,by="year")
  TSbio_dat$F <- TSbio_dat$F.Fmsy*FMSY
  TSbio_dat$dir <- dirs[i]
  TSbio_dat <- get_M_h(TSbio_dat)
  TSbio_datlist[[i]] <- TSbio_dat
}

TSbio_dat <- do.call('rbind', TSbio_datlist)

TSBio$M <- as.numeric(as.character(TSBio$M))
TSBio$steepness <- as.numeric(as.character(TSBio$steepness))

TSBio3 <- bind_rows(TSBio, TSbio_dat)
TSBio3$steepness <- factor(TSBio3$steepness)
TSBio3$M <- factor(TSBio3$M)

TSBio3 <- TSBio3 %>% filter(steepness %in% c(0.7, 0.75, 0.9))
ggplot(TSBio3, aes(year, y=B.Bmsy, color=steepness)) +
  facet_wrap(~M) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw() +
  geom_hline(yintercept = 1, linetype=3) +
  labs(x="Year", y=expression(SB/SB[MSY]),
       color='Steepness (h)')

ggsave(file.path(out.dir, 'Ref_OMs_h0.7.png'), width=9, height=3)



SB0 <- 1
R0 <- 1
SBpR <- SB0/R0
SB <- seq(0, SB0, length.out=1000)
h_vals <- c(0.7, 0.9)
out_list <- list()
for (i in seq_along(h_vals)) {
  h <- h_vals[i]
  out_list[[i]] <- data.frame(SB=SB, Rec=BH_SRR(R0, h, SB, SBpR), h=h)
}

df <- do.call('rbind', out_list)
df$h <- factor(df$h)
df$linetype <-  1
df$linetype[df$h==0.6] <- 3
df$linetype <- factor(df$linetype)

ggplot(df, aes(x=SB, y=Rec, color=h, linetype=linetype)) +
  geom_line(linewidth=1) +
  labs(x='Relative Spawning Biomass', y='Relative Recruitment', color='Steepness (h)') +
  geom_vline(xintercept = 0.2, linetype=2, color='black') +
  theme_bw() +
  guides(linetype='none')
ggsave(file.path(out.dir, 'BH_SRR3.png'), width=6, height=4)


steepness <- seq(0.2, 0.98, by=0.001)
CR <- h2CR(steepness)

df <- data.frame(steepness, CR)

ggplot(df, aes(x=steepness, y=CR)) +
  geom_line() +
  theme_bw() +
  labs(x='Steepness (h)', y='Compensation Ratio (CR)')

ggsave(file.path(out.dir, 'h_CR1.png'), width=4, height=4)


CRs <- h2CR(c(0.7, 0.82))
CRs[3] <- (CRs[2] * CRs[2]/CRs[1])
hs <- CR2h(CRs)
hs

df2 <- data.frame(steepness=hs, CR=CRs)

ggplot(df, aes(x=steepness, y=CR)) +
  geom_line() +
  theme_bw() +
  geom_point(data=df2 %>% filter(steepness!=0.82), size=4) +
  geom_hline(data=df2 %>% filter(steepness!=0.82), aes(yintercept=CR), linetype=2) +
  labs(x='Steepness (h)', y='Compensation Ratio (CR)')

ggsave(file.path(out.dir, 'h_CR2.png'), width=4, height=4)

ggplot(df, aes(x=steepness, y=CR)) +
  geom_line() +
  theme_bw() +
  geom_point(data=df2, size=4) +
  geom_hline(data=df2, aes(yintercept=CR), linetype=2) +
  labs(x='Steepness (h)', y='Compensation Ratio (CR)')

ggsave(file.path(out.dir, 'h_CR3.png'), width=4, height=4)




TSBio3 <- bind_rows(TSBio, TSbio_dat)
TSBio3$steepness <- factor(TSBio3$steepness)
TSBio3$M <- factor(TSBio3$M)

TSBio3 <- TSBio3 %>% filter(steepness %in% c(0.7, 0.82, 0.9))
ggplot(TSBio3, aes(year, y=B.Bmsy, color=steepness)) +
  facet_wrap(~M) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw() +
  geom_hline(yintercept = 1, linetype=3) +
  labs(x="Year", y=expression(SB/SB[MSY]),
       color='Steepness (h)')

ggsave(file.path(out.dir, 'Ref_OMs_h0.7_0.82.png'), width=9, height=3)



