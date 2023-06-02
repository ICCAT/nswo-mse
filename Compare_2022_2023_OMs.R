base.dir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs'

new <- 'grid_2022/Reference'
old <- 'grid_2022 - Copy/Reference'

dir.list <- list(old, new)
models <- c('2022', '2023')

outList <- list()
for (i in seq_along(models)) {
  dirs <- list.dirs(file.path(base.dir, dir.list[[i]]), recursive = FALSE)

  out <- list()
  for (j in seq_along(dirs)) {
    dir <- dirs[j]
    replist <- r4ss::SS_output(dir)
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

    TSbio_dat$M <- replist$parameters[which(rownames(replist$parameters) == 'NatM_uniform_Fem_GP_1'),3]
    TSbio_dat$h <- replist$parameters[which(rownames(replist$parameters) == 'SR_BH_steep'),3]
    TSbio_dat$Model <- models[i]
    out[[j]] <- TSbio_dat
  }
  outList[[i]] <- do.call('rbind', out)

}

df <- do.call('rbind', outList)


df$h_cat <- 1
df$h_cat[df$Model=='2022'& df$h==0.6] <- 'Low'
df$h_cat[df$Model=='2023'& df$h==0.69] <- 'Low'

df$h_cat[df$Model=='2022'& df$h==0.75] <- 'Center'
df$h_cat[df$Model=='2023'& df$h==0.80] <- 'Center'

df$h_cat[df$Model=='2022'& df$h==0.90] <- 'High'
df$h_cat[df$Model=='2023'& df$h==0.88] <- 'High'

df$h_cat <- factor(df$h_cat, ordered = TRUE, levels=c('Low', 'Center', 'High'))

df$h <- factor(df$h)
ggplot(df, aes(x=year, y=B.Bmsy, color=Model)) +
  facet_grid(h_cat~M) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw() +
  labs(x="Year", y='SB/SBMSY') +
  geom_hline(yintercept = 1, linetype=2)


ggsave('img/compareOM_2022_2023.png')


df %>% filter(M==0.1, h_cat=='High') %>% distinct(Model)




df %>% filter(M==0.3, h_cat=='Center') %>% distinct(h)

df %>% filter(M==0.1, h==0.69, Model==2023, year==2020)
df %>% filter(M==0.2, h==0.69, Model==2023, year==2020)
df %>% filter(M==0.3, h==0.69, Model==2023, year==2020)

df %>% filter(M==0.1, h==0.80, Model==2023, year==2020)
df %>% filter(M==0.2, h==0.80, Model==2023, year==2020)
df %>% filter(M==0.3, h==0.80, Model==2023, year==2020)

df %>% filter(M==0.1, h==0.88, Model==2023, year==2020)
df %>% filter(M==0.2, h==0.88, Model==2023, year==2020)
df %>% filter(M==0.3, h==0.88, Model==2023, year==2020)


