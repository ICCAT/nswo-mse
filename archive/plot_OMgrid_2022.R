library(ggplot2)
library(dplyr)

OM.root <- 'G:/My Drive/1_Projects/North_Atlantic_Swordfish/OMs'

RepList <- readRDS(paste0(OM.root, '/OM_objects/RepList.rda'))

lapply(RepList, '[[', 'log_det_hessian') %>% unlist()
RepList[[1]]$log_det_hessian

# --- Plot relative SB ---
dirs <- list.dirs(file.path(OM.root, 'grid_2022'),
                  recursive = FALSE, full.names = FALSE)

TSBio_List <- readRDS(file.path(OM.root, 'OM_objects/TSBio_List.rda'))
dflist <- list()
for (i in 1:length(TSBio_List)) {
  if (grepl('base_case', dirs[i])) {
    name <- 'Base Case'
    M <- 0.2
    h <- 0.88
    indices <- 'all'
  } else {
    name <- strsplit(dirs[i], '_')[[1]][1]
    M <- as.numeric(strsplit(strsplit(dirs[i], 'M')[[1]][2], '_')[[1]][1])
    h <- as.numeric(strsplit(strsplit(dirs[i], 'steepness')[[1]][2], '_')[[1]][1])
    indices <- strsplit(strsplit(dirs[i], 'indices')[[1]][2], '_')[[1]][1]
  }
  df <- TSBio_List[[i]] %>% dplyr::select(year, B.Bmsy, F.Fmsy, Depletion )
  df$name <- name
  df$M <- M
  df$h <- h
  df$indices <- indices
  dflist[[i]] <- df
}


DF <- do.call('rbind', dflist)
DF_OMs <- DF %>% filter(name!='Base Case')
DF_BC <- DF %>% filter(name=='Base Case')
M_h <- expand.grid(M=c(0.1,0.2,0.3),h=c(0.6, 0.75, 0.9))
BC_list <- list()
for (i in 1:nrow(M_h)) {
  df <- DF_BC
  df$M <- M_h$M[i]
  df$h <- M_h$h[i]
  BC_list[[i]] <- df
}
DF_BC <- do.call('rbind', BC_list)

# investigate high B.BMSY
DF_OMs %>% filter(B.Bmsy>100, year==2020)

RepList[[4]]$derived_quants %>% dplyr::filter(Label %in% c('SSB_Virgin', 'SSB_MSY'))
RepList[[13]]$derived_quants %>% dplyr::filter(Label %in% c('SSB_Virgin', 'SSB_MSY'))
RepList[[22]]$derived_quants %>% dplyr::filter(Label %in% c('SSB_Virgin', 'SSB_MSY'))




# filter out high B.BMSY
DF_OMs$B.Bmsy[DF_OMs$B.Bmsy>100] <- NA


ggplot(DF_OMs, aes(x=year, y=Depletion)) +
  facet_grid(h~M, scales='free') +
  expand_limits(y=0) +
  geom_line(aes(color=indices, group=name)) +
  geom_line(data=DF_BC, color='black', linetype=2) +
  theme_bw() +
  labs(x='Year', y='SB/SB0', color='Indices')
ggsave('img/2022_small_grid/SB_SB0.png')

ggplot(DF_OMs, aes(x=year, y=B.Bmsy)) +
  facet_grid(h~M, scales='free') +
  expand_limits(y=0) +
  geom_line(aes(color=indices, group=name)) +
  geom_line(data=DF_BC, color='black', linetype=2) +
  geom_hline(yintercept = c(0.5, 1), linetype=2, color='gray') +
  theme_bw() +
  labs(x='Year', y='SB/SBMSY', color='Indices')
ggsave('img/2022_small_grid/SB_SBMSY.png')

ggplot(DF_OMs, aes(x=year, y=F.Fmsy)) +
  facet_grid(h~M, scales='free') +
  expand_limits(y=0) +
  geom_hline(yintercept = 1, linetype=2, color='gray') +
  geom_line(aes(color=indices, group=name)) +
  geom_line(data=DF_BC, color='black', linetype=2) +
  theme_bw() +
  labs(x='Year', y='F/FMSY', color='Indices')
ggsave('img/2022_small_grid/F_FBMSY.png')


RepList[[4]]$derived_quants %>% dplyr::filter(Label %in% c('SSB_Virgin', 'SSB_MSY'))
RepList[[13]]$derived_quants %>% dplyr::filter(Label %in% c('SSB_Virgin', 'SSB_MSY'))
RepList[[22]]$derived_quants %>% dplyr::filter(Label %in% c('SSB_Virgin', 'SSB_MSY'))
