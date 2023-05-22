library(SWOMSE)

out.dir <- 'Analyses/MSE'

MPs <- avail('MP', 'SWOMSE')

Ref_OMs <- OM_DF %>% filter(Class=='Reference')

# Load hist objects
histlist <- list()
for (i in seq_along(Ref_OMs$OM.object)) {
  histlist[[i]] <- readRDS(paste0('dev/', Ref_OMs$OM.object[i], '.mhist'))
}


# MSE
rerun <- FALSE
if (rerun) {
  mselist <- list()

  for (i in seq_along(Ref_OMs$OM.object)) {
    mse <- ProjectMOM(histlist[[i]], MPs=MPs)
    nm <- Ref_OMs$OM.object[i]
    saveRDS(mse, paste0('Analyses/MSE/', nm, '.mmse'))
    mselist[[i]] <- mse
  }

  saveRDS(mselist, 'Analyses/MSE/MSElist.mmse')

} else {
  mselist <- readRDS('Analyses/MSE/MSElist.mmse')
}

MSE_all <- combine_MMSE(mselist, 'Reference OMs')


PM_vals <- PM_table(MSE_all)

PMs <- c('PGK_med', 'AvTAC30')

p <- TradeOff(PM_vals, c('PGK_med', 'PGK_long'), vline=0.6, hline=0.6)
ggsave(file.path(out.dir, 'TradeOff1.png'), p, width=6, height=5)

p <- TradeOff(PM_vals, c('PGK_med', 'PGK_long'), vline=0.6, hline=0.6,
              ylim=0.51, xlim = 0.51)
ggsave(file.path(out.dir, 'TradeOff1a.png'), p, width=6, height=5)


p <- TradeOff(PM_vals, c('PGK_med', 'PGK_30'), vline=0.6, hline=0.6,
              ylim=0.51, xlim = 0.51)
p
ggsave(file.path(out.dir, 'TradeOff2.png'), p, width=6, height=5)

p <- TradeOff(PM_vals, c('PGK_med', 'nLRP_long'), xlim=0.51, ylim=0.85)
ggsave(file.path(out.dir, 'TradeOff2a.png'), p, width=6, height=5)


p <- TradeOff(PM_vals, c('AvTAC10', 'AvTAC30'))
ggsave(file.path(out.dir, 'TradeOff3.png'), p, width=6, height=5)

tab <- PM_vals %>% filter(Name %in% c('PGK_med', 'PGK_long', 'PGK_30',
                               'nLRP_long', 'AvTAC10', 'AvTAC30')) %>%
  select(Name, MP, Value) %>%
  mutate(Value=round(Value,2)) %>%
  tidyr::pivot_wider(., names_from = Name, values_from=Value)

library(DT)
DT::datatable(tab)

DT::datatable(tab) %>% formatStyle(
  'PGK_med',
  backgroundColor = styleInterval(c(0.51), c('red', 'green')))%>%
  formatStyle(
  'PGK_long',
  backgroundColor = styleInterval(c(0.51), c('red', 'green'))) %>%
  formatStyle(
    'PGK_30',
    backgroundColor = styleInterval(c(0.51), c('red', 'green'))) %>%
  formatStyle(
    'nLRP_long',
    backgroundColor = styleInterval(c(0.85), c('red', 'green')))



p <- SB_SBMSY_TS(MSE_all, fill='none', mp=c('SP1_a', 'SP2_a', 'CE_a'))
ggsave(file.path(out.dir, 'SB_SBMSY1.png'), p, width=8, height=3)

p <- F_FMSY_TS(MSE_all, fill='none', mp=c('SP1_a', 'SP2_a', 'CE_a'))
ggsave(file.path(out.dir, 'F_FMSY1.png'), p, width=8, height=3)


p <- SB_SBMSY_Box(MSE_all, mp=c('SP1_a', 'SP2_a', 'CE_a'))
ggsave(file.path(out.dir, 'SB_SBMSY_box.png'), p, width=8, height=3)

p <- F_FMSY_Box(MSE_all, mp=c('SP1_a', 'SP2_a', 'CE_a'))
ggsave(file.path(out.dir, 'F_FMSY_box.png'), p, width=8, height=3)




for (i in seq_along(Ref_OMs$OM.object)) {
  OM <- Ref_OMs$OM.object[i]
  p <- SB_SBMSY_TS(mselist[[i]], fill='none', mp=c('SP1_a'))
  nm <- paste0(OM, '_SB_SBMSY1.png')
  ggsave(file.path(out.dir, nm), p, width=4, height=3)
}

for (i in seq_along(Ref_OMs$OM.object)) {
  OM <- Ref_OMs$OM.object[i]
  p <- SB_SBMSY_TS(mselist[[i]], fill='none', mp=c('CE_a'))
  nm <- paste0(OM, '_SB_SBMSY2.png')
  ggsave(file.path(out.dir, nm), p, width=4, height=3)
}

p <- SB_SBMSY_Box(MSE_all, mp=c('SP1_a', 'SP2_a', 'CE_a'))
ggsave(file.path(out.dir, 'SB_SBMSY_box.png'), p, width=8, height=4)

p <- SB_SBMSY_Box(mselist[[i]], mp=c('SP1_a', 'SP2_a', 'CE_a'))
ggsave(file.path(out.dir, 'SB_SBMSY_box.png'), p, width=8, height=4)



## Why is h=0.9 more risky than h=0.75?

MMSE <- mselist[[5]]
SB_SBMSY <- MMSE@SB_SBMSY[,1,1,]
ind <- which(SB_SBMSY[,10]<0.4)[1]

MMSE@MPs
Data <- MMSE@PPD[[1]][[1]][[1]]
TAC <- apply(MMSE@TAC[,,1,1,], c(1,3),sum)

Data@Ind[ind,]





plot(Data@Ind[1,], type='l')


MMSE@PPD[[1]][[1]][[1]]@steep



