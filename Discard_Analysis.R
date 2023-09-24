library(SWOMSE)

source_CMPs()

OMs <- OM_DF %>% filter(Class=='Reference')

OM_list <- list()
for (i in 1:nrow(OMs)) {
  fl <- paste0(OMs$OM.object[i], '.hist')

  multiHist <- readRDS(file.path('Hist_Objects', 'Reference', fl))

  MSE <- ProjectMOM(multiHist, MPs=c('CE_b', 'FX4_b', 'MCC5_b', 'MCC7_b', 'SPSSFox_b'))

  Removals <- apply(MSE@Removals, c(1,4,5), sum)
  Landings <- apply(MSE@Catch, c(1,4,5), sum)
  Discards <- Removals - Landings

  years <- seq(2021, by=1, length.out=MSE@proyears)
  dimnames(Discards) <- list(Sim=1:MSE@nsim, MP=MSE@MPs[[1]], Year=years)
  Discards <- as.data.frame.table(Discards, responseName = 'Discards')
  dimnames(Landings) <- list(Sim=1:MSE@nsim, MP=MSE@MPs[[1]], Year=years)
  Landings <- as.data.frame.table(Landings, responseName = 'Landings')

  dfout <- left_join(Discards, Landings, by = join_by(Sim, MP, Year))
  dfout$OM <- OMs$OM.object[i]
  OM_list[[i]] <- dfout
}

DF <- do.call('rbind', OM_list)
saveRDS(DF, 'dev/Discard_Analysis.rda')

DF <- DF %>% tidyr::pivot_longer(., cols=c(Discards, Landings))



ggplot(DF, aes(x=Year, y=value, fill=name)) +
  facet_wrap(~MP, nrow=2) +
  geom_boxplot(outlier.shape = NA) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  expand_limits(y=0) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(fill='', y='Landings/Discards (t)', x='Year')

ggsave('img/Discards_Landings.png', width=12, height=6)


df2 <- DF %>% group_by(Year, MP, name) %>%
  summarise(`2.5th`=quantile(value, 0.025),
            'Median'=quantile(value, 0.5),
            `97.5th`=quantile(value, 0.975))

df2$Year <- as.numeric(as.character(df2$Year))
df3 <- df2 %>% filter(Year>=2024) %>%
  tidyr::pivot_wider(., names_from=name, values_from = c(`2.5th`, Median, `97.5th`))

df3 <- df3 %>% select(Year, MP,
               `2.5th_Landings`, Median_Landings, `97.5th_Landings`,
               `2.5th_Discards`, Median_Discards, `97.5th_Discards`,
               )

write.csv(df3, 'dev/Landings_Discards.csv')
