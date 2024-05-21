
hist_dir <- 'Hist_Objects/Reference'
hist_files <- list.files(hist_dir)

years <- 1950:2020

b_list <- list()
for (i in seq_along(hist_files)) {
  hist <- readRDS(file.path(hist_dir, hist_files[i]))

  b <- apply(hist$Female$`Fleet 1`@TSdata$Biomass[1,,] +
               hist$Male$`Fleet 1`@TSdata$Biomass[1,,], 1, sum)
  b <- b/mean(b)

  b_list[[i]] <- data.frame(OM=strsplit(hist_files[i], '.hist')[[1]],
                            Year=years,  Biomass=b)
}

df <- do.call('rbind', b_list)

write.csv(df, 'standardized_biomass.csv')
