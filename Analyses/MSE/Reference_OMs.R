library(SWOMSE)

MPs <- avail('MP', 'SWOMSE')

Ref_OMs <- OM_DF %>% filter(Class=='Reference')

# Load hist objects
histlist <- list()
for (i in seq_along(Ref_OMs$OM.object)) {
  histlist[[i]] <- readRDS(paste0('dev/', Ref_OMs$OM.object[i], '.mhist'))
}


# MSE
rerun <- TRUE
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

PM_table <- function(MMSE, PMs=NULL) {
  if (is.null(PMs))
    PMs <- PM_desc$Name

  PMlist <- list()
  for (i in seq_along(PMs)) {
    fun <- try(get(PMs[i]), silent = TRUE)
    if (inherits(fun, 'try-error'))
      stop(PMs[i], ' not a valid function of class `PM`')
    if (class(fun)!='PM')
      stop(PMs[i], ' not a valid function of class `PM`')

    PMlist[[PMs[i]]] <- fun
  }

  PM_Values <- list()
  for (i in seq_along(PMs)) {
    nm <- PMs[i]
    message('Calculating: ', nm)
    MPs <- MMSE@MPs[[1]]
    pm <- PMlist[[i]](MMSE)
    val <- pm@Mean
    name <- pm@Name
    caption <- pm@Caption

    PM_Values[[i]] <- data.frame(PM=nm, MP=MPs, Value=val, name=name,
                                  caption=caption)
  }

  do.call(rbind.data.frame, PM_Values) %>%
    rename(Name=PM)

}

PM_vals <- PM_table(MSE_all)

PMs <- c('PGK_med', 'AvTAC30')

df <- PM_vals %>% filter(Name %in% PMs) %>%
  tidyr::pivot_wider(., names_from=Name, values_from = Value)

## fix this



ggplot(df, aes_string(x=PMs[1], y=PMs[2], color='MP')) +
  geom_point() +
  expand_limits(x=c(0,1),
                y=c(0,1)) +
  ggrepel::geom_label_repel(aes(label=MP)) +
  theme_bw() +
  labs()



PM_vals %>% filter(PM=='PGK_med')
PM_vals %>% filter(PM=='PGK_long')
PM_vals %>% filter(PM=='PGK_30')


Yrs <- MSEgraph::get_Years(MSE_all)
Yrs$i <- NA
ind <- which(Yrs$Year==2024)
Yrs$i[ind:nrow(Yrs)] <- 1:(length(ind:nrow(Yrs)))
Yrs %>% filter(i%in% 6:10)

Kobe(MSE_all, fill='GK', year_range =2029:2033)

Kobe(mselist[[1]], fill='GK')
Kobe(mselist[[2]], fill='GK', year_range =2029:2033)


SB_SBMSY_TS(MSE_all, fill='none')
Catch_TS(MSE_all, fill='none')

# Performance Metric Table





colnames(PM_desc)[4] <- trimws(gsub('\\.', ' ', colnames(PM_desc)[4]))




Kobe(mselist[[1]])
SB_SBMSY_TS(mselist[[1]])

PGK_med(MSE_all)
PGK_long(MSE_all)
tt <- PGK_30(MSE_all)

data.frame(MP=MSE_all@MPs[[1]], PGK_30=tt@Mean)


obj <- readRDS('dev/MP_tuning/Tuning_Objects/IR1_PGK_30_0.6.rda')

obj$df %>% filter(i==max(i)) %>% summarise(mean(TuneVal))
obj$tune_val
obj$i_vals
