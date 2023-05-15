library(SWOMSE)

MPs <- avail('MP','SWOMSE')

df <- strsplit(MPs, '_') %>% do.call('rbind', .) %>% data.frame()
colnames(df) <- c('CMP', 'Tuning')


MP_description <- read.csv('dev/CMP_Description.csv')
MP_description$Tuned <- ''

for (mm in 1:nrow(MP_description)) {
  MP <- MP_description$Code[mm]
  tunedf <- df %>% filter(CMP ==MP)
  MP_description$Tuned[mm] <- paste(tunedf$Tuning, collapse=', ')

}


saveRDS(MP_description, 'dev/MP_description.rda')


DT::datatable(MP_description)
