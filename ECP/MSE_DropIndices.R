source('ECP/Functions.R')

# ---- Make Indicies -----
load("G:/Shared drives/BM shared/1. Projects/ICCAT NSWO/2025/ECP/NATL_7Fleets.RData")
IndexData <- NATL_7Fleets |> dplyr::mutate(YearC=as.numeric(YearC))

NomCPUE <- IndexData |> dplyr::group_by(YearC, FlagName) |>
  dplyr::summarise(CPUE=mean(CPUE, na.rm=TRUE)) |>
  dplyr::group_by(FlagName) |>
  dplyr::mutate(CPUE=CPUE/mean(CPUE, na.rm=TRUE))

ggplot(NomCPUE, aes(x=YearC, y=CPUE)) +
  facet_wrap(~FlagName) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw() +
  labs(x='Year', y='Nominal CPUE')

ggsave('ECP/Figures/NominalIndices.png', width=6, height=4)

# Drop 1 Flag sequentially and fit index
Flags <- NATL_7Fleets$FlagName|> unique()

for (fl in seq_along(Flags)) {
  DropFlag <- Flags[fl]
  Index <- GenerateIndex(NATL_7Fleets,DropFlag)
  saveRDS(Index, file.path("ECP/Indices/Dropped1", paste0(DropFlag, '.rda')))
}



# Drop Japan and 1 other Flag sequentially and fit index
MostImpacted <- 'Japan'

Flags2 <- Flags[!Flags %in% MostImpacted]

for (fl in seq_along(Flags2)) {
  DropFlag <- Flags2[fl]
  Index <- GenerateIndex(NATL_7Fleets,c(DropFlag, 'Japan'))


  saveRDS(Index, file.path("ECP/Indices/Dropped2", paste0(DropFlag, '.rda')))
}


Index1 <- GenerateIndex(NATL_7Fleets,c(DropFlag, 'Japan'))
Index2 <- GenerateIndex(NATL_7Fleets,c('Japan'))

# ---- Do MSE Projections ----
source("CMPs/MPs_ND.R")

RunMSEs('Dropped1')
RunMSEs('Dropped2')


## Run Base Case
hist_objects <- list.files('Hist_Objects/Reference', full.names = TRUE)
for (i in seq_along(hist_objects)) {
  Hist <- readRDS(hist_objects[i])
  MSE <- ProjectMOM(Hist, 'MCC11_b')

  om_name <- gsub('.hist', '', basename(hist_objects[i]))

  nm <- paste0(om_name, '_Base.rda')
  name <- file.path('ECP/MSE_Objects', nm)
  saveRDS(MSE, name)

}



