source('ECP/Functions.R')

load("G:/Shared drives/BM shared/1. Projects/ICCAT NSWO/2025/ECP/NATL_7Fleets.RData")

IndexData <- NATL_7Fleets |> dplyr::mutate(YearC=as.numeric(YearC))

# ---- Plot Nominal Indices ----


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


# ---- Make Indices -----

# Drop 1 Flag sequentially and fit index
Flags <- IndexData$FlagName|> unique()

for (fl in seq_along(Flags)) {
  DropFlag <- Flags[fl]
  Index <- GenerateIndex(IndexData, DropFlag, DropYears=2020:2022)
  saveRDS(Index, file.path("ECP/Indices/DropYears/Dropped1", paste0(DropFlag, '.rda')))
}

# Drop USA and 1 Other Flag sequentially and fit index
MostImpacted <- 'USA'
Flags2 <- Flags[!Flags %in% MostImpacted]
for (fl in seq_along(Flags2)) {
  DropFlag <- Flags[fl]
  Index <- GenerateIndex(IndexData, c(MostImpacted, DropFlag), DropYears=2020:2022)
  saveRDS(Index, file.path("ECP/Indices/DropYears/Dropped2", paste0(DropFlag, '.rda')))
}

# ---- Plot Indices ----

Dropped1_Indices <- MakeIndicesDF('DropYears/Dropped1', 'Dropped 1 Flag')
Dropped2_Indices <- MakeIndicesDF('DropYears/Dropped2', 'USA + Dropped 1 Flag')

df <- dplyr::bind_rows(Dropped1_Indices, Dropped2_Indices) |> dplyr::filter(Year>=2015)

p <- ggplot(df, aes(x=Year)) +
  # geom_rect(aes(xmin=1950, xmax=1998, ymin=0, ymax=Inf), alpha=0.5, fill='lightgray') +
  geom_line(aes(y=Index, color=Run, linetype=Run)) +
  geom_line(aes(y=CombinedIndex), linetype=2) +
  facet_wrap(~DroppedFlag) +
  expand_limits(y=0) +
  theme_bw() +
  labs(y='Combined Index', color='', linetype='')

# https://stackoverflow.com/a/58734961
shift_legend3 <- function(p) {
  pnls <- cowplot::plot_to_gtable(p) |> gtable::gtable_filter("panel") %>%
    with(setNames(grobs, layout$name)) |> purrr::keep(~identical(.x,zeroGrob()))

  if( length(pnls) == 0 ) stop( "No empty facets in the plot" )

  lemon::reposition_legend( p, "center", panel=names(pnls) )
}

p <- shift_legend3(p)
ggsave('ECP/Figures/Indices_Dropped.png', p, width=6, height=4)

# ----- Run MSE  ----

source("CMPs/MPs_ND.R")

RunMSEs('DropYears/Dropped1', TRUE)
RunMSEs('DropYears/Dropped2', TRUE)

# ---- Figures and Tables ----

MSE_Base <- GetMSEObjects('Base')

Dropped1_Indices <- MakeIndicesDF('DropYears/Dropped1')
Dropped2_Indices <- MakeIndicesDF('DropYears/Dropped2')

Dropped1Flags <- Dropped1_Indices$DroppedFlag |> unique()
Dropped2Flags <- Dropped2_Indices$DroppedFlag |> unique()

MSE_Dropped1 <- purrr::map(Dropped1Flags, \(DroppedFlag) GetMSEObjects('DropYears/Dropped1', DroppedFlag))
names(MSE_Dropped1) <- Dropped1Flags

MSE_Dropped2 <- purrr::map(Dropped2Flags, \(DroppedFlag) GetMSEObjects('DropYears/Dropped2', DroppedFlag))
names(MSE_Dropped2) <- Dropped2Flags

Dropped1_DF <- MakePlotDF(MSE_Dropped1, MSE_Base, 'Dropped 1 Flag')
Dropped2_DF <- MakePlotDF(MSE_Dropped2, MSE_Base, 'USA + Dropped 1 Flag')

PlotDF <- dplyr::bind_rows(Dropped1_DF, Dropped2_DF) |>
  tidyr::pivot_longer(cols=c(SB_SBMSY, F_FMSY, Catch))

ggplot(PlotDF, aes(x=Year, y=value, color=Run)) +
  facet_grid(name~Dropped, scale="free_y") +
  geom_line() +
  expand_limits(y=c(0.9, 1.1)) +
  geom_hline(yintercept = 1, linetype=2) +
  theme_bw() +
  labs(color='', y='Mean')

 ggsave('ECP/Figures/TS_plot.png', width=12, height=4)



source('R/PMs.r')
PMs <- c('TAC1' , "PGK_short", 'PGK_med',
         'PBMSY', "PNOF",
         "AvCatch",  "VarC")

BaseCase_PMs <- purrr::map(PMs, \(PM) {
  fun <- get(PM)
  PMValues <- fun(MSE_Base)@Mean
  data.frame(PM=PM,
             Value=PMValues |> unlist()
  )
}) |> dplyr::bind_rows() |>
  dplyr::mutate(PM=factor(PM, levels=unique(PM), ordered=TRUE))


Dropped1_PMs <- purrr::map2(MSE_Dropped1, names(MSE_Dropped1), \(MSE, name) {
  purrr::map(PMs, \(PM) {
    fun <- get(PM)
    PMValues <- fun(MSE)@Mean
    data.frame(PM=PM,
               Value=PMValues |> unlist()
    )
  }) |> dplyr::bind_rows() |>
    dplyr::mutate(Dropped=name,
                  PM=factor(PM, levels=unique(PM), ordered=TRUE))
}) |> dplyr::bind_rows() |> dplyr::mutate(Run='Dropped 1 Flag')

Dropped2_PMs <- purrr::map2(MSE_Dropped2, names(MSE_Dropped2), \(MSE, name) {
  purrr::map(PMs, \(PM) {
    fun <- get(PM)
    PMValues <- fun(MSE)@Mean
    data.frame(PM=PM,
               Value=PMValues |> unlist()
    )
  }) |> dplyr::bind_rows() |>
    dplyr::mutate(Dropped=name,
                  PM=factor(PM, levels=unique(PM), ordered=TRUE))
}) |> dplyr::bind_rows() |> dplyr::mutate(Run='USA + Dropped 1 Flag')


t1 <- BaseCase_PMs |>
  dplyr::mutate(Run='Base', Value=round(Value,2)) |>
  tidyr::pivot_wider(names_from = PM, values_from = Value)

t2 <- Dropped1_PMs |>
  dplyr::mutate(Value=round(Value,2)) |>
  tidyr::pivot_wider(names_from = PM, values_from = Value)

t3 <- Dropped2_PMs |>
  dplyr::mutate(Value=round(Value,2)) |>
  tidyr::pivot_wider(names_from = PM, values_from = Value)

table1 <-  dplyr::bind_rows(t1, t2, t3)
table1$TAC1 <- round(table1$TAC1, 0)

table1 |> dplyr::arrange(Run, PGK_med)

library(flextable)

Table1 <- flextable::flextable(table1) |>
  flextable::save_as_docx(path="ECP/Tables/Table1.docx")
