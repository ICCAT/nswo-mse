library(MSEtool)
library(SWOMSE)

if (packageVersion('MSEtool') < '4.0.0')
  stop('Requires MSEtool v4+')


# ---- Process Results ----

MSEFiles <- list.files('MLL Analysis/Objects/MSE/SQ', full.names = TRUE, recursive = TRUE)
MSE_List <- purrr::map(MSEFiles, readRDS)
MSE      <- MSEtool::CombineMSE(MSE_List)

MSEFiles_Lorenzen <- list.files('MLL Analysis/Objects/MSE/Lorenzen', full.names = TRUE, recursive = TRUE)
MSE_List_Lorenzen <- purrr::map(MSEFiles_Lorenzen, readRDS)
MSE_Lorenzen      <- MSEtool::CombineMSE(MSE_List_Lorenzen)

YearList <- list(Short  = 2025:2034,
                 Medium = 2035:2044,
                 Long   = 2045:2054)

DF_List    <- list()

for (i in seq_along(YearList)) {

  DF_List[[i]] <- dplyr::bind_rows(
    PM_Status(MSE, Years = YearList[[i]]) |> Mean() |>
      dplyr::mutate(Ratio = Value/Value[MP=='MCC11'],
                    Period = names(YearList)[i],
                    Model  = 'Base Case',
                    PM     = 'Status') ,


    PM_Yield(MSE, Years = YearList[[i]]) |> Mean() |>
      dplyr::mutate(Ratio = Value/Value[MP=='MCC11'],
                    Period = names(YearList)[i],
                    Model  = 'Base Case',
                    PM     = 'Yield') ,

    PM_Status(MSE_Lorenzen, Years = YearList[[i]]) |> Mean() |>
      dplyr::mutate(Ratio = Value/Value[MP=='MCC11'],
                    Period = names(YearList)[i],
                    Model  = 'Lorenzen',
                    PM     = 'Status') ,

    PM_Yield(MSE_Lorenzen, Years = YearList[[i]]) |> Mean() |>
      dplyr::mutate(Ratio = Value/Value[MP=='MCC11'],
                    Period = names(YearList)[i],
                    Model  = 'Lorenzen',
                    PM     = 'Yield')
  )

}


DF <- dplyr::bind_rows(DF_List)

MSEtool::Save(DF, 'MLL Analysis/DF.rds')


DF |> dplyr::filter(Model == 'Base Case') |> dplyr::arrange(MP)

DF |> dplyr::arrange(Model) |>
  dplyr::filter(MP == 'MCC11_FR') |>
  dplyr::select(PM,Period, Model, Ratio) |> print(n=30)

p1 <- PlotBiomass(MSE,
             Years = 2025:2054,
             IncHist = FALSE,
             probs = FALSE,
             Stock ='Female')

p2 <- PlotLandings(MSE,
             Years = 2025:2054,
             IncHist = FALSE,
             probs = FALSE,
             byStock ='sum',
             byFleet = FALSE)

p3 <- PlotBiomass(MSE_Lorenzen,
             Years = 2025:2054,
             IncHist = FALSE,
             probs = FALSE,
             Stock ='Female') +
  ggplot2::labs(y='Biomass (t)')

p4 <- PlotLandings(MSE_Lorenzen,
             Years = 2025:2054,
             IncHist = FALSE,
             probs = FALSE,
             byStock ='sum',
             byFleet = FALSE) +
  ggplot2::labs(y='Landings (t)')

patchwork::wrap_plots(p1, p2, p3, p4, ncol = 2,
                      guides = 'collect') +
  patchwork::plot_annotation(tag_levels = 'a',
                             tag_suffix = ')')

ggsave('MLL Analysis/RemoveMLL.png', width = 8, height = 6)

# ---- Relative Figs ----
OM_number <- data.frame(Sim = 1:900, OM_num = rep(1:9, each = 100))

SB <- SBiomass(MSE) |>
  dplyr::filter(Year >= 2025, Stock == 'Female') |>
  dplyr::left_join(OM_number, by = 'Sim') |>
  tidyr::pivot_wider(names_from = MP, values_from = Value) |>
  dplyr::mutate(Rel = MCC11_FR/MCC11, Variable = 'SBiomass') |>
  dplyr::group_by(Year, Variable) |>
  dplyr::summarise(Mean = median(Rel),
                   Lower = quantile(Rel, 0.05),
                   Upper = quantile(Rel, 0.95))

Landings <- Landings(MSE, byFleet = FALSE) |>
  dplyr::filter(Year >= 2025) |>
  dplyr::group_by(Sim, Year, MP) |>
  dplyr::summarise(Value = sum(Value)) |>
  tidyr::pivot_wider(names_from = MP, values_from = Value) |>
  dplyr::mutate(Rel = MCC11_FR/MCC11, Variable = 'Landings') |>
  dplyr::group_by(Year, Variable) |>
  dplyr::summarise(Mean = median(Rel),
                   Lower = quantile(Rel, 0.05),
                   Upper = quantile(Rel, 0.95))

Removals <- Removals(MSE, byFleet = FALSE) |>
  dplyr::filter(Year >= 2025) |>
  dplyr::group_by(Sim, Year, MP) |>
  dplyr::summarise(Value = sum(Value)) |>
  tidyr::pivot_wider(names_from = MP, values_from = Value) |>
  dplyr::mutate(Rel = MCC11_FR/MCC11, Variable = 'TAC') |>
  dplyr::group_by(Year, Variable) |>
  dplyr::summarise(Mean = median(Rel),
                   Lower = quantile(Rel, 0.05),
                   Upper = quantile(Rel, 0.95))

df <- dplyr::bind_rows(SB, Removals, Landings)
df$Variable <- factor(df$Variable, unique(df$Variable), ordered = TRUE)

ggplot(df, aes(x=Year)) +
  facet_wrap(~Variable) +
  geom_line(aes(y=Mean)) +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.4) +
  labs(y = 'Relative Change (FR/SQ)') +
  theme_bw()

ggsave('MLL Analysis/RelDiffernceL.png', width = 8, height = 3)
