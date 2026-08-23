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

DF_List <- list()
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

DF |> dplyr::arrange(Model) |> dplyr::select(PM,Period, Model, Ratio) |> print(n=30)

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
