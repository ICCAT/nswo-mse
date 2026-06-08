devtools::load_all()

library(MSEtool)
library(ggplot2)
library(patchwork)

OM.root <- 'G:/My Drive/1_Projects/North_Atlantic_Swordfish/OMs'
OM.object <- file.path(OM.root, 'OM_objects')
OMgrid.dir <- file.path(OM.root, "2024_OMs")
OMgrid.dirs <- list.dirs(OMgrid.dir, recursive = TRUE)

# Base Case - Model 5
# TODO - show for all M & h combinations?

SSDir          <- OMgrid.dirs[7]
OM_Imported    <- ImportSS(SSDir)

Hist <- Simulate(OM_Imported)

# CompareSS(SSDir, OM_Imported)


OM <- OM_Imported
LoadArgs(Simulate_om)

# ---- Combine into Single Fleet -----
FleetList <- list('Comb. Fleet' = FleetNames(OM_Imported))

OM_CombFleet <- CombineFleets(OM_Imported, FleetList) |> Populate()



# ---- Plot Selectivity & Retention ----

## By Fleet

late_fleet_names <- c("SPN_1",
                      "US_2",
                      "CAN_3",
                      "JPN_LATE_5",
                      "PORT_6",
                      "CHT_LATE_8",
                      "MOR_9",
                      "HRPN_10",
                      "OTH_11")


Sel_Length <- purrr::map(Selectivity(OM_Imported@Fleet), \(stocklist) {
  purrr::map(stocklist, MeanAtLength) |> List2Array()
}) |> List2Array('Stock', pos = 2) |>
  DropDimension('Area') |>
  Subset(Year = max(Years(OM_Imported, 'H')),
         Fleets = late_fleet_names) |>
  ReduceDims(IncYear = TRUE) |>
  Array2DF() |>
  dplyr::mutate(Variable = 'Selectivity',
                Year     = as.factor(Year))

Sel_Age <- purrr::map(Selectivity(OM_Imported@Fleet), \(stocklist) {
  purrr::map(stocklist, MeanAtAge) |> List2Array()
}) |> List2Array('Stock', pos = 2) |>
  DropDimension('Area') |>
  Subset(Year = max(Years(OM_Imported, 'H')),
         Fleets = late_fleet_names) |>
  ReduceDims(IncYear = TRUE) |>
  Array2DF() |>
  dplyr::mutate(Variable = 'Selectivity',
                Year     = as.factor(Year))

Ret_Length <- purrr::map(Retention(OM_Imported@Fleet), \(stocklist) {
  purrr::map(stocklist, MeanAtLength) |> List2Array()
}) |> List2Array('Stock', pos = 2) |>
  DropDimension('Area') |>
  Subset(Year = max(Years(OM_Imported, 'H')),
         Fleets = late_fleet_names) |>
  ReduceDims(IncYear = TRUE) |>
  Array2DF() |>
  dplyr::mutate(Variable = 'Retention',
                Year     = as.factor(Year))

Ret_Age <- purrr::map(Retention(OM_Imported@Fleet), \(stocklist) {
  purrr::map(stocklist, MeanAtAge) |> List2Array()
}) |> List2Array('Stock', pos = 2) |>
  DropDimension('Area') |>
  Subset(Year = max(Years(OM_Imported, 'H')),
         Fleets = late_fleet_names) |>
  ReduceDims(IncYear = TRUE) |>
  Array2DF() |>
  dplyr::mutate(Variable = 'Retention',
                Year     = as.factor(Year))

DM <- purrr::map(DiscardMortality(OM_Imported@Fleet), \(stocklist) {
  purrr::map(stocklist, MeanAtAge) |> List2Array()
}) |> List2Array('Stock', pos = 2) |>
  DropDimension('Area') |>
  Subset(Year = max(Years(OM_Imported, 'H')),
         Fleets = late_fleet_names) |>
  ReduceDims(IncYear = TRUE) |>
  Array2DF() |>
  dplyr::mutate(Variable = 'Discard Mortality',
                Year     = as.factor(Year))

DF_Age    <- dplyr::bind_rows(Sel_Age, Ret_Age)
DF_Length <- dplyr::bind_rows(Sel_Length, Ret_Length)

ggplot(DF_Age, aes(x=Age, y=Value, color=Variable)) +
  facet_grid(Stock ~ Fleet) +
  expand_limits(y = 0) +
  geom_line() +
  theme_bw() +
  labs(x = 'Age (year)', y = 'Probability')

ggsave('../nswo-mse/MLL Analysis/Sel_Ret_Age.png', width = 12, height = 3)

ggplot(DF_Length, aes(x=Class, y=Value, color=Variable)) +
  facet_wrap( ~ Fleet) +
  expand_limits(y = 0) +
  geom_line() +
  theme_bw() +
  labs(x = 'Length (cm)', y = 'Probability')

ggsave('../nswo-mse/MLL Analysis/Sel_Ret_Length.png', width = 8, height = 6)

## Combined

Sel_Comb_Age <- purrr::map(Selectivity(OM_CombFleet@Fleet), \(stocklist) {
  purrr::map(stocklist, MeanAtAge) |> List2Array()
}) |> List2Array('Stock', pos = 2) |>
  DropDimension('Area') |>
  Subset(Year = max(Years(OM_CombFleet, 'H'))) |>
  ReduceDims(IncYear = TRUE) |>
  Array2DF() |>
  dplyr::mutate(Variable = 'Selectivity',
                Year     = as.factor(Year))

Ret_Comb_Age <- purrr::map(Retention(OM_CombFleet@Fleet), \(stocklist) {
  purrr::map(stocklist, MeanAtAge) |> List2Array()
}) |> List2Array('Stock', pos = 2) |>
  DropDimension('Area') |>
  Subset(Year = max(Years(OM_CombFleet, 'H'))) |>
  ReduceDims(IncYear = TRUE) |>
  Array2DF() |>
  dplyr::mutate(Variable = 'Retention',
                Year     = as.factor(Year))

df_comb_age <- dplyr::bind_rows(Sel_Comb_Age, Ret_Comb_Age)


ggplot(df_comb_age, aes(x=Age, y=Value, color=Variable, linetype = Stock)) +
  expand_limits(y = 0) +
  geom_line() +
  theme_bw() +
  labs(x = 'Age (year)', y = 'Probablity')

ggsave('../nswo-mse/MLL Analysis/Sel_Ret_Age_Comp.png', width = 6, height = 4)


# ---- Equilibrium Analysis ----


## ---- Full Retention -----

OM_full_retention <- OM_Imported

RetentionList <- Retention(OM_full_retention)

RetentionList <- purrr::map(RetentionList, \(stockfleet) {
  purrr::map(stockfleet, \(fleet) {
    fleet@MeanAtAge[] <- 1
    fleet
  })
})

Retention(OM_full_retention) <- RetentionList

Eq_base           <- CalcEquilibrium(OM_Imported)
Eq_full_retention <- CalcEquilibrium(OM_full_retention)

MSYRefs_Base           <- CalcMSY(OM_Imported, silent = TRUE)
MSYRefs_full_retention <- CalcMSY(OM_full_retention, silent = TRUE)

df <- data.frame(FMSY = c(FMSY(MSYRefs_Base),
                    FMSY(MSYRefs_full_retention)),
           MSY =  c(MSYLandings(MSYRefs_Base)  |> SumOverStock(),
                    MSYLandings(MSYRefs_full_retention) |> SumOverStock()),
           Model = c('Base', 'Full Retention')
           )

df <- df |> dplyr::mutate(relMSY = MSY/MSY[Model == 'Base'])
df

## ---- Full Retention & Shift in Selectivity ----

delta_vec <- c(1, 0.9, 0.8, 0.7)

shift_selectivity_left <- function(MeanAtLength, classes, delta) {
  dnames <- dimnames(MeanAtLength)

  # Identify peak as the first index where the mean curve (over sims/years/areas)
  # reaches its maximum - use first peak to handle flat-topped curves
  mean_curve <- apply(MeanAtLength, 'Class', mean)
  peak_ind   <- which.max(mean_curve)

  asc_ind     <- seq_len(peak_ind - 1)
  asc_classes <- classes[asc_ind]
  shifted_asc <- asc_classes * delta

  out <- apply(MeanAtLength, c('Sim', 'Year', 'Area'), \(v) {
    new_asc <- approx(shifted_asc, v[asc_ind],
                      xout = asc_classes, rule = 2)$y
    c(new_asc, v[seq(peak_ind, length(v))])
  }) |> aperm(c(2, 1, 3, 4))

  dimnames(out) <- dnames
  out
}


MSY_RefList <- list()
SelectivityDF_List <- list()

for (i in seq_along(delta_vec)) {
  OM <- OM_full_retention
  SelectivityList <- Selectivity(OM)

  SelectivityList <- purrr::map(SelectivityList, \(stockfleet) {
    purrr::map(stockfleet, \(fleet) {
      fleet@MeanAtLength <- shift_selectivity_left(MeanAtLength = fleet@MeanAtLength,
                                                   classes      = fleet@Classes,
                                                   delta        = delta_vec[i]
                                                   )
      fleet@MeanAtAge <- NULL

      fleet
    })
  })

  Selectivity(OM) <- SelectivityList

  MSYRefs_alt_select <- CalcMSY(OM, silent = TRUE)

  MSY_RefList[[i]] <- data.frame(FMSY = FMSY(MSYRefs_alt_select) |> as.numeric(),
                   MSY  = MSYLandings(MSYRefs_alt_select) |> SumOverStock() |> as.numeric(),
                   delta = delta_vec[i]
  )


  SelectivityDF_List <- purrr::imap(SelectivityList, \(stockfleet, i) {
    purrr::imap(stockfleet, \(fleet, j) {
      fleet@MeanAtLength |> Array2DF() |> dplyr::mutate(Stock = i,
                                                        Fleet = j)

    }) |> dplyr::bind_rows()
  }) |> dplyr::bind_rows() |>
    dplyr::select(Stock, Fleet, Class, Value) |>
    dplyr::mutate(delta = delta_vec[i])




}


## ---- Reductions in Discard Mortality ----
DiscMortReduction <- 1 -  seq(0, 1, by = 0.2)
sb_list   <- list()
rem_list  <- list()
land_list <- list()
msy_refs  <- list()

for (i in seq_along(DiscMortReduction)) {

  OM <- OM_Imported

  DiscMortList <- DiscardMortality(OM)
  DiscMortList <- purrr::map(DiscMortList, \(stockfleet) {
    purrr::map(stockfleet, \(fleet) {
      fleet@MeanAtAge[] <- fleet@MeanAtAge[] * (1 - DiscMortReduction[i])
      fleet
    })
  })

  DiscardMortality(OM) <- DiscMortList

  Eq <- CalcEquilibrium(OM)

  sb_list[[i]] <- Array2DF(Eq@SBiomass) |>
    dplyr::filter(Sim == 1, Stock == 'Female') |>
    dplyr::mutate(Variable = 'Sp. Biomass',
                  `Disc. Mort. Reduction` = DiscMortReduction[i])

  land_list[[i]] <- Array2DF(Eq@Landings) |>
  dplyr::filter(Sim == 1) |>
  dplyr::group_by(Year, F) |>
  dplyr::summarise(Value = sum(Value), .groups = 'drop') |>
  dplyr::mutate(Variable = 'Landings',
                `Disc. Mort. Reduction` = DiscMortReduction[i])

  rem_list[[i]] <- Array2DF(Eq@Removals) |>
    dplyr::filter(Sim == 1) |>
    dplyr::group_by(Year, F) |>
    dplyr::summarise(Value = sum(Value), .groups = 'drop') |>
    dplyr::mutate(Variable = 'Removals',
                  `Disc. Mort. Reduction` = DiscMortReduction[i])

  # MSY
  MSYRefs <- CalcMSY(OM, silent = TRUE)
  msy_refs[[i]] <- data.frame(FMSY  = as.numeric(FMSY(MSYRefs)),
                              SBMSY = as.numeric(SPMSY(MSYRefs)[1,1,1]),
                              MSY   = as.numeric(MSYLandings(MSYRefs) |> SumOverStock()),
                              `Disc. Mort. Reduction` = DiscMortReduction[i]
  )

  message(i, '/', length(DiscMortReduction))

}

sb  <- dplyr::bind_rows(sb_list)
sb  <- sb |> dplyr::mutate(Value = Value/Value[F == 0])
sb$`Disc. Mort. Reduction` <- factor(sb$`Disc. Mort. Reduction`)

landings <- dplyr::bind_rows(land_list)
landings$`Disc. Mort. Reduction` <- factor(landings$`Disc. Mort. Reduction`)

removals <- dplyr::bind_rows(rem_list)
removals$`Disc. Mort. Reduction` <- factor(removals$`Disc. Mort. Reduction`)

p1 <- ggplot(sb,
       aes(x=F, y=Value, color = `Disc. Mort. Reduction`)) +
  geom_line() +
  theme_bw() +
  labs(x = 'Fishing Mortality', y = 'SB/SB0') +
  guides(color = 'none')

p2 <- ggplot(landings,
       aes(x=F, y=Value, color = `Disc. Mort. Reduction`)) +
  geom_line() +
  theme_bw() +
  labs(x = 'Fishing Mortality', y = 'Landings') +
  theme()

p1 + p2

ggsave('../nswo-mse/MLL Analysis/F_DiscM.png', width = 10, height = 4)


MSYRef_DF <- dplyr::bind_rows(msy_refs) |> dplyr::mutate(`Disc. Mort. Reduction` = DiscMortReduction)

MSYRef_DF <- MSYRef_DF |> dplyr::mutate(SB_Rel = SBMSY/SBMSY[`Disc. Mort. Reduction` == 0],
                                        MSY_Rel = MSY/MSY[`Disc. Mort. Reduction` == 0])








# Alternative Selectivity Curves










