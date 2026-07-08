

library(MSEtool)
library(ggplot2)
library(patchwork)

source('MLL Analysis/functions.r')

# ---- Import Operating Model ----

OM.root     <- 'G:/My Drive/1_Projects/North_Atlantic_Swordfish/OMs'
OM.object   <- file.path(OM.root, 'OM_objects')
OMgrid.dir  <- file.path(OM.root, "2024_OMs")
OMgrid.dirs <- list.dirs(OMgrid.dir, recursive = TRUE)

# Base Case - Model 5
OM   <- ImportSS(SSDir = OMgrid.dirs[7])
Hist <- Simulate(OM)


# ---- Combine into Single Fleet -----

FleetList    <- list('Comb. Fleet' = FleetNames(OM))
OM_CombFleet <- CombineFleets(OM, FleetList) |> Populate()
Hist_Comb    <- Simulate(OM_CombFleet)

# ---- Plot Growth and Maturity Schedules ----

Female   <- Stock(OM, 1)
LenAtAge <- Length(Female)    |> MeanAtAge() |>
  Array2DF() |> dplyr::mutate(Variable = 'Length')
MatAtAge <- Maturity(Female)  |> MeanAtAge() |>
  Array2DF() |> dplyr::mutate(Variable = 'Maturity')
FecAtAge <- Fecundity(Female) |> MeanAtAge() |>
  Array2DF() |> dplyr::mutate(Variable = 'Fecundity')

DF_Age <- dplyr::bind_rows(LenAtAge, MatAtAge, FecAtAge)
DF_Age$Variable <- factor(DF_Age$Variable,
                          levels = unique(DF_Age$Variable),
                          ordered = TRUE)


# Populate At Size arrays
Maturity(Female) <- PopulateMaturity(Maturity     = Maturity(Female),
                                     Ages         = Ages(Female),
                                     Length       = Length(Female),
                                     Weight       = Weight(Female),
                                     Years        = Years(Female),
                                     nSim         = nSim(Female),
                                     CalcAtLength = TRUE
                                     )

Fecundity(Female) <- PopulateFecundity(Fecundity  = Fecundity(Female),
                                     Ages         = Ages(Female),
                                     Length       = Length(Female),
                                     Weight       = Weight(Female),
                                     Maturity     = Maturity(Female),
                                     Years        = Years(Female),
                                     nSim         = nSim(Female),
                                     CalcAtLength = TRUE
)



MatAtSize <- Maturity(Female)  |> MeanAtLength() |>
  Array2DF() |> dplyr::mutate(Variable = 'Maturity')
FecAtSize <- Fecundity(Female) |> MeanAtLength() |>
  Array2DF() |> dplyr::mutate(Variable = 'Fecundity')

DF_Size <- dplyr::bind_rows(MatAtSize, FecAtSize)
DF_Size$Variable <- factor(DF_Size$Variable,
                          levels = unique(DF_Size$Variable),
                          ordered = TRUE)

ggplot(DF_Age, aes(x=Age, y=Value)) +
  facet_wrap(~Variable, scales='free_y') +
  geom_line() +
  theme_bw()

ggplot(DF_Size, aes(x=Class, y=Value)) +
  facet_wrap(~Variable, scales='free_y') +
  geom_line() +
  theme_bw()


L50 <- MatAtSize |>
  dplyr::summarise(Length = approx(x = Value, y = Class, xout = 0.5)$y)

F50 <- FecAtSize |> dplyr::mutate(RelValue = Value/max(Value)) |>
  dplyr::summarise(Length = approx(x = RelValue, y = Class, xout = 0.5)$y)

line_df <- data.frame(yintercept = c(119, 125, L50$Length, F50$Length),
                      name = c('119', '125', 'L50', 'Fec50'))

line_df$name <- factor(line_df$name,
                       levels = c('Fec50', 'L50', '125', '119'),
                       ordered = TRUE)

p1 <- ggplot(DF_Age |> dplyr::filter(Variable == 'Length'), aes(x=Age, y=Value)) +
  labs(x = 'Age (year)', y = 'Length (cm)') +
  geom_line(linewidth = 0.9) +
  geom_hline(data=line_df, aes(yintercept=yintercept, color=name, linetype = name)) +
  labs(color='', linetype='') +
  expand_limits(y = 0) +
  scale_x_continuous(expand = expansion(mult = c(0.001, 0.02))) +
  scale_y_continuous(expand = expansion(mult = c(0.001, 0.02))) +
  scale_linetype_manual(values = c('119'=2, '125'=3, 'L50'=4, 'Fec50'=5)) +
  scale_color_brewer(palette = 'Dark2') +
  theme_classic()


p2 <- ggplot(DF_Size |> dplyr::filter(Variable == 'Maturity'), aes(x=Class, y=Value)) +
  labs(x = 'Length (cm)', y = 'Propability Mature') +
  geom_line(linewidth = 0.9) +
  expand_limits(y = 0) +
  scale_x_continuous(expand = expansion(mult = c(0.001, 0.02))) +
  scale_y_continuous(expand = expansion(mult = c(0.001, 0.02))) +
  theme_classic()

p3 <- ggplot(DF_Size |> dplyr::filter(Variable == 'Fecundity'), aes(x=Class, y=Value)) +
  labs(x = 'Length (cm)', y = 'Fecundity') +
  geom_line(linewidth = 0.9) +
  expand_limits(y = 0) +
  scale_x_continuous(expand = expansion(mult = c(0.001, 0.02))) +
  scale_y_continuous(expand = expansion(mult = c(0.001, 0.02))) +
  theme_classic()

pout <- patchwork::wrap_plots(p2, p3, p1) +
  patchwork:: plot_annotation(tag_levels = 'a',
                              tag_suffix = ')')

pout

ggsave('MLL Analysis/Figures/Growth_Maturity.png', pout, width = 9,
       height = 3)


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


Sel_Length <- ExtractFleetQuantity(OM, 'Selectivity',      'length', fleets = late_fleet_names)
Sel_Age    <- ExtractFleetQuantity(OM, 'Selectivity',      'age',    fleets = late_fleet_names)
Ret_Length <- ExtractFleetQuantity(OM, 'Retention',        'length', fleets = late_fleet_names)
Ret_Age    <- ExtractFleetQuantity(OM, 'Retention',        'age',    fleets = late_fleet_names)
DM         <- ExtractFleetQuantity(OM, 'DiscardMortality', 'age',    fleets = late_fleet_names)


DF_Age          <- dplyr::bind_rows(Sel_Age, Ret_Age)
DF_Age$Variable <- factor(DF_Age$Variable, levels=unique(DF_Age$Variable), ordered = TRUE)

ggplot(DF_Age |> dplyr::filter(Stock =='Female'), aes(x=Age, y=Value, color=Variable)) +
  facet_wrap(~ Fleet) +
  expand_limits(y = 0) +
  geom_line() +
  theme_bw() +
  scale_color_brewer(palette ='Dark2') +
  labs(x = 'Age (year)', y = 'Probability', color='')

ggsave('MLL Analysis/Figures/Sel_Ret_Age.png', width = 8,  height = 6)

DF_Length <- dplyr::bind_rows(Sel_Length, Ret_Length)
DF_Length$Variable <- factor(DF_Length$Variable, levels=unique(DF_Length$Variable), ordered = TRUE)

ggplot(DF_Length, aes(x=Class, y=Value, color = Variable, linetype = Variable)) +
  facet_wrap( ~ Fleet) +
  expand_limits(y = 0) +
  geom_line() +
  theme_bw() +
  scale_color_brewer(palette ='Dark2') +
  labs(x = 'Length (cm)', y = 'Probability', color='', linetype='')

ggsave('MLL Analysis/Figures/Sel_Ret_Length.png', width = 8,  height = 6)


## ---- Combined At Length ----
SelFleet <- purrr::imap(Hist@OM@Fleet$Female, \(fl, i) data.frame(Class=fl@Selectivity@Classes,
                                                             Fleet =i,
                                                             Value=fl@Selectivity@MeanAtLength[1,,73,1])) |>
  dplyr::bind_rows() |>
  dplyr::filter(Fleet %in% late_fleet_names)

Fs <- Hist@FDead[1,1,73,match(late_fleet_names, FleetNames(OM))]

F_df <- data.frame(Fleet = late_fleet_names, F = as.numeric(Fs))

SelOverall <- SelFleet |>
  dplyr::left_join(F_df, by = "Fleet") |>
  dplyr::group_by(Class) |>
  dplyr::summarise(Sel = sum(F * Value) / sum(F), .groups = "drop") |>
  dplyr::mutate(Value = Sel / max(Sel)) |>
  dplyr::mutate(Variable = 'Selectivity')


RetFleet <- purrr::imap(Hist@OM@Fleet$Female, \(fl, i) data.frame(Class=fl@Retention@Classes,
                                                                  Fleet =i,
                                                                  Value=fl@Retention@MeanAtLength[1,,73,1])) |>
  dplyr::bind_rows() |>
  dplyr::filter(Fleet %in% late_fleet_names) |>
  dplyr::rename(Ret = Value)

SelFleet2 <- SelFleet |> dplyr::rename(Sel = Value)

RetOverall <- SelFleet2 |>
  dplyr::inner_join(RetFleet, by = c("Class", "Fleet")) |>
  dplyr::left_join(F_df, by = "Fleet") |>
  dplyr::mutate(w = F * Sel) |>
  dplyr::group_by(Class) |>
  dplyr::summarise(Value = sum(w * Ret) / sum(w),  TotalW = sum(w), .groups = "drop") |>
  dplyr::mutate(Value = dplyr::if_else(TotalW < max(TotalW) * 1e-4, NA_real_, Value)) |>
  dplyr::mutate(Variable = 'Retention')

df_comb_size <- dplyr::bind_rows(SelOverall, RetOverall, MatAtSize)

df_comb_size$Variable <- factor(df_comb_size$Variable,
                                levels=unique(df_comb_size$Variable),
                                ordered = TRUE)
ggplot(df_comb_size ) +
  expand_limits(y = 0, x = 0) +
  geom_line(aes(x=Class, y=Value, color=Variable, linetype = Variable)) +
  theme_bw() +
  scale_x_continuous(expand = expansion(mult = c(0.001, 0.02))) +
  scale_y_continuous(expand = expansion(mult = c(0.001, 0.02))) +
  scale_color_brewer(palette ='Dark2') +
  labs(x = 'Length (cm)', y = 'Probablity', color = '', linetype = '')

ggsave('MLL Analysis/Figures/Sel_Ret_Mat_Size.png', width = 6,  height = 4)





# ---- Landings & Discards by Fleet ----

L <- Hist@Landings[1,,71:73,match(late_fleet_names, FleetNames(OM))] |> SumOverStock() |> SumOverYear()
D <- Hist@Discards[1,,71:73,match(late_fleet_names, FleetNames(OM))] |> SumOverStock() |> SumOverYear()

sort(D) |> rev()
sort(L) |> rev()
(D/(L+D) * 100) |> sort() |> rev()


sum(D)/(sum(L+D)) * 100

Landings <- LandingsAtSize(Hist) |>
  purrr::imap(\(stock, stock_name)
              stock |>
                purrr::imap(\(fl, fleet_name)
                            Array2DF(fl) |>
                              dplyr::mutate(Stock = stock_name, Fleet = fleet_name, .before = 1)
                ) |>
                purrr::list_rbind()
  ) |>
  purrr::list_rbind() |>
  dplyr::filter(Year %in% c(2020, 2021, 2022)) |>
  dplyr::mutate(Variable = 'Landings')

Discards <- DiscardsAtSize(Hist) |>
  purrr::imap(\(stock, stock_name)
              stock |>
                purrr::imap(\(fl, fleet_name)
                            Array2DF(fl) |>
                              dplyr::mutate(Stock = stock_name, Fleet = fleet_name, .before = 1)
                ) |>
                purrr::list_rbind()
  ) |>
  purrr::list_rbind() |>
  dplyr::filter(Year %in% c(2020, 2021, 2022)) |>
  dplyr::mutate(Variable = 'Discards', .groups = 'drop')

DF <- dplyr::bind_rows(Landings, Discards) |>
  dplyr::group_by(Fleet) |>
  dplyr::filter(Value > 0) |>
  dplyr::group_by(Class, Fleet, Variable) |>
  dplyr::summarise(Value = sum(Value))

DF$Fleet <- factor(DF$Fleet, ordered = TRUE, levels = unique(Landings$Fleet))

ggplot(DF, aes(x=Class, y=Value, fill=Variable)) +
  facet_wrap(~Fleet) +
  geom_bar(stat = 'identity', linewidth = 0) +
  labs(fill = '', x = 'Length (cm)', y = 'Number (1000 fish)') +
  theme_bw()

ggsave('MLL Analysis/Figures/Landing_Discards.png', width = 9,
       height = 6)


# ---- Equilibrium Analysis ----

## ---- Retention and Discard Mortality Scenarios ----

# Retention-at-Size Scenarios
# 1. No size limit - full retention
# 2. Current size limit
# 3. Increase - 140
# 4. Increase - 160

# Discard Mortality Assumptions:
# 1. 0, 0.5, 1 times current levels

RetScens  <- c(0, NA, 140, 160)
DiscScens <- c(0, 0.33, 0.66, 1)

grid <- expand.grid(Retention=RetScens, DiscMort=DiscScens)


names <- paste(grid$Retention, grid$DiscMort, sep='-')
OM_List <- MakeNamedList(names)

for (i in seq_len(nrow(grid))) {
  om <- OM

  ret <- grid$Retention[i]
  RetentionList <- Retention(om)
  if (!is.na(ret)) {
    RetentionList <- purrr::map(RetentionList, \(stockfleet) {
      purrr::map(stockfleet, \(fleet) {
        fleet@Pars <- list(RL = ret)
        fleet@MeanAtAge <- NULL
        fleet@MeanAtLength <- NULL
        fleet
      })
    })
  }
  Retention(om) <- RetentionList

  dm  <- grid$DiscMort[i]
  DiscMortList <- DiscardMortality(om)
  DiscMortList <- purrr::map(DiscMortList, \(stockfleet) {
    purrr::map(stockfleet, \(fleet) {
      fleet@MeanAtAge[] <- fleet@MeanAtAge[] *  dm
      fleet
    })
  })

  DiscardMortality(om) <- DiscMortList
  OM_List[[i]] <- om
}


Eq_List <- purrr::map(OM_List, \(om) CalcEquilibrium(om))

DF_List <- MakeNamedList(names)
for (i in seq_along(Eq_List)) {

  dm  <- grid$DiscMort[i]
  ret <- grid$Retention[i]
  if (is.na(ret)) {
    ret <- 'Current Retention'
  } else if (ret == 0) {
    ret <- 'Full Retention'
  } else {
    ret <- paste(ret, 'cm')
  }

  DF_List[[i]] <- data.frame(Ret      = as.character(ret),
                             DM       = as.character(dm),
                             F        = Eq_List[[i]]@apicalF,
                             Landings = as.vector(Eq_List[[i]]@Landings |> SumOverStock()),
                             SBiomass = Eq_List[[i]]@SBiomass[1,1,1,]
             )



}

DF <- dplyr::bind_rows(DF_List)
DF$Ret <- factor(DF$Ret, levels=c("Current Retention", "Full Retention", "140 cm","160 cm" ), ordered = TRUE)

ggplot(DF, aes(x = F, y = Landings, color = Ret, linetype = Ret)) +
  facet_wrap(~DM) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(expand = expansion(mult = c(0.001, 0.02))) +
  scale_y_continuous(expand = expansion(mult = c(0.001, 0.02))) +
  scale_color_brewer(palette = 'Dark2') +
  labs(x= 'Fishing mortality',
       y = 'Landings (ton)',
       color = '', linetype = '')

ggsave('MLL Analysis/Figures/YieldCurve.png', width = 9, height = 6)


ggplot(DF, aes(x = F, y = SBiomass/max(SBiomass), color = Ret, linetype = Ret)) +
  facet_wrap(~DM) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(expand = expansion(mult = c(0.001, 0.02))) +
  scale_y_continuous(expand = expansion(mult = c(0.001, 0.02))) +
  scale_color_brewer(palette = 'Dark2') +
  labs(x= 'Fishing mortality', y = bquote(SB/SB[0]),
         color = '', linetype = '')

ggsave('MLL Analysis/Figures/SBCurve.png', width = 9, height = 6)


# Make Word Table
library(flextable)
library(officer)

tab_df <- DF |>
  dplyr::group_by(Ret, DM) |>
  dplyr::filter(Landings == max(Landings)) |>
  dplyr::ungroup() |>
  dplyr::arrange(DM, Ret) |>
  dplyr::select(DM, Ret, F, Landings, SBiomass)


ft <- flextable(tab_df) |>
  set_header_labels(
    DM       = "Relative Discard\nMortality",
    Ret      = "Retention Scenario",
    F        = "F",
    Landings = "MSY Landings (t)",
    SBiomass = "SB_MSY (t)"   # placeholder, overridden below
  ) |>
  compose(
    part = "header", j = "SBiomass",
    value = as_paragraph("SB", as_sub("MSY"))
  ) |>
  merge_v(j = "DM") |>                                   # merge repeated DM values vertically
  colformat_double(j = "F", digits = 3) |>
  colformat_double(j = c("Landings", "SBiomass"), digits = 0, big.mark = ",") |>
  align(align = "center", part = "all") |>
  align(j = "Ret", align = "left", part = "body") |>
  bg(part = "header", bg = "lightgray") |>
  color(part = "header", color = "black") |>
  bold(part = "header") |>
  fontsize(size = 10, part = "all") |>
  border_outer(border = fp_border(color = "#BFBFBF", width = 1)) |>
  border_inner(border = fp_border(color = "#BFBFBF", width = 1)) |>
  valign(valign = "center", part = "all") |>
  autofit() |>
  fix_border_issues()

grp <- match(tab_df$DM, unique(tab_df$DM))
shaded_rows <- which(grp %% 2 == 1)
ft <- bg(ft, i = shaded_rows, bg = "#F2F2F2", part = "body")

top_rows <- tab_df |>
  dplyr::mutate(row_id = dplyr::row_number()) |>
  dplyr::group_by(DM) |>
  dplyr::filter(Landings == max(Landings)) |>
  dplyr::pull(row_id)

ft <- ft |>
  bg(i = top_rows, bg = "darkgray", part = "body") |>   # highlight color
  bold(i = top_rows, part = "body")

ft

doc <- read_docx() |>
  body_add_flextable(ft)

print(doc, target = "MLL Analysis/Retention_DM_Scenarios.docx")



## ---- Shifted Selectivity to Left ----

delta_select_vec <- c(1, 0.7, 0.5)
ret_vec <- c(0, NA)

grid <- expand.grid(Retention=ret_vec, Delta=delta_select_vec)

SelectivityDF_List <- list()
OM_ShiftList <- list()

for (i in seq_len(nrow(grid))) {
  OM_Shift <- OM
  SelectivityList <- Selectivity(OM_Shift)

  if (grid$Delta[i] != 1) {
  SelectivityList <- purrr::map(SelectivityList, \(stockfleet) {
    purrr::map(stockfleet, \(fleet) {
      fleet@MeanAtLength <- shift_selectivity_left(MeanAtLength = fleet@MeanAtLength,
                                                   classes      = fleet@Classes,
                                                   delta        = grid$Delta[i]
      )
      fleet@MeanAtAge <- NULL

      fleet
    })
  })
  }

  Selectivity(OM_Shift) <- SelectivityList
  OM_Shift <- Populate(OM_Shift, silent = TRUE)


  SelectivityDF_List[[i]] <- ExtractFleetQuantity(OM_Shift,
                                                  quantity = 'Selectivity',
                                                  by = 'length',
                                                  fleets = late_fleet_names) |>
    dplyr::mutate(delta = grid$Delta[i])


  ret <- grid$Retention[i]
  RetentionList <- Retention(OM_Shift)
  if (!is.na(ret)) {
    RetentionList <- purrr::map(RetentionList, \(stockfleet) {
      purrr::map(stockfleet, \(fleet) {
        fleet@Pars <- list(RL = ret)
        fleet@MeanAtAge <- NULL
        fleet@MeanAtLength <- NULL
        fleet
      })
    })
  }
  Retention(OM_Shift) <- RetentionList
  OM_ShiftList[[i]] <- OM_Shift

}

SelectivityDF <- dplyr::bind_rows(SelectivityDF_List) |>
  dplyr::mutate(Delta = as.factor(delta))

ggplot(SelectivityDF, aes(x=Class, y=Value, color=Delta)) +
  facet_wrap( ~ Fleet) +
  expand_limits(y = 0) +
  geom_line() +
  theme_bw() +
  labs(x = 'Length (cm)', y = 'Probability')

ggsave('MLL Analysis/Figures/Shifting_selectivity.png', width = 9, height = 6)


Eq_List_Shift <- purrr::map(OM_ShiftList, \(om) CalcEquilibrium(om))

DF_List_Shift <- list()
for (i in seq_along(Eq_List_Shift)) {

  ret <- grid$Retention[i]
  if (is.na(ret)) {
    ret <- 'Current Retention'
  } else {
    ret <- 'Full Retention'
  }

  DF_List_Shift[[i]] <- data.frame(Delta = grid$Delta[i],
                                   Ret   = ret,
                                   F        = Eq_List_Shift[[i]]@apicalF,
                                   Landings = as.vector(Eq_List_Shift[[i]]@Landings |> SumOverStock()),
                                   SBiomass = Eq_List_Shift[[i]]@SBiomass[1,1,1,]
  )


}

DF_Shift <- dplyr::bind_rows(DF_List_Shift)
DF_Shift$Ret <- factor(DF_Shift$Ret, levels = unique(DF_Shift$Ret), ordered = TRUE)
DF_Shift$Delta <- factor(DF_Shift$Delta)

tab_df_shift <- DF_Shift |>
  dplyr::group_by(Ret, Delta) |>
  dplyr::filter(Landings == max(Landings)) |>
  dplyr::ungroup() |>
  dplyr::arrange(Delta) |>
  dplyr::select(Delta, Ret, F, Landings, SBiomass)


tab_df_shift

ft_shift <- flextable(tab_df_shift) |>
  set_header_labels(
    Delta    = "Relative Selectivity Shift",
    Ret      = "Retention Scenario",
    F        = "F",
    Landings = "MSY Landings (t)",
    SBiomass = "SB_MSY (t)"
  ) |>
  compose(
    part = "header", j = "SBiomass",
    value = as_paragraph("SB", as_sub("MSY"))
  ) |>
  merge_v(j = "Delta") |>
  colformat_double(j = "F", digits = 3) |>
  colformat_double(j = c("Landings", "SBiomass"), digits = 0, big.mark = ",") |>
  align(align = "center", part = "all") |>
  align(j = "Ret", align = "left", part = "body") |>
  bg(part = "header", bg = "lightgray") |>
  color(part = "header", color = "black") |>
  bold(part = "header") |>
  fontsize(size = 10, part = "all") |>
  border_outer(border = fp_border(color = "#BFBFBF", width = 1)) |>
  border_inner(border = fp_border(color = "#BFBFBF", width = 1)) |>
  valign(valign = "center", part = "all") |>
  autofit() |>
  fix_border_issues()

grp <- match(tab_df_shift$Delta, unique(tab_df_shift$Delta))
shaded_rows <- which(grp %% 2 == 1)
ft_shift <- bg(ft_shift, i = shaded_rows, bg = "#F2F2F2", part = "body")

top_rows <- tab_df_shift |>
  dplyr::mutate(row_id = dplyr::row_number()) |>
  dplyr::group_by(Delta) |>
  dplyr::filter(Landings == max(Landings)) |>
  dplyr::pull(row_id)

ft_shift <- ft_shift |>
  bg(i = top_rows, j = 2:5,, bg = "darkgray", part = "body") |>
  bold(i = top_rows, j = 2:5, part = "body")

ft_shift


doc <- read_docx() |>
  body_add_flextable(ft_shift)

print(doc, target = "MLL Analysis/Retention_DM_Scenarios_shifted.docx")






ggplot(DF_Shift, aes(x = F, y = Landings, color = Delta, linetype = Delta)) +
  facet_wrap(~Ret) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(expand = expansion(mult = c(0.001, 0.02))) +
  scale_y_continuous(expand = expansion(mult = c(0.001, 0.02))) +
  scale_color_brewer(palette = 'Dark2') +
  labs(x= 'Fishing mortality',
       y = 'Landings (ton)',
       color = 'Delta', linetype = 'Delta')

ggsave('MLL Analysis/Figures/YieldCurve_Shifted.png', width = 9, height = 6)

ggplot(DF_Shift, aes(x = F, y = SBiomass/max(SBiomass), color = Delta, linetype = Delta)) +
  facet_wrap(~Ret) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(expand = expansion(mult = c(0.001, 0.02))) +
  scale_y_continuous(expand = expansion(mult = c(0.001, 0.02))) +
  scale_color_brewer(palette = 'Dark2') +
  labs(x= 'Fishing mortality', y = bquote(SB/SB[0]),
       color ='Delta', linetype = 'Delta')

ggsave('MLL Analysis/Figures/SBCurve_Shifted.png', width = 9, height = 6)






