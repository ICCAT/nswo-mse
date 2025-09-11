
if (!packageVersion('MSEtool') >= '4.0.0') {
  cli::cli_alert_warning('This analysis requires latest development version of `MSEtool`. Installing now ...')
  pak::pkg_install('blue-matter/MSEtool@dev')
}

library(MSEtool)
library(SWOMSE)

# ---- Run MSE with Full Retention ----

SSDir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/2024_OMs/Reference/005_M0.2_sigmaR0.2_steepness0.80_cpuelambda1_llq1_env7'

OM <- ImportSS(SSDir, nSim=100)

Hist <- Simulate(OM)

saveRDS(Hist, 'Additional_Robustness_Tests/MinimumSizeLimits/Hist.hist')

source('Additional_Robustness_Tests/MinimumSizeLimits/MLL_MPs.R')

MPs <- c('MCC11', 'MCC11_FR')

MSE <- Project(Hist, MPs)
saveRDS(MSE, 'Additional_Robustness_Tests/MinimumSizeLimits/FullRetention.mse')


# ---- Load MSE ----
Hist <- readRDS('Additional_Robustness_Tests/MinimumSizeLimits/Hist.hist')
MSE <- readRDS('Additional_Robustness_Tests/MinimumSizeLimits/FullRetention.mse')



# Plots
Landings <- Landings(Hist, FALSE, TRUE)

KeepFleets <- Landings |>
  dplyr::filter(TimeStep==max(TimeStep), Value>0) |>
  dplyr::reframe(Fleet=as.character(unique(Fleet))) |>
  dplyr::pull(Fleet)

# Plot Selectivity & Retention

SelectivityAtAge <- GetSelectivityAtAge(MSE) |>
  dplyr::filter(TimeStep==max(TimeStep), Fleet %in% KeepFleets)

SelectivityAtLength <- GetSelectivityAtLength(Hist) |>
  dplyr::filter(TimeStep==max(TimeStep), Fleet %in% KeepFleets)

RetentionAtAge <- GetRetentionAtAge(Hist) |>
  dplyr::filter(TimeStep==max(TimeStep), Fleet %in% KeepFleets)

RetentionAtLength <- GetRetentionAtLength(Hist) |>
  dplyr::filter(TimeStep==max(TimeStep), Fleet %in% KeepFleets)


# Plot Landings & Discards

Landings <- Landings(MSE, FALSE, TRUE)


Landings <- Landings |>
  dplyr::group_by(Fleet) |>
  dplyr::group_by(Sim, TimeStep, Variable, Period, Fleet, MP) |>
  dplyr::summarise(Value=sum(Value))

Discards <- Discards(MSE, FALSE, TRUE) |>
  dplyr::group_by(Fleet) |>
  dplyr::group_by(Sim, TimeStep, Variable, Period, Fleet, MP) |>
  dplyr::summarise(Value=sum(Value))

DF <- dplyr::bind_rows(Landings, Discards)
DF$Variable <- factor(DF$Variable, levels=unique(DF$Variable), ordered = TRUE)

ggplot(DF |> dplyr::filter(Sim==1),
       aes(x=TimeStep, y=Value, color=Variable)) +
  facet_grid(MP~Fleet) +
  geom_line() +
  labs(x='Year', y='Catch (t)', color='') +
  theme_bw() +
  scale_color_manual(values=c('black', 'blue'))


Landings |> dplyr::filter(Sim==1, Fleet=='SPN_1', Period=='Projection') |>
  tidyr::pivot_wider(values_from = Value, names_from = MP) |>
  print(n=30)

Discards |> dplyr::filter(Sim==1, Fleet=='SPN_1') |> dplyr::filter(Sim==1, Fleet=='SPN_1', Period=='Projection') |>
  tidyr::pivot_wider(values_from = Value, names_from = MP)


TACs(MSE)

AtLength <- dplyr::bind_rows(SelectivityAtLength, RetentionAtLength)
plot(AtLength |> dplyr::filter(Stock=='Female'), color='Variable', xlab='Length', ylab='Probability',
     ColorLab='')



AtAge <- dplyr::bind_rows(SelectivityAtAge, RetentionAtAge)
plot(AtAge |> dplyr::filter(Stock=='Female'), color='Variable', ylab='Probability',
     ColorLab='')

Discards <- Discards(MSE)
Landings <- Landings(MSE)


# plots

# - different selectivity pattern????










df <- Biomass(MSE) |> dplyr::group_by(TimeStep, MP) |>
  dplyr::summarise(Value=mean(Value))

ggplot(df, aes(x=TimeStep, y=Value, color=MP)) +
  geom_line()


SelectivityAtAge <- GetSelectivityAtAge(Hist) |> dplyr::filter(TimeStep==max(TimeStep))
SelectivityAtLength <- GetSelectivityAtLength(Hist) |> dplyr::filter(TimeStep==max(TimeStep))

plot(SelectivityAtAge, color='Stock')
plot(SelectivityAtLength, color='Stock')





ArrayReduceDims <- MSEtool:::ArrayReduceDims
ConvertDF <- MSEtool:::ConvertDF
CheckClass <- MSEtool:::CheckClass

slotName <- 'NaturalMortality'





MaturityAtAge <- GetAtAge(Hist, 'Maturity')



FullRetention <- function(Advice) {
  Advice@Retention@Pars <- list(RL50=0, RL50_95=0)
  Advice
}

ShiftSelectivity <- function(Advice) {
  Advice@Selectivity@Pars <- list
}

MCC11_FR <- function(Data,...) {
  MCC11(Data, ...) |> FullRetention()
}


Hist@OM@Interval <- 1
MPs <- c('MCC11', 'MCC11_FR')
MSE <- Project(Hist, MPs)

r = MSE@Landings |> apply(c('Sim', 'TimeStep', 'MP'), sum)
r[5,,]

# TODO - why is this named Male - ie second stock??
MSE@Misc$Retention$MCC11_b_FR$Male

SBiomass(MSE, RelTo='SB0')
Biomass(MSE, RelTo='B0')


MSE@Removals |> apply(c('Sim', 'TimeStep'), sum)

# Plot Selectivity and Retention Curves
# Plot new Retention
# Shift Selectivity

# Performance of MCC11 MP
