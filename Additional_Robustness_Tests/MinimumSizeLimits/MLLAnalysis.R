
if (!packageVersion('MSEtool') >= '4.0.0') {
  cli::cli_alert_warning('This analysis requires latest development version of `MSEtool`. Installing now ...')
  pak::pkg_install('blue-matter/MSEtool@dev')
}

library(MSEtool)
library(SWOMSE)
library(flextable)

fig.dir <- 'Additional_Robustness_Tests/MinimumSizeLimits/Figures'

# ---- Run MSE with Full Retention ----

SSDir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/2024_OMs/Reference/005_M0.2_sigmaR0.2_steepness0.80_cpuelambda1_llq1_env7'

OM <- ImportSS(SSDir, nSim=100)


saveRDS(OM, 'Additional_Robustness_Tests/MinimumSizeLimits/OM.om')
Hist <- Simulate(OM)

saveRDS(Hist, 'Additional_Robustness_Tests/MinimumSizeLimits/Hist.hist')

source('Additional_Robustness_Tests/MinimumSizeLimits/MLL_MPs.R')

MPs <- c('MCC11', 'MCC11_FR')

MSE <- Project(Hist, MPs)
saveRDS(MSE, 'Additional_Robustness_Tests/MinimumSizeLimits/FullRetention.mse')

# ---- Plots and Tables ----
Hist <- readRDS('Additional_Robustness_Tests/MinimumSizeLimits/Hist.hist')
MSE <- readRDS('Additional_Robustness_Tests/MinimumSizeLimits/FullRetention.mse')



# Plot Landings & Discards
Landings <- Landings(MSE, ByFleet=TRUE)
Discards <- Discards(MSE, ByFleet=TRUE, type='all')
Discards <- Discards(MSE, ByFleet=TRUE)

DF <- dplyr::bind_rows(Landings, Discards) |>
  dplyr::group_by(Sim, TimeStep, Period, Variable, Fleet) |>
  dplyr::summarise(Value=sum(Value))

DF$Variable <- factor(DF$Variable, levels=rev(unique(DF$Variable)), ordered = TRUE)

DFHist <- DF |> dplyr::filter(Period=='Historical')

ggplot(DFHist, aes(x=TimeStep, y=Value, color=Variable)) +
  facet_wrap(~Fleet) +
  geom_line() +
  theme_bw() +
  labs(x='Year', y='Metric Ton', color='') +
  scale_color_manual(values=c('black', 'darkgray')) +
  theme(strip.background = element_blank())

ggsave(file.path(fig.dir, 'Landings_Discards.png'), width=7, height=4)


FracDiscarded <- DFHist |> dplyr::ungroup() |>
  dplyr::filter(TimeStep>=2020) |>
  dplyr::mutate(Value=ifelse(Value<0,0, Value)) |>
  dplyr::filter(Value>0) |>
  tidyr::pivot_wider(values_from = Value, names_from = Variable) |>
  dplyr::filter(`Discards (dead)`>0) |>
  dplyr::mutate(Removals=Landings+`Discards (dead)`) |>
  dplyr::group_by(Fleet) |>
  dplyr::summarise(Removals=sum(Removals), Discards=sum(`Discards (dead)`)) |>
  dplyr::mutate(FracDiscard=Discards/Removals) |>
  dplyr::mutate(Removals=round(Removals,0),
                Discards=round(Discards,0),
                FracDiscard=round(FracDiscard,2)*100)

table <- flextable::flextable(FracDiscarded)
flextable::save_as_docx(table, path=file.path(fig.dir, '../FracDiscard.docx'))


KeepFleets <- Landings |>
  dplyr::filter(Period=='Historical') |>
  dplyr::filter(TimeStep==max(TimeStep), Value>0) |>
  dplyr::reframe(Fleet=as.character(unique(Fleet))) |>
  dplyr::pull(Fleet)

SelectivityAtAge <- GetSelectivityAtAge(Hist) |>
  dplyr::filter(TimeStep==max(TimeStep), Fleet %in% KeepFleets)

SelectivityAtLength <- GetSelectivityAtLength(Hist) |>
  dplyr::filter(TimeStep==max(TimeStep), Fleet %in% KeepFleets)

RetentionAtAge <- GetRetentionAtAge(Hist) |>
  dplyr::filter(TimeStep==max(TimeStep), Fleet %in% KeepFleets)

RetentionAtLength <- GetRetentionAtLength(Hist) |>
  dplyr::filter(TimeStep==max(TimeStep), Fleet %in% KeepFleets)



AtLength <- dplyr::bind_rows(SelectivityAtLength, RetentionAtLength) |>
  dplyr::mutate(Variable=factor(Variable, c('Selectivity', 'Retention'), ordered=TRUE))

plot(AtLength |> dplyr::filter(Stock=='Female'), color='Variable', xlab='Length', ylab='Probability',
     ColorLab='') +
  scale_color_manual(values=c('black', 'darkgrey')) +
  labs(x='Length (cm)')

ggsave(file.path(fig.dir, 'AtLength.png'), width=6, height=4)

AtAge <- dplyr::bind_rows(SelectivityAtAge, RetentionAtAge) |>
  dplyr::mutate(Variable=factor(Variable, c('Selectivity', 'Retention'), ordered=TRUE))

plot(AtAge |> dplyr::filter(Stock=='Female'), color='Variable', ylab='Probability',
     ColorLab='') +
  scale_color_manual(values=c('black', 'darkgrey'))

ggsave(file.path(fig.dir, 'AtAge.png'), width=6, height=4)


# Plot Projections and Performance ....
library(Slick)

Status <- function(MSE) {
  FFMSY <- F_FMSY(MSE) |> dplyr::filter(Period=='Projection')
  SBSBMSY <- SB_SBMSY(MSE) |> dplyr::filter(Period=='Projection')

  dplyr::bind_rows(SBSBMSY, FFMSY) |>
    dplyr::select(Sim, TimeStep, Value, Variable, MP) |>
    tidyr::pivot_wider(names_from = Variable, values_from = Value) |>
    dplyr::mutate(Green=SB_SBMSY>=1 & F_FMSY<=1) |>
    dplyr::group_by(MP) |>
    dplyr::summarise(Value=mean(Green),
                     Variable="Status")

}

Status(MSE)


Slick <- MSEtool::MSE2Slick(MSE)

ggplot(Landings, aes(x=TimeStep, y=Value, color=MP)) +
  geom_line()

source('R/PMs.r')
PMs <- c('TAC1' , "PGK_short", 'PGK_med',
         'PBMSY', "PNOF",
         "AvCatch",  "VarC")




# Plot Landings & Discards




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

ggplot(DF |> dplyr::filter(Sim==1, Period=='Historical'),
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
