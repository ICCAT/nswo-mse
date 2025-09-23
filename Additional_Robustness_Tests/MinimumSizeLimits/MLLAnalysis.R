
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

DFHist |> dplyr::filter(Variable=='Discards (dead)', Value>10)

ggplot(DFHist, aes(x=TimeStep, y=Value, color=Variable)) +
  facet_wrap(~Fleet) +
  geom_line() +
  theme_bw() +
  labs(x='Year', y='Metric Ton', color='') +
  scale_color_manual(values=c('black', 'darkgray')) +
  theme(strip.background = element_blank())

ggsave(file.path(fig.dir, 'Landings_Discards.png'), width=7, height=4)

ggplot(DFHist |> dplyr::filter(Variable=='Landings'),
       aes(x=TimeStep, y=Value)) +
  facet_wrap(~Fleet) +
  geom_line() +
  theme_bw() +
  labs(x='Year', y='Landings (Metric Ton)', color='') +
  scale_color_manual(values=c('black', 'darkgray')) +
  theme(strip.background = element_blank())

ggsave(file.path(fig.dir, 'Landings.png'), width=7, height=4)

ggplot(DFHist |> dplyr::filter(Variable!='Landings'),
       aes(x=TimeStep, y=Value)) +
  facet_wrap(~Fleet) +
  geom_line() +
  theme_bw() +
  labs(x='Year', y='Dead Discards (Metric Ton)', color='') +
  scale_color_manual(values=c('black', 'darkgray')) +
  theme(strip.background = element_blank())

ggsave(file.path(fig.dir, 'Discards.png'), width=7, height=4)

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


sum(FracDiscarded$Discards)/sum(FracDiscarded$Removals)




FracDiscarded <- DFHist |> dplyr::ungroup() |>
  dplyr::filter(TimeStep>=2020) |>
  # dplyr::mutate(Value=ifelse(Value<0,0, Value)) |>
  # dplyr::filter(Value>0) |>
  tidyr::pivot_wider(values_from = Value, names_from = Variable) |>
  dplyr::filter(Landings>0) |>
  dplyr::mutate(Removals=Landings+`Discards (dead)`) |>
  dplyr::group_by(Fleet, TimeStep) |>
  dplyr::summarise(Removals=sum(Removals), Discards=sum(`Discards (dead)`)) |>
  dplyr::mutate(FracDiscard=Discards/Removals) |>
  dplyr::mutate(Removals=round(Removals,0),
                Discards=round(Discards,0),
                FracDiscard=round(FracDiscard,2)*100)

table <- flextable::flextable(FracDiscarded)
table


flextable::save_as_docx(table, path=file.path(fig.dir, '../FracDiscard_2.docx'))






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

SBSBMSY <- SB_SBMSY(MSE) |>
  dplyr::filter(Stock=='Female') |>
  dplyr::group_by(TimeStep, Period, Variable, MP) |>
  dplyr::summarise(Value=mean(Value))

FFMSY <- F_FMSY(MSE) |>
  dplyr::filter(Stock=='Female') |>
  dplyr::group_by(TimeStep, Period, Variable, MP) |>
  dplyr::summarise(Value=mean(Value))

Landings <- Landings(MSE) |>
  dplyr::group_by(Sim, TimeStep, Period, Variable, MP) |>
  dplyr::summarise(Value=sum(Value)) |>
  dplyr::group_by(TimeStep, Period, Variable, MP) |>
  dplyr::summarise(Value=mean(Value))

Discards <- Discards(MSE) |>
  dplyr::group_by(Sim, TimeStep, Period, Variable, MP) |>
  dplyr::summarise(Value=sum(Value)) |>
  dplyr::group_by(TimeStep, Period, Variable, MP) |>
  dplyr::summarise(Value=mean(Value))

DF <- dplyr::bind_rows(SBSBMSY, FFMSY, Landings, Discards)
DF$Variable <- factor(DF$Variable, c('SB_SBMSY', 'F_FMSY', 'Landings', 'Discards (dead)'), ordered = TRUE)

ggplot(DF |> dplyr::filter(TimeStep>=2025), aes(x=TimeStep, y=Value, color=MP)) +
  facet_wrap(~Variable, scales='free_y') +
  expand_limits(y=0) +
  geom_line() +
  theme_bw() +
  labs(x="Year", y='Mean', color='')

ggsave(file.path(fig.dir, 'Projections.png'), width=6, height=4)


SBiomass(MSE) |> dplyr::filter(Period=='Projection', Stock=='Female') |>
  tidyr::pivot_wider(names_from = MP, values_from = Value) |>
  dplyr::mutate(Rel=MCC11_FR/MCC11) |>
  dplyr::summarise(Mean=mean(Rel))

Landings(MSE) |> dplyr::filter(Period=='Projection') |>
  dplyr::group_by(Sim, TimeStep, Stock, MP) |>
  dplyr::summarise(Value=sum(Value)) |>
  tidyr::pivot_wider(names_from = MP, values_from = Value) |>
  dplyr::mutate(Rel=MCC11_FR/MCC11) |>
  dplyr::ungroup() |>
  dplyr::summarise(Mean=mean(Rel))


t = TACs(MSE)
t |> dplyr::filter(Sim==1, TimeStep%in% 2026:2027)


tt <- SBiomass(MSE) |> dplyr::filter(Period=='Projection', Stock=='Female') |>
  tidyr::pivot_wider(names_from = MP, values_from = Value) |>
  dplyr::mutate(Rel=MCC11_FR/MCC11)

tt |> dplyr::filter(Rel>1)

R <- apply(MSE@Landings$Female[1,,,,1,], c(2,4), sum) + apply(MSE@Landings$Male[1,,,,1,], c(2,4), sum) +
apply(MSE@Discards$Female[1,,,,1,], c(2,4), sum) + apply(MSE@Discards$Male[1,,,,1,], c(2,4), sum)


MSE@SBiomass[1,1,,]
R

(MSE@SBiomass[1,1,,1]/MSE@SBiomass[1,1,,2])
MSE@Biomass[1,1,,1]/MSE@Biomass[1,1,,2]

colSums(MSE@Number$Female[1,,4,1,] + MSE@Number$Male[1,,4,1,])


apply(MSE@FDeadAtAge$Female[1,,4,,], c(1,3), sum) |> apply('MP', max)


L
D


yr <- MSE@PPD$MCC11$`1`$`Female Male`@TimeSteps
ind <- MSE@PPD$MCC11$`1`$`Female Male`@Survey@Value[,8]


plot(yr, ind, type='b', ylim=c(0,2))
lines(yr[yr<=2022], ind[yr<=2022], col='blue')

Data <- DataTrim(MSE@PPD$MCC11$`1`$`Female Male`, 2022)

MSE@PPD$MCC11$`1`$`Female Male`@Survey@Value[1:73,8]

tacs <- TACs(MSE)
tacs |> dplyr::filter(TimeStep==2025)

Removals |> dplyr::filter(TimeStep==2028)



Status <- function(MSE) {
  FFMSY <- F_FMSY(MSE) |> dplyr::filter(Period=='Projection', Stock=='Female')
  SBSBMSY <- SB_SBMSY(MSE) |> dplyr::filter(Period=='Projection', Stock=='Female')

  SBSBMSY |> dplyr::filter(is.na(Value)!=TRUE)
  SBSBMSY$TimeStep |> unique()

  dplyr::bind_rows(SBSBMSY, FFMSY) |>
    dplyr::select(Sim, TimeStep, Value, Variable, MP) |>
    tidyr::pivot_wider(names_from = Variable, values_from = Value) |>
    dplyr::mutate(Green=SB_SBMSY>=1 & F_FMSY<=1) |>
    dplyr::group_by(MP) |>
    dplyr::summarise(Value=mean(Green),
                     Variable="Status")

}



Safety <- function(MSE, Ref=0.4) {
  SB_SBMSY(MSE) |>
    dplyr::filter(Period=='Projection') |>
    dplyr::mutate(Above=Value>=Ref) |>
    dplyr::group_by(MP, Sim) |>
    dplyr::mutate(AboveAll=prod(Above)) |>
    dplyr::group_by(MP) |>
    dplyr::summarise(Value=mean(AboveAll))
}

MeanLandings <- function(MSE) {
  Landings(MSE) |> dplyr::filter(Period=='Projection') |>
    dplyr::group_by(Sim, MP, Stock) |>
    dplyr::summarise(Value=sum(Value)) |>
    dplyr::group_by(MP) |>
    dplyr::summarise(Value=mean(Value),
                     Variable='Mean Landings')
}

Status(MSE)
Safety(MSE)
MeanLandings(MSE)



t =   dplyr::bind_rows(SBSBMSY, FFMSY) |>
  dplyr::select(Sim, TimeStep, Value, Variable, MP)

t

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
