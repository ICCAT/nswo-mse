
if (!packageVersion('MSEtool') >= '4.0.0') {
  cli::cli_alert_warning('This analysis requires latest development version of `MSEtool`. Installing now ...')
  pak::pkg_install('blue-matter/MSEtool@dev')
}

library(MSEtool)
library(SWOMSE)

SSDir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/2024_OMs/Reference/005_M0.2_sigmaR0.2_steepness0.80_cpuelambda1_llq1_env7'

OM <- ImportSS(SSDir, nSim=100, Interval = 3, DataLag = 2)


# TODO - Allocation for SS

Hist <- Simulate(OM)


ArrayReduceDims <- MSEtool:::ArrayReduceDims
ConvertDF <- MSEtool:::ConvertDF
CheckClass <- MSEtool:::CheckClass

slotName <- 'NaturalMortality'



SelectivityAtAge <- GetSelectivityAtAge(Hist) |> dplyr::filter(TimeStep==max(TimeStep))
SelectivityAtLength <- GetSelectivityAtLength(Hist) |> dplyr::filter(TimeStep==max(TimeStep))

plot(SelectivityAtAge, color='Stock')
plot(SelectivityAtLength, color='Stock')

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
