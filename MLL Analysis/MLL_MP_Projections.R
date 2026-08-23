
library(MSEtool)
library(SWOMSE)

if (packageVersion('MSEtool') < '4.0.0')
  stop('Requires MSEtool v4+')

MainDir <- 'G:/Shared drives/BM shared/1. Projects/ICCAT NSWO/OMs/'
SSDir   <- file.path(MainDir, 'Reference')
OMDirs  <- list.dirs(SSDir, recursive = FALSE)

# ---- Import SS Reports ----
SetupParallel()

RepList <- MSEtool::ImportSSReport(OMDirs,
                                   parallel = TRUE)
DisableParallel()

# ---- Import OMs ----

for (i in seq_along(OMDirs)) {
 name <- basename(OMDirs[i])
 Name <- strsplit(name, "_")[[1]][1]

 OM <- MSEtool::ImportSS(RepList[i], Name = Name,
                         nSim = 100,
                         LengthUnits = 'cm',
                         WeightUnits = 'kg',
                         R0Units = 1000)
 MSEtool::Save(OM, file.path('MLL Analysis/Objects/OM', paste0(Name, '.om')),
               overwrite = TRUE)

 Hist <- MSEtool::Simulate(OM)
 MSEtool::Save(Hist, file.path('MLL Analysis/Objects/Hist', paste0(Name, '.hist')),
               overwrite = TRUE)

}


# ---- Define MPs ----

MCC11 <- function(Data,
                  Data_Lag = 2,
                  Interval = 3,
                  tunepar = 0.756222283813747,
                  mc = NA, ...) {

  advice <- Advice()
  CurrentTS <- tail(Data@Years, 1)
  if ((CurrentTS+1) %in% Catchdf$Year) {
    advice@TAC <-  Catchdf$Catch[match(CurrentTS, Catchdf$Year)]
    return(advice)
  }

  Initial_MP_Yr <- max(Catchdf$Year)+1
  ManagementTimeSteps <- seq(Initial_MP_Yr, by=Interval, length.out=50)

  if (!(CurrentTS+1) %in% ManagementTimeSteps) {
    advice@TAC <- tail(Data@Advice@TAC[!is.na(Data@Advice@TAC)],1) |> as.numeric()
    return(advice)
  }

  TACbase <- 12600 * tunepar

  CombinedIndex <- Data@Survey@Value[,8]

  Ibase <- mean(CombinedIndex[match(2017:2019, Data@Years)], na.rm=TRUE)

  # combined index averaged over last available 3 # years in time-series (y-4, y-3, y-2)
  Icurr <- mean(tail(CombinedIndex,3))

  Irat <- Icurr/Ibase

  fixed_low_TAC <- NULL  # initialize

  if (Irat>=1.85) {
    deltaTAC <- 1.85
  }
  if (Irat>=1.75 & Irat<1.85) {
    deltaTAC <- 1.75
  }
  if (Irat>=1.65 & Irat<1.75) {
    deltaTAC <- 1.65
  }
  if (Irat>=1.55 & Irat<1.65) {
    deltaTAC <- 1.55
  }
  if (Irat>=1.45 & Irat<1.55) {
    deltaTAC <- 1.45
  }
  if (Irat>=1.35 & Irat<1.45) {
    deltaTAC <- 1.35
  }
  if (Irat>=1.25 & Irat<1.35) {
    deltaTAC <- 1.25
  }
  if (Irat>=1.15 & Irat<1.25) {
    deltaTAC <- 1.15
  }
  if (Irat>=0.75 & Irat<1.15) {
    deltaTAC <- 1
  }
  if (Irat>=0.5 & Irat<0.75) {
    deltaTAC <- 0.75
  }
  if (Irat<0.5) {
    deltaTAC <- 0.5
  }

  advice@TAC <- TACbase * deltaTAC
  advice
}
class(MCC11) <- 'mp'

FullRetention <- function(Advice) {
  Advice@Retention <- Retention(Pars = list(RL50=0, RL50_95=0, MaxRet = 1))
  Advice
}

MCC11_FR <- function(Data,...) {
  Advice <-  MCC11(Data, ...)
  CurrentTS <- tail(Data@Years,1)
  if ((CurrentTS+1) %in% Catchdf$Year)
    return(Advice)
  Advice |> FullRetention()
}
class(MCC11_FR) <- 'mp'

# ---- Base Case Projections ----

## ---- Status Quo ----
HistFiles <- list.files('MLL Analysis/Objects/Hist', full.names = TRUE)

for (i in seq_along(HistFiles)) {
  Hist <- readRDS(HistFiles[i])
  Name <- Name(Hist@OM)

  MSE <- Project(Hist, MPs = c("MCC11", "MCC11_FR"))

  MSEtool::Save(MSE, file.path('MLL Analysis/Objects/MSE/SQ', paste0(Name, '.mse')),
                overwrite = TRUE)
}


# ---- Robustness: Age-Based Natural Mortality ----

library(furrr)

MSEtool::SetupParallel(workers = 9)

future_walk(seq_along(RepList), function(i) {

  replist <- RepList[[i]]

  name <- basename(OMDirs[i])
  Name <- strsplit(name, "_")[[1]][1]

  robustDir <- file.path(MainDir, 'Lorenzen', Name)

  dir.create(robustDir)
  file.copy(file.path(MainDir, 'Lorenzen', 'ss3.exe'),
            file.path(robustDir, 'ss3.exe')
  )
  files <- list.files(OMDirs[[i]])
  file.copy(file.path(OMDirs[[i]], files),
            file.path(robustDir, files),
            overwrite = TRUE)

  dat <- r4ss::SS_readdat(file = file.path(robustDir, replist$Data_File))

  ctl <- r4ss::SS_readctl(
    file = file.path(robustDir, replist$Control_File),
    datlist = dat,
    use_datlist = TRUE,
    verbose = FALSE
  )

  ctl$natM_type <- 2       # Lorenzen
  ctl$Lorenzen_refage <- 5 # age of 50% maturity

  r4ss::SS_writectl(ctl, file.path(robustDir, replist$Control_File), overwrite = TRUE)

  r4ss::run(
    dir = robustDir,
    skipfinished = FALSE,
    show_in_console = FALSE,
    extras = "-nohess -nox"
  )

  OM <- ImportSS(robustDir, Name = Name)

  MSEtool::Save(OM, file.path('MLL Analysis/Objects/Lorenzen/OM', paste0(OM@Name, '.om')),
                overwrite = TRUE)

  Hist <- Simulate(OM)

  MSEtool::Save(Hist, file.path('MLL Analysis/Objects/Lorenzen/OM', paste0(OM@Name, '.hist')),
                overwrite = TRUE)

}, .options = furrr_options(seed = TRUE))

DisableParallel()

## ---- Robustness Projections ----

HistFiles <- list.files('MLL Analysis/Objects/Lorenzen/Hist', full.names = TRUE)

for (i in seq_along(HistFiles)) {
  Hist <- readRDS(HistFiles[i])
  Name <- Name(Hist@OM)

  MSE <- Project(Hist, MPs = c("MCC11", "MCC11_FR"))

  MSEtool::Save(MSE,
                file.path('MLL Analysis/Objects/MSE/Lorenzen', paste0(Name, '.mse')),
                overwrite = TRUE)
}



