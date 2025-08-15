
if (!packageVersion('MSEtool') >= '4.0.0') {
  cli::cli_alert_warning('This analysis requires latest development version of `MSEtool`. Installing now ...')
  pak::pkg_install('blue-matter/MSEtool@dev')
}

library(MSEtool)
library(SWOMSE)

SSDir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/2024_OMs/Reference/005_M0.2_sigmaR0.2_steepness0.80_cpuelambda1_llq1_env7'

OM <- ImportSS(SSDir, nSim=100)

# TODO - Allocation for SS

OM@Interval <- 3
OM@DataLag <- 2

Hist <- Simulate(OM)

ArrayReduceDims <- MSEtool:::ArrayReduceDims
ConvertDF <- MSEtool:::ConvertDF
CheckClass <- MSEtool:::CheckClass

slotName <- 'NaturalMortality'

GetSchedule <- function(OM, Variable='Length', Slot='MeanAtAge', df=TRUE) {
  CheckClass(OM, c('om', 'hist', 'mse'))

  isStock <- Variable %in% slotNames('stock')
  isFleet <- Variable %in% slotNames('fleet')

  if (!isStock & !isFleet)
    cli::cli_abort('{.val {Variable}} is not a slot in `stock` or `fleet` class objects')

  if (inherits(OM, 'om')) {
    Array <- GetScheduleOM(OM, Variable, Slot, isStock)
  } else {
    Array <- GetScheduleHist(OM, Variable, Slot, isStock)
  }
  if (!df)
    return(Array)

  DF <- Array |>
    array2DF() |>
    ConvertDF() |>
    dplyr::mutate(Variable=Variable)

  ColOrder <- c('Sim', 'Stock', 'Age', 'Class', 'TimeStep', 'Fleet', 'Value', 'Variable')
  ColOrder <- ColOrder[ColOrder %in% colnames(DF)]

  DF <- DF |> dplyr::select(dplyr::all_of(ColOrder))
  class(DF) <- c('Schedule', class(DF))
  DF

}

GetAtLength <- function(OM, Variable, df=TRUE) {
  GetSchedule(OM, Variable, Slot='MeanAtLength', df=TRUE)
}

GetAtAge <- function(OM, slotName, df=TRUE) {
  GetSchedule(OM, Variable, Slot='MeanAtAge', df=TRUE)
}

GetScheduleOM <- function(OM, slotName, Slot, isStock) {
  CheckClass(OM, c('om', 'hist', 'mse'))
  OM <- PopulateOM(OM, silent=TRUE)
  if (isStock)
    return(
      purrr::map(OM@Stock, \(Stock)
                 Stock |> slot(slotName) |> slot(Slot) |>
                   ArrayReduceDims()) |>
        List2Array("Stock")
    )
  purrr::map(OM@Fleet, \(Stock) {
    purrr::map(Stock, \(Fleet) Fleet |> slot(slotName) |> slot(Slot) |>
                 ArrayReduceDims()) |>
      List2Array("Fleet")
  }) |> List2Array("Stock")
}

GetScheduleHist <- function(Hist, slotName, Slot,isStock) {
  CheckClass(Hist, c('hist', 'mse'))
  OM <- Hist@OM
  if (isStock)
    return(
      purrr::map(OM@Stock, \(Stock)
                 Stock |> slot(slotName) |> slot(Slot) |>
                   ArrayReduceDims())|>
        List2Array("Stock")
    )

  purrr::map(OM@Fleet, \(Stock)
             Stock |> slot(slotName) |> slot(Slot) |>
               ArrayReduceDims())|>
    List2Array("Stock")
}


GetSelectivityAtAge <- function(OM, df=TRUE) {
  GetSchedule(OM, "Selectivity", df=df)
}

GetRetentionAtAge <- function(OM, df=TRUE) {
  GetSchedule(OM, "Retention", df=df)
}

GetSelectivityAtLength <- function(OM, df=TRUE) {
  GetSchedule(OM, "Selectivity", 'MeanAtLength', df=df)
}

GetRetentionAtLength <- function(OM, df=TRUE) {
  GetSchedule(OM, "Retention", 'MeanAtLength', df=df)
}

plot.Schedule <- function(x, TimeLab='Year', color='TimeStep',
                       ylab=NULL) {

  if (is.null(ylab))
    ylab <- x$Variable |> unique()
  x[[color]] <- as.factor(x[[color]])

  nColor <- unique(x[[color]]) |> length()
  ColNames <- colnames(x)
  if ("Age" %in% ColNames)
    XVar <- "Age"
  if ("Class" %in% ColNames)
    XVar <- "Class"

  nStock <- unique(x$Stock) |> length()
  nFleet <- suppressWarnings(unique(x$Fleet)) |> length()

  # TO DO - average over sims

  if (nColor<2) {
    p <- ggplot(x, aes(x=.data[[XVar]], y=Value))
  } else {
    p <- ggplot(x, aes(x=.data[[XVar]], y=Value, color=.data[[color]]))
  }

  if (nFleet<=1 & nStock>1 & color!='Stock')
    p <- p + facet_wrap(~Stock)

  if (nFleet>1 & nStock<=1 & color!='Fleet')
    p <- p + facet_wrap(~Fleet)

  if (nFleet>1 & nStock>1) {
    if (color!='Stock' & color!='Fleet') {
      p <- p + facet_grid(Fleet~Stock)
    } else if (color=='Stock') {
      p <- p + facet_wrap(~Fleet)
    } else if (color=='Fleet') {
      p <- p + facet_wrap(~Stock)
    }
  }

  if (color=='TimeStep')
    ColorLab <- TimeLab

  if (color=='Stock')
    ColorLab <- "Stock"

  if (color=='Fleet')
    ColorLab <- "Fleet"

  p <- p + geom_line() +
      expand_limits(y=0) +
      theme_bw() +
      labs(y=ylab,  color=ColorLab)

  if (nColor<2)
    p <- p + guides(color='none')
  p

}


SelectivityAtAge <- GetSelectivityAtAge(Hist) |> dplyr::filter(TimeStep==max(TimeStep))
SelectivityAtLength <- GetSelectivityAtLength(Hist) |> dplyr::filter(TimeStep==max(TimeStep))

plot(SelectivityAtAge, color='Stock')
plot(SelectivityAtLength, color='Stock')

MaturityAtAge <- GetAtAge(Hist, 'Maturity')



MCC11 <- function(Data,
               Data_Lag = 2,
               Interval = 3,
               tunepar = 0.756222283813747,
               mc = NA, ...) {

  advice <- Advice()

  CurrentTS <- tail(Data@TimeSteps,1)

  if (CurrentTS %in% Catchdf$Year) {
    advice@TAC <-  Catchdf$Catch[match(CurrentTS, Catchdf$Year)]
    return(advice)
  }

  Initial_MP_Yr <- max(Catchdf$Year)+1
  ManagementTimeSteps <- seq(Initial_MP_Yr, by=Interval, length.out=50)

  if (!(CurrentTS+1) %in% ManagementTimeSteps) {
    advice@TAC <- tail(Data@TAC[!is.na(Data@TAC)],1) |> as.numeric()
    return(advice)
  }





  TACbase <- 12600 * tunepar

  CombinedIndex <- Data@Index@Value[,15]

  Ibase <- mean(CombinedIndex[match(2017:2019, Data@TimeSteps)], na.rm=TRUE)

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
