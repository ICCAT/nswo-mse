
library(SWOMSE)
library(Slick)

basedir <- 'Slick_objects/SCRS_P_2022_051'
MSEdir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/Results'


# --- OM Grid Design ----

Design <- OM_DF %>%
  filter(Class %in% c('Reference', 'R3. increase q')) %>%
  select(OM.object, M, steepness, Class)

Design2 <- Design[,2:ncol(Design)]
for(i in 1:ncol(Design2))Design2[,i]<-match(Design2[,i],unique(Design2[,i]))

# --- Make the MSE list ---

# Reference OMs
Ref_OMs <- list.files(file.path(MSEdir, 'Reference'), pattern='.mmse',recursive = TRUE)
Ref_OMs_dirs <- paste0('Reference/', Ref_OMs)

# Robustness
Rob_OMs <- list.files(file.path(MSEdir, 'Robustness'), pattern='.mmse', recursive = TRUE)
Rob_OMs_dirs <- paste0('Robustness/', Rob_OMs)

All_OMs_dirs <- c(Ref_OMs_dirs, Rob_OMs_dirs)


MSElist<-list()
for (i in 1:nrow(Design)) {
  fl <- All_OMs_dirs[grepl(Design$OM.object[i], All_OMs_dirs)]
  mmse <- readRDS(file.path(MSEdir, fl))

  # Add multihist
  hist.fl <- gsub('.mmse', '.hist', fl)
  hist <- readRDS(file.path(MSEdir, hist.fl))
  mmse@multiHist <- hist
  MSElist[[i]]<- mmse
}

# --- Management Procedures ---
MPs <- MSElist[[1]]@MPs[[1]]
MP_Desc <- c('Surplus Production (SP) Schaefer Model with TAC = MSY',
             'Schaefer SP Model with TAC = FMSY x Biomass',
             'Schaefer SP Model with TAC calculated from harvest control rule',
             'Fox SP Model with TAC = MSY',
             'Fox SP Model with TAC = FMSY x Biomass',
             'Fox SP Model with TAC calculated from harvest control rule',
             'State-Space Schaefer SP Model with TAC = MSY',
             'State-Space Schaefer SP with TAC = FMSY x Biomass',
             'State-Space Schaefer SP with TAC calculated from harvest control rule'
             )

# --- Construct the SLICK object using the Make_SLICK function for MSEtool compatible MSEs ----

PMs<-c("Safety_S",
       'Safety_M',
       'Status_S',
       'Status_M',
       'Stability',
       'Yield_S',
       'Yield_M')


obj<-Make_Slick(name="SCRS/P/2022/051 Reference and R3 OMs",
                MPs=MPs,
                PMs=PMs,
                Design = Design2,
                MP_Desc=MP_Desc,
                SN = list(
                  Factor_Labels=c("Natural Mortality","Steepness", "Robustness"),
                  Labels = list(c("M=0.1","M=0.2", 'M=0.3'),
                                c('h=0.6', 'h=0.75', 'h=0.9'),
                                c('Reference', 'R3. Increasing q')),
                  Codes =  list(c("0.1","0.2", '0.3'),
                                c('0.6', '0.75', '0.9'),
                                c('Reference', 'R3. 1% Increase q')
                                ),
                  Description = list(paste("Natural mortality rate of",c(0.1, 0.2, 0.3)),
                                     paste("Beverton-Holt stock recruitment steepness (resilience) of",c(0.6, 0.75, 0.9)),
                                     c('Reference Case', 'Assumed 1% Annual Increase in Historical Catchability')
                                     )
                ),
                MSElist=MSElist,
                fstYr=2021
)

# Last historical year
obj$StateVar$TimeNow <- 2020

# -- Title --
obj$Text$Title <- "North Atlantic Swordfish Demonstration MSE Results"
obj$Text$Sub_title <- "Surplus Production Candidate MPs for Reference and Robustness Set 3. OMs"

# - Metadata -
obj$Misc$Author<-"Adrian Hordyk"
obj$Misc$Contact <-"<a href='mailto:adrian@bluematterscience.com'>adrian@bluematterscience.com</a>"
obj$Misc$Date<-"September 2022"
obj$Misc$Institution <- "Blue Matter Science"
obj$Misc$Fishery <- 'North Atlantic Swordfish'

# - Intro Text -
obj$Text$Introduction[[1]]<-"These results show the performance of a set of management procedures that use surplus production assessment models and several different harvest control rules to generate TAC advice. This analysis was conducted as a preliminary investigation of the performance of surplus production assessment models. These results were presented to the ICCAT Swordfish Species Working Group in SCRS/P/2022/051."

obj$Text$Introduction[[2]]<-"<strong>Note:</strong>These results were posted with permission from the International Commission for the Conservation of Atlantic Tunas (ICCAT) for the purpose of demonstrating the features of Slick. The North Atlantic Swordfish MSE process in still ongoing. The operating models, candidate management procedures, and performance metrics shown here are for demonstration purposes only and are subject to change as the MSE process contiunes. The results presented here do not
necessarily reflect the point of view of ICCAT or other funders and in no ways anticipate ICCAT future policy in this area."


# Standardize Yield Metric
# All Performance Metrics in Slick must be scaled between 0 (worst) and 100 (best)
# The average yield PM in the SWO MSE must be re-scaled accordingly.
#
# Here they are calculated as a percent difference from the highest yield

# Deterministic
Yind<-grepl('Yield_',obj$Perf$Det$Codes)
obj$Perf$Det$Values[,,Yind]<-obj$Perf$Det$Values[,,Yind]/max(obj$Perf$Det$Values[,,Yind])*100

# Stochastic
Yind<-grepl('Yield_',obj$Perf$Stoch$Codes)
obj$Perf$Stoch$Values[,,,Yind]<-obj$Perf$Stoch$Values[,,,Yind]/max(obj$Perf$Stoch$Values[,,,Yind])*100

obj$Perf$Det$Labels[Yind] <- c("Mean Short-Term Yield",
                               "Mean Medium-Term Yield")
obj$Perf$Stoch$Labels[Yind] <- c("Mean Short-Term Yield",
                                 "Mean Medium-Term Yield")

obj$Perf$Det$Description[Yind] <- c("Mean Yield Relative to Highest Mean Yield (Years 1-10)",
                               "Mean Yield Relative to Highest Mean Yield (Years 11-30)")
obj$Perf$Stoch$Description[Yind] <- c("Mean Yield Relative to Highest Mean Yield (Years 1-10)",
                                "Mean Yield Relative to Highest Mean Yield (Years 11-30)")

library(RColorBrewer)
obj$Misc$Cols$MP <- RColorBrewer::brewer.pal(length(MPs), 'Paired')

saveRDS(obj,file.path(basedir,"NSWO.slick"))

