
library(SWOMSE)
library(Slick)

basedir <- 'Slick_objects/SCRS_P_2022_051'
MSEdir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/Results'

Des <-read.csv(file.path(basedir, 'Design.csv'))

# --- Make the MSE list ---

# Reference OMs
Ref_OMs <- list.files(file.path(MSEdir, 'Reference'), pattern='.mmse')

# Robustness
Rob_OMs <- list.files(file.path(MSEdir, 'Robustness'), pattern='.mmse', recursive = TRUE)


MSElist<-list()
for (i in 1:nrow(Des))   MSElist[[i]]<-readRDS(file.path(MSEdir, paste0(Des$OM[i], '.mmse')))

MPs <- MSElist[[1]]@MPs[[1]]
MP_Desc <- MPs # TODO add description


Design = as.matrix(Des[,3:ncol(Des)])
for(i in 1:ncol(Design))Design[,i]<-match(Design[,i],unique(Design[,i]))

# --- Construct the SLICK object using the Make_SLICK function for MSEtool compatible MSEs ----

PMs<-c("Safety_S",
       'Safety_M',
       'Status_S',
       'Status_M',
       'Stability',
       'Yield_S',
       'Yield_M')


obj<-Make_Slick(name="SCRS/P/2022/051 Reference OMs",
                MPs=MPs,
                PMs=PMs,
                Design = Design,
                MP_Desc=MP_Desc,
                SN = list(
                  Factor_Labels=c("Natural Mortality","Steepness", "Robustness"),
                  Labels = list(c("M=0.1","M=0.2", 'M=0.3'),
                                c('h=0.6', 'h=0.75', 'h=0.9'),
                                c('Reference', 'Increasing q')),
                  Codes =  list(c("0.1","0.2", '0.3'),
                                c('0.6', '0.75', '0.9'),
                                c('Reference', '1% Increase q')
                                ),
                  Description = list(paste("Natural mortality rate of",c(0.1, 0.2, 0.3)),
                                     paste("Beverton-Holt stock recruitment steepness (resilience) of",c(0.6, 0.75, 0.9)),
                                     c('Reference Case', 'Assumed 1% Annual Increase in Historical Catchability')
                                     )
                ),
                MSElist=MSElist,
                fstYr=2021
)

# Standardize Yield

Yind<-grepl('Yield_M',obj$Perf$Det$Codes)

obj$Perf$Det$Values[,,Yind]<-obj$Perf$Det$Values[,,Yind]/max(obj$Perf$Det$Values[,,Yind])*100
obj$Perf$Stoch$Values[,,,Yind]<-obj$Perf$Stoch$Values[,,,Yind]/max(obj$Perf$Stoch$Values[,,,Yind])*100

obj$StateVar$TimeNow<-2017

obj$Misc$Author<-"Adrian Hordyk"
obj$Misc$Contact <-"adrian@bluematterscience.com"
obj$Misc$Date<-"September 2022"
obj$Misc$Institution <- "Blue Matter Science"


obj$Text$Title <- "North Atlantic Swordfish Demonstration MSE Results"
obj$Text$Sub_title <- "SP CMPs for Reference OMs"
obj$Text$Introduction[[1]]<-"These results are presented to the Swordfish Species Working Group in SCRS/P/2022/051."

obj$Text$Introduction[[2]]<-"Note that all performance metrics have been normalized to the range 0-100 across MPs and OMs and that
                               average annual variability in catches is the additive inverse (compliment) such that high variability is low (less good performance)."

obj$Text$Introduction[[3]]<-"The North Atlantic swordfish MSE is funded by the International Commission for the Conservation of Atlantic tunas. The results presented here do not
necessarily reflect the point of view of ICCAT or other funders and in no ways anticipate ICCAT future policy in this area."


obj$MP$Description<-MPs


saveRDS(obj,file.path(basedir,"NSWO.slick"))


