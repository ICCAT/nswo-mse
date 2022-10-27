
library(SWOMSE)

# install Slick package
# devtools::install_github('blue-matter/Slick')

library(Slick)

basedir <- 'G:/Shared drives/BM shared/1. Projects/SLICK/Case Studies/SWO'
MSEdir <- file.path(basedir,'MSEs')

Des<-read.csv(file.path(basedir, 'Design.csv'))

MPs <- c('AvC', 'Itarget1', 'Itarget3', 'ITM', 'Islope1', 'Islope3')

# --- Make the MSE list ---

MSElist<-list()

for (i in 1:nrow(Des))   MSElist[[i]]<-readRDS(file.path(MSEdir, paste0(Des$OM[i], '.rda')))

Design = as.matrix(Des[,3:ncol(Des)])
for(i in 1:ncol(Design))Design[,i]<-match(Design[,i],unique(Design[,i]))

# --- Construct the SLICK object using the Make_SLICK function for MSEtool compatible MSEs ----

PMs<-c("Safety_M", "AAVY", "Status_M", "Yield_M")

obj<-Make_Slick(name="Atlantic swordfish example MSE results",
                MPs=MPs,
                PMs=PMs,
                Design = Design,
                SN = list(
                  Factor_Labels=c("Natural Mortality","Recruitment Variability","Resilience","Index Fishing Efficiency Adjustment"),
                  Labels = list(c("M=0.1","M=0.3"), c("sigR=0.2", "sigR=0.6"), c("h=0.6","h=0.9"), c("FE=0","FE=1")),
                  Codes =  list(c("M1","M3"),          c("sigR2", "sigR6"),       c("h60","h90"),        c("FE0","FE1")),
                  Description = list(paste("Natural mortality rate of",c(0.1,0.3)),
                                     paste("Standard deviation in log-normal annual recruitment of",c(0.2, 0.6)),
                                     paste("Beverton-Holt stock recruitment steepness (resilience) of",c(0.6, 0.9)),
                                     paste("Annual fishing efficiency gains added to indices used for fitting of ",c(0, 1),"percent"))
                ),
                MSElist=MSElist
)

# Standardize Yield

Yind<-grepl('Yield_M',obj$Perf$Det$Codes)

obj$Perf$Det$Values[,,Yind]<-obj$Perf$Det$Values[,,Yind]/max(obj$Perf$Det$Values[,,Yind])*100
obj$Perf$Stoch$Values[,,,Yind]<-obj$Perf$Stoch$Values[,,,Yind]/max(obj$Perf$Stoch$Values[,,,Yind])*100

obj$StateVar$TimeNow<-2017

obj$Misc$Author<-"Adrian Hordyk"
obj$Misc$Contact <-"adrian@bluematterscience.com"
obj$Misc$Date<-"March 2022"
obj$Misc$Institution <- "Blue Matter Science"


obj$Text$Title <- "North Atlantic Swordfish Demonstration MSE Results"
obj$Text$Sub_title <- "PRELIMINARY 2022 RESULTS (DEMO ONLY)"
obj$Text$Introduction[[1]]<-"This results are for demonstration purposes and have been codified in SLICK as a placeholder for the full swordfish MSE results. Development of
operating models, candidate management procedures and formalization of performance metrics are all still under way and have yet to be finalized."

obj$Text$Introduction[[2]]<-"Note that all performance metrics have been normalized to the range 0-100 across MPs and OMs and that
                               average annual variability in catches is the additive inverse (compliment) such that high variability is low (less good performance)."

obj$Text$Introduction[[3]]<-"The North Atlantic swordfish MSE is funded by the International Commission for the Conservation of Atlantic tunas. The results presented here do not
necessarily reflect the point of view of ICCAT or other funders and in no ways anticipate ICCAT future policy in this area."


obj$MP$Description<-c("The TAC in all projection years is the average historical catch",
                      "A more aggressive MP that incrementally changes the TAC to reach a target level of the abundance index",
                      "A less aggressive MP that incrementally changes the TAC to reach a target level of the abundance index",
                      "An index target MP like Itarget but using average index levels over recent years (where 'recent' is defined by the natural mortality rate)",
                      "A more aggressive MP that incrementally changes the TAC to achieve a stable index of relative abundance",
                      "A less aggressive MP that incrementally changes the TAC to achieve a stable index of relative abundance")


saveRDS(obj,"NSWO.slick")


