library(tidyverse)
library(glmmTMB)
library(car)
library(emmeans)


load("G:/Shared drives/BM shared/1. Projects/ICCAT NSWO/2025/ECP/NATL_7Fleets.RData")

unique(NATL_7Fleets$FlagName) |> sort()

#ensure data types are correct
NATL_7Fleets$YearC=as.factor(NATL_7Fleets$YearC)
NATL_7Fleets$Quarter=as.factor(NATL_7Fleets$Quarter)
NATL_7Fleets$FlagName=as.factor(NATL_7Fleets$FlagName)
NATL_7Fleets$AssignedClass=as.factor(NATL_7Fleets$AssignedClass)
NATL_7Fleets$zone=as.factor(NATL_7Fleets$zone)
NATL_7Fleets$fTarget2=as.factor(NATL_7Fleets$fTarget2)

GenerateIndex <- function(IndexData, DropFlag) {

  Flags <- unique(IndexData$FlagName)
  KeepFlags <- Flags[!Flags %in% DropFlag]

  IndexData <- IndexData |> dplyr::filter(FlagName %in% KeepFlags)

  #Calculate nominal index
  nom<- IndexData%>%
    dplyr::group_by(YearC)%>%
    dplyr::summarise(Nominal=mean(CPUE), NObs=length(CPUE))
  nom = data.table::data.table(nom)
  nom[,YearC := as.numeric(as.character(YearC))]


  # ggplot(nom, aes(x=Year, y=Nominal)) +
  #   theme_bw() + ylab("Relative Abundance") +
  #   geom_line(data=nom,aes(x=YearC, y=Nominal), linewidth=1)

  #Final tweedie model
  mod20= glmmTMB(CPUE~ YearC + Quarter + FlagName + zone + fTarget2 + AssignedClass, data= IndexData,
                 family = tweedie(link = "log"), control = glmmTMBControl(optCtrl = list(iter.max=1e5,eval.max=1e3)))

  #model summary, deviance etc.
  mod20summary<-summary(mod20)
  mod20anova<-glmmTMB:::Anova.glmmTMB(mod20)

  #Predict by Year effect. ls means - using type = "response" as that puts it back in the right scale
  ls.Mod20 <- emmeans(mod20, "YearC", type = "response", rg.limit=440000)

  #make a quick plot
  # p<-plot(ls.Mod20)
  # p + coord_flip()

  ls.Mod20pred = as.data.frame(ls.Mod20)
  ls.Mod20pred$YearC<-as.numeric(as.character(ls.Mod20pred$YearC))
  ls.Mod20pred<-cbind(ls.Mod20pred, nom$Nominal, nom$NObs)
  ls.Mod20pred
}


Flags <- NATL_7Fleets$FlagName|> unique()

for (fl in seq_along(Flags)) {
  DropFlag <- Flags[fl]
  Index <- GenerateIndex(NATL_7Fleets,DropFlag)
  saveRDS(Index, file.path("ECP/Indices", paste0(DropFlag, '.rda')))
}


#
#
#
#
#
# #Calculate nominal index
# nom<- NATL_7Fleets%>%
#   dplyr::group_by(YearC)%>%
#   dplyr::summarise(Nominal=mean(CPUE), NObs=length(CPUE))
# nom = data.table::data.table(nom)
# nom[,YearC := as.numeric(as.character(YearC))]
#
#
# ggplot(nom, aes(x=Year, y=Nominal)) +
#   theme_bw() + ylab("Relative Abundance") +
#   geom_line(data=nom,aes(x=YearC, y=Nominal), linewidth=1)
#
# #Final tweedie model
# mod20= glmmTMB(CPUE~ YearC + Quarter + FlagName + zone + fTarget2 + AssignedClass, data= NATL_7Fleets,
#                family = tweedie(link = "log"), control = glmmTMBControl(optCtrl = list(iter.max=1e5,eval.max=1e3)))
#
# #model summary, deviance etc.
# mod20summary<-summary(mod20)
# mod20anova<-glmmTMB:::Anova.glmmTMB(mod20)
#
# #Predict by Year effect. ls means - using type = "response" as that puts it back in the right scale
# ls.Mod20 <- emmeans(mod20, "YearC", type = "response", rg.limit=440000)
#
# #make a quick plot
# p<-plot(ls.Mod20)
# p + coord_flip()
#
# ls.Mod20pred = as.data.frame(ls.Mod20)
# ls.Mod20pred$YearC<-as.numeric(as.character(ls.Mod20pred$YearC))
# ls.Mod20pred<-cbind(ls.Mod20pred, nom$Nominal, nom$NObs)
#
# ################################################################################
# library(SWOMSE)
#
#
# Hist <- readRDS("Hist_Objects/Reference/MOM_001.hist")
#
# dim(Hist$Female$`Fleet 1`@SampPars$Obs$Ierr_y)
#
# SWOMSE::MOM_001@cpars$Female[[1]]$Data@Ind
#
#
#
# ################################################################################
