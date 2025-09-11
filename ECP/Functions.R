library(SWOMSE)
library(tidyverse)
library(glmmTMB)
library(car)
library(emmeans)
library(patchwork)

GenerateIndex <- function(IndexData, DropFlag, DropYears=NULL) {

  #ensure data types are correct
  IndexData$YearC=as.factor(IndexData$YearC)
  IndexData$Quarter=as.factor(IndexData$Quarter)
  IndexData$FlagName=as.factor(IndexData$FlagName)
  IndexData$AssignedClass=as.factor(IndexData$AssignedClass)
  IndexData$zone=as.factor(IndexData$zone)
  IndexData$fTarget2=as.factor(IndexData$fTarget2)

  Flags <- unique(IndexData$FlagName)

  if (is.null(DropYears)) {
    KeepFlags <- Flags[!Flags %in% DropFlag]
    IndexData <- IndexData |> dplyr::filter(FlagName %in% KeepFlags)
  } else {
    ind <- which(IndexData$FlagName == DropFlag & IndexData$YearC%in%DropYears)
    IndexData <- IndexData[-ind, ]
  }

  #Calculate nominal index
  nom<- IndexData%>%
    dplyr::group_by(YearC)%>%
    dplyr::summarise(Nominal=mean(CPUE), NObs=length(CPUE))
  nom = data.table::data.table(nom)
  nom[,YearC := as.numeric(as.character(YearC))]


  # ggplot(nom, aes(x=Year, y=Nominal)) +
  #   theme_bw() + ylab("Relative Abundance") +
  #   geom_line(data=nom,aes(x=YearC, y=Nominal), linewidth=1)
  IndexData$YearC <- as.factor(IndexData$YearC)
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

# update obs error for projections
UpdateObsError <- function(Hist, Index, ind_yrs=1999:2022) {

  nyears <- length(SWOData@Ind[1,])
  proyears <- MOM_001@proyears
  nsim <- MOM_001@nsim

  yrs_index <- match(ind_yrs, SWOData@Year)

  B <- apply(Hist[[1]][[1]]@TSdata$Biomass[1,,] + Hist[[2]][[1]]@TSdata$Biomass[1,,], 1, sum)
  Index <- Index$response/mean(Index$response)

  Resids <- MSEtool:::Calc_Residuals(B[yrs_index], Index[yrs_index], 1)
  Stats <- MSEtool:::Calc_Stats(Resids$res)
  Stats <- data.frame(AC=replicate(nsim, Stats$AC),
                      SD=replicate(nsim, Stats$SD),
                      lst.err=replicate(nsim, Stats$lst.err)
  )

  Resid_Proj <- MSEtool:::Gen_Residuals(Stats, nsim, proyears)
  UpdateErrorYears <- 74:105 # starts in 2023
  UpdateErrorProjYears <- 1:32

  Hist$Female$`Fleet 1`@SampPars$Obs$Ierr_y[,UpdateErrorYears] <- Resid_Proj[,UpdateErrorProjYears]
  Hist$Male$`Fleet 1`@SampPars$Obs$Ierr_y[,UpdateErrorYears] <- Resid_Proj[,UpdateErrorProjYears]

  Hist
}

RunMSEs <- function(dropped, updateIndex=FALSE) {
  hist_objects <- list.files('Hist_Objects/Reference', full.names = TRUE)
  indices <- list.files(file.path('ECP/Indices', dropped), full.names = TRUE)

  for (i in seq_along(hist_objects)) {
    for (j in seq_along(indices)) {
      Index <- readRDS(indices[j])
      Hist <- readRDS(hist_objects[i])

      Hist <- UpdateObsError(Hist, Index)

      if (updateIndex) {
        NewIndex <- Index$response/mean(Index$response)
        n <- SWOData@Year |> length()
        NewIndex <- c(rep(NA, n - length(NewIndex)), NewIndex)
        nsim <- nrow(Hist$Female$`Fleet 1`@Data@Ind)
        Hist$Female$`Fleet 1`@Data@Ind[,] <- matrix(NewIndex, nrow=nsim, ncol=n, byrow=TRUE)
      }

      MSE <- ProjectMOM(Hist, 'MCC11_b')
      om_name <- gsub('.hist', '', basename(hist_objects[i]))

      nm <- gsub(".rda", "", basename(indices[j]))
      nm <- paste0(om_name, '_Dropped_',nm, '.rda')
      name <- file.path('ECP/MSE_Objects', dropped, nm)
      saveRDS(MSE, name)
    }
  }
}


MakeIndicesDF <- function(dropped, name='') {
  Dropped_Indices_Files <- list.files(file.path('ECP/Indices/', dropped),full.names = TRUE)
  IndexList <- list()
  for (i in seq_along(Dropped_Indices_Files)) {
    index <- readRDS(Dropped_Indices_Files[i])
    DroppedFlag <- basename(Dropped_Indices_Files[i])
    DroppedFlag <- gsub('.rda', '', DroppedFlag)
    Years <- index$YearC
    YrInd <- match(Years, SWOData@Year)
    IndexList[[i]] <- data.frame(Year=index$YearC,
                                 Index=index$response/mean(index$response, na.rm=TRUE),
                                 CombinedIndex=SWOData@Ind[1,YrInd],
                                 DroppedFlag=DroppedFlag,
                                 Run=name)
  }
  do.call('rbind', IndexList)

}

GetMSEObjects <- function(run, DroppedFlag=NULL) {
  if (run=='Base') {
    MSE_Files <- list.files('ECP/MSE_Objects', full.names = TRUE, recursive = FALSE)
    MSE_Files <- MSE_Files[!grepl('Dropped', MSE_Files)]
    MSE_Files <- MSE_Files[!grepl('DropYears', MSE_Files)]
  } else {
    MSE_Files <- list.files(file.path('ECP/MSE_Objects', run), full.names = TRUE, recursive = FALSE)
    MSE_Files <- MSE_Files[grepl(paste0('Dropped_', DroppedFlag), MSE_Files)]
  }

  MSEList <- list()
  for (j in seq_along(MSE_Files)) {
    MSEList[[j]] <- readRDS(MSE_Files[j])
  }
  if (run=='Base') {
    return(combine_MMSE(MSEList, 'Base'))
  }

  combine_MMSE(MSEList, paste0('Dropped_', DroppedFlag))
}

AvCatch <- function(MMSEobj=NULL, Ref=NULL, Yrs=c(7,32)) {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- ''
  PMobj@Caption <- ''

  Stat_y <- apply(MMSEobj@Catch[,,,,Yrs[1]:Yrs[2], drop=FALSE], c(1,4,5), sum)

  PMobj@Stat <- apply(Stat_y, c(1,2), median)
  PMobj@Ref <- 1
  PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj) # no probability to calculate

  PMobj@Mean <- apply(Stat_y, 2, median)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(AvCatch) <- 'PM'

MakePlotDF <- function(MSEList, MSE_Base, Run) {
  Years <- seq(2025, by=1, length.out=30)
  purrr::map2(MSEList, names(MSEList), \(MSE, DroppedFlag) {

    Catch <- MSE@Catch[,,1,1,3:32] |> apply(c(1,3), sum)
    CatchRef <- MSE_Base@Catch[,,1,1,3:32] |> apply(c(1,3), sum)
    Catch <- (Catch/CatchRef) |> apply(2, mean)

    SB_SBMSY <- apply(MSE@SB_SBMSY[,1,1,3:32]/MSE_Base@SB_SBMSY[,1,1,3:32],2, mean)
    F_FMSY <-  apply(MSE@F_FMSY[,1,1,1,3:32]/MSE_Base@F_FMSY[,1,1,1,3:32],2, mean)

    data.frame(Year=Years,
               SB_SBMSY=SB_SBMSY,
               F_FMSY=F_FMSY,
               Catch=Catch,
               Dropped=DroppedFlag,
               Run=Run)

  }) |> dplyr::bind_rows()

}
