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

