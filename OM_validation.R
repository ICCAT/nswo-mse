
OM_DF <- SWOMSE::OMs_DF
OM_DF$OM <- OM_DF$n
OM_DF <- OM_DF[,c(11, 1:10)]

dflist <- list()
for (i in seq_along(SWOMSE::Like_List)) {
  dflist[[i+1]] <- data.frame(OM=as.character(i), SWOMSE::Like_List[[i]][[1]][1,])
}
Like_DF <- do.call('rbind', dflist)
Like_DF$OM <- Like_DF$OM %>% as.character() %>% as.numeric()

DF <- left_join(OM_DF, Like_DF, by="OM") %>%
  dplyr::select(-dir, -Equil_catch, -Forecast_Recruitment, -Parm_devs, -Crash_Pen )


# Check parameters at bounds
# Bounds first
RepList <- readRDS("OM_objects/replist.rda")
nbound <- NA
templist <- list()
for (i in seq_along(RepList)) {
  tt <- RepList[[i]]$estimated_non_dev_parameters
  nbound[i] <- sum((tt$Value<0.99*tt$Min) | (tt$Value>0.99*tt$Max))
  if (nbound[i] > 0) {
    ind <- which((tt$Value<0.99*tt$Min) | (tt$Value>0.99*tt$Max))
    templist[[i]] <- data.frame(OM=i,
                                Parameter=rownames(tt[ind,]),
                                Value= tt$Value[ind],
                                Min=tt$Min[ind],
                                Max=tt$Max[ind])
  }
}
DF_bound <- do.call('rbind', templist)

DF <- left_join(DF, DF_bound, by="OM")

write.csv(DF, "OM_Converge.csv")





















usname <- Sys.getenv("USERNAME")
base.dir <- paste0('C:/Users/', usname, '/Dropbox/SWO_MSE/OMs/SS/2018_GRID/base_case')
grid.dir <- paste0('C:/Users/', usname, '/Dropbox/SWO_MSE/OMs/SS/2018_GRID/grid')
grid.dirs <- list.dirs(grid.dir, recursive = FALSE)

all.dirs <- c(base.dir, grid.dirs)

out <- list()
for (i in seq_along(all.dirs)) {
  ss.dir <- all.dirs[i]
  rep <- r4ss::SS_output(dir=all.dirs[i])

  log_det_hessian <-rep$log_det_hessian
  warn_msg <- rep$inputs$warn[4]
  bound_msg <- rep$inputs$warn[5]
  num <- strsplit(basename(ss.dir), split = "-", perl = TRUE)[[1]][1]

  tdf <- str_extract_all(basename(ss.dir),"\\(?[0-9,.]+\\)?")[[1]]
  tdf <- matrix(tdf, nrow=1, ncol=9) %>% data.frame()
  colnames(tdf) <- c("Num", 'M', "sigmaR", 'steepness', 'cpuecv', 'ess', 'llq', 'env', 'num2')
  tdf <- tdf %>% select('M', "sigmaR", 'steepness', 'cpuecv', 'ess', 'llq', 'env')
  # tdf$env <- NULL

  current_depletion <- rep$current_depletion
  SBzero <- rep$SBzero
  maximum_gradient_component <- rep$maximum_gradient_component
  RunTime <- rep$RunTime

  est <- rep$estimated_non_dev_parameters
  # est <- est[est$Status !='OK',]
  # est <- est %>% select(Value, Min, Max, Init, Status)
  est$Parameter <- rownames(est)

  DF <- data.frame(OM=num, log_det_hessian, maximum_gradient_component,tdf, current_depletion, SBzero, RunTime)


  if(nrow(est)>0) {
    DF <- data.frame(DF, est)
    rownames(DF) <- NULL
  } else {
    DF <- data.frame(DF, Value=NA, Min=NA, Max=NA, Init=NA, Status=NA, Parameter=NA)
  }

  out[[i]] <- DF
}


DF <- do.call('rbind', out)
write.csv(DF, "OM_Converge.csv")




SSList <- list()
Converge <- rep(TRUE, length(all.dirs))
for (i in seq_along(all.dirs)) {
  SSList[[i]] <- r4ss::SS_output(dir=all.dirs[i])
  Converge[i] <- SSList[[i]]$log_det_hessian != -1
}


sum(Converge)
sum(!Converge)

name <- basename(all.dirs)
num <- sapply(seq_along(all.dirs), function(i)
  strsplit(name[i], split = "-", perl = TRUE)[[1]][1])
ord <- num[2:length(num)] %>% as.numeric() %>% order()
ord <- c(1, ord+1)

DF <- str_extract_all(name,"\\(?[0-9,.]+\\)?")
DF <- do.call('rbind', DF)
colnames(DF) <- c("Num", 'M', "sigmaR", 'steepness', 'cpuecv', 'ess', 'llq', 'env', 'num2')

DF <- apply(DF, 2, as.numeric) %>% as.data.frame()
DF$Converge <- Converge[2:length(Converge)]
DF <- DF %>% arrange(Num)

write.csv(DF, "OM_Converge.csv")

DF2 <- tidyr::gather(DF, "key", "value", 2:7)
pout <- ggplot(DF2, aes(x=as.factor(value), fill=Converge)) +
  facet_wrap(~key, scales="free") + geom_bar() +
  theme_classic() + theme(strip.background = element_blank()) +
  labs(x="Factor", y="Count")

ggsave("Figures/Converge.png", pout, width=9, height = 5)


# Bounds first
nbound <- NA
for (i in seq_along(all.dirs)) {
  tt <- SSList[[i]]$estimated_non_dev_parameters
  nbound[i] <- sum((tt$Value<0.99*tt$Min) | (tt$Value>0.99*tt$Max))
}

nbound <- nbound[ord]



df <- data.frame(Number=num[ord], Name=name, Converge, nbound)


df2 <- df %>% filter(nbound>0 & Converge)
dim(df2)
grp <- rep(1:4, each=45)
df2$Group <- grp[1:nrow(df2)]
df2$Number <- factor(df2$Number, levels=df2$Number, ordered = TRUE)

table(df2$nbound)

pout <- ggplot(df2, aes(x=Number, y=nbound)) + geom_bar(stat='identity') +
  facet_wrap(~Group, scales="free_x") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  labs(x="OM Number", y="N parameters at bounds")

ggsave("Figures/Nbounds.png", pout, width=10, height = 6)


df3 <- df %>% filter(nbound<=1 & Converge)

ind <- match(df3$Name, basename(all.dirs))



retseq<-function(res){
  nres<-length(res)
  str<-rep(1,nres)
  i<-1
  going<-TRUE
  while(going){
    same=TRUE
    j<-0
    while(same){
      if(res[i]!=res[i+j+1]|(j+i+1)==(nres+1)){
        same=FALSE
        str[i:(i+j)]<-j+1
      }else{
        j<-j+1
      }
    }
    if(i+j+1>nres)going=FALSE
    i<-i+j+1
  }
  str
}

RMSE <- function(m, o){
  sqrt(mean((m - o)^2))
}

fit.diag <- function(cpue) {
  Fleets <- cpue$Fleet_name %>% unique()
  outlist <- list()
  for (i in seq_along(Fleets)) {
    dt <- cpue %>% filter(Fleet_name == Fleets[i])
    dt$cor <- cor(dt$Exp, dt$Obs)
    dt$ac <- acf(dt$Dev,plot=F)$acf[2,1,1]
    dt$sd <- round(sd(dt$Dev),2)
    dt$runs <- retseq(dt$Dev<0)
    dt$mean.runs <- mean(dt$runs)
    dt$nyears <- length(dt$Yr)
    dt$color <- dt$runs
    dt$color[dt$color>6] <- 6
    dt$color[dt$Dev<0] <- -dt$color[dt$Dev<0]

    rmse <- RMSE(dt$Exp, dt$Obs)
    dt$rmse <- rmse/sd(dt$Obs, na.rm=TRUE)
    dt <- dt %>% select(Fleet_name, Yr, Vuln_bio,
                        Obs, Exp, Calc_Q, SE, Dev, Like,
                        rmse,
                        cor, ac, sd, runs, mean.runs, nyears, color)

    outlist[[i]] <- dt
  }
  do.call("rbind", outlist)
}



fit.plot <- function(cpue, figname=NULL) {
  fit.diags <- fit.diag(cpue)
  Fleets <- cpue$Fleet_name %>% unique()
  plist <- list()
  for (i in seq_along(Fleets)) {
    dt <- filter(fit.diags, Fleet_name==Fleets[i])


    p1 <- ggplot(dt, aes(x=Yr, y=Obs)) + geom_point() +
      geom_line(aes(y=Exp), color='blue') +
      theme_classic() +
      labs(x="Year", y="Index", title=unique(dt$Fleet_name))


    ylim <- ceiling(max(abs(dt$Dev)*10))/10
    ylim <- c(-ylim, ylim)

    textdf <- dt %>% distinct(Fleet_name, sd, ac, cor, rmse, mean.runs)
    textdf$color <- 1

    textdf <- textdf %>% gather("key", "value", 2:5)
    textdf$label <- paste(textdf$key, round(textdf$value,2), sep=" = ")
    textdf$x <- min(dt$Yr)
    textdf$y <- seq(max(ylim), 0.5*max(ylim), length.out = nrow(textdf))

    colramp <- colorRampPalette(c("#3e3e40", "#707073", "#8d8d91",
                                  'white',
                                  '#6b6bed', '#3939ed', '#0505ed'))
    p2 <- ggplot(dt, aes(x=Yr, y=Dev, fill=color, color=color)) + geom_bar(stat='identity') +
      scale_fill_gradientn(colours=colramp(12)) +
      scale_color_gradientn(colours=colramp(12)) +
      guides(fill=FALSE, color=FALSE) +
      theme_classic() +
      expand_limits(y=ylim) +
      labs(x='Year', y="Deviation") +
      geom_text(data=textdf, aes(x=x, y=y, label=label), color="black",
                hjust = 0)

    plist[[i]] <- cowplot::plot_grid(p1, p2, ncol=2)

  }

  Plist1 <- plist[1:9]
  Plist2 <- plist[10:14]
  pout1 <- cowplot::plot_grid(plotlist=Plist1,
                             nrow=5,
                             ncol=2)
  pout2 <- cowplot::plot_grid(plotlist=Plist2,
                              nrow=3,
                              ncol=2)
  # pout
  nm1 <- paste0("Figures/Indices/", figname, "_fl.png")
  nm2 <- paste0("Figures/Indices/", figname, "_age.png")
  ggsave(nm1, pout1, width=12, height=12)
  ggsave(nm2, pout2, width=12, height=9)

}

# Only SS OMs that Converge and <=1 parameter at bound
SSList2 <- SSList[ind]

# Calculate Stats
saveList <- list()
for (i in seq_along(ind)) {
  SSdat <- SSList2[[i]]

  DF <- fit.diag(SSdat$cpue) %>% distinct(Fleet_name,
                                          cor, ac,sd, rmse,
                                          mean.runs, nyears)
  DF$i <- i

  DF$num <- strsplit(basename(all.dirs[ind])[i], split = "-", perl = TRUE)[[1]][1]
  saveList[[i]] <- DF
  fit.plot(SSdat$cpue, figname=unique(DF$num))
}

DFTab <- do.call("rbind", saveList)

DFTab %>% group_by(Fleet_name) %>% summarize(mean=mean(rmse), sd=sd(rmse))

DFTab %>% filter(Fleet_name == "SPN_1") %>% summarize(min=min(rmse), max=max(rmse))

age3ind <- DFTab %>% filter(Fleet_name == "Age-3") %>% select(rmse, num)

age3ind %>% filter(rmse==min(rmse))
age3ind %>% filter(rmse==median(rmse))
age3ind %>% filter(rmse==max(rmse))



meanRMSE <- DFTab %>% group_by(num) %>% summarize(Mrmse=mean(rmse))

meanRMSE %>% arrange(Mrmse)
meanRMSE %>% arrange(desc(Mrmse))


DFTab %>% filter(num=='base_case')

