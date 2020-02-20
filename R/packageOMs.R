root <- '../../../Google Drive/Projects/Projects_2020/ICCAT_Swordfish/OMs/SS/2018_GRID'

OMdir <- file.path(root, "grid")
dirs <- list.dirs(OMdir, recursive = FALSE)

# devtools::install_github("r4ss/r4ss", build_vignettes = TRUE, force=TRUE)
library(r4ss)

library(dplyr)
library(MSEtool)




# ---- Package all OMs as data ----
DFlist <- DataList <- RepList <- LHlist <- list()
for (i in seq_along(dirs)) {
  message(i, '/', length(dirs))
  replist <- suppressWarnings(r4ss::SS_output(dirs[i], verbose = FALSE, hidewarn = TRUE,
                                              printstats=FALSE))
  data <- r4ss::SS_readdat(file.path(dirs[i], 'data.ss_new'), version='3.24',
                           verbose = FALSE)
  # Natural mortality
  tt <- replist$M_at_age %>% filter(Year <= replist$endyr)
  M <- unique(tt[,4])
  M <- M[!is.na(M)]

  # sigma R
  sigmaR <- replist$sigma_R_in

  # steepness
  h <- replist$parameters[grepl("steep", rownames(replist$parameters)), ]$Value

  # cpue cv
  cpue_cv <- data$CPUE$se_log %>% unique()


  # ess
  L_ESS <- data$lencomp$Nsamp %>% unique()

  # llq
  txt <- strsplit(dirs[i], 'llq')[[1]][2]
  llq <- strsplit(txt, '_env')[[1]][1] %>% as.numeric()

  # env
  est_pars <- replist$estimated_non_dev_parameters
  nms <- rownames(est_pars)
  env <- FALSE
  ind <- which(grepl('Q_envlink', rownames(est_pars)))
  if (length(ind)>0) {
    flts_env <- gsub('Q_envlink_[[:digit:]]+_', '', nms[ind])
    env <- TRUE
  }

  # converge
  log_det_hessian <- replist$log_det_hessian
  if (log_det_hessian>0) conv <- TRUE
  if (log_det_hessian==-1) conv <- FALSE

  LHlist[[i]] <- list(replist$likelihoods_used %>% t() %>% data.frame(),
                      replist$likelihoods_by_fleet)

  DFlist[[i]] <- data.frame(M=M, sigmaR=sigmaR, h=h, cpue_cv=cpue_cv,
                   L_ESS=L_ESS, llq=llq, env=env, dir=basename(dirs[i]),
                   n=i, conv=conv)

  RepList[[i]] <- replist
  DataList[[i]] <- data

}

# Save objects ....
DF <- do.call('rbind', DFlist)

saveRDS(DF, 'OM_objects/OM_DF.rda')
saveRDS(LHlist, 'OM_objects/LHlist.rda')
saveRDS(DataList, 'OM_objects/DataList.rda')
saveRDS(RepList, 'OM_objects/RepList.rda')

head(DF)




tt <- DF %>% filter(M==0.3, h==0.6, cpue_cv==0.6, L_ESS==2, llq==1.01, sigmaR==0.2,
                    env==FALSE)
ns <- tt$n
like <- LHlist[[ns]][[1]]
like[1,]

# Fits to CPUE indices
replist <- RepList[[ns]]

library(ggplot2); library(tidyr)
fit.plot(replist$cpue)
cpue <- replist$cpue

object.size(replist)
length(replist)






DF %>% group_by(n) %>% summarise(l=length(n), dir=unique(dir)) %>% tail()

OM <- SS2OM(dirs[i])
OM@cpars$M_ageArray[1,,]



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
