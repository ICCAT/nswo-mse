
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

fit.diagnostics <- function(df) {
  df$cor <- cor(df$Exp, df$Obs) # correlation between Observed and Expected
  df$res <- log(df$Obs) - log(df$Exp) # residuals - log transformed
  df$ac <- acf(df$res,plot=F)$acf[2,1,1]
  df$sd <- sd(df$res)
  df$runs <- retseq(df$res<0)
  df$mean.runs <- mean(df$runs)
  df$nyears <- length(df$year)
  df$rmse <- RMSE(df$Exp, df$Obs)/sd(df$Obs, na.rm=TRUE)
  df
}

cpue.plot <- function(cpue) {
  Fleets <- cpue$Name %>% unique()
  n.fleet <- length(Fleets)
  par(mfrow=c(n.fleet, 2), mar=c(2,2,1,1), oma=c(3,3,0,0))

  pch <- 16
  lwd <- 1.5
  col1 <- 'darkblue'
  colramp <- colorRampPalette(c("#3e3e40", "#707073", "#8d8d91",
                                'white',
                                '#6b6bed', '#3939ed', '#0505ed'))
  colors <- colramp(13)
  cpue$color <- cpue$runs
  cpue$color[cpue$color>6] <- 6
  cpue$color[cpue$res<0] <- -cpue$color[cpue$res<0]
  cpue$color <- cpue$color+7

  for (x in seq_along(Fleets)) {

    dt <- cpue %>% dplyr::filter(Name==Fleets[x])

    # plot observed and fitted index
    ylim <- c(dt$Exp, dt$Obs) %>% range()
    plot(dt$year, dt$Obs, type="p", pch=pch, ylim=ylim, bty="n", las=1, xlab="", ylab="")
    lines(dt$year, dt$Exp, lwd=lwd, col=col1)
    text(min(dt$year), max(ylim)*.95, dt$Name, xpd=NA, pos=4)

    # plot deviations
    ylim <- ceiling(max(abs(dt$res)*10))/10
    ylim <- c(-ylim, ylim)
    textdf <- dt %>% distinct(Name, sd, ac, cor, rmse, mean.runs)
    textdf$color <- 1
    textdf <- textdf %>% tidyr::gather("key", "value", 2:5)
    textdf$label <- paste(textdf$key, round(textdf$value,2), sep=" = ")
    textdf$x <- min(dt$year)
    textdf$y <- seq(max(ylim), 0.5*max(ylim), length.out = nrow(textdf))

    tt <- barplot(dt$res, col=colors[dt$color], border=FALSE, ylim=ylim)
    # for (x in 1:nrow(textdf)) {
    #   text(tt[1], textdf$y[x], textdf$label[x], xpd=NA, pos=4)
    # }
  }
}

recdevs.plot <- function(SSB_DF) {
  rec_diag <- SSB_DF %>% select(year, Obs=Obs_Rec, Exp=Exp_Rec_biasadj) %>%
    fit.diagnostics()

  par(mfrow=c(1,1))
  # plot deviations
  ylim <- ceiling(max(abs(rec_diag$res)*10))/10
  ylim <- c(-ylim, ylim)
  textdf <- rec_diag %>% distinct(sd, ac, cor, rmse, mean.runs)
  textdf$color <- 1
  textdf <- textdf %>% tidyr::gather("key", "value", 1:2)
  textdf$label <- paste(textdf$key, round(textdf$value,2), sep=" = ")
  textdf$x <- min(rec_diag$year)
  textdf$y <- seq(max(ylim), 0.9*max(ylim), length.out = nrow(textdf))

  pch <- 16
  lwd <- 1.5
  col1 <- 'darkblue'
  colramp <- colorRampPalette(c("#3e3e40", "#707073", "#8d8d91",
                                'white',
                                '#6b6bed', '#3939ed', '#0505ed'))
  colors <- colramp(13)
  rec_diag$color <- rec_diag$runs
  rec_diag$color[rec_diag$color>6] <- 6
  rec_diag$color[rec_diag$res<0] <- -rec_diag$color[rec_diag$res<0]
  rec_diag$color <- rec_diag$color+7

  tt <- barplot(rec_diag$res, col=colors[rec_diag$color], border=FALSE, ylim=ylim,
                axes=FALSE)
  for (x in 1:nrow(textdf)) {
    text(tt[1], textdf$y[x], textdf$label[x], xpd=NA, pos=4)
  }
  axis(side=2)
  mtext(side=2, "Log recruitment deviations", line=3)
  axis(side=1, labels=rec_diag$year[seq(1, length(tt), by=5)],
       at=tt[seq(1, length(tt), by=5)])

  mtext(side=1, "Year", line=3)
}
