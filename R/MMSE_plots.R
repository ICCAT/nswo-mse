
# Helper functions -----
get_OM_details <- function(MMSE) {
  name <- MMSE@Name
  vals <- strsplit(name,' ')[[1]]
  vals <- as.numeric(unlist(regmatches(vals, gregexpr("[[:digit:]]+\\.*[[:digit:]]*",vals))))
  vals <- matrix(vals, nrow=nrow(OM_DF[,1:5]), ncol=5, byrow=TRUE)
  ind <- which(apply(OM_DF[,1:5] - vals, 1, sum) == 0 )
  OM_DF[ind,]
}

#' Combine multiple MMSE objects together
#'
#' @param MMSElist A list of objects of class `MMSE`
#' @param name The name for the `MMSE` object that is returned
#'
#' @return An object of class `MMSE`
#' @export
#'
combine_MMSE <- function(MMSElist, name) {
  classess <- lapply(MMSElist, class) %>% unlist()
  if (!all(classess %in% 'MMSE'))
    stop('MMSElist must be a list of `MMSE` objects')

  MMSE_out <- MMSElist[[1]]
  MMSE_out@nsim <- lapply(MMSElist, slot, 'nsim') %>% unlist() %>% sum()

  slots <- c('SB_SBMSY', 'F_FMSY', 'N', 'B', 'SSB', 'VB', 'Catch', 'Removals', 'TAC')
  for (sl in slots) {
    vals <- lapply(MMSElist, slot, sl)
    slot(MMSE_out, sl) <- do.call(abind:::abind, c(vals, along = 1))
  }
  MMSE_out@Name <- name
  MMSE_out
}


get_TS_results <- function(MMSE, sl) {

  # DF <- get_OM_details(MMSE)
  # rownames(DF) <- NULL
  MPs <- MMSE@MPs[[1]]
  nMPs <- length(MPs)
  nsim <- MMSE@nsim

  vals <- slot(MMSE, sl)

  # process vals
  if (sl %in% c('SB_SBMSY')) {
    vals <- vals[,1,,] # female
  }
  if (sl %in% c('F_FMSY')) {
    vals <- vals[,1,1,,] # female
  }

  if (sl %in% c('Catch', 'Removals')) {
    vals <- apply(vals, c(1,4,5), sum)
  }

  fst.proj.yr <- MMSE@PPD[[1]][[1]][[1]]@LHYear+1
  lst.proj.yr <- fst.proj.yr+MMSE@proyears-1
  Years <- fst.proj.yr:lst.proj.yr
  nyears <- length(Years)

  data.frame(Sim=1:nsim,
             Year=rep(Years, each=nsim*nMPs),
             MP=rep(MPs,each=nsim),
             Value=as.vector(vals),
             Name=sl)
}

# Projection plots ----

#' Projection plots
#'
#' @param MMSE An object of class `MMSE`
#' @param sl Slot name. Either: `SB_SBMSY`, `F_FMSY`, `Catch`, or `Removals`
#' @param quants Quantiles to plot
#'
#' @return A `ggplot2` object
#' @export
#'
Proj_plot <- function(MMSE, sl='SB_SBMSY', quants=c(0.05, 0.95), MPs=NULL,
                      ncol=NULL) {
  TS_DF <- get_TS_results(MMSE, sl)
  # across OMs
  TS_DF2 <- TS_DF %>%
    dplyr::group_by(MP, Name, Year) %>%
    dplyr::summarise(Median=median(Value),
                     Upper=quantile(Value, quants[2]),
                     Lower=quantile(Value, quants[1]),
                     .groups = 'drop')

  if (!is.null(MPs)) {
    TS_DF2 <- TS_DF2 %>% filter(MP %in% MPs)
  }

  # plot
  ylab <- switch(sl,
                 'SB_SBMSY'=expression(SB/SB[MSY]),
                 'F_FMSY'=expression(F/F[MSY]),
                 'Catch'='Catch (ton)')
  p <- ggplot(TS_DF2, aes(x=Year)) +
    facet_wrap(~MP, ncol=ncol) +
    expand_limits(y=0) +
    geom_ribbon(aes(ymin=Lower, ymax=Upper, fill=MP), alpha=0.6) +
    geom_line(aes(y=Median, color=MP)) +
    theme_bw() +
    labs(y=ylab) +
    guides(fill='none', color='none')

  if (sl =='SB_SBMSY') {
    p <- p +
      geom_hline(yintercept = 1, linetype=2) +
      geom_hline(yintercept = 0.4, linetype=3)
  }
  if (sl =='F_FMSY') {
    p <- p +
      geom_hline(yintercept = 1, linetype=2)
  }
  p
}



#' Create Performance Dataframe
#'
#' Creates a dataframe with F/FMSY, SB/SBMSY, TAC, and Catch for each simulation, year, and MP
#'
#' @param MSE An object of class `MMSE`
#'
#' @return A dataframe
#' @export
MakePerformanceDF <- function(MSE) {
  all.yrs <- c(MSE@PPD[[1]][[1]][[1]]@Year, max(MSE@PPD[[1]][[1]][[1]]@Year)+1)
  p.yrs <- all.yrs[all.yrs>2020]
  yr.ind <- which(p.yrs %in% 2024:2053)
  years <- p.yrs[yr.ind]
  nyears <- length(years)

  YearDF <- data.frame(ind=1:33, Year=2021:2053, ind2=-2:30)

  mps <- MSE@MPs[[1]]
  nMPs <- length(mps)
  results_list <- list()
  for (i in 1:nMPs) {
    mp <- i
    F_FMSY <- MSE@F_FMSY[,1,1,mp,yr.ind]
    B_BMSY <- MSE@SB_SBMSY[,1,mp,yr.ind]
    TAC  <- MSE@TAC[,1,1,mp,yr.ind] + MSE@TAC[,2,1,mp,yr.ind]
    Catch <- MSE@Catch[,1,1,mp,yr.ind] + MSE@Catch[,2,1,mp,yr.ind]

    nsim <- MSE@nsim
    DF <- data.frame(Sim=1:nsim,
                     Year=rep(years, each=nsim),
                     F_FMSY=as.vector(F_FMSY),
                     SB_SBMSY=as.vector(B_BMSY),
                     TAC=as.vector(TAC),
                     Catch=as.vector(Catch),
                     MP=mps[i])

    DF <- DF %>% group_by(Sim, MP) %>%
      mutate(int=1:length(unique(Year)),
             interval=firstChange(TAC))

    DF$Sim <- as.factor(DF$Sim)
    DF$GK <- DF$F_FMSY<=1 & DF$SB_SBMSY>=1
    DF$GK <- factor(DF$GK, levels=c(TRUE, FALSE), ordered = TRUE)
    DF$OF <- DF$F_FMSY>1
    DF$OF <- factor(DF$OF, levels=c(TRUE, FALSE), ordered = TRUE)
    DF$LRP <- DF$SB_SBMSY<0.4
    DF$LRP <- factor(DF$LRP, levels=c(TRUE, FALSE), ordered = TRUE)

    DF$Year <- lubridate::as_date(paste(DF$Year, '01', '01', sep='-'))
    results_list[[i]] <- DF
  }
  do.call('rbind', results_list) %>% ungroup()
}


#' Plot time-series of Projections
#' @param MMSE An object of class `MMSE`
#' @param year_range Numeric calendar years
#' @param mp Optional. Names of the MPs to include
#' @param ptsize Size of points
#' @return A `ggplot` object
#' @name TSplots
NULL


#' @param fill Green Kobe 'GK' or Overfishing 'OF'. Anything else is ignored.
#' @describeIn TSplots Plot time-series of F/FMSY
#' @export
F_FMSY_TS <- function(MMSE, year_range=NULL, mp=NA, fill='GK', ptsize=2, ncol=3) {
  DF <- MakePerformanceDF(MMSE)
  DF$F_FMSY[DF$F_FMSY>2] <- 2
  breaks.vec <- seq(min(DF$Year), max(DF$Year), by = "2 years")

  if (!all(is.na(mp))) {
    DF <- DF %>% filter(MP %in% mp)
  }

  Years <- lubridate::year(DF$Year)

  DF2 <- DF
  if (!is.null(year_range)) {
    DF2 <- DF %>% ungroup() %>% filter(Years %in% year_range)
  }

  p <- ggplot(DF, aes(x=Year, color=Sim, y=F_FMSY)) +
    facet_grid(~MP) +
    expand_limits(y=c(0,2)) +
    geom_hline(yintercept = 1, color='darkgray', linetype=2) +
    geom_line() +
    theme_bw() +
    scale_x_date(date_labels="%Y", breaks=breaks.vec)+
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    guides(color='none') +
    labs(y=expression(F/F[MSY]))

  if (fill=='GK') {
    fill_lab <- 'Green Kobe'
    p <- p + geom_point(data=DF2, size=ptsize, shape = 21, aes(fill=GK)) +
      scale_fill_manual(values=c('green', 'red'), drop=FALSE) +
      labs(fill='Green Kobe', y=expression(F/F[MSY]))

  }

  if (fill=='OF') {
    fill_lab <- 'Overfishing'
    p <- p + geom_point(data=DF2, size=ptsize, shape = 21, aes(fill=OF))+
      scale_fill_manual(values=c('red', 'green'), drop=FALSE) +
      labs(fill='Overfishing', y=expression(F/F[MSY]))
  }

  p + coord_cartesian(clip = 'off')
}


#' @param fill Green Kobe 'GK' or Limit Reference Point 'LRP'. Anything else is ignored.
#' @describeIn TSplots Plot time-series of SB/SBMSY
#' @export
SB_SBMSY_TS <- function(MMSE, year_range=NULL, mp=NA, fill='GK', ref=1, ptsize=2, ncol=3) {
  DF <- MakePerformanceDF(MMSE)
  DF$SB_SBMSY[DF$SB_SBMSY>2] <- 2
  breaks.vec <- seq(min(DF$Year), max(DF$Year), by = "2 years")

  if (!all(is.na(mp))) {
    DF <- DF %>% filter(MP %in% mp)
  }

  Years <- lubridate::year(DF$Year)

  DF2 <- DF
  if (!is.null(year_range)) {
    DF2 <- DF %>% ungroup() %>% filter(Years %in% year_range)
  }

  if (fill=='LRP') {
    DF2 <- DF2 %>% group_by(Sim) %>% mutate(LRP_line=ifelse(any(LRP==TRUE), TRUE, FALSE)) %>% ungroup()
    DF2$LRP_line <- factor(DF2$LRP_line, levels=c(TRUE, FALSE), ordered = TRUE)
  }

  p <- ggplot(DF, aes(x=Year, y=SB_SBMSY)) +
    facet_wrap(~MP, ncol=ncol) +
    expand_limits(y=c(0,2)) +
    geom_hline(yintercept = ref, color='darkgray', linetype=2) +
    theme_bw()  +
    scale_x_date(date_labels="%Y", breaks=breaks.vec)+
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    guides(color='none') +
    geom_line(aes(group=Sim))
  if (fill!='LRP') {
    p <- p + geom_line(aes(color=Sim))
  }

  if (fill=='GK') {
    p <- p + geom_point(data=DF2, size=ptsize, shape = 21, aes(fill=GK)) +
      labs(fill='Green Kobe', y=expression(SB/SB[MSY]))+
      scale_fill_manual(values=c('green', 'red'), drop=FALSE)
  }
  if (fill=='LRP') {
    p <- p + geom_point(data=DF2, size=ptsize, shape = 21, aes(fill=LRP, group=Sim)) +
      geom_line(data=DF2, aes(color=LRP_line,  group=Sim)) +
      labs(fill='Below LRP', y=expression(SB/SB[MSY])) +
      scale_fill_manual(values=c('red', 'green'), drop=FALSE) +
      scale_color_manual(values=c('red', 'green'), drop=FALSE)
  }
  p + coord_cartesian(clip = 'off')


}


#' @param fill Green Kobe 'GK', Limit Reference Point 'LRP', of Overfishing 'OF'
#' @describeIn TSplots Kobe plot
#' @export
Kobe <- function(MMSE, year_range=NULL, mp=NA, fill='GK', ptsize=2,
                 ncol=3, alpha=0.1) {
  DF <- MakePerformanceDF(MMSE)
  kobe_df <- bind_rows(
    data.frame(x=c(0,0, 1, 1), y=c(0,1, 1,0), fill='bl'),
    data.frame(x=c(1,1, 2, 2), y=c(0,1, 1,0), fill='br'),
    data.frame(x=c(0,0, 1, 1), y=c(1,2, 2,1), fill='tl'),
    data.frame(x=c(1,1, 2, 2), y=c(1,2, 2,1), fill='tr'))
  kobe_df$alpha <- 0.3

  mps <- DF$MP %>% unique()
  if (!all(is.na(mp))) {
    DF <- DF %>% filter(MP %in% mp)
  }

  Years <- lubridate::year(DF$Year)

  df <- DF
  if (!is.null(year_range)) {
    df <- DF %>% filter(Years %in% year_range)
  }

  df$SB_SBMSY[df$SB_SBMSY>2] <- 2
  df$F_FMSY[df$F_FMSY>2] <- 2

  df <- df %>% mutate(OF=ifelse(F_FMSY<=1, FALSE, TRUE),
                      OFD=ifelse(SB_SBMSY<=1, TRUE, FALSE)) %>%
    group_by(MP) %>% mutate(n=n())

  n_points <- unique(df$n)

  valdf <- df %>% group_by(MP) %>%
    summarise(BL=sum(OF==FALSE & OFD==TRUE)/n_points*100,
              BR=sum(OF==FALSE & OFD==FALSE)/n_points*100,
              TL=sum(OF==TRUE & OFD==TRUE)/n_points*100,
              TR=sum(OF==TRUE & OFD==FALSE)/n_points*100)

  valdf <- valdf %>% tidyr::pivot_longer(., cols=2:5)

  valdf$x <- -Inf
  valdf$y <- -Inf
  valdf$y[valdf$name=='TL'] <- Inf
  valdf$y[valdf$name=='TR'] <- Inf
  valdf$x[valdf$name=='BR'] <- Inf
  valdf$x[valdf$name=='TR'] <- Inf

  valdf$value  <- round(valdf$value, 2)
  valdf$value <- paste0(valdf$value, '%')

  valdf$hjustvar <- -2
  valdf$vjustvar <- -2

  valdf$hjustvar[valdf$name=='TL'] <- -1
  valdf$hjustvar[valdf$name=='TR'] <- 2
  valdf$hjustvar[valdf$name=='BL'] <- -1
  valdf$hjustvar[valdf$name=='BR'] <- 2

  valdf$vjustvar[valdf$name=='TL'] <- 2
  valdf$vjustvar[valdf$name=='TR'] <- 2
  valdf$vjustvar[valdf$name=='BL'] <- -2
  valdf$vjustvar[valdf$name=='BR'] <- -2


  p <- ggplot(df) +
    facet_wrap(~MP, ncol=ncol) +
    geom_polygon(data = kobe_df, alpha = alpha, aes(x=x, y=y, fill=fill, alpha=alpha)) +
    scale_fill_manual(values=c('#F8DC7A',  '#67C18B', "#D8775D", '#FDBD56')) +
    expand_limits(x=c(0,2), y=c(0,2)) +
    geom_hline(yintercept = 1, color='darkgray', linetype=2) +
    geom_vline(xintercept = 1, color='darkgray', linetype=2) +
    theme_bw() +
    guides(color='none', fill='none') +
    labs(x=expression(SB/SB[MSY]), y=expression(F/F[MSY])) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 2)) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 2))

  if (fill=='GK') {
    p <- p + geom_point(size=ptsize, aes(x=SB_SBMSY, y=F_FMSY, color=GK)) +
      scale_color_manual(values=c('green', 'red'), drop=FALSE)
  }
  if (fill=='OF') {
    p <- p + geom_point(size=ptsize, aes(x=SB_SBMSY, y=F_FMSY, color=OF)) +
      scale_color_manual(values=c('green', 'red'), drop=FALSE)
  }
  if (fill=='LRP') {
    p <- p + geom_point(size=ptsize, aes(x=SB_SBMSY, y=F_FMSY, color=LRP)) +
      scale_color_manual(values=c('red', 'green'), drop=FALSE) +
      geom_vline(xintercept = 0.4, color='darkgray', linetype=2)
  }

  p + coord_cartesian(clip = 'off') +
    geom_text(data=valdf, aes(x=x, y=y, label=value, hjust=hjustvar,vjust=vjustvar))

}


#' @describeIn TSplots Time-series of projected TAC
#' @param hline Optional. Horizontal line to indicate summary statistic. Numeric length MP.
#' @export
TAC_TS <- function(MMSE, year_range=NULL, mp=NA, hline=NULL) {
  DF <- MakePerformanceDF(MMSE)
  breaks.vec <- seq(min(DF$Year), max(DF$Year), by = "2 years")

  if (!all(is.na(mp))) {
    DF <- DF %>% filter(MP %in% mp)
  }
  Years <- lubridate::year(DF$Year)

  DF2 <- DF
  if (!is.null(year_range)) {
    DF2 <- DF %>% filter(Years %in% year_range)
  }

  p <- ggplot(DF, aes(x=Year, color=Sim, y=TAC)) +
    facet_grid(~MP) +
    expand_limits(y=c(0,2)) +
    geom_line() +
    theme_bw() +
    scale_x_date(date_labels="%Y", breaks=breaks.vec)+
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    guides(color='none') +
    labs(y='TAC (t)')

  if (!is.null(hline)) {
    hline_df <- data.frame(MP=unique(DF$MP), yintercept=hline)
    p <- p +
      geom_hline(data=hline_df, aes(yintercept=yintercept), linetype=2)
  }

  p + geom_point(data=DF2, size=2, shape = 21, aes(fill=Sim)) +
    guides(fill='none')
}



#' @describeIn TSplots Time-series of projected catch
#' @export
Catch_TS <- function(MMSE, year_range=NULL, mp=NA, hline=NULL) {
  DF <- MakePerformanceDF(MMSE)
  breaks.vec <- seq(min(DF$Year), max(DF$Year), by = "2 years")

  if (!all(is.na(mp))) {
    DF <- DF %>% filter(MP %in% mp)
  }
  Years <- lubridate::year(DF$Year)

  DF2 <- DF
  if (!is.null(year_range)) {
    DF2 <- DF %>% ungroup() %>%  filter(Years %in% year_range)
  }

  p <- ggplot(DF, aes(x=Year, color=Sim, y=Catch)) +
    facet_grid(~MP) +
    expand_limits(y=c(0,2)) +
    geom_line() +
    theme_bw() +
    scale_x_date(date_labels="%Y", breaks=breaks.vec)+
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    guides(color='none') +
    labs(y='Landed Catch (t)')

  if (!is.null(hline)) {
    hline_df <- data.frame(MP=unique(DF$MP), yintercept=hline)
    p <- p +
      geom_hline(data=hline_df, aes(yintercept=yintercept), linetype=2)
  }

  p + geom_point(data=DF2, size=2, shape = 21, aes(fill=Sim)) +
    guides(fill='none')
}


#' @describeIn TSplots Time-series of percent change in TAC between management cycles
#' @export
Var_TS <- function(MMSE, mp=NA) {
  DF <- MakePerformanceDF(MMSE)
  breaks.vec <- seq(min(DF$Year), max(DF$Year), by = "2 years")

  if (!all(is.na(mp))) {
    DF <- DF %>% filter(MP %in% mp)
  }

  DF <- DF %>% mutate(Years=lubridate::year(Year))
  Years <- unique(DF$Years)
  change_yrs <- seq(min(Years), by=unique(DF$interval), to=max(Years))

  DF2 <- DF %>% filter(Years %in% change_yrs)
  nsim <- DF$Sim %>% as.character() %>% as.numeric() %>% max()

  calc_Change_MP <- function(DF2, ...) {
    sapply(1:nsim, calc_Change, DF2=DF2)

  }
  calc_Change <- function(x, DF2) {
    tt <- DF2 %>% filter(Sim==x)
    y1 <- 1:(nrow(tt)-1)
    y2 <- y1+1
    ((((tt$TAC[y2] - tt$TAC[y1])/tt$TAC[y1])^2)^0.5)
    ((tt$TAC[y2] - tt$TAC[y1])/tt$TAC[y1])

  }

  tac_change_list <- DF2 %>% group_by(MP) %>% group_map(., calc_Change_MP)

  make_df <- function(i, tac_change_list, change_yrs, MPs) {
    vals <- tac_change_list[[i]]
    dd <- dim(vals)
    nchange <- dd[1]
    nsim <- dd[2]
    data.frame(Sim=1:nsim,
               Year=rep(change_yrs[2:(length(change_yrs))], each=nsim),
               Change=as.vector(t(vals)),
               MP=MPs[i])
  }

  MPs <- DF$MP %>% unique()
  nMPs <- length(MPs)
  DFlist <- lapply(1:nMPs, make_df, tac_change_list=tac_change_list,
                   change_yrs=change_yrs,
                   MPs=MPs)
  DF <- do.call('rbind', DFlist)

  DF$Year <- paste(DF$Year, '01', '01', sep='-')
  DF <- DF %>% mutate(Year=lubridate::as_date(Year))
  DF$Sim <- factor(DF$Sim, levels=unique(DF$Sim))

  DF <- DF %>% mutate(Abs_Change=abs(Change))

  med <- DF %>% group_by(Sim, MP) %>% mutate(med=median(Abs_Change)) %>%
    ungroup() %>% group_by(MP) %>% summarize(med=median(med))
  max <- DF %>% group_by(MP) %>% summarize(max=max(Abs_Change))

  text_df <- data.frame(x=rep(min(DF$Year),2),
                        y=rep(max(DF$Abs_Change),2),
                        MP=DF$MP %>% unique())

  text_df <- left_join(text_df, med, by='MP')
  text_df <- left_join(text_df, max, by='MP')

  text_df$med <- round(text_df$med*100, 2)
  text_df$lab <- paste('Median = ', paste0(text_df$med, "%"))

  text_df$max <- round(text_df$max*100, 2)
  text_df$lab <- paste('Median = ', paste0(text_df$med, "%, Maximum = ", paste0(text_df$max, "%")))

  p <- ggplot(DF) +
    facet_grid(~MP) +
    expand_limits(y=c(0)) +
    geom_line(aes(x=Year, color=Sim, y=Change)) +
    geom_point(size=2, aes(x=Year, color=Sim, y=Change)) +
    theme_bw() +
    geom_hline(yintercept = 0, linetype=2) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    guides(color='none') +
    labs(y='Change in TAC (%)') +
    geom_text(data=text_df,
              aes(x=x, y=y, label=lab,  hjust=0,vjust=2)) +
    scale_x_date(date_labels="%Y", breaks=breaks.vec)

  p
}


