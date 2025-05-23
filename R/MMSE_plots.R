
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

  # add MSY reference points
  MSYlist <- list()
  for (i in 1:length(MMSElist)) {
    MSYlist[[i]] <- MMSElist[[i]]@RefPoint$ByYear$MSY
  }

  MMSE_out@RefPoint$ByYear$MSY <- do.call(abind:::abind, c(MSYlist, along = 1))

  # PPD

  MMSE_out@Name <- name
  MMSE_out
}

#' Subset an MMSE object by MP
#'
#' @param MMSE An object of class `MMSE`
#' @param name The names of the MPs to keep
#'
#' @return An object of class `MMSE`
#' @export
Sub_MMSE <- function(MMSE, MPs=NA) {
  MMSE_out <- MMSE
  mp_ind <- match(MPs, MMSE@MPs[[1]])

  slots <- c('SB_SBMSY', 'F_FMSY', 'N', 'B', 'SSB', 'VB', 'Catch', 'Removals', 'TAC', 'FM')
  for (sl in slots) {
    vals <- slot(MMSE, sl)
    if (length(dim(vals))==5) {
      slot(MMSE_out, sl) <- vals[,,, mp_ind,,drop=FALSE]
    } else if (length(dim(vals))==6) {
      slot(MMSE_out, sl) <- vals[,,,mp_ind,,,drop=FALSE]
    } else {
      slot(MMSE_out, sl) <- vals[,,mp_ind,,drop=FALSE]
    }
  }

  MMSE_out@RefPoint$ByYear$MSY <- MMSE_out@RefPoint$ByYear$MSY[,,mp_ind,]
  MMSE_out@RefPoint$ByYear$FMSY <- MMSE_out@RefPoint$ByYear$FMSY[,,mp_ind,]
  MMSE_out@RefPoint$ByYear$SSBMSY <- MMSE_out@RefPoint$ByYear$SSBMSY[,,mp_ind,]
  MMSE_out@RefPoint$ByYear$BMSY <- MMSE_out@RefPoint$ByYear$BMSY[,,mp_ind,]


  ## PPD
  ns <- MMSE_out@nstocks
  nf <- MMSE_out@nfleets

  for (st in 1:ns) {
    for (fl in 1:nf) {
      MMSE_out@PPD[[st]][[fl]] <- list()

      for (mm in seq_along(mp_ind)) {
        MMSE_out@PPD[[st]][[fl]][[mm]] <- MMSE@PPD[[st]][[fl]][[mp_ind[mm]]]

      }

    }


  }

  MMSE_out@MPs[[1]] <- MMSE@MPs[[1]][mp_ind]
  MMSE_out@nMPs <- length(MMSE_out@MPs[[1]])
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
  openMSE::get_Years(MSE)
  all.yrs <- c(MSE@PPD[[1]][[1]][[1]]@Year, max(MSE@PPD[[1]][[1]][[1]]@Year)+1)
  p.yrs <- all.yrs[all.yrs>2022]
  yr.ind <- which(p.yrs %in% 2025:2054)
  years <- p.yrs[yr.ind]
  nyears <- length(years)

  YearDF <- data.frame(ind=1:32, Year=2023:2054, ind2=-1:30)

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
    facet_wrap(~MP,ncol=ncol) +
    expand_limits(y=c(0,2)) +
    geom_hline(yintercept = 1, color='darkgray', linetype=2) +
    geom_line(alpha=0.6) +
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

  p + coord_cartesian(clip = 'off') +
    labs(y=expression(F/F[MSY]))
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
    DF2 <- DF2 %>% group_by(Sim, MP) %>% mutate(LRP_line=ifelse(any(LRP==TRUE), TRUE, FALSE)) %>% ungroup()
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
    geom_line(aes(group=Sim), alpha=0.6)
  if (fill!='LRP') {
    p <- p + geom_line(aes(color=Sim), alpha=0.6)
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
  p + coord_cartesian(clip = 'off') +
    labs(y=expression(SB/SB[MSY]))


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

  valdf$value  <- signif(valdf$value, 2)
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
TAC_TS <- function(MMSE, year_range=NULL, mp=NA, hline=NULL, ncol=3) {
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
    facet_wrap(~MP, ncol=ncol) +
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
Catch_TS <- function(MMSE, year_range=NULL, mp=NA, hline=NULL, ncol=3) {
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
    facet_wrap(~MP, ncol=ncol) +
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

  if (!is.null(year_range))
    p <- p + geom_point(data=DF2, size=2, shape = 21, aes(fill=Sim))

  p + guides(fill='none')

}


#' @describeIn TSplots Time-series of percent change in TAC between management cycles
#' @export
Var_TS <- function(MMSE, mp=NA, ncol=3) {


  nsim <- MMSE@nsim
  temp <- VarC(MMSE)
  nmanyr <- dim(temp@Stat)[2]
  if (nmanyr==0) {
    vals <- 0
    nmanyr <- 1
  } else {
    vals <- as.vector(temp@Stat)
  }

  varCdf <- data.frame(Sim=1:nsim,
                       Management_Year=rep(1:nmanyr, each=nsim),
                       MP=rep(MMSE@MPs[[1]], each=nmanyr*nsim),
                       Value=vals)


  p <- ggplot(varCdf, aes(x=MP, y=Value*100, fill=MP)) +
    geom_violin(scale='width') +
    theme_bw() +
    expand_limits(y=0) +
    guides(fill='none') +
    labs(x='Candidate Management Procedure',
         y='Absolute change in TAC (%)') +
    theme(axis.text.x = element_text(angle=90))

  p
}



SB_SBMSY_Box <- function(MMSE, year_range=NULL, mp=NA, ref=1,ncol=3) {
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


  p <- ggplot(DF, aes(x=Year, y=SB_SBMSY)) +
    facet_wrap(~MP, ncol=ncol) +
    expand_limits(y=c(0,2)) +
    geom_hline(yintercept = ref, color='darkgray', linetype=2) +
    theme_bw()  +
    scale_x_date(date_labels="%Y", breaks=breaks.vec)+
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    guides(color='none') +
    geom_boxplot(aes(group=Year), fill='gray')

  p + coord_cartesian(clip = 'off') +
    labs(y=expression(SB/SB[MSY]))

}


F_FMSY_Box <- function(MMSE, year_range=NULL, mp=NA, ref=1,ncol=3) {
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


  p <- ggplot(DF, aes(x=Year, y=F_FMSY)) +
    facet_wrap(~MP, ncol=ncol) +
    expand_limits(y=c(0,2)) +
    geom_hline(yintercept = ref, color='darkgray', linetype=2) +
    theme_bw()  +
    scale_x_date(date_labels="%Y", breaks=breaks.vec)+
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    guides(color='none') +
    geom_boxplot(aes(group=Year), fill='gray')

  p + coord_cartesian(clip = 'off') +
    labs(y=expression(F/F[MSY]))

}


#' Time-series plot of SB/SBMSY and TAC
#'
#' @param MMSE An object of class `MMSE`
#' @param relY Logical. Make TAC relative to MSY?
#'
#' @return A `ggplot` object
#' @export
TS_plot <- function(MMSE, relY=FALSE, incFMSY=FALSE) {

  MPs <- MMSE@MPs[[1]]
  nMPs <- MMSE@nMPs

  CurrentYr <- MMSE@Fleets[[1]][[1]]@CurrentYr
  nyears <- MMSE@nyears
  proyears <- MMSE@proyears
  proj.yrs <- (CurrentYr+1):(CurrentYr[1]+proyears)


  SB_SBMSY_list <- list()
  F_FMSY_list <- list()
  TAC_list <- list()
  for (mm in 1:nMPs) {
    SB_SBMSY_list[[mm]] <- data.frame(median=apply(MMSE@SB_SBMSY[,1,mm,], 2, median),
                                      t(apply(MMSE@SB_SBMSY[,1,mm,], 2, quantile, c(0.25, 0.75))),
                                      t(apply(MMSE@SB_SBMSY[,1,mm,], 2, quantile, c(0.1, 0.9))),
                                      Year=proj.yrs,
                                      MP=MPs[mm])

    # FMSY
    F_FMSY_list[[mm]] <- data.frame(median=apply(MMSE@F_FMSY[,1,1,mm,], 2, median),
                                      t(apply(MMSE@F_FMSY[,1,1,mm,], 2, quantile, c(0.25, 0.75))),
                                      t(apply(MMSE@F_FMSY[,1,1,mm,], 2, quantile, c(0.1, 0.9))),
                                      Year=proj.yrs,
                                      MP=MPs[mm])


    # yield
    TAC <- apply(MMSE@TAC, c(1,2, 4,5), sum)
    MSY <- MMSE@RefPoint$ByYear$MSY[,,,(nyears+1):(nyears+proyears)]
    TAC <- apply(TAC, c(1,3,4), sum)
    MSY <- apply(MSY, c(1,3,4), sum)

    if (relY)
      TAC <- TAC/MSY

    TAC_list[[mm]] <- data.frame(median=apply(TAC[,mm,], 2, median),
                                 t(apply(TAC[,mm,], 2, quantile, c(0.25, 0.75))),
                                 t(apply(TAC[,mm,], 2, quantile, c(0.1, 0.9))),
                                 Year=proj.yrs,
                                 MP=MPs[mm])

  }

  SB_SBMSY_df <- do.call('rbind', SB_SBMSY_list)
  SB_SBMSY_df$Var <- 'SB/SBMSY'
  SB_SBMSY_df$yline1 <- 1
  SB_SBMSY_df$yline2 <- 0.4

  F_FMSY_df <- do.call('rbind', F_FMSY_list)
  F_FMSY_df$Var <- 'F/FMSY'
  F_FMSY_df$yline1 <- 1
  F_FMSY_df$yline2 <- NA

  TAC_df <- do.call('rbind', TAC_list)
  TAC_df$Var <- 'TAC'
  TAC_df$yline1 <- 1
  TAC_df$yline2 <- NA

  if(incFMSY) {
    df <- bind_rows(SB_SBMSY_df, F_FMSY_df, TAC_df)
  } else {
    df <- bind_rows(SB_SBMSY_df, TAC_df)
  }


  df <- df %>% filter(Year>=2024)
  df$MP <- factor(df$MP, levels=MMSE@MPs[[1]], ordered = TRUE)


  alpha <- 0.7
  fill1 <- 'darkgrey'
  fill2 <- 'lightgrey'
  ggplot(df, aes(x=Year)) +
    facet_grid(Var~MP, scales='free') +
    geom_ribbon(aes(ymin=X10. , ymax=X90.), fill=fill1, alpha=alpha) +
    geom_ribbon(aes(ymin=X25. , ymax=X75.), fill=fill2, alpha=alpha) +
    geom_line(aes(y=median)) +
    expand_limits(y=0) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_bw() +
    labs(x='Projection Year', y='Median (percentiles)') +
    geom_hline(aes(yintercept=yline1), linetype=2) +
    geom_hline(aes(yintercept=yline2), linetype=3)

}

## Kobe Plot -----

#' Kobe Time Plot
#'
#' @param MMSE An object of class `MMSE`
#'
#' @return A `ggplot` object
#' @export
Kobe_Time <- function(MMSE) {

  nMPs <- MMSE@nMPs
  nrow <- ceiling(nMPs/4)

  res_list <- lapply(1:nMPs, Kobe_Time_MP, MMSE=MMSE)

  df1 <- do.call('rbind', res_list) %>% filter(Year>=2024)

  df <- df1 %>% tidyr::pivot_longer(., cols=1:4)
  df$name <- factor(df$name, levels=c('br', 'tr', 'bl', 'tl'), ordered = TRUE)

  df$MP <- factor(df$MP, levels=MMSE@MPs[[1]], ordered = TRUE)
  cols <- c('green', 'orange', 'yellow', 'red')
  ggplot(df, aes(x=Year, y=value, fill=name)) +
    facet_wrap(~MP, nrow=nrow) +
    geom_bar(position="stack", stat="identity", width = 1) +
    scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
    scale_fill_manual(values=cols) +
    guides(fill='none') +
    labs(y="Percent of total simulatons (%)",
         x='Projection Year') +
    theme_bw()

}

Kobe_Time_MP <- function(mm, MMSE) {
  nsim <- MMSE@nsim
  CurrentYr <- MMSE@Fleets[[1]][[1]]@CurrentYr
  proyears <- MMSE@proyears
  proj.yrs <- (CurrentYr+1):(CurrentYr[1]+proyears)
  MP <- MMSE@MPs[[1]][mm]
  results_list <- list()
  for (y in seq_along(proj.yrs)) {
    bl <- sum(MMSE@SB_SBMSY[,1,mm,y] < 1 &  MMSE@F_FMSY[,1,1, mm,y] < 1)/nsim * 100
    tl <- sum(MMSE@SB_SBMSY[,1,mm,y] < 1 &  MMSE@F_FMSY[,1,1, mm,y] > 1)/nsim * 100
    br <- sum(MMSE@SB_SBMSY[,1,mm,y] > 1 &  MMSE@F_FMSY[,1,1, mm,y] < 1)/nsim * 100
    tr <- sum(MMSE@SB_SBMSY[,1,mm,y] > 1 &  MMSE@F_FMSY[,1,1, mm,y] > 1)/nsim * 100

    results_list[[y]] <- data.frame(bl=round(bl,2),
                                    tl=round(tl,2),
                                    br=round(br,2),
                                    tr=round(tr,2),
                                    Year=proj.yrs[y],
                                    MP=MP)

  }

  do.call('rbind', results_list)

}


## Violin Var C ----

#' Violin Plot for Median Absolute Change in TAC
#'
#' @param MMSE An object of class `MMSE`
#'
#' @return A `ggplot` object
#' @export
VarC_Violin <- function(MMSE, np=1) {
  tt <- VarC(MMSE)

  df <- data.frame(Sim=1:MMSE@nsim, MP=rep(MMSE@MPs[[1]], each=MMSE@nsim), Var=as.vector(tt@Stat))
  mp_names <- MMSE@MPs[[1]]
  mp_names <- sort(mp_names)

  df_2 <- df %>% group_by(MP) %>% summarize(val=median(Var)) %>% arrange(val)
  mp_names_2 <- df_2$MP

  df$Var <- df$Var * 100
  df2 <- df
  df$MP <- factor(df$MP, levels=mp_names, ordered = TRUE)
  df2$MP <- factor(df2$MP, levels=mp_names_2, ordered = TRUE)

  p1 <-ggplot(df, aes(x=MP, y=Var, fill=MP)) +
    geom_violin(scale='width') +
    theme_bw() +
    guides(fill='none') +
    labs(x='Candiate Management Procedure',
         y='Median absolute change in TAC (%)') +
    theme(axis.text.x = element_text(angle=90))

  p2 <- ggplot(df2, aes(x=MP, y=Var, fill=MP)) +
    geom_violin(scale='width') +
    theme_bw() +
    guides(fill='none') +
    labs(x='Candiate Management Procedure',
         y='Median absolute change in TAC (%)') +
    theme(axis.text.x = element_text(angle=90))

  if (length(np)==1) {
    if (np==1) {
      out <- p1
    }
    if (np==2) {
      out <- p2
    }
  }

  if (length(np)==2) {
    out <- cowplot::plot_grid(p1,p2,nrow=2, align='v')
  }
  out
}


