

#' Diagnostic Plots for the Indices
#'
#' @param MMSE Object of class `MMSE`
#' @param Index 'CI' for Combined Index or numeric value for one of the Additional Indices (not currently implemented)
#'
#' @return A list of ggplot objects
#' @export
Index_Diagnostic <- function(MMSE, Index='CI') {

  if (Index=='CI')
    Index_Diagnostic_CI(MMSE)

}

#' Get the Combined Index for an MMSE object
#'
#' @param MMSE Object of class `MMSE`
#'
#' @return a data.frame
#' @export
get_CI_Index <- function(MMSE) {
  Name <- "Combined Index"
  Stock <- 1
  Fleet <- 1
  nsim <- MMSE@nsim
  nMPs <- MMSE@nMPs
  MPs <- MMSE@MPs[[1]]
  Years <- get_Years(MMSE)
  Years <- Years %>% filter(Year<max(Year))

  index_list <- list()
  for (mm in 1:nMPs) {
    Ind <- MMSE@PPD[[Stock]][[Fleet]][[mm]]@Ind
    df <- data.frame(Sim=1:nsim,
                     Year=rep(Years$Year, each=nsim),
                     Value=as.vector(Ind), Index=Name, MP=MPs[mm])
    index_list[[mm]] <- df
  }
  df <- do.call('rbind', index_list)
  df <-left_join(df, Years, by='Year')
  df$include <- FALSE
  df$include[df$Period=='Historical' & !is.na(df$Value)] <- TRUE
  df
}

#' Diagnostic Plots for the Combined Index
#'
#' @param MMSE  Object of class `MMSE`
#' @param fit.years Years where the index statistical properties are calculated
#' @param nsims Maximum number of simulations to plot
#'
#' @return A list of `ggplot` objects
#' @export
Index_Diagnostic_CI <- function(MMSE, fit.years=1999:2020, nsims=6) {

  B_at_Age <- get_Biomass_at_Age(MMSE)
  B_DF <- B_at_Age  %>%
    group_by(Year, Sim, MP, Period) %>%
    summarize(Value=sum(Biomass))

  Index_DF <- get_CI_Index(MMSE)

  nsim <- length(unique(B_DF$Sim))
  mps <- unique(Index_DF$MP)

  if (nsim>nsims) {
    sims <- 1:nsims
  } else {
    sims <- 1:nsim
  }

  BiomassDF <- B_DF %>% filter(Sim %in% sims)
  IndexDF <- Index_DF %>% filter(Sim %in% sims)

  # Standardize the Biomass and Index
  IndexDF$include <- FALSE
  IndexDF$include[IndexDF$Year %in% fit.years] <- TRUE

  BiomassDF$include <- FALSE
  BiomassDF$include[BiomassDF$Year %in% fit.years] <- TRUE

  IndMean <- IndexDF %>% ungroup() %>%
    filter(Sim==min(Sim), Period=='Historical', is.na(Value)==FALSE, MP==mps[1], include==TRUE) %>%
    summarise(mean=mean(Value))

  BMean <- BiomassDF %>% ungroup() %>%
    filter(Sim==min(Sim), Period=='Historical', is.na(Value)==FALSE, MP==mps[1], include==TRUE) %>%
    summarise(mean=mean(Value))

  # Standardize to mean 1 over `fit.years`
  BiomassDF <- BiomassDF %>% group_by(Sim, MP) %>%  mutate(BSt=Value/BMean$mean)
  IndexDF <- IndexDF %>% group_by(Sim, MP, Index) %>%  mutate(ISt=Value/IndMean$mean)

  BDF <- BiomassDF %>%  filter(Year<max(Year)) %>% select(Year, Sim, MP, Biomass=Value, BSt, Period)
  IDF <- IndexDF %>% select(Year, Sim, MP, Index, Value , ISt)

  DF <- left_join(IDF, BDF, by=c('Sim', 'Year', 'MP'))
  DF <- DF %>% group_by(MP, Sim) %>% mutate(I_err=ISt/BSt)

  DF$Period <- factor(DF$Period)
  DF$MP <- factor(DF$MP, levels=unique(DF$MP), ordered = TRUE)
  DF$Fit <- FALSE
  DF$Fit[DF$Year %in% fit.years] <- TRUE

  DF2 <- DF %>% filter(Fit==TRUE, MP==mps[1], Sim==1)

  DF3 <- DF2 %>%  ungroup() %>% select(Year, Biomass=BSt, Index=ISt) %>%
    tidyr::pivot_longer(., cols=2:3)

  # Historical Plots
  p1 <- ggplot(DF3, aes(x=Year, y=value, color=name)) +
    geom_line() +
    theme_bw() +
    labs(y='Standardized Biomass/Index',
         color='Legend') +
    scale_color_manual(values=c('black', 'blue'))

  p2 <- ggplot(DF2, aes(x=Year, y=I_err)) +
    geom_line() +
    theme_bw() +
    labs(y='Deviations') +
    geom_hline(yintercept = 1, linetype=2)


  # Statistical Properties
  log_resids <- log(DF2$I_err)
  resid_df <- data.frame(Year=DF2$Year, log_resids=log_resids)
  resid_df$Color <- 'Positive'
  resid_df$Color[resid_df$log_resids<0] <- 'Negative'

  ind_stats_1 <-MMSE@multiHist[[1]][[1]]@SampPars$Obs$Ind_Stat[1,]
  ind_stats <- ind_stats_1 %>% tidyr::pivot_longer(., cols=1:2)
  ind_stats$label <- paste(ind_stats$name, round(ind_stats$value,2), sep='=')
  ind_stats$x <- min(resid_df$Year)
  qq <- quantile(log_resids)
  ind_stats$y <- c(qq[1], qq[1]*0.9)

  p3 <- ggplot(resid_df) +
    geom_bar(aes(x=Year, y=log_resids, fill=Color), stat='identity') +
    theme_bw() +
    labs(y='Log Residuals') +
    geom_text(data=ind_stats, aes(x=x, y=y, label=label))


  rr <- qnorm(0.99, 0, ind_stats_1$SD)
  p4 <- ggplot(resid_df, aes(x=log_resids)) +
    geom_histogram(aes(y = after_stat(density))) +
    stat_function(fun = dnorm, args = list(mean =0, sd = ind_stats_1$SD),
                  color='blue') +
    theme_bw() +
    expand_limits(x=c(-rr, max(rr))) +
    labs(x='Log Deviations', y='Count')

  p1 <- cowplot::plot_grid(p1, p2, p3, p4, labels=c('a)', 'b)', 'c)', 'd)'))


  # Projected Indices
  pDF <- DF %>% filter(Period=='Projection') %>%
    group_by(Year, Sim, MP) %>%
    summarise(log_resid=log(BSt)-log(ISt))

  txt <- paste0('Projections: ', paste(paste('SD =', round(sd(pDF$log_resid),2)),
           paste('AC =', round(acf(pDF$log_resid, plot=FALSE)$acf[2,1,1],2)),
           sep='; '))

  p2 <- ggplot(DF, aes(x=Year)) +
    facet_grid(Sim~MP) +
    geom_line(aes(y=BSt)) +
    geom_line(aes(y=ISt, color=Period)) +
    scale_color_discrete(drop=FALSE) +
    theme_bw() +
    expand_limits(y=0) +
    labs(y='Standardized Biomass/Index',
         title=txt)

  list(p1, p2)

}



get_Add_Indices <- function(MMSE) {
  Stock <- Fleet <- 1
  Years <- get_Years(MMSE@multiHist)
  Years <- Years %>% filter(Year<max(Year))
  nsim <- MMSE@nsim
  nMPs <- MMSE@nMPs
  MPs <- MMSE@MPs[[1]]

  nms <-dimnames(SWOData@AddInd)[[2]]
  tt <- Fleet_DF %>% filter(Code %in% nms) %>% select(Name)
  Names <- tt$Name

  AddInd_List <- list()
  dd <- dim(MMSE@PPD[[Stock]][[Fleet]][[1]]@AddInd)
  n.indices <- dd[2]
  if (is.null(Names))
    Names <- paste('Additional Index: ', 1:n.indices)

  if (length(Names)!=n.indices)
    stop('Length `Names` does not equal ', n.indices)

  for (mm in 1:nMPs) {
    templist <- list()
    for (ind in 1:n.indices) {
      Ind <- MMSE@PPD[[Stock]][[Fleet]][[mm]]@AddInd[,ind,]
      templist[[ind]] <- data.frame(Sim=1:nsim,
                                    Year=rep(Years$Year, each=nsim),
                                    Value=as.vector(Ind),
                                    Index=Names[ind], MP=MPs[mm] )
    }
    AddInd_List[[mm]] <- do.call('rbind', templist)
  }
  df <- do.call('rbind', AddInd_List)
  df <- left_join(df, Years, by = join_by('Year'))
  df$include <- FALSE
  df$include[df$Period=='Historical' & !is.na(df$Value)] <- TRUE
  df
}


Index_Diagnostic_AI <- function(MMSE, index=1, nsims=6) {

  B_at_age <-  get_Biomass_at_Age(MMSE)
  Additional.Indices <- get_Add_Indices(MMSE)

  MMSE@Name
  OM_DF

  nsim <- length(unique(B_at_age$Sim))
  mps <- unique(Additional.Indices$MP)
  if (nsim>nsims) {
    sims <- 1:nsims
  } else {
    sims <- 1:nsim
  }

  BiomassDF <- B_at_age %>% filter(Sim %in% sims)
  Index_Names <- unique(Additional.Indices$Index)
  IndexDF <- Additional.Indices %>% filter(Sim %in% sims, Index ==Index_Names[index])

  # Standardize
  nonNA <- IndexDF %>% filter(Sim==min(Sim), Period=='Historical', is.na(Value)==FALSE, MP==mps[1])
  IndexDF$include <- FALSE
  BiomassDF$include <- FALSE
  IndexDF$include[IndexDF$Year %in% nonNA$Year] <- TRUE
  BiomassDF$include[BiomassDF$Year %in% nonNA$Year] <- TRUE

  stop('TBC')

}








