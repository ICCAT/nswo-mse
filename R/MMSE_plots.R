
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

  slots <- c('SB_SBMSY', 'F_FMSY', 'N', 'B', 'SSB', 'VB', 'Catch', 'Removals')
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

# Kobe plot -----
