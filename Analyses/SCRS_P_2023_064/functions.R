
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
  df$cor <- cor(df$BSt , df$ISt ) # correlation between Observed and Expected
  df$res <- log(df$ISt ) - log(df$BSt ) # residuals - log transformed
  df$ac <- acf(df$res,plot=F)$acf[2,1,1]
  df$sd <- sd(df$res, na.rm=TRUE)
  df$runs <- retseq(df$res<0)
  df$mean.runs <- mean(df$runs)
  df$nyears <- length(df$Year)
  df$rmse <- RMSE(df$BSt , df$ISt )/sd(df$ISt , na.rm=TRUE)
  df
}

Hist_CI_plot <- function(MMSE, MOM) {
  p <- Compare_Index(MMSE, plot=FALSE)
  DF <- p$DF

  DF$OM <-MOM
  sims <- unique(DF$Sim)
  mps <- MPs
  DF <- DF %>% filter(Period=='Historical', Sim==min(sims), MP==mps[1])

  p <- ggplot(DF, aes(x=Year)) +
    geom_line(aes(y=BSt)) +
    geom_line(aes(y=ISt), color='blue') +
    theme_bw() +
    labs(y='Standardized Total Biomass/Index') +
    guides(color='none')

  DF$log_I_err <- log(DF$I_err)
  DF <- DF %>% mutate(negative=ifelse(log_I_err<0, TRUE, FALSE))

  p2 <- ggplot(DF) +
    geom_bar(aes(x=Year, y=log_I_err, fill=negative), stat='identity') +
    guides(fill='none') +
    labs(y='Log deviations') +
    theme_bw()

  df <- DF %>% filter(is.na(ISt)==FALSE) %>% reframe(., fit.diagnostics(.)) %>%
    distinct(cor, ac, sd, mean.runs)
  dfa <- df %>% tidyr::pivot_longer(., cols=1:4)
  dfa$value <- round(dfa$value,2)
  label <- paste0(dfa$name, ' = ', dfa$value, collapse='\n')

  df2 <- data.frame(x=min(DF$Year), y=Inf, label=label)


  p2 <- p2 + geom_text(data=df2, aes(x=x, y=y, label=label), hjust=0, vjust=1.2)


  p3 <- ggplot(DF, aes(x=log_I_err)) +        # Draw histogram with density
    geom_histogram(aes(y = after_stat(density))) +
    stat_function(fun = dnorm,
                  args = list(mean = mean(DF$log_I_err, na.rm = TRUE),
                              sd = sd(DF$log_I_err, na.rm = TRUE)),
                  col = "#1b98e0",
                  size = 1) +
    theme_bw() +
    labs(x='Log deviations', y='Count')


  p4 <- ggplot(DF, aes(x=Year)) +
    geom_line(aes(y=I_err)) +
    guides(fill='none') +
    labs(y='Deviations') +
    geom_hline(yintercept = 1, linetype=2) +
    theme_bw()

  pout <- cowplot::plot_grid(p, p4, p2, p3, nrow=2,
                             labels = c('a)', 'b)', 'c)', 'd'))
  out <- list()
  out$p <- pout
  out$stats <- df
  out
}


Compare_Additional_Index_Hist <- function(MMSE, index, MOM, nsims=3, seed=101, plot=TRUE) {

  B_at_age <-  get_Biomass_at_Age(MMSE)
  Additional.Indices <- get_Add_Indices(MMSE, Names=names(AddIndList(MOM)))

  nsim <- length(unique(B_at_age$Sim))
  mps <- unique(Additional.Indices$MP)
  set.seed(seed)
  sims <- sample(1:nsim, nsims)

  BiomassDF <- B_at_age %>% filter(Sim %in% sims)
  IndexDF <- Additional.Indices %>% filter(Sim %in% sims, Index ==index)

  # Standardize
  nonNA <- IndexDF %>% filter(Sim==min(Sim), Period=='Historical', is.na(Value)==FALSE, MP==mps[1])
  IndexDF$include <- FALSE
  BiomassDF$include <- FALSE
  IndexDF$include[IndexDF$Year %in% nonNA$Year] <- TRUE
  BiomassDF$include[BiomassDF$Year %in% nonNA$Year] <- TRUE

  indlist <- AddIndList(MOM)


  ind <- match(index, names(indlist))
  ind_info <- indlist[[ind]]

  if (ind_info$Units=="Biomass") {
    OM <- BiomassDF  %>%
      select(Year, Sim, Age, Period, Model, MP, Value=Biomass, include)
  }

  if (ind_info$Units=="Number") {
    OM <- BiomassDF %>%
      select(Year, Sim, Age, Period, Model, MP, Value, include)
  }


  selectdf <- ind_info$Select %>% rename(Model=Sex) %>% mutate(Age=as.numeric(Age))


  OM <- OM %>% filter(Period=='Historical', MP==mps[1], Sim==min(Sim))
  OM$MP <- OM$Sim <- NULL

  df <- left_join(OM, selectdf, by = join_by('Age', 'Model', 'Year'))
  df <- df %>% mutate(AdjusVal=Value*Select)

  df2 <- df %>% group_by(Year, include) %>% summarise(OM_val=sum(AdjusVal))

  BMean <- df2 %>% ungroup() %>% filter(include==TRUE) %>%
    summarise(mean=mean(OM_val))


  df2 <- df2  %>%  mutate(BSt=OM_val/BMean$mean)

  IndMean <- IndexDF %>% filter(Period=='Historical', MP==mps[1], Sim==min(Sim)) %>% filter(include==TRUE) %>%
    summarise(mean=mean(Value))
  IndexDF <- IndexDF %>% filter(Period=='Historical', MP==mps[1], Sim==min(Sim)) %>%  mutate(ISt=Value/IndMean$mean)


  DF <- left_join(IndexDF, df2, by=c('Year', 'include'))

  DF <- DF  %>% mutate(I_err=ISt/BSt, OM=MOM,
                       log_I_err=log(I_err))

  p1 <- ggplot(selectdf %>% filter(Year==2020), aes(x=Age, y=Select, color=Model)) +
    geom_line() +
    theme_bw() +
    labs(y='Selectivity')

  ylab <- ind_info$Units

  p <- ggplot(DF, aes(x=Year)) +
    geom_line(aes(y=BSt)) +
    geom_line(aes(y=ISt, color=Period)) +
    theme_bw() +
    labs(y=ylab) +
    guides(color='none')

  p3 <- ggplot(DF, aes(x=log_I_err)) +        # Draw histogram with density
    geom_histogram(aes(y = after_stat(density))) +
    stat_function(fun = dnorm,
                  args = list(mean = mean(DF$log_I_err, na.rm = TRUE),
                              sd = sd(DF$log_I_err, na.rm = TRUE)),
                  col = "#1b98e0",
                  size = 1) +
    theme_bw() +
    labs(x='Log deviations', y='Count')


  df <- DF %>% filter(is.na(ISt)==FALSE) %>% reframe(., fit.diagnostics(.)) %>%
    distinct(cor, ac, sd, mean.runs)

  dfa <- df %>% tidyr::pivot_longer(., cols=1:4)
  dfa$value <- round(dfa$value,2)
  label <- paste0(dfa$name, ' = ', dfa$value, collapse='\n')

  df2 <- data.frame(x=min(DF$Year), y=Inf, label=label)

  DF <- DF %>% mutate(negative=ifelse(log_I_err<0, TRUE, FALSE))

  p2 <- ggplot(DF) +
    geom_bar(aes(x=Year, y=log_I_err, fill=negative), stat='identity') +
    guides(fill='none') +
    labs(y='Log deviations') +
    theme_bw()

  p2 <- p2 + geom_text(data=df2, aes(x=x, y=y, label=label), hjust=0, vjust=1.2)

  p4 <- ggplot(DF, aes(x=Year)) +
    geom_line(aes(y=I_err)) +
    guides(fill='none') +
    labs(y='Deviations') +
    geom_hline(yintercept = 1, linetype=2) +
    theme_bw()

  pout <- cowplot::plot_grid(p, p1, p4, p2, nrow=2,
                             labels = c('a)', 'b)', 'c)', 'd'))

  df$OM <- MOM
  df$Index <- index

  out <- list()
  out$p <- pout
  out$DF <- DF
  out$stats <- df
  out$units <- ind_info$Units
  if (plot) suppressWarnings(print(pout))
  invisible(out)
}


AddIndList <- function(MOM) {
  mom <- get(MOM)
  dd <- dimnames(mom@cpars$Female[[1]]$Data@AddInd)
  nms <- dd[[2]]
  n.indices <- length(nms)
  index_df <- Fleet_DF %>% filter(Code%in% nms)
  out <- list()
  for (i in seq_along(nms)) {
    nm <- nms[i]
    out[[nm]]$Index <- mom@cpars$Female[[1]]$Data@AddInd[1,i,]
    out[[nm]]$Name <- as.character(index_df$Name[match(nm, index_df$Code)])
    out[[nm]]$Name <- gsub('Taipai', 'Taipei', out[[nm]]$Name)
    out[[nm]]$Select <- readRDS(file.path(Index_select_dir, paste0(MOM, '.rda'))) %>%
      filter(Fleet==nm)
    units <- mom@cpars$Female[[1]]$Data@AddIunits[i]
    if (units) out[[nm]]$Units <- 'Biomass'
    if (!units) out[[nm]]$Units <- 'Number'
  }
  out
}


Compare_Index <- function(MMSE, nsims=3, seed=101, name='Combined Index',
                          plot=TRUE, MPs=NULL, Years=NULL) {

  B_at_age <-  get_Biomass_at_Age(MMSE)
  B_DF <- B_at_age  %>%
    group_by(Year, Sim, MP, Period) %>%
    summarize(Value=sum(Biomass))

  Index_DF <- get_Index(MMSE,name)

  nsim <- length(unique(B_DF$Sim))
  mps <- unique(Index_DF$MP)
  set.seed(seed)

  sims <- sample(1:nsim, nsims)

  BiomassDF <- B_DF %>% filter(Sim %in% sims)
  IndexDF <- Index_DF %>% filter(Sim %in% sims)

  # Standardize
  nonNA <- IndexDF %>% filter(Sim==min(Sim), Period=='Historical', is.na(Value)==FALSE, MP==mps[1])
  IndexDF$include <- FALSE
  BiomassDF$include <- FALSE
  IndexDF$include[IndexDF$Year %in% nonNA$Year] <- TRUE
  BiomassDF$include[BiomassDF$Year %in% nonNA$Year] <- TRUE

  IndMean <- IndexDF %>% ungroup() %>% filter(Sim==min(Sim), Period=='Historical', is.na(Value)==FALSE, MP==mps[1], include==TRUE) %>%
    summarise(mean=mean(Value))

  BMean <- BiomassDF %>% ungroup() %>% filter(Sim==min(Sim), Period=='Historical', is.na(Value)==FALSE, MP==mps[1], include==TRUE) %>%
    summarise(mean=mean(Value))

  BiomassDF <- BiomassDF %>% group_by(Sim, MP) %>%  mutate(BSt=Value/BMean$mean)
  IndexDF <- IndexDF %>% group_by(Sim, MP, Index) %>%  mutate(ISt=Value/IndMean$mean)

  BDF <- BiomassDF %>%  filter(Year<max(Year)) %>% select(Year, Sim, MP, Biomass=Value, BSt, Period)
  IDF <- IndexDF %>% select(Year, Sim, MP, Index, Value , ISt)

  DF <- left_join(IDF, BDF, by=c('Sim', 'Year', 'MP'))
  DF <- DF %>% group_by(MP, Sim) %>% mutate(I_err=ISt/BSt)

  DF$Period <- factor(DF$Period)
  if (!is.null(MPs))
    DF <- DF %>% filter(MP %in% MPs)


  DF$MP <- factor(DF$MP, levels=unique(DF$MP), ordered = TRUE)
  if (!is.null(Years))
    DF <- DF %>% filter(Year %in% Years)

  p <- ggplot(DF, aes(x=Year)) +
    facet_grid(Sim~MP) +
    geom_line(aes(y=BSt)) +
    geom_line(aes(y=ISt, color=Period)) +
    scale_color_discrete(drop=FALSE) +
    theme_bw() +
    labs(y='Standardized Biomass/Index',
         title='Combined Index')

  out <- list()
  out$p <- p
  out$DF <- DF
  if (plot)
    suppressWarnings(print(p))
  invisible(out)

}


get_Add_Indices <- function(MMSE, Names=NULL, Stock=1, Fleet=1) {
  Years <- get_Years(MMSE@multiHist)
  Years <- Years %>% filter(Year<max(Year))
  nsim <- MMSE@nsim
  nMPs <- MMSE@nMPs
  MPs <- MMSE@MPs[[1]]

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
