


#' Get the Index from `PPD@Ind` slot in an MMSE object
#'
#' @param MMSE An object of class `MMSE`
#' @param Name Name for the index.
#' @param Stock Stock index number. Default to 1
#' @param Fleet Fleet index number. Default to 1
#'
#' @return A data.frame
#' @export
#'
get_Index <- function(MMSE, Name="Combined Index", Stock=1, Fleet=1) {
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

#' @describeIn get_Index Get the additional indices
#' @export
get_Add_Indices <- function(MMSE, Names=NULL, Stock=1, Fleet=1) {
  Years <- get_Years(MMSE@multiHist)
  Years <- Years %>% filter(Year<max(Year))

  nMPs <- MMSE@nMPs
  MPs <- MMSE@MPs[[1]]

  AddIndList <- list()
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
                                    Value=as.vector(Ind), Index=Names[ind], MP=MPs[mm] )
    }
    AddIndList[[mm]] <- do.call('rbind', templist)
  }
  df <- do.call('rbind', AddIndList)
  df <- left_join(df, Years, by = join_by('Year', 'Sim', 'Age', 'Model'))
  df$include <- FALSE
  df$include[df$Period=='Historical' & !is.na(df$Value)] <- TRUE
  df
}

#' @describeIn get_Index Creates a dataframe and plot comparing the index and standardized biomass
#' @export
Compare_Index <- function(MMSE, nsims=3, seed=101, name='Combined Index', plot=TRUE) {

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
  DF$MP <- factor(DF$MP, levels=unique(DF$MP), ordered = TRUE)

  p <- ggplot(DF, aes(x=Year)) +
    facet_grid(Sim~MP) +
    geom_line(aes(y=BSt)) +
    geom_line(aes(y=ISt, color=Period)) +
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

AddIndList <- function() {
  dd <- dimnames(MOM_000@cpars$Female[[1]]$Data@AddInd)
  nms <- dd[[2]]
  n.indices <- length(nms)
  index_df <- Fleet_DF %>% filter(Code%in% nms)
  out <- list()
  for (i in seq_along(nms)) {
    nm <- nms[i]
    out[[nm]]$Index <- MOM_000@cpars$Female[[1]]$Data@AddInd[1,i,]
    out[[nm]]$Name <- as.character(index_df$Name[match(nm, index_df$Code)])
    out[[nm]]$Name <- gsub('Taipai', 'Taipei', out[[nm]]$Name)
    out[[nm]]$Select <- MOM_000@cpars$Female[[1]]$Data@AddIndV[1,i,]
    units <- MOM_000@cpars$Female[[1]]$Data@AddIunits[i]
    if (units) out[[nm]]$Units <- 'Biomass'
    if (!units) out[[nm]]$Units <- 'Number'
  }
  out
}



for (om in 1:length(RefOMs$OM.object)) {
  MMSE <- MSElist[[om]]
  MMSE@multiHist <- multiHistList[[om]]

  B_at_age <-  get_Biomass_at_Age(MMSE)

  Additional.Indices <- get_Add_Indices.MMSE(MMSE, Names=names(AddIndList()))

  for (ind in c("SPN_1", "CAN_3", "JPN_LATE_5", "CHT_LATE_8", "MOR_9",
                "US_Survey_12", "PORT_Survey_13")) {
    p <- Compare_Additional_Index(B_at_age, Additional.Indices,ind)

    nm <- paste(RefOMs$OM.object[om], ind, sep='_')
    ggsave(paste0('dev/Index_Diagnostic/RefOMs/', nm, '.png'), p$p)


  }


}

Compare_Additional_Index <- function(MMSE, index, nsims=3, seed=101, plot=TRUE) {

  B_at_age <-  get_Biomass_at_Age(MMSE)
  Additional.Indices <- get_Add_Indices.MMSE(MMSE, Names=names(AddIndList()))

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

  indlist <- AddIndList()

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

  selectdf <- data.frame(Age=OM$Age %>% unique(), V=ind_info$Select)
  df <- left_join(OM, selectdf, by = join_by(Age))
  df$AdjusVal <- df$Value*df$V

  df2 <- df %>% group_by(Year,Sim,Period,MP, include) %>% summarise(OM_val=sum(AdjusVal))

  BMean <- df2 %>% ungroup() %>% filter(Sim==min(Sim), Period=='Historical', MP==mps[1], include==TRUE) %>%
    summarise(mean=mean(OM_val))

  df2 <- df2 %>% group_by(Sim, MP) %>%  mutate(BSt=OM_val/BMean$mean)

  IndMean <- IndexDF %>% ungroup() %>% filter(Sim==min(Sim), Period=='Historical', is.na(Value)==FALSE, MP==mps[1], include==TRUE) %>%
    summarise(mean=mean(Value))
  IndexDF <- IndexDF %>% group_by(Sim, MP, Index) %>%  mutate(ISt=Value/IndMean$mean)

  DF <- left_join(IndexDF, df2, by=c('Sim', 'Year', 'MP', 'Period', 'include'))
  DF <- DF %>% group_by(Sim, MP) %>% mutate(I_err=ISt/BSt)

  p <- ggplot(DF, aes(x=Year)) +
    facet_grid(MP~Sim) +
    geom_line(aes(y=BSt)) +
    geom_line(aes(y=ISt, color=Period)) +
    theme_bw() +
    labs(y='Standardized Biomass/Index',
         title=ind_info$Name)

  out <- list()
  out$p <- p
  out$DF <- DF
  if (plot) suppressWarnings(print(p))
  invisible(out)

}


