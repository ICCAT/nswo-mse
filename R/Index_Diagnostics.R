
## redundant - see \Analyses\SCRS_P_2023_064


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


#' @describeIn get_Index Creates a dataframe and plot comparing the index and standardized biomass
#' @export


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




Compare_Additional_Index <- function(MMSE, index, MOM, nsims=3, seed=101, plot=TRUE) {

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

  selectdf$Sim <- unique(OM$Sim)[1]
  selectdflist <- list()
  selectdflist[[1]] <- selectdf
  temp <- selectdf
  for (ii in 2:nsim) {
    temp$Sim <- ii
    selectdflist[[ii]] <- temp
  }
  selectdf <- do.call('rbind', selectdflist)

  head(selectdf)
  head(OM)


  df <- left_join(OM, selectdf, by = join_by('Age', 'Sim'))


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


