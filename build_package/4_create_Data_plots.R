
library(ggplot2); library(dplyr); library(tidyr)


OM.root <- 'G:/My Drive/1_Projects/North_Atlantic_Swordfish/OMs'
DataList <- readRDS(paste0(OM.root, '/OM_objects/DataList.rda'))

Fleet_DF <- readRDS(paste0(OM.root, '/OM_objects/Fleet_DF.rda'))

# --- Make Data Figures ----
Data <- DataList

CatchDat <- Data$catch %>%  dplyr::filter(year>0)  %>%
  dplyr::rename(Code=fleet) %>%
  dplyr::select(year, Code, catch)

CatchDat$Code <- Fleet_DF$Code[match(CatchDat$Code, Fleet_DF$index)]
CatchDat <- left_join(CatchDat, Fleet_DF, by="Code")

WghtDat <- Data$meanbodywt %>% dplyr::filter(Year>0) %>%
  dplyr::rename(year=Year, index=Fleet, Mean.Weight=Value, Weight.CV=Std_in) %>%
  dplyr::select(year, index, Mean.Weight, Weight.CV)
WghtDat <- left_join(WghtDat, Fleet_DF, by="index")

LenDat <- Data$lencomp %>% dplyr::filter(Yr>0) %>%
  dplyr::rename(year=Yr, index=FltSvy)
cols <- names(LenDat)[7:length(names(LenDat))] # convert to long
LenDat <- LenDat %>% tidyr::pivot_longer(all_of(cols), names_to='Length.Class', values_to="Length.Comp") %>%
  dplyr::select(year, index, Length.Class, Length.Comp, Nsamp) %>%
  dplyr::rename(Length.Nsamp=Nsamp)
LenDat <- left_join(LenDat, Fleet_DF, by="index")

CPUEDat <- Data$CPUE %>% dplyr::filter(year>0) %>%
  dplyr::rename(CPUE_Obs=obs, CPUE_SE_log=se_log) %>%
  dplyr::select(year, index, CPUE_Obs, CPUE_SE_log) %>%
  dplyr::mutate(llq=1)
CPUEDat <- dplyr::left_join(CPUEDat, Fleet_DF, by="index")

CPUEDat2 <- CPUEDat %>% dplyr::filter(Name!='Combined Index', Name!='EU-Spain longline (LL)')

AllDat <- dplyr::full_join(CatchDat, CPUEDat2, by = c("year", "Code", "Name", "index"))
AllDat <- dplyr::full_join(AllDat, LenDat, by = c("year", "Code", "Name", "index"))
AllDat <- dplyr::full_join(AllDat, WghtDat, by =  c("year", "Code", "Name", "index"))
AllDat <- dplyr::select(AllDat, year, index, Code, Name, catch, CPUE_Obs,
                        CPUE_SE_log, Length.Class, Length.Comp, Length.Nsamp,
                        Mean.Weight, Weight.CV)
AllDat$year2 <- as.Date(paste(AllDat$year, 1, 1, sep = "-"))

DataSummary <- AllDat %>% dplyr::select(year, Code, Name, catch, CPUE_Obs,
                                        Length.Comp, Mean.Weight, year2) %>%
  tidyr::pivot_longer(cols=c('catch', 'CPUE_Obs', 'Length.Comp', 'Mean.Weight'),
                      names_to = 'Data') %>%
  dplyr::mutate(is.data=ifelse(value>0, TRUE, NA)) %>%
  na.omit()

Data.Names <- c("Catch", 'CPUE', "Length Composition", "Mean Weight")
DataSummary$Data <- factor(DataSummary$Data, labels=Data.Names)

# ---- Data summary plot -----
p <- ggplot2::ggplot(DataSummary, ggplot2::aes(year2, Code, color=is.data)) +
  ggplot2::facet_wrap(~Data, ncol=2) +
  ggplot2::geom_line(linewidth=2) +
  ggplot2::labs(y="Fleet", x="Year", color='') +
  ggplot2::scale_color_manual(values='#357DE9') +
  ggplot2::theme_bw()+
  ggplot2::guides(color="none") +
  ggplot2::scale_x_date(breaks=seq.Date(min(DataSummary$year2), max(DataSummary$year2), "5 year"),
                        labels = scales::date_format("%Y")) +
  ggplot2::theme(axis.text.x= ggplot2::element_text(angle=90, vjust = 0.5, hjust=1),
                 strip.background = ggplot2::element_blank(),
                 strip.text = ggplot2::element_text(size=12))
ggplot2::ggsave('img/Data_overview.png', p, width=6, height=6)

# ---- Plot Main Index (Combined Index) ----

IndexDF <- data.frame(Year=SWOData@Year, Index=SWOData@Ind[1,])

p <- ggplot(IndexDF, aes(x=Year, y=Index)) +
  geom_line(size=1) +
  theme_bw() +
  expand_limits(y=0) +
  labs(y='Combined Index')

ggsave('img/Combined_Index.png', p, width=6, height=4)


# ---- Plot Additional Indices -----
AddInd <- SWOMSE::MOM_001@cpars[[1]][[1]]$Data@AddInd


length(SWOMSE::MOM_001@cpars[[1]][[1]]$Data@AddIunits)

ni <- dim(AddInd)[2]
yrs <- dimnames(AddInd)[[3]]
nms <- dimnames(AddInd)[[2]]
outlist <- list()
for (i in 1:ni) {
  outlist[[i]] <- data.frame(Year=yrs, Index=nms[i], Value=AddInd[1,i,])
}
df <- do.call('rbind', outlist)
df$Year <- as.numeric(df$Year)
df$Index <- factor(df$Index, levels=unique(df$Index), ordered = TRUE)

p <- ggplot(df, aes(x=Year, y=Value)) +
  facet_wrap(~Index) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw()

ggsave('img/Individual_Indices.png', p, width=8, height=6)

# ---- Plot Catch and TACs ----
dat <- read.csv('inst/Index_Catch_TAC.csv') %>%
  tidyr::pivot_longer(.,cols=c(Catch, TAC))

p <- ggplot(dat, aes(x=Year, y=value, linetype=name)) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw() +
  labs(y='Ton', linetype='')

ggsave('img/Catch_TAC.png', p, width=6, height=4)

