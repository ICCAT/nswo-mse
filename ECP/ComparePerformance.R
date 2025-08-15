library(SWOMSE)

MSE_Files <- list.files('ECP/MSE_Objects', full.names = TRUE)

indicesFiles <- list.files('ECP/Indices', full.names = TRUE)
indices <- gsub('.rda', '', indicesFiles)


# Plot Indices ----

Year <- SWOData@Year
BaseCaseIndex <- SWOData@Ind[1,]

IndexList <- list()
for (i in seq_along(indicesFiles)) {
  index <- readRDS(indicesFiles[i])
  dropped <- basename(indices[i])
  IndexList[[i]] <- data.frame(Year=index$YearC,
                               Index=index$response/mean(index$response, na.rm=TRUE),
                               CombinedIndex=SWOData@Ind[1,14:73],
                               Dropped=dropped)
}

DroppedIndices <- do.call('rbind', IndexList)


ggplot(DroppedIndices, aes(x=Year)) +
  geom_line(aes(y=Index, color=Dropped)) +
  geom_line(aes(y=CombinedIndex), linetype=2) +
  geom_vline(xintercept = c(1999, 2022), linetype=2, color='darkgray') +
  facet_wrap(~Dropped) +
  expand_limits(y=0) +
  theme_bw() +
  guides(color='none')

ggsave('ECP/Figures/CompareIndices.png')

nLRP

# Calculate PMs for Base Case ----

BaseCaseFiles <- MSE_Files[grepl('Base.rda', MSE_Files)]
BaseMSEList <- list()
for (j in seq_along(BaseCaseFiles)) {
  BaseMSEList[[j]] <- readRDS(BaseCaseFiles[j])
}

BaseCaseMSE <- combine_MMSE(BaseMSEList, 'BaseCase')

AvCatch <- function(MMSEobj=NULL, Ref=NULL, Yrs=c(7,32)) {
  if(!inherits(MMSEobj,'MMSE'))
    stop('This PM method is designed for objects of class `MMSE`')
  Yrs <- ChkYrs(Yrs, MMSEobj)

  PMobj <- new("PMobj")
  PMobj@Name <- ''
  PMobj@Caption <- ''

  Stat_y <- apply(MMSEobj@Catch[,,,,Yrs[1]:Yrs[2], drop=FALSE], c(1,4,5), sum)

  PMobj@Stat <- apply(Stat_y, c(1,2), median)
  PMobj@Ref <- 1
  PMobj@Prob <- calcProb(PMobj@Stat, MMSEobj) # no probability to calculate

  PMobj@Mean <- apply(Stat_y, 2, median)
  PMobj@MPs <- MMSEobj@MPs[[1]]
  PMobj
}
class(AvCatch) <- 'PM'


source('R/PMs.r')
PMs <- c("PGK_short", "PGK_med",
         'PBMSY', "PNOF",
         "AvCatch",  "VarC")


BaseCasePMs <- lapply(PMs, function(x) {
  fun <- get(x)
  fun(BaseCaseMSE)
})

# Calculate PMs for each Dropped Index ----

PMList <- list()
IndexMSEList <- list()

for (i in seq_along(indices)) {
  index <- basename(indices[i])
  IndexFiles <- MSE_Files[grepl(paste0('Dropped_', index), MSE_Files)]

  MSEList <- list()
  for (j in seq_along(IndexFiles)) {
    MSEList[[j]] <- readRDS(IndexFiles[j])
  }
  IndexMSEList[[i]] <- combine_MMSE(MSEList, paste0('Dropped_', index))


  IndexPMs <- lapply(PMs, function(x) {
    fun <- get(x)
    fun(IndexMSEList[[i]])
  })

  PMList[[i]] <- data.frame(PM=PMs,
                            Mean=lapply(IndexPMs, slot, 'Mean') |> unlist(),
                            Dropped=index)

}

# Make Plot of Mean SB/SBMSY, F/FMSY, Catch - examples
Sim <- 1
PlotList <- list()
for (i in seq_along(IndexMSEList)) {
  Years <- seq(2025, by=1, length.out=30)
  PlotList[[i]] <- data.frame(Year=Years,
                              SB_SBMSY=IndexMSEList[[i]]@SB_SBMSY[Sim,1,1,3:32],
                              SB_SBMSYRef=BaseCaseMSE@SB_SBMSY[Sim,1,1,3:32],
                              F_FMSY=IndexMSEList[[i]]@F_FMSY[Sim,1,1,1,3:32],
                              F_FMSYRef=BaseCaseMSE@F_FMSY[Sim,1,1,1,3:32],
                              Catch=IndexMSEList[[i]]@Catch[Sim,1,1,1,3:32],
                              CatchRef=BaseCaseMSE@Catch[Sim,1,1,1,3:32],
                              Dropped=basename(indices[i]))

  # PlotList[[i]] <- data.frame(Year=Years,
  #                             SB_SBMSY=apply(IndexMSEList[[i]]@SB_SBMSY[,1,1,3:32],2, mean),
  #                             SB_SBMSYRef=apply(BaseCaseMSE@SB_SBMSY[,1,1,3:32], 2, mean),
  #                             F_FMSY=apply(IndexMSEList[[i]]@F_FMSY[,1,1,1,3:32], 2, mean),
  #                             F_FMSYRef=apply(BaseCaseMSE@F_FMSY[,1,1,1,3:32], 2, mean),
  #                             Catch=apply(IndexMSEList[[i]]@Catch[,1,1,1,3:32], 2, mean),
  #                             CatchRef=apply(BaseCaseMSE@Catch[,1,1,1,3:32], 2, mean),
  #                             Dropped=basename(indices[i]))

}

PlotDF <- do.call('rbind', PlotList)

ggplot(PlotDF, aes(x=Year)) +
  geom_line(aes(y=SB_SBMSY, color=Dropped)) +
  geom_line(aes(y=SB_SBMSYRef), linetype=2) +
  facet_wrap(~Dropped) +
  expand_limits(y=0) +
  theme_bw() +
  guides(color='none')

ggplot(PlotDF, aes(x=Year)) +
  geom_line(aes(y=F_FMSY, color=Dropped)) +
  geom_line(aes(y=F_FMSYRef), linetype=2) +
  facet_wrap(~Dropped) +
  expand_limits(y=0) +
  theme_bw() +
  guides(color='none')

ggplot(PlotDF, aes(x=Year)) +
  geom_line(aes(y=Catch, color=Dropped)) +
  geom_line(aes(y=CatchRef), linetype=2) +
  facet_wrap(~Dropped) +
  expand_limits(y=0) +
  theme_bw() +
  guides(color='none')




PMDF <- do.call('rbind', PMList)

BaseCaseDF <- data.frame(PM=PMs,
                         BaseCase=lapply(BaseCasePMs, slot, 'Mean') |> unlist()
                         )

PMDF <- dplyr::left_join(PMDF, BaseCaseDF)

PMDF$PM <- factor(PMDF$PM, levels=unique(PMDF$PM), ordered = TRUE)

temp <- PMDF |> tidyr::pivot_longer(cols=BaseCase) |>
  dplyr::select(Dropped=name, Mean=value, PM=PM)

PMDF2 <- dplyr::bind_rows(PMDF, temp)


PMDF2 |> dplyr::filter(Dropped=='BaseCase', PM=='PGK_short')
PMDF2 |> dplyr::filter(Dropped=='Canada', PM=='PGK_short')

ggplot(PMDF2 |> dplyr::filter(Dropped!='BaseCase'), aes(x=Dropped, y=Mean, fill=Dropped)) +
  facet_wrap(~PM, scales='free_y', ncol=2) +
  geom_bar(stat='identity') +
  theme_bw() +
  expand_limits(y=c(0,1)) +
  labs(x='Dropped Index', y='Value') +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5))  +
  geom_hline(data=PMDF2|> dplyr::filter(Dropped=='BaseCase'),
             aes(yintercept = Mean), linetype=2, color='black') +
  guides(fill='none')

ggsave('ECP/Figures/Drop1Index.png')



DF <- PMDF |>
  dplyr::group_by(PM, Dropped) |>
  dplyr::summarise(Ratio=(Mean/BaseCase-1) * 100)

ggplot(DF, aes(x=Dropped, y=Ratio, fill=Dropped)) +
  facet_wrap(~PM, ncol=2) +
  geom_bar(stat='identity') +
  theme_bw() +
  expand_limits(y=c(-15,15)) +

  geom_hline(yintercept = 0, linetype=2, color='darkgray') +
  labs(x='Dropped Index', y='Relative Change (%)') +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  guides(fill='none')

ggsave('ECP/Figures/Drop1IndexRelative.png')










# plot some examples ...
c1 <- apply(BaseCaseMSE@Catch, c(1,5), sum)
c2 <- apply(MSE@Catch, c(1,5), sum)



mean(c1)
mean(c2)

plot(c1[81,])
lines(c2[81,])
