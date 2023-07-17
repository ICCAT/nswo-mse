library(SWOMSE)

MSE.dir <- 'MSE_Objects'

Ref_OMs <- OM_DF %>% filter(Class=='Reference')

MSElist <- list()
for (i in 1:nrow(Ref_OMs)) {
  nm <- paste0(Ref_OMs$OM.object[i], '.mse')
  if (file.exists(file.path(MSE.dir, nm))) {
    MSElist[[i]] <- readRDS(file.path(MSE.dir, nm))
  }
}

MSE_all <- combine_MMSE(MSElist, 'Reference OMs')

MPs_all <- MSE_all@MPs[[1]]

MPs_all %>% sort()
MPs_all[c(2, 4:length(MPs_all))]


mmse <- Sub_MMSE(MSE_all, MPs=c('CI1_c', 'SPS_a'))

LRP(mmse)


mmse <- Sub_MMSE(MSE_all, MPs=c('SPS_a', 'SPS_b', 'SPS_c',
                                'SPFox_a', 'SPFox_b', 'SPFox_c',
                                'SPFox25_a', 'SPFox25_b', 'SPFox25_c'))

TradeOff(mmse, PMs=c('PGK_med', 'AvTAC_med'), quants=NULL,
         xlim=0.51, ylim=0.85,
         vline=c(0.51, 0.6, 0.7),
         hline=c(0.85, 0.90, 0.95),
         inc.labels = TRUE,
         pt.size=4, inc.leg = FALSE)



## ---- MP Filtering ----
PM_results <- PM_table(MSE_all)

pass_MPs <- PM_results %>% filter(Name=='LRP', Value<=0.15)

# Fail LRP 0.15
failMPs <- MPs_all[!MPs_all %in% pass_MPs$MP]
MSE_fail <- Sub_MMSE(MSE_all, MPs=failMPs)

p1 <- TradeOff(MSE_fail, PMs=c('PGK_short', 'nLRP'), quants=NULL,
               xlim=0.51, ylim=0.85,
               vline=c(0.51, 0.6, 0.7),
               hline=c(0.85, 0.90, 0.95),
               inc.labels = FALSE,
               pt.size=4, inc.leg = FALSE)
p1


TS_plot(MSE_fail)
ggsave('img/July_2023/Fail_LRP.png', width=12, height=4)

# ---- CI1_a in detail ----
df_list <- list()
for (i in 1:9) {
  df_list[[i]] <- data.frame(i=i, LRP=LRP(MSElist[[i]])@Mean[6])
}
do.call('rbind', df_list)



MSE_fail_1 <- Sub_MMSE(MSElist[[2]], MPs=failMPs)

TS_plot(MSE_fail_1)




mm <- 5
MMSE <- MSE_fail_1
MMSE@MPs[[1]]
MMSE@MPs[[1]][mm]

All_Data <- MMSE@PPD[[1]][[1]][[mm]]

x <- 10
Imp_Years <- seq(2024, by=3, to=2053)
df_list <- list()
for (y in seq_along(Imp_Years)) {
  year <- Imp_Years[y]
  Data <- Trim_Data(All_Data, year-1)

  TACs <- colSums(MMSE@TAC[x,,1,mm,])

  ind <- which(All_Data@Year==year)-which(All_Data@Year==2020)

  Data@MPrec[x] <- TACs[ind]
  Mod <- CI1_b(x, Data)

  All_Years <- get_Years(MMSE)
  # Biomass
  B_Hist <- rowSums(MMSE@multiHist[[1]][[1]]@TSdata$Biomass[x,,]) +
    rowSums(MMSE@multiHist[[2]][[1]]@TSdata$Biomass[x,,])
  B_Proj <- apply(MMSE@B[x,,mm,], 2, sum)
  B <- data.frame(Year=All_Years$Year, B=c(B_Hist, B_Proj)) %>%
    filter(Year <=year)

  # Index <- Mod@Misc[[x]]$Index_raw
  Index <- Mod@Misc[[x]]$Index_fitted

  Index_df <- data.frame(Year=All_Years$Year[1:length(Index)],
                         Index=Index,
                         Target=Mod@Misc[[x]]$Ind_Target)

  c_hist <- rowSums(MMSE@multiHist[[1]][[1]]@TSdata$Landings[x,,]) +
    rowSums(MMSE@multiHist[[2]][[1]]@TSdata$Landings[x,,])
  c_proj <- colSums(MMSE@TAC[x,,1,mm,])
  Catch_df <- data.frame(Year=All_Years$Year, TAC=c(c_hist, c_proj)) %>%
    filter(Year<=year)

  Mod@Misc[[x]]
  Mod@TAC


  df <- left_join(B, Index_df, by = join_by(Year))
  df <- left_join(df, Catch_df, by = join_by(Year))
  df$Imp <- year

  df2 <- df %>% tidyr::pivot_longer(., cols=c(B, Index, TAC))
  df_list[[y]] <- df2

}

df <- do.call('rbind', df_list)
df$Imp <- factor(df$Imp)
df$name <- factor(df$name, levels=c('B', 'Index', 'TAC'), ordered=TRUE)
ggplot(df) +
  facet_grid(name~Imp, scales='free_y') +
  expand_limits(y=0) +
  geom_line(aes(x=Year, y=value)) +
  geom_hline(aes(yintercept=Target), linetype=2) +
  theme_bw() +
  labs(color='TAC Year')






Step_MP <- function(MMSE, mm, sim=1) {

  Last_Data <- MMSE@PPD[[1]][[1]][[mm]]
  All_Years <- get_Years(MMSE)
  tyears <- length(All_Years$Year)

  # Biomass
  B_Hist <- rowSums(MMSE@multiHist[[1]][[1]]@TSdata$Biomass[sim,,]) +
    rowSums(MMSE@multiHist[[2]][[1]]@TSdata$Biomass[sim,,])
  B_Proj <- apply(MMSE@B[sim,,mm,], 2, sum)
  B <- data.frame(Year=All_Years$Year, B=c(B_Hist, B_Proj))

  # Index
  I <- data.frame(Year=All_Years$Year[1:(tyears-1)], Index=Last_Data@Ind[sim,])

  # TAC
  C_Hist <- rowSums(MMSE@multiHist[[1]][[1]]@TSdata$Landings[sim,,]) +
    rowSums(MMSE@multiHist[[2]][[1]]@TSdata$Landings[sim,,])
  TAC <- data.frame(Year=All_Years$Year, TAC=c(C_Hist, apply(MMSE@TAC[sim,,1,mm,], 2, sum)))

  p1 <- ggplot(B, aes(x=Year, y=B)) +
    expand_limits(y=0) +
    geom_line() +
    labs(y='Biomass') +
    theme_bw()

  p2 <- ggplot(I, aes(x=Year, y=Index)) +
    expand_limits(y=0) +
    geom_line() +
    labs(y='Index') +
    theme_bw()

  p3 <- ggplot(TAC, aes(x=Year, y=TAC)) +
    expand_limits(y=0) +
    geom_line() +
    labs(y='TAC') +
    theme_bw()

  out <- list()
  out$p <- cowplot::plot_grid(p1, p2, p3, ncol=1)
  out$B <- B
  out$I <- I
  out$TAC <- TAC
  out


}


# ---- Only CMPs that pass LRP ----
MSE_pass <- Sub_MMSE(MSE_all, MPs=pass_MPs$MP)


p1 <- TradeOff(MSE_pass, PMs=c('PGK_short', 'nLRP'), quants=NULL,
               xlim=0.51, ylim=0.85,
               vline=c(0.51, 0.6, 0.7),
               hline=c(0.85, 0.90, 0.95),
               inc.labels = FALSE,
               pt.size=4, inc.leg = FALSE)
p1


# ---- Dominated for PGK and TAC ----
is.dominated <- function(x, y, greater=TRUE) {
  if (greater)
    sdf <- x %>% filter(Value>=y$Value)
  if (!greater)
    sdf <- x %>% filter(Value<=y$Value)
  sdf$MP
}

Calc_Dominated <- function(PM1, PM2, PM_results, greater=c(TRUE, TRUE)) {
  PMs <- c(PM1, PM2)
  df <- PM_results %>% filter(Name %in% PMs)
  MPs <- unique(df$MP)
  dom_MP_list <- list()
  for (mm in seq_along(MPs)) {
    tdf <- df %>% filter(MP==MPs[mm])
    dom_MP_list[[MPs[mm]]] <- list()
    for (pm in 1:2) {
      x <- df %>% filter( Name==PMs[pm])
      y <- tdf %>% filter( Name==PMs[pm])
      dom_MP <- is.dominated(x,y, greater=greater[pm])

      dom_MP_list[[MPs[mm]]][[PMs[[pm]]]] <- dom_MP[!dom_MP==MPs[mm]]
    }
    dom_MP_list[[MPs[mm]]] <- intersect(dom_MP_list[[MPs[mm]]][[1]], dom_MP_list[[MPs[mm]]][[2]] )
  }
  names(which(lapply(dom_MP_list, length)==0))
}

PM_results <- PM_table(MSE, msg=FALSE)
makeplot <- function(MSE, PMs, greater=c(TRUE, TRUE), subset=TRUE) {

  non_dom <- Calc_Dominated(PMs[1], PMs[2], PM_results, greater=greater)

  p1 <- TradeOff(MSE, PMs=c(PMs[1], PMs[2]), quants=NULL,
                 xlim=0.51,
                 vline=c(0.51, 0.6, 0.7),
                 inc.labels = FALSE,
                 pt.size=4,
                 inc.line=FALSE,
                 subset = subset,
                 inc.leg = FALSE)

  MSE_sub <- Sub_MMSE(MSE, MPs=non_dom)

  p2 <- TradeOff(MSE_sub, PMs=c(PMs[1], PMs[2]), quants=NULL,
                 xlim=0.51,
                 vline=c(0.51, 0.6, 0.7),
                 inc.labels = FALSE,
                 pt.size=4,
                 subset = subset,
                 inc.line=FALSE,
                 inc.leg = FALSE)
  out <- list()
  out$p1 <- p1
  out$p2 <- p2
  out$non_dom <- non_dom
  out
}

PM_list <-  list(short=c("PGK_short", "AvTAC_short"),
                 medium=c("PGK_med", "AvTAC_med"),
                 long=c("PGK_long", "AvTAC_long"))

plot_list <- list()
for (i in 1:length(PM_list)) {
  PMs <- PM_list[[i]]

  plot_list[[names(PM_list)[i]]] <- makeplot(MSE_pass, PMs)
}

p <- cowplot::plot_grid(plot_list$short$p1 +
                          theme(axis.title.x=element_blank()),
                        plot_list$med$p1 +
                          theme(axis.title.x=element_blank()),
                        plot_list$long$p1 +
                          theme(axis.title.x=element_blank()),
                        plot_list$short$p2,
                        plot_list$med$p2 ,

                        plot_list$long$p2,
                        nrow=2, align='v')

p

ggsave('img/July_2023/Filter_Dominated.png', width=15, height=8)



Final_MPs <- union(union(plot_list$short$non_dom,
                         plot_list$med$non_dom),
                   plot_list$long$non_dom)


MSE_final <- Sub_MMSE(MSE_all, MPs=Final_MPs)

p1 <- TradeOff(MSE_final, PMs=c('PGK_short', 'AvTAC_short'), quants=NULL,
               xlim=0.51,
               vline=c(0.51, 0.6, 0.7),
               inc.labels = FALSE,
               pt.size=4,
               subset = TRUE,
               inc.line=FALSE,
               inc.leg = FALSE)

p2 <- TradeOff(MSE_final, PMs=c('PGK_med', 'AvTAC_med'), quants=NULL,
               xlim=0.51,
               vline=c(0.51, 0.6, 0.7),
               inc.labels = FALSE,
               pt.size=4,
               subset = TRUE,
               inc.line=FALSE,
               inc.leg = FALSE)


p3 <- TradeOff(MSE_final, PMs=c('PGK_long', 'AvTAC_long'), quants=NULL,
               xlim=0.51,
               vline=c(0.51, 0.6, 0.7),
               inc.labels = FALSE,
               pt.size=4,
               subset = TRUE,
               inc.line=FALSE,
               inc.leg = FALSE)

p <- cowplot::plot_grid(p1,
                        p2,
                        p3,
                        nrow=1, align='v')

ggsave('img/July_2023/Final_TO.png', width=15, height=4)

Final_MPs %>% sort()

mps <- MSE_final@MPs[[1]]
mps <- mps[!mps=='NFref']
MSE_final_dropNFref <- Sub_MMSE(MSE_final, MPs=mps)
p1 <- TS_plot(MSE_final_dropNFref, relY=FALSE) +
  theme(axis.text.x=element_text(angle=90))
ggsave('img/July_2023/TS_plot_all.png', p1, width=16, height=4)


# ------------- Variability --------------
VarC_Violin(MSE_final)

ggsave('img/July_2023/Violin.png', width=12, height=4)

p1 <- TradeOff(MSE_final, PMs=c('VarC', 'TAC1'), quants=NULL,
               inc.labels = FALSE,
               pt.size=4,
               subset = TRUE,
               inc.line=FALSE,
               inc.leg = FALSE)

p2 <- TradeOff(MSE_final, PMs=c('VarC', 'AvTAC_short'), quants=NULL,
               inc.labels = FALSE,
               pt.size=4,
               subset = TRUE,
               inc.line=FALSE,
               inc.leg = FALSE)

p3 <- TradeOff(MSE_final, PMs=c('VarC', 'AvTAC_med'), quants=NULL,
               inc.labels = FALSE,
               pt.size=4,
               subset = TRUE,
               inc.line=FALSE,
               inc.leg = FALSE)

p4 <- TradeOff(MSE_final, PMs=c('VarC', 'AvTAC_long'), quants=NULL,
               inc.labels = FALSE,
               pt.size=4,
               subset = TRUE,
               inc.line=FALSE,
               inc.leg = FALSE)

p <- cowplot::plot_grid(p1,p2,p3,p4,
                        nrow=2, align='v')

p


PM_results <- PM_table(MSE_final, msg=FALSE)
makeplot2 <- function(MSE, PMs, greater=c(TRUE, TRUE), subset=TRUE) {

  non_dom <- Calc_Dominated(PMs[1], PMs[2], PM_results, greater=greater)

  p1 <- TradeOff(MSE, PMs=c(PMs[1], PMs[2]), quants=NULL,
                 inc.labels = FALSE,
                 pt.size=4,
                 inc.line=FALSE,
                 subset = subset,

                 inc.leg = FALSE, ymax=0.25)

  MSE_sub <- Sub_MMSE(MSE, MPs=non_dom)

  p2 <- TradeOff(MSE_sub, PMs=c(PMs[1], PMs[2]), quants=NULL,
                 inc.labels = FALSE,
                 pt.size=4,
                 subset = subset,
                 inc.line=FALSE,
                 inc.leg = FALSE, ymax=0.25)
  out <- list()
  out$p1 <- p1
  out$p2 <- p2
  out$non_dom <- non_dom
  out
}

PM_list <-  list(TAC1=c("TAC1", "VarC"),
                 short=c("AvTAC_short", "VarC"),
                 medium=c("AvTAC_med", "VarC"),
                 long=c("AvTAC_long", "VarC"))

plot_list2 <- list()
for (i in 1:length(PM_list)) {
  PMs <- PM_list[[i]]
  plot_list2[[names(PM_list)[i]]] <- makeplot2(MSE_final, PMs, greater = c(TRUE, FALSE),
                                               subset = FALSE)
}

p <- cowplot::plot_grid(plot_list2$TAC1$p1 +
                          theme(axis.title.x=element_blank()),
                        plot_list2$short$p1 +
                          theme(axis.title=element_blank()),
                        plot_list2$med$p1 +
                          theme(axis.title=element_blank()),
                        plot_list2$long$p1 +
                          theme(axis.title=element_blank()),

                        plot_list2$TAC1$p2,
                        plot_list2$short$p2 +
                          theme(axis.title.y=element_blank()),
                        plot_list2$med$p2 +
                          theme(axis.title.y=element_blank()),
                        plot_list2$long$p2 +
                          theme(axis.title.y=element_blank()),
                        nrow=2, align='v',
                        rel_heights = c(0.95,1))


ggsave('img/July_2023/Filter_VarC.png', width=16, height=8)


Final_MPs_2 <- union(union(union(plot_list2$TAC1$non_dom,
                                 plot_list2$short$non_dom),
                           plot_list2$med$non_dom),
                     plot_list2$long$non_dom)



Final_MPs_2 %>% length()

MSE_final_2 <- Sub_MMSE(MSE_final, MPs=Final_MPs_2)


TradeOff(MSE_final, PMs=c('VarC', 'AvTAC_med'))





p1 <- TS_plot(MSE_final_2, relY=FALSE) +
  theme(axis.text.x=element_text(angle=90))
ggsave('img/July_2023/TS_plot.png', p1, width=12, height=4)


PM_results_Final  <- PM_results %>% filter(MP %in% Final_MPs_2)



Quilt <- function(PM_results, PMs=NULL) {
  library(DT)
  PM_results$Value <- round(PM_results$Value,2)
  tab <- PM_results %>% select(Name, MP, Value) %>% filter(Name %in% PMs)

  tab$Name <- factor(tab$Name, levels=PMs, ordered = TRUE)
  tab <- tab %>% group_by(Name) %>% arrange()
  tab <- tab  %>%
    tidyr::pivot_wider(., names_from = Name, values_from = Value)


  colfunc <- colorRampPalette(c("blue", "white"), alpha=TRUE)

  # Probability colors
  probs <- seq(0, 1.01, length.out=50)
  prob_colors <- rev(colfunc(length(probs)+1))
  rev_prob_colors <- rev(prob_colors)


  # TAC colors
  TAC_PMs <- PM_results$Name[grepl('TAC', PM_results$Name )] %>% unique()



  # Variability colors


  # Make table
  quilt <-  DT::datatable(tab, options = list(dom = 't', pageLength =20))

  for (i in 2:ncol(tab)) {
    pm <- colnames(tab)[i]

    if (grepl('TAC', pm)) {
      cuts <- seq(min(tab[,i]), max(tab[,i])*1.1, length.out=10)
      values <- rev(colfunc(length(cuts)+1))

    } else if (grepl('VarC', pm)) {
      # variability
      cuts <- seq(0, 1, length.out=10)
      values <- (colfunc(length(cuts)+1))

    } else {
      # probabilities
      cuts <- seq(0, 1.01, length.out=50)
      values <- rev(colfunc(length(cuts)+1))
    }
    quilt <- quilt %>%
      formatStyle(
        pm,
        backgroundColor = styleInterval(cuts=cuts,
                                        values=values)

      )

  }
  quilt
}
PMs <- c('PGK', 'TAC1', 'AvTAC_short', 'AvTAC_med', 'AvTAC_long', 'VarC')

Quilt(PM_results_Final, PMs)


Kobe_Time(MSE_final_2)
ggsave('img/July_2023/Kobe_time.png', width=10, height=6)


# ---- TS plot by OM ----
MPs <- c('SPFox_c', 'EA1_c')

MSElist_2 <- list()
for (i in seq_along(MSElist)) {
  MSElist_2[[i]] <- Sub_MMSE(MSElist[[i]], MP=MPs)
}


TS_plot(MSElist_2[[1]])
TS_plot(MSElist_2[[2]])
TS_plot(MSElist_2[[3]])
TS_plot(MSElist_2[[4]])
TS_plot(MSElist_2[[5]])
TS_plot(MSElist_2[[6]])
TS_plot(MSElist_2[[7]])


x <- which.max(MSElist_2[[5]]@SB_SBMSY[,1,1,33])

dfs <- Step_MP(MSElist_2[[5]], mm=1, sim=x)
ggsave('img/July_2023/SPFox_c.png', dfs$p)
TAC <- dfs$TAC

All_Data <- MSElist_2[[5]]@PPD[[1]][[1]][[1]]

Imp_Years <- seq(2024, by=3, to=2053)
df_list <- list()
for (y in seq_along(Imp_Years)) {

  year <- Imp_Years[y]
  Data <- Trim_Data(All_Data, year-1)
  Mod <- SPFox_c(x, Data)


  B_est <- Mod@Misc[[x]]@B
  Years <- as.numeric(names(B_est))
  tunepar <- formals('SPFox_c')$tunepar
  Ftar <- tunepar * 0.1 #  Mod@FMSY
  Fmin <- 0.1 * Ftar #  * Mod@FMSY
  Bcurr <- B_est[length(B_est)]
  Bthresh=Mod@Misc[[x]]@BMSY
  if (Bcurr>=Bthresh) {
    Fmort <- Ftar
  } else if (Bcurr>Blim) {
    Fmort <- Mod@FMSY * (-0.367 + 1.167*  Bcurr/Bthresh)
  } else {
    Fmort <- Fmin
  }


  c_hist <- rowSums(MSElist_2[[5]]@multiHist[[1]][[1]]@TSdata$Landings[x,,]) +
    rowSums(MMSE@multiHist[[2]][[1]]@TSdata$Landings[x,,])
  c_proj <- colSums(MSElist_2[[5]]@TAC[x,,1,1,])
  Catch_df <- data.frame(Year=All_Years$Year, TAC=c(c_hist, c_proj), Imp=year) %>%
    filter(Year<=year)

  df <- data.frame(Year=Years, B_est=B_est, Bthresh= Bthresh,  Imp=year,
                   B_BMSY_est=B_est/Bthresh,
                   F=Fmort)
  df <- left_join(Catch_df, df)



  df2 <- df %>% tidyr::pivot_longer(., cols=c(B_est, B_BMSY_est, TAC))
  df_list[[y]] <- df2

}



df <- do.call('rbind', df_list)
df$Imp <- factor(df$Imp)

ggplot(df) +
  facet_wrap(~name, scales='free_y') +
  expand_limits(y=0) +
  geom_line(aes(x=Year, y=value, color=Imp)) +
  theme_bw() +
  labs(color='TAC Year')


ggsave('img/July_2023/SPFox.png', width=10, height=3)


df %>% filter(name=='B', Imp==2048, Year>2040)
df %>% filter(name=='B', Imp==2051, Year>2040)


df %>% filter(name=='TAC', Imp==2048, Year>2040)
df %>% filter(name=='TAC', Imp==2051, Year>2040)










# ---- CE_c ----

mps <- paste0('CE_', c('a', 'b', 'c'))
mse <- Sub_MMSE(MSElist[[7]], MPs=mps)
TS_plot(mse)

All_Data <- mse@PPD[[1]][[1]][[3]]


which.max(mse@SB_SBMSY[,1,3,33])

x <- 439



Imp_Years <- seq(2024, by=3, to=2053)
df_list <- list()
for (y in seq_along(Imp_Years)) {
  year <- Imp_Years[y]
  Data <- Trim_Data(All_Data, year-1)
  Mod <- CE_c(x, Data)

  tunepar <- formals('CE_c')$tunepar

  years <- Data@Year[1:(length(Data@Year)-1)]
  data.frame(Year=years, Index= Mod@Misc[[x]]$Data.Ind.x...
             )



  Mod@Misc[[x]]

  B_est <- Mod@Misc[[x]]@B
  Years <- as.numeric(names(B_est))

  Ftar <- tunepar * 0.1 #  Mod@FMSY
  Fmin <- 0.1 * Ftar #  * Mod@FMSY
  Bcurr <- B_est[length(B_est)]
  Bthresh=Mod@Misc[[x]]@BMSY
  if (Bcurr>=Bthresh) {
    Fmort <- Ftar
  } else if (Bcurr>Blim) {
    Fmort <- Mod@FMSY * (-0.367 + 1.167*  Bcurr/Bthresh)
  } else {
    Fmort <- Fmin
  }

  df <- data.frame(Year=Years, B=B_est, Bthresh= Bthresh,  Imp=year,
                   relB=B_est/Bthresh,
                   F=Fmort, TAC=Mod@TAC, Catch=Data@Cat[x,])

  df2 <- df %>% tidyr::pivot_longer(., cols=c(B, relB, Catch))
  df_list[[y]] <- df2

}

df <- do.call('rbind', df_list)
df$Imp <- factor(df$Imp)
ggplot(df) +
  facet_wrap(~name, scales='free_y') +
  expand_limits(y=0) +
  geom_line(aes(x=Year, y=value, color=Imp)) +
  theme_bw() +
  labs(color='TAC Year')









