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




# ----- Demo Tuning -----------------------------------
MMSE_sub <- Sub_MMSE(MSE_all, MPs=MPs_all[c(2, 4:length(MPs_all))])


p1 <- TradeOff(MMSE_sub, PMs=c('PGK_short', 'nLRP'), quants=NULL,
               xlim=0.51, ylim=0.85,
               vline=c(0.51, 0.6, 0.7),
               hline=c(0.85, 0.90, 0.95),
               inc.labels = FALSE,
               pt.size=4, inc.leg = FALSE)
p1

ggsave('img/June_2023/Tune_PGK_short.png', width=7, height=6)

## --- Show Trade-Off Space -----

MMSE_sub <- Sub_MMSE(MSE_all, MPs=c('SPS_a', 'CI1_a',
                                    'SPS_b', 'CI1_b',
                                    'SPS_c', 'CI1_c'))

p1 <- TradeOff(MMSE_sub, PMs=c('PGK_short', 'nLRP'), quants=NULL,
         xlim=0.51, ylim=0.85,
         vline=c(0.51, 0.6, 0.7),
         hline=c(0.85, 0.90, 0.95),
         inc.labels = FALSE,
         pt.size=4,
         inc.line=TRUE,
         subset =TRUE,
         inc.leg = FALSE) +
  theme(axis.title.x = element_blank())

p2 <- TradeOff(MMSE_sub, PMs=c('PGK_med', 'nLRP'), quants=NULL,
         xlim=0.51, ylim=0.85,
         vline=c(0.51, 0.6, 0.7),
         hline=c(0.85, 0.90, 0.95),
         inc.labels = FALSE,
         pt.size=4,
         inc.line=TRUE,
         subset =TRUE,
         inc.leg = FALSE) +
  theme(axis.title = element_blank())

p3 <- TradeOff(MMSE_sub, PMs=c('PGK_long', 'nLRP'), quants=NULL,
         xlim=0.51, ylim=0.85,
         vline=c(0.51, 0.6, 0.7),
         hline=c(0.85, 0.90, 0.95),
         inc.labels = FALSE,
         pt.size=4,
         inc.line=TRUE,
         subset =TRUE,
         inc.leg = FALSE) +
  theme(axis.title = element_blank())


p4 <- TradeOff(MMSE_sub, PMs=c('PGK_short', 'AvTAC_short'), quants=NULL,
         xlim=0.51,
         vline=c(0.51, 0.6, 0.7),
         inc.labels = FALSE,
         pt.size=4,
         inc.line=TRUE,
         subset =TRUE,
         inc.leg = FALSE)

p5 <- TradeOff(MMSE_sub, PMs=c('PGK_med', 'AvTAC_med'), quants=NULL,
         xlim=0.51,
         vline=c(0.51, 0.6, 0.7),
         inc.labels = FALSE,
         pt.size=4,
         inc.line=TRUE,
         subset =TRUE,
         inc.leg = FALSE)

p6 <- TradeOff(MMSE_sub, PMs=c('PGK_long', 'AvTAC_long'), quants=NULL,
         xlim=0.51,
         vline=c(0.51, 0.6, 0.7),
         inc.labels = FALSE,
         pt.size=4,
         inc.line=TRUE,
         subset =TRUE,
         inc.leg = FALSE)

pout <- cowplot::plot_grid(p1, p2, p3, p4, p5, p6, nrow=2, align='v')

pout

ggsave('img/June_2023/Tune_PGY.png', width=15, height=8)

## ---- MP Filtering ----
PM_results <- PM_table(MSE_all)

pass_MPs <- PM_results %>% filter(Name=='LRP', Value<=0.15)

MPs <- pass_MPs$MP %>% unique()

PM_results <- PM_results %>% filter(MP %in% MPs)
MSE_pass <- Sub_MMSE(MSE_all, MPs=MPs)




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

# Short

makeplot <- function(MSE, PMs, greater=c(TRUE, TRUE), subset=TRUE) {
  non_dom <- Calc_Dominated(PMs[1], PMs[2], PM_results, greater=greater)

  p1 <- TradeOff(MSE_all, PMs=c(PMs[1], PMs[2]), quants=NULL,
           xlim=0.51,
           vline=c(0.51, 0.6, 0.7),
           inc.labels = FALSE,
           pt.size=4,
           inc.line=FALSE,
           subset = subset,
           inc.leg = FALSE)

  MSE_sub <- Sub_MMSE(MSE_all, MPs=non_dom)

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

ggsave('img/June_2023/Filter_Dominated.png', width=15, height=8)



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

ggsave('img/June_2023/Final_TO.png', width=15, height=4)



# ------------- Variability --------------
VarC_Violin(MSE_final)

ggsave('img/June_2023/Violin.png', width=12, height=4)

VarC_Violin(MSE_final, 1:2)

ggsave('img/June_2023/Violin_2.png', width=12, height=4)


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



makeplot2 <- function(MSE, PMs, greater=c(TRUE, TRUE), subset=TRUE) {
  non_dom <- Calc_Dominated(PMs[1], PMs[2], PM_results, greater=greater)

  p1 <- TradeOff(MSE_all, PMs=c(PMs[1], PMs[2]), quants=NULL,
                 inc.labels = FALSE,
                 pt.size=4,
                 inc.line=FALSE,
                 subset = subset,

                 inc.leg = FALSE, ymax=0.25)

  MSE_sub <- Sub_MMSE(MSE_all, MPs=non_dom)

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
  plot_list2[[names(PM_list)[i]]] <- makeplot2(MSE_pass, PMs, greater = c(TRUE, FALSE),
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


ggsave('img/June_2023/Filter_VarC.png', width=16, height=8)


Final_MPs_2 <- union(union(union(plot_list2$TAC1$non_dom,
                               plot_list2$short$non_dom),
                         plot_list2$med$non_dom),
                   plot_list2$long$non_dom)


MSE_final_2 <- Sub_MMSE(MSE_final, MPs=Final_MPs_2)




p1 <- TS_plot(MSE_final_2, relY=FALSE) +
  theme(axis.text.x=element_text(angle=90))
ggsave('img/June_2023/TS_plot.png', p1, width=16, height=4)


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
ggsave('img/June_2023/Kobe_time.png', width=10, height=6)
