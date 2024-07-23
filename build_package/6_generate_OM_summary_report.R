library(SWOMSE)
library(r4ss)
library(dplyr)
library(ggplot2)

img_dir <- 'img/OM_Summary_Report'
obj_dir <- 'docs/Reports/OM_Summary/2024'

# Generate OM Summary Report ----

OM.root <- 'G:/My Drive/1_Projects/North_Atlantic_Swordfish/OMs'
OMs.dir <- file.path(OM.root, '2024_OMs')
OM.obj <- file.path(OM.root, 'OM_objects')
Replist.dir <- file.path(OM.obj, 'Replists')
RefPoint.dir <- file.path(OM.obj, 'Ref_Points')

## Model Diagnostic Checks ----

### Check Estimated Parameters Near Bounds ----


range01 <- function(r) {
  x <- r[1]
  min <- r[2]
  max <- r[3]
  (x-min)/(max-min)
}

Check_Bounds <- function(i) {
  dir <- basename(OM_DF$dir[i])
  replist <- readRDS(file.path(Replist.dir, dir, 'replist.rda'))
  pars <- replist$estimated_non_dev_parameters %>%
    dplyr::select(Value, Min, Max, SD=Parm_StDev)
  st_pars <- apply(pars, 1, range01)
  # parameters within 1% of bounds
  ind <- as.numeric(which(st_pars<0.01 | st_pars >0.99))
  near_bounds <- pars[ind,]
  names <- rownames(near_bounds)
  near_bounds$Parameter <- names
  rownames(near_bounds) <- NULL
  if (nrow(near_bounds)>0) {
    df <- OM_DF[i,]
    return(cbind(df %>% select(OM.num, Class),near_bounds))
  }
}

Check_Bounds_List <- lapply(1:nrow(OM_DF), Check_Bounds)
Check_Bounds_DF <- do.call('rbind', Check_Bounds_List)
Check_Bounds_DF$dir <- NULL

saveRDS(Check_Bounds_DF, file.path(obj_dir, 'Check_Bounds_DF.rda'))

Parameters <- Check_Bounds_DF$Parameter %>% unique()
Parameters

Bound_Table <- Check_Bounds_DF %>% group_by(Parameter) %>%
  summarize(nOM=length(unique(OM.num))) %>%
  arrange(desc(nOM))


### Check for Convergence: Invertible Hessian and Final Gradient ----

Check_Convergence <- function(i) {
  dir <- basename(OM_DF$dir[i])
  replist <- readRDS(file.path(Replist.dir, dir, 'replist.rda'))

  # log_det_hessian <- replist$log_det_hessian
  high_grad <- 0.0001
  max_final_gradient <- replist$maximum_gradient_component
  # df <- data.frame(log_det_hessian=log_det_hessian,
  #            invertible=is.finite(log_det_hessian),
  #            max_final_gradient=max_final_gradient,
  #            high_grad=max_final_gradient>high_grad)
  df <- data.frame(max_final_gradient=max_final_gradient,
                   high_grad=max_final_gradient>high_grad)

  df <- df%>% filter(high_grad==TRUE)
  if (nrow(df)>0) {
    df2 <- replist$parameters_with_highest_gradients
    df2$Parameter <-rownames(df2)
    rownames(df2) <- NULL
    df3 <- OM_DF[i,]
    return(cbind(df3, df, df2))
  }
}

Check_Convergence_List <- lapply(1:nrow(OM_DF), Check_Convergence)
Check_Convergence_DF <- do.call('rbind', Check_Convergence_List)
Check_Convergence_DF$dir <- NULL
if (!is.null(Check_Convergence_DF)) {
  Check_Convergence_DF <- Check_Convergence_DF %>% relocate(., OM.num)
  Check_Convergence_DF <- Check_Convergence_DF %>%
    mutate(across(c(1:7, ncol(Check_Convergence_DF)), as.factor))
}
saveRDS(Check_Convergence_DF, file.path(obj_dir, 'Check_Convergence_DF.rda'))


# Check_Convergence_DF %>% filter(invertible==FALSE)
# all invertible
if (!is.null(Check_Convergence_DF)) {
  high_grads <- Check_Convergence_DF %>% filter(high_grad==TRUE)

  high_grads %>% summarise(OM.n=length(unique(OM.num)))

  grad_table <- high_grads %>% group_by(Parameter) %>%
    summarize(nOM=length(unique(OM.num)),
              max_gradient=max(abs(Gradient))) %>%
    arrange(desc(nOM))

  high_grads$Gradient %>% abs() %>%  boxplot()

  high_grads %>% filter(abs(Gradient)>0.02)

  ggplot(high_grads %>% filter(Parameter=='SR_LN(R0)'), aes(x=Gradient)) +
    facet_grid(M~steepness) +
    geom_histogram()

  tt <- high_grads %>% filter(Parameter=="SR_LN(R0)", abs(Gradient)>0.0001) %>% select(Gradient)
  hist(tt$Gradient)

  high_grads %>% group_by(M) %>% summarize(n=length(unique(OM.num)))
}
#
# ### Check Correlations ----
# Check_Correlations <- function(i) {
#   dir <- basename(OM_DF$dir[i])
#   replist <- readRDS(file.path(Replist.dir, dir, 'replist.rda'))
#   cor_df <- replist$corstats$cormessage3
#   if (!is.null(cor_df)) {
#     cor_df$OM.num <- OM_DF$OM.num[i]
#     cor_df <- cor_df %>% relocate(OM.num) %>%
#       mutate(across(1:3, as.factor))
#     colnames(cor_df)[2] <- 'Parameter i'
#     colnames(cor_df)[3] <- 'Parameter j'
#     colnames(cor_df)[4] <- 'Correlation'
#     return(cor_df)
#   }
# }
#
# Check_Correlation_List <- lapply(1:nrow(OM_DF), Check_Correlations)
# Check_Correlation_DF <- do.call('rbind', Check_Correlation_List)
# Check_Correlation_DF <- left_join(OM_DF %>% select(OM.num, Class),
#                                   Check_Correlation_DF)
# rownames(Check_Correlation_DF) <- NULL
# Check_Correlation_DF <- Check_Correlation_DF %>% filter(is.na(Check_Correlation_DF$`Parameter i`)==FALSE)
#
#
# saveRDS(Check_Correlation_DF, file.path(obj_dir, 'Check_Correlation_DF.rda'))


# Check_Correlation_DF %>% summarise(OM.n=length(unique(OM.num)))
#
# Check_Correlation_DF$`Parameter i` %>% unique()
# Check_Correlation_DF$`Parameter j` %>% unique()
#
# Check_Correlation_DF %>% filter(`Parameter i`=='LnQ_base_Age-3(16)')


## Biological Reference Points ----

# Make Reference Point Table
RefPointDF_List <- list()
for (i in 1:nrow(OM_DF)) {
  dir <- basename(OM_DF$dir[i])
  RefPointDF <- readRDS(file.path(RefPoint.dir, dir, 'ref_points.rda'))
  df <- OM_DF[i,] %>% select(-dir)
  RefPointDF_List[[i]] <- cbind(df, RefPointDF)
}
RefPointDF <- do.call('rbind',RefPointDF_List)


RefPointDF$SB0 <- RefPointDF$SBMSY / RefPointDF$SBMSY_SB0
RefPointDF$SB_SB0 <- RefPointDF$Depletion
RefPointDF$SB <- RefPointDF$SB_SB0 * RefPointDF$SB0
ReferencePoint_DF <- RefPointDF
saveRDS(ReferencePoint_DF, file.path(obj_dir, 'ReferencePoint_DF.rda'))

### Summary Table of Calculated Reference Points ----
RefPointDF3 <- RefPointDF %>% select(OM.num, SB0, SBMSY, FMSY, MSY_d, MSY_r, Class)
RefPointDF3$OM.num <- factor(RefPointDF3$OM.num)


## Plot Stock Status ----

getTS <- function(i) {
  dir <- basename(OM_DF$dir[i])
  TSbio <- readRDS(file.path(OM.root, '/OM_objects/Timeseries', dir, 'timeseries.rda'))
  df <- OM_DF[i,]
  df <- df %>% select(-dir)
  cbind(df, TSbio)
}

TSBio_List <- lapply(1:nrow(OM_DF), getTS)
TSBio <- do.call('rbind', TSBio_List)

TSBio <- TSBio %>% mutate(across(1:6, as.factor))

TSBio$Class %>% unique()


BC_TS <- TSBio %>% filter(Class=='Base Case')
cols <- c('B.Bmsy', 'F.Fmsy', 'F', 'SSB', 'Depletion')
cols2 <- paste0('BC_', cols)
bc <- BC_TS  %>% select(cols)
names(bc) <- cols2

DF_TS <- TSBio %>% filter(Class!='Base Case')
# DF_TS <- cbind(DF_TS, bc)
DF_TS$Class <- factor(DF_TS$Class, levels=unique(DF_TS$Class),
                      ordered = TRUE)
Ylabs <- list(expression(SB/SB[MSY]),
              expression(F/F[MSY]),
              'Fishing mortality (F)',
              'SSB',
              expression(SB/SB[0]))
Vars <- c('B.Bmsy', 'F.Fmsy', 'F', 'SSB', 'Depletion')
BC_Vars <- paste0('BC_', Vars)

for (i in seq_along(Vars)) {
  p <- ggplot(DF_TS, aes_string(x='year', y=Vars[i],
                                color='Class'))+
    facet_grid(steepness~M) +
    geom_line() +
    expand_limits(y=c(0)) +
    geom_line() +
    # geom_line(aes_string(y=BC_Vars[i]), linetype=2, color='darkgray') +
    theme_bw() +
    labs(x="Year", y=Ylabs[[i]], color='OM Group') +
    scale_color_brewer(type='div', palette = 'PRGn')


  if (Vars[i] %in% c('B.Bmsy')){
    p <- p +
      geom_hline(yintercept = 1, linetype=2) +
      geom_hline(yintercept = 0.5, linetype=3)
  }

  if (Vars[i] %in% c('F.Fmsy')){
    p <- p +
      geom_hline(yintercept = 1, linetype=2)

  }
  if (Vars[i] %in% c('Depletion')){
    p <- p +
      expand_limits(y=c(0,1))

  }
  name <- paste0(paste(Vars[i], sep='_'), '.png')
  ggsave(file.path(img_dir, name), width=8, height=6)
}


# Compile RMD ----

input <- file.path(obj_dir, 'OM_Summary_Report.Rmd')
output_file <- 'OM_Summary_Report.html'
rmarkdown::render(input,
                  output_format = 'html_document', output_file=output_file,
                  output_dir=obj_dir, quiet=TRUE)
utils::browseURL(file.path(obj_dir,output_file))




#
#
#
#
# library(RColorBrewer)
# cols <- RColorBrewer::brewer.pal(3,'Set1')
#
# plot_relSB_TS <- function(DF, cols, factor, fact_label) {
#   ggplot(DF,
#          aes_string(x='year', y='B.Bmsy',
#                     color='cpuelambda',
#                     linetype=factor,
#                     group='OM.num')) +
#     facet_grid(steepness~M) +
#     expand_limits(y=c(0,1)) +
#     geom_hline(yintercept = 1, linetype=2) +
#     geom_hline(yintercept = 0.5, linetype=3) +
#     geom_line() +
#     geom_line(aes(y=BC_SSB_SSBMSY), linetype=2, color='black') +
#     theme_classic() +
#     scale_color_manual(values=cols) +
#     labs(x="Year", y=expression(SB/SB[MSY]),
#          linetype=fact_label,
#          color='Relative Weighting of Indices') +
#     guides(colour = guide_legend(order = 1),
#            linetype = guide_legend(order = 2))
# }
#
# ggsave(file.path(img_dir, nm), plot_list[[i]][[j]], width=6, height=6)
#
#
#
#
# ### Boxplots by Axes ----
#
#
# ReferencePoint_DF <- ReferencePoint_DF %>% mutate(across(1:6, as.factor))
#
# tt <- ReferencePoint_DF %>% filter(Class=='Reference')
#
# ggplot(tt, aes(x=M, y=FMSY, color=steepness)) +
#   geom_point()
#
# ggplot(ReferencePoint_DF, aes(x=))
#
# ref_points <- c('SB0', 'SBMSY', 'FMSY', 'MSY_r')
# factors <- colnames(ReferencePoint_DF)[c(2,4:6)]
# fact_label <- c('sigmaR',
#                 'Include CAL',
#                 'Assumed historical increase in catchability')
#
# plot_list <-  fig_name_list <- list()
#
# for (i in seq_along(ref_points)) {
#   ref <- ref_points[i] %>% as.character()
#   plot_list[[i]] <- list()
#   fig_name_list[[i]] <- list()
#
#   for (j in seq_along(factors)) {
#     var <- factors[j]  %>% as.character()
#     p <- ggplot(ReferencePoint_DF, aes_string(x=var, y=ref, fill=var)) +
#       facet_grid(steepness~M,) +
#       expand_limits(y=0) +
#       geom_boxplot() +
#       theme_bw() +
#       guides(fill='none')
#     plot_list[[i]][[j]] <- p
#
#     name <- paste0('Boxplot_', ref, "_", var, '.png')
#     fig_name_list[[i]][[j]] <- name
#   }
# }
#
# # save images
# saveRDS(fig_name_list,file.path(obj_dir, 'ref_plot_list.rda'))
#
# for (i in seq_along(ref_points)) {
#   for (j in seq_along(factors)) {
#     nm <- fig_name_list[[i]][[j]]
#     ggsave(file.path(img_dir, nm), plot_list[[i]][[j]], width=6, height=6)
#
#   }
# }
#
# ## Stock Status ----
#
# ### Summary Table ----
#
# ReferencePoint_DF <- RefPointDF2 %>% mutate(across(1:6, as.factor))
#
# ref_points <- c('SB_SB0', 'SB_SBMSY', 'F_FMSY')
# factors <- colnames(ReferencePoint_DF)[c(2,4:6)]
# fact_label <- c('sigmaR',
#                 'Relative CPUE Weighting',
#                 'Assumed historical increase in catchability',
#                 'Include AMO Covariate')
# plot_list <-  fig_name_list <- list()
#
# for (i in seq_along(ref_points)) {
#   ref <- ref_points[i] %>% as.character()
#   plot_list[[i]] <- list()
#   fig_name_list[[i]] <- list()
#
#   for (j in seq_along(factors)) {
#     var <- factors[j]  %>% as.character()
#     p <- ggplot(ReferencePoint_DF, aes_string(x=var, y=ref, fill=var)) +
#       facet_grid(steepness~M,) +
#       expand_limits(y=0) +
#       geom_boxplot() +
#       theme_bw() +
#       guides(fill='none')
#     plot_list[[i]][[j]] <- p
#
#     name <- paste0('Boxplot_status_', ref, "_", var, '.png')
#     fig_name_list[[i]][[j]] <- name
#   }
# }
#
# # save images
# saveRDS(fig_name_list,file.path(obj_dir, 'status_ref_plot_list.rda'))
#
# for (i in seq_along(ref_points)) {
#   for (j in seq_along(factors)) {
#     nm <- fig_name_list[[i]][[j]]
#     ggsave(file.path(img_dir, nm), plot_list[[i]][[j]], width=6, height=6)
#
#   }
# }
#
#
# ### Tri-variate SSB/SSB_MSY Time-Series Plots ----
# TSBio_List <- readRDS(file.path(OM.root, '/OM_objects/TSBio_List.rda'))
# TSBio_List2 <- TSBio_List
# TSBio_List2[[1]] <- NULL # drop base case assessment
#
# Create_TS_DF <- function(i, TSBio_List) {
#   ts_df <- TSBio_List[[i]]
#   ts_df$OM.num <- converti(i)
#   left_join(SWOMSE::OM_DF %>% filter(OM.num==converti(i)), ts_df, by='OM.num')
# }
#
# TSBio_List2 <- lapply(1:length(TSBio_List2), Create_TS_DF, TSBio_List=TSBio_List2)
# TSBio_DF <- do.call('rbind', TSBio_List2)
#
# # Base Case Assessment DF
# BC_TS_DF <- cbind(SWOMSE::OM_DF %>% filter(OM.num==converti(0)),
#                       TSBio_List[[1]])
#
# TSBio_DF$BC_SSB_SSBMSY <- BC_TS_DF$B.Bmsy
#
#
# TSBio_DF <- TSBio_DF %>% mutate(across(1:6, as.factor))
#
# factors <- colnames(TSBio_DF)[c(2,4:6)]
# fact_label <- c('sigmaR',
#                 'Relative CPUE Weighting',
#                 'Assumed historical increase in catchability',
#                 'Include AMO Covariate')
#
# library(RColorBrewer)
# cols <- RColorBrewer::brewer.pal(3,'Set1')
#
# plot_relSB_TS <- function(DF, cols, factor, fact_label) {
#   ggplot(DF,
#          aes_string(x='year', y='B.Bmsy',
#              color='cpuelambda',
#              linetype=factor,
#              group='OM.num')) +
#     facet_grid(steepness~M) +
#     expand_limits(y=c(0,1)) +
#     geom_hline(yintercept = 1, linetype=2) +
#     geom_hline(yintercept = 0.5, linetype=3) +
#     geom_line() +
#     geom_line(aes(y=BC_SSB_SSBMSY), linetype=2, color='black') +
#     theme_classic() +
#     scale_color_manual(values=cols) +
#     labs(x="Year", y=expression(SB/SB[MSY]),
#          linetype=fact_label,
#          color='Relative Weighting of Indices') +
#     guides(colour = guide_legend(order = 1),
#            linetype = guide_legend(order = 2))
# }
#
# # default weighting
# p1 <- plot_relSB_TS(TSBio_DF %>% filter(cpuelambda==1),
#               cols[2], 'llq', fact_label[3])
#
# ggsave(file.path(img_dir, '1_TS_SB_SBMSY_defaultweighting.png'),
#        p1, width=9, height=6)
#
# # Down-weight CAL
# p2 <- plot_relSB_TS(TSBio_DF %>% filter(cpuelambda== 0.05),
#               cols[1], 'llq', fact_label[3])
# ggsave(file.path(img_dir, '2_TS_SB_SBMSY_up.png'),
#        p2, width=9, height=6)
#
# # Default and Down-weight CAL
# p2a <- plot_relSB_TS(TSBio_DF %>% filter(cpuelambda!= 20),
#                     cols[1:2], 'llq', fact_label[3])
# ggsave(file.path(img_dir, '2_TS_SB_SBMSY_default_up.png'),
#        p2a, width=9, height=6)
#
# # Down-weight Indices
# p3 <- plot_relSB_TS(TSBio_DF %>% filter(cpuelambda== 20),
#               cols[3], 'llq', fact_label[3])
# ggsave(file.path(img_dir, '3_TS_SB_SBMSY_down.png'),
#        p3, width=9, height=6)
#
# # All
# p4 <- plot_relSB_TS(TSBio_DF,
#                     cols, 'llq', fact_label[3])
# ggsave(file.path(img_dir, '4_TS_SB_SBMSY_all.png'),
#        p4, width=9, height=6)
#
# ggplot(TSBio_DF, aes(x=year, y=B.Bmsy, group=OM.num, color=cpuelambda)) +
#   facet_grid(steepness~M) +
#   expand_limits(y=0) +
#   geom_line()
#
#
#
#
# p1 <- ggplot(TSBio_DF %>% filter(cpuelambda==1),
#              aes(x=year, y=B.Bmsy, group=OM.num,
#                  color=llq)) +
#   facet_grid(steepness~M) +
#   expand_limits(y=0) +
#   geom_line() +
#   geom_hline(yintercept = 1, linetype=2) +
#   geom_hline(yintercept = 0.5, linetype=3) +
#   geom_line(aes(y=BC_SSB_SSBMSY), linetype=2, color='black') +
#   theme_bw() +
#   labs(x="Year", y='SB/SBMSY')
# p1
#
# ggsave(file.path(img_dir, 'TS_cpuelambda=1.png'),
#        p1, width=9, height=6)
#
# p1 <- ggplot(TSBio_DF %>% filter(cpuelambda==1),
#        aes(x=year, y=B.Bmsy, group=OM.num, color=steepness,
#            linetype=llq)) +
#   facet_grid(~M) +
#   expand_limits(y=0) +
#   geom_line() +
#   geom_hline(yintercept = 1, linetype=2) +
#   geom_hline(yintercept = 0.5, linetype=3) +
#   geom_line(aes(y=BC_SSB_SSBMSY), linetype=2, color='black') +
#   theme_bw() +
#   labs(x="Year", y='SB/SBMSY')
#
# ggsave(file.path(img_dir, 'TS_cpuelambda=1.png'),
#        p1, width=8, height=3)
#
# p2 <- ggplot(TSBio_DF %>% filter(cpuelambda==0.05),
#              aes(x=year, y=B.Bmsy, group=OM.num, color=steepness,
#                  linetype=llq)) +
#   facet_grid(~M) +
#   expand_limits(y=0) +
#   geom_line() +
#   geom_hline(yintercept = 1, linetype=2) +
#   geom_hline(yintercept = 0.5, linetype=3) +
#   geom_line(aes(y=BC_SSB_SSBMSY), linetype=2, color='black') +
#   theme_bw() +
#   labs(x="Year", y='SB/SBMSY')
#
# ggsave(file.path(img_dir, 'TS_cpuelambda=0.05.png'),
#        p2, width=8, height=3)
#
# p3 <- ggplot(TSBio_DF %>% filter(cpuelambda==20),
#              aes(x=year, y=B.Bmsy, group=OM.num, color=steepness,
#                  linetype=llq)) +
#   facet_grid(~M) +
#   expand_limits(y=0) +
#   geom_line() +
#   geom_hline(yintercept = 1, linetype=2) +
#   geom_hline(yintercept = 0.5, linetype=3) +
#   geom_line(aes(y=BC_SSB_SSBMSY), linetype=2, color='black') +
#   theme_bw() +
#   labs(x="Year", y='SB/SBMSY')
#
# ggsave(file.path(img_dir, 'TS_cpuelambda=20.png'),
#        p3, width=8, height=3)
#
#
# # Figures for Presentation ----
#
# ## Parameters at Bounds ----
#
# # Base Case Can Select
# getSelect <- function(i, Year, fleet) {
#   SizeSelect <- RepList_All[[i]]$sizeselex %>% dplyr::rename(index=Fleet, year=Yr)
#   SizeSelect <- SizeSelect %>% tidyr::pivot_longer(6:ncol(SizeSelect),
#                                                    'Length.Class', values_to="Prob")
#   SizeSelect <- left_join(SizeSelect, Fleet_DF, by="index")
#   SizeSelect$Length.Class <- as.numeric(SizeSelect$Length.Class)
#
#   SizeSelect %>% filter(year==Year, Name==fleet, Factor=='Lsel', Sex==1) %>%
#     select(Length=Length.Class, Prob=Prob) %>% mutate(OM.num=converti(i-1))
# }
#
# makeDF <- function(select_df, Year, Fleet) {
#   select_list <- list()
#   for (i in 2:217) {
#     message(i)
#     df <-  getSelect(i, Year, Fleet)
#     if(df$OM.num[1] %in% select_df$OM.num) {
#       df$bound <- TRUE
#     } else {
#       df$bound <- FALSE
#     }
#     select_list[[i]] <- df
#   }
#   DF <- do.call('rbind', select_list)
#   left_join(OM_DF %>% filter(OM.num!='000'), DF, by='OM.num')
# }
#
# makePlot <- function(DF, title) {
#   cols <- c('darkgray', 'blue')
#   cols <- alpha(cols, c(0.6,1))
#   df <- DF %>% group_by(M, steepness, bound) %>% mutate(nB=length(unique(OM.num)))
#   df <- df %>% filter(Length==22.5) %>% select(OM.num, Length, M, steepness, bound, nB)
#   df$Prob <- 0.95
#   df$Prob[df$bound==FALSE] <- 0.75
#
#   ggplot(DF, aes(x=Length, y=Prob, color=bound)) +
#     geom_line(aes(group=OM.num)) +
#     facet_grid(steepness~M) +
#     geom_text(aes(label=nB), data=df, show.legend  = FALSE) +
#     theme_classic() +
#     labs(y='Probability', color='At Bound',
#          title=title) +
#     scale_color_manual(values=cols)
# }
#
#
#
# Parameters
#
# CAN_Select_1950 <- Check_Bounds_DF %>%
#   filter(Parameter=='Size_DblN_ascend_se_CAN_3(3)_BLK1repl_1950')
# CAN_Select_1950_DF <- makeDF(CAN_Select_1950, 1950, 'Canada LL')
# makePlot(CAN_Select_1950_DF2, 'Selectivity: Canada LL (1950)')
# ggsave(file.path(img_dir, 'Size_DblN_ascend_se_CAN_3(3)_BLK1repl_1950.png'), width=6, height=4)
#
# CAN_Select_1993 <-  Check_Bounds_DF %>%
#   filter(Parameter=='Size_DblN_peak_CAN_3(3)_BLK1repl_1993')
# CAN_Select_1993_DF <- makeDF(CAN_Select_1993, 1993, 'Canada LL')
# makePlot(CAN_Select_1993_DF, 'Selectivity: Canada LL (1993)')
# ggsave(file.path(img_dir, 'Size_DblN_peak_CAN_3(3)_BLK1repl_1993.png'), width=6, height=4)
#
#
# SPN_Select_1950 <-  Check_Bounds_DF %>%
#   filter(Parameter=='Size_DblN_ascend_se_SPN_1(1)_BLK1repl_1950')
# SPN_Select_1950_DF <- makeDF(SPN_Select_1950, 1950, 'EU-Spain longline (LL)')
# makePlot(SPN_Select_1950_DF, 'Selectivity: EU-Spain (1950)')
# ggsave(file.path(img_dir, 'Size_DblN_ascend_se_SPN_1(1)_BLK1repl_1950.png'), width=6, height=4)
#
#
# SPN_Select_1993 <-  Check_Bounds_DF %>%
#   filter(Parameter=='Size_DblN_ascend_se_SPN_1(1)_BLK1repl_1993')
# SPN_Select_1993_DF <- makeDF(SPN_Select_1993, 1993, 'EU-Spain longline (LL)')
# makePlot(SPN_Select_1993_DF, 'Selectivity: EU-Spain (1993)')
# ggsave(file.path(img_dir, 'Size_DblN_ascend_se_SPN_1(1)_BLK1repl_1993.png'), width=6, height=4)
#
# SPN_Select_2020 <-  Check_Bounds_DF %>%
#   filter(Parameter=='Size_DblN_top_logit_SPN_1(1)')
# SPN_Select_2020_DF <- makeDF(SPN_Select_2020, 2020, 'EU-Spain longline (LL)')
# makePlot(SPN_Select_2020_DF, 'Selectivity: EU-Spain')
# ggsave(file.path(img_dir, 'Size_DblN_top_logit_SPN_1(1).png'), width=6, height=4)
#
#
# Check_Bounds_DF %>% group_by(Parameter,  cpuelambda) %>%
#   summarize(n=length(unique(OM.num)))
#
# Check_Bounds_DF %>% group_by(Parameter,  cpuelambda) %>%
#   summarize(n=length(unique(OM.num)))
#
#
#
