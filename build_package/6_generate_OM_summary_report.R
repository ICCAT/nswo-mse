library(SWOMSE)
library(r4ss)
library(dplyr)
library(ggplot2)

img_dir <- 'img/OM_Summary_Report'
obj_dir <- 'docs/Reports/OM_Summary/2022'

# Generate OM Summary Report ----


OM.root <- 'G:/My Drive/1_Projects/North_Atlantic_Swordfish/OMs'
OMgrid.dir <- file.path(OM.root, "grid_2022")
OMgrid.dirs <- list.dirs(OMgrid.dir, recursive = FALSE)

RepList <- readRDS(file.path(OM.root, '/OM_objects/RepList.rda'))
RepList_All <- RepList

RepList[[1]] <- NULL # drop 2022 base case assessment
OMgrid.dirs <- OMgrid.dirs[-1]

n.OMs <- length(RepList)

## Model Diagnostic Checks ----

### Check Estimated Parameters Near Bounds ----

converti <- function(i) {
  OM.num <- as.character(i)
  OM.num.out <- OM.num
  OM.num.out[nchar(OM.num)==1] <- paste0('00', OM.num[nchar(OM.num)==1])
  OM.num.out[nchar(OM.num)==2] <- paste0('0', OM.num[nchar(OM.num)==2])
  OM.num.out
}

range01 <- function(r) {
  x <- r[1]
  min <- r[2]
  max <- r[3]
  (x-min)/(max-min)
}

Check_Bounds <- function(i, RepList) {
  pars <- RepList[[i]]$estimated_non_dev_parameters %>%
    dplyr::select(Value, Min, Max, SD=Parm_StDev)
  st_pars <- apply(pars, 1, range01)
  # parameters within 1% of bounds
  ind <- as.numeric(which(st_pars<0.01 | st_pars >0.99))
  near_bounds <- pars[ind,]
  names <- rownames(near_bounds)
  near_bounds$Parameter <- names
  rownames(near_bounds) <- NULL
  if (nrow(near_bounds)>0) {
    df <- OM_DF %>% dplyr::filter(OM.num==converti(i))
    return(cbind(df, near_bounds))
  }
}

Check_Bounds_List <- lapply(1:n.OMs, Check_Bounds, RepList=RepList)
Check_Bounds_DF <- do.call('rbind', Check_Bounds_List)
Check_Bounds_DF$dir <- NULL
Check_Bounds_DF <- Check_Bounds_DF %>% relocate(., OM.num)

saveRDS(Check_Bounds_DF, file.path(obj_dir, 'Check_Bounds_DF.rda'))


Parameters <- Check_Bounds_DF$Parameter %>% unique()

Bound_Table <- Check_Bounds_DF %>% group_by(Parameter) %>%
  summarize(nOM=length(unique(OM.num))) %>%
  arrange(desc(nOM))

Check_Bounds_DF %>% dplyr::filter(Parameter ==Parameters[6])


### Check for Convergence: Invertible Hessian and Final Gradient ----

Check_Convergence <- function(i, RepList) {
  replist <- RepList[[i]]
  log_det_hessian <- replist$log_det_hessian
  max_final_gradient <- replist$maximum_gradient_component
  df <- data.frame(log_det_hessian=log_det_hessian,
             invertible=is.finite(log_det_hessian),
             max_final_gradient=max_final_gradient,
             high_grad=max_final_gradient>0.0001)

  df2 <- replist$parameters_with_highest_gradients
  df2$Parameter <-rownames(df2)
  rownames(df2) <- NULL
  df3 <- OM_DF %>% dplyr::filter(OM.num==converti(i))
  cbind(df3, df, df2)
}

Check_Convergence_List <- lapply(1:n.OMs, Check_Convergence, RepList=RepList)
Check_Convergence_DF <- do.call('rbind', Check_Convergence_List)
Check_Convergence_DF$dir <- NULL
Check_Convergence_DF <- Check_Convergence_DF %>% relocate(., OM.num)
Check_Convergence_DF <- Check_Convergence_DF %>%
  mutate(across(c(1:7, ncol(Check_Convergence_DF)), as.factor))
saveRDS(Check_Convergence_DF, file.path(obj_dir, 'Check_Convergence_DF.rda'))


Check_Convergence_DF %>% filter(invertible==FALSE)
# all invertible

high_grads <- Check_Convergence_DF %>% filter(high_grad==TRUE)

high_grads %>% summarise(OM.n=length(unique(OM.num)))

grad_table <- high_grads %>% group_by(Parameter) %>%
  summarize(nOM=length(unique(OM.num)),
            max_gradient=max(abs(Gradient))) %>%
  arrange(desc(nOM))

grad_table$Parameter %>% unique()

high_grads %>% filter(Parameter=="SR_LN(R0)")

high_grads %>% group_by(M) %>% summarize(n=length(unique(OM.num)))


### Check Correlations ----
Check_Correlations <- function(i, RepList) {
  cor_df <- RepList[[i]]$corstats$cormessage3
  if (!is.null(cor_df)) {
    cor_df$OM.num <- converti(i)
    cor_df <- cor_df %>% relocate(OM.num) %>%
      mutate(across(1:3, as.factor))
    colnames(cor_df)[2] <- 'Parameter i'
    colnames(cor_df)[3] <- 'Parameter j'
    colnames(cor_df)[4] <- 'Correlation'
    return(cor_df)
  }
}

Check_Correlation_List <- lapply(1:n.OMs, Check_Correlations, RepList=RepList)
Check_Correlation_DF <- do.call('rbind', Check_Correlation_List)
rownames(Check_Correlation_DF) <- NULL
saveRDS(Check_Correlation_DF, file.path(obj_dir, 'Check_Correlation_DF.rda'))


Check_Correlation_DF %>% summarise(OM.n=length(unique(OM.num)))


## Biological Reference Points ----

RefPointDF <- readRDS(file.path(OM.root, '/OM_objects/RefPoint_DF.rda'))
RefPointDF$i <- RefPointDF$i-1
RefPointDF$OM.num <- converti(RefPointDF$i)
RefPointDF <- left_join(OM_DF, RefPointDF, by='OM.num')
RefPoint_BC <-  RefPointDF %>% dplyr::filter(OM.num=='000')
RefPointDF2 <- RefPointDF %>% dplyr::filter(OM.num!='000') # drop base case assessment

RefPointDF2$SB0 <- RefPointDF2$SBMSY / RefPointDF2$SBMSY_SB0
RefPointDF2$SB_SB0 <- RefPointDF2$Depletion

saveRDS(RefPointDF2, file.path(obj_dir, 'ReferencePoint_DF.rda'))

### Summary Table of Calculated Reference Points ----
RefPointDF3 <- RefPointDF2 %>% select(OM.num, SB0, SBMSY, FMSY, MSY_d, MSY_r)
RefPointDF3$OM.num <- factor(RefPointDF3$OM.num)

### Boxplots by Axes ----
ReferencePoint_DF <- RefPointDF2 %>% mutate(across(1:6, as.factor))

ref_points <- c('SB0', 'SBMSY', 'FMSY', 'MSY_r')
factors <- colnames(ReferencePoint_DF)[c(2,4:6)]
fact_label <- c('sigmaR',
                'Relative CPUE Weighting',
                'Assumed historical increase in catchability',
                'Include AMO Covariate')
plot_list <-  fig_name_list <- list()

for (i in seq_along(ref_points)) {
  ref <- ref_points[i] %>% as.character()
  plot_list[[i]] <- list()
  fig_name_list[[i]] <- list()

  for (j in seq_along(factors)) {
    var <- factors[j]  %>% as.character()
    p <- ggplot(ReferencePoint_DF, aes_string(x=var, y=ref, fill=var)) +
      facet_grid(steepness~M,) +
      expand_limits(y=0) +
      geom_boxplot() +
      theme_bw() +
      guides(fill='none')
    plot_list[[i]][[j]] <- p

    name <- paste0('Boxplot_', ref, "_", var, '.png')
    fig_name_list[[i]][[j]] <- name
  }
}

# save images
saveRDS(fig_name_list,file.path(obj_dir, 'ref_plot_list.rda'))

for (i in seq_along(ref_points)) {
  for (j in seq_along(factors)) {
    nm <- fig_name_list[[i]][[j]]
    ggsave(file.path(img_dir, nm), plot_list[[i]][[j]], width=6, height=6)

  }
}

## Stock Status ----

### Summary Table ----

ReferencePoint_DF <- RefPointDF2 %>% mutate(across(1:6, as.factor))

ref_points <- c('SB_SB0', 'SB_SBMSY', 'F_FMSY')
factors <- colnames(ReferencePoint_DF)[c(2,4:6)]
fact_label <- c('sigmaR',
                'Relative CPUE Weighting',
                'Assumed historical increase in catchability',
                'Include AMO Covariate')
plot_list <-  fig_name_list <- list()

for (i in seq_along(ref_points)) {
  ref <- ref_points[i] %>% as.character()
  plot_list[[i]] <- list()
  fig_name_list[[i]] <- list()

  for (j in seq_along(factors)) {
    var <- factors[j]  %>% as.character()
    p <- ggplot(ReferencePoint_DF, aes_string(x=var, y=ref, fill=var)) +
      facet_grid(steepness~M,) +
      expand_limits(y=0) +
      geom_boxplot() +
      theme_bw() +
      guides(fill='none')
    plot_list[[i]][[j]] <- p

    name <- paste0('Boxplot_status_', ref, "_", var, '.png')
    fig_name_list[[i]][[j]] <- name
  }
}

# save images
saveRDS(fig_name_list,file.path(obj_dir, 'status_ref_plot_list.rda'))

for (i in seq_along(ref_points)) {
  for (j in seq_along(factors)) {
    nm <- fig_name_list[[i]][[j]]
    ggsave(file.path(img_dir, nm), plot_list[[i]][[j]], width=6, height=6)

  }
}


### Tri-variate SSB/SSB_MSY Time-Series Plots ----
TSBio_List <- readRDS(file.path(OM.root, '/OM_objects/TSBio_List.rda'))
TSBio_List2 <- TSBio_List
TSBio_List2[[1]] <- NULL # drop base case assessment

Create_TS_DF <- function(i, TSBio_List) {
  ts_df <- TSBio_List[[i]]
  ts_df$OM.num <- converti(i)
  left_join(SWOMSE::OM_DF %>% filter(OM.num==converti(i)), ts_df, by='OM.num')
}

TSBio_List2 <- lapply(1:length(TSBio_List2), Create_TS_DF, TSBio_List=TSBio_List2)
TSBio_DF <- do.call('rbind', TSBio_List2)

# Base Case Assessment DF
BC_TS_DF <- cbind(SWOMSE::OM_DF %>% filter(OM.num==converti(0)),
                      TSBio_List[[1]])

TSBio_DF$BC_SSB_SSBMSY <- BC_TS_DF$B.Bmsy


TSBio_DF <- TSBio_DF %>% mutate(across(1:6, as.factor))

factors <- colnames(TSBio_DF)[c(2,4:6)]
fact_label <- c('sigmaR',
                'Relative CPUE Weighting',
                'Assumed historical increase in catchability',
                'Include AMO Covariate')

library(RColorBrewer)
cols <- RColorBrewer::brewer.pal(3,'Set1')

plot_relSB_TS <- function(DF, cols, factor, fact_label) {
  ggplot(DF,
         aes_string(x='year', y='B.Bmsy',
             color='cpuelambda',
             linetype=factor,
             group='OM.num')) +
    facet_grid(steepness~M) +
    expand_limits(y=c(0,1)) +
    geom_hline(yintercept = 1, linetype=2) +
    geom_hline(yintercept = 0.5, linetype=3) +
    geom_line() +
    geom_line(aes(y=BC_SSB_SSBMSY), linetype=2, color='black') +
    theme_classic() +
    scale_color_manual(values=cols) +
    labs(x="Year", y=expression(SB/SB[MSY]),
         linetype=fact_label,
         color='Relative Weighting of Indices') +
    guides(colour = guide_legend(order = 1),
           linetype = guide_legend(order = 2))
}

# default weighting
p1 <- plot_relSB_TS(TSBio_DF %>% filter(cpuelambda==1),
              cols[2], 'llq', fact_label[3])

# Down-weight CAL
p2 <- plot_relSB_TS(TSBio_DF %>% filter(cpuelambda%in% c(0.05,1)),
              cols[1:2], 'llq', fact_label[3])

# Down-weight Indices
p3 <- plot_relSB_TS(TSBio_DF,
              cols, 'llq', fact_label[3])


ggsave(file.path(img_dir, '1_TS_SB_SBMSY_defaultweighting.png'),
       p1, width=9, height=6)

ggsave(file.path(img_dir, '2_TS_SB_SBMSY_default_up.png'),
       p2, width=9, height=6)

ggsave(file.path(img_dir, '3_TS_SB_SBMSY_default_up_down.png'),
       p3, width=9, height=6)



