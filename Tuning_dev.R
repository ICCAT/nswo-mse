library(dplyr)
library(ggplot2)

# ---- Scoping ----
objects <- list.files('Tuning_Objects', pattern='.scope')

data.frame(i=1:length(objects), objects)

obj <- readRDS(file.path('Tuning_Objects', objects[1]))

Plot_Scope('SP1')

Plot_Scope('SP3')



# ---- Tuning ----
MP <- 'SP3'

Plot_Tune <- function(MP, PM='PGK_6_10', Target=0.6, Tune_dir='Tuning_Objects', plot=TRUE) {
  fls <- list.files(Tune_dir, pattern='.tune')

  if (!paste0(MP, '.tune') %in% fls) {
    stop(paste0(MP, '.tune'), ' not found in ', Tune_dir )
  } else {
    fl <- fls[fls==paste0(MP, '.tune')]
  }

  df <- readRDS(file.path(Tune_dir, fl))
  df <- df %>% filter(Name==PM)

  ind <- order(abs(df$Value - Target))

  y <- df$test_vals[ind[1:2]]
  x <- df$Value[ind[1:2]]

  proposed <- try(suppressWarnings(approx(x, y, xout=Target)$y[[1]]), silent=TRUE)

  df <- bind_rows(df, data.frame(test_vals=proposed, Value=Target))

  p <- ggplot(df, aes(x=test_vals , y=Value)) +
    geom_point() +
    geom_line() +
    expand_limits(y=c(0,1)) +
    theme_bw() +
    geom_hline(yintercept=Target, linetype=2) +
    labs(x='Tuning Parameter', y=PM) +
    expand_limits(y=c(0,1))
  if (plot)
    print(p)
  proposed

}

Plot_Tune('IR1', PM='LRP', Target=0.15)


objects <- list.files('Tuning_Objects', pattern='.tune')
objects

i <- 1
obj <- readRDS(file.path('Tuning_Objects', objects[i]))

tt <- obj %>% filter(i ==max(i), Name=='PGK_6_10') %>% select(MP, Value, test_vals)
tt



