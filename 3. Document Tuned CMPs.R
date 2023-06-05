library(SWOMSE)

Tune_dir='Tuning_Objects'




get_tune_val <- function(df, PM='PGK_6_10', Target=0.6) {
  df <- df %>% filter(Name==PM) %>% arrange(test_vals) %>%
    distinct(Value, test_vals, MP)

  MP <- unique(df$MP)

  ind <- which.min(abs(df$Value - Target))
  if (df$Value[ind] > Target) {
    tt <- df %>% filter(Value<Target)
    ind2 <- which.min(abs(tt$Value - Target))
    val2 <- tt$test_vals[ind2]
    ind2 <- which(df$test_vals==val2)
  }
  if (df$Value[ind] < Target) {
    tt <- df %>% filter(Value>Target)
    ind2 <- which.min(abs(tt$Value - Target))
    val2 <- tt$test_vals[ind2]
    ind2 <- which(df$test_vals==val2)
  }

  y <- df$test_vals[c(ind, ind2)]
  x <- df$Value[c(ind, ind2)]

  proposed <- try(suppressWarnings(approx(x, y, xout=Target)$y[[1]]), silent=TRUE)

  if (class(proposed)!='try-error') {
    pdf <- bind_rows(df[c(ind, ind2),], data.frame(test_vals=proposed, Value=Target))
  } else {
    pdf <- df
  }

  pdf$PM <- PM
  pdf$Target <- Target
  pdf$MP <- MP
  pdf
}



TuneTargets

fls <- list.files(Tune_dir, pattern='.tune')

MPs <- strsplit(fls, '.tune') %>% unlist()

df_list <- pdf_list <- list()
for (i in seq_along(MPs)) {
  df <- readRDS(file.path(Tune_dir, fls[i]))
  MP_name <- MPs[i]
  MP_family <- (MP_name %>% strsplit(., '_'))[[1]][1]

  MP.file <- paste0('R/', MP_name, '.R')
  if(!file.exists(MP.file))
    stop(MP.file, ' does not exist')

  # loop over TuneTargets
  df_out <- list()
  df_out2 <- list()
  for (j in 1:nrow(TuneTargets)) {
    df_out[[j]] <- get_tune_val(df, PM=TuneTargets$Metric[j], Target=TuneTargets$Target[j])
    tuned <- out[[j]] %>% filter(round(Value,2)==Target)
    if (nrow(tuned)>0) {
      ind <- which.min(abs(tuned$Value-tuned$Target))
      tune_val <- tuned$test_vals[ind]

      df_out2[[j]] <- data.frame(Code= TuneTargets$Code[j],
                             Name=paste(MP_name, TuneTargets$Code[j], sep="_"),
                             Family=MP_family,
                             target=TuneTargets$Target[j],
                             metric=TuneTargets$Metric[j],
                             tunepar=tune_val)

    }
  }
  pdf_list[[i]] <- do.call('rbind', df_out)
  df_list[[i]] <- do.call('rbind', df_out2)

}

pdf <- do.call('rbind', pdf_list)
df <- do.call('rbind', df_list)


# plot
ggplot(pdf, aes(x=test_vals, y=Value)) +
  facet_grid(PM~MP, scales='free') +
  geom_line() +
  expand_limits(y=c(0,1)) +
  geom_hline(aes(yintercept=Target), linetype=2) +
  theme_bw()


MPs <- unique(df$Family)
for (i in seq_along(MPs)) {
  # document
  df_mp <- df %>% filter(Family==MPs[i])
  n <- nrow(df_mp)
  MP_name <- MPs[i]
  MSEtool:::message(n, 'tuned CMPs found for', MP_name)
  MP.file <- paste0('R/', MP_name, '.R')
  MSEtool:::message_info('Writing tuned CMPs to', MP.file)

  txt <- readLines(MP.file)
  begin.tune <- which(grepl('# ---- Tuned CMPs ----', txt))

  if (length(begin.tune)<1) {
    # no tuned MPs exist
    tune_txt <- '\n# ---- Tuned CMPs ----'
    cat(tune_txt,file=MP.file, sep="\n", append = TRUE)
  } else {
    # delete all tuned MPs
    txt[begin.tune:length(txt)] <- ''
    last.line <- max(which(nchar(txt)!=0))
    txt <- txt[1:last.line]
    cat(txt,file=MP.file, sep="\n", append = FALSE)
    tune_txt <- '\n# ---- Tuned CMPs ----'
    cat(tune_txt,file=MP.file, sep="\n", append = TRUE)
  }

  txt <- readLines(MP.file)
  begin.tune <- which(grepl('# ---- Tuned CMPs ----', txt))

  txt.list <- lapply(1:nrow(df_mp), write_rmd, df=df_mp)

  for (i in seq_along(txt.list)) {
    cat(txt.list[[i]],file=MP.file, sep="\n", append = TRUE)
  }

}



