# kobe time

kobe_results <- readRDS('inst/shiny_apps/SWOMSE/data/kobe_results.rda')

df <-kobe_results %>% filter(Model=='Reference',
                             MP %in%c('CE_b',
                                      'FX4_b',
                                      'MCC5_b',
                                      'MCC7_b',
                                      'SPSSFox_b')) %>%
  tidyr::pivot_longer(., cols=6:9)
df$name <- factor(df$name, levels=c('br', 'tr', 'bl', 'tl'), ordered = TRUE)
cols <- c('green', 'orange', 'yellow', 'red')

ggplot(df, aes(x=Year, y=value, fill=name)) +
  facet_grid(~MP) +
  geom_bar(position="stack", stat="identity", width = 1) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values=cols) +
  guides(fill='none') +
  labs(y="Percent of total simulatons (%)",
       x='Projection Year') +
  theme_bw()

ggsave('img/comm/kobe_time.png', width=10, height=2.7)

# time-series plots
summary_TS_results <- readRDS('inst/shiny_apps/SWOMSE/data/summary_TS_results.rda')

df <- summary_TS_results %>% filter(Model=='Reference',
                                    MP %in%c('CE_b',
                                             'FX4_b',
                                             'MCC5_b',
                                             'MCC7_b',
                                             'SPSSFox_b'))
fills=c('#373737', '#363639', '#CDCDCD')
ggplot(df, aes(x=Year)) +
  facet_grid(name~MP, scales='free_y') +
  geom_ribbon(aes(ymin=Lower , ymax=Upper, fill=fill), alpha=0.7) +
  geom_line(aes(y=Median)) +
  expand_limits(y=0) +
  scale_fill_manual(values=fills) +
  guides(fill='none', color='none') +
  labs(x='Projection Year', y='Median (60th, 70th, & 90th percentiles)') +
  theme_bw()

ggsave('img/comm/ts_plot.png', width=10, height=4)


# quilt plot

Quilt <- function(PM_results, PMs=NULL, show_dominated=FALSE) {

  PM_results$Value <- round(PM_results$Value,2)
  tab <- PM_results %>% select(PM, MP, Value) %>% filter(PM %in% PMs)

  tab$PM <- factor(tab$PM, levels=PMs, ordered = TRUE)
  tab <- tab %>% group_by(PM) %>% arrange()
  tab <- tab  %>%
    tidyr::pivot_wider(., names_from = PM, values_from = Value)


  colorRampAlpha <- function(..., n, alpha) {
    colors <- colorRampPalette(...)(n)
    paste(colors, sprintf("%x", ceiling(255*alpha)), sep="")
  }

  cols <- c("blue", "white")


  # Probability colors
  probs <- seq(0, 1.01, length.out=50)
  prob_colors <- rev(colorRampAlpha(cols, n=length(probs)+1, alpha=0.5))
  rev_prob_colors <- rev(prob_colors)

  # TAC colors
  TAC_PMs <- PM_results$Name[grepl('TAC', PM_results$Name )] %>% unique()


  # Variability colors


  # Make table
  tab$MP <- factor(tab$MP)

  # calculate dominated
  if (show_dominated)
    tab <- calc_dominated(tab)



  quilt <-  DT::datatable(tab, extensions = 'Buttons',
                          options = list( dom = 't', pageLength=100),
)

  for (i in 2:ncol(tab)) {
    pm <- colnames(tab)[i]

    if (grepl('TAC', pm)) {
      cuts <- seq(min(tab[,i]), max(tab[,i])*1.1, length.out=10)
      values <- rev(colorRampAlpha(cols, n=length(cuts)+1, alpha=0.5))

    } else if (grepl('VarC', pm)) {
      # variability
      cuts <- seq(0, 1, length.out=100)
      values <- colorRampAlpha(cols, n=length(cuts)+1, alpha=0.5)

    } else {
      # probabilities
      cuts <- seq(0, 1.01, length.out=50)
      values <- rev(colorRampAlpha(cols, n=length(cuts)+1, alpha=0.5) )
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

PM_results <- readRDS('inst/shiny_apps/SWOMSE/data/PM_results.rda') %>%
  filter(Model=='Reference',
         MP_name %in%c('CE',
                  'FX4',
                  'MCC5',
                  'MCC7',
                  'SPSSFox'))


PMs <- c("AvTAC_short", "AvTAC_med", "AvTAC_long", "TAC1",  "nLRP",
         "PNOF", "PGK_short", "PGK_med", "PGK",  "VarC")

PM_results$Value <- signif(PM_results$Value, 4)

dtable <- Quilt(PM_results, PMs)
library(webshot)
html <- "dtable.html"
saveWidget(dtable, html)
webshot(html, "Quilt.png")

