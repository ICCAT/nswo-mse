
source('ECP/Functions.R')

# Plot Indices ----

Dropped1_Indices <- MakeIndicesDF('Dropped1')
Dropped2_Indices <- MakeIndicesDF('Dropped2')

DF <- dplyr::bind_rows(Dropped1_Indices, Dropped2_Indices)
DF$Run <- DF$Run |> dplyr::case_match('Dropped1'~'Dropped 1 Flag',
                                      'Dropped2'~'Dropped 2 Flags')


ggplot(DF, aes(x=Year)) +
  geom_rect(aes(xmin=1950, xmax=1998, ymin=0, ymax=Inf), alpha=0.5, fill='lightgray') +
  geom_line(aes(y=Index, color=Run, linetype=Run)) +
  geom_line(aes(y=CombinedIndex), linetype=2) +
  facet_wrap(~DroppedFlag) +
  expand_limits(y=0) +
  theme_bw() +
  labs(y='Combined Index', color='', linetype='')

ggplot(DF, aes(x=Year)) +
  geom_rect(aes(xmin=1950, xmax=1998, ymin=0, ymax=Inf), alpha=0.5, fill='lightgray') +
  geom_line(aes(y=Index, color=DroppedFlag)) +
  geom_line(aes(y=CombinedIndex), linetype=2) +
  facet_grid(DroppedFlag~Run) +
  expand_limits(y=0) +
  theme_bw() +
  guides(color='none') +
  labs(y='Combined Index', color='', linetype='')
ggsave('ECP/Figures/CompareIndices.png', width=6, height=8)

ggplot(DF |> dplyr::filter(Year>=1999), aes(x=Year)) +
  geom_line(aes(y=Index, color=Run)) +
  geom_line(aes(y=CombinedIndex), linetype=2) +
  facet_grid(DroppedFlag~Run) +
  expand_limits(y=0) +
  theme_bw() +
  guides(color='none') +
  labs(y='Combined Index', color='', linetype='')

ggsave('ECP/Figures/CompareIndices2.png', width=6, height=8)


# Time Series Plots ----

Dropped1Flags <- Dropped1_Indices$DroppedFlag |> unique()
Dropped2Flags <- Dropped2_Indices$DroppedFlag |> unique()

MSE_Base <- GetMSEObjects('Base')
MSE_Dropped1 <- purrr::map(Dropped1Flags, \(DroppedFlag) GetMSEObjects('Dropped1', DroppedFlag))
names(MSE_Dropped1) <- Dropped1Flags

MSE_Dropped2 <- purrr::map(Dropped2Flags, \(DroppedFlag) GetMSEObjects('Dropped2', DroppedFlag))
names(MSE_Dropped2) <- Dropped2Flags

Dropped1_DF <- MakePlotDF(MSE_Dropped1, MSE_Base, 'Dropped 1 Flag')
Dropped2_DF <- MakePlotDF(MSE_Dropped2, MSE_Base, 'Dropped 2 Flags')


PlotDF <- dplyr::bind_rows(Dropped1_DF, Dropped2_DF) |>
  tidyr::pivot_longer(cols=c(SB_SBMSY, F_FMSY, Catch))

ggplot(PlotDF, aes(x=Year, y=value, color=Dropped, linetype=Dropped)) +
  facet_grid(Run~name) +
  geom_line() +
  expand_limits(y=c(0.9, 1.1)) +
  geom_hline(yintercept = 1, linetype=2) +
  theme_bw() +
  labs(color='Dropped Flag', linetype='Dropped Flag', y='Relative Value')

ggsave('ECP/Figures/TimeSeries1.png', width=6, height=3)


# Calculate PMs for Base Case ----

source('R/PMs.r')
PMs <- c("PGK_short", 'PGK_med',
         'PBMSY', "PNOF",
         "AvCatch",  "VarC")

BaseCase_PMs <- purrr::map(PMs, \(PM) {
  fun <- get(PM)
  PMValues <- fun(MSE_Base)@Mean
  data.frame(PM=PM,
             Value=PMValues |> unlist()
  )
}) |> dplyr::bind_rows() |>
  dplyr::mutate(PM=factor(PM, levels=unique(PM), ordered=TRUE))


Dropped1_PMs <- purrr::map2(MSE_Dropped1, names(MSE_Dropped1), \(MSE, name) {
  purrr::map(PMs, \(PM) {
    fun <- get(PM)
    PMValues <- fun(MSE)@Mean
    data.frame(PM=PM,
               Value=PMValues |> unlist()
    )
  }) |> dplyr::bind_rows() |>
    dplyr::mutate(Dropped=name,
                  PM=factor(PM, levels=unique(PM), ordered=TRUE))
}) |> dplyr::bind_rows() |> dplyr::mutate(Run='Dropped 1 Flag')


Dropped2_PMs <- purrr::map2(MSE_Dropped2, names(MSE_Dropped2), \(MSE, name) {
  purrr::map(PMs, \(PM) {
    fun <- get(PM)
    PMValues <- fun(MSE)@Mean
    data.frame(PM=PM,
               Value=PMValues |> unlist()
    )
  }) |> dplyr::bind_rows() |>
    dplyr::mutate(Dropped=name,
                  PM=factor(PM, levels=unique(PM), ordered=TRUE))
}) |> dplyr::bind_rows() |> dplyr::mutate(Run='Dropped 2 Flags')


PMDF <- dplyr::bind_rows(Dropped1_PMs, Dropped2_PMs)



ggplot(PMDF, aes(x=Dropped, y=Value, color=Dropped)) +
  facet_grid(PM~Run, scales='free_y') +
  geom_point(size=1.2) +
  geom_hline(data=BaseCase_PMs,
             aes(yintercept = Value), linetype=2, color='darkgray') +
  theme_bw() +
  expand_limits(y=c(0,1)) +
  labs(x='Dropped Flag', y='Value', color='') +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
  guides(color='none')

ggsave('ECP/Figures/PI.png', width=6, height=6)

# ---- Make Table ----


t1 <- BaseCase_PMs |>
  dplyr::mutate(Run='Base', Value=round(Value,2)) |>
  tidyr::pivot_wider(names_from = PM, values_from = Value)


t2 <- Dropped1_PMs |>
  dplyr::mutate(Value=round(Value,2)) |>
  tidyr::pivot_wider(names_from = PM, values_from = Value)

t3 <- Dropped2_PMs |>
  dplyr::mutate(Value=round(Value,2)) |>
  tidyr::pivot_wider(names_from = PM, values_from = Value)

table1 <-  dplyr::bind_rows(t1, t2, t3)

library(flextable)

Table1 <- flextable::flextable(table1) |>
  flextable::save_as_docx(path="ECP/Tables/Table1.docx")


# Make figures
# write paper
