library(ggplot2)

source_CMPs()

# CE
Data <- SWOData
Data@Year <- 1950:2024

index <- Data@Ind[1,]
smoothed <- stats::smooth(index[!is.na(index)])
index[!is.na(index)] <- smoothed

smoothed_df <- data.frame(Year=Data@Year, Index=c(SWOData@Ind[1,],NA, NA),
                          Smoothed=c(index, NA, NA)) |>
  tidyr::pivot_longer(cols=2:3) |>
  dplyr::filter(Year>=2010)


ggplot(smoothed_df, aes(x=Year, y=value, color=name)) +
  geom_line() +
  expand_limits(y=0) +
  theme_bw() +
  labs(color='Legend', x='Year', y='Index')

ggsave('img/smoothed_index_example.png', width=7, height=3)

df <- data.frame(Year=Data@Year, Index=c(index, NA, NA),
           Catch=c(Data@Cat[1,], NA, NA))

hist_df <- df |> dplyr::filter(Year%in%2016:2020) |>
  dplyr::mutate(Index=mean(Index), Catch=mean(Catch)) |>
  dplyr::mutate(ER=Catch/Index, Period='Historical')

curr_df <- df |> dplyr::filter(Year%in%2020:2022) |>
  dplyr::mutate(Index=mean(Index), Catch=mean(Catch)) |>
  dplyr::mutate(ER=Catch/Index, Period='Current')

df <- df |> tidyr::pivot_longer(cols=2:3) |>
  dplyr::filter(Year %in% 2015:2024)

df2 <- bind_rows(hist_df, curr_df) |>
  tidyr::pivot_longer(cols=2:3)

newTAC <- data.frame(Year=2025, TAC=CE_b(1, Data)@TAC, name='Catch')

df2$Year <- lubridate::ymd(paste0(df2$Year, '-01-01'))
df$Year <- lubridate::ymd(paste0(df$Year, '-01-01'))
newTAC$Year <- lubridate::ymd(paste0(newTAC$Year, '-01-01'))


hist_df$ER[1]/curr_df$ER[1]

ggplot() +
  facet_wrap(~name, scales='free_y') +
  expand_limits(y=0) +
  geom_line(data=df,  aes(x=Year, y=value)) +
  theme_bw() +
  labs(x='Year', y='Value') +
  geom_line(data=df2, aes(x=Year, y=value, color=Period),
            size=1) +
  geom_point(data=newTAC, aes(x=Year, y=TAC), size=2) +
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y") +
  guides(color='none')

ggsave('img/CE_example.png', width=9, height=4)


ind_ratio <- seq(0.0, 1.5, by=0.05)
TAC_adjust <- rep(NA, length(ind_ratio))
histER <- 1

for (i in seq_along(ind_ratio)) {
  if (ind_ratio[i]>=0.8) {
    TAC_adjust[i] <- histER
  } else if (ind_ratio[i]> 0.5) {
    TAC_adjust[i] <- histER * ( -1.4+ 3 *ind_ratio[i])
  } else {
    TAC_adjust[i] <- 0.1 * histER
  }
}

df <- data.frame(x=ind_ratio, y=TAC_adjust)
ggplot(df, aes(x=x, y=y)) + geom_line() +
  expand_limits(y=0) +
  theme_bw() +
  labs(x='Index Ratio', y='Historical ER Multiplier')

ggsave('img/CE_HCR.png', width=5, height=4)


SPSSFox_b(1,Data)
