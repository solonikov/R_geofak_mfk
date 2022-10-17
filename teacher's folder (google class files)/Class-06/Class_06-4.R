library(tidyverse)
library(nasapower)

daily_single_ag = get_power(
  community = "ag",
  lonlat = c(60.59, 56.84),
  pars = c("RH2M", "T2M", "PRECTOTCORR"),
  dates = c("1995-04-01", "1995-04-30"),
  temporal_api = "daily"
)

interannual_sse = get_power(
  community = "sb",
  lonlat = c(60.59, 56.84),
  dates = 1995:2015,
  temporal_api = "climatology",
  pars = c("CLRSKY_SFC_SW_DWN",
           "ALLSKY_SFC_SW_DWN")
)

ggplot(daily_single_ag, mapping = aes(YYYYMMDD, T2M)) +
  geom_line() +
  geom_point() +
  xlab('Дата') +
  ylab('Температура, °C') +
  ggtitle('Ход температуры') +
  theme_bw()


sse_tidy = interannual_sse |> 
  select(-LON, -LAT) |> 
  pivot_longer(JAN:ANN, names_to = 'month', values_to = 'value') |> 
  pivot_wider(YEAR:month, names_from = PARAMETER) |> 
  filter(YEAR == 1995, month != 'ANN') |> 
  mutate(month = factor(month, levels = month, ordered = TRUE))

ggplot(sse_tidy, mapping = aes(month, ALLSKY_SFC_SW_DWN)) +
  geom_col(fill = 'olivedrab') +
  xlab('Месяц') +
  ylab(expression(кВт/ч/м^2/день))+
  theme_bw()
