library(tidyverse)
library(eurostat)

trades = get_eurostat('tet00034') |> 
  label_eurostat() |> 
  bind_rows() |> # объединим прочитанные таблицы в одну
  select(-geo) |> # убираем столбец с территорией торговли, т.к. там только Евросоюз
  filter(stringr::str_detect(indic_et, 'Exports in|Imports in')) |> # оставим только экспорт и импорт
  pivot_wider(names_from = indic_et, values_from = values) |>  # вынесем данные по экспорту и импорту в отдельные переменные
  rename(export = `Exports in million of ECU/EURO`, # дадим им краткие названия
         import = `Imports in million of ECU/EURO`) |> 
  mutate(partner = as.factor(partner))

trades_tidy = trades |> 
  pivot_longer(export:import, names_to = 'trade', values_to = 'value') |> 
  group_by(trade, time) |> 
  summarise(value = sum(value))

ggplot(trades_tidy, mapping = aes(time, value, color = trade)) +
  geom_line() +
  geom_point() +
  ggtitle('Импорт и экспорт продуктов питания, напитков и табака в Евросоюзе') + 
  xlab('Год') +
  ylab('млн. евро')

options(scipen = 999)

ggplot(trades |> filter(time == as.Date('2019-01-01')), 
       mapping = aes(x = '', y = export, fill = partner), color = 'black', size = 0.2) +
  geom_col(position = 'fill') +
  geom_text(aes(label = paste0(round(export / sum(export) * 100, 1), '%')), position = position_fill(vjust = 0.5)) +
  coord_polar(theta = 'y') +
  labs(fill = 'Партнер', title = 'Экспорт продуктов питания, напитков и табака в Евросоюзе', subtitle = '2019 г.') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

ggplot(trades |> filter(time == as.Date('2019-01-01')), 
       mapping = aes(x = '', y = import, fill = partner), color = 'black', size = 0.2) +
  geom_col(position = 'fill') +
  geom_text(aes(label = paste0(round(import / sum(import) * 100, 1), '%')), position = position_fill(vjust = 0.5)) +
  coord_polar(theta = 'y') +
  labs(fill = 'Партнер', title = 'Импорт продуктов питания, напитков и табака в Евросоюзе', subtitle = '2019 г.') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

trades_tidy2 = trades |> 
  pivot_longer(export:import, names_to = 'trade', values_to = 'value') |> 
  mutate(trade = if_else(trade == 'export', 'Экспорт', 'Импорт'))

ggplot(trades_tidy2 |> filter(time == as.Date('2019-01-01')), 
       mapping = aes(x = '', y = value, fill = partner), color = 'black', size = 0.2) +
  geom_col(position = 'fill') +
  geom_text(aes(label = paste0(round(value / sum(value) * 100, 1), '%')), position = position_fill(vjust = 0.5)) +
  coord_polar(theta = 'y') +
  labs(fill = 'Партнер', title = 'Импорт и экспорт продуктов питания, напитков и табака в Евросоюзе', subtitle = '2019 г.') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  xlab(NULL) +
  ylab(NULL) +
  facet_wrap(~trade)
