library(dplyr)
library(tidyr)
library(ggplot2)
library(soilDB)
library(RColorBrewer)

# Загрузка данных
soils <- c('cecil', 'altavista', 'lloyd', 'wickham')
series <- fetchOSD(soils, extended = T)


## График 1
tab1 <- series$hillpos |>
  select(1:6) |>     # выбираем нужные столбцы с вероятностями
  pivot_longer(cols = 2:6,    # приводим к длинному виду из широкого
               names_to = 'position',
               values_to = 'probability') |> 
  # преобразуем position в фактор для правильного отображения на графике
  mutate(position = factor(position,
                           levels = c('Summit', 'Shoulder', 'Backslope',
                                      'Footslope', 'Toeslope'),
                           ordered = T))


ggplot(tab1, aes(x = series, y = probability, fill = position)) +
  geom_col(color = 'black', linewidth = .2) +
  coord_flip() +    # разворачиваем диаграмму
  # инвертируем последовательность цветов в цветовой шкале
  scale_fill_viridis_d(direction = -1) +
  ylab('Probability') +
  xlab('Soil series') +
  labs(fill = 'Hillslope position') +
  ggtitle('Empirical probability for hillslope position')


## График 2
# собираем цветовую палитру
my_colors <- c(brewer.pal(8, 'Set3'),
               brewer.pal(8, 'Set1'))

series$pmorigin |> 
  ggplot(aes(x = '', y = P, fill = pmorigin)) +
  geom_col(color = 'black', linewidth = .2) +
  coord_polar(theta = 'y') +    # переходим к полярным координатам
  facet_wrap(~series) +    # разбиваем график на фасеты по типам почвы
  # исправляем отображение значений на оси
  scale_y_continuous(breaks = seq(0, .95, .05)) +
  scale_fill_manual(values = my_colors) + # применяем созданную выше палитру
  labs(fill = 'Origin', x = '', y = '') +
  ggtitle('Empirical probability for parent material origin')
  

## График 3
series$climate.monthly |>
  # выбираем из таблицы только данные по осадкам
  filter(variable == 'Precipitation (mm)') |> 
  # превращаем столбец с номером месяца в упорядоченный фактор с названиями
  mutate(month = factor(month.abb[month], levels = month.abb, ordered = T)) |> 
  ggplot(aes(x = month, y = q50, color = series, group = series)) +
  geom_line(linewidth = .7) +
  geom_point(size = 2) +
  labs(color = 'Series', x = 'Month', y = 'mm') +
  ggtitle('Precipitation')










