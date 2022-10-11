# Упражнение 1
# Постройте для набора данных quakes пакета datasets гистограммы распределения
# глубин и магнитуд, а также диаграмму рассеяния для двух этих характеристик.
# Используйте сначала функцию qplot(), а затем выполните то же самое с
# использованием полного синтаксиса ggplot2().

rm(list = ls())

library(tidyverse)
library(ggplot2)

data("quakes")

qplot(x = depth, data = quakes, bins = 10)
qplot(x = mag, data = quakes, bins = 15)
qplot(x = depth, y = mag, data = quakes)

ggplot(data = quakes, aes(depth)) +
  geom_histogram(color = 'black', fill = 'dark blue')

ggplot(data = quakes, aes(mag)) +
  geom_histogram(color = 'black', fill = 'dark red', binwidth = .2)

ggplot(data = quakes) +
  geom_point(aes(mag, depth)) +
  theme_bw()
