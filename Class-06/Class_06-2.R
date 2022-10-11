# Упражнение 2
# На портале открытых данных Тульской области есть данные о распределении площади
# лесов и запасов древесины по преобладающим породам и группам возраста. Скачайте
# эти данные в виде таблицы CSV и постройте по ним круговую и столбчатую
# диаграмму для категории Площадь земель, занятых лесными насаждениями (покрытых
# лесной растительностью), всего. Подберите цвета, попробуйте изменить
# ориентировку столбцов на горизонтальную, а для круговой диаграммы поменять угол
# поворота.
# https://opendata71.ru/opendata/7107096442-stateforestregisterTularegion/table

rm(list = ls())

library(tidyverse)
library(ggplot2)

setwd("/Users/ivan.solonikov/Desktop/R_geofak_mfk/Class-06")
tab <- read_csv2("data-2022-06-03T11-34-structure-2022-06-03T11-34.csv") %>% 
  select(1:2) %>% 
  rename(type = 1, value = 2) %>%     # с помощью этой функции переименовали первый столбец в 'type', а второй столбец в 'value'
  filter(!is.na(value)) %>% # убираем пустые строки
  filter(!stringr::str_detect(type,
                              'Итoгo|Итого|Всего|том')) %>%     # убираем все строки "итого" и "всего". "Итого" напечатали два раза, потому что в таблице что-то не так, мб буквы "о" английские или что-то ещё
  arrange(desc(value))

ggplot(tab, aes(x = type, y = value)) +
  geom_col(fill = 'plum4', color = 'black', size = 0.2) +
  coord_flip()

ggplot(tab, aes(x = type, y = value, fill = type)) +
  geom_col() +
  coord_polar(start = 1) +
  scale_y_sqrt()
  
