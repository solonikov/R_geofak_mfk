# Упражнение 3
# Используя данные по балансу масс ледника Гарабаши, постройте график с тремя
# кривыми (аккумуляции, абляции и кумулятивного баланса) за период 1981 по
# 2017 г. Добавьте на график легенду. Обратите внимание на то, что таблица
# содержит агрегирующие строки (1982-1997, 1998-2017, 1982-2017), которые вам
# необходимо предварительно исключить.
# Подсказка: Чтобы построить кривую кумулятивного баланса, используйте
# функцию cumsum.
# https://raw.githubusercontent.com/tsamsonov/r-geo-course/master/data/garabashi.xlsx

rm(list = ls())

library(tidyverse)
library(ggplot2)
library(readxl)

setwd("/Users/ivan.solonikov/Desktop/R_geofak_mfk/Class-06")
tab <- read_excel(path = "garabashi.xlsx") %>% 
  filter(!stringr::str_detect(Год, '-')) %>%     # убираем агрегирующие строки с данными за несколько лет
  separate(Год, c('Year', 'Year2'), '/') %>%     # приводим названия колонок с годом к нормальному виду, для этого делим колонку года на две
  select(-Year2) %>% 
  mutate(Кумбаланс = cumsum(Балансмассы))

tab_long <- tab %>% 
  pivot_longer(Аккумуляция:Кумбаланс,
               names_to = "type",
               values_to = "value") %>% 
  mutate(Год = as.numeric(Year)) %>% 
  select(-Year)

ggplot(tab_long, aes(x = Год, y = value, group = type, color = type)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1980, 2020, by = 5))


