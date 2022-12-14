library(tidyverse)
data(quakes, package = 'datasets')
# Подсчет количества землетрясений
# с заданной магнитудой
(nquakes = quakes %>% 
  group_by(mag) %>% 
  summarise(ncases = n()))

summarise(group_by(quakes, mag), ncases = n())

grouped = group_by(quakes, mag)
result = summarise(grouped, 
                   ncases = n())

