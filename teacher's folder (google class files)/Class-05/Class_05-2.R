library(tidyverse)
library(RColorBrewer)

tab = read_csv('data/okruga.csv') %>% 
  rename_with(~paste0('y', .), starts_with('2')) %>% 
  arrange(y2013)

var = tab$y2013
pie(var, 
    main = 'Структура населения по федеральным\nокругам России в 2013 г.',
    col = brewer.pal(length(var),"Set2"),
    labels = paste0(tab$Регион, ' (', round(100 * var / sum(var), 0), '%)'))
