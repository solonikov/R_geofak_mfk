library(readxl)
library(tidyverse)

tab = read_excel('data/garabashi.xlsx') %>% 
  rename(year = 1,
         acc = 2, 
         abl = 3,
         bal = 4) %>% 
  filter(!stringr::str_detect(year, '-')) %>% 
  separate(year, c('year', 'y'), 4) %>% 
  select(-y) %>% 
  mutate(year = as.integer(year),
         cumbal = cumsum(bal))

plot(tab$year, tab$cumbal, type = 'l',
     lwd = 2, col = 'steelblue',
     ylim = c(-1200, 400), 
     xlab = 'Год', ylab = 'см в.э.',
     main = 'Годовой баланс массы\nледника Гарабаши')
abline(h = 0, lty = 3)
lines(tab$year, tab$acc, col = 'green')
lines(tab$year, tab$abl, col = 'red')

legend('bottomleft', 
       c('Аккумуляция', 'Абляция', 'Кумулятивный баланс'),
       title = 'Легенда', 
       col = c('green', 'red', 'steelblue'), 
       lwd = c(1, 1, 2))
