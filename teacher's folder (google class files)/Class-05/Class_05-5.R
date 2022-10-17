library(tidyverse)
data(storms, package = 'dplyr')

storm = storms %>% 
  filter(name == 'Katrina', year == 2005, !is.na(tropicalstorm_force_diameter)) %>% 
  mutate(status = factor(status, levels = c('tropical depression', 
                                            'tropical storm', 
                                            'hurricane'), 
                            ordered = TRUE),
         wind = 0.514444 * wind,
         tropicalstorm_force_diameter = 1.60934 * tropicalstorm_force_diameter) %>% 
  arrange(year, month, day, hour)

n = length(storm$pressure)

cols = adjustcolor(colorRampPalette(c('steelblue', 'firebrick'))(3), alpha = 0.75)

plot(storm$pressure, storm$wind, 
     type = 'l', col = 'black', lwd = 1,
     main = 'Тропический циклон Катрина (2005 г.)', 
     xlab = 'Давление, [мБ]', 
     ylab = 'Скорость ветра, [м/с]')
grid()

arrows(storm$pressure[-n], 
       storm$wind[-n],
       storm$pressure[-n] + 0.5 * diff(storm$pressure), 
       storm$wind[-n] + 0.5 * diff(storm$wind),
       length = 0.1,
       lwd = 1,
       col = 'black')

points(storm$pressure, storm$wind, pch = 19, 
       col = cols[storm$status],
       cex = 2.5 * sqrt(cut(storm$tropicalstorm_force_diameter, include.lowest = T, (0:4) * 200, labels = FALSE)))

text(storm$pressure, storm$wind, labels = as.integer(storm$tropicalstorm_force_diameter),
     font = 2,
     cex = 0.6, col = 'white')

text(910, 25, labels = 'Цифрами указан диаметр (в км)\nтерритории со скоростью ветра,\nсоответствующей классу\nтропического шторма и сильнее', pos = 4)

legend('topright', legend = c('Тропическая депрессия', 
                             'Тропический шторм', 
                             'Ураган'), pch = 19, col = cols, pt.cex = 2)
