data(quakes)

hist(quakes$depth,
     col = rgb(0.5, 1, 0.8),
     xlab = 'Глубина',
     ylab = 'Количество',
     main = 'Гистограмма распределения\n глубин землетрясений')

hist(quakes$mag,
     col = 'orangered',
     xlab = 'Магнитуда',
     ylab = 'Количество',
     main = 'Гистограмма распределения\n магнитуды землетрясений')

plot(quakes$depth, quakes$mag, 
     pch = 19, cex = 0.2*log(quakes$stations),
     col = rgb(1, 0, 0, 0.15),
     main = 'Землетрясения',
     xlab = 'Глубина, км', 
     ylab = 'Магнитуда, баллы')
