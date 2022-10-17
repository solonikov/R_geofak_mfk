library(dplyr)
library(ggplot2)
data(quakes)

qplot(mag, data = quakes,
      geom = 'histogram')
ggplot(quakes) +
  geom_histogram(aes(mag))

qplot(mag, depth, data = quakes)
ggplot(quakes, aes(mag, depth)) +
  geom_point(alpha = 0.5, colour = 'red')

qplot(depth, data = quakes,
      fill = I('olivedrab'),
      color = I('black'),
      alpha = 0.5,
      size = I(0.1),
      geom = 'histogram')
