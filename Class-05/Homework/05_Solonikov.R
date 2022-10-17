rm(list = ls())

library(tidyverse)
library(readxl)
library(readr)

tab <- read_table("graph.txt",
                  skip = 5,
                  col_names = c("Year", "No_Smoothing", "Lowess"))

plot(x = tab$Year, y = tab$Lowess,
     type = 'l', col = 'blue', lwd = 1,
     main = 'Аномалия температуры по отношению к средней\nза период 1951-1980 (по данным NASA, 2020 г.)',
     xlab = 'Год', ylab = 'ºC')

grid()

abline(h = 0, col = 'red')

lines(x = tab$Year, y = tab$No_Smoothing, col = 'gray')

points(x = tab$Year, y = tab$No_Smoothing, pch = 1)
