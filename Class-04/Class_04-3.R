setwd('/Volumes/Cloud/Dropbox/R/2021/Classwork/Class_04')

library(readxl)
library(dplyr)

winds = read_excel('winds.xlsx') |> 
  rename(dir = 1) |> 
  filter(dir != 'штиль')

idx = sapply(winds[-1], which.max)

stats = tibble(month = colnames(winds)[-1],
               dir = winds$dir[idx])

