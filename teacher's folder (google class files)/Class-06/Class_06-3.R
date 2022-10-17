library(dplyr)
library(ggplot2)

tab = readxl::read_excel('garabashi.xlsx') |> 
  filter(!stringr::str_detect(Год, '-')) |> 
  mutate(Кумбаланс = cumsum(Балансмассы),
         Год = as.integer(substr(Год, 1, 4)))
ggplot(tab, aes(x = Год)) +
  geom_line(aes(y = Аккумуляция), color = 'blue') +
  geom_line(aes(y = Абляция), color = 'red') +
  geom_line(aes(y = Кумбаланс), color = 'green')
tab_long = tidyr::pivot_longer(
  tab,
  Аккумуляция:Кумбаланс,
  names_to = 'Тип',
  values_to = 'Объем'
)
ggplot(tab_long, aes(x = Год, y = Объем, color = Тип)) +
  geom_line()