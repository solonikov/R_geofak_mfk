library(tidyverse)
library(readxl)
library(writexl)


# данные о выбросах загрязняющих веществ
emission <- read_xlsx('emissions.xlsx', sheet = 1, skip = 1) %>%
    # создали столбец с фед. округом - заполнен пока только для строк с округами
    mutate(fed_distr = ifelse(str_detect(Регион, 'федеральный округ'),
                              Регион,
                              NA),
           Region = Регион) %>% 
    select(-Регион) %>% 
    # заполняем столбец с фед. округами для всех регионов
    fill(fed_distr) %>%
    # убираем из таблицы записи, соответствующие фед. окргам и всей стране
    filter(!str_detect(Region, 'Федерация|федеральный округ')) %>%
    # переводим таблицу в длинный формат
    pivot_longer(cols = '2005':'2016',
                 names_to = 'year',
                 values_to = 'emission',
                 values_ptypes = list('emission' = numeric()),
                 names_ptypes = list('year' = factor(ordered = T)))

# данные об улавливании загрязняющих веществ
# проделываем все те же операции, что выше
capture <- read_xlsx('emissions.xlsx', sheet = 2, skip = 1, na = '-') %>%
  mutate(fed_distr = ifelse(str_detect(Регион, 'федеральный округ'),
                            Регион,
                            NA),
         Region = Регион) %>% 
  select(-Регион) %>% 
  fill(fed_distr) %>%
  filter(!str_detect(Region, 'Федерация|федеральный округ')) %>%
  pivot_longer(cols = '2005':'2016',
               names_to = 'year',
               values_to = 'capture',
               values_ptypes = list('capture' = numeric()),
               names_ptypes = list('year' = factor(ordered = T)))

# Соединяем две таблицы:
# (inner_join, так как нужны записи, содержащиеся в обеих таблицах одновременно)
join_df <- inner_join(emission, capture,
           by = c('Region' = 'Region',
                  'year' = 'year',
                  'fed_distr' = 'fed_distr')) %>% 
  # разность между улавливанием и выбросом загрязняющих веществ
  mutate(diff = capture - emission) %>%
  # группируем по федеральным округам и по году
  group_by(fed_distr, year) %>%
  # сортируем по величине разности между улавливанием и выбросом
  arrange(diff) %>%
  # выбраем субъект федерации с худшим показателем
  filter(row_number() == 1) %>%
  # для удобства просмотра сортируем по таблицу по фед. округу и году
  arrange(fed_distr, year) %>%
  select(fed_distr, year, Region, diff, emission, capture)

# записываем результат в файл:
write_xlsx(join_df, path = 'polluters.xlsx')
