library(tidyverse)
library(readxl)
library(writexl)


# данные о выбросах загрязняющих веществ
emission <- read_xlsx('emissions.xlsx', sheet = 1, skip = 1) %>%    # чтение данных из таблицы
    mutate(fed_distr = ifelse(str_detect(Регион, 'федеральный округ'), Регион, NA),
           Region = Регион) %>%    # создали столбец с фед. округом и заполнили его именами округов пока только для записей таблицы с округами
    select(-Регион) %>% 
    tidyr::fill(fed_distr) %>%    # заполняем столбец с фед. округами для всех регионов, используя значения из записей, соответствующих самим округам
    dplyr::filter(!str_detect(Region, 'Федерация|федеральный округ')) %>%    # убираем из таблицы записи, соответствующие фед. окргам и всей стране
    pivot_longer(cols = '2005':'2016',    # переводим таблицу в длинный формат
                 names_to = 'year',
                 values_to = 'emission',
                 values_ptypes = list('emission' = numeric()),
                 names_ptypes = list('year' = factor(ordered = T)))

# данные об улавливании загрязняющих веществ
# проделываем все те же операции, что выше
capture <- read_xlsx('emissions.xlsx', sheet = 2, skip = 1, na = '-') %>%
  mutate(fed_distr = ifelse(str_detect(Регион, 'федеральный округ'), Регион, NA),
         Region = Регион) %>% 
  select(-Регион) %>% 
  tidyr::fill(fed_distr) %>%
  dplyr::filter(!str_detect(Region, 'Федерация|федеральный округ')) %>%
  pivot_longer(cols = '2005':'2016',
               names_to = 'year',
               values_to = 'capture',
               values_ptypes = list('capture' = numeric()),
               names_ptypes = list('year' = factor(ordered = T)))

join_df <- inner_join(emission, capture,    # соединяем две таблицы. Используем inner_join, так как нам нужны только записи, содержащиеся в обеих таблицах одновременно
           by = c('Region' = 'Region', 'year' = 'year', 'fed_distr' = 'fed_distr')) %>% 
  mutate(diff = capture - emission) %>%    # вычисляем разность между улавливанием и выбросом загрязняющих веществ
  group_by(fed_distr, year) %>%    # группируем по федеральным округам и по году
  arrange(diff) %>%    # сортируем по значениям вычисленной разности между улавливанием и выбросом
  filter(row_number() == 1) %>%    # берём в каждой группе только первую строчку, чтобы выбрать субъект федерации с худшим показателем
  arrange(fed_distr, year) %>%    # для удобства просмотра сортируем по таблицу по фед. округу и году
  select(fed_distr, year, Region, diff, emission, capture)

# записываем результат в файл:
write_xlsx(join_df, path = 'polluters.xlsx')
