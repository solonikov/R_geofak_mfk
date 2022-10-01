library(readxl)
library(writexl)
library(stringr)


emission <- read_xlsx('emissions.xlsx', sheet = 1, skip = 1) %>% 
    mutate(fed_distr = ifelse(str_detect(Регион, 'федеральный округ'), Регион, NA),
           Region = Регион) %>% 
    select(-Регион) %>% 
    tidyr::fill(fed_distr) %>%
    dplyr::filter(!str_detect(Region, 'Федерация|федеральный округ')) %>%
    pivot_longer(cols = '2005':'2016',
                 names_to = 'year',
                 values_to = 'emission',
                 values_ptypes = list('emission' = numeric()),
                 names_ptypes = list('year' = factor(ordered = T)))

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

join_df <- inner_join(emission, capture,
           by = c('Region' = 'Region', 'year' = 'year', 'fed_distr' = 'fed_distr')) %>% 
  mutate(diff = capture - emission) %>% 
  group_by(fed_distr, year) %>% 
  arrange(diff) %>% 
  filter(row_number() == 1) %>%
  arrange(fed_distr, year) %>% 
  select(fed_distr, year, Region, diff, emission, capture)

write_xlsx(join_df, path = 'polluters.xlsx')
