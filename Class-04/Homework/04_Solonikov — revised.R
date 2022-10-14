rm(list = ls())

library(tidyverse)
library(readxl)
library(writexl)

# Считываем исходные данные. read_xls очень хочет превратить многие колонки в
# логический тип, теряя при этом информацию, поэтому при считывании все данные
# принудительно приведены к character с помощью col_types. Типы данных
# в колонках не имеют значения в нашей задаче.
data <- read_xls("soil_data.xls", sheet = 1, col_types = 'text')
# Считываем таблицу с текстовым описанием атрибутов:
data_dsc <- read_xls("soil_data.xls", sheet = 2,
                     col_names = c('name', 'desc'))

# Функция, принимающая на вход датафрейм и возвращающая таблицу с информацией
# о заполненности полей:
check_completeness <- function(df) {
  result <- df %>% 
    # получаем логическую матрицу, где TRUE соответствует заполненным ячейкам,
    # FALSE - незаполненным:
    sapply(complete.cases) %>%
    # рассчитываем долю заполненных ячеек для каждого поля:
    apply(FUN = function(x) {sum(x) / length(x) * 100}, MARGIN = 2) %>%
    # из получившегося именованного вектора делаем датафрейм:
    tibble(name = names(.), completeness = .) %>%
    # заполняем колонку "статус" в соответствии с показателем заполненности
    # ячеек "completeness":
    mutate(status = case_when(
      completeness == 100 ~ "полностью",
      completeness == 0 ~ "не заполнен",
      TRUE ~ "частично"
    ))
  return(result)
}

# список с результирующими датафреймами:
result_list <- data %>% 
                # С помощью split разбиваем исходный датафрейм на список
                # датафреймов по типам почвы:
                split(f = data$SOIL_ID) %>% 
                # к каждому датафрейму из списка применяем функцию из lapply:
                lapply(FUN = function(x) {
                  x %>%
                    # применяем к каждому датафрейму функцию check_completeness,
                    # которая возвращает информацию о заполненности полей:
                    check_completeness() %>%
                    # добавляем текстовые описания атрибутов из другой таблицы:
                    inner_join(data_dsc, by = c("name" = "name")) %>%
                    # меняем порядок колонок:
                    select(name, desc, completeness, status)
                })

# записываем полученные датафреймы в файл -- каждая таблица на отдельном листе,
# так как в функцию передан список датафреймов:
write_xlsx(result_list, path = "completeness.xlsx")

