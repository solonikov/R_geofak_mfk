rm(list = ls())

library('tidyverse')
library('readxl')
library('writexl')

# Считываем исходные данные. read_xls очень хочет превратить многие колонки в
# логический тип, теряя при этом информацию, поэтому при считывании все данные
# принудительно приведены к character с помощью col_types. Типы данных
# в колонках не имеют значения в нашей задаче.
data <- read_xls("soil_data.xls", sheet = 1, col_types = 'text')
# С помощью split разбиваем исходный датафрейм на список датафреймов по типам
# почвы:
datalist <- split(data, f = data$SOIL_ID)
# Считываем таблицу с текстовым описанием атрибутов:
data_dsc <- read_xls("soil_data.xls", sheet = 2,
                     col_names = c('name', 'desc'))

# Функция, принимающая на вход датафрейм и возвращающая таблицу с информацией
# о заполненности полей:
check_completeness <- function(df) {
  result <- df %>% 
    sapply(complete.cases) %>%     # получаем логическую матрицу, где TRUE соответствует заполненным ячейкам, FALSE - незаполненным
    apply(FUN = function(x) {sum(x) / length(x) * 100}, MARGIN = 2) %>%     # рассчитываем долю заполненных ячеек для каждого поля
    tibble(name = names(.), completeness = .) %>%     # из получившегося именованного вектора делаем датафрейм
    mutate(status = case_when(    # заполняем колонку "статус" в соответствии с показателем заполненности ячеек "completeness"
      completeness == 100 ~ "полностью",
      completeness == 0 ~ "не заполнен",
      TRUE ~ "частично"
    ))
  return(result)
}

# список с результирующими датафреймами:
# к каждому исходному датафрейму из datalist применяем функцию, обрабатывающую его в несколько этапов
result_list <- datalist %>% lapply(FUN = function(x) {
                  x %>%
                    check_completeness() %>%     # применяем к каждому датафрейму функцию check_completeness, которая возвращает информацию о заполненности полей
                    inner_join(data_dsc, by = c("name" = "name")) %>%     # добавляем текстовые описания атрибутов из другой таблицы
                    select(name, desc, completeness, status)    # меняем порядок колонок
                })

# записываем полученные датафреймы в файл -- каждая таблица на отдельном листе, так как в функцию передан список датафреймов
write_xlsx(result_list, path = "completeness.xlsx")
