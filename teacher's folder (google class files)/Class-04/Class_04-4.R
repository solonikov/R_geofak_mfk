library(readxl)
library(tidyverse)

df = read_xlsx('data/sevzap.xlsx', col_types = c('text', rep('numeric', 17)))

normalize = function(X) {
  if (is.numeric(X)) 
    round(X / mean(X, na.rm = TRUE), 2) 
  else X
}
# Нормализация (деление на среднее значение)
(normalized_df = df %>% lapply(normalize) %>% as_tibble())

cols = colnames(normalized_df[-1])
get_min_var = function(R) {
  cols[which.min(R)]
}
get_max_var = function(R) {
  cols[which.max(R)]
}

(result = data.frame(
  Region = normalized_df[1],
  min_var = apply(normalized_df[-1], 1, get_min_var),
  max_var = apply(normalized_df[-1], 1, get_max_var)
))
