is_leap = function(year) {
  (year %% 400 == 0) || ((year %% 4 == 0) && (year %% 100 != 0))
}

y = as.integer(readline('Введите год: '))
cat(ifelse(is_leap(y), 'Високосный', 'Обычный'))21
