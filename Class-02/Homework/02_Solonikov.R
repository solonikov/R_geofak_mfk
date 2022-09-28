rm(list = ls())
library(dplyr)

# создаём датафрейм
data <- rbind(list(1, "Орёл", 52.9380, 36.0657, 111, 1389, 146.31),
  list(2, "Калуга", 54.5059, 36.2336, 391, 1109, 116.72),
  list(3, "Серпухов", 54.8835, 37.4181, 522, 978, 107.54),
  list(4, "Коломна", 55.0705, 38.8308, 645, 855, 100.26),
  list(5, "Рязань", 54.6518, 39.8069, 801, 699, 93.41),
  list(6, "Муром", 55.5768, 42.0721, 1285, 215, 73.27),
  list(7, "Нижний Новгород", 56.2105, 43.8720, 1500, 0, 62)
) %>% 
  data.frame()

# устанавливаем имена столбцов
colnames(data) = c('id', 'name', 'lat', 'lon', 'distFromSource', 'distToMouth', 'zeroLevel')

print(data)

# создаём пустой датафрейм секций
sections <- data.frame()

# # Реализация через цикл (я уже позже увидел в требованиях, что нужно было по-другому)
#
# # заполняем его информацией в цикле
# route_length <- 0
# for (num in 1:(nrow(data) - 1)) {
#   start_id <- data$id[[num]]    # номер начального пункта
#   stop_id <- data$id[[num + 1]]    # номер конечного пункта
#   section_name <- paste(data$name[[num]], data$name[[num + 1]], sep = '_')    # имя секции
#   length_km <- data$distFromSource[[num + 1]] - data$distFromSource[[num]]    # длина секции
#   route_length <- route_length + length_km    # длина пройденного пути от начала маршрута до конца данной секции
#   H_begin <- data$zeroLevel[[num]]    # высота начального пункта секции
#   H_end <- data$zeroLevel[[num + 1]]    # высота конечного пункта секции
#   H_diff <- H_end - H_begin    # перепад высоты между пунктами
#   slope <- abs(round(H_diff / length_km, 2))    # уклон
#   # считаем расстояние между пунктами по прямой по координатам:
#   lat1 <- data$lat[[num]] * pi / 180
#   lat2 <- data$lat[[num + 1]] * pi / 180
#   lon1 <- data$lon[[num]] * pi / 180
#   lon2 <- data$lon[[num + 1]] * pi / 180
#   dSigma = acos(sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(lon2 - lon1))    # центральный угол между пунктами
#   distance_coord <- dSigma * 6371    # расстояние между пунктами по прямой по координатам
#   meanderCoeff <- round(length_km / distance_coord, 2)    # коэффициент извилистости
#   # прикрепляем создаваемую в каждой итерации цикла запись в конец датафрейма
#   sections <- rbind(sections, list(start_id, stop_id, section_name, length_km, 
#                                    route_length, H_diff, slope, meanderCoeff, H_begin, H_end))
# }

# реализация через векторы
ids <- as.vector(data$id, mode = 'numeric')
distsFromSource <- as.vector(data$distFromSource, mode = 'numeric')
start_ids <- ids[1:length(ids) - 1]
stop_ids <- ids[2:length(ids)]
section_names <- paste(data$name[start_ids], "_", data$name[stop_ids], sep = "")
lengths_km <- distsFromSource[stop_ids] - distsFromSource[start_ids]
# route_lengths


# устанавливаем имена столбцов
colnames(sections) <- c('start_id', 'stop_id', 'section_name', 'length_km',
                        'route_length', 'H_diff', 'slope', 'meanderCoeff', 'H_begin', 'H_end')
print(sections)

# просим пользователя ввести расстояние:
user_km <- as.numeric(readline("Введите расстояние в км: "))

# создаём строку для вывода и выводим информацию о пройденном пути:

# используем цикл, чтобы найти нужную секцию
for (num in 1:(nrow(sections))) {
  # находим нужную секцию
  if (user_km <= sections$route_length[[num]]) {
    break
  }
}

name1 <- strsplit(sections$section_name[[num]], '_')[[1]] %>% .[1]    # название первого пункта
name2 <- strsplit(sections$section_name[[num]], '_')[[1]] %>% .[2]    # название второго пункта
progr <- round(user_km / sections$route_length[[nrow(sections)]] * 100, 1)    # пройденная доля всего маршрута в %
progr_in_section <- sections$length_km[[num]] - (sections$route_length[[num]] - user_km)    # пройденная доля маршрута внутри секции
height <- progr_in_section * sections$H_diff[[num]] / sections$length_km[[num]] + sections$H_begin[[num]]    # примерная высота
height <- round(height, 1)
print(paste("Вы находитесь между городами ", name1, " и ", name2, ". Пройдено ",
      progr, "% маршрута. Ваша высота примерно ", height, " м.", sep = ""))


