rm(list = ls())

# создаём датафрейм
data <- data.frame(
  rbind(list(1, "Орёл", 52.9380, 36.0657, 111, 1389, 146.31),
  list(2, "Калуга", 54.5059, 36.2336, 391, 1109, 116.72),
  list(3, "Серпухов", 54.8835, 37.4181, 522, 978, 107.54),
  list(4, "Коломна", 55.0705, 38.8308, 645, 855, 100.26),
  list(5, "Рязань", 54.6518, 39.8069, 801, 699, 93.41),
  list(6, "Муром", 55.5768, 42.0721, 1285, 215, 73.27),
  list(7, "Нижний Новгород", 56.2105, 43.8720, 1500, 0, 62)
  )
)

# устанавливаем имена столбцов
colnames(data) = c('id', 'name', 'lat', 'lon', 'distFromSource', 'distToMouth', 'zeroLevel')

print(data)

# # Реализация через цикл (я уже позже увидел в требованиях, что нужно было по-другому,
# # но сохранил эту реализацию как комментарий тоже)
#
# # создаём пустой датафрейм секций
# sections <- data.frame()
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
# # устанавливаем имена столбцов
# colnames(sections) <- c('start_id', 'stop_id', 'section_name', 'length_km',
#                         'route_length', 'H_diff', 'slope', 'meanderCoeff', 'H_begin', 'H_end')

# реализация через векторы
ids <- as.vector(data$id, mode = 'numeric')    # номера пунктов
heights <- as.vector(data$zeroLevel, mode = 'numeric')    # высоты пунктов
start_lats <- as.vector(data$lat, mode = 'numeric')[1:nrow(data) - 1] * pi / 180    # широты начальных пунктов секций
stop_lats <- as.vector(data$lat, mode = 'numeric')[2:nrow(data)] * pi / 180    # широты конечных пунктов секций
start_lons <- as.vector(data$lon, mode = 'numeric')[1:nrow(data) - 1] * pi / 180    # долготы начальных пунктов секций
stop_lons <- as.vector(data$lon, mode = 'numeric')[2:nrow(data)] * pi / 180    # долготы конечных пунктов секций

distsFromSource <- as.vector(data$distFromSource, mode = 'numeric')
start_ids <- ids[1:length(ids) - 1]    # номера начальных пунктов
stop_ids <- ids[2:length(ids)]    # номера конечных пунктов
section_names <- paste(data$name[start_ids], "_", data$name[stop_ids], sep = "")    # имена секций
lengths_km <- distsFromSource[stop_ids] - distsFromSource[start_ids]    # длины секций
route_lengths <- cumsum(lengths_km)    # длина пройденного пути от начала маршрута до конца данной секции
heights_start <- heights[1:length(ids) - 1]    # высоты начальных пунктов секций
heights_stop <- heights[2:length(ids)]    # высоты конечных пунктов секций
heights_diff <- heights_stop - heights_start    # перепады высот между начальным и конечным пунктами секции
slopes <- abs(round(heights_diff / lengths_km, 2))    # уклоны
# считаем расстояние между пунктами по прямой по координатам и вычисляем коэффициенты извилистости:
dSigmas <- acos(sin(start_lats) * sin(stop_lats) + cos(start_lats) * cos(stop_lats) * cos(stop_lons - start_lons))    # центральные углы между пунктами
dists_coord <- dSigmas * 6371    # расстояния между пунктами по прямой по координатам
meanderCoeffs <- round(lengths_km / dists_coord, 2)    # коэффициенты извилистости
# собираем всё в датафрейм:
sections <- data.frame(start_ids, stop_ids, section_names, lengths_km, heights_diff,
           slopes, meanderCoeffs)

print(sections)

# просим пользователя ввести расстояние:
user_km <- as.numeric(readline("Введите расстояние в км: "))

# создаём строку для вывода и выводим информацию о пройденном пути:

# # используем цикл, чтобы найти нужную секцию
# for (num in 1:(nrow(sections))) {
#   # находим нужную секцию
#   if (user_km <= route_lengths[num]) {
#     break
#   }
# }
# Можно найти номер секции, соответствующий вводу пользователя, проще:
num <- which(user_km <= route_lengths)[1] 


name <- strsplit(sections$section_names[[num]], '_')[[1]]
name1 <- name[1]    # название первого пункта
name2 <- name[2]    # название второго пункта
progr <- round(user_km / sum(lengths_km) * 100, 1)    # пройденная доля всего маршрута в %
progr_in_section <- lengths_km[num] - (route_lengths[num] - user_km)    # пройденная доля маршрута внутри секции
height <- progr_in_section * heights_diff[num] / lengths_km[num] + heights_start[num]    # примерная высота
height <- round(height, 1)
print(paste("Вы находитесь между городами ", name1, " и ", name2, ". Пройдено ",
      progr, "% маршрута. Ваша высота примерно ", height, " м.", sep = ""))


