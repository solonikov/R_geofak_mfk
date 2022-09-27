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

# заполняем его информацией в цикле
for (num in 1:(length(data) - 1)) {
  start_id <- data$id[[num]]    # номер начального пункта
  stop_id <- data$id[[num + 1]]    # номер конечного пункта
  section_name <- paste(data$name[[num]], data$name[[num + 1]], sep = '_')    # имя секции
  length_km <- data$distFromSource[[num + 1]] - data$distFromSource[[num]]    # длина секции
  H_diff <- data$zeroLevel[[num + 1]] - data$zeroLevel[[num]]    # перепад высоты между пунктами
  slope <- abs(round(H_diff / length_km, 2))    # уклон
  # считаем расстояние между пунктами по прямой по координатам:
  lat1 <- data$lat[[num]] * pi / 180
  lat2 <- data$lat[[num + 1]] * pi / 180
  lon1 <- data$lon[[num]] * pi / 180
  lon2 <- data$lon[[num + 1]] * pi / 180
  dSigma = acos(sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(lon2 - lon1))    # центральный угол между пунктами
  distance_coord <- dSigma * 6371    # расстояние между пунктами по прямой по координатам
  meanderCoeff <- round(length_km / distance_coord, 2)    # коэффициент извилистости
  # прикрепляем создаваемую в каждой итерации цикла запись в конец датафрейма
  sections <- rbind(sections, list(start_id, stop_id, section_name, length_km,
                                   H_diff, slope, meanderCoeff))
}

# устанавливаем имена столбцов
colnames(sections) <- c('start_id', 'stop_id', 'section_name',
                        'length_km', 'H_diff', 'slope', 'meanderCoeff')
print(sections)

# просим пользователя ввести расстояние:
user_km <- as.numeric(readline("Введите расстояние в км: "))


