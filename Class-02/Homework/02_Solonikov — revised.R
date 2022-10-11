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
colnames(data) = c('id', 'name', 'lat', 'lon', 'distFromSource',
                   'distToMouth', 'zeroLevel')

print(data)

# константа для переводы градусов в радианы:
PIRAD <- pi / 180

# Номера пунктов:
ids <- as.numeric(data$id)
# номера начальных пунктов:
start_ids <- ids[1:length(ids) - 1]
# номера конечных пунктов:
stop_ids <- ids[2:length(ids)]

# Высоты и координаты пунктов для дальнейших вычислений:
# высоты пунктов:
H <- as.numeric(data$zeroLevel)
# широты начальных пунктов секций:
start_lats <- as.numeric(data$lat)[1:nrow(data) - 1] * PIRAD
# широты конечных пунктов секций:
stop_lats <- as.numeric(data$lat)[2:nrow(data)] * PIRAD
# долготы начальных пунктов секций:
start_lons <- as.numeric(data$lon)[1:nrow(data) - 1] * PIRAD
# долготы конечных пунктов секций:
stop_lons <- as.numeric(data$lon)[2:nrow(data)] * PIRAD

# Имена секций:
sec_names <- paste(data$name[start_ids], "_", data$name[stop_ids],
                       sep = "")

# Вычисляем длины секций:
# для этого сперва берём расстояния от истока:
dSource <- as.numeric(data$distFromSource)
len <- dSource[stop_ids] - dSource[start_ids]
# длина пройденного пути от начала маршрута до конца данной секции:
route_len <- cumsum(len)

# Вычисляем перепад высот и уклон:
# высоты начальных пунктов секций:
H_start <- H[1:length(ids) - 1]
# высоты конечных пунктов секций:
H_stop <- H[2:length(ids)]
# перепады высот между начальным и конечным пунктами секции:
H_diff <- H_stop - H_start
# уклоны:
slopes <- abs(round(H_diff / len, 2))

# Высичлем коэффициенты извилистости:
# для этого сперва считаем расстояние между пунктами по прямой по координатам:
# центральные углы между пунктами:
dSigmas <- acos(sin(start_lats) * sin(stop_lats) + cos(start_lats) * cos(
  stop_lats) * cos(stop_lons - start_lons))
# расстояния между пунктами по прямой по координатам:
dists_coord <- dSigmas * 6371
# коэффициенты извилистости:
meander <- round(len / dists_coord, 2)

# Собираем всё в датафрейм:
sections <- data.frame(start_ids, stop_ids, sec_names, len,
                       H_diff, slopes, meander)
print(sections)

# просим пользователя ввести расстояние:
user_km <- as.numeric(readline("Введите расстояние в км: "))

# Создаём строку для вывода и выводим информацию о пройденном пути:

# сперва находим номер секции, соответствующий вводу пользователя:
num <- which(user_km <= route_len)[1] 

# получаем названия пунктов:
name <- strsplit(sections$sec_names[[num]], '_')[[1]]
# название первого пункта:
name1 <- name[1]
# название второго пункта:
name2 <- name[2]
# пройденная доля всего маршрута в %:
progr <- round(user_km / sum(len) * 100, 1)
# пройденная доля маршрута внутри секции:
progr_in_sec <- len[num] - (route_len[num] - user_km)
# примерная высота:
height <- progr_in_sec * (
  H_diff[num] / len[num]) + H_start[num]
height <- round(height, 1)

cat("Вы находитесь между городами ", name1, " и ", name2, ". Пройдено ",
    progr, "% маршрута. Ваша высота примерно ", height, " м.", sep = "")


