rm(list = ls())

library(tidyverse)
library(readxl)
library(readr)
library(RColorBrewer)


## График 1
# чтение данных
tab <- read_table("graph.txt",
                  skip = 5,
                  col_names = c("Year", "No_Smoothing", "Lowess"))

# сперва строим график со сглаженной кривой
plot(x = tab$Year, y = tab$Lowess,
     type = 'l', col = 'blue', lwd = 2,
     main = 'Аномалия температуры по отношению к средней
     за период 1951-1980 (по данным NASA, 2020 г.)',
     xlab = 'Год', ylab = 'ºC', yaxt = 'n', ylim = c(-0.51, 1.01))

grid()    # сетка координат

abline(h = 0, col = 'red')    # добавляем прямую 0ºC

# доюавляем несглаженную ломаную и точки
lines(x = tab$Year, y = tab$No_Smoothing, col = 'gray', type = 'b', pch = 19)

# размещаем легенду в левос верхнем углу
legend(x = 'topleft', c('Скользящее среднее', 'Среднегодовая температура'),
       lwd = c(2, 1), col = c('blue', 'gray'), pch = c(NA, 19))

# добавляем размеченную вручную ось Y
axis(side = 2,
     at = seq(-0.5, 1, 0.5),
     tck = -0.02)


## График 2
# чтение данных
tab2 <- read_csv('fao_treecover_extent__ha.csv') |> 
  na.omit() |>     # убираем пропуски
  # рассчитываем площадь разных типов лесного покрова в процентах
  summarize(Первичный = sum(primary_forest__ha) / sum(fao_treecover__ha) * 100,
        Восстановленный =
              sum(regenerated_forest__ha) / sum(fao_treecover__ha) * 100,
        Высаженный = sum(planted_forest__ha) / sum(fao_treecover__ha) * 100) |> 
  # приводим к длинному виду из широкого
  pivot_longer(cols = everything(),
               names_to = 'forest_type',
               values_to = 'value') |>
  # округляем величины в процентах до целых значений
  mutate(value = round(value, 0))

# круговая диаграмма
pie(tab2$value,
    labels = paste0(tab2$forest_type, ' (', tab2$value, '%)'),
    main = 'Структура лесного покрова земли\n(по данным ФАО, 2015 г.)',
    clockwise = T,
    #col = brewer.pal(n = length(tab2$value), 'Greens'),
    col = c('#018b3e', '#7ccd75', '#bcee56')
    )


## График 3
# чтение данных
tab3 <- read_excel('PgC.xlsx', skip = 3) |>
  select(1, 3) |> 
  rename(Climate_Region = `...1`, PgC = `PgC...3`) |> 
  filter(!stringr::str_detect(Climate_Region, 'Total')) |> 
  mutate(PgC = as.numeric(PgC))

# увеличиваем левое поле, чтобы влезали подписи
margins.default = par('mar') #запишем текущее значение, чтобы восстановить позже
par(mar = c(5, 10, 4, 2))    # задаём новые величины полей

# горизонтальная столбчатая диаграмма
barplot(tab3$PgC, names.arg = tab3$Climate_Region, horiz = T, las = 1,
        xaxt = 'n', xlim = c(0, max(tab3$PgC)), xlab = 'PgC', 
        main = 'Содержание органического углерода в верхнем слое почвы (0-30 см)
        по климатическим регионам IPCC (2010 г.)', 
        col = rainbow(nrow(tab3)))

# добавляем размеченную вручную ось X
axis(side = 1,
     at = seq(0, 150, 50),
     tck = -0.02)

# восстанавливаем величины полей по умолчанию
par(mar = margins.default)


## График 4
# чтение данных
tab4 <- read_excel('GVP_Volcano_List_Holocene.xlsx', skip = 1)

# задаём значения карманов гистограммы
my_seq <- seq(-6000, 8000, 500)
# в соответствии с ними задаём цвета
my_colors <- rep('#ff8046', length(my_seq))
my_colors[my_seq < 0] <- '#02eeef'    # для подводных вулканов - голубой цвет

# строим гистограмму
hist(tab4$`Elevation (m)`, breaks = my_seq,
     xlab = 'Абсолютная отметка вершины над уровнем моря, м',
     ylab = 'Количество',
     main = 'Распределение высот действующих вулканов мира
     (по данным Смитсоновского института, 2020 г.)',
     col = my_colors)

# размещаем легенду справа
legend( 'right', c('Подводные', 'Надводные'), fill = c('#02eeef', '#ff8046'))





