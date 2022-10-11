library(tidyverse)
tab = read_csv2('data/forest.csv') %>% 
  filter(naimenovanie_pokazatelya == "Площадь земель, занятых лесными насаждениями (покрытых лесной растительностью), всего")

par(mar = c(5, 10, 4, 2))
barplot(tab$znachenie_pokazatelya,
        names.arg = tab$preobladayushchie_drevesnye_i_kustarnikovye_porody, 
        horiz = TRUE, 
        las = 1)
