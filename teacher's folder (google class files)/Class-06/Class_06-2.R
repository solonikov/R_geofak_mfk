library(dplyr)
library(readr)
library(ggplot2)
tab = read_csv2('trees.csv') |> 
  setNames(c('Species', 'Index', 'Units', 'Value')) |> 
  filter(Index == 'Площадь земель, занятых лесными насаждениями (покрытых лесной растительностью), всего')
ggplot(tab) +
  geom_col(aes(Species, Value,
               fill = Species)) +
  coord_flip() +
  scale_fill_discrete(
    type = sample(colors(), nrow(tab))
  )
ggplot(tab) +
  geom_col(aes('', Value, fill = Species),
           color = 'black', 
           size = 0.1) +
  coord_polar(theta = 'y')

