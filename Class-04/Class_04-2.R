# Функция зависимости скорости от угла наклона
tobler_speed = function(slope) {
  6 * exp(-3.5 * abs(slope + 0.05))
}

hiking_time = function(profile) {
  dh = diff(profile[, 2])       # превышения
  dl = diff(profile[, 1])       # длины участков
  ds = sqrt(dl^2 - dh^2)        # гориз проложения
  slopes = dh/ds                # уклоны участков
  speeds = tobler_speed(slopes) # скорости на участках
  return(sum(dl / speeds))      # суммарное время похода
}

p = cbind(1:10, runif(10, 0.5, 1)) # в км
hiking_time(p)
