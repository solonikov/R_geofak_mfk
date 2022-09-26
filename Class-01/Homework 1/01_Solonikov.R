# auxiliary function to convert degrees to radians
deg2rad <- function(x) {x*pi/180}


flight_info <- function(point1, point2, speed=850, dist_precis=0, time_precis=0.5, r=6371) {
  
  # check that all parameters are of numeric type and comply to basic rules:
  stopifnot(is.numeric(point1), is.numeric(point2), length(point1) == 2, length(point2) == 2,
            is.numeric(speed), is.numeric(dist_precis), is.numeric(time_precis), is.numeric(r),
            point1[1] >= -90, point1[1] <= 90, point2[1] >= -90, point2[1] <= 90,
            point1[2] >= -180, point1[2] <= 180, point2[2] >= -180, point2[2] <= 180)
  
  # assign latitude and longitude values to different variables and convert degrees to radians:
  lat1 = deg2rad(point1[1])
  lon1 = deg2rad(point1[2])
  lat2 = deg2rad(point2[1])
  lon2 = deg2rad(point2[2])
  
  # central angle between two points:
  dsigma = acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2)*cos(lon2-lon1))
  
  # precise distance:
  distance = dsigma*r
  
  # precise time:
  time = distance/speed
  
  # rounded distance and time:
  distance_r = round(distance, dist_precis)
  time_r = round(time/time_precis)*time_precis
  
  # find out whether the route crosses prime or anti- meridians (0, 180 deg.).
  # we presume the route doesn't cross neither meridian:
  prime_med_flag = F
  anti_med_flag = F
  # if longitude signs of two points differ, then the route crosses one of the meridians.
  # sum of longitude values modules is used as a criterion to choose the right meridian.
  if (sign(lon1) != sign(lon2)) {
    if ((abs(lon1) + abs(lon2)) <= deg2rad(180)) {
      prime_med_flag = T
    } else {
      anti_med_flag = T
    }
  }
  
  return(list('distance'=distance_r, 'time'=time_r, 'prime_meridian'=prime_med_flag, 'anti_meridian' = anti_med_flag))
}


# interface:
flight_info_interface <- function() {
  p1 = as.numeric(strsplit(readline("Enter geographical coordinates of the first point as follows: Latitude Longitude --- "), split = ' ')[[1]])
  p2 = as.numeric(strsplit(readline("Enter geographical coordinates of the second point as follows: Latitude Longitude --- "), split = ' ')[[1]])
  result = flight_info(p1, p2)
  prime_meridian_message = ifelse(result$prime_meridian, '', 'не ')
  anti_meridian_message = ifelse(result$anti_meridian, '', 'не ')
  message = paste('Длина полёта составила ', result$distance, ' километров. Время в пути ~', result$time,
                  ' часов. Маршрут полёта ', prime_meridian_message, 'пересекает нулевой меридиан и ',
                  anti_meridian_message, 'пересекает 180-й меридиан.', sep = '')
  print(message)
}


# flight_info(c(55.55, 37.97), c(23.55, 15.1))
# flight_info(c(65.55, 166.67), c(67.2, -165.4))
# flight_info(c(55.55, 37.97), c(-23.55, -15.1))

flight_info_interface()

