rm(list = ls())

# a helpful constant for degrees-to-radians conversion
D2R = pi / 180

# other constants
speed = 850    # aircraft speed
dPrec = 0    # distance precision (0 decimals  -->  1 km)
tPrec = 0.5    # time precision (0.5 hours or 30 min)
r = 6371    # Earth radius

# ask for user input
lat1 = as.numeric(readline("Enter the latitude of the first geographical spot: "))
lon1 = as.numeric(readline("Enter the longitude of the first geographical spot: "))
lat2 = as.numeric(readline("Enter the latitude of the second geographical spot: "))
lon2 = as.numeric(readline("Enter the longitude of the second geographical spot: "))

# check that all parameters comply to basic rules:
stopifnot(
  lat1 >= -90, lat1 <= 90,
  lat2 >= -90, lat2 <= 90,
  lon1 >= -180, lon1 <= 180,
  lon2 >= -180, lon2 <= 180
)

# convert degrees to radians
lat1 = lat1 * D2R
lat2 = lat2 * D2R
lon1 = lon1 * D2R
lon2 = lon2 * D2R

# central angle between two points:
dSigma = acos(sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(lon2 - lon1))

# precise distance:
d = dSigma * r

# precise time:
t = d / speed

# rounded distance and time:
dr = round(d, dPrec)
tr = round(t / tPrec) * tPrec

# find out whether the route crosses prime or anti- meridians (0, 180 deg.):
# we presume the route doesn't cross neither meridian:
primeFlag = F
antiFlag = F
# if longitude signs of two points differ, then the route crosses one of the meridians.
# sum of longitude values modules is used as a criterion to choose the right meridian.
if (sign(lon1) != sign(lon2)) {
  if ((abs(lon1) + abs(lon2)) <= 180 * D2R) {
    primeFlag = T
  } else {
    antiFlag = T
  }
}

# having the output message broken down into substring elements, we determine 
# their final contents provided the above set flags context
if (primeFlag) {
  primeMessage = 'crosses the prime meridian'
} else {
  primeMessage = "doesn't cross the prime meridian"
}

if (antiFlag) {
  antiMessage = 'crosses the anti-meridian.'
} else {
  antiMessage = "doesn't cross the anti-meridian."
}

# the output message
message = paste('The flight length: ', dr, ' km. The flight duration: ~',
                tr, ' h. The route ', primeMessage,
                ' and ', antiMessage, sep = '')

print(message)



# Geographical coordinates examples:
# (55.55, 37.97), (23.55, 15.1)
# (65.55, 166.67), (67.2, -165.4)
# (55.55, 37.97), (-23.55, -15.1)

