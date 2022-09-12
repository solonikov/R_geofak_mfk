a = as.integer(readline('Введите целое число: '))
if (is.integer(a)) {
  rem = a %% 2
  if (rem == 0) {
    cat('Число является чётным')
  } else {
    cat('Число является нечётным')
  }
}