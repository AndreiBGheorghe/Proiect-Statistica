#a)
# Functie pentru calculul constantei de normalizare
calc_const <- function() {
  int <- integrate(function(x) x^4,0,2)$value
  c <- 1/int
  return(c)
}
# Functie pentru calculul mediei
calc_media <- function(c) {
  media <- integrate(function(x) c*x^5,0,2)$value
  return(media)
}
# Functie pentru calculul variantei
calc_var <- function(c, media) {
  media_sq <- integrate(function(x) c*x^6,0,2)$value
  var <- media_sq - media^2
  return(var)
}
c <- calc_const()
print(c)

media <- calc_media(c)
print(media)

var <- calc_var(c, media)
print(var)


#b)
# Functie pentru calculul constantei de normalizare
calc_const <- function() {
  a <- 1
  b <- 6*(1-a/2)/3
  return(c(a,b))
}
# Functie pentru calculul mediei
calc_media <- function(a, b) {
  media <- integrate(function(x) x*(a*x+b*x^2),0,1)$value
  return(media)
}
# Functie pentru calculul variantei
calc_var <- function(a, b, media) {
  media_sq <- integrate(function(x) x^2*(a*x+b*x^2),0,1)$value
  var <- media_sq - media^2
  return(var)
}
const <- calc_const()
a <- const[1]
b <- const[2]
print(a,b)

media <- calc_media(a, b)
print(media)

var <- calc_var(a, b, media)
print(var)


#c)
# Functie pentru calculul mediei
calc_media <- function() {
  media <- sum(sapply(1:1000, function(x) x*4/(x*(x+1)*(x+2))))
  return(media)
}
# Functie pentru calculul variantei
calc_var <- function(media) {
  media_sq <- sum(sapply(1:1000, function(x) x^2*4/(x*(x+1)*(x+2))))
  var <- media_sq - media^2
  return(var)
}
media <- calc_media()
print(media)

var <- calc_var(media)
print(var)


#d)
# Functie pentru calculul densitatilor de probabilitate
calc_dens <- function(x) {
  return(log10(x/(x+1)))
}
# Functie pentru calculul mediei
calc_media <- function() {
  x_values <- 1:9
  densitati <- sapply(x_values, calc_dens)
  media <- sum(x_values * densitati)
  return(media)
}
# Functie pentru calculul variantei
calc_var <- function(media) {
  x_values <- 1:9
  densitati <- sapply(x_values, calc_dens)
  media_sq <- sum((x_values^2) * densitati)
  var <- media_sq - media^2
  return(var)
}
media <- calc_media()
print(media)

var <- calc_var(media)
print(var)


#e)
# Functie pentru calculul densitatilor de probabilitate
calc_dens <- function(x, teta) {
  if (x > 0) {
    return((teta^2 / (1 + teta)) * (1 + x) * exp(-teta * x))
  } else {
    return(0)
  }
}
# Functie pentru calculul mediei
calc_media <- function(teta) {
  x_values <- seq(0, 100, by=0.01)
  densitati <- sapply(x_values, calc_dens, teta)
  media <- sum(x_values * densitati) * 0.01
  return(media)
}
# Functie pentru calculul variantei
calc_var <- function(teta, media) {
  x_values <- seq(0, 100, by=0.01)
  densitati <- sapply(x_values, calc_dens, teta)
  media_sq <- sum((x_values^2) * densitati) * 0.01
  var <- media_sq - media^2
  return(var)
}
teta <- 2 #exemplu
media <- calc_media(teta)
print(media)

var <- calc_var(teta, media)
print(var)


#f)
# Functie pentru calculul densitatilor de probabilitate
calc_dens <- function(x) {
  if (x < 0) {
    return((1/3) * exp(x))
  } else if (0 <= x && x < 1) {
    return(1/3)
  } else if (x >= 1) {
    return((1/3) * exp(-(x-1)))
  } else {
    return(0)
  }
}
# Functie pentru calculul mediei
calc_media <- function() {
  x_values <- seq(-10, 10, by=0.01)
  densitati <- sapply(x_values, calc_dens)
  media <- sum(x_values * densitati) * 0.01
  return(media)
}
# Functie pentru calculul variantei
calc_var <- function(media) {
  x_values <- seq(-10, 10, by=0.01)
  densitati <- sapply(x_values, calc_dens)
  media_sq <- sum((x_values^2) * densitati) * 0.01
  var <- media_sq - media^2
  return(var)
}
media <- calc_media()
print(media)

var <- calc_var(media)
print(var)


#g)
# Functie pentru calculul densitatilor de probabilitate
calc_dens <- function(x) {
  return(1 / (pi * (1 + x^2)))
}
# Functie pentru calculul mediei
calc_media <- function() {
  x_values <- seq(-1000, 1000, by=0.01)
  densitati <- sapply(x_values, calc_dens)
  media <- sum(x_values * densitati) * 0.01
  return(media)
}
# Functie pentru calculul variantei
calc_var <- function(media) {
  x_values <- seq(-1000, 1000, by=0.01)
  densitati <- sapply(x_values, calc_dens)
  media_sq <- sum((x_values^2) * densitati) * 0.01
  var <- media_sq - media^2
  return(var)
}
media <- calc_media()
print(media)

var <- calc_var(media)
print(var)