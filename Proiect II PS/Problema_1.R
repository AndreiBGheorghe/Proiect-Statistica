set.seed(6)
N <- 10^6

#a)
f_a <- function(x) {
  1/((x^5*(1-x))^(1/6))
}
x_a <- runif(N, 0, 1)
valori_a <- f_a(x_a)
integrala_a <- mean(valori_a)
val_teoretica_a <- integrate(f_a, 0, 1)
print(integrala_a)
print(val_teoretica_a)

#b)
f_b <- function(x) {
  sin(x)^3*cos(x)^5
}
x_b <- runif(N, 0, pi/2)
valori_b <- f_b(x_b)
integrala_b <- mean(valori_b)*(pi/2)
val_teoretica_b <- integrate(f_b, 0, pi/2)
print(integrala_b)
print(val_teoretica_b)


#c)
f_c <- function(x) {
  x^2/(1+x^4)
}
transformare <- function(t) {
  t/(1-t)
}
jacobian <- function(t) {
  1/(1-t)^2
}
x_c <- runif(N, 0, 1)
x_c2 <- transformare(x_c)
valori_c <- f_c(x_c2)*jacobian(x_c)
integrala_c <- mean(valori_c)
val_teoretica_c <- integrate(f_c, 0, Inf)
print(integrala_c)
print(val_teoretica_c)


#d)
f_d <- function(x) {
  exp(-x^2+2*x-4)
}
x_d <- runif(N, 0, 1)
x_d2 <- 1/x_d
jacobian_d <- 1/x_d^2
valori_d <- f_d(x_d2)*jacobian_d
integrala_d <- mean(valori_d)
val_teoretica_d <- integrate(f_d, 0, Inf)
print(integrala_d)
print(val_teoretica_d)


#e)
f_e <- function(x, y) {
  exp(sqrt(x^2+y^2))
}
regiune_e <- function(x, y) {
  return(x^2+y^2>=1 & x^2+y^2<=4 & x>=0 & x<=y)
}
x_e <- runif(N, -2, 2)
y_e <- runif(N, 0, 2)
regiune <- regiune_e(x_e, y_e)
valori_e <- f_e(x_e[regiune], y_e[regiune])
integrala_e <- mean(valori_e)*4*pi
print(integrala_e)


#f)
f_f <- function(x, y) {
  sqrt(x^2+y^2)
}
regiune_f <- function(x, y) {
  return(x^2+y^2>=4 & x^2+y^2<=9 & y>=0)
}
x_f <- runif(N, -3, 3)
y_f <- runif(N, 0, 3)
regiune <- regiune_f(x_f, y_f)
valori_f <- f_f(x_f[regiune], y_f[regiune])
integrala_f <- mean(valori_f)*5*pi
print(integrala_f)


#g)
f_g <- function(x, y) {
  exp(-x-y)
}
x_g <- runif(N, 0, pi/2)
y_g <- runif(N, 0, pi/2)
valori_g <- f_g(x_g, y_g)
integrala_g <- mean(valori_g)*(pi/2)^2
print(integrala_g)


#h)
U <- runif(N, 0, 1)
x_h <- U
y_h <- sqrt(1-U^2)
corelatie <- cor(x_h, y_h)
print(corelatie)


#i)
x_i <- U^2
y_i <- sqrt(1-U^2)
corelatie <- cor(x_i, y_i)
print(corelatie)