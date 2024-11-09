gen_numere <- function(n) {
  # Functie pentru generarea primei valori x1
  gen_x1 <- function() {
    cnt <- 0
    repeat {
      # Citim timpul sistemului
      time <- format(Sys.time(), "%H:%M:%S")
      h <- as.numeric(substr(time, 1, 2))
      m <- as.numeric(substr(time, 4, 5))
      s <- as.numeric(substr(time, 7, 8))
      t1 <- m * 100 + s
      
      t1_mod_23 <- t1 %% 23
      
      if (t1_mod_23 == 0) {
        return(rnorm(1, mean = m, sd = s))
      } else if (t1_mod_23 == 3) {
        return(rpois(1, lambda = m) + runif(1, min = -1, max = 1))
      } else if (t1_mod_23 == 5) {
        return(rexp(1, rate = h))
      } else if (t1_mod_23 == 7) {
        return(rbinom(1, size = h, prob = 1/m) + runif(1, min = 0, max = 5))
      } else if (t1_mod_23 == 8) {
        return(runif(1, min = -5, max = 7))
      } else if (t1_mod_23 == 11) {
        return(rgamma(1, shape = h, rate = 1) - rhyper(1, m = m, n = 23, k = s))
      }
      
      cnt <- cnt + 1
      if (cnt == 2) {
        return(rnorm(1, mean = 0, sd = 1))
      }
    }
  }
  
  # Generam prima valoare x1
  x1 <- gen_x1()
  
  # Initializam vectorul de rezultate
  rez <- numeric(n)
  rez[1] <- x1
  
  # Generam restul valorilor xn
  for (i in 2:n) {
    a <- rexp(1, rate = 5)
    b <- rnorm(1, mean = 2, sd = 1)
    rez[i] <- a * rez[i - 1] + b
  }
  
  # Returnam vectorul de rezultate si realizam histograma
  hist(rez, main = "Histograma", xlab = "Valori", ylab = "Frecventa", col = "blue")
  return(rez)
}

# Exemplu de utilizare
set.seed(6)  # Setam un seed pentru reproductibilitate
gen <- gen_numere(100)
print(gen)