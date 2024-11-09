set.seed(6)

# Generam 100 de intrări
n <- 100

# Generam datele pentru fiecare coloana
pret <- round(runif(n, 50, 300), 2) 
gen_muzical <- sample(c("Rock", "Pop", "Jazz", "Classical", "Hip-Hop", "Electronic"), n, replace = TRUE)
data <- sample(seq(as.Date('2023-01-01'), as.Date('2023-12-31'), by="day"), n, replace = TRUE)
oras <- sample(c("Bucuresti", "Cluj-Napoca", "Timisoara", "Iasi", "Brasov", "Constanta"), n, replace = TRUE)
artisti <- replicate(n, list(sample(c("Eminem", "Florin Salam", "Rihanna", "Drake", "Tyga", "Mihai Treistariu"), sample(1:3, 1), replace = TRUE)), simplify = FALSE)
soldout <- sample(c("da", "nu"), n, replace = TRUE)

# Construim dataframe-ul
c_data <- data.frame(
  Pret = pret,
  Gen_Muzical = gen_muzical,
  Data = data,
  Oras = oras,
  Artisti = I(artisti),
  SoldOut = soldout,
  stringsAsFactors = FALSE
)

c_data$SoldOut <- sample(c("da", "nu"), nrow(c_data), replace = TRUE, prob = c(0.01, 0.99))

f1 <- function(buget, intervale) {
  # Convertim datele de start si de sfarsit ale intervalelor in format Date
  intervale <- lapply(intervale, function(x) {
    list(start = as.Date(x$start), end = as.Date(x$end))
  })
  # Filtram evenimentele care se incadreaza in intervalele de timp specificate
  evenimente_filtrate <- c_data[FALSE, ]
  for (interval in intervale) {
    evenimente_filtrate <- rbind(evenimente_filtrate, subset(c_data, Data >= interval$start & Data <= interval$end))
  }
  # Functie recursiva pentru a gasi combinatiile de evenimente care se incadreaza in buget
  combina_evenimente <- function(evenimente, buget) {
    if (nrow(evenimente) == 0) {
      return(list())
    }
    rezultate <- list()
    for (i in 1:nrow(evenimente)) {
      eveniment <- evenimente[i, ]
      pret <- eveniment$Pret
      if (pret <= buget) {
        ramase <- evenimente[-i, ]
        sub_comb <- combina_evenimente(ramase, buget - pret)
        if (length(sub_comb) == 0) {
          rezultate <- append(rezultate, list(list(eveniment)))
        } else {
          for (comb in sub_comb) {
            rezultate <- append(rezultate, list(c(list(eveniment), comb)))
          }
        }
      }
    }
    return(rezultate)
  }
  # Obtinem toate combinatiile de evenimente care se incadreaza in buget
  combinatii <- combina_evenimente(evenimente_filtrate, buget)
  # Convertim rezultatele in format usor de citit
  combinatii_formate <- lapply(combinatii, function(comb) {
    do.call(rbind, comb)
  })
  return(combinatii_formate)
}

f2 <- function(buget, intervale) {
  intervale <- lapply(intervale, function(x) {
    list(start = as.Date(x$start), end = as.Date(x$end))
  })
  # Filtram evenimentele care se incadreaza in intervalele de timp specificate și care nu sunt soldout
  evenimente_filtrate <- c_data[FALSE, ]
  for (interval in intervale) {
    evenimente_filtrate <- rbind(evenimente_filtrate, subset(c_data, Data >= interval$start & Data <= interval$end & SoldOut == "nu"))
  }
  combina_evenimente <- function(evenimente, buget) {
    if (nrow(evenimente) == 0) {
      return(list())
    }
    rezultate <- list()
    for (i in 1:nrow(evenimente)) {
      eveniment <- evenimente[i, ]
      pret <- eveniment$Pret
      if (pret <= buget) {
        ramase <- evenimente[-i, ]
        sub_comb <- combina_evenimente(ramase, buget - pret)
        if (length(sub_comb) == 0) {
          rezultate <- append(rezultate, list(list(eveniment)))
        } else {
          for (comb in sub_comb) {
            rezultate <- append(rezultate, list(c(list(eveniment), comb)))
          }
        }
      }
    }
    return(rezultate)
  }
  combinatii <- combina_evenimente(evenimente_filtrate, buget)
  combinatii_formate <- lapply(combinatii, function(comb) {
    do.call(rbind, comb)
  })
  return(combinatii_formate)
}

intervale <- list(
  list(start = '2023-06-01', end = '2023-06-30'),
  list(start = '2023-09-01', end = '2023-09-30')
)
buget <- 200
combinatii_evenimente <- f1(buget, intervale)
print(combinatii_evenimente)
combinatii_soldout <- f2(buget, intervale)
print(combinatii_soldout)