#Setam seed-ul pentru reproducibilitate
set.seed(6)

#Datele problemei
zile_luna <- 30  # Numarul de zile din iunie 2024
medie <- 512  # Media calatorilor pe zi
min_calatori <- 210  # Numarul minim de calatori pe zi
max_calatori <- 983  # Numarul maxim de calatori pe zi
procent_lejere <- 0.20  # Procentul zilelor lejere
procent_normale <- 0.50  # Procentul zilelor normale
procent_aglomerate <- 0.30  # Procentul zilelor aglomerate

#Generam numarul de zile pentru fiecare categorie
numar_zile_lejere <- round(zile_luna * procent_lejere)
numar_zile_normale <- round(zile_luna * procent_normale)
numar_zile_aglomerate <- zile_luna - numar_zile_lejere - numar_zile_normale

#Generam numarul de calatori pentru fiecare tip de zi
calatori_lejere <- runif(numar_zile_lejere, min = min_calatori, max = 350)
calatori_normale <- runif(numar_zile_normale, min = 351, max = 670)
calatori_aglomerate <- runif(numar_zile_aglomerate, min = 671, max = max_calatori)

#Combinam toate valorile intr-un singur vector
calatori_zi <- c(calatori_lejere, calatori_normale, calatori_aglomerate)

#Amestecam vectorul pentru a simula ordinea aleatoare a zilelor
calatori_zi <- sample(calatori_zi)

#Construim histograma valorilor
hist(calatori_zi, breaks = 10, main = "Histograma numar calatori pe zi în iunie 2024",
     xlab = "Numar calatori pe zi", ylab = "Frecventa", col = "magenta")

#B
set.seed(6)

# Datele problemei
zile_luna <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)  # Numarul de zile pentru fiecare luna în 2024
medie <- 512  
min_calatori <- 210  
max_calatori <- 983  
procent_lejere <- 0.20 
procent_normale <- 0.50 
procent_aglomerate <- 0.30  

# Functie pentru generarea calatorilor pentru o luna
genereaza_calatori <- function(zile) {
  numar_zile_lejere <- round(zile * procent_lejere)
  numar_zile_normale <- round(zile * procent_normale)
  numar_zile_aglomerate <- zile - numar_zile_lejere - numar_zile_normale
  
  calatori_lejere <- runif(numar_zile_lejere, min = min_calatori, max = 350)
  calatori_normale <- runif(numar_zile_normale, min = 351, max = 670)
  calatori_aglomerate <- runif(numar_zile_aglomerate, min = 671, max = max_calatori)
  
  calatori_zi <- c(calatori_lejere, calatori_normale, calatori_aglomerate)
  calatori_zi <- sample(calatori_zi)
  
  return(calatori_zi)
}

# Dataframe pentru centralizarea rezultatelor
rezultate <- data.frame(
  luna = character(),
  medie_calatori = numeric(),
  minim_calatori = numeric(),
  maxim_calatori = numeric(),
  procent_lejere = numeric(),
  procent_normale = numeric(),
  procent_aglomerate = numeric(),
  stringsAsFactors = FALSE
)

# Generam datele pentru fiecare luna si calculam statisticile
for (i in 1:12) {
  calatori_zi <- genereaza_calatori(zile_luna[i])
  
  medie_calatori <- mean(calatori_zi)
  minim_calatori <- min(calatori_zi)
  maxim_calatori <- max(calatori_zi)
  
  procent_lejere <- sum(calatori_zi < 350) / zile_luna[i]
  procent_normale <- sum(calatori_zi >= 351 & calatori_zi <= 670) / zile_luna[i]
  procent_aglomerate <- sum(calatori_zi > 671) / zile_luna[i]
  
  rezultate <- rbind(rezultate, data.frame(
    luna = month.name[i],
    medie_calatori = medie_calatori,
    minim_calatori = minim_calatori,
    maxim_calatori = maxim_calatori,
    procent_lejere = procent_lejere,
    procent_normale = procent_normale,
    procent_aglomerate = procent_aglomerate
  ))
}

# Afisam rezultatele
print(rezultate)

#C)
library(ggplot2)
set.seed(6)

# Datele problemei
zile_luna <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)  
medie <- 512  
min_calatori <- 210 
max_calatori <- 983  
procent_lejere <- 0.20 
procent_normale <- 0.50  
procent_aglomerate <- 0.30  
procent_abonamente <- 0.38  
procent_platitori_bilet <- 0.74  # Procentul pasagerilor care platesc bilet
pret_bilet <- 3  # Pretul unui bilet
pret_abonament <- 70  # Pretul unui abonament

# Functie pentru generarea calatorilor pentru o luna
genereaza_calatori <- function(zile) {
  numar_zile_lejere <- round(zile * procent_lejere)
  numar_zile_normale <- round(zile * procent_normale)
  numar_zile_aglomerate <- zile - numar_zile_lejere - numar_zile_normale
  
  calatori_lejere <- runif(numar_zile_lejere, min = min_calatori, max = 350)
  calatori_normale <- runif(numar_zile_normale, min = 351, max = 670)
  calatori_aglomerate <- runif(numar_zile_aglomerate, min = 671, max = max_calatori)
  
  calatori_zi <- c(calatori_lejere, calatori_normale, calatori_aglomerate)
  calatori_zi <- sample(calatori_zi)
  
  return(calatori_zi)
}

# Dataframe pentru centralizarea rezultatelor
rezultate <- data.frame(
  luna = character(),
  medie_calatori = numeric(),
  minim_calatori = numeric(),
  maxim_calatori = numeric(),
  procent_lejere = numeric(),
  procent_normale = numeric(),
  procent_aglomerate = numeric(),
  pasageri_abonament = numeric(),
  pasageri_platitori_bilet = numeric(),
  pasageri_neplatitori_bilet = numeric(),
  venituri_bilete = numeric(),
  venituri_abonamente = numeric(),
  venituri_nerealizate = numeric(),
  stringsAsFactors = FALSE
)

# Generam datele pentru fiecare luna si calculam statisticile
for (i in 1:12) {
  calatori_zi <- genereaza_calatori(zile_luna[i])
  
  medie_calatori <- mean(calatori_zi)
  minim_calatori <- min(calatori_zi)
  maxim_calatori <- max(calatori_zi)
  
  procent_lejere <- sum(calatori_zi < 350) / zile_luna[i]
  procent_normale <- sum(calatori_zi >= 351 & calatori_zi <= 670) / zile_luna[i]
  procent_aglomerate <- sum(calatori_zi > 671) / zile_luna[i]
  
  pasageri_abonament <- sum(calatori_zi) * procent_abonamente
  pasageri_fara_abonament <- sum(calatori_zi) - pasageri_abonament
  pasageri_platitori_bilet <- pasageri_fara_abonament * procent_platitori_bilet
  pasageri_neplatitori_bilet <- pasageri_fara_abonament * (1 - procent_platitori_bilet)
  
  venituri_bilete <- pasageri_platitori_bilet * pret_bilet
  venituri_abonamente <- (pasageri_abonament / zile_luna[i]) * pret_abonament
  venituri_nerealizate <- pasageri_neplatitori_bilet * pret_bilet
  
  rezultate <- rbind(rezultate, data.frame(
    luna = month.name[i],
    medie_calatori = medie_calatori,
    minim_calatori = minim_calatori,
    maxim_calatori = maxim_calatori,
    procent_lejere = procent_lejere,
    procent_normale = procent_normale,
    procent_aglomerate = procent_aglomerate,
    pasageri_abonament = pasageri_abonament,
    pasageri_platitori_bilet = pasageri_platitori_bilet,
    pasageri_neplatitori_bilet = pasageri_neplatitori_bilet,
    venituri_bilete = venituri_bilete,
    venituri_abonamente = venituri_abonamente,
    venituri_nerealizate = venituri_nerealizate
  ))
}

# Afisam rezultatele
print(rezultate)

# Venituri pe luna
ggplot(rezultate, aes(x = luna)) +
  geom_bar(aes(y = venituri_bilete, fill = "Venituri bilete"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = venituri_abonamente, fill = "Venituri abonamente"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = venituri_nerealizate, fill = "Venituri nerealizate"), stat = "identity", position = "dodge") +
  labs(title = "Venituri pe luna în 2024", y = "Venituri (lei)", fill = "Tip venituri") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#D)
set.seed(6)

# Datele problemei
zile_luna <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)  
medie <- 512  
min_calatori <- 210  
max_calatori <- 983 
procent_lejere <- 0.20  
procent_normale <- 0.50
procent_aglomerate <- 0.30 
procent_abonamente <- 0.38  
procent_platitori_bilet <- 0.74 
pret_bilet <- 3 
pret_abonament <- 70  
cost_controlor <- 214  # Costul pentru fiecare verificare a controlorului
amenda <- 50  # Valoarea amenzii

# Functie pentru generarea calatorilor pentru o luna
genereaza_calatori <- function(zile) {
  numar_zile_lejere <- round(zile * procent_lejere)
  numar_zile_normale <- round(zile * procent_normale)
  numar_zile_aglomerate <- zile - numar_zile_lejere - numar_zile_normale
  
  calatori_lejere <- runif(numar_zile_lejere, min = min_calatori, max = 350)
  calatori_normale <- runif(numar_zile_normale, min = 351, max = 670)
  calatori_aglomerate <- runif(numar_zile_aglomerate, min = 671, max = max_calatori)
  
  calatori_zi <- c(calatori_lejere, calatori_normale, calatori_aglomerate)
  calatori_zi <- sample(calatori_zi)
  
  return(calatori_zi)
}

# Functii pentru verificarea biletelor
verifica_bilete_lejere <- function(num_calatori) {
  num_verificati <- sample(2:11, 1)
  num_amendati <- 0
  for (i in 1:num_verificati) {
    if (runif(1) > procent_platitori_bilet) {
      num_amendati <- num_amendati + 1
      if (num_amendati >= 3) break
    }
  }
  return(num_amendati)
}

verifica_bilete_normale <- function(num_calatori) {
  num_verificati <- 0
  num_amendati <- 0
  while (num_amendati < 5 && num_verificati < num_calatori) {
    num_verificati <- num_verificati + 1
    if (runif(1) > procent_platitori_bilet) {
      num_amendati <- num_amendati + 1
    }
  }
  return(num_amendati)
}

verifica_bilete_aglomerate <- function(num_calatori) {
  num_verificati <- sample(3:5, 1)
  num_amendati <- 0
  for (i in 1:num_verificati) {
    if (runif(1) > procent_platitori_bilet) {
      num_amendati <- 1
      break
    }
  }
  return(num_amendati)
}

# Dataframe pentru centralizarea rezultatelor
rezultate <- data.frame(
  luna = character(),
  medie_calatori = numeric(),
  minim_calatori = numeric(),
  maxim_calatori = numeric(),
  procent_lejere = numeric(),
  procent_normale = numeric(),
  procent_aglomerate = numeric(),
  pasageri_abonament = numeric(),
  pasageri_platitori_bilet = numeric(),
  pasageri_neplatitori_bilet = numeric(),
  venituri_bilete = numeric(),
  venituri_abonamente = numeric(),
  venituri_nerealizate = numeric(),
  amendati_lejere = numeric(),
  amendati_normale = numeric(),
  amendati_aglomerate = numeric(),
  venituri_amenzi = numeric(),
  cost_controlori = numeric(),
  profit_pierdere = numeric(),
  stringsAsFactors = FALSE
)

# Generam datele pentru fiecare luna si calculam statisticile
for (i in 1:12) {
  calatori_zi <- genereaza_calatori(zile_luna[i])
  
  medie_calatori <- mean(calatori_zi)
  minim_calatori <- min(calatori_zi)
  maxim_calatori <- max(calatori_zi)
  
  procent_lejere <- sum(calatori_zi < 350) / zile_luna[i]
  procent_normale <- sum(calatori_zi >= 351 & calatori_zi <= 670) / zile_luna[i]
  procent_aglomerate <- sum(calatori_zi > 671) / zile_luna[i]
  
  pasageri_abonament <- sum(calatori_zi) * procent_abonamente
  pasageri_fara_abonament <- sum(calatori_zi) - pasageri_abonament
  pasageri_platitori_bilet <- pasageri_fara_abonament * procent_platitori_bilet
  pasageri_neplatitori_bilet <- pasageri_fara_abonament * (1 - procent_platitori_bilet)
  
  venituri_bilete <- pasageri_platitori_bilet * pret_bilet
  venituri_abonamente <- (pasageri_abonament / zile_luna[i]) * pret_abonament
  venituri_nerealizate <- pasageri_neplatitori_bilet * pret_bilet
  
  amendati_lejere <- sum(sapply(calatori_zi[calatori_zi < 350], verifica_bilete_lejere))
  amendati_normale <- sum(sapply(calatori_zi[calatori_zi >= 351 & calatori_zi <= 670], verifica_bilete_normale))
  amendati_aglomerate <- sum(sapply(calatori_zi[calatori_zi > 671], verifica_bilete_aglomerate))
  
  venituri_amenzi <- (amendati_lejere + amendati_normale + amendati_aglomerate) * amenda 
  cost_controlori <- zile_luna[i] * 2 * cost_controlor 
  profit_pierdere <- venituri_amenzi - cost_controlori - venituri_nerealizate
  
  rezultate <- rbind(rezultate, data.frame(
    luna = month.name[i],
    medie_calatori = medie_calatori,
    minim_calatori = minim_calatori,
    maxim_calatori = maxim_calatori,
    procent_lejere = procent_lejere,
    procent_normale = procent_normale,
    procent_aglomerate = procent_aglomerate,
    pasageri_abonament = pasageri_abonament,
    pasageri_platitori_bilet = pasageri_platitori_bilet,
    pasageri_neplatitori_bilet = pasageri_neplatitori_bilet,
    venituri_bilete = venituri_bilete,
    venituri_abonamente = venituri_abonamente,
    venituri_nerealizate = venituri_nerealizate,
    amendati_lejere = amendati_lejere,
    amendati_normale = amendati_normale,
    amendati_aglomerate = amendati_aglomerate,
    venituri_amenzi = venituri_amenzi,
    cost_controlori = cost_controlori,
    profit_pierdere = profit_pierdere
  ))
}

# Afisam rezultatele
print(rezultate)

# Venituri pe luna
ggplot(rezultate, aes(x = luna)) +
  geom_bar(aes(y = venituri_bilete, fill = "Venituri bilete"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = venituri_abonamente, fill = "Venituri abonamente"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = venituri_amenzi, fill = "Venituri amenzi"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = -venituri_nerealizate, fill = "Venituri nerealizate"), stat = "identity", position = "dodge") +
  labs(title = "Venituri pe luna în 2024", y = "Venituri", fill = "Tip venit") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Profit/Pierdere pe luna
ggplot(rezultate, aes(x = luna, y = profit_pierdere, fill = profit_pierdere > 0)) +
  geom_bar(stat = "identity") +
  labs(title = "Profit/Pierdere pe luna în 2024", y = "Profit/Pierdere", fill = "Profit") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#E)
# Functie pentru verificarea biletelor cu un al treilea control
verifica_bilete_suplimentar <- function(num_calatori) {
  num_verificati <- 0
  num_amendati <- 0
  while (num_amendati < 5 && num_verificati < num_calatori) {
    num_verificati <- num_verificati + 1
    if (runif(1) > procent_platitori_bilet) {
      num_amendati <- num_amendati + 1
    }
  }
  return(num_amendati)
}

# Dataframe pentru rezultatele cu al treilea control
rezultate_suplimentar <- data.frame(
  luna = character(),
  venituri_amenzi_suplimentar = numeric(),
  cost_controlori_suplimentar = numeric(),
  venituri_nerealizate_suplimentar = numeric(),
  profit_pierdere_suplimentar = numeric(),
  stringsAsFactors = FALSE
)

# Simulam efectele introducerii unui al treilea control pe zi
for (i in 1:12) {
  calatori_zi <- genereaza_calatori(zile_luna[i])
  
  amendati_lejere <- sum(sapply(calatori_zi[calatori_zi < 350], verifica_bilete_lejere))
  amendati_normale <- sum(sapply(calatori_zi[calatori_zi >= 351 & calatori_zi <= 670], verifica_bilete_normale))
  amendati_aglomerate <- sum(sapply(calatori_zi[calatori_zi > 671], verifica_bilete_aglomerate))
  
  venituri_amenzi_suplimentar <- sum(sapply(calatori_zi, verifica_bilete_suplimentar)) * amenda * 0.7  # 70% din amenzi sunt raportate oficial
  
  cost_controlori_suplimentar <- zile_luna[i] * 3 * cost_controlor  # Costul pentru trei controale pe zi
  
  venituri_nerealizate_suplimentar <- sum(calatori_zi) * (1 - procent_platitori_bilet) * pret_bilet  # Pierderile din neplata biletelor
  
  profit_pierdere_suplimentar <- venituri_amenzi_suplimentar - cost_controlori_suplimentar - venituri_nerealizate_suplimentar
  
  rezultate_suplimentar <- rbind(rezultate_suplimentar, data.frame(
    luna = month.name[i],
    venituri_amenzi_suplimentar = venituri_amenzi_suplimentar,
    cost_controlori_suplimentar = cost_controlori_suplimentar,
    venituri_nerealizate_suplimentar = venituri_nerealizate_suplimentar,
    profit_pierdere_suplimentar = profit_pierdere_suplimentar
  ))
}

# Afisam rezultatele
print(rezultate_suplimentar)

# Vizualizam rezultatele
# Profit/Pierdere pe luna cu al treilea control
ggplot(rezultate_suplimentar, aes(x = luna, y = profit_pierdere_suplimentar, fill = profit_pierdere_suplimentar > 0)) +
  geom_bar(stat = "identity") +
  labs(title = "Profit/Pierdere pe luna cu al treilea control în 2024", y = "Profit/Pierdere", fill = "Profit") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
