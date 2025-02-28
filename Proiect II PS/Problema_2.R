#a)
esantion_a <- c(8, 12, 6, 14, 9, 12, 15, 7, 10, 10, 14, 9, 12, 15, 11, 8, 6, 
              9, 13, 12, 10, 8, 11, 13, 15, 13, 15, 10, 8, 7, 13, 8, 9, 11, 
              14, 7, 12, 16, 15, 10, 9, 10, 12, 8, 13, 9, 11)

#Estimatorul metodei verosimilitatii maxime
teta_ver_a <- mean(esantion_a)/2

# Estimatorul metodei momentelor
teta_mom_a <- mean(esantion_a)/2

print(teta_mom_a)
print(teta_ver_a)

#b)
esantion_b <- c(3, 2, 1, 4, 2, 3, 4, 1, 3, 2, 2, 4, 2, 1, 7, 5, 4, 5, 5, 2, 
              3, 4, 3, 1, 2, 4, 1, 1, 2, 3, 1, 3, 1, 4, 1, 3, 1, 6, 1, 3, 
              3, 4, 3, 1, 3, 2, 2, 3, 2, 4, 1, 1, 2, 6, 3, 1, 3, 6, 1, 2, 
              3, 6, 3, 2, 2, 2, 4, 2, 1, 3, 3, 4, 2, 3, 4, 1, 4, 4, 6, 3, 
              3, 5, 2, 2, 2, 3, 1, 3, 1, 3, 3, 5, 3, 4, 3, 2, 4, 2, 3, 3)

#Estimatorul metodei verosimilitatii maxime
teta_ver_b <- mean(esantion_b)

print(teta_ver_b)

#d)
esantion_d <- c(6, 3, 24, 24, 4, 56, 10, 13, 2, 28, 24, 2, 22, 11, 2, 8, 118,
                2, 14, 19, 7, 9, 8, 189, 2, 9, 21, 6, 6, 2, 3, 2, 3, 18, 3, 2, 
                21, 1, 5, 9, 11, 13, 19, 76, 1, 5, 9, 4, 57, 1, 2, 16, 5, 2, 20, 
                8, 1, 40, 6, 4, 19, 6, 3, 2, 4, 9, 1, 5, 10, 12, 6, 525, 19, 6, 
                17, 2, 5, 159, 5, 62, 6, 3, 45, 21, 23, 3, 17, 2, 1, 1, 474, 15, 
                3, 3, 7, 7, 13, 4, 38, 4)

#Estimatorul metodei verosimilitatii maxime
teta_ver_d <- mean(esantion_d)

# Estimatorul metodei momentelor
teta_mom_d <- mean(esantion_d)

print(teta_mom_d)
print(teta_ver_d)

#e)
esantion_e <- c(3.5930579, 2.1027540, 1.7820777, 9.6550388, 6.8803846, 0.7388358, 2.9194654, 3.1178660, 
                1.2323236, 2.9776820, 1.1172078, 2.4184586, 3.3258971, 1.9498871, 2.6088612, 3.9535062, 
                3.0389107, 4.4226628, 3.9366318, 2.4551569, 5.2814487, 5.6778622, 4.7683935, 1.1581498, 
                3.1270783, 4.1473311, 7.4830426, 1.1342893, 1.7773392, 7.7510826, 1.3919927, 2.3613291, 
                2.6234826, 1.6562602, 1.4992235, 2.3455062, 3.8458809, 5.8333841, 3.3834034, 1.5202546, 
                3.1248186, 5.3029567, 3.6225571, 4.8309931, 3.1579595, 3.2640258, 3.9538891, 4.0796841, 
                4.0991772, 3.2779944, 2.5002127, 3.0654695, 1.6996010, 3.2175175, 1.9033087, 4.4052061, 
                2.3158379, 2.4778345, 5.4382190, 4.9141207, 6.0978745, 1.1428936, 3.5639106, 7.4541937, 
                7.7778289, 3.2859563, 0.7432908, 1.4442696, 3.6619932, 2.8361371, 4.3180773, 1.6763585, 
                4.4464154, 2.5049617, 0.4448735, 5.0518839, 3.4151834, 1.6823650, 5.4517583, 2.8212788, 
                2.1566837, 2.9893287, 1.6925123, 6.5197938, 4.2165408, 1.6728425, 2.7650830, 2.6742755, 
                2.9622047, 0.7809781, 1.3913415, 5.3430751, 2.4859925, 3.7329465, 6.3129236, 0.6635228, 
                3.7640343, 2.1850174, 4.3773328, 5.0931544)

#Estimatorul metodei verosimilitatii maxime
teta_ver_e <- mean(esantion_e)

# Estimatorul metodei momentelor
teta_mom_e <- (gamma(4)/mean(esantion_e))^(1/4)

print(teta_mom_e)
print(teta_ver_e)