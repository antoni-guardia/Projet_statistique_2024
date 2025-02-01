rm(list=ls())



# On récupère les données

donnees <- readRDS("base_PC_Var_X_Var_Y_ENSAI_Respi_FINALE.RData", "rb")
summary(donnees[,c(2,3,4,5)])
summary(donnees$y)


# On transforme les variables à expliquer en variables catégorielles afin d'effectuer
# un test du chi-deux avec les variables explicatives

donnees$PS_Tx_freq <- ifelse(donnees$PS_Tx_freq == 0,0,
                             ifelse(donnees$PS_Tx_freq > 0 & donnees$PS_Tx_freq < 1.25, 0.5,
                                    ifelse(donnees$PS_Tx_freq > 1.25 & donnees$PS_Tx_freq < 4, 3,
                                           ifelse(donnees$PS_Tx_freq > 4 & donnees$PS_Tx_freq < 12, 8, 20))))

donnees$PS_Eter_freq <- ifelse(donnees$PS_Eter_freq == 0,0,
                            ifelse(donnees$PS_Eter_freq > 0 & donnees$PS_Eter_freq < 2, 1,
                                   ifelse(donnees$PS_Eter_freq > 2 & donnees$PS_Eter_freq < 4, 3,
                                          ifelse(donnees$PS_Eter_freq > 4 & donnees$PS_Eter_freq < 8, 6,
                                                 ifelse(donnees$PS_Eter_freq > 8 & donnees$PS_Eter_freq < 15, 11.5, 30)))))

donnees$ENG_Tx_freq <- ifelse(donnees$ENG_Tx_freq == 0,0,
                             ifelse(donnees$ENG_Tx_freq > 0 & donnees$ENG_Tx_freq < 1.5, 0.75,
                                    ifelse(donnees$ENG_Tx_freq > 1.5 & donnees$ENG_Tx_freq < 5, 3,
                                           ifelse(donnees$ENG_Tx_freq > 5 & donnees$ENG_Tx_freq < 13, 9, 25))))

donnees$ENG_Eter_freq <- ifelse(donnees$ENG_Eter_freq == 0,0,
                               ifelse(donnees$ENG_Eter_freq > 0 & donnees$ENG_Eter_freq < 1, 0.5,
                                      ifelse(donnees$ENG_Eter_freq > 1 & donnees$ENG_Eter_freq < 2, 1.5,
                                             ifelse(donnees$ENG_Eter_freq > 2 & donnees$ENG_Eter_freq < 5, 3.5, 10))))

donnees$PS_Tx_freq <- as.factor(donnees$PS_Tx_freq)
donnees$PS_Eter_freq <- as.factor(donnees$PS_Eter_freq)
donnees$ENG_Tx_freq <- as.factor(donnees$ENG_Tx_freq)
donnees$ENG_Eter_freq <- as.factor(donnees$ENG_Eter_freq)

table(donnees$PS_Tx_freq)
table(donnees$PS_Eter_freq)
table(donnees$ENG_Tx_freq)
table(donnees$ENG_Eter_freq)



# On effectue les tests du chi-deux pour les 4 variables à expliquer avec toutes les variables

chi_PS_Tx <- c()
for (i in 1:560){
  if (i != 549){
    chi_PS_Tx[i] <- chisq.test(donnees[,2], donnees[,i])$p.value
  }
}

chi_PS_Eter <- c()
for (i in 1:560){
  if (i != 549){
    chi_PS_Eter[i] <- chisq.test(donnees[,3], donnees[,i])$p.value
  }
}

chi_ENG_Tx <- c()
for (i in 1:560){
  if (i != 549){
    chi_ENG_Tx[i] <- chisq.test(donnees[,4], donnees[,i])$p.value
  }
}

chi_ENG_Eter <- c()
for (i in 1:560){
  if (i != 549){
    chi_ENG_Eter[i] <- chisq.test(donnees[,5], donnees[,i])$p.value
  }
}



# On conserve les variables où la p-value est inférieure à 0.05

chi_PS_Tx_05 <- chi_PS_Tx[chi_PS_Tx < 0.05]
indices_chi_PS_Tx_05 <- which(chi_PS_Tx < 0.05)

chi_PS_Eter_05 <- chi_PS_Eter[chi_PS_Eter < 0.05]
indices_chi_PS_Eter_05 <- which(chi_PS_Eter < 0.05)

chi_ENG_Tx_05 <- chi_ENG_Tx[chi_ENG_Tx < 0.05]
indices_chi_ENG_Tx_05 <- which(chi_ENG_Tx < 0.05)

chi_ENG_Eter_05 <- chi_ENG_Eter[chi_ENG_Eter < 0.05]
indices_chi_ENG_Eter_05 <- which(chi_ENG_Eter < 0.05)



# On observe lesquelles apparaissent plusieurs fois

all_values <- unlist(list(indices_chi_PS_Tx_05,
                          indices_chi_PS_Eter_05,
                          indices_chi_ENG_Tx_05,
                          indices_chi_ENG_Eter_05))
value_counts <- table(all_values)

# Obtenir les valeurs apparaissant au moins 2 fois
values_at_least_2 <- as.numeric(names(value_counts[value_counts >= 2]))

# Obtenir les valeurs apparaissant au moins 3 fois
values_at_least_3 <- as.numeric(names(value_counts[value_counts >= 3]))

# Obtenir les valeurs apparaissant au moins 4 fois
values_at_least_4 <- as.numeric(names(value_counts[value_counts >= 4]))



# On affiche les résultats et les graphiques associés

print(values_at_least_2)
print(values_at_least_3)
print(values_at_least_4)
colnames(donnees)[values_at_least_2]
colnames(donnees)[values_at_least_3]
colnames(donnees)[values_at_least_4]

par(mfrow = c(2, 2))

plot(donnees[,2]~donnees[,179])
plot(donnees[,3]~donnees[,179])
plot(donnees[,4]~donnees[,179])
plot(donnees[,5]~donnees[,179])



# On conserve les variables où la p-value est inférieure à 0.1

chi_PS_Tx_1 <- chi_PS_Tx[chi_PS_Tx < 0.1]
indices_chi_PS_Tx_1 <- which(chi_PS_Tx < 0.1)

chi_PS_Eter_1 <- chi_PS_Eter[chi_PS_Eter < 0.1]
indices_chi_PS_Eter_1 <- which(chi_PS_Eter < 0.1)

chi_ENG_Tx_1 <- chi_ENG_Tx[chi_ENG_Tx < 0.1]
indices_chi_ENG_Tx_1 <- which(chi_ENG_Tx < 0.1)

chi_ENG_Eter_1 <- chi_ENG_Eter[chi_ENG_Eter < 0.1]
indices_chi_ENG_Eter_1 <- which(chi_ENG_Eter < 0.1)



# On observe lesquelles apparaissent plusieurs fois

all_values_1 <- unlist(list(indices_chi_PS_Tx_1,
                          indices_chi_PS_Eter_1,
                          indices_chi_ENG_Tx_1,
                          indices_chi_ENG_Eter_1))
value_counts_1 <- table(all_values_1)

# Obtenir les valeurs apparaissant au moins 2 fois
values_at_least_2_1 <- as.numeric(names(value_counts_1[value_counts_1 >= 2]))

# Obtenir les valeurs apparaissant au moins 3 fois
values_at_least_3_1 <- as.numeric(names(value_counts_1[value_counts_1 >= 3]))

# Obtenir les valeurs apparaissant au moins 4 fois
values_at_least_4_1 <- as.numeric(names(value_counts_1[value_counts_1 >= 4]))



# On affiche les résultats et les graphiques associés

print(values_at_least_2_1)
print(values_at_least_3_1)
print(values_at_least_4_1)
colnames(donnees)[values_at_least_2_1]
colnames(donnees)[values_at_least_3_1]
colnames(donnees)[values_at_least_4_1]

par(mfrow = c(2, 2))

plot(donnees[,2]~donnees[,48])
plot(donnees[,3]~donnees[,48])
plot(donnees[,4]~donnees[,48])
plot(donnees[,5]~donnees[,48])

par(mfrow = c(2, 2))

plot(donnees[,2]~donnees[,179])
plot(donnees[,3]~donnees[,179])
plot(donnees[,4]~donnees[,179])
plot(donnees[,5]~donnees[,179])

par(mfrow = c(2, 2))

plot(donnees[,2]~donnees[,208])
plot(donnees[,3]~donnees[,208])
plot(donnees[,4]~donnees[,208])
plot(donnees[,5]~donnees[,208])

par(mfrow = c(2, 2))

plot(donnees[,2]~donnees[,234])
plot(donnees[,3]~donnees[,234])
plot(donnees[,4]~donnees[,234])
plot(donnees[,5]~donnees[,234])

par(mfrow = c(2, 2))

boxplot(donnees[,2]~donnees[,502])
boxplot(donnees[,3]~donnees[,502])
boxplot(donnees[,4]~donnees[,502])
boxplot(donnees[,5]~donnees[,502])



# On conserve les variables où la p-value est inférieure à 0.15

chi_PS_Tx_15 <- chi_PS_Tx[chi_PS_Tx < 0.15]
indices_chi_PS_Tx_15 <- which(chi_PS_Tx < 0.15)

chi_PS_Eter_15 <- chi_PS_Eter[chi_PS_Eter < 0.15]
indices_chi_PS_Eter_15 <- which(chi_PS_Eter < 0.15)

chi_ENG_Tx_15 <- chi_ENG_Tx[chi_ENG_Tx < 0.15]
indices_chi_ENG_Tx_15 <- which(chi_ENG_Tx < 0.15)

chi_ENG_Eter_15 <- chi_ENG_Eter[chi_ENG_Eter < 0.15]
indices_chi_ENG_Eter_15 <- which(chi_ENG_Eter < 0.15)



# On observe lesquelles apparaissent plusieurs fois

all_values_15 <- unlist(list(indices_chi_PS_Tx_15,
                            indices_chi_PS_Eter_15,
                            indices_chi_ENG_Tx_15,
                            indices_chi_ENG_Eter_15))
value_counts_15 <- table(all_values_15)

# Obtenir les valeurs apparaissant au moins 2 fois
values_at_least_2_15 <- as.numeric(names(value_counts_15[value_counts_15 >= 2]))

# Obtenir les valeurs apparaissant au moins 3 fois
values_at_least_3_15 <- as.numeric(names(value_counts_15[value_counts_15 >= 3]))

# Obtenir les valeurs apparaissant au moins 4 fois
values_at_least_4_15 <- as.numeric(names(value_counts_15[value_counts_15 >= 4]))



# On affiche les résultats et les graphiques associés

print(values_at_least_2_15)
print(values_at_least_3_15)
print(values_at_least_4_15)
colnames(donnees)[values_at_least_2_15]
colnames(donnees)[values_at_least_3_15]
colnames(donnees)[values_at_least_4_15]

