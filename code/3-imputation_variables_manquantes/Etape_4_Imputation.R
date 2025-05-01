
# Initialisation ----------------------------------------------------------

rm(list=ls())

data <- readRDS("Etape_3_clem.Rdata", "rb")

# install.packages("Rcpp")
install.packages("Amelia")
# install.packages("psych")
# install.packages("VIM")
# install.packages("missForest")
library(missForest)
library(VIM)
library(psych)
library(Amelia)
library(dplyr)
str(data)

# Conception des fonctions nécessaires au programme -----------------------

# Fonction pour effectuer le test de Little MCAR
do_little_mcar_test <- function(data) {
  # 1. Création de la matrice des indicateurs de données manquantes
  missing_matrix <- is.na(data)  # TRUE si manquant, FALSE si observé
  
  # 2. Calcul des covariances entre les variables (en utilisant la matrice des indicateurs)
  cov_matrix <- cov(missing_matrix)
  
  # 3. Calcul de la statistique du chi-deux
  n <- nrow(data)  # Nombre d'observations
  k <- ncol(data)  # Nombre de variables
  
  # Calcul de la statistique chi-deux
  chi_square_stat <- sum(cov_matrix^2) * n
  
  # 4. Degrés de liberté (df) - pour un test de Little, c'est généralement (k-1) * (k-1)
  
  # Point important ici il est possible que ce ne soit pas le bon nombre de degrés de liberté 
  # car en réalité le test de little fait le test en fonction du type 
  # de motifs de données manquantes ici il semble qu'il n'y en ait pas 
  # donc il y a un unique groupe p = 1
  
  # Voir avec Valentin sur la méthodologie à suivre
  
  df <- k
  
  # 5. Calcul de la p-valeur à partir de la statistique du chi-deux
  p_value <- 1 - pchisq(chi_square_stat, df)
  
  # Retourner les résultats
  result <- list(
    chi_square_statistic = chi_square_stat,
    degrees_of_freedom = df,
    p_value = p_value
  )
  
  return(result)
}

# Fonction qui renvoie les noms des variables où il y a au moins une donnée manquante
do_variables_avec_NA <- function(df) {
  # Vérifier les colonnes avec des valeurs manquantes
  missing_columns <- sapply(df, function(column) any(is.na(column)))
  
  # Retourner les noms des colonnes avec des valeurs manquantes
  names(df)[missing_columns]
}


# Visualisation -----------------------------------------------------------

missmap(data)

# Environ X% de données manquantes sans paterns (je sais pas car missmap marche pas sur mon cluster ...)

variables_avec_NA <- do_variables_avec_NA(data)
print(variables_avec_NA)

length(variables_avec_NA)

# 8 variables concernées

# Effectuer le test de Little afin de valider l'hypothèse MCAR

resultat_test_little <- do_little_mcar_test(data)
print(resultat_test_little)

# p_value bien supérieure à 0.05 donc Données manquantes MCAR !!!


# Imputation --------------------------------------------------------------

###### Imputation avec random forest une méthode aléatoire ######

# (aucune notion de matching normalement)
summary(data)

# Récuperation que du categ et num car ne prend pas les chr
data_factor_numeric_forest <- data[sapply(data, function(x) is.factor(x) | is.numeric(x))]
missForest <- missForest(data_factor_numeric_forest, ntree = 1000, decreasing = TRUE) 
# Long à la computation !!!


# Exportation et vérification
data_impute_rd_forest <- missForest$ximp

# Sélectionner la colonne 'code_elevage' de 'data'
code_elevage <- data$CODE_ELEVAGE

# Ajouter la colonne 'code_elevage' à 'data_impute_rd_forest' en première position
data_impute_rd_forest <- cbind(code_elevage, data_impute_rd_forest)

# Réexportation de la base imputée

length(do_variables_avec_NA(data_impute_rd_forest)) # Petite vérif de l'exportation

saveRDS(data_impute_rd_forest, "data_post_etape_4_Forest.Rdata")

###### Imputation par la moyenne (pour numeric) et par le mode (pour catégorielle) ######

# Fonction pour calculer le mode
calculer_mode <- function(x) {
  ux <- unique(x[!is.na(x)])  # Ignorer les NA
  ux[which.max(tabulate(match(x, ux)))]  # Retourner la valeur la plus fréquente
}

# Fonction principale d'imputation
imputer_valeurs <- function(data) {
  # Parcourir chaque colonne du data frame
  for (col in names(data)) {
    if (is.numeric(data[[col]])) {
      # Imputation par la moyenne pour les variables numériques
      moyenne <- mean(data[[col]], na.rm = TRUE)
      data[[col]][is.na(data[[col]])] <- moyenne
    } else if (is.factor(data[[col]])) {
      # Imputation basée sur les fréquences pour les variables de type factor
      freq <- table(data[[col]], useNA = "no")  # Calculer les fréquences des niveaux présents
      prob <- freq / sum(freq)  # Calculer les probabilités relatives
      niveaux <- names(freq)    # Extraire les niveaux correspondants
      
      # Remplacer les NA par des échantillons aléatoires basés sur les probabilités
      data[[col]][is.na(data[[col]])] <- sample(niveaux, 
                                                sum(is.na(data[[col]])), 
                                                replace = TRUE, 
                                                prob = prob)
      
      # S'assurer que la colonne reste un facteur avec les mêmes niveaux
      data[[col]] <- factor(data[[col]], levels = niveaux)
    }
  }
  return(data)
}

data_impute_mean_freq <- imputer_valeurs(data)

saveRDS(data_impute_mean_freq, "data_post_etape_4_Mean.Rdata")

###### Différences entre les deux méthodes ? ######

# Le nombre de valeurs manquantes initialement (et donc de valeurs imputées):
sum(is.na(data)) # 59

# Le nombre de valeurs imputées différemment selon les deux méthodes :
sum(data_impute_mean_freq != data_impute_rd_forest) # 34 dont 15 numériques

# Donc 19 valeurs sur 44 sont différentes ~ la moitié (2/5 pour Y)

# Les valeurs différentes
data_impute_mean_freq[data_impute_mean_freq != data_impute_rd_forest]
data_impute_rd_forest[data_impute_mean_freq != data_impute_rd_forest]

# Pour y c'est impératif d'en discuter avec la tutrice. Pour le reste euuuh aussi je pense !

##### test #####
