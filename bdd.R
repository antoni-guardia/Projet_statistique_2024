rm(list = ls() )
library(dplyr)
library(corrplot)

# Charger le fichier .rds
data <- readRDS("nouvelles_donnees.Rdata",  "rb")
nom_colonnes <- colnames(data)

# On passe la variable Y (1 = ENG malade, 2 = PS malade, 3 = sain) en facteur :

data$y <- as.factor(data$y)
summary(data$y)


# Etude univariée du lien entre Y et les variables X ----------------------

# Fonction qui prend en argument le seuil de la p-value de selection
# Prend Y la variable explicative présente dans le jeu de donnée en format chr
# Prend data le jeu de donnée avec l'ensemble des X explicatives et du Y à expliquer

# la fct réalise : 
# si la variable X rencontrée dans le jeu de donnée est numeric réalise un test de comparaison de moyennes entre X et Y
# si la varibale X rencontrée dans le jeu de donnée est factor réalise un test du khi deux 

# renvoie la liste des variables dont la p-value est inférieur au seuil

etude_univariee <- function(seuil_pvalue, Y, data) {
  # Vérifier que Y est dans les colonnes de data
  if (!(Y %in% colnames(data))) {
    stop("La variable Y n'est pas présente dans le jeu de données.")
  }
  
  # Initialiser une liste pour stocker les variables sélectionnées
  variables_selectionnees <- list()
  
  # Parcourir toutes les variables du jeu de données sauf Y
  for (var in colnames(data)) {
    if (var == Y) next # Sauter Y
    
    if (is.numeric(data[[var]])) {
      # Test de comparaison de moyennes (ANOVA)
      p_value <- tryCatch({
        summary(aov(data[[var]] ~ data[[Y]]))[[1]]$`Pr(>F)`[1]
      }, error = function(e) NA)
      
    } else if (is.factor(data[[var]])) {
      # Test du Khi-carré
      p_value <- tryCatch({
        chisq.test(table(data[[var]], data[[Y]]))$p.value
      }, error = function(e) NA)
      
    } else {
      # Si la variable n'est ni numérique ni facteur, on l'ignore
      next
    }
    
    # Vérifier la p-value et ajouter à la liste si inférieure au seuil
    if (!is.na(p_value) && p_value < seuil_pvalue) {
      variables_selectionnees[[var]] <- p_value
    }
  }
  
  # Retourner les variables sélectionnées
  return(variables_selectionnees)
}


# Exemple de jeu de données
#data <- data.frame(
#  Y = factor(c("A", "A", "B", "B", "C", "C")),
#  X1 = c(5.1, 6.2, 7.3, 4.8, 6.1, 5.5),
#  X2 = factor(c("Oui", "Non", "Oui", "Non", "Oui", "Non")),
#  X3 = c("Text1", "Text2", "Text3", "Text4", "Text5", "Text6"),
#  X4 = c(1,1,2,2,3,3),
#  X5 = factor(c("Abien", "Abien", "Bien", "Bien", "Nul", "Nul"))
#)

# Ci-dessous la liste des colonnes à supprimer suite à l'analyse passée vis-à-vis des 85% et 15%

names_suppr <- c(
  "T16_BS_Pres.EssuiMain", "T21_Elev_ContPo",
  "T21_Elev_Cot", "X04_AUTRESP",
  "X13x2_LAVMain_Cad", "X13x2_BOTTSpecEqua",
  "LIT_PS", "A03_Pos10sTs",
  "A03_sd10sSt", "A03_sd22sSt",
  "A03_My10sAs", "A03_My22sAs",
  "A03_Md10sAs", "A03_Md22sAs",
  "A03_sd10sAs", "A03_sd22sAs",
  "A03_TxPos10sTs", "A03_My10sTs",
  "A03_Md10sTs", "A03_sd10sTs",
  "A03_sd22sTs", "A03_sdSero22sTgPP",
  "A08_Classe_1TO10_10s", "A01_TxPos22sHAPTO",
  "T11_PS_NoteIndPoQue_0", "T14_ENG_NoteIndPoBoit_2",
  "T14_ENG_NoteIndPoQue_0", "T14_ENG_NoteIndPoQue_BIN",
  "T11_PS_NoteIndPoDiarr_2", "T11_PS_NoteIndPoDys",
  "T11_PS_NoteIndPoGroinD", "T14_ENG_NoteIndPoDiarr_0",
  "T14_ENG_NoteIndPoDiarr_1", "T14_ENG_NoteIndPoDiarr_2",
  "T14_ENG_NoteIndPoDiarr_BIN", "T14_ENG_NoteIndPoDys",
  "T14_ENG_NoteIndPoGroinD", "T14_ENG_NoteIndPoAnémi",
  "T10_PS_EauNbPopPo_1", "T10_PS_EauNbPopPo_3",
  "T13_ENG_EauNbPo_1", "Label",
  "A06_TxPos22sSDRPreel", "A01_TxPos10sHAPTO",
  "A01_TxPos22sHAPTO", "A01_TxDtx22sHAPTO",
  "T11_PS_NoteIndPoBles_2", "T14_ENG_NoteIndPoBoit_2",
  "T14_ENG_NoteIndPoBoit_2", "T14_ENG_NoteIndPoHOmb"
)


colonnes_a_conserver <- setdiff(names(data), names_suppr)

# Mise à jour du dataframe
data <- data[, colonnes_a_conserver]

# On représente les anciennes variables à expliquer en fonction de Y :
# Nous aurions pu pousser l'analyse pour étudier le lien entre les anciennes
# variables à expliquer et Y mais il en existe un nécessairement par construction.

par(mfrow = c(2, 2))

plot(data[,2]~data$y, xlab = "Y", ylab = "Toux en post-sevrage", main = "Toux en post-sevrage / Y")
plot(data[,3]~data$y, xlab = "Y", ylab = "Eternuements en post-sevrage", main = "Eternuements en post-sevrage / Y")
plot(data[,4]~data$y, xlab = "Y", ylab = "Toux en engraissement", main = "Toux en engraissement / Y")
plot(data[,5]~data$y, xlab = "Y", ylab = "Eternuements en engraissement", main = "Eternuements en engraissement / Y")

# On supprime aussi les 4 anciennes variables à expliquer car elles sont
# désormais dépassées et il faudrait les enlever à chaque analyse car elles
# empêcheraient une analyse statistique pertinente

data <- data[,-c(2,3,4,5)]

# Appel de la fonction
# (On fera varier p afin d'obtenir 100 variables explicatives potentiellement cohérentes)
resultats <- etude_univariee(seuil_pvalue = 0.254, Y = colnames(data)[511], data = data)

# Afficher les résultats
print(resultats)
length(resultats)

# On établit alors un nouveau data_frame qui contient la variables y et les 100
# variables à expliquer les plus pertinentes (on rajoute CODE_ELEVAGE ?)

data <- cbind(data[colnames(data) %in% names(resultats)], data$y)

# Parmi les variables qui représentent le même caractère mais codées
# différemment, il va falloir supprimer celle(s) qui a(ont) la p-value la plus grande

data <- data %>% select(-c())
