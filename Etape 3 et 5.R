rm(list = ls() )
library(dplyr)
library(corrplot)

# Charger le fichier .rds
data <- readRDS("nouvelles_donnees",  "rb")

nom_colonnes <- colnames(data)


# Etude univariée du lien entre Y et les variables X ----------------------

# Fonction qui prend en argument le seuil de la p-value de selection
# Prend Y la variable explicative présente dans le jeu de donnée en format chr
# Prend data le jeu de donnée avec l'ensemble des X explicatives et du Y à expliquer

# la fct réalise : 
# si la variable X rencontrée dans le jeu de donnée est numeric réalise un test de comparaison de moyennes entre X et Y
# si la varibale X rencontrée dans le jeu de donnée est factor réalise un test du khi deux 
#     Si il y a moins de 5 individus dansle croisement alors réalise un test du khi-2 exact 

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
      # Vérifier les effectifs pour le test du khi-deux
      contingency_table <- table(data[[var]], data[[Y]])
      
      if (any(contingency_table < 5)) {
        # Si des effectifs < 5, faire un test exact du khi-deux
        p_value <- tryCatch({
          fisher.test(contingency_table)$p.value
        }, error = function(e) NA)
      } else {
        # Sinon, utiliser le test du khi-deux classique
        p_value <- tryCatch({
          chisq.test(contingency_table)$p.value
        }, error = function(e) NA)
      }
      
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
data1 <- data.frame(
  Y = factor(c("A", "A", "B", "B", "C", "C")),
  X1 = c(5.1, 6.2, 7.3, 4.8, 6.1, 5.5),
  X2 = factor(c("Oui", "Non", "Oui", "Non", "Oui", "Non")),
  X3 = c("Text1", "Text2", "Text3", "Text4", "Text5", "Text6"),
  X4 = c(1,1,2,2,3,3),
  X5 = factor(c("Abien", "Abien", "Bien", "Bien", "Nul", "Nul"))
)

# Appel de la fonction
resultats <- etude_univariee(seuil_pvalue = 0.20, Y = "Y", data = data1)

# Afficher les résultats
print(resultats)

