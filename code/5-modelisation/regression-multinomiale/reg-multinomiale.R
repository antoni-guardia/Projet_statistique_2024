
rm(list=ls())
library(VGAM)

# Récupération des données
donnees_mean <- readRDS("data_post_etape_4_Mean.Rdata", "rb")
donnees_mean2 <- donnees_mean[,-1]

donnees_mean2$y <- relevel(donnees_mean2$y, ref = "ENG_malade")

levels(donnees_mean2$y)

# Fonction qui permet de stepwise (en backward)
# Ses critères d'arrêts sont :
# - l'AIC est plus grand lorsque je supprime du modèle la variable dont la p-value est la plus forte dans le test anova
# - Toutes les variables ont une p-value < alpha dans le test anova
# Elle fait des warnings lorsque certains coefficients changent trop
stepwise_multinomial_vglm <- function(data, response_var, alpha = 0.05, coef_change_threshold = 0.25) {
  require(VGAM)
  
  # Vérifications initiales
  if (!response_var %in% colnames(data)) {
    stop("La variable réponse ", response_var, " n'existe pas dans le dataframe")
  }
  
  if (!is.factor(data[[response_var]])) {
    data[[response_var]] <- as.factor(data[[response_var]])
    message("La variable réponse ", response_var, " a été convertie en facteur")
  }
  
  # Préparation des termes initiaux
  predictors <- setdiff(colnames(data), response_var)
  formula_current <- as.formula(paste(response_var, "~", paste(predictors, collapse = " + ")))
  
  # Modèle initial
  model_current <- vglm(formula_current, data = data, family = multinomial)
  cat("Modèle initial ajusté avec", length(predictors), "prédicteurs\n")
  
  # Boucle de sélection
  repeat {
    # Analyse des termes actuels
    current_terms <- attr(terms(model_current), "term.labels")
    if (length(current_terms) == 0) break
    
    # Test ANOVA
    anova_results <- tryCatch(
      anova(model_current),
      error = function(e) {
        cat("Erreur dans ANOVA : ", e$message, "\n")
        return(NULL)
      }
    )
    
    if (is.null(anova_results)) break
    
    # Identification de la variable la moins significative
    p_values <- anova_results[, "Pr(>Chi)"]
    if (all(is.na(p_values))) break
    
    max_p_var <- current_terms[which.max(p_values)]
    max_p_value <- max(p_values, na.rm = TRUE)
    
    if (max_p_value <= alpha) {
      cat("Toutes les variables restantes sont significatives (p <", alpha, ")\n")
      break
    }
    
    # Construction du modèle réduit
    new_terms <- setdiff(current_terms, max_p_var)
    if (length(new_terms) == 0) {
      formula_reduced <- as.formula(paste(response_var, "~ 1"))
    } else {
      formula_reduced <- as.formula(paste(response_var, "~", paste(new_terms, collapse = " + ")))
    }
    
    model_reduced <- tryCatch(
      vglm(formula_reduced, data = data, family = multinomial),
      error = function(e) {
        cat("Erreur dans l'ajustement du modèle réduit : ", e$message, "\n")
        return(NULL)
      }
    )
    
    if (is.null(model_reduced)) break
    
    # Test de déviance
    dev_test <- (AIC(model_reduced) - AIC(model_current))
    p_dev <- dev_test<0
    
    # Vérification des coefficients
    coef_comparison <- compare_coefficients(model_current, model_reduced, 
                                            alpha = alpha, 
                                            threshold = coef_change_threshold,
                                            max_p_var = max_p_var)
    
    # Décision
    if (dev_test>0 && max_p_value < alpha) {
      cat("Conservation de ", max_p_var, p_dev, " (la suppression détériore significativement le modèle)\n")
      break
    } else {
      cat("Suppression de ", max_p_var, " (p-value ANOVA:", round(max_p_value, 4),
          " p-value déviance:", round(p_dev, 4), " p-value Wald:", round(max_p_value, 4), ")\n")
      model_current <- model_reduced
    }
  }
  
  # Résultats finaux
  cat("\nModèle final sélectionné :\n")
  print(formula(model_current))
  cat("\nRésumé du modèle :\n")
  print(summary(model_current))
  
  return(model_current)
}

# Fonction utilitaire pour comparer les coefficients
compare_coefficients <- function(model_full, model_reduced, alpha, threshold, max_p_var) {
  coef_full <- coef(model_full)
  coef_reduced <- coef(model_reduced)
  
  common <- intersect(names(coef_full), names(coef_reduced))
  if (length(common) == 0) return()
  
  summ_full <- summary(model_full)
  p_values <- summ_full@coef3[common, "Pr(>|z|)"]
  
  for (coef_name in common) {
    if (!is.na(p_values[coef_name]) && p_values[coef_name] < alpha) {
      change <- abs((coef_reduced[coef_name] - coef_full[coef_name]) / abs(coef_full[coef_name]))
      if (change > threshold) {
        warning(paste("Changement important (>", threshold*100, "%) pour", coef_name,
                      ": Ancien =", round(coef_full[coef_name], 4),
                      " Nouveau =", round(coef_reduced[coef_name], 4),
                      " Rapport =", round(coef_full[coef_name], 4)/round(coef_reduced[coef_name], 4),
                      " Etape :", max_p_var))
      }
    }
  }
}


final_model_avant <- stepwise_multinomial_vglm(data = donnees_mean2, response_var = "y")

anova(final_model)
summary(final_model)

# On remarque que gene_majo ne sera pas interprétable significativement, on le supprime et on vérifie que les coefficients ne changent pas trop
final_model <- vglm(y ~ X09x3_FAB_CROISS_reg_rec + X12x2_MAT_PC + 
                      T10_PS_EauDebi_3 + X07x1_AN_CONST4_mean_3 + LR_LRF + T13_ENG_milieuDegrad.x + 
                      A03_Pos10sVERS + A03_PosSeroMyAs + A03_TxPosSero22sTgReel + 
                      A04_My10Mfloc, family = multinomial, data=donnees_mean2)
summary(final_model)


# Création du tableau comparatif
library(dplyr)

# Extraire les coefficients des deux modèles
coef_avant <- coef(summary(final_model_avant)) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("Variable") %>% 
  rename(Estimate_avant = Estimate, 
         SE_avant = "Std. Error", 
         p_value_avant = "Pr(>|z|)")

coef_apres <- coef(summary(final_model)) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("Variable") %>% 
  rename(Estimate_apres = Estimate, 
         SE_apres = "Std. Error", 
         p_value_apres = "Pr(>|z|)")

# Fusionner les deux tableaux
comparaison_coef <- full_join(coef_avant, coef_apres, by = "Variable") %>%
  mutate(
    Change_Estimate = round((Estimate_apres - Estimate_avant)/abs(Estimate_avant)*100, 1),
    Signif_change = case_when(
      abs(Change_Estimate) > 10 & p_value_avant < 0.05 ~ "ATTENTION",
      TRUE ~ "ok"
    )
  )

# Afficher le tableau
comparaison_coef %>%
  select(Variable, Estimate_avant, Estimate_apres, Change_Estimate, 
         p_value_avant, p_value_apres, Signif_change) %>%
  arrange(desc(abs(Change_Estimate))) %>%
  knitr::kable(digits = 3)

# Les coefficients ne changent pas significativement

# Calcul du pseudo R²
pseudo_r2 <- 1 - (logLik(final_model) / logLik(vglm(y ~ 1, data = donnees_mean2, family = multinomial)))
cat("Pseudo R² :", pseudo_r2, "\n")
print(pseudo_R2 <- 1 - deviance(final_model) / deviance(vglm(y ~ 1, data = donnees_mean2, family = multinomial)))


levels(donnees_mean2$y)
