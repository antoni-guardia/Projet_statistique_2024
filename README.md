# Description du sujet
TODO
---
---

# Configuration de l'environnement pour le projet

## Étape 1 : Créer un fichier `.env`
1. Dans chaque séction du projet, créez un fichier nommé `.env`.
2. Ajoutez-y la ligne suivante en remplaçant `path_bdd=/chemin/vers/votre/base_de_donnees` par le chemin local vers votre base de données.

## Étape 2 : Installer la librairie `dotenv`
Pour utiliser le fichier `.env` dans R, vous devez installer la librairie `dotenv`. Si elle n'est pas déjà installée, exécutez la commande suivante dans R :
```R
install.packages("dotenv")
```
# Gestion des dépendances avec `renv`

Ce projet utilise le package `renv` pour gérer les dépendances. Cela garantit que tous les membres de l'équipe travaillent avec les mêmes versions des packages R nécessaires au projet.

### Étape 1 : Installer `renv`
Assurez-vous que le package `renv` est installé sur votre machine :
```R
install.packages("renv")
```
### Étape 2 : Restaurer l'environement
Restaurez l'environnement pour installer les dépendances nécessaires :
```R
renv::restore()
```
### Étape 3 : Ajouter de nouvelles dépendances

Lorsque vous ajoutez un nouveau package au projet, installez-le normalement avec `install.packages()`. Ensuite, synchronisez l'environnement avec :
```R
renv::snapshot()
```

---
---

# Installation R sur Vscode
## Mise en route
Vous allez dans github et clonnez facilement le projet et installez tout ce qui est proposé par Vscode.
Faut installer R sur votre machine [https://cran.r-project.org/bin/windows/base/](https://cran.r-project.org/bin/windows/base/)

Sur R, il va falloir installer les libreries suivantes afin d'executer le R Markdown.
  - install.packages("rmarkdown")
  - install.packages("knitr")
  - install.packages("tinytex")


Finallement installez padoc : [https://pandoc.org/installing.html](https://pandoc.org/installing.html)
Et normalment tout est bon, sinon, n'hésitez pas à m'envoyer un message.

## Génération d'un Document R Markdown

1. **Créer un fichier `.Rmd`** :
   - Dans VSCode, créez un nouveau fichier avec l'extension `.Rmd`.

2. **Exécuter le fichier** :
   - Dans le terminal intégré de VSCode, exécutez :
     ```bash
     Rscript -e "rmarkdown::render('votre_fichier.Rmd')"
     ```
   - Le fichier généré sera disponible dans le même répertoire que le fichier source.

3. **Résolution des Erreurs** :
   - Si des erreurs surviennent, vérifiez que toutes les dépendances sont installées.

## Ressources Supplémentaires
- Documentation R Markdown : [https://rmarkdown.rstudio.com/](https://rmarkdown.rstudio.com/)
- Support VSCode : [https://code.visualstudio.com/docs](https://code.visualstudio.com/docs)

---
---

# Mise en Route de LaTeX sur VSCode

Bien que LaTeX ne soit pas requis pour exécuter les fichiers R Markdown, si vous souhaitez générer des documents PDF ou tirer parti des fonctionnalités avancées de LaTeX, voici comment le configurer :

### 1. Installer une Distribution LaTeX
- Téléchargez et installez une distribution LaTeX :
  - [MiKTeX](https://miktex.org/) (recommandé pour Windows).

### 2. Vérifier l'Installation de LaTeX
- Ouvrez une console et exécutez :
  ```bash
  pdflatex --version
  ```

### 3. Configurer VSCode pour LaTeX
- Installez l'extension **LaTeX Workshop** depuis le marketplace de VSCode.
- Configurez les paramètres pour reconnaître la distribution LaTeX installée.

### 4. Générer un PDF
- Si votre fichier R Markdown nécessite LaTeX pour générer un PDF, assurez-vous que LaTeX est installé et exécutez :
  ```bash
  Rscript -e "rmarkdown::render('votre_fichier.Rmd')"
  ```
