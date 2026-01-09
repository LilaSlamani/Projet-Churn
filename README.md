# Projet Churn – Prédiction du Désabonnement Client

Ce projet vise à analyser et prédire le désabonnement (churn) des clients d’un fournisseur d’accès à internet.  
L’objectif est d’identifier les clients à risque de churn afin de soutenir des actions de fidélisation ciblées.

---

## Contexte

Dans un marché très concurrentiel comme celui des fournisseurs d’accès à internet, fidéliser un client coûte moins cher que d’en acquérir un nouveau.  
La compréhension et la prédiction du churn sont donc essentielles pour optimiser les actions marketing et améliorer la rétention des clients.

---

## Contenu du dépôt


## Auteurs et Organisation
*   Membres du groupe : Chaimae RAMDANI & Lila SLAMANI
*   Suivi du projet : https://trello.com/invite/b/695d3107d1362cde02eafabc/ATTI8ef4ff99a6d8611cbbfd0402d5430c3a18737293/projet-churn
*   Lien de l'application :  https://6i1l7w-lila-slamani.shinyapps.io/churn_app/
  ```text
Projet-Churn/

├── data/
│ ├── churn_internet.csv # Données brutes
│ └── data_clean.rds # Données nettoyées prêtes à l’analyse
├── scripts/
│ ├── 01_analyse_exploratoire.R # Nettoyage + EDA
│ └── 02_modelisation.R # Modélisation + comparaison
├── app.R # WebApp Shiny
├── .gitignore
└── README.md
```

---

##  Technologies utilisées

Le projet est réalisé entièrement en **R** :

| Usage | Outil |
|-------|------|
| Langage | **R** |
| Analyse & visualisation | **tidyverse, GGally** |
| Statistiques descriptives | **gtsummary** |
| Modélisation | **caret**, **pROC** |
| Déploiement web | **Shiny** |
| Versionning & collaboration | **GitHub** |

---

##  Étapes du projet

### 1.  Chargement et nettoyage des données
- Conversion des variables financières au format numérique.
- Transformation des variables catégorielles en facteurs.
- Suppression des observations avec valeurs manquantes.
- (Optionnel) Suppression ou traitement des valeurs aberrantes.

➡️ Création du dataset **`data_clean`**.

---

### 2.  Analyse exploratoire des données (EDA)

Objectifs :
- Comprendre la structure du dataset.
- Visualiser la variable cible `target` (churn).
- Identifier les facteurs influençant le churn :
  - ancienneté,
  - statistique Senior,
  - type de contrat,
  - charges mensuelles/ totales.

Résultats :
- Données fortement déséquilibrées.
- Contrats mensuels et charges élevées associés à un churn plus élevé.
- Clients récents plus susceptibles de churner.

---

### 3. Modélisation

#### 🔹 Préparation des données
- Séparation Train / Test (70% / 30%).
- Ré-échantillonnage du jeu d’entraînement (`upSample`) pour corriger le déséquilibre.

#### 🔹 Modèles comparés
- **Régression logistique**
- **Random Forest**

#### 🔹 Sélection de variables
- Algorithme **RFE** (Recursive Feature Elimination) pour identifier les variables les plus pertinentes.

---

### 4. Évaluation des modèles

Critères évalués :
- Matrice de confusion
- Courbe ROC & AUC
- Accuracy
- Sensibilité (Recall)
- Spécificité

Résultats :
- Random Forest montre de meilleures performances (AUC plus élevée) que la régression logistique.
  
---

### 5.  Importance des variables

L’analyse a révélé que certaines variables influencent fortement la prédiction du churn (par exemple, ancienneté, charges mensuelles, type de contrat).

---

### 6. Déploiement Shiny

Une **WebApp interactive** a été développée permettant :
- la saisie des informations client,
- la prédiction de probabilité de churn,
- l’affichage du résultat de manière lisible.

---
###  Préparer l’environnement R

Installer les packages nécessaires :

```r
install.packages(c(
  "tidyverse",
  "caret",
  "pROC",
  "ggplot2",
  "gtsummary",
  "naniar",
  "GGally",
  "shiny",
  "DT"
))



