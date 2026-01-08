# ==============================================================================
# PROJET : PRÉDICTION DU CHURN CLIENT D'UN FAI
# ==============================================================================

library(tidyverse)
library(caret)
library(pROC)
library(knitr)

cat("\n************************************************************\n")
cat("* 1. PRÉPARATION ET ÉQUILIBRAGE DES DONNÉES                *\n")
cat("************************************************************\n")

# Chargement des données nettoyées 
data_clean <- readRDS("data/data_clean.rds")

# Suppression de l'identifiant et conversion des colonnes textuelles en facteurs
data_model <- data_clean %>%
  select(-ID) %>%
  mutate_if(is.character, as.factor)

# --- SÉPARATION TRAIN / TEST ---
set.seed(123)
index <- createDataPartition(data_model$target, p = 0.7, list = FALSE) # 70 vs 30
train_initial <- data_model[index, ]
test  <- data_model[-index, ]

# --- RÉ-ÉCHANTILLONNAGE (UPSAMPLING) ---
train_balance <- upSample(x = train_initial[, -ncol(train_initial)], 
                          y = train_initial$target)

names(train_balance)[names(train_balance) == "Class"] <- "target" 

# Affichage de la distribution sous forme de tableau
distrib <- data.frame(
  Etape = c("Avant UpSampling", "Après UpSampling"),
  Non_Churn = c(table(train_initial$target)[1], table(train_balance$target)[1]),
  Oui_Churn = c(table(train_initial$target)[2], table(train_balance$target)[2])
)
print(kable(distrib, caption = "Évolution de la distribution des classes"))

cat("\n************************************************************\n")
cat("* 2. SÉLECTION RÉCURSIVE DES VARIABLES (RFE)               *\n")
cat("************************************************************\n")

# Définition du contrôle pour la sélection de variables
control_rfe <- rfeControl(functions = rfFuncs, method = "cv", number = 5)

# Calcul du sous-ensemble optimal
resultats_rfe <- rfe(x = train_balance[, -ncol(train_balance)], 
                     y = train_balance$target,
                     sizes = c(1:13),
                     rfeControl = control_rfe)

variables_opti <- predictors(resultats_rfe)
train_balance <- train_balance[, c(variables_opti, "target")]

cat("Variables retenues par l'algorithme RFE :\n")
print(variables_opti)

cat("\n************************************************************\n")
cat("* 3. ENTRAÎNEMENT ET BENCHMARKING DES MODÈLES              *\n")
cat("************************************************************\n")

# Configuration de la validation croisée (10-Fold CV)
train_control <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# Modèle A : Régression Logistique (GLM)
model_reg_logistique <- train(
  target ~ ., 
  data = train_balance, 
  method = "glm", 
  family = "binomial",
  preProcess = c("center", "scale"), 
  trControl = train_control,
  metric = "ROC"
)

# Modèle B : Random Forest (RF) - Sélectionné comme modèle final
modele_rf <- train(
  target ~ ., 
  data = train_balance, 
  method = "rf", 
  trControl = train_control, 
  metric = "ROC", 
  ntree = 100
)

# --- TABLEAU COMPARATIF DES PERFORMANCES ---
comparaison <- resamples(list(Logistique = model_reg_logistique, RandomForest = modele_rf))
stats_roc <- summary(comparaison)$statistics$ROC[, "Mean", drop = FALSE] %>%
  as.data.frame() %>%
  rename(AUC_Moyen = Mean)

stats_roc$Sensibilite <- summary(comparaison)$statistics$Sens[, "Mean"]
stats_roc$Specificite <- summary(comparaison)$statistics$Spec[, "Mean"]

# Identification claire des modèles dans le tableau
stats_roc <- stats_roc %>% mutate(Modele = rownames(stats_roc)) %>% select(Modele, everything())

print(kable(stats_roc, digits = 4, row.names = FALSE, caption = "Tableau 1 : Comparaison des métriques de performance"))

cat("\n************************************************************\n")
cat("* 4. ANALYSE DE L'IMPORTANCE DES VARIABLES (RANDOM FOREST) *\n")
cat("************************************************************\n")

# Extraction et affichage de l'importance pour le modèle Random Forest
imp_var_rf <- varImp(modele_rf)$importance
imp_df <- data.frame(Variable = rownames(imp_var_rf), Importance = imp_var_rf$Overall) %>%
  arrange(desc(Importance))

print(kable(imp_df, digits = 2, row.names = FALSE, caption = "Tableau 2 : Importance des variables (Random Forest)"))

plot(varImp(modele_rf), main = "Hiérarchie de l'importance des prédicteurs - Random Forest")

cat("\n************************************************************\n")
cat("* 5. ÉVALUATION FINALE SUR LE JEU DE TEST (RANDOM FOREST)  *\n")
cat("************************************************************\n")

# Prédictions sur données non vues avec la Random Forest
prob_test <- predict(modele_rf, newdata = test, type = "prob")[, "Oui"]
pred_test <- predict(modele_rf, newdata = test)

# --- TABLEAU DE LA MATRICE DE CONFUSION ---
conf_mat <- as.data.frame(confusionMatrix(pred_test, test$target)$table)
print(kable(conf_mat, caption = "Tableau 3 : Matrice de Confusion (Random Forest)"))

# --- INDICATEURS DE PRÉCISION ---
perf_finale <- data.frame(
  Indicateur = c("Accuracy (Précision globale)", "Sensibilité (Rappel)", "Spécificité", "AUC Final"),
  Valeur = c(
    confusionMatrix(pred_test, test$target)$overall["Accuracy"],
    confusionMatrix(pred_test, test$target)$byClass["Sensitivity"],
    confusionMatrix(pred_test, test$target)$byClass["Specificity"],
    auc(roc(test$target, prob_test))
  )
)
print(kable(perf_finale, digits = 4, row.names = FALSE, caption = "Tableau 4 : Synthèse des indicateurs finaux (Random Forest)"))

# Visualisation de la courbe ROC
roc_obj <- roc(test$target, prob_test)
plot(roc_obj, col = "#E67E22", lwd = 3, main = "Courbe ROC - Performance Finale (Random Forest)")

# --- SAUVEGARDE DU MODÈLE FINAL ---
saveRDS(modele_rf, "data/modele_churn.rds")

cat("\n************************************************************\n")
cat("* FIN DU SCRIPT : RANDOM FOREST SAUVEGARDÉE AVEC SUCCÈS    *\n")
cat("************************************************************\n")