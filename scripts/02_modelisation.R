
library(caret)      # Machine Learning : train/test, modèles, évaluation
library(pROC)       # Courbe ROC et calcul de l’AUC
library(knitr)      # Affichage propre des tableaux (kable)

cat("PRÉPARATION ET ÉQUILIBRAGE DES DONNÉES                 \n")

# Chargement du dataset nettoyé depuis l'analyse exploratoire
data_clean <- readRDS("data/data_clean.rds")

# Suppression de l'identifiant (non informatif pour la prédiction)
# Conversion des variables caractères en facteurs

data_model <- data_clean %>%
  select(-ID) %>%
  mutate_if(is.character, as.factor)

# Fixation de la graine aléatoire pour garantir la reproductibilité
set.seed(123)

# Séparation stratifiée Train / Test (70% / 30%)
index <- createDataPartition(data_model$target, p = 0.7, list = FALSE)
train_initial <- data_model[index, ]
test  <- data_model[-index, ]

# Ré-équilibrage du jeu d’entraînement par up-sampling
# Objectif : corriger le déséquilibre des classes (churn / non churn)
train_balance <- upSample(x = train_initial[, -ncol(train_initial)], 
                          y = train_initial$target)

# upSample renomme la variable cible en "Class"
# On la renomme explicitement en "target"
names(train_balance)[names(train_balance) == "Class"] <- "target" 

# Tableau comparatif de la distribution des classes avant / après up-sampling
distrib <- data.frame(
  Etape = c("Avant UpSampling", "Après UpSampling"),
  Non_Churn = c(table(train_initial$target)[1], table(train_balance$target)[1]),
  Oui_Churn = c(table(train_initial$target)[2], table(train_balance$target)[2])
)
print(kable(distrib, caption = "Évolution de la distribution des classes"))

cat("SÉLECTION RÉCURSIVE DES VARIABLES (RFE)                \n")

# Configuration du RFE avec Random Forest et validation croisée
control_rfe <- rfeControl(functions = rfFuncs, method = "cv", number = 5)

# Lancement du RFE pour identifier les variables les plus pertinentes
resultats_rfe <- rfe(x = train_balance[, -ncol(train_balance)], 
                     y = train_balance$target,
                     sizes = c(1:13),
                     rfeControl = control_rfe)

# Récupération des variables optimales sélectionnées
variables_opti <- predictors(resultats_rfe)

# Conservation uniquement des variables sélectionnées + cible
train_balance <- train_balance[, c(variables_opti, "target")]

cat("Variables retenues par l'algorithme RFE :\n")
print(variables_opti)

cat("ENTRAÎNEMENT ET BENCHMARKING DES MODÈLES               \n")

# Configuration de la validation croisée
train_control <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# Entraînement du modèle de régression logistique
model_reg_logistique <- train(
  target ~ ., 
  data = train_balance, 
  method = "glm", 
  family = "binomial",
  preProcess = c("center", "scale"), 
  trControl = train_control,
  metric = "ROC"
)

# Entraînement du modèle Random Forest
modele_rf <- train(
  target ~ ., 
  data = train_balance, 
  method = "rf", 
  trControl = train_control, 
  metric = "ROC", 
  ntree = 100
)

# Comparaison des performances des deux modèles
comparaison <- resamples(list(Logistique = model_reg_logistique, RandomForest = modele_rf))

# Extraction des métriques moyennes
stats_roc <- summary(comparaison)$statistics$ROC[, "Mean", drop = FALSE] %>%
  as.data.frame() %>%
  rename(AUC_Moyen = Mean)

stats_roc$Sensibilite <- summary(comparaison)$statistics$Sens[, "Mean"]
stats_roc$Specificite <- summary(comparaison)$statistics$Spec[, "Mean"]

stats_roc <- stats_roc %>% mutate(Modele = rownames(stats_roc)) %>% select(Modele, everything())

print(kable(stats_roc, digits = 4, row.names = FALSE, caption = "Tableau 1 : Comparaison des métriques de performance"))

cat("ANALYSE DE L'IMPORTANCE DES VARIABLES (RANDOM FOREST) \n")

# Calcul de l’importance des variables
imp_var_rf <- varImp(modele_rf)$importance

# Mise en forme et tri décroissant
imp_df <- data.frame(Variable = rownames(imp_var_rf), Importance = imp_var_rf$Overall) %>%
  arrange(desc(Importance))

print(kable(imp_df, digits = 2, row.names = FALSE, caption = "Tableau 2 : Importance des variables (Random Forest)"))

# Visualisation de l'importance des prédicteurs
plot(varImp(modele_rf), main = "Hiérarchie de l'importance des prédicteurs - Random Forest")

cat("ÉVALUATION FINALE SUR LE JEU DE TEST (RANDOM FOREST)  \n")

# Probabilités de churn prédites
prob_test <- predict(modele_rf, newdata = test, type = "prob")[, "Oui"]

# Classes prédites
pred_test <- predict(modele_rf, newdata = test)

# Matrice de confusion
conf_mat <- as.data.frame(confusionMatrix(pred_test, test$target)$table)
print(kable(conf_mat, caption = "Tableau 3 : Matrice de Confusion (Random Forest)"))

# Calcul des métriques finales
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

# Objet ROC
roc_obj <- roc(test$target, prob_test)

cat("VISUALISATIONS - RANDOM FOREST                        \n")

# Préparation de la matrice de confusion pour ggplot
df_cm <- as.data.frame(confusionMatrix(pred_test, test$target)$table)

# Heatmap de la matrice de confusion
print(
  ggplot(df_cm, aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile(color = "white", size = 1) +
    scale_fill_gradient(low = "#ebf4fb", high = "#084594") +
    geom_text(aes(label = Freq), color = "black", size = 6) +
    labs(
      title = "Matrice de confusion – Random Forest",
      x = "Classe réelle",
      y = "Classe prédite",
      fill = "Fréquence"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", color = "#084594", size = 16),
      panel.grid = element_blank()
    )
)
# Courbe ROC
plot(roc_obj, 
     col = "#084594", 
     lwd = 3, 
     main = "Courbe ROC - Random Forest",
     print.auc = FALSE,
     grid = TRUE)

# Sauvegarde du modèle final
saveRDS(modele_rf, "data/modele_churn.rds")

cat("RANDOM FOREST SAUVEGARDÉE AVEC SUCCÈS     \n")
