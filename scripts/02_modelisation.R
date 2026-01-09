# ==============================================================================
# PROJET : PRÉDICTION DU CHURN CLIENT D'UN FAI
# ==============================================================================

library(tidyverse)
library(caret)
library(pROC)
library(knitr)

cat("\n************************************************************\n")
cat("* 1. PRÉPARATION ET ÉQUILIBRAGE DES DONNÉES                 *\n")
cat("************************************************************\n")

data_clean <- readRDS("data/data_clean.rds")

data_model <- data_clean %>%
  select(-ID) %>%
  mutate_if(is.character, as.factor)

set.seed(123)
index <- createDataPartition(data_model$target, p = 0.7, list = FALSE)
train_initial <- data_model[index, ]
test  <- data_model[-index, ]

train_balance <- upSample(x = train_initial[, -ncol(train_initial)], 
                          y = train_initial$target)

names(train_balance)[names(train_balance) == "Class"] <- "target" 

distrib <- data.frame(
  Etape = c("Avant UpSampling", "Après UpSampling"),
  Non_Churn = c(table(train_initial$target)[1], table(train_balance$target)[1]),
  Oui_Churn = c(table(train_initial$target)[2], table(train_balance$target)[2])
)
print(kable(distrib, caption = "Évolution de la distribution des classes"))

cat("\n************************************************************\n")
cat("* 2. SÉLECTION RÉCURSIVE DES VARIABLES (RFE)                *\n")
cat("************************************************************\n")

control_rfe <- rfeControl(functions = rfFuncs, method = "cv", number = 5)

resultats_rfe <- rfe(x = train_balance[, -ncol(train_balance)], 
                     y = train_balance$target,
                     sizes = c(1:13),
                     rfeControl = control_rfe)

variables_opti <- predictors(resultats_rfe)
train_balance <- train_balance[, c(variables_opti, "target")]

cat("Variables retenues par l'algorithme RFE :\n")
print(variables_opti)

cat("\n************************************************************\n")
cat("* 3. ENTRAÎNEMENT ET BENCHMARKING DES MODÈLES               *\n")
cat("************************************************************\n")

train_control <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

model_reg_logistique <- train(
  target ~ ., 
  data = train_balance, 
  method = "glm", 
  family = "binomial",
  preProcess = c("center", "scale"), 
  trControl = train_control,
  metric = "ROC"
)

modele_rf <- train(
  target ~ ., 
  data = train_balance, 
  method = "rf", 
  trControl = train_control, 
  metric = "ROC", 
  ntree = 100
)

comparaison <- resamples(list(Logistique = model_reg_logistique, RandomForest = modele_rf))
stats_roc <- summary(comparaison)$statistics$ROC[, "Mean", drop = FALSE] %>%
  as.data.frame() %>%
  rename(AUC_Moyen = Mean)

stats_roc$Sensibilite <- summary(comparaison)$statistics$Sens[, "Mean"]
stats_roc$Specificite <- summary(comparaison)$statistics$Spec[, "Mean"]

stats_roc <- stats_roc %>% mutate(Modele = rownames(stats_roc)) %>% select(Modele, everything())

print(kable(stats_roc, digits = 4, row.names = FALSE, caption = "Tableau 1 : Comparaison des métriques de performance"))

cat("\n************************************************************\n")
cat("* 4. ANALYSE DE L'IMPORTANCE DES VARIABLES (RANDOM FOREST) *\n")
cat("************************************************************\n")

imp_var_rf <- varImp(modele_rf)$importance
imp_df <- data.frame(Variable = rownames(imp_var_rf), Importance = imp_var_rf$Overall) %>%
  arrange(desc(Importance))

print(kable(imp_df, digits = 2, row.names = FALSE, caption = "Tableau 2 : Importance des variables (Random Forest)"))

plot(varImp(modele_rf), main = "Hiérarchie de l'importance des prédicteurs - Random Forest")

cat("\n************************************************************\n")
cat("* 5. ÉVALUATION FINALE SUR LE JEU DE TEST (RANDOM FOREST)  *\n")
cat("************************************************************\n")

prob_test <- predict(modele_rf, newdata = test, type = "prob")[, "Oui"]
pred_test <- predict(modele_rf, newdata = test)

conf_mat <- as.data.frame(confusionMatrix(pred_test, test$target)$table)
print(kable(conf_mat, caption = "Tableau 3 : Matrice de Confusion (Random Forest)"))

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

roc_obj <- roc(test$target, prob_test)

cat("\n************************************************************\n")
cat("* 6. VISUALISATIONS - RANDOM FOREST                        *\n")
cat("************************************************************\n")

df_cm <- as.data.frame(confusionMatrix(pred_test, test$target)$table)

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

plot(roc_obj, 
     col = "#084594", 
     lwd = 3, 
     main = "Courbe ROC - Random Forest",
     print.auc = FALSE,
     grid = TRUE)

saveRDS(modele_rf, "data/modele_churn.rds")

cat("\n************************************************************\n")
cat("* FIN DU SCRIPT : RANDOM FOREST SAUVEGARDÉE AVEC SUCCÈS     *\n")
cat("************************************************************\n")