
# LIBRAIRIES

library(tidyverse)
library(caret)
library(pROC)

# CHARGEMENT DES DONNÉES NETTOYÉES
data_clean <- readRDS("data/data_clean.rds")

# Suppression de l'identifiant
data_model <- data_clean %>%
  select(-ID)

# Vérification
str(data_model)

# SÉPARATION TRAIN / TEST

set.seed(123)

index <- createDataPartition(
  data_model$target,
  p = 0.7,
  list = FALSE
)

train <- data_model[index, ]
test  <- data_model[-index, ]

# Vérification du déséquilibre
prop.table(table(train$target))
prop.table(table(test$target))


# ENTRAÎNEMENT : RÉGRESSION LOGISTIQUE


modele_logistique <- glm(
  target ~ .,
  data = train,
  family = binomial
)

summary(modele_logistique)

# PRÉDICTIONS

prob_test <- predict(
  modele_logistique,
  newdata = test,
  type = "response"
)

# Seuil adapté au déséquilibre
seuil <- 0.4

pred_test <- ifelse(prob_test > seuil, "Oui", "Non")
pred_test <- factor(pred_test, levels = c("Non", "Oui"))

# ÉVALUATION DU MODÈLE

# Matrice de confusion
confusionMatrix(pred_test, test$target)

# Courbe ROC et AUC
roc_obj <- roc(test$target, prob_test)

plot(
  roc_obj,
  col = "blue",
  main = "Courbe ROC - Régression Logistique"
)

auc(roc_obj)
