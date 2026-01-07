
# LIBRAIRIES

library(caret) # séparation train/test et évaluation
library(pROC) #courbe ROC et AUC

# CHARGEMENT DES DONNÉES NETTOYÉES
data_clean <- readRDS("data/data_clean.rds")

# Suppression de l'identifiant
data_model <- data_clean %>%
  select(-ID) %>%
  mutate_if(is.character, as.factor)


# SÉPARATION TRAIN / TEST

#fige le hasard pour que les résultats soient reproductibles
set.seed(123)

# Séparation stratifiée (respect du déséquilibre)
index <- createDataPartition(
  data_model$target,
  p = 0.7,
  list = FALSE
)

train <- data_model[index, ]
test  <- data_model[-index, ]

# RÉ-ÉCHANTILLONNAGE
train_balance <- upSample(x = train[, -ncol(train)], # Toutes les colonnes sauf target
                          y = train$target)          # La colonne target

names(train_balance)[names(train_balance) == "Class"] <- "target" 
# upSample renomme la cible en "Class", on remet "target"


print("Distribution AVANT ré-échantillonnage :")
print(prop.table(table(train$target)))
print("Distribution APRÈS ré-échantillonnage :")
print(prop.table(table(train_balance$target))) # Devrait être 50/50


# 2. Modélisation (Sur les données équilibrées 'train_balance')
modele_logistique <- glm(
  target ~ .,
  data = train_balance, 
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
seuil <- 0.5

pred_test <- ifelse(prob_test > seuil, "Oui", "Non")
pred_test <- factor(pred_test, levels = c("Non", "Oui"))

# ÉVALUATION DU MODÈLE

# Matrice de confusion
print(confusionMatrix(pred_test, test$target))

# Courbe ROC et AUC
roc_obj <- roc(test$target, prob_test)

plot(
  roc_obj,
  col = "blue",
  main = "Courbe ROC - Régression Logistique"
)

print(auc(roc_obj))
