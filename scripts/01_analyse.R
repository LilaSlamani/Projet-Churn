
library(tidyverse)     # manipulation des données + visualisation
library(gtsummary)     # tableaux statistiques propres
library(naniar)        # gestion des valeurs manquantes
library(GGally)        #graphiques exploratoires



# CHARGEMENT & NETTOYAGE ---
data <- read.csv("data/churn_internet.csv", stringsAsFactors = TRUE)

# Aperçu rapide des données
head(data)      # premières lignes
str(data)       # structure des variables
summary(data)   # statistiques descriptives globales


# Création d’un nouveau jeu de données "data_clean" afin de conserver les données brutes intactes

data_clean <- data %>%
  mutate(
    # Les variables monétaires sont stockées en texte avec une virgule comme séparateur décimal.
    # On remplace la virgule par un point puis on convertit en numérique.
    charges.mensuelles = as.numeric(
      str_replace(charges.mensuelles, ",", ".")
    ),
    
    Charges.totales = as.numeric(
      str_replace(Charges.totales, ",", ".")
    ),
    
    # Transformation de la variable Senior (0 / 1) en variable catégorielle lisible
    Senior = factor(
      Senior,
      levels = c(0, 1),
      labels = c("Non", "Oui")
    ),
    
    # Transformation explicite de la variable cible : target = Oui (churn) / Non (pas de churn)
    target = factor(target)
  )

# Cela devrait afficher 11 NA dans "Charges.totales"
print(colSums(is.na(data_clean)))
cat('\n')

# On supprime les lignes vides
data_clean <- data_clean %>% drop_na()

# Vérification : Doit afficher 0 partout
print(colSums(is.na(data_clean)))

# ANALYSE DESCRIPTIVE 

mon_tableau <- data_clean %>%
  select(Genre, Senior, Anciennete, charges.mensuelles, target) %>%
  tbl_summary(
    by = target, 
    statistic = list(all_continuous() ~ "{mean} ({sd})") 
  ) %>%
  add_p()

print(mon_tableau) # S'affiche dans le Viewer


# ANALYSE DESCRIPTIVE DE LA VARIABLE CIBLE
# Nombre de clients churn / non churn
table(data_clean$target)

# Proportion de churn
prop.table(table(data_clean$target))

# Visualisation de la répartition du churn
ggplot(data_clean, aes(x = target, fill = target)) +
  geom_bar() +
  labs(
    title = "Répartition des clients selon le churn",
    x = "Churn",
    y = "Nombre de clients"
  ) +
  theme_minimal()

# Churn selon le type de contrat cela permet d’analyser l’impact de la durée d’engagement

print(
ggplot(data_clean, aes(x = Contrat, fill = target)) +
  geom_bar(position = "fill") +
  labs(
    title = "Churn selon le type de contrat",
    y = "Proportion de clients"
  ) +
  theme_minimal()
)

# Charges mensuelles et churn : comparaison des distributions à l’aide d’un boxplot

print(
ggplot(data_clean, aes(x = target, y = charges.mensuelles, fill = target)) +
  geom_boxplot() +
  labs(
    title = "Charges mensuelles selon le churn",
    x = "Churn",
    y = "Charges mensuelles"
  ) +
  theme_minimal()
)

# Ancienneté des clients et churn -> Hypothèse que les clients récents quittent plus souvent
print(
ggplot(data_clean, aes(x = target, y = Anciennete, fill = target)) +
  geom_boxplot() +
  labs(
    title = "Ancienneté des clients selon le churn",
    y = "Ancienneté (en mois)"
  ) +
  theme_minimal()
)



# Tableau comparatif des variables numériques selon la variable cible avec test statistique

data_clean %>%
  select(target, charges.mensuelles, Charges.totales, Anciennete) %>%
  tbl_summary(by = target) %>%
  add_p()


# Senior vs Désabonnement
print(
  data_clean %>% 
    ggplot(aes(x = Senior, fill = target)) + 
    geom_bar(position = "fill") +            
    labs(
      title = "Churn selon le statut Senior",
      y = "Proportion",
      x = "Est Senior ?",
      fill = "Désabonnement"
    ) +
    theme_minimal() 
)

# TABLEAU STATISTIQUE (GTSUMMARY)
data_clean %>%
  select(target, charges.mensuelles, Charges.totales, Anciennete) %>%
  tbl_summary(by = target) %>%
  add_p()

# SAUVEGARDE DES DONNÉES NETTOYÉES
saveRDS(data_clean, "data/data_clean.rds")

