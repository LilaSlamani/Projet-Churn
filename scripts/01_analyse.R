
library(tidyverse)# manipulation des données + visualisation
library(gtsummary)# tableaux statistiques propres
library(naniar) # gestion des valeurs manquantes
library(GGally)#graphiques exploratoires



# --- PARTIE 1 : CHARGEMENT & NETTOYAGE ---
data <- read.csv("data/churn_internet.csv", stringsAsFactors = TRUE)

# Aperçu rapide des données
head(data)      # premières lignes
str(data)       # structure des variables
summary(data)   # statistiques descriptives globales


# Création d’un nouveau jeu de données "data_clean"
# afin de conserver les données brutes intactes

data_clean <- data %>%
  mutate(
    # Les variables monétaires sont stockées en texte
    # avec une virgule comme séparateur décimal.
    # On remplace la virgule par un point puis on convertit en numérique.
    charges.mensuelles = as.numeric(
      str_replace(charges.mensuelles, ",", ".")
    ),
    
    Charges.totales = as.numeric(
      str_replace(Charges.totales, ",", ".")
    ),
    
    # Transformation de la variable Senior (0 / 1)
    # en variable catégorielle lisible
    Senior = factor(
      Senior,
      levels = c(0, 1),
      labels = c("Non", "Oui")
    ),
    
    # Transformation explicite de la variable cible
    # target = Oui (churn) / Non (pas de churn)
    target = factor(target)
  )

# Cela devrait afficher 11 NA dans "Charges.totales"
print(colSums(is.na(data_clean)))
print('\n')

# On supprime les lignes vides
data_clean <- data_clean %>% drop_na()

# Doit afficher 0 partout
print(colSums(is.na(data_clean)))

# --- PARTIE 2 : ANALYSE UNIVARIÉE (Tableau) ---

mon_tableau <- data_clean %>%
  select(Genre, Senior, Anciennete, charges.mensuelles, target) %>%
  tbl_summary(
    by = target, 
    statistic = list(all_continuous() ~ "{mean} ({sd})") 
  ) %>%
  add_p()

print(mon_tableau) # S'affiche dans le Viewer


#ANALYSE DESCRIPTIVE DE LA VARIABLE CIBLE
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

#Visualisation 

# Churn selon le type de contrat cela permet d’analyser l’impact de la durée d’engagement

ggplot(data_clean, aes(x = Contrat, fill = target)) +
  geom_bar(position = "fill") +
  labs(
    title = "Churn selon le type de contrat",
    y = "Proportion de clients"
  ) +
  theme_minimal()


# Charges mensuelles et churn : comparaison des distributions à l’aide d’un boxplot

ggplot(data_clean, aes(x = target, y = charges.mensuelles, fill = target)) +
  geom_boxplot() +
  labs(
    title = "Charges mensuelles selon le churn",
    x = "Churn",
    y = "Charges mensuelles"
  ) +
  theme_minimal()


# Ancienneté des clients et churn -> Hypothèse que les clients récents quittent plus souvent

ggplot(data_clean, aes(x = target, y = Anciennete, fill = target)) +
  geom_boxplot() +
  labs(
    title = "Ancienneté des clients selon le churn",
    y = "Ancienneté (en mois)"
  ) +
  theme_minimal()


# Churn selon le statut Senior

ggplot(data_clean, aes(x = Senior, fill = target)) +
  geom_bar(position = "fill") +
  labs(
    title = "Churn selon le statut Senior",
    y = "Proportion"
  ) +
  theme_minimal()


# Tableau comparatif des variables numériques selon la variable cible avec test statistique

data_clean %>%
  select(target, charges.mensuelles, Charges.totales, Anciennete) %>%
  tbl_summary(by = target) %>%
  add_p()


# Senior vs Désabonnement
plot_senior <- data_clean %>%
  ggplot(aes(x = Senior, fill = target)) + 
  geom_bar(position = "fill") +            
  labs(
    title = "Impact du statut Senior sur le Désabonnement",
    y = "Proportion",
    x = "Est Senior ?",
    fill = "Désabonnement"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") # Couleurs distinctes

print(plot_senior) 


# La Matrice de Corrélation (Prix vs Ancienneté)

plot_corr <- data_clean %>%
  select(Anciennete, charges.mensuelles, Charges.totales, target) %>%
  ggpairs(
    aes(color = target, alpha = 0.5), # Colorié selon le départ
    columns = 1:3,
    title = "Matrice de Corrélation : Prix vs Ancienneté"
  )

print(plot_corr)

# TABLEAU STATISTIQUE (GTSUMMARY)
data_clean %>%
  select(target, charges.mensuelles, Charges.totales, Anciennete) %>%
  tbl_summary(by = target) %>%
  add_p()

# SAUVEGARDE DES DONNÉES NETTOYÉES
<<<<<<< HEAD
saveRDS(data_clean, "data/data_clean.rds")
=======
saveRDS(data_clean, "data/data_clean.rds")
>>>>>>> 0fe38bd1dadec43c0c1c1d67d13e86e6a127858a
