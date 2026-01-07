
library(tidyverse)  # Contient ggplot2, dplyr, readr
library(gtsummary)  # Pour le tableau statistique 
library(naniar)     # Pour les données manquantes 
library(GGally)

# --- PARTIE 1 : CHARGEMENT & NETTOYAGE ---
data <- read.csv("data/churn_internet.csv", stringsAsFactors = TRUE)

data_clean <- data %>%
  mutate(
    charges.mensuelles = as.numeric(str_replace(charges.mensuelles, ",", ".")),
    Charges.totales = as.numeric(str_replace(Charges.totales, ",", ".")),
    Senior = factor(Senior, levels = c(0, 1), labels = c("Non", "Oui"))
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

# --- PARTIE 3 : Graphiques ---

# Graphique : Senior vs Désabonnement
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


# 2. Deuxième Plot : La Matrice de Corrélation (Prix vs Ancienneté)

plot_corr <- data_clean %>%
  select(Anciennete, charges.mensuelles, Charges.totales, target) %>%
  ggpairs(
    aes(color = target, alpha = 0.5), # Colorié selon le départ
    columns = 1:3,
    title = "Matrice de Corrélation : Prix vs Ancienneté"
  )

print(plot_corr)