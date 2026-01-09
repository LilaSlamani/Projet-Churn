# --- GÉNÉRATION DE LA MATRICE (IMAGE 1) ---
df_cm <- as.data.frame(confusionMatrix(pred_test, test$target)$table)

p_matrix <- ggplot(df_cm, aes(x = Reference, y = Prediction, fill = Freq)) +
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

# Affiche la matrice dans la console Plots
print(p_matrix)

# Sauvegarde la matrice en haute définition pour tes slides
ggsave("matrice_confusion_rf.png", p_matrix, width = 8, height = 6, dpi = 300)

# --- GÉNÉRATION DE LA COURBE ROC (IMAGE 2) ---
# Note : Clique sur les flèches bleues dans l'onglet "Plots" pour revenir à la matrice
plot(roc_obj, 
     col = "#084594", 
     lwd = 3, 
     main = "Courbe ROC - Random Forest",
     grid = TRUE)