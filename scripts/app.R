library(shiny)
library(ggplot2)
library(dplyr)
library(DT)           
library(caret)
library(stringr)

# --- CONFIGURATION DU CHEMIN DU MODÈLE ---
chemin_modele <- "../data/modele_churn.rds" 
modele_final <- if(file.exists(chemin_modele)) readRDS(chemin_modele) else NULL

# UI

ui <- fluidPage(
  title = "Interface Rétention FAI",
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600&display=swap"),
    tags$style(HTML("
      :root {
        --navy: #2F4156; --teal: #567C8D; --skyblue: #C8D9E6; --beige: #F5EFEB; 
        --white: #FFFFFF; --green-res: #27AE60; --red-res: #C0392B;
      }
      
      body { 
        background-color: var(--beige); font-family: 'Inter', sans-serif; 
        color: var(--navy); margin: 0; padding: 0; min-height: 100vh; overflow-y: auto !important; 
      }
      
      .main-container { padding: 20px; max-width: 1200px; margin: 0 auto; }
      
      .app-header { 
        display: flex; justify-content: space-between; align-items: center; 
        padding: 15px 25px; background: var(--white); border-radius: 12px; 
        box-shadow: 0 2px 10px rgba(0,0,0,0.03); margin-bottom: 20px;
      }
      .app-header h2 { margin: 0; font-weight: 600; font-size: 1.4em; color: var(--navy); }
      
      .nav-tabs { border-bottom: none !important; }
      .nav-tabs > li > a { border: none !important; color: var(--teal); padding: 10px 20px; transition: 0.3s; }
      .nav-tabs > li.active > a { 
        color: var(--navy) !important; font-weight: 600; 
        background: transparent !important; border-bottom: 3px solid var(--navy) !important; 
      }

      .card { 
        background: var(--white); border-radius: 12px; padding: 20px; 
        box-shadow: 0 4px 15px rgba(47, 65, 86, 0.05); margin-bottom: 20px;
      }
      .section-title { font-weight: 600; font-size: 11px; margin-bottom: 15px; color: var(--teal); text-transform: uppercase; letter-spacing: 1px; }

      .form-control { border-radius: 8px; border: 1px solid var(--skyblue); }
      .btn-predict { 
        background: var(--navy); color: white; border-radius: 8px; 
        font-weight: 600; padding: 12px; border: none; width: 100%; transition: 0.3s;
      }
      .btn-predict:hover { background: var(--teal); transform: translateY(-1px); }

      .res-box { 
        border-radius: 12px; padding: 25px; text-align: center; 
        background: var(--beige); border-left: 10px solid var(--skyblue); margin-bottom: 20px;
      }
      .res-box.high { border-left-color: var(--red-res) !important; background: #FDECEA; }
      .res-box.low { border-left-color: var(--green-res) !important; background: #E9F7EF; }
      
      .score { font-size: 3.5em; font-weight: 700; margin: 0; color: var(--navy); }
      .reco-container { padding: 20px; background: #FDFDFD; border-radius: 10px; border-left: 4px solid var(--skyblue); }
    "))
  ),
  
  div(class = "main-container",
      div(class = "app-header",
          div(h2("Pilotage de la Rétention Client")),
          tags$ul(class = "nav nav-tabs", id = "tab_nav",
                  tags$li(class = "active", tags$a(href = "#tab1", `data-toggle` = "tab", icon("database"), " Données")),
                  tags$li(tags$a(href = "#tab2", `data-toggle` = "tab", icon("robot"), " Simulation IA")),
                  tags$li(tags$a(href = "#tab3", `data-toggle` = "tab", icon("chart-bar"), " Visualisations"))
          )
      ),
      
      div(class = "tab-content",
          
          # --- ONGLET 1 : DONNÉES (AVEC SÉLECTEUR DE LIGNES) ---
          div(class = "tab-pane active", id = "tab1",
              div(class = "card",
                  div(class = "section-title", "Importation du dataset"),
                  fluidRow(
                    column(4, fileInput("file1", NULL, accept = ".csv", buttonLabel = "Sélectionner CSV", width = "100%")),
                    column(8, helpText("Le système nettoie automatiquement les formats et prépare les variables pour l'IA."))
                  ),
                  hr(),
                  div(class = "section-title", "Aperçu des données et filtrage"),
                  dataTableOutput("raw_data_table")
              )
          ),
          
          # --- ONGLET 2 : SIMULATION IA ---
          div(class = "tab-pane", id = "tab2",
              fluidRow(
                column(4, 
                       div(class = "card",
                           div(class = "section-title", "Profil Client"),
                           selectInput("input_contract", "Type de Contrat", choices = c("Mensuel", "Annuel", "Bisannuel")),
                           selectInput("input_internet", "Internet", choices = c("DSL", "Fibre optique", "Non")),
                           selectInput("input_senior", "Senior ?", choices = c("Non", "Oui")),
                           numericInput("input_tenure", "Ancienneté (Mois)", value = 12),
                           numericInput("input_charges", "Charges Mensuelles (€)", value = 65),
                           numericInput("input_total", "Charges Totales (€)", value = 1000),
                           actionButton("btn_predict", "ANALYSER LE RISQUE", class = "btn-predict")
                       )
                ),
                column(8,
                       div(class = "card",
                           div(class = "section-title", "Résultat de la Prédiction"),
                           uiOutput("modern_res_ui"),
                           div(class = "reco-container",
                               h4(style="margin-top:0; font-size:16px;", icon("lightbulb"), strong("Action Recommandée :")),
                               textOutput("txt_recommendation")
                           )
                       )
                )
              )
          ),
          
          # --- ONGLET 3 : VISUALISATIONS ---
          div(class = "tab-pane", id = "tab3",
              fluidRow(
                column(6, div(class = "card", div(class = "section-title", "Impact du Contrat"), plotOutput("plot_contrat", height = "350px"))),
                column(6, div(class = "card", div(class = "section-title", "Analyse des Charges"), plotOutput("plot_charges", height = "350px")))
              ),
              div(class = "card", div(class = "section-title", "Impact de l'Ancienneté"), plotOutput("plot_anciennete", height = "400px"))
          )
      )
  )
)

# ==============================================================================
# LOGIQUE SERVEUR
# ==============================================================================
server <- function(input, output) {
  
  viz_palette <- c("Non" = "#567C8D", "Oui" = "#D35400")
  
  data_reactive <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath, stringsAsFactors = TRUE)
    colnames(df)[colnames(df) == "Churn"] <- "target"
    df$charges.mensuelles <- as.numeric(str_replace(as.character(df$charges.mensuelles), ",", "."))
    df$Charges.totales <- as.numeric(str_replace(as.character(df$Charges.totales), ",", "."))
    df$Senior <- factor(df$Senior, levels = c(0, 1), labels = c("Non", "Oui"))
    na.omit(df)
  })
  
  # Ajout du lengthMenu pour sélectionner le nombre de lignes
  output$raw_data_table <- renderDataTable({ 
    datatable(data_reactive(), 
              options = list(
                scrollX = TRUE, 
                pageLength = 10, 
                lengthMenu = list(c(10, 25, 50, 100), c('10', '25', '50', '100')),
                dom = 'lftp' # 'l' active le sélecteur de longueur
              )) 
  })
  
  output$plot_contrat <- renderPlot({ 
    ggplot(data_reactive(), aes(x = Contrat, fill = target)) + 
      geom_bar(position = "fill", width = 0.65) + scale_fill_manual(values = viz_palette) + 
      theme_minimal() + labs(x="", y="", fill="Désabonnement")
  })
  
  output$plot_charges <- renderPlot({ 
    ggplot(data_reactive(), aes(x = target, y = charges.mensuelles, fill = target)) + 
      geom_boxplot(width = 0.45, alpha = 0.9) + scale_fill_manual(values = viz_palette) + 
      theme_minimal() + labs(x="Churn", y="Charges (€)") + theme(legend.position = "none")
  })
  
  output$plot_anciennete <- renderPlot({ 
    ggplot(data_reactive(), aes(x = target, y = Anciennete, fill = target)) + 
      geom_boxplot(width = 0.5, alpha = 0.9) + scale_fill_manual(values = viz_palette) + 
      theme_minimal() + labs(x="Churn", y="Mois d'ancienneté") + theme(legend.position = "none")
  })
  
  observeEvent(input$btn_predict, {
    req(modele_final)
    
    # 1. Création de l'individu (identique)
    indiv <- data.frame(
      Contrat = factor(input$input_contract, levels = c("Mensuel", "Annuel", "Bisannuel")),
      
      # Les levels doivent être IDENTIQUES aux choices de l'UI
      Service.Internet = factor(input$input_internet, levels = c("DSL", "Fibre optique", "Non")),
      
      Senior = factor(input$input_senior, levels = c("Non", "Oui")),
      Anciennete = input$input_tenure,
      charges.mensuelles = input$input_charges,
      Charges.totales = input$input_total,
      
      # Valeurs par défaut pour les autres colonnes
      Genre = factor("Homme", levels = c("Femme", "Homme")),
      Enfants = factor("Non", levels = c("Non", "Oui")),
      Partenaire = factor("Non", levels = c("Non", "Oui")),
      Multi.lignes = factor("Non", levels = c("Non", "Oui")),
      Autres.Services = factor("Non", levels = c("Non", "Oui")),
      Facturation.electronique = factor("Oui", levels = c("Non", "Oui")),
      Mode.de.paiement = factor("Carte bancaire", levels = c("Carte bancaire", "Virement bancaire", "Cheque electronique", "Cheque papier"))
    )
    
    tryCatch({
      # 2.Utiliser type = "prob"
      prob_df <- predict(modele_final, newdata = indiv, type = "prob")
      
      # Avec caret, prob_df est un tableau avec deux colonnes : 'Non' et 'Oui'
      # On récupère la colonne 'Oui' 
      score <- round(prob_df$Oui * 100, 2)
      
      # 3. Mise à jour de l'interface (identique)
      output$modern_res_ui <- renderUI({
        status_class <- if(score > 50) "high" else "low"
        div(class = paste("res-box", status_class),
            p(class = "score", paste0(score, "%")),
            span(style="color: #567C8D; font-weight: 600;", 
                 if(score > 50) "ALERTE : RISQUE DE DÉPART ÉLEVÉ" else "STABILITÉ : CLIENT FIDÈLE")
        )
      })
      
      output$txt_recommendation <- renderText({
        if(score > 50) "Risque critique. Une offre de rétention ou un appel de fidélisation est vivement conseillé." else "Profil stable. Aucune action particulière n'est requise."
      })
      
    }, error = function(e) { 
      showNotification(paste("Erreur de prédiction :", e$message), type = "error") 
    })
  })
}

shinyApp(ui, server)