library(shiny)
library(shinythemes)
library(ggplot2)
library(DT)
library(shinyWidgets)
library(dplyr)
library(cowplot)
library(patchwork)
library(shinydashboard)
library(gganimate)
library(flexdashboard)
library(leaflet)
library(sf)
library(scales)


setwd("~/Desktop/M2 SARADS/Data challenge 27nov-13dec/Bases de données")

#BVF_comm<-read.csv("DVF_stat_comm")
BVF_France<-read.csv("DVF_stat_France")
BVF_dep<-read.csv("DVF_stat_Dep")

#BVF_dep$Val.fonciere.maisons <-  as.numeric(BVF_dep$Val.fonciere.maisons)
#BVF_dep$Val.fonciere.appartements <-  as.numeric(BVF_dep$Val.fonciere.appartements)
# catnat.inondation<-read.csv("Analyse_evolution_inondation_dep.csv")
# catnat.mouvement<-read.csv("Analyse_evolution_Mouvement_terrain_dep.csv")
# catnat.secheresse<-read.csv("Analyse_evolution_Secheresse_dep.csv")
# catnat.seisme<-read.csv("Analyse_evolution_Seisme_dep.csv")
# catnat.TNG<-read.csv("Analyse_evolution_TNG_dep.csv")
# catnat.autres<-read.csv("Analyse_evolution_Autres_dep.csv")
# catnat.ChocMeca<-read.csv("Analyse_evolution_Choc_meca_dep.csv")
# catnat.Glissement<-read.csv("Analyse_evolution_Glissement_dep.csv")
# 
# catnat.inondation$risque<-"Inondation"
# catnat.mouvement$risque<-"Mouvement Terrain"
# catnat.secheresse$risque<-"Secheresse"
# catnat.seisme$risque<-"Seisme"
# catnat.TNG$risque<-"TNG"
# catnat.autres$risque<-"Autres"
# catnat.ChocMeca$risque<-"Choc Mecanique"
# catnat.Glissement$risque<-"Glissement/Effondrement/Eboulement"
# 
# catnat<-rbind(catnat.inondation,catnat.mouvement,catnat.secheresse,catnat.seisme,catnat.TNG,catnat.autres,catnat.ChocMeca,catnat.Glissement)

catnat <- read.csv("catnat1.csv")

#evolution
# evolution.catnat.inondation<-read.csv("Analyse_evolution_inondation_dep_annee.csv")
# evolution.catnat.mouvement<-read.csv("Analyse_evolution_Mouvement_terrain_dep_annee.csv")
# evolution.catnat.secheresse<-read.csv("Analyse_evolution_Secheresse_dep_annee.csv")
# evolution.catnat.seisme<-read.csv("Analyse_evolution_Seisme_dep_annee.csv")
# evolution.catnat.TNG<-read.csv("Analyse_evolution_TNG_dep_annee.csv")
# evolution.catnat.autres<-read.csv("Analyse_evolution_Autres_dep_annee.csv")
# evolution.catnat.ChocMeca<-read.csv("Analyse_evolution_Choc_meca_dep_annee.csv")
# evolution.catnat.Glissement<-read.csv("Analyse_evolution_Glissement_dep_annee.csv")
# 
# evolution.catnat.inondation$risque<-"Inondation"
# evolution.catnat.mouvement$risque<-"Mouvement Terrain"
# evolution.catnat.secheresse$risque<-"Secheresse"
# evolution.catnat.seisme$risque<-"Seisme"
# evolution.catnat.TNG$risque<-"TNG"
# evolution.catnat.autres$risque<-"Autres"
# evolution.catnat.ChocMeca$risque<-"Choc Mecanique"
# evolution.catnat.Glissement$risque<-"Glissement/Effondrement/Eboulement"
# 
# catnat2<-rbind(evolution.catnat.inondation,evolution.catnat.mouvement,evolution.catnat.secheresse,evolution.catnat.seisme,evolution.catnat.TNG,evolution.catnat.autres ,evolution.catnat.Glissement,evolution.catnat.ChocMeca)
catnat2 <- read.csv("catnat2.csv")

# Bases de données pour les cartes
BVF1 <- read.csv("BVF_retraitee.csv")
BVF1$departement <- substr(BVF1$codeGeo, 1, 2)
data <- BVF1
data_geo <- read_sf("georef-france-commune.geojson")
essai <- data_geo


idx <- match(data_geo$com_code, data$codeGeo)
concordance <- data[idx, "Valeur.fonciere"]
essai$valeur <- concordance
essai <- subset(essai, essai$com_area_code == "FXX")


quartiles <- quantile(essai$valeur, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
essai$quartile <- cut(essai$valeur, breaks = quartiles, 
                      labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE)

# Bases de données pour la carte des cat nat
catnatss <- read.csv("catnat_finale.csv")
catnatss <- catnatss %>%
  rename("Mouvement Terrain" = "Mouvement.Terrain",
         "Glissement" = "Glissement.Effondrement.Eboulement",
         "Choc Mecanique" = "Choc.Mecanique")


intfce_util <- dashboardPage(
  dashboardHeader(title = "Projet R-Shiny"),
  dashboardSidebar(
    selectInput(inputId = "catnat", label = "Sélectionner une catastrophe naturelle", choices = c("Toutes catastrophes naturelles", unique(catnat$risque)),selected="Toutes catastrophes naturelles"),
    selectInput(inputId = "dep", label = "Sélectionner un département", choices = c("France entière", unique(catnat$dep)), selected = "France entière"),
    numericInput("pourcentage", "Sélectionnez le pourcentage de dégât", min = 0, max = 100, value = 100, step = 10)
  ),
  

  
  dashboardBody(
    tabsetPanel(
      tabPanel(
        title = "Cartographie",
        # tabItem(
        #   tabName = "Cartes",
        tabItem(
          tabName = "Choix",
          fluidRow(infoBoxOutput("vbox_maisons", width=4),
                   infoBoxOutput("vbox_appartements", width=4),
                   infoBoxOutput("vbox_total", width=4)
          )
        ),
       fluidRow(
       column(
         width = 6,
         h4(style = "text-align: center;", "Valeur foncière"),
         plotOutput(outputId = "carte_val_foncière")
       ),
         column(
           width = 6,
           h4(style = "text-align: center;", "Catastrophes natuelles"),
           plotOutput(outputId = "carte_par_catnat")
         )
       )
  
      ),
      tabPanel(
        title = "Evolution",
        titlePanel(div(style = "text-align: center;", "")),
        tabItem(
          tabName = "Graphique",
          fluidRow(
            column(
              width = 6,
              h4(style = "text-align: center;", ""),
              plotOutput(outputId = "graph_1", height = "300px")),
            
            
            column(
              width = 6,
              h4(style = "text-align: center;", ""),
              plotOutput(outputId = "graph_2", height = "300px")),
            
            
            column(
              width = 6,
              h4(style = "text-align: center;", ""),
              plotOutput(outputId = "graph_4", height = "300px")),
            
            
            column(
              width = 6,
              h4(style = "text-align: center;", ""),
              plotOutput(outputId = "graph_5", height = "300px"))
            )))
      )
           
          )
)
 


serveur <- function(input, output, session){
  


  # graph_evolution vente maisons/ appart 
  x<-BVF_France
  output$graph_1 <- renderPlot({
    # Vérifier si x n'est pas vide
    if (input$dep=="France entière") {
      ggplot(x) +
        aes(x = Annee.mutation) + 
        geom_point(aes(y = ID.maisons, color = "Maisons")) +
        geom_line(aes(y = ID.maisons, color = "Maisons"), linetype = "solid") +
        geom_point(aes(y = ID.appartements, color = "Appartements")) +
        geom_line(aes(y = ID.appartements, color = "Appartements"), linetype = "solid") +
        labs(title = "Évolution du nombre de maisons et \n d'appartements vendus",
             x = "Année",
             y = "Nombre de ventes") +
        scale_color_manual(values = c("Maisons" = "blue", "Appartements" = "red"),
                           labels = c("Maisons", "Appartements")) +
        theme_minimal() +
        theme(axis.text = element_text(size = 15), legend.text = element_text(size = 12), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
    } else {
      x1<-subset(BVF_dep,dep== input$dep)
      ggplot(x1) +
        aes(x = Annee.mutation) + 
        geom_point(aes(y = ID.maisons, color = "Maisons")) +
        geom_line(aes(y = ID.maisons, color = "Maisons"), linetype = "solid") +
        geom_point(aes(y = ID.appartements, color = "Appartements")) +
        geom_line(aes(y = ID.appartements, color = "Appartements"), linetype = "solid") +
        labs(title = "Évolution du nombre de maisons et \n d'appartements vendus",
             x = "Année",
             y = "Nombre de ventes") +
        scale_color_manual(values = c("Maisons" = "blue", "Appartements" = "red"),
                           labels = c("Maisons", "Appartements")) +
        theme_minimal() +
        theme(axis.text = element_text(size = 15), legend.text = element_text(size = 12), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
    }
  })
  #graph_evolution valeurs foncieres maisons/appart
  output$graph_2 <- renderPlot({
    if (input$dep=="France entière") {
      ggplot(x) +
        aes(x = Annee.mutation) + 
        geom_point(aes(y = valeursfoncières.maisons.médian, color = "Maisons")) +
        geom_line(aes(y = valeursfoncières.maisons.médian, color = "Maisons"), linetype = "solid") +
        geom_point(aes(y = valeursfoncières.appartements.médian, color = "Appartements")) +
        geom_line(aes(y = valeursfoncières.appartements.médian, color = "Appartements"), linetype = "solid") +
        labs(title = "Évolution de la valeur foncière médian \n des maisons et d'appartements vendus",
             x = "Année",
             y = "Nombre de ventes") +
        scale_color_manual(values = c("Maisons" = "blue", "Appartements" = "red"),
                           labels = c("Maisons", "Appartements")) +
        theme_minimal() 
    } else {
      x1<-subset(BVF_dep,dep== input$dep)      
      ggplot(x1) +
        aes(x = Annee.mutation) + 
        geom_point(aes(y = valeursfoncières.maisons.médian, color = "Maisons")) +
        geom_line(aes(y = valeursfoncières.maisons.médian, color = "Maisons"), linetype = "solid") +
        geom_point(aes(y = valeursfoncières.appartements.médian, color = "Appartements")) +
        geom_line(aes(y = valeursfoncières.appartements.médian, color = "Appartements"), linetype = "solid") +
        labs(title = "Évolution de la valeur foncière médian \n des maisons et d'appartements vendus",
             x = "Année",
             y = "Nombre de ventes") +
        scale_color_manual(values = c("Maisons" = "blue", "Appartements" = "red"),
                           labels = c("Maisons", "Appartements")) +
        theme_minimal() 
    }
  })
  
  # graph_evolution prix metre carré median 
  # output$graph_3 <- renderPlot({
  #   # Vérifier si x n'est pas vide
  #   if (nrow(x) > 0) {
  #     ggplot(x) +
  #       aes(x = Annee.mutation) + 
  #       geom_point(aes(y = prix_m2.maisons.median, color = "Maisons")) +
  #       geom_line(aes(y = prix_m2.maisons.median, color = "Maisons"), linetype = "solid") +
  #       geom_point(aes(y = prix_m2.appartements.median, color = "Appartements")) +
  #       geom_line(aes(y = prix_m2.appartements.median, color = "Appartements"), linetype = "solid") +
  #       labs(title = "Évolution du prix au mètre carré médian \n des maisons et d'appartements vendus",
  #            x = "Année",
  #            y = "Nombre de ventes") +
  #       scale_color_manual(values = c("Maisons" = "blue", "Appartements" = "red"),
  #                          labels = c("Maisons", "Appartements")) +
  #       theme_minimal() +
  #       theme(axis.text = element_text(size = 15), legend.text = element_text(size = 12), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
  #   } else {
  #     # Si x est vide, afficher un message ou un graphique par défaut
  #     ggplot() + ggtitle("Aucune donnée disponible")
  #   }
  # })
  
  
  
  
  #   #graph_evolution nb catnat
  y<-catnat2

  output$graph_4 <- renderPlot({
    y4 <- subset(y, risque %in% input$catnat & departement == input$dep)
    # Vérifier si x n'est pas vide
    if (nrow(y4) > 0 &  input$dep != "France entière" & input$catnat != "Toutes catastrophes naturelles") {
      ggplot(y4) +
        aes(x = annee_deb) +
        geom_point(aes(y = nb.catnat)) +
        geom_line(aes(y = nb.catnat), linetype = "solid") +
        labs(title = "Évolution du nombre de catastophe naturelle",
             x = "Année",
             y = "Nombre de ventes") +
        theme_minimal()
    } else {
      # Si x est vide, afficher un message ou un graphique par défaut
      ggplot() + ggtitle("Aucune donnée disponible")
    }
  })

  # Graphe du nombre de cat nat par département
  agg_catnat <- catnat %>%
    group_by(risque) %>%
    summarise(nb.catnat = sum(nb.catnat))
  y5 <- catnat
  
  output$graph_5 <- renderPlot({
    if(input$dep!="France entière"){
      y <- subset(catnat, departement == input$dep)
      ggplot(catnat, aes(x = risque, y = nb.catnat)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = nb.catnat), vjust = -0.5, size = 3) +  
        labs(title = "Nombre de catastrophes par type de risque", x = "Type de catastrophe", y = "Nombre") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    ggplot(agg_catnat, aes(x = risque, y = nb.catnat)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = nb.catnat), vjust = -0.5, size = 3) +  
      labs(title = "Nombre de catastrophes par type de risque", x = "Type de catastrophe", y = "Nombre") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$carte_val_foncière <- renderPlot({
    essai <- data_geo
    idx <- match(data_geo$com_code, data$codeGeo)
    concordance <- data[idx, "Valeur.fonciere"]
    essai$valeur <- concordance
    essai <- subset(essai, essai$com_area_code=="FXX")
    if(input$dep!="France entière"){
      essai <- subset(essai, essai$dep_code==input$dep)
    }

    quartiles <- quantile(essai$valeur, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm=TRUE)
    essai$quartile <- cut(essai$valeur, breaks = quartiles, labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE)
    ggplot() +
      geom_sf(data = essai, aes(fill = quartile), color = NA) +
      scale_fill_manual(values = c("Q1" = "yellow", "Q2" = "orange", "Q3" = "red", "Q4" = "darkred")) +
      theme_void()
  })

     output$carte_par_catnat <- renderPlot({
       essai <- data_geo
       idx <- match(essai$com_code, catnatss$cod_commune)
       if(input$catnat!="Toutes catastrophes naturelles"){
         concordance <- catnatss[idx, input$catnat]
       } else {
         concordance <- catnatss[idx, "risque"]
       }
       concordance <- unlist(concordance)
       essai$valeur <- concordance
       essai <- subset(essai, essai$com_area_code=="FXX")
       if(input$dep!="France entière"){
         essai <- subset(essai, essai$dep_code==input$dep)
       }
    
       if(input$catnat=="Toutes catastrophes naturelles"){
         ggplot() +
           geom_sf(data = essai, aes(fill = valeur)) +
           scale_fill_manual(values = c("Inondation" = "cyan3", "Secheresse" = "orange", "Choc Mecanique" = "red", "Autres"="black", "Glissement"="yellow", "Mouvement Terrain"="darkolivegreen4", "Seisme"="purple", "TNG"="white")) +
           labs(fill = "risque") +
           theme_minimal()
       } else {
         ggplot() +
           geom_sf(data = essai, aes(fill = valeur,  color = "transparent"), color = NA) +
           scale_fill_gradient(name = "Fréquence", low = "seagreen1", high = "seagreen4") +
           theme_void()
       }
     })
    
    
    output$vbox_maisons <- renderInfoBox({
      #z <- subset(BVF_dep, dep== input$dep & risque %in% input$catnat)
      if(input$dep == "France entière"){
        infoBox("Valeur foncière maisons", paste0(number(round(sum(BVF_dep$valeursfoncières.maisons_sum) * input$pourcentage / 100)), " €"), icon = icon("home"), color="purple", fill=TRUE)
      } 
      else{
      z <- subset(BVF_dep, dep== input$dep)
      infoBox("Valeur foncière maisons", paste0(number(round(sum(z$valeursfoncières.maisons_sum) * input$pourcentage / 100)), " €"), icon = icon("home"), color="purple", fill=TRUE)
      }
      })
    
    output$vbox_appartements <- renderInfoBox({
      if(input$dep == "France entière"){
        infoBox("Valeur foncière appartements", paste0(number(round(sum(BVF_dep$valeursfoncières.appartements_sum) * input$pourcentage / 100)), " €"), icon = icon("building"), color="purple", fill=TRUE)
      } 
      else{
        z <- subset(BVF_dep, dep== input$dep)
        infoBox("Valeur foncière appartements", paste0(number(round(sum(z$valeursfoncières.appartements_sum) * input$pourcentage / 100)), " €"), icon = icon("building"), color="purple", fill=TRUE)
      }
    })
    
    output$vbox_total <- renderInfoBox({
      if(input$dep == "France entière"){
        infoBox("Valeur foncière totale", paste0(number(round(sum(BVF_dep$Valeur.fonciere) * input$pourcentage / 100)), " €"), icon = icon("city"), color="purple", fill=TRUE)
      } 
      else{
        z <- subset(BVF_dep, dep== input$dep)
        infoBox("Valeur foncière totale", paste0(round(sum(z$Valeur.fonciere) * input$pourcentage / 100)), icon = icon("city"), color="purple", fill=TRUE)
      }
    })
     
  
 
}

shinyApp(ui = intfce_util, server = serveur)
