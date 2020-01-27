# Barathon MakeR
# Authors:  Victor Bonnin
#           Jérémy Kalsron
#
# Analyse basée sur l'API OpenRouteService
# https://openrouteservice.org/
# Librairie R associée :
# https://giscience.github.io/openrouteservice-r/articles/openrouteservice.html
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

library(shiny)
library(dqshiny)
library(openrouteservice)
library(sf)
library(leaflet)
library(htmltools)
library(ggplot2)
library(ggforce)
library(units)
library(dplyr)
library(stringr)
library(reshape2)
library(geojsonsf)
library(lwgeom)

# Clé d'API OpenRouteService
ors_api_key(readLines("apikey")[1])

#
# Interface client
#

ui <- fluidPage(
  
  titlePanel("Barathon MakeR"),

  # Widgets
  tabsetPanel(
    tabPanel(
      "Création de l'itinéraire",
      titlePanel(""),
      sidebarLayout(
        sidebarPanel(
          
          # UI d'entrée d'adresse
          uiOutput("addrUi"),
          hr(),
          # Slider de définition du temps de parcours
          uiOutput("parcoursUi"),
          # sliderInput("min",
          #             "Temps de parcours (mn)",
          #             min = 1,
          #             max = 30,
          #             value = 15),
          # Slider du nombre de bars à prendre en compte pour le barathon
          sliderInput("nbar",
                      "Nombre de bars",
                      min = 2,
                      max = 10,
                      value = 6),
          # Liste de choix du type de parcours
          selectInput("typeparcours", "Type de parcours", 
                      choices = list("À pieds" = "walking", "À vélo" = "bike"), 
                      selected = 1),
          # Bouton de lancement de la requête
          actionButton("lancer", "Lancer")
          
        ),
        # Carte
        mainPanel(
          textOutput("message"),
          leafletOutput("carte", width = "100%")
        )
      )
    ),
    tabPanel(
      "Analyse de la tournée",
      titlePanel(""),
      mainPanel(
        column(12, align="center", plotOutput(outputId="graphe", width = "100%"), br(), br(), plotOutput(outputId="graphe2", width="110%"))
      )
    ),
    tabPanel(
      "Téléchargement des données",
      titlePanel(""),
      mainPanel(
        column(12, align="center", dataTableOutput('table'), downloadButton("downloadData", "Download"))
      )
    ),
    tabPanel(
      "Et après le barathon...",
      titlePanel("Une petite faim ? Un peu fatigué ?"),
      mainPanel(
        "Ce dernier onglet permet de trouver les fast-foods et hôtels à proximité immédiate du dernier bar visité (rayon de 1000m) ! On pense à tout, on pense à vous !"
      ),
      br(),br(),
      mainPanel(
        align="center", " ;-)",
        br(),br(),br(),br(),
        fluidRow(
          textOutput("message2"),
          splitLayout(
            cellWidths = c("50%", "50%"),
            leafletOutput("fast_food", width = 280, height = 400),
            leafletOutput("hotel", width = 280, height = 400)
          ),
          br(),br(),br(),br(),br(), 
          mainPanel("Si il manque des informations, n'hésitez pas à aller contribuer sur OSM"), 
          br(), 
          a(href="https://www.openstreetmap.org/", "OSM")
        )
      )
    )
  )
)

#
# Serveur
#

# UI de recherche de l'adresse
ui_addrRech = function(addr) {
  return(
    renderUI({
      tagList(
        textInput("addr", "Adresse", addr),
        actionButton("rechercher", "Rechercher")
      )
    })
  )
}

# UI de liste des adresses
ui_addrList = function(addr) {
  return(
    renderUI({
      tagList(
        selectInput("addrl", label="Adresse", c(ors_geocode(addr, size=5, output="sf")$label)),
        actionButton("effacer", "Effacer")
      )
    })
  )
}

# Slider pour le parcours à vélo limité à 10 mn
ui_parcoursVelo = function() {
  return(
    renderUI({
      sliderInput(
        "min",
        "Temps de parcours (mn)",
        min = 1,
        max = 10,
        value = 5
      )
    })
  )
}

# Slider pour le parcours à piedslimité à 30 mn
ui_parcoursPieds = function() {
  return(
    renderUI({
      sliderInput("min",
        "Temps de parcours (mn)",
        min = 1,
        max = 30,
        value = 15)
    })
  )
}

repairTags = function(pois) {
  tags = c()
  for(tag in pois$osm_tags) {
    if(is.list(tag)) {
      tag = tag$name
    }
    if(is.null(tag)) {
      tag = "Pas de nom renseigné"
    }
    tags = c(tags, tag)
  }
  pois$osm_tags = tags
  return(pois)
}

# Fonction de génération de l'itinéraire de proche en proche (bar)
conceptionItineraire = function(matrix, max) {
  itineraire = c(1)
  while(length(itineraire) < max+1) {
    last = itineraire[length(itineraire)]
    mdistance = matrix %>% filter(depart == last, !(arrivee %in% itineraire))
    if(nrow(mdistance) == 0) {
      break
    }
    bar = slice(mdistance, which.min(distance))$arrivee
    itineraire = c(itineraire, bar)
  }
  return(itineraire)
}

server <- function(input, output, session) {
  
  # Affichage de l'UI de recherche d'adresse au lancement
  output$addrUi = ui_addrRech("15 Parvis René Descartes Lyon")
  
  # Changement du slider de temps de parcours selon le type de trajet
  observeEvent(input$typeparcours, {
    if(input$typeparcours == "walking") {
      output$parcoursUi = ui_parcoursPieds()
    } else {
      output$parcoursUi = ui_parcoursVelo()
    }
  })
  
  # En cliquant sur rechercher, on change l'UI en une liste affichant les 5 premiers résultats de la recherche d'adresse (seulement 5 pour ne pas dépsser les quotas de l'API)
  observeEvent(input$rechercher, {
    if(str_length(str_trim(input$addr)) > 0)
      output$addrUi = ui_addrList(input$addr)
  })
  
  # En cliquant sur effacer, on réaffiche le champ de recherche
  observeEvent(input$effacer, {
    output$addrUi = ui_addrRech("")
  })
  
  # En cliquant sur lancer, on exécute la requête
  observeEvent(input$lancer, {
    
    # Nettoyage des outputs
    output$message = NULL
    output$message2 = NULL
    output$graphe = NULL
    output$graphe2 = NULL
    output$table = NULL
    output$downloadData = NULL
    output$carte = NULL
    output$fast_food = NULL
    output$hotel = NULL
    
    # Définition du message d'erreur par défaut
    message = "Erreur inconnue :("
    
    tryCatch({
      # Récupération des coordonnées de l'adresse entrée
      message = "Veuillez entrer et sélectionner une adresse !"
      c = ors_geocode(isolate(input$addrl), size=1, output="sf")
      coordinates = 
        data.frame(lon = c(st_coordinates(c)[1]), lat = c(st_coordinates(c)[2]))
      res <- ors_isochrones(coordinates, range = isolate(input$min) * 60, interval = isolate(input$min) * 60, ors_profile(isolate(input$typeparcours)))
      message = "Oh non, il n'y a pas de bar dans le périmètre ! Ce n'est sûrement pas le bon endroit pour faire un barathon : ( !"
      pois = ors_pois(
        request = 'pois',
        geometry = list(
          geojson = list(
            type = "Polygon",
            coordinates = res$features[[1]]$geometry$coordinates
          )
        ),
        filters = list(
          category_ids = 561
        ),
        output = "sf"
      )
      
      message = "Oups ! Il y a sûrement trop de bars dans cette zone !"
      # Calcul et formatage de la matrice des distances (entre le points de départ et les bars)
      hmatrice <- ors_matrix(rbind(st_coordinates(c), st_coordinates(pois)), metrics = c("duration", "distance"), units = "km", ors_profile(isolate(input$typeparcours)))$distances # Génération de la matrice des distances
      matrice = melt(hmatrice)
      colnames(matrice) = c("depart", "arrivee", "distance")
      
      depart = c %>% mutate(osm_type = 0, osm_id = 0, distance = 0, osm_tags = "Départ", category_ids = NA) %>% select(osm_type, osm_id, distance, osm_tags, category_ids, geometry)
      pois = rbind(depart, pois)
      
      pois = repairTags(pois)
      
      nLigneItineraire = conceptionItineraire(matrice, input$nbar) # Mobilisation de la fonction pour recherche des bars de proche en proche
      pois_itineraire = slice(pois, nLigneItineraire)
      pois_itineraire$distDep = hmatrice[nLigneItineraire,1]
      
      # Préparation des listes de données pour les graphiques et la génération de l'itinéraire
      bars_itineraire = pois_itineraire[-1,]
      bars = pois[-1,]
      
      # Préparation pour le graphique (nombre de bars en fonction de la distance à parcourir depuis mon lieu de départ)
      moy<-mean(bars_itineraire$distDep)
      etiquette <- paste("Moyenne distance :", round(moy, 2), "km")
      
      # Génération du graphique (nombre de bars en fonction de la distance à parcourir depuis mon lieu de départ)
      output$graphe<- renderPlot({ggplot(bars_itineraire, aes(x=distDep))+
          geom_histogram(color="black", bins=10, fill="#78A7AA")  +
          ggtitle("Nombre de bars disponibles en fonction de la distance à parcourir \n (depuis mon lieu de départ avec le nombre de bars choisis)") +
          xlab("Distance [km]") + ylab("Nombre de bars") +
          theme(plot.title = element_text(hjust = 0.5)) + theme(
            plot.title = element_text(color="black", size=14, face="bold.italic"))+
          geom_vline(aes(xintercept=moy), colour="red",linetype="dashed")+
          geom_text(mapping = aes(x = moy,
                                  y = 0,
                                  label =  etiquette,
                                  hjust = -1.2,
                                  vjust = -1,
                                  angle = 90),
                    data = bars_itineraire)
      })
      
      # Affichage du profil topographique de l'itinéraire réalisé
      output$graphe2<- renderPlot ({ 
        x <- ors_directions(st_coordinates(pois_itineraire), ors_profile(isolate(input$typeparcours)), elevation = TRUE, extra_info = "steepness", format = "geojson")
        # Pirouette pour permettre un fonctionnement sous Linux et ShinyApps
        xdf = data.frame(x["features"][[1]][[1]]["geometry"][[1]]["coordinates"]) %>% select(X = coordinates.1, Y=coordinates.2, Z=coordinates.3)
        xdf$wkt = paste("POINT(",xdf$X,xdf$Y,")")
        xsf = st_as_sf(xdf, coords=c("X", "Y", "Z"), crs=4326)
        height <- xdf$Z
        points <- st_cast(st_geometry(xsf), "POINT")
        n <- length(points)
        segments <- cumsum(st_distance(points[-n], points[-1], by_element = TRUE))
        # Calcul des pentes 
        steepness <- x["features"][[1]][[1]]["properties"][[1]]["extras"][[1]]["steepness"][[1]]["values"][[1]]
        steepness <- rep(steepness[,3], steepness[,2]-steepness[,1])
        steepness <- factor(steepness, -5:5)
        palette = setNames(rev(RColorBrewer::brewer.pal(11, "RdYlBu")), levels(steepness))
        units(height) <- as_units("m")
        pois_df3 <- data.frame(x1 = c(set_units(0, "km"), segments[-(n-1)]),
                         x2 = segments,
                         y1 = height[-n],
                         y2 = height[-1],
                         steepness)
        y_ran = range(height) * c(0.9, 1.1)
        n = n-1
        df2 = data.frame(x = c(pois_df3$x1, pois_df3$x2, pois_df3$x2, pois_df3$x1),
                         y = c(rep(y_ran[1], 2*n), pois_df3$y2, pois_df3$y1),
                         steepness,
                         id = 1:n)
        # Génération du graphique (profil topo) avec ggplot
        ggplot() + theme_bw() +
          geom_segment(data = pois_df3, aes(x1, y1, xend = x2, yend = y2), size = 1) +
          geom_polygon(data = df2, aes(x, y, group = id), fill = "white") +
          geom_polygon(data = df2, aes(x, y , group = id, fill = steepness)) +
          scale_fill_manual(values = alpha(palette, 0.8), drop = FALSE) +
          scale_x_unit(unit = "km", expand = c(0,0)) +
          scale_y_unit(expand = c(0,0), limits = y_ran) +
          labs(x = "Distance", y = "Altitude") + 
          ggtitle("Profil topographique de l'itinéraire") +
          theme(plot.title = element_text(hjust = 0.5)) + 
          theme(plot.title = element_text(color="black", size=14, face="bold.italic"))  +
          labs(fill="Pente")
          
      })
      
      # Affichage des informations précises sur les bars (affichage de la table)
      data_exp <- select(bars_itineraire, osm_id,	osm_type,	distDep,	category_ids,	osm_tags)
      data_export<- apply(data_exp,2,as.character)
      stt<-st_coordinates(bars_itineraire)
      data2<-cbind(data_export, stt)
      
      output$table <- renderDataTable(data_exp)
      
      # Téléchargement des résultats (bars du barathon au format .CSV)
      output$downloadData <- output$downloadData <- downloadHandler(
        filename = function() {
          paste('data_export-', Sys.Date(), '.csv', sep='')
        },
        content = function(file) {
          write.csv(data2, file)
        }
      )

      # Génération de l'itinéraire
      itineraire <- ors_directions(st_coordinates(pois_itineraire), ors_profile(isolate(input$typeparcours)))
      
      # Carte principale pour le barathon      
      
      # Définition de l'icone pour les bars (biere)
      beer_icon <- makeIcon(
        iconUrl = "https://image.flaticon.com/icons/png/512/110/110181.png",
        iconWidth = 30, iconHeight = 30)
      
      # Définition de la symbologie pour les points de passage (couleur et numéro)
      labels <- (1:nrow(bars_itineraire))
      markers <- awesomeIcons(markerColor = "green", text = labels, fontFamily = "arial")

      # Assemblage carte leaflet
      output$carte = renderLeaflet({leaflet(res) %>%
        addTiles("http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png") %>%
        addGeoJSON(itineraire, fill=FALSE, group="Itinéraire") %>%
        addMarkers(data= bars, icon = beer_icon, popup = ~htmlEscape(osm_tags), group ='Bars') %>%
        addAwesomeMarkers(data = pois_itineraire[-1,], icon = markers, group ='Bars du barathon')%>%
        addAwesomeMarkers(data = c, icon = awesomeIcons("home"), group="Point de départ") %>%
        addGeoJSON(res, color = "#cc00cc", opacity = 0.30, fill = FALSE, group ='Espace de recherche') %>%
        addLayersControl(
          overlayGroups = c("Bars", "Espace de recherche", "Bars du barathon", "Point de départ", "Itinéraire")) %>%
        addScaleBar( position = c("bottomleft"))})
      
      message = "X"
      # Carte pour l'onglet "Et après le barathon..."
      last_bar<-tail(pois_itineraire, n=1) # Recherche du dernier bar visité lors du barathon   
        
      #Recherche des fast-foods à proximité qui seraient encore ouvert tard le soir...
      tampon_ff <- list(
        geojson = list(
          type = "Point",
          coordinates = c(st_coordinates(last_bar))
        ),
        buffer = 1000
      )
      food<-ors_pois(
        request = 'pois',
        geometry = tampon_ff,
        limit = 10,
        sortby = "distance",
        filters = list(
          category_ids = 566
        ),
        output = "sf"
      )
      food = repairTags(food)
      
      # Définition de l'icone pour les fast-food (hamburger)
      fastfood_icon <- makeIcon(
        iconUrl = "http://cdn.onlinewebfonts.com/svg/img_478419.png",
        iconWidth = 30, iconHeight = 30)
      
      # Affichage de la carte
      output$fast_food = renderLeaflet({leaflet(res) %>%
          addTiles("http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png") %>%
          addAwesomeMarkers(data = last_bar, icon = awesomeIcons("flag"), group="Dernier bar") %>%
          addMarkers(data= food, icon = fastfood_icon, popup = ~htmlEscape(osm_tags), group ='Fast-food')  %>%
        addLayersControl(
          overlayGroups = c("Dernier bar", "Fast-food")) %>%
        addScaleBar( position = c("bottomleft"))})
      
      # Recherche des hotels à proximité pour aller dormir...
      tampon_h <- list(
        geojson = list(
          type = "Point",
          coordinates = c(st_coordinates(last_bar))
        ),
        buffer = 1000
      )
      hotel<-ors_pois(
        request = 'pois',
        geometry = tampon_h,
        limit = 10,
        sortby = "distance",
        filters = list(
          category_ids = 108
        ),
        output = "sf"
      )
      hotel=repairTags(hotel)
      
      # Définition de l'icone pour les hôtels (lit)
      hotel_icon <- makeIcon(
        iconUrl = "https://cdn.pixabay.com/photo/2014/03/25/16/58/sleeping-297724_960_720.png",
        iconWidth = 30, iconHeight = 30)
      
      # Affichage de la carte
      output$hotel = renderLeaflet({leaflet(res) %>%
          addTiles("http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png") %>%
          addAwesomeMarkers(data = last_bar, icon = awesomeIcons("flag"), group="Dernier bar") %>%
          addMarkers(data= hotel, icon = hotel_icon, popup = ~htmlEscape(osm_tags), group ='Hôtel') %>%
          addLayersControl(
            overlayGroups = c("Dernier bar", "Hôtel")) %>%
          addScaleBar( position = c("bottomleft"))})
    
      # S'il y a une erreur quelconque, on affiche un message
      },error=function(e) {
         if(message == "X") {
           output$message2 = renderText({"Il n'y a malheureusement pas toutes les commodités ici :( ."})
         } else {
           output$message = renderText({message})
         }
      }
    )
  })   
}

shinyApp(ui = ui, server = server)