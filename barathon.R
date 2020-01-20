# Barathon MakeR
# Authors:  Victor Bonnin
#           Jérémy Kalsron
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

# Clé d'API OpenRouteService
# Lue depuis la première ligne du fichier apikey.conf
ors_api_key(readLines("apikey.conf")[1])

# Interface
ui <- basicPage(

    titlePanel("Barathon MakeR"),

    # Widgets
    sidebarLayout(
        sidebarPanel(
            # UI d'entrée d'adresse
            uiOutput(
                "addrUi"
            ),
            # Slider de définition du temps de parcours
            sliderInput("min",
                        "Temps de parcours (mn)",
                        min = 1,
                        max = 30,
                        value = 15),
            # Slider du nombre de bars à prendre en compte
            sliderInput("nbar",
                        "Nombre de bars",
                        min = 2,
                        max = 10,
                        value = 6),
            # Liste de choix du type de parcours
            selectInput("typeparcours", "Type de parcours", 
                        choices = list("À pieds" = "walking", "Vélo" = "bike", "Voiture" = "car"), 
                        selected = 1),
            # Bouton de lancement de la requête
            actionButton("lancer", "Lancer")
        ),

        # Carte
        mainPanel(
           leafletOutput("carte")
        )
    )
    
)

# UI de recherche de l'adresse
ui_addrRech = function() {
    return(
        renderUI({
            tagList(
                textInput("addr", "Adresse", "15 Parvis René Descartes Lyon"),
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
        )})
    )
}

# Serveur
server <- function(input, output, session) {
    
    # Affichage de l'UI de recherche d'adresse au lancement
    output$addrUi = ui_addrRech()
    
    # En cliquant sur rechercher, on change l'UI en une liste affichant les 5 premiers résultats de la recherche d'adresse
    observeEvent(input$rechercher, {
        output$addrUi = ui_addrList(input$addr)
    })
    
    # En cliquant sur effacer, on réaffiche le champ de recherche
    observeEvent(input$effacer, {
        output$addrUi = ui_addrRech()
    })
    
    # En cliquant sur lancer, on exécute la requête
    observeEvent(input$lancer, {
        output$carte = renderLeaflet({
            c = ors_geocode(input$addrl, size=1, output="sf")
            coordinates = 
                data.frame(lon = c(st_coordinates(c)[1]), lat = c(st_coordinates(c)[2]))
            res <- ors_isochrones(coordinates, range = input$min * 60, interval = input$min * 60, ors_profile(input$typeparcours))
            leaflet(res) %>%
                addTiles() %>%
                addGeoJSON(res) %>%
                fitBBox(res$bbox)
        })
    })

}

shinyApp(ui = ui, server = server)
