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
library(shinysky)
library(openrouteservice)
library(sf)
library(leaflet)

# Clé d'API OpenRouteService
# Lue depuis la première ligne du fichier apikey.conf
ors_api_key(readLines("apikey.conf")[1])

# Interface
ui <- basicPage(

    titlePanel("Barathon Maker"),

    # Widgets
    sidebarLayout(
        sidebarPanel(
            textInput("addr", "Adresse", "15 Parvis René Descartes Lyon"),
            sliderInput("min",
                        "Temps de parcours:",
                        min = 1,
                        max = 60,
                        value = 5),
            selectInput("typeparcours", "Type de parcours", 
                        choices = list("À pieds" = "walking", "Vélo" = "bike", "Voiture" = "car"), 
                        selected = 1),
            submitButton(text = "Rafraîchir", icon("refresh")),
        ),

        # Carte
        mainPanel(
           leafletOutput("carte")
        )
    )
    
)

# Serveur
server <- function(input, output) {
    
    output$carte = renderLeaflet({
        c = ors_geocode(input$addr, size=1, output="sf")
        coordinates <- 
            data.frame(lon = c(st_coordinates(c)[1]), lat = c(st_coordinates(c)[2]))
        res <- ors_isochrones(coordinates, range = input$min * 60, interval = input$min * 60, ors_profile(input$typeparcours))
        leaflet(res) %>%
            addTiles() %>%
            addGeoJSON(res) %>%
            fitBBox(res$bbox)
    })
    
}

shinyApp(ui = ui, server = server)
