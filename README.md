# Barathon MakeR - Générateur de Barathon

Cette application permet de générer, depuis une adresse de départ et une adresse d'arrivée, un itinéraire de barathon personnalisable selon différents critères (voir plus bas). Les données sont issues d'OpenStreetMap et les fonctionnalités de géocodage et de routage font appel à l'API gratuite OpenRouteService. Il est possible de créer une clé [à cette adresse](https://openrouteservice.org/), à insérer dans le fichier apikey.

⚠ L'API est limitée en nombre de requêtes journalières, et il est possible qu'elle atteigne la limite du quota si l'application est fortement sollicitée.

## Dépendance

La librairie R OpenRouteService n'étant pas disponible sur le dépôt du CRAN, il faut l'installer manuellement (et installer le package remotes si nécessaire):

```r
# install.packages("remotes")
remotes::install_github("GIScience/openrouteservice-r")
```

## Comment l'utiliser :

### Onglet "Création de l'itinéraire"

  * Renseignez l'adresse voulue, cliquez sur Rechercher et sélectionnez l'adresse.
  * Ajustez les temps de parcours, nombre de bars et type de parcours à votre convenance.
  * Cliquez sur Lancer pour faire apparaître la carte et l'itinéraire
    * Les étapes de l'itinéraire sont numérotées dans l'ordre
    * Il est possible que l'application affiche un message d'erreur si la zone est trop large ou trop fournie en bar. Dans ce cas, limitez le temps de parcours ou le nombre de bars.

### Onglet "Analyse de la tournée"

  * Le premier graphique présente le nombre de bars disponibles selon la distance au lieu de départ (autrement dit la distance la plus courte pour rentrer au point de départ depuis ce bar).
  * Le second graphique présente un profil topographique de l'itinéraire complet.
  
### Onglet "Téléchargement des données"

  * Permet de visualiser les données attributaires des bars du trajet et de les télécharger.
  
### Onglet "Et après le barathon..."

  * Ici sont présentés les fast-foods et les hotels à proximité du dernier bar.
  
Et pour finir : l'abus d'alcool est dangereux pour la santé, à consommer avec modération !
