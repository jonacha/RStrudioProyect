rm(list = ls());cat("\014")
# (incluya aquÃ cualquier librerÃa a utilizar)
#---------------------------------------------------------------------------

library(ggmap)
library(leaflet)

puntos = data.frame(localizacion = c("Plaza Nueva Bilbao",
                                     "Paris",
                                     "Roma"), stringsAsFactors = F)

# api de google maps (2500 llamadas)

geocodeQueryCheck(userType = "free")

#Sleep con timeout para muchos datos
coord = geocode(puntos$localizacion)

puntos$lat = coord$lat
puntos$lon = coord$lon

leaflet(puntos) %>% addTiles() %>%
  addMarkers(puntos$lon, puntos$lat, popup = puntos$localizacion)

distancias = mapdist(puntos$localizacion[2], puntos$localizacion[3], mode="driving")

rute = route(puntos$localizacion[2], puntos$localizacion[3], structure="route")

leaflet(puntos) %>% addTiles() %>%
  addAwesomeMarkers(rute$lon, rute$lat) %>%
  addPolylines(rute$lon, rute$lat)
