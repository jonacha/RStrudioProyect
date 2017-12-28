#Nombre: Asier Salsidua y Jon Acha
#Asignatura:Inteligencia Artificial Avanzada
#Proyecto: Obtener el recorrido mas optimo recorriendo todas las ciudades de España
#Limpiar datos
rm(list = ls());cat("\014")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
library(GGally)
library(ggplot2)
library(gridExtra)

#Librerias
#install.packages("ggmap", dep = T)
library(ggmap)
#install.packages("leaflet", dep = T)
library(leaflet)
#install.packages("GGally", dep = T)
library(GGally)
library(ggplot2)
library(gridExtra)
#install.packages("xlsx", dep = T)
library(xlsx)
#----------------------------------------------------------------------------------------

Ciudades <- read.xlsx("CiudadesEspaña.xlsx", sheetIndex = 1,stringsAsFactors = F)
Ciudades
class(Ciudades)
Ciudadesorigen = Ciudades[52,1]
CiudadDestino=Ciudades[1,1]
numFilas=1;
ContCiudades=  nrow(Ciudades)
Recorridos=matrix("NA",ContCiudades*ContCiudades,5)
#colnames(Recorridos)
class(Recorridos)
cnames <-c("Ciudad Origen", "Ciudad Destino", "Tiempo", "Gasolina", "Precio")
colnames(Recorridos) <- cnames

for(i in 1:ContCiudades){
  Ciudadesorigen=Ciudades[i,1]
  for(j in 1:ContCiudades){
  
    t <- proc.time()
    CiudadDestino=Ciudades[j,1]
    Recorridos[numFilas][1]=Ciudadesorigen
    Recorridos[numFilas][2]=CiudadDestino
    a=proc.time()-t[1]
    numFilas=numFilas+1
  }
  numFilas=numFilas+1
}
numFilas=0
Recorridos[numFilas][1]=Ciudadesorigen

cordenadas <- geocode(Ciudades$Ciudades) 


Ciudades$lat = cordenadas$lat
Ciudades$lon = cordenadas$lon

geocodeQueryCheck(userType = "free")
  cordenadas
cordenadas<-  geocode("lleida")
  
  
Ciudades$cordenadas.lat
coord$lon 
  
  
  
  
geocodeQueryCheck(userType = "free")
coord = geocode(Ciudades$Ciudades)

#Antiguo
puntos = data.frame(localizacion = c("Vitoria",
                                     "Albacete",
                                     "Roma"), stringsAsFactors = F)
geocodeQueryCheck(userType = "free")
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
