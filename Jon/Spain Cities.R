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
#class(Ciudades)
ContCiudades=  nrow(Ciudades)
cordenadas <- geocode(Ciudades$Ciudades) 
geocodeQueryCheck(userType = "free")


distancias=matrix(0,ContCiudades,ContCiudades)
tiempo=matrix(0,ContCiudades,ContCiudades)


fallos=matrix(0, 2704, 2, byrow=F)
cnames <-c("Ciudad Origen", "Ciudad Destino")
colnames(fallos) <- cnames


nfallos=1;
for(i in 1:ContCiudades){
  for(j in i:ContCiudades){
    if(j==1){
      j=i;
    }
    else{
    if(i!=j){
      datos = mapdist(Ciudades$Ciudades[i], Ciudades$Ciudades[j], mode="driving")
    a=datos[,3]
    if( is.na(a)){
    fallos[nfallos,1]=datos[,1]
    fallos[nfallos,2]=datos[,2]
    distancias[i,j]=0
    tiempo[i,j]= 0
    nfallos=nfallos+1;
    }else{
      distancias[i,j]= datos[,4]
      tiempo[i,j]= datos[,7]
    }
    }
    }
    
  }
}
write.xlsx(distancias, "C:/Proyecto/RStrudioProyect/Jon/DistanciasCuidades.xlsx") 
write.xlsx(tiempo, "C:/Proyecto/RStrudioProyect/Jon/TiemposCiudades.xlsx") 

a=distancias
b=tiempo;
class(a)
for(i in 1:ContCiudades){
  a[1:(52-i),i]=distancias[i,1:(52-i)]
  b[1:(52-i),i]=tiempo[i,1:(52-i)]
  
}

for(i in 1:ContCiudades){
  a[i,1:i]=distancias[1:i,i]
  b[i,1:i]=tiempo[1:i,i]
  
}


a[1:52,1]
a[1:52,1]=distancias[1,1:52]
distancias[1,1:52]
#Santa+Cruz+de+Tenerife&mode=driving&sensor=false
#matching was not perfect, returning what was found.
#Error in `*tmp*`[[c(1, 1)]] : no such index at level 1
#(userType = "free")
#coord = geocode(puntos$localizacion)
#puntos$lat = coord$lat
#puntos$lon = coord$lon


#Zona de pruevas----------------------------------------------------------------------------------
if(datos[,3]==N)  
  datos[complete.cases(datos), ]  
complete.cases()
if(is.data.frame(datos) && nrow(datos)==0){
  datos[,7]=0;
}

puntos = data.frame(localizacion = c("Barcelona",
                                     "bilbao",
                                     "Albacete","Centro Comercial Ferial Plaza, Guadalajara","Catedral de Leon","Tenerife","Parque De Santa Ana, Cuenca","Museo Maritimo del Cantabrico,Santander"), stringsAsFactors = F)

ad = mapdist("Barcelona", "bilbao", mode="driving", output = c( "all"), messaging = FALSE, sensor = FALSE, language = "en-EN", override_limit = FALSE)
a = mapdist(puntos$localizacion[3], puntos$localizacion[2], mode="driving")
#a = mapdist(puntos$localizacion[4],puntos$localizacion[2],mode="driving")
#a = mapdist(puntos$localizacion[5],puntos$localizacion[2],mode="driving")
a = mapdist(puntos$localizacion[6], puntos$localizacion[2], mode="driving")
a = mapdist(puntos$localizacion[7], puntos$localizacion[2], mode="driving")
a = mapdist(puntos$localizacion[8], puntos$localizacion[2], mode="driving")
geocodeQueryCheck(userType = "free")
cordenadas
cordenadas<-  geocode("Melilla")
cordenadas=0;

puntos = data.frame(localizacion = c("Plaza Nueva Bilbao",
                                     "Paris",
                                     "Roma"), stringsAsFactors = F)

d = mapdist(puntos$localizacion[2], puntos$localizacion[3], mode="driving")
rute = route(puntos$localizacion[2], puntos$localizacion[3], structure="route")
#Antiguo

leaflet(Ciudades) %>% addTiles() %>%
  addMarkers(Ciudades$lon, Ciudades$lat, popup = Ciudades$Ciudades)



Recorridos[1:52][2] = Ciudades[, 1]
numFilas=0
Recorridos[numFilas][1]=Ciudadesorigen
Ciudades$lat = cordenadas$lat
Ciudades$lon = cordenadas$lon
Ciudades$cordenadas.lat
coord$lon 


#for(i in 1:ContCiudades){
# Ciudadesorigen=Ciudades[i,1]
# for(j in 1:ContCiudades){

#   CiudadDestino=Ciudades[j,1]
#   Recorridos[numFilas][1]=Ciudadesorigen
#   Recorridos[j*52:52][2]=CiudadDestino
#   a=proc.time()-t[1]
#    numFilas=numFilas+1
# }
# numFilas=numFilas+1
#}

Recorridos=matrix("NA",ContCiudades*ContCiudades,5)
#colnames(Recorridos)
class(Recorridos)
cnames <-c("Ciudad Origen", "Ciudad Destino", "Tiempo", "Gasolina", "Precio")
colnames(Recorridos) <- cnames
#Antiguo -------------------------------------------------------------------------------------
rute = route(puntos$localizacion[2], puntos$localizacion[3], structure="route")

leaflet(puntos) %>% addTiles() %>%
  addAwesomeMarkers(rute$lon, rute$lat) %>%
  addPolylines(rute$lon, rute$lat)
