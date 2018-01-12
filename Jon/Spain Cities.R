#Nombre: Asier Salsidua y Jon Acha
#Asignatura:Inteligencia Artificial Avanzada
#Proyecto: Obtener el recorrido mas optimo recorriendo todas las ciudades de España
#Limpiar datos


rm(list = ls());cat("\014")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#♠ Inicio Paso 1 - Librerias
#install.packages("ggmap", dep = T)
library(ggmap)
#install.packages("leaflet", dep = T)
library(leaflet)
#install.packages("GGally", dep = T)
library(GGally)
#install.packages("ggplot2", dep = T)
library(ggplot2)
#install.packages("gridExtra", dep = T)
library(gridExtra)
#install.packages("xlsx", dep = T)
library(xlsx)
#♠ Fin Paso 1
#♠ Inicio Paso 2 Lectura de las ciudades con sus cordenadas
Ciudades <- read.xlsx("CiudadesEspaña.xlsx", sheetIndex = 1,stringsAsFactors = F)[,-1]
ContCiudades=  nrow(Ciudades)
#♠ Fin del paso 2

#♠ Inicion Paso 3 Sacar las cordenadas de las ciudades. Sacar al paso 7 si ya se ha probado el correcto funcionamiento 
#de la estracion de datos
sacarCordenadasCiudades=function(Ciudades){
  cordenadas <- geocode(Ciudades$Ciudades) 
  for (ciudad in 1:ContCiudades) {
   while(is.na(cordenadas[ciudad,1])){
     cordenadas[ciudad,] <- geocode(Ciudades[ciudad,1])
     geocodeQueryCheck(userType = "free")
   }
  }
  Ciudades$lat = cordenadas$lat
  Ciudades$lon = cordenadas$lon
  return(cordenadas)
}
cordenadas=sacarCordenadasCiudades(Ciudades)
#♠ Fin de paso 3 

#♠ Inicio Paso 4 inicializacion de las matrices de distancias y tiempo entre ciudades
distancias=matrix(0,ContCiudades,ContCiudades)
tiempo=matrix(0,ContCiudades,ContCiudades)
fallos=matrix(0, 2704, 2, byrow=F)
cnames <-c("Ciudad Origen", "Ciudad Destino")
colnames(fallos) <- cnames
#♠ Fin Paso 4

#♠ Inicio Paso 5 Sacar las distacias y el tiempo entre las ciudades 
nfallos=1;
for(i in 1:ContCiudades){
  for(j in i:ContCiudades){
    if(j==1){
      j=i;
    }
    else{
    if(i!=j){
      repeat{
        datos = mapdist(Ciudades$Ciudades[i], Ciudades$Ciudades[j], mode="driving")
        if(!is.na(datos[,4])){
          break
        }
      }

      distancias[i,j]= datos[,4]
      distancias[j,i]= datos[,4]
      tiempo[i,j]= datos[,7]
      tiempo[j,i]= datos[,7]
    }
    }
    
  }
}
#♠ Fin de Paso 5

#♠ Inicio de Paso 6 escritura  de los datos  
write.xlsx(distancias, "DistanciasCuidades.xlsx") 
write.xlsx(tiempo, "TiemposCiudades.xlsx") 
write.xlsx(fallos, "ciudadesQueFallan.xlsx")
write.xlsx(Ciudades, "CiudadesEspaña.xlsx") 
#♠ Fin de Paso 6

#♠ Inicio de Paso 7 lectura de los xlsx con los datos para que no se tengan que sacar los datos otra vez
distancias <- read.xlsx("DistanciasCuidades.xlsx", sheetIndex = 1,stringsAsFactors = F)[,-1]
tiempo <- read.xlsx("TiemposCiudades.xlsx", sheetIndex = 1,stringsAsFactors = F)[,-1]
fallos <- read.xlsx("ciudadesQueFallan.xlsx", sheetIndex = 1,stringsAsFactors = F)[,-1]
#♠ Fin de Paso 7

#♠ Inicio de Paso 8 Metodo para sacar la distancia total del recorrido entre las ciudades
fitness = function(individuos, distancias){
  suma = 0;
  for(i in 1:(length(individuos)-1)){
    suma = suma + distancias[individuos[i],individuos[i+1]]
  }
  suma = suma + distancias[individuos[length(individuos)],individuos[1]]
  return(suma)
}
#♠ Fin de Paso 8

#♠ Inicio de Paso 9 MEtodo para sacar el tiempo total en lo que se tarda en sacar el recorrido entre las ciudades
timeness= function(individuos, tiempo){
  suma = 0;
  for(i in 1:(length(individuos)-1)){
    suma = suma + tiempo[individuos[i],individuos[i+1]]
  }
  suma = suma + tiempo[individuos[length(individuos)],individuos[1]]
  return(suma)
}
#♠ Fin de paso 9

#♠ Inicio de Paso 10 Inicializar diferenter recorridos entre ciudades 
initial = function(number, ContCiudades){
  result = matrix(0, number, ContCiudades)
  for(i in 1:number){
    result[i, ] = sample(1:ContCiudades)
  }
  return(result)
}

#♠ Fin de Paso 10

#♠ Inicio de Paso 11 torneo Binario entre las difirentes poblaciones
tournamentselection = function(evaluation,number){
  indexes = matrix(0,1,length(evaluation))
  for (i in 1:length(evaluation)){
    a = sample(length(evaluation),size=number)
    indexes[i]=a[which.min(evaluation[a])]
  } 
  return(indexes)
}
#♠ Fin de Paso 11

#♠ Inicion de Paso 12 realizamos Un cruce de orden utizando el indexparex para identificar con que 
#deveriamos cruzarlos
crossover = function(indexparents,population,pcross){
  offspring = population
  for (i in seq(1,length(indexparents),2)){
    if (runif(1)<pcross){
      p1 = population[indexparents[i],]
      p2 = population[indexparents[i+1],]
      ContCiudades=length(population[1,])
      
      cortes = sample(2:((ContCiudades-1)),2) 
      cortem = min(cortes)
      corteM = max(cortes)
      comienzo = corteM+1
      
      hijo1 = matrix(0, 1, ContCiudades)
      hijo2 = matrix(0, 1, ContCiudades)
      
      hijo1[(cortem:corteM)] = p1[(cortem:corteM)]
      hijo2[(cortem:corteM)] = p2[(cortem:corteM)]
      
      trans1 = matrix(0, 1, ContCiudades);
      trans2 = matrix(0, 1, ContCiudades);
      
      
      trans1[1:(ContCiudades-corteM)]=p2[(corteM+1):ContCiudades]
      trans1[(ContCiudades-corteM+1):ContCiudades]=p2[1:corteM]
      trans2[1:(ContCiudades-corteM)]=p1[(corteM+1):ContCiudades]
      trans2[(ContCiudades-corteM+1):ContCiudades]=p1[1:corteM]
      
      q = comienzo;
      for (j in 1:ContCiudades) {
        if (trans1[j] %in% hijo1 == FALSE) {
          hijo1[q] = trans1[j];
          if (q == ContCiudades) {
            q = 1;
          } else {
            q = q+1;
          }
        }
      }
      
      q = comienzo;
      for (j in 1:ContCiudades) {
        if (trans2[j] %in% hijo2 == FALSE) {
          hijo2[q] = trans2[j];
          if (q == ContCiudades) {
            q = 1;
          } else {
            q = q+1;
          }
        }
      }
      
      offspring[i,] = hijo1
      offspring[i+1,] = hijo2
    }
  }
  return(offspring)
}
#♠ Fin de Paso 12

#♠ Inicion de Paso 13 Mutacion Uniforme cambiamos la posicion de una ciudad con otra
mutation = function(population,pmut){
  ContCiudades=length(population[1,])
  Nhijos=length(population[,1])
  for(i in 1: Nhijos){
    if(runif(1)<pmut){
      puntos=sample(2:((ContCiudades)),2)
      p1=population[i,puntos[1]];
      p2=population[i,puntos[2]];
      population [i,puntos[1]]=p2
      population [i,puntos[2]]=p1
    }
  }
  return(population)
}
#♠ Fin de Paso 13

#♠ Inicio de Paso 14 Inicializacion de bariables para realizar el numero de generaciones
#Ademas de inicializar la probavilidad de cruce y de mutacion
generations = 10000
tournamentsize = 2
probcrossover = 0.45
probmutation  = 0.3
popsize = 100
#♠ Fin de Paso 14

#♠ Inicio de Paso 15 Realizar la permituacion y cruce de las diferenter recorridos y sacamos
#el mejor individuo

best = c()
bestfitness = Inf
population = initial(popsize,ContCiudades)
evaluation = apply(population,1,fitness,distancias)
progreso = data.frame(g=numeric(),distancia=numeric(),promedio=numeric(),peor=numeric(),distancia=numeric())
for (g in 1:generations){
  indexparents = tournamentselection(evaluation,tournamentsize)
  offspring1 = crossover(indexparents,population,probcrossover)
  offspring2 = mutation(offspring1,probmutation)
  population = offspring2
  evaluation = apply(population,1,fitness,distancias)
  
  # Actualizamos el mejor individuo
  if (min(evaluation)<bestfitness){
    bestfitness=min(evaluation)
    best = population[which.min(evaluation),]
  }
  # Hacemos elitismo
  if (bestfitness!=min(evaluation)){
    population[1,]=best
  }
  print(paste("Generación ",g," Fitness Mejor individuo - ",bestfitness))
  print(best)
  progreso = rbind(progreso,
                   data.frame(g=g,mejor=bestfitness,
                              promedio=mean(evaluation),
                              peor=max(evaluation),
                              distancia=mean(as.matrix(dist(population, method = "euclidean")))))
}
#♠ Fin de Paso 15

#♠ Inicio de Paso 16 Dibujamos el promedio que el cual saca la informacion de datos
plot1 = ggplot(progreso)+
  geom_line(aes(x=g,y=mejor),col="green")+
  geom_line(aes(x=g,y=promedio),col="blue")+
  geom_line(aes(x=g,y=peor),col="red")+
  scale_y_log10()+
  labs(title = "Evolución de los fitness Mejor, Promedio y Peor",
       subtitle = paste("Mejor Individuo Final: ",bestfitness),
       caption = "Universidad de deusto")

grid.arrange(plot1,ncol=1)
#♠ Fin de Paso 16

#♠ Inicio de Paso 17 Dibujamos las ciudades en el mapa sin recorridos
leaflet(Ciudades) %>% addTiles() %>%
  addAwesomeMarkers(Ciudades$lon, Ciudades$lat,label  =Ciudades$Ciudades)
#♠ Inicio de Paso 17 Dibujamos las ciudades en el mapa sin recorridos

#♠ Inicio de Paso 18 Ordenamos las ciudades por su recorrido
result=(stringsAsFactors = F)  
result = Ciudades
result$dist=0
best
for(i in 1:ContCiudades){
  result[i,] = Ciudades[best[i],]
}
#♠ Fin de Paso 18

#♠ Inicio de Paso 19 Ponemos los balores a las ciudades para poner el numero en el cual se recorren y
#Ponemos las distancias ente las ciudades
for(i in 1:ContCiudades){
  if((i)<ContCiudades){
  result[i,4]=paste("Distancia: ",toString(distancias[best[i],best[i+1]])," Tiempo:",toString(tiempo[best[i],best[i+1]]) )
  result[i,1]= paste(toString (i),"º ", result[i,1])
  }
}
result[ContCiudades+1,] = Ciudades[best[1],]
result[ContCiudades,1]= paste(toString (52),"º ", result[52,1])
result[ContCiudades+1,4]=paste("Distancia: ",toString(distancias[best[ContCiudades],best[1]])," Tiempo:",toString(tiempo[best[ContCiudades],best[1]]) )
#♠ Fin de Paso 19 

#♠ Inicion de Paso 20 dibujamos el mapa con las ciudades y su recorrido en linea recta
leaflet(Ciudades) %>% addTiles() %>%
  addAwesomeMarkers(result$lon, result$lat,label  =result$Ciudades) %>%
  addPolylines(result$lon, result$lat, popup = result$dist)
#♠ Fin de paso 20



#♠ Inicio de Paso 21 Inicializamos las ciudades de nuevo para poder sacar las rutas 
# Entre de las diferentes ciudades y sacamos las diferentes rutas. Si falla en alguna parte 
# Mirar donde se a quedado y poner el valor de la i
for(i in 1:ContCiudades){
  result[i,] = Ciudades[best[i],]
} 
rutas= route(result[1,1] ,result[2,1]  , structure="route");
for(i in 17:(ContCiudades-1)){
  rutas=rbind(rutas,route(result[i,1] ,result[i+1,1]  , structure="route"))

  }
rutas=rbind(rutas,route(result[ContCiudades,1] ,result[ContCiudades+1,1], structure="route"))
#♠ Fin de Paso 21

#Inicio de Paso 22 dibujar el mapa con las rutas que tiene que seguir los vehiculos
leaflet(Ciudades) %>% addTiles() %>%
  addAwesomeMarkers(Ciudades$lon, Ciudades$lat,label  =Ciudades$Ciudades) %>%
  # addCircles(rutas$lon, rutas$lat, weight = 5, radius=100, 
  #            color="#ffa500", stroke = TRUE, fillOpacity = 0.8) %>%
  addPolylines(rutas$lon, rutas$lat,label=rutas$km)
#Fin de Paso 22


#Fin Zona Curro-----------------------------------------------------------------------------------
length(population[,1])
WhereIsOne=which(population[1,]==1)
WhereIsOne
hastaUno=population[1,1:WhereIsOne-1]
hastaUno
desdeUno=population[1,WhereIsOne:length(population[1,])]
desdeUno
length(hastaUno)
length(desdeUno)
population[1,]
completo=population[1,]

completo[1:(length(population[1,])-WhereIsOne)]=desdeUno
completo[(length(population[1,])-WhereIsOne):length(population[1,])]=hastaUno
for (sift in 1:length(population[,1])) {
  WhereIsOne=which(population[sift,]==1)
  hastaUno=population[sift,1:WhereIsOne]
  desdeUno=population[sift,WhereIsOne:length(population[1,])]
}

#Zona de pruevas----------------------------------------------------------------------------------
if(datos[,3]==N)  
  datos[complete.cases(datos), ]  
complete.cases()
if(is.data.frame(datos) && nrow(datos)==0){
  datos[,7]=0;
}

puntos = data.frame(localizacion = c("Melilla",
                                     "Tenerife",
                                     "Albacete","Centro Comercial Ferial Plaza, Guadalajara","Catedral de Leon","Tenerife","Parque De Santa Ana, Cuenca","Museo Maritimo del Cantabrico,Santander"), stringsAsFactors = F)

ad = mapdist("Barcelona", "bilbao", mode="driving", output = c( "all"), messaging = FALSE, sensor = FALSE, language = "en-EN", override_limit = FALSE)
a = mapdist(puntos$localizacion[1], puntos$localizacion[2], mode="driving")
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

for(i in 1:ContCiudades){
  if((i)<ContCiudades){
    # recorrido[i,2]=distancias[best[i],best[i+1]]
    #recorrido[i,3]=tiempo[best[i],best[i+1]]
    result[i,4]=paste("Distancia: ",toString(distancias[best[i],best[i+1]])," Tiempo:",toString(tiempo[best[i],best[i+1]]) )
    result[i,1]= paste(toString (i),"º ", result[i,1])
    #recorrido[i,1]="Distancia: "+toString(recorrido[i,2])+" Tiempo: "+ toString(recorrido[i,3])
  }
}

result[ContCiudades+1,] = Ciudades[best[1],]
result[ContCiudades,1]= paste(toString (52),"º ", result[52,1])
result[ContCiudades+1,4]=paste("Distancia: ",toString(distancias[best[ContCiudades],best[1]])," Tiempo:",toString(tiempo[best[ContCiudades],best[1]]) )
#♠ Fin de Paso 19 

#cordenadas <- geocode(Ciudades$Ciudades) 
# geocodeQueryCheck(userType = "free")
#for (ciudad in 1:ContCiudades) {
# while(is.na(cordenadas[ciudad,1])){
#     cordenadas[ciudad,] <- geocode(Ciudades[ciudad,1])
#      geocodeQueryCheck(userType = "free")
#  }
#}


#Ciudades$lat = cordenadas$lat
#Ciudades$lon = cordenadas$lon

#cordenadas <- geocode(Ciudades$Ciudades) 
# geocodeQueryCheck(userType = "free")
#for (ciudad in 1:ContCiudades) {
# while(is.na(cordenadas[ciudad,1])){
#     cordenadas[ciudad,] <- geocode(Ciudades[ciudad,1])
#      geocodeQueryCheck(userType = "free")
#  }
#}


#Ciudades$lat = cordenadas$lat
#Ciudades$lon = cordenadas$lon
#result[53,1]=paste("Distancia: ",toString(distancias[best[ContCiudades],best[1]])," Km Tiempo:",toString(tiempo[best[ContCiudades],best[1]])," Min" )

#Recorridos=matrix("NA",ContCiudades*ContCiudades,5)
#colnames(Recorridos)
#class(Recorridos)
#cnames <-c("Ciudad Origen", "Ciudad Destino", "Tiempo", "Gasolina", "Precio")
#colnames(Recorridos) <- cnames
#Antiguo -------------------------------------------------------------------------------------
rute = route(puntos$localizacion[1], puntos$localizacion[2], structure="route")

leaflet(puntos) %>% addTiles() %>%
  addAwesomeMarkers(rute$lon, rute$lat) %>%
  addPolylines(rute$lon, rute$lat)
