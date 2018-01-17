#Nombre: Asier Salsidua y Jon Acha
#Asignatura:Inteligencia Artificial Avanzada
#Proyecto: Obtener el recorrido mas optimo recorriendo todas las ciudades de España

#Inicio de Paso 0 Limpiar datos
  rm(list = ls());cat("\014")
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  getwd()
#Fin de Paso 0

# Inicio Paso 1 - Librerias
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
# Fin Paso 1
# Inicio Paso 2 Lectura de las ciudades con sus cordenadas
  Ciudades <- read.xlsx("CiudadesEspana.xlsx", sheetIndex = 1,stringsAsFactors = F)[,-1]
  ContCiudades=  nrow(Ciudades)
# Fin del paso 2

# Inicio Paso 3 Sacar las coordenadas de las ciudades. Ir al paso 7 si ya se ha probado el correcto funcionamiento 
#de la extraccion de datos
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
# Fin de paso 3 

# Inicio Paso 4 inicializacion de las matrices de distancias y tiempo entre ciudades
  distancias=matrix(0,ContCiudades,ContCiudades)
  tiempo=matrix(0,ContCiudades,ContCiudades)
  fallos=matrix(0, 2704, 2, byrow=F)
  cnames <-c("Ciudad Origen", "Ciudad Destino")
  colnames(fallos) <- cnames
# Fin Paso 4

# Inicio Paso 5 Sacar las distancias y el tiempo entre las ciudades 
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
# Fin de Paso 5

# Inicio de Paso 6 escritura  de los datos  
  write.xlsx(distancias, "DistanciasCuidades.xlsx") 
  write.xlsx(tiempo, "TiemposCiudades.xlsx") 
  #write.xlsx(fallos, "ciudadesQueFallan.xlsx")
  write.xlsx(Ciudades, "CiudadesEspaña.xlsx") 
# Fin de Paso 6

# Inicio de Paso 7 lectura de los xlsx con los datos para que no se tengan que sacar los datos otra vez
  distancias <- read.xlsx("DistanciasCuidades.xlsx", sheetIndex = 1,stringsAsFactors = F)[,-1]
  tiempo <- read.xlsx("TiemposCiudades.xlsx", sheetIndex = 1,stringsAsFactors = F)[,-1]
  #fallos <- read.xlsx("ciudadesQueFallan.xlsx", sheetIndex = 1,stringsAsFactors = F)[,-1]
# Fin de Paso 7

# Inicio de Paso 8 Metodo para sacar la distancia total del recorrido entre las ciudades
  fitness = function(individuos, distancias){
    suma = 0;
    for(i in 1:(length(individuos)-1)){
      suma = suma + distancias[individuos[i],individuos[i+1]]
    }
    suma = suma + distancias[individuos[length(individuos)],individuos[1]]
    return(suma)
  }
# Fin de Paso 8

# Inicio de Paso 9 Metodo para sacar el tiempo total en lo que se tarda en sacar el recorrido entre las ciudades
  timeness= function(individuos, tiempo){
    suma = 0;
    for(i in 1:(length(individuos)-1)){
      suma = suma + tiempo[individuos[i],individuos[i+1]]
    }
    suma = suma + tiempo[individuos[length(individuos)],individuos[1]]
    return(suma)
  }
# Fin de paso 9

# Inicio de Paso 10 Inicializar diferentes recorridos entre ciudades
  initial = function(number, ContCiudades){
    result = matrix(0, number, ContCiudades)
    for(i in 1:number){
      result[i, ] = sample(1:ContCiudades)
    }
    return(result)
  }
# Fin de Paso 10

# Inicio de Paso 11 torneo Binario entre las diferentes poblaciones
  tournamentselection = function(evaluation,number){
    indexes = matrix(0,1,length(evaluation))
    for (i in 1:length(evaluation)){
      a = sample(length(evaluation),size=number)
      indexes[i]=a[which.min(evaluation[a])]
    } 
    return(indexes)
  }
# Fin de Paso 11

# Inicio de Paso 12 realizamos un cruce de orden utilizando el indexparents para identificar con que 
#deberiamos cruzarlos
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
# Fin de Paso 12

# Inicio de Paso 13 Mutacion Uniforme cambiamos la posicion de una ciudad con otra
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
# Fin de Paso 13
# Paso 14 metodo para comprobar si la poblacion tiene repeticiones y generar una nueva poblacion aleatoria en caso de que sea repetido
  comprabarRepetido=function(population){
    completo = c()
    populationOrdenada=population
    for (sift in 1:  length(population[1,])) {
      WhereIsOne=which(population[sift,]==1)
      hastaUno=population[sift,1:WhereIsOne-1]
      desdeUno=population[sift,(WhereIsOne):  ContCiudades]
      if(length(desdeUno)!=0){
        completo[1:length(desdeUno)]=desdeUno
      }
      if(length(hastaUno)!=0){
        completo[(length(desdeUno)+1):length(population[1,])]=hastaUno
      }
      populationOrdenada[sift, ]=completo
    }
  
    duplicadosPopulation= duplicated(populationOrdenada[,])
    #Ver los duplicados en la poblacion
    #print( duplicadosPopulation)
    for(sift in 1:  length(population[1,])){
      if(duplicadosPopulation[sift]){
        population[sift,]=sample(1:ContCiudades)
      }
    }
    return(population)
  }
 #Fin de paso 14 
  
  
# Inicio de Paso 15 Inicializacion de variables para realizar el numero de generaciones
# ademas de inicializar la probabilidad de cruce y de mutacion
  generations = 10000
  tournamentsize = 2
  probcrossover = 0.45
  probmutation  = 0.3
  popsize = 100
# Fin de Paso 15

# Inicio de Paso 16a Realizar la permutacion y cruce de las diferentes recorridos y sacamos
#el mejor individuo por medio de la distancia saltar al paso 17 

  best = c()
  bestfitness = Inf
  population = initial(popsize,ContCiudades)
  population =  comprabarRepetido(population)
  evaluation = apply(population,1,fitness,distancias)
  progreso = data.frame(g=numeric(),distancia=numeric(),promedio=numeric(),peor=numeric(),distancia=numeric())
  for (g in 1:generations){
    indexparents = tournamentselection(evaluation,tournamentsize)
    offspring1 = crossover(indexparents,population,probcrossover)
    offspring1 =  comprabarRepetido(offspring1)
    offspring2 = mutation(offspring1,probmutation)
    population = offspring2
    population =  comprabarRepetido(population)
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
    print(paste("Generación ",g," Fitness Mejor distancia - ",bestfitness))
    print(best)
    progreso = rbind(progreso,
                     data.frame(g=g,mejor=bestfitness,
                                promedio=mean(evaluation),
                                peor=max(evaluation),
                                distancia=mean(as.matrix(dist(population, method = "euclidean")))))
  }
# Fin de Paso 16a
  
# Inicio de Paso 16 b Realizar la permutacion y cruce de las diferente recorridos y sacamos
# el mejor individuo por medio de la tiempo saltar al paso 17 
  
  best = c()
  bestfitness = Inf
  population = initial(popsize,ContCiudades)
  population =  comprabarRepetido(population)
  evaluation = apply(population,1,timeness,tiempo)
  progreso = data.frame(g=numeric(),tiempo=numeric(),promedio=numeric(),peor=numeric(),minutos=numeric())
  for (g in 1:generations){
    indexparents = tournamentselection(evaluation,tournamentsize)
    offspring1 = crossover(indexparents,population,probcrossover)
    offspring1 =  comprabarRepetido(offspring1)
    offspring2 = mutation(offspring1,probmutation)
    population = offspring2
    population =  comprabarRepetido(population)
    evaluation = apply(population,1,  timeness,tiempo)
    
    # Actualizamos el mejor individuo
    if (min(evaluation)<bestfitness){
      bestfitness=min(evaluation)
      best = population[which.min(evaluation),]
    }
    # Hacemos elitismo
    if (bestfitness!=min(evaluation)){
      population[1,]=best
    }
    print(paste("Generación ",g," Timeness Mejor Tiempo - ",bestfitness))
    print(best)
    progreso = rbind(progreso,
                     data.frame(g=g,mejor=bestfitness,
                                promedio=mean(evaluation),
                                peor=max(evaluation),
                                distancia=mean(as.matrix(dist(population, method = "euclidean")))))
  }  
  
  # Fin de Paso 16 b  
  
  # Inicio de Paso 16 c Realizar la permutacion y cruce de las diferente recorridos y sacamos
  # el mejor individuo por medio de la tiempo y distancia saltar al paso 17  
  
  
  best = c()
  bestfitness = Inf
  bestfitness2 = Inf
  population = initial(popsize,ContCiudades)
  population =  comprabarRepetido(population)
  evaluation = apply(population,1,timeness,tiempo)
  progreso = data.frame(g=numeric(),tiempo=numeric(),distancia=numeric(),promedio=numeric(),peor=numeric(),minutos=numeric())
  for (g in 1:generations){
    indexparents = tournamentselection(evaluation,tournamentsize)
    offspring1 = crossover(indexparents,population,probcrossover)
    offspring1 =  comprabarRepetido(offspring1)
    offspring2 = mutation(offspring1,probmutation)
    population = offspring2
    population =  comprabarRepetido(population)
    evaluation = apply(population,1,  timeness,tiempo)
    evaluation2 = apply(population,1,fitness,distancias)
    # Actualizamos el mejor individuo
    #Con una AND para que el caso en que el tiempo y la distancia sean mejores
    if (min(evaluation)<bestfitness&&min(evaluation2)<bestfitness2){
    #Con una OR para que el caso en el que el tiempo o la distancia sea mejor
    #if (min(evaluation)<bestfitness||min(evaluation2)<bestfitness2){ 
      
      bestfitness=min(evaluation)
      bestfitness2=min(evaluation2)
      best = population[which.min(evaluation),]
    }
    # Hacemos elitismo
    #Con una AND para que el caso en que el tiempo y la distancia sean mejores
    if (bestfitness!=min(evaluation)&&bestfitness2!=min(evaluation2)){
   #Con una OR para que el caso en el que el tiempo o la distancia sea mejor
   #if (bestfitness!=min(evaluation)||bestfitness2!=min(evaluation2)){
      population[1,]=best
    }
    print(paste("Generación ",g," Timeness Mejor Tiempo - ",bestfitness, " Mejor distancia es - ",bestfitness2 ))
    print(best)
    progreso = rbind(progreso,
                     data.frame(g=g,mejor=bestfitness,distancia=bestfitness2,
                                promedio=mean(evaluation),
                                peor=max(evaluation),
                                distancia=mean(as.matrix(dist(population, method = "euclidean")))))
  }  
  
  # Fin de Paso 16 c

# Inicio de Paso 17 Dibujamos el promedio que el cual saca la información de datos
  plot1 = ggplot(progreso)+
    geom_line(aes(x=g,y=mejor),col="green")+
    geom_line(aes(x=g,y=promedio),col="blue")+
    geom_line(aes(x=g,y=peor),col="red")+
    scale_y_log10()+
    labs(title = "Evolución de los fitness Mejor, Promedio y Peor",
         subtitle = paste("Mejor Individuo Final: ",bestfitness),
         caption = "Universidad de deusto")
  
  grid.arrange(plot1,ncol=1)
# Fin de Paso 17

# Inicio de Paso 18 Dibujamos las ciudades en el mapa sin recorridos
  leaflet(Ciudades) %>% addTiles() %>%
    addAwesomeMarkers(Ciudades$lon, Ciudades$lat,label  =Ciudades$Ciudades)
# Fin de Paso 18

# Inicio de Paso 19 Ordenamos las ciudades por su recorrido
  result=(stringsAsFactors = F)  
  result = Ciudades
  result$dist=0
  #best
  for(i in 1:ContCiudades){
    result[i,] = Ciudades[best[i],]
  }
# Fin de Paso 19

# Inicio de Paso 20 Ponemos los valores a las ciudades para poner el numero en el cual se recorren y
#Ponemos las distancias entre las ciudades
  for(i in 1:ContCiudades){
    if((i)<ContCiudades){
    result[i+1,4]=paste("Distancia: ",toString(distancias[best[i],best[i+1]]),"Km Tiempo:",toString(tiempo[best[i],best[i+1]]) ,"Min")
    result[i,1]= paste(toString (i),"N ", result[i,1])
    }
  }
  result[ContCiudades+1,] = Ciudades[best[1],]
  result[ContCiudades,1]= paste(toString (52),"N ", result[52,1])
  result[ContCiudades+1,4]=paste("Distancia: ",toString(distancias[best[ContCiudades],best[1]]),"Km Tiempo:",toString(tiempo[best[ContCiudades],best[1]]),"Min" )
# Fin de Paso 20

# Inicion de Paso 21 dibujamos el mapa con las ciudades y sus recorridos en linea recta
  leaflet(Ciudades) %>% addTiles() %>%
    addAwesomeMarkers(result$lon, result$lat,label  =result$Ciudades, popup = result$dist) %>%
    addPolylines(result$lon, result$lat)

# Fin de paso 21
  
# Inicio de Paso 22 Inicializamos las ciudades de nuevo para poder sacar las rutas 
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
# Fin de Paso 22

# Inicio de Paso 23 dibujar el mapa con las rutas que tiene que seguir los vehiculos
  leaflet(Ciudades) %>% addTiles() %>%
    addAwesomeMarkers(result$lon, result$lat,label  =result$Ciudades, popup = result$dist) %>%
    # addCircles(rutas$lon, rutas$lat, weight = 5, radius=100, 
    #            color="#ffa500", stroke = TRUE, fillOpacity = 0.8) %>%
    addPolylines(rutas$lon, rutas$lat,label=rutas$km)
# Fin de Paso 23


  population[1,i]
  
#Fin Zona Curro-----------------------------------------------------------------------------------
