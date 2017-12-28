# Nombre: Jon Acha Quintial 
# Entrega: Practica e3 - Algoritmos gen�ticos
# Fecha:
##---------------------------------------------------------------------------
# 1. Asegúrese de incluir, junto a esta plantilla, cualquier fichero necesario
#    para su ejecución, incluidos datasets
# 2. Si utiliza una función de un determinado paquete, no olvide incluir la
#    correspondiente llamada a la función "library()"
# 3. No olvide comentar el código, en especial aquellos comandos no-triviales
#    (recuerda que parte de la calificación depende de la limpieza del código)
#---------------------------------------------------------------------------
rm(list = ls());cat("\014")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
library(GGally)
library(ggplot2)
library(gridExtra)
# (incluya aqu�??? cualquier libreria adicional)
#---------------------------------------------------------------------------


puntos = read.table('bayg29.txt');
long = nrow(puntos);
#

# Paso 1: Crear una matriz llamada distancias, de longxlong elementos. En cada 
# posición debe almacenar la distancia entre 2 ciudades
# distancias[i,j] <- distancia eucl�???dea entre puntos[i,] y puntos[j,]

#Utilizamos el teorema de pitagoras para sacar la distancia de los puntos
distancias=matrix(0,long,long)

for(i in 1:long){
  for(j in 1:long){
    distancias[i,j] = sqrt((( puntos [i,1]-puntos[j,1])^2 + ( puntos [i,2]-puntos[j,2])^2 )); 
  }
}


# Paso 2: Defina la función fitness, que reciba un individuo y la matriz de distancias
# y devuelva la longitud total del camino recorrido
# --> fitness = function(ind,distancias){...}

#error en el paso  49 dim recorrido null



#--------------------------
fitness=function(ind,distancias){
  fitness=0;
    for (i in 1:(length(ind)-1)){
      px= ind[i];
      py =ind[i+1];
      fitness=fitness+distancias[px,py];
    } 
  
  #el ultimo punto con el primero para terminar el recorrido
    px=ind[length(ind)];
    py=ind[1]
   fitness=fitness+distancias[px,py];
    
    
  
  return(fitness)

}


# Paso 3: Defina la función de inicialización, que reciba el número de individuos a crear
# y el número de puntos (variable long). Ésta función debe devolver una matriz con tantas
# filas como individuos y "long" columnas.
# En cada fila, habrá una permutación (aleatoria) de los valores entre 1 y long
# --> initial = function(number,long){...}
initial = function(number,long){
  initial=matrix(0,number,long)
  for(i in 1:number){
    initial[i,]=sample(1:long) #crea un random de numeros desde 1 hasta la cantidad maxima de puntos
  }
  initial  
  return (initial)
}

# Paso 4: Puede utilizar la función de torneo binario vista en clase

tournamentselection = function(evaluation,number){
  indexes = matrix(0,1,length(evaluation))
  for (i in 1:length(evaluation)){
    a = sample(length(evaluation),size=number)
    indexes[i]=a[which.min(evaluation[a])]
  } 
  return(indexes)
}

# Paso 5: Operador de cruce de orden
# Implemente una función que recibe los �???ndices de padres, la población y la probabilidad de cruce
# y realiza el operador de cruce de orden, como ha visto en clase
# crossover = function(indexparents,population,pcross){
# -->   offspring = population
# -->     for (i in seq(1,length(indexparents),2)){
# -->       if (runif(1)<pcross){
# -->         ...Realizar el cruce aqu�???...
# -->       }
# -->     }
# -->     return(offspring)
# -->   } Es un recorrido 
#Hay 100 se deverian hacer tantos como recorridos halla
# -->   Diapo 48 el padre 1 es el punto 1 y el padre 2 es el punto 2 asi sucesivamente

#-------------------------------------------------------
crossover = function(indexparents,population,pcross){
  offspring = population
  for (i in seq(1,length(indexparents),2)){
    if (runif(1)<pcross){
      p1 = population[indexparents[i],]
      p2 = population[indexparents[i+1],]
      long=length(population[1,])
      cortes=sample(2:((long-1)),2) 
      corte1=min(cortes) 
      corte2=max(cortes)
      
      matrix1=matrix(0,1,long);
      #Hijo 1
      #copiamos los datos del padre 1
      matrix1[(corte1:corte2)]=p1[(corte1:corte2)];
      #Ordenamos el padre 2 deacuerdo al cruze
      #Esto se hace para que el primer valor a intercambiar sea
      #el de despues del corte 2 hasta el final se pone en la primera posicion
      #de la matrix ha cambiar y despues ponemos la otra parte hasta el corte 2
      matrix2=matrix(0,1,long);
      matrix2[1:(long-corte2)]=p2[(corte2+1):long]
      matrix2[(long-corte2+1):long]=p2[1:corte2]

 
      pos=corte2+1;
      for(x in 1:long){
        if(matrix2[x]%in% matrix1==FALSE ){#comprobamos si el punto ya ha sido introducido
          
          matrix1[pos]=matrix2[x];
          
          
          if(pos==long){
            pos=1;
          }else{
            pos=pos+1;
          }
        }
      }
      offspring[i,] =matrix1;
      
      #Hijo 2
      #Padre 2
      matrix1=matrix(0,1,long);
      matrix1[(corte1:corte2)]=p2[corte1:corte2];
      #Ordenamos el Padre 1 deacuerdo al cruze
      #el de despues del corte 2 hasta el final se pone en la primera posicion
      #de la matrix ha cambiar y despues ponemos la otra parte hasta el corte 2
      matrix2=matrix(0,1,long);
      matrix2[1:(long-corte2)]=p1[(corte2+1):long]
      matrix2[(long-corte2+1):long]=p1[1:corte2]
      
      pos=corte2+1
      for(x in 1:long){
        if(matrix2[x]%in% matrix1==FALSE ){ #comprobamos si el punto ya ha sido introducido
          matrix1[pos]=matrix2[x];
          
          if(pos==long){
            pos=1;
          }else{
            pos=pos+1;
          }
        }
      }
      offspring[i+1,] =matrix1;
    }
    
  }
  
  return(offspring)
}
# Comandos utilizados

#Operador de mutacion mal
# Paso 6: Operador de Mutación
# Para cada hijo, con probabilidad pmut, intercambiar dos posiciones elegidas aleatoriamente
# -->   mutation = function(population,pmut){...}
  mutation = function(population,pmut){
    long=length(population[1,])
    Nhijos=length(population[,1])
    for(i in 1: Nhijos){#Recorremos todos los hijos
      if(runif(1)<pmut){
    puntos=sample(2:((long)),2)# sacamos dos puntos aleatorios a intercambiar
    p1=population[i,puntos[1]];#sacamos 1 el punto que queramos combinar
    p2=population[i,puntos[2]];#sacamos 2 el punto que queramos combinar
    population [i,puntos[1]]=p2 # Sustituimos el punto 1 por el 2
    population [i,puntos[2]]=p1 # Sustituimos el punto 2 por el 1
      }
    }
    return(population)
}


# Paso 7: Realize hasta 5 pruebas con diferentes configuraciones de los siguientes parámetros
# analice y comente los resultados a modo de comentarios:
#---------------------------------------------------------------------------
# Prueba1: 
# Parámetros--> ...
# Resultados--> ...
# Justificación/Razonamiento--> ...
#---------------------------------------------------------------------------
generations = 500
tournamentsize = 2
probcrossover = 0.9
probmutation = 0.05
popsize=100

# Paso 8: Comente cuáles han sido las dificultades encontradas para afrontar esta práctica y cómo las ha resuelto



#---------------------------------------------------------------------------
# Si todo está bien, el código deber�???a funcionar a partir de aqu�???
# haga las comprobaciones pertienentes para cada operador
# (Si hubiera alguna errata o fallo en el código, que lo puede haber, 
# comun�???camelo lo antes posible con el fin de solventarlo)
#---------------------------------------------------------------------------
best = c()
bestfitness = Inf
population = initial(popsize,long)
evaluation = apply(population,1,fitness,distancias)
progreso = data.frame(g=numeric(),mejor=numeric(),promedio=numeric(),peor=numeric(),distancia=numeric())
#--------------------------------
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

# Dibujamos los resultados
plot1 = ggplot(progreso)+
  geom_line(aes(x=g,y=mejor),col="green")+
  geom_line(aes(x=g,y=promedio),col="blue")+
  geom_line(aes(x=g,y=peor),col="red")+
  scale_y_log10()+
  labs(title = "Evoluc�on de los fitness Mejor, Promedio y Peor",
       subtitle = paste("Mejor Individuo Final: ",bestfitness),
       caption = "Universidad de deusto")

plot2 = ggplot(progreso,aes(x=g,y=distancia))+geom_line()+
  labs(title = "Evoluci�n de la distancia promedio entre individuos",
       subtitle = "Ojo, en este caso la distancia eucl�dea no es significativa",
       caption = "Universidad de deusto")
grid.arrange(plot1,plot2,ncol=1)

