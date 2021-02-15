##########################################################################
#' En este programa se encuentra la función que se encarga de limpiar los
#' nombres de los profesores de asginatura.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")



# Pasos -------------------------------------------------------------------

load("mat_nom_prof_total.RData")
View(mat_nom_prof_total)
dim(mat_nom_prof_total)#1387 2

prof_TC <- mat_nom_prof_total[mat_nom_prof_total[,2]==1,]
prof_asig <- mat_nom_prof_total[mat_nom_prof_total[,2]==0,]
# dim(prof_TC)#94 2
# dim(prof_asig)#1293 2


#Vamos a trabajar con "vec_prof_asig"
vec_prof_asig <- sort(prof_asig[,1])

#Quitamos el "/" del inicio
for(d in 1:9){
  num_char <- nchar(vec_prof_asig[d])
  vec_prof_asig[d] <- substr(vec_prof_asig[d],4,num_char)
}
vec_prof_asig <- sort(vec_prof_asig)

#Vemos los nombres que tienen "/" y dejamos sólo el primer nombre
for(d in 1:length(vec_prof_asig)){
  nom_prof <- vec_prof_asig[d]
  diagonal <- gregexpr(pattern ='/',nom_prof)
  
  if(diagonal[[1]] > 0){
    fin <- diagonal[[1]][1]
    # num_char <- nchar(vec_prof_asig[d])
    vec_prof_asig[d] <- substr(vec_prof_asig[d],1,(fin-1))
  }
}
vec_prof_asig <- unique(vec_prof_asig)
length(vec_prof_asig)#1246
vec_prof_asig <- sort(vec_prof_asig)
View(vec_prof_asig)
mat_repeticiones <- data.frame(Profesor = vec_prof_asig,
                               Nom1 = 0,Nom2 = 0,Nom3 = 0,Nom4 = 0,Nom5 = 0,
                               Nom6 = 0,Nom7 = 0,Nom8 = 0,Nom9 = 0,Nom10 = 0)
# mat_repeticiones <- data.frame(Profesor = vec_prof_asig,
#                                Nom1 = 0,Nom2 = 0,Nom3 = 0,Nom4 = 0,Nom5 = 0,
#                                Nom6 = 0,Nom7 = 0,Nom8 = 0,Nom9 = 0,Nom10 = 0,
#                                Nom11 = 0,Nom12 = 0,Nom13 = 0,Nom14 = 0,Nom15 = 0,
#                                Nom16 = 0,Nom17 = 0,Nom18 = 0,Nom19 = 0,Nom20 = 0)

#' El siguiente for va a recorrer todos los nombres en "vec_prof_asig" y va
#' a guardar los índices de aquellos nombres que tengan más de 50% de 
#' coincidencia.
for(p in 1:(length(vec_prof_asig)-1)){
  cat("\n p = ",p)
  nom_prof_1 <- vec_prof_asig[p]
  cont <- 2
  for(r in (p+1):length(vec_prof_asig)){
    nom_prof_2 <- vec_prof_asig[r]
    compara <- stringsim(nom_prof_1,nom_prof_2)
    
    if(compara > 0.6){
      mat_repeticiones[p,cont] <- nom_prof_2
      cont <- cont + 1
    }
  }#Fin for(r)
}#Fin for(p)

#Vimos la matriz y escribimos los índices con los nombres repetidos
ind_rep <- c(12,17,19,25,29,34,36,48,61,65,70,
             78,83,88,93,96,98,106,109,133,136,137,141,146,
             155,157,163,167,169,174,177,187,190,201,213,216,
             219,232,238,249,257,272,290,305,310,328:333,348,
             352,358,367,401,412,420,460,462:463,467,476,482,
             486,489,491,496,510,572,583,595,611,644,647,674,
             686,700,708,733,742,760,768,784,805,816,819,823,
             828:829,836,839,847,852,857,863,874,879,882,898,
             907,932,943,1002,1015,1042,1066,1069,1088,1106,
             1133,1142,1147,1160,1244)

#'#' Nombres que se eligió uno de ellos
# 156: Antonmaria Gerolamo Enrico Minzoni Alessio
# 157: Antonmaria Minzoni Alessio
# 
# 158: Araceli Arteaga Jiménez 
# 163: Aracely Arteaga Jiménez 
# 
# 695: José de Jesús Carlos Quintanar Sierra 
# 708: José Jesús Carlos Quintanar Sierra 
# 
# 767: Juan Manuel Eugenio Ramírez de Arellano Niño-Rincón
# 768: Juan Manuel Eugenio Ramírez de Arellano Niño Rincón
# 
# 829: Loiret Alejandria Dosal Trujillo
# 830: Loiret Alejandría Dosal Trujillo
# 
# 874: Ma. Susana Barrera Ocampo
# 947: María Susana Barrera Ocampo
# 
# 881: Manuel de Llano de la Garza
# 882: Manuel De Llano De la Garza
# 
# 1014: Mónica Alicia Clapp Jiménez-Labora
# 1015: Mónica Alicia Clapp Jiménez Labora
# 
# 1042: Omar Antolin Camarena
# 1043: Omar Antolín Camarena
# 
# 1133: Roberto Carrillo Larraga
# 1134: Roberto Carrillo Lárraga
# 
# 1142: Rocío Jauregui Renaud
# 1143: Rocío Jáuregui Renaud
# 
# 1146: Rodrigo Domínguez López
# 1147: Rodrígo Domínguez López
# 
# 1160: Rosalio Fernando Rodríguez Zepeda
# 1161: Rosalío Fernando Rodríguez Zepeda


#' Ambos nombres se quedaron
# 639: Jonás Raffael Martínez Sánchez
# 1089: Rafael Martínez Sánchez


vec_prof_asig <- vec_prof_asig[-ind_rep]
length(vec_prof_asig)#1131
View(vec_prof_asig)

#Checamos una segunda vez en caso de que aún haya repetidos
mat_rep_2 <- data.frame(Profesor = vec_prof_asig,
                               Nom1 = 0,Nom2 = 0,Nom3 = 0,Nom4 = 0,Nom5 = 0)

#' El siguiente for va a recorrer todos los nombres en "vec_prof_asig" y va
#' a guardar los índices de aquellos nombres que tengan más de 50% de 
#' coincidencia.
for(p in 1:(length(vec_prof_asig)-1)){
  cat("\n p = ",p)
  nom_prof_1 <- vec_prof_asig[p]
  cont <- 2
  for(r in (p+1):length(vec_prof_asig)){
    nom_prof_2 <- vec_prof_asig[r]
    compara <- stringsim(nom_prof_1,nom_prof_2)
    
    if(compara > 0.7){
      mat_rep_2[p,cont] <- nom_prof_2
      cont <- cont + 1
    }
  }#Fin for(r)
}#Fin for(p)
View(mat_rep_2)


# limpia_mat_nom_prof_total -----------------------------------------------
#' Title limpia_mat_nom_prof_total: Función que se encarga de limpiar los
#' nombres de los profesores de asginatura. Guarda la matriz
#' "mat_nom_prof_total".
#'
#' @examples
#' limpia_mat_nom_prof_total()
#' 
limpia_mat_nom_prof_total <- function(){
  load("mat_nom_prof_total.RData")
  
  prof_TC <- mat_nom_prof_total[mat_nom_prof_total[,2]==1,]
  prof_asig <- mat_nom_prof_total[mat_nom_prof_total[,2]==0,]
  
  #Vamos a trabajar con "vec_prof_asig"
  vec_prof_asig <- sort(prof_asig[,1])
  
  #Quitamos el "/" del inicio
  for(d in 1:9){
    num_char <- nchar(vec_prof_asig[d])
    vec_prof_asig[d] <- substr(vec_prof_asig[d],4,num_char)
  }
  vec_prof_asig <- sort(vec_prof_asig)
  
  #Vemos los nombres que tienen "/" y dejamos sólo el primer nombre
  for(d in 1:length(vec_prof_asig)){
    nom_prof <- vec_prof_asig[d]
    diagonal <- gregexpr(pattern ='/',nom_prof)
    
    if(diagonal[[1]] > 0){
      fin <- diagonal[[1]][1]
      # num_char <- nchar(vec_prof_asig[d])
      vec_prof_asig[d] <- substr(vec_prof_asig[d],1,(fin-1))
    }
  }
  vec_prof_asig <- sort(unique(vec_prof_asig))
  
  #' El siguiente for va a recorrer todos los nombres en "vec_prof_asig" y va
  #' a guardar los índices de aquellos nombres que tengan más de 50% de 
  #' coincidencia.
  mat_repeticiones <- data.frame(Profesor = vec_prof_asig,
                                 Nom1 = 0,Nom2 = 0,Nom3 = 0,Nom4 = 0,Nom5 = 0,
                                 Nom6 = 0,Nom7 = 0,Nom8 = 0,Nom9 = 0,Nom10 = 0)
  for(p in 1:(length(vec_prof_asig)-1)){
    cat("\n p = ",p)
    nom_prof_1 <- vec_prof_asig[p]
    cont <- 2
    for(r in (p+1):length(vec_prof_asig)){
      nom_prof_2 <- vec_prof_asig[r]
      compara <- stringsim(nom_prof_1,nom_prof_2)
      
      if(compara > 0.6){
        mat_repeticiones[p,cont] <- nom_prof_2
        cont <- cont + 1
      }
    }#Fin for(r)
  }#Fin for(p)
  
  #Vimos la matriz y escribimos los índices con los nombres repetidos
  ind_rep <- c(12,17,19,25,29,34,36,48,61,65,70,
               78,83,88,93,96,98,106,109,133,136,137,141,146,
               155,157,163,167,169,174,177,187,190,201,213,216,
               219,232,238,249,257,272,290,305,310,328:333,348,
               352,358,367,401,412,420,460,462:463,467,476,482,
               486,489,491,496,510,572,583,595,611,644,647,674,
               686,700,708,733,742,760,768,784,805,816,819,823,
               828:829,836,839,847,852,857,863,874,879,882,898,
               907,932,943,1002,1015,1042,1066,1069,1088,1106,
               1133,1142,1147,1160,1244)
  vec_prof_asig <- vec_prof_asig[-ind_rep]
  vec_prof_asig <- sort(vec_prof_asig)
  #'#' Nombres que se eligió uno de ellos
  # 156: Antonmaria Gerolamo Enrico Minzoni Alessio
  # 157: Antonmaria Minzoni Alessio
  # 
  # 158: Araceli Arteaga Jiménez 
  # 163: Aracely Arteaga Jiménez 
  # 
  # 695: José de Jesús Carlos Quintanar Sierra 
  # 708: José Jesús Carlos Quintanar Sierra 
  # 
  # 767: Juan Manuel Eugenio Ramírez de Arellano Niño-Rincón
  # 768: Juan Manuel Eugenio Ramírez de Arellano Niño Rincón
  # 
  # 829: Loiret Alejandria Dosal Trujillo
  # 830: Loiret Alejandría Dosal Trujillo
  # 
  # 874: Ma. Susana Barrera Ocampo
  # 947: María Susana Barrera Ocampo
  # 
  # 881: Manuel de Llano de la Garza
  # 882: Manuel De Llano De la Garza
  # 
  # 1014: Mónica Alicia Clapp Jiménez-Labora
  # 1015: Mónica Alicia Clapp Jiménez Labora
  # 
  # 1042: Omar Antolin Camarena
  # 1043: Omar Antolín Camarena
  # 
  # 1133: Roberto Carrillo Larraga
  # 1134: Roberto Carrillo Lárraga
  # 
  # 1142: Rocío Jauregui Renaud
  # 1143: Rocío Jáuregui Renaud
  # 
  # 1146: Rodrigo Domínguez López
  # 1147: Rodrígo Domínguez López
  # 
  # 1160: Rosalio Fernando Rodríguez Zepeda
  # 1161: Rosalío Fernando Rodríguez Zepeda
  
  
  #' Ambos nombres se quedaron
  # 639: Jonás Raffael Martínez Sánchez
  # 1089: Rafael Martínez Sánchez
  
  num_prof <- length(vec_prof_asig) + dim(prof_TC)[1]
  mat_nom_prof_total <- data.frame(Profesor = 1:num_prof,Tiempo_Completo = 0)
  mat_nom_prof_total[1:dim(prof_TC)[1],] <- prof_TC
  mat_nom_prof_total[(dim(prof_TC)[1]+1):num_prof,1] <- vec_prof_asig
  
  save(mat_nom_prof_total, file = "mat_nom_prof_total.RData")
}



# Ej. ---------------------------------------------------------------------
limpia_mat_nom_prof_total()

