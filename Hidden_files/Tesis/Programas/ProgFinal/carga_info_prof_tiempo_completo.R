##########################################################################
#' En este programa se encuentra la función que extrae los nombres de los
#' profesores de tiempo completo del Departamento de Matemáticas.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")


# carga_info_prof_tiempo_completo -----------------------------------------
#' Title carga_info_prof_tiempo_completo: Función que extrae los nombres de
#' los profesores de tiempo completo del Departamento de Matemáticas.
#'
#' @return vec_prof_TC: Vector con el nombre de los profesores de tiempo
#' completo del Departamento de Matemáticas.
#'
#' @examples
#' vec_prof_TC <- carga_info_prof_tiempo_completo()
#' 
carga_info_prof_tiempo_completo <- function(){
  #Página del Departamento de Matemáticas
  url <- "http://www.matematicas.unam.mx/index.php/nosotros/profesores-de-tiempo-completo"
  
  # Sacamos la información de la página
  webpage <- read_html(url)
  profesor_data_html <- html_nodes(webpage,'td a')#Selecciona 94 profesores
  prof_TC <- html_text(profesor_data_html)  
  prof_TC
  
  #Se define el vector en el que vamos a guardar los nombres
  vec_aux_prof_TC <- rep(0,length(prof_TC))
  vec_prof_TC <- 0
  
  ### Quitamos "\n\t\t\t\t\t\t\t"
  for(d in 1:length(vec_aux_prof_TC)){
    vec_aux_prof_TC[d] <- substr(prof_TC[d],9,nchar(prof_TC[d]))
  }
  
  #' Quitamos los títulos de los profesores: Dr., Dra., M., Act., ...
  #' Hay varios casos por lo que vamos a dividir los datos
  #' en grupos para poder quitar todos los títulos.
  #' Para ver de una mejor manera los casos de los títulos
  #' ordenamos los nombres:
  # sort(vec_aux_prof_TC)[1:10]
  # View(sort(vec_aux_prof_TC))
  ind_prof <- 0
  
  ### " Dra. " Un espacio al inicio, un espacio antes del nombre ###
  ### "	Dr.  " Un espacio al inicio, 2 espacios antes del nombre ###
  for(d in 1:length(vec_aux_prof_TC)){
    texto <- substr(vec_aux_prof_TC[d],1,6)
    if(texto==" Dr.  " || texto==" Dra. " || texto=="Act.  "){
      num_char <- nchar(vec_aux_prof_TC[d])
      nom_prof <- substr(vec_aux_prof_TC[d],7,num_char)
      vec_prof_TC <- c(vec_prof_TC,nom_prof)
      ind_prof <- c(ind_prof,d)
    }
  }#Fin for(d)
  #Se quita el cero del inicio
  ind_prof <- ind_prof[-1]
  #' Se actualiza el vector auxiliar para que la búsqueda sea cada vez
  #' de menos elementos
  vec_aux_prof_TC <- vec_aux_prof_TC[-ind_prof]
  # sort(vec_aux_prof_TC)[1:10]
  #Se reinicializa el vector de índices
  ind_prof <- 0
  
  
  ### " Dr. " Un espacio al inicio, un espacio antes del nombre ###
  ### "Act. " Un espacio antes del nombre ###
  ### "Dra. " Un espacio antes del nombre ###
  ### "Mat. " Un espacio antes del nombre ###
  for(d in 1:length(vec_aux_prof_TC)){
    texto <- substr(vec_aux_prof_TC[d],1,5)
    if(texto==" Dr. " || texto=="Act. " || texto=="Dra. " || texto=="Mat. "){
      num_char <- nchar(vec_aux_prof_TC[d])
      nom_prof <- substr(vec_aux_prof_TC[d],6,num_char)
      vec_prof_TC <- c(vec_prof_TC,nom_prof)
      ind_prof <- c(ind_prof,d)
    }
  }#Fin for(d)
  #Se quita el cero del inicio
  ind_prof <- ind_prof[-1]
  #' Se actualiza el vector auxiliar para que la búsqueda sea cada vez
  #' de menos elementos
  vec_aux_prof_TC <- vec_aux_prof_TC[-ind_prof]
  # sort(vec_aux_prof_TC)[1:10]
  #Se reinicializa el vector de índices
  ind_prof <- 0
  
  
  ### " M. en C. " Un espacio al inicio, un espacio antes del nombre ###
  for(d in 1:length(vec_aux_prof_TC)){
    texto <- substr(vec_aux_prof_TC[d],1,10)
    if(texto == " M. en C. "){
      num_char <- nchar(vec_aux_prof_TC[d])
      nom_prof <- substr(vec_aux_prof_TC[d],11,num_char)
      vec_prof_TC <- c(vec_prof_TC,nom_prof)
      ind_prof <- c(ind_prof,d)
    }
  }#Fin for(d)
  #Se quita el cero del inicio
  ind_prof <- ind_prof[-1]
  #' Se actualiza el vector auxiliar para que la búsqueda sea cada vez
  #' de menos elementos
  vec_aux_prof_TC <- vec_aux_prof_TC[-ind_prof]
  # sort(vec_aux_prof_TC)[1:10]
  #Se reinicializa el vector de índices
  ind_prof <- 0
  
  
  ### "Dr. " Un espacio antes del nombre ###
  for(d in 1:length(vec_aux_prof_TC)){
    texto <- substr(vec_aux_prof_TC[d],1,4)
    if(texto == "Dr. "){
      num_char <- nchar(vec_aux_prof_TC[d])
      nom_prof <- substr(vec_aux_prof_TC[d],5,num_char)
      vec_prof_TC <- c(vec_prof_TC,nom_prof)
      ind_prof <- c(ind_prof,d)
    }
  }#Fin for(d)
  #Se quita el cero del inicio
  ind_prof <- ind_prof[-1]
  #' Se actualiza el vector auxiliar para que la búsqueda sea cada vez
  #' de menos elementos
  vec_aux_prof_TC <- vec_aux_prof_TC[-ind_prof]
  # sort(vec_aux_prof_TC)[1:10]
  #Se reinicializa el vector de índices
  ind_prof <- 0
  
  
  ### "M. en C. " Un espacio antes del nombre ###
  for(d in 1:length(vec_aux_prof_TC)){
    texto <- substr(vec_aux_prof_TC[d],1,9)
    if(texto == "M. en C. "){
      num_char <- nchar(vec_aux_prof_TC[d])
      nom_prof <- substr(vec_aux_prof_TC[d],10,num_char)
      vec_prof_TC <- c(vec_prof_TC,nom_prof)
      ind_prof <- c(ind_prof,d)
    }
  }#Fin for(d)
  #Se quita el cero del inicio
  ind_prof <- ind_prof[-1]
  #' Se actualiza el vector auxiliar para que la búsqueda sea cada vez
  #' de menos elementos
  vec_aux_prof_TC <- vec_aux_prof_TC[-ind_prof]
  # sort(vec_aux_prof_TC)[1:10]
  #Se reinicializa el vector de índices
  ind_prof <- 0
  
  
  ### ""M. en I. de O. " Un espacio antes del nombre ###
  ### "M. en E. I. O. " Un espacio antes del nombre ###
  for(d in 1:length(vec_aux_prof_TC)){
    texto <- substr(vec_aux_prof_TC[d],1,15)
    if(texto=="M. en I. de O. " || texto=="M. en E. I. O. "){
      num_char <- nchar(vec_aux_prof_TC[d])
      nom_prof <- substr(vec_aux_prof_TC[d],16,num_char)
      vec_prof_TC <- c(vec_prof_TC,nom_prof)
      ind_prof <- c(ind_prof,d)
    }
  }#Fin for(d)
  #Se quita el cero del inicio
  vec_prof_TC <- vec_prof_TC[-1]
  
  
  #' Quitamos los espacios del final
  for(d in 1:length(vec_prof_TC)){
    num_char <- nchar(vec_prof_TC[d])
    texto <- substr(vec_prof_TC[d],num_char,num_char)
    if(texto==" "){
      nom_prof <- substr(vec_prof_TC[d],1,(num_char-1))
      vec_prof_TC[d] <- nom_prof
    }
  }#Fin for(d)
  
  #' Quitamos los espacios del final (en caso de tener 2)
  for(d in 1:length(vec_prof_TC)){
    num_char <- nchar(vec_prof_TC[d])
    texto <- substr(vec_prof_TC[d],num_char,num_char)
    if(texto==" "){
      nom_prof <- substr(vec_prof_TC[d],1,(num_char-1))
      vec_prof_TC[d] <- nom_prof
    }
  }#Fin for(d)
  
  return(vec_prof_TC)
}



# Ej. ---------------------------------------------------------------------
vec_prof_TC <- carga_info_prof_tiempo_completo()
length(vec_prof_TC)##94
