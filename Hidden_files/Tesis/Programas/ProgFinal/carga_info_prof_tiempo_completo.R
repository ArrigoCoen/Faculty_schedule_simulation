##########################################################################
#' En este programa se encuentra la función que extrae los nombres de los
#' profesroes de tiempo completo del Departamento de Matemáticas.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Programas/Prog PRUEBAS V02")
source("Fn_Asignacion.R")



# carga_info_prof_tiempo_completo -----------------------------------------
carga_info_prof_tiempo_completo <- function(){
  url <- "http://www.matematicas.unam.mx/index.php/nosotros/profesores-de-tiempo-completo"
  
  # Sacamos la información de la página
  webpage <- read_html(url)
  profesor_data_html <- html_nodes(webpage,'td a')#Selecciona 94 profesores
  prof_TC <- html_text(profesor_data_html)  
  prof_TC
  
  vec_prof_TC <- rep(0,length(prof_TC))
  
  ### Quitamos "\n\t\t\t\t\t\t\t"
  for(d in 1:length(vec_prof_TC)){
    # texto <- substr(prof_TC[d],1,8)
    vec_prof_TC[d] <- substr(prof_TC[d],9,nchar(prof_TC[d]))
  }
  
  
  #' Quitamos los títulos de los profesores: Dr., Dra., M., Act., ...
  #' Hay varios casos por lo que vamos a dividir los datos
  #' en grupos para poder quitar todos los títulos.
  #' Para ver de una mejor manera los casos de los títulos
  #' ordenamos los nombres:
  sort(vec_prof_TC)
  
  ### "" ###
  
  
  ### "" ###
  
  
  ### "" ###
  
  
  ### "" ###
  
  
  ### "" ###
  
  
  ### "" ###
  
  
  ### "" ###
  
  
  ### "" ###
  
  
  
  
  
  
  
  
  
  
  
  
  return(vec_prof_TC)
}



# Ej. ---------------------------------------------------------------------

vec_prof_TC <- carga_info_prof_tiempo_completo()
