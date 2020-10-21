##########################################################################
#' En este programa se encuentra la función que extrae los nombres de las
#' materias de cada carrera del Depto. de Mate (Actuaría,CdC,Mate,MateAp)
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")



# carga_info_materias_x_carrera -------------------------------------------
carga_info_materias_x_carrera <- function(){
  raiz <- "http://www.fciencias.unam.mx/docencia/horarios/indiceplan/20201/"
  
  ### QUITAR NÚMERO DE GRUPOS
  
  
  # Actuaría ----------------------------------------------------------------
  # http://www.fciencias.unam.mx/docencia/horarios/indiceplan/20201/2017
  # div > a #Código SelectorGadget Selecciona 96 materias
  url <- paste0(raiz,2017)
  # Sacamos la información de la página
  webpage <- read_html(url)
  materias_act_data_html <- html_nodes(webpage,'div > a')#Selecciona 91 materias
  materias_act <- html_text(materias_act_data_html)  
  materias_act
  ### QUITAR FB TWITTER ...
  
  
  # Ciencias de la Computación ----------------------------------------------
  # http://www.fciencias.unam.mx/docencia/horarios/indiceplan/20201/1556
  # div > a#Selecciona 83 materias
  url <- paste0(raiz,1556)
  # Sacamos la información de la página
  webpage <- read_html(url)
  materias_CdC_data_html <- html_nodes(webpage,'div > a')#Selecciona 78 materias
  materias_CdC <- html_text(materias_CdC_data_html)  
  materias_CdC
  ### QUITAR FB TWITTER ...
  
  
  # Matemáticas -------------------------------------------------------------
  # http://www.fciencias.unam.mx/docencia/horarios/indiceplan/20201/217
  # div > a#Selecciona 134 materias
  url <- paste0(raiz,217)
  # Sacamos la información de la página
  webpage <- read_html(url)
  materias_mate_data_html <- html_nodes(webpage,'div > a')#Selecciona 129 materias
  materias_mate <- html_text(materias_mate_data_html)  
  materias_mate
  ### QUITAR FB TWITTER ...
  
  # Matemáticas Aplicadas ---------------------------------------------------
  # http://www.fciencias.unam.mx/docencia/horarios/indiceplan/20201/2055
  # div > a#Selecciona 74 materias
  url <- paste0(raiz,2055)
  # Sacamos la información de la página
  webpage <- read_html(url)
  materias_mateAp_data_html <- html_nodes(webpage,'div > a')#Selecciona 69 materias
  materias_mateAp <- html_text(materias_mateAp_data_html)  
  materias_mateAp
  ### QUITAR FB TWITTER ...
  
  
  
}



