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


# quita_num_gpos_x_materia ------------------------------------------------
#' Title quita_num_gpos_x_materia: Función que se encarga de limpiar los
#' nombres de las materias de "vec_materias".
#'
#' @param vec_materias: Vector con los nombres de las materias, con el
#' número de grupos por cada materia.
#'
#' @return vec_materias: Vector con los nombres de las materias, sin el
#' número de grupos por cada materia.
#'
#' @examples
#' quita_num_gpos_x_materia(materias_act)
#' quita_num_gpos_x_materia(materias_CdC)
#' quita_num_gpos_x_materia(materias_mate)
#' quita_num_gpos_x_materia(materias_mateAp)
#' 
quita_num_gpos_x_materia <- function(vec_materias){
  # Quitamos el número de grupos por materia
  for(d in 1:length(vec_materias)){
    num_char <- nchar(vec_materias[d])
    texto <- substr(vec_materias[d],(num_char-5),num_char)
    if(texto==" grupo" || texto=="grupos"){#Menores a 10
      vec_materias[d] <- substr(vec_materias[d],1,(num_char-10))
    }
  }#Fin for(d)
  
  #Quitamos las comas finales
  for(d in 1:length(vec_materias)){
    num_char <- nchar(vec_materias[d])
    texto <- substr(vec_materias[d],num_char,num_char)
    if(texto==","){
      vec_materias[d] <- substr(vec_materias[d],1,(num_char-1))
    }
  }#Fin for(d)
  
  return(vec_materias)
}




# guarda_info_materias_x_carrera -------------------------------------------
#' Title guarda_info_materias_x_carrera: Función que extrae los nombres de
#' las materias de cada carrera del Depto. de Mate (Actuaría, CdC, Mate y
#' MateAp) en un vector. Dicho vector lo guarda.
#'
#' @examples
#' guarda_info_materias_x_carrera()
#' 
guarda_info_materias_x_carrera <- function(){
  raiz <- "http://www.fciencias.unam.mx/docencia/horarios/indiceplan/20201/"
  
  ### Actuaría ###
  # http://www.fciencias.unam.mx/docencia/horarios/indiceplan/20201/2017
  # div > a #Código SelectorGadget Selecciona 96 materias
  url <- paste0(raiz,2017)
  # Sacamos la información de la página
  webpage <- read_html(url)
  materias_act_data_html <- html_nodes(webpage,'div > a')
  materias_act <- html_text(materias_act_data_html)  
  # materias_act
  
  #Quitamos las entradas vacías
  materias_act <- materias_act[materias_act!=""]
  
  # Quitamos entradas con FB TWITTER ...
  materias_act <- materias_act[1:90]#9 materias
  
  materias_act <- quita_num_gpos_x_materia(materias_act)
  save(materias_act, file = paste0("materias_act.RData"))
  
  
  ### Ciencias de la Computación ###
  # http://www.fciencias.unam.mx/docencia/horarios/indiceplan/20201/1556
  # div > a#Selecciona 83 materias
  url <- paste0(raiz,1556)
  # Sacamos la información de la página
  webpage <- read_html(url)
  materias_CdC_data_html <- html_nodes(webpage,'div > a')
  materias_CdC <- html_text(materias_CdC_data_html)  
  # materias_CdC
  
  #Quitamos las entradas vacías
  materias_CdC <- materias_CdC[materias_CdC!=""]
  
  # Quitamos entradas con FB TWITTER ...
  materias_CdC <- materias_CdC[1:77]#77 materias
  
  materias_CdC <- quita_num_gpos_x_materia(materias_CdC)
  save(materias_CdC, file = paste0("materias_CdC.RData"))
  
  
  ### Matemáticas ###
  # http://www.fciencias.unam.mx/docencia/horarios/indiceplan/20201/217
  # div > a#Selecciona 134 materias
  url <- paste0(raiz,217)
  # Sacamos la información de la página
  webpage <- read_html(url)
  materias_mate_data_html <- html_nodes(webpage,'div > a')
  materias_mate <- html_text(materias_mate_data_html)  
  # materias_mate
  
  #Quitamos las entradas vacías
  materias_mate <- materias_mate[materias_mate!=""]
  
  # Quitamos entradas con FB TWITTER ...
  materias_mate <- materias_mate[1:128]#128 materias
  
  materias_mate <- quita_num_gpos_x_materia(materias_mate)
  save(materias_mate, file = paste0("materias_mate.RData"))
  
  
  ### Matemáticas Aplicadas ###
  # http://www.fciencias.unam.mx/docencia/horarios/indiceplan/20201/2055
  # div > a#Selecciona 74 materias
  url <- paste0(raiz,2055)
  # Sacamos la información de la página
  webpage <- read_html(url)
  materias_mateAp_data_html <- html_nodes(webpage,'div > a')
  materias_mateAp <- html_text(materias_mateAp_data_html)  
  # materias_mateAp
  
  #Quitamos las entradas vacías
  materias_mateAp <- materias_mateAp[materias_mateAp!=""]
  
  # Quitamos entradas con FB TWITTER ...
  materias_mateAp <- materias_mateAp[1:68]#68 materias
  
  materias_mateAp <- quita_num_gpos_x_materia(materias_mateAp)
  save(materias_mateAp, file = paste0("materias_mateAp.RData"))
}


# Ej. ---------------------------------------------------------------------

guarda_info_materias_x_carrera()
