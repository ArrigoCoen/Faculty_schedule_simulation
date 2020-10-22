##########################################################################
#' En este programa se encuentran la función que carga la matriz
#' "m_grande_total" de los semestres 2008-1 a 2020-1 de la cual se va
#' obtiene la lista de nombres de los profesores sin repetición. La matriz
#' "mat_nom_prof_total" tiene 2 columnas, en la primera se tiene el nombre
#' de los profesores y en la segunda se tiene un 1 si el profesor es de
#' tiempo completo y 0 si no.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")

# gen_mat_nom_prof_total --------------------------------------------------
#' Title gen_mat_nom_prof_total: Función que carga la matriz
#' "m_grande_total" de los semestres 2015-1 a 2020-1 de la cual se va
#' obtiene la lista de nombres de los profesores sin repetición. La matriz
#' "mat_nom_prof_total" tiene 2 columnas, en la primera se tiene el nombre
#' de los profesores y en la segunda se tiene un 1 si el profesor es de
#' tiempo completo y 0 si no.
#'
#' @return mat_nom_prof_total: Matriz de 2 columnas, en la primera se
#' tiene el nombre de los profesores y en la segunda se tiene un 1 si
#' el profesor es de tiempo completo y 0 si no.
#' 
#' @examples
#' mat_nom_prof_total <- gen_mat_nom_prof_total()
#'
gen_mat_nom_prof_total <- function(){
  #' Se carga la matriz m_grande_total de 2015-1 a 2020-1 de la cual
  #' se va a obtener la lista de nombres que se desea
  load("Matrices m_grande_total/m_grande_total_20151_20201.RData")
  # View(m_grande_total)
  
  #' Se carga el vector con los nombres de los profesores de tiempo
  #' completo del Departamento de Matemáticas.
  vec_prof_TC <- carga_info_prof_tiempo_completo()#94
  
  #Se definen las variables que se van a utilizar:
  num_col_Profesor <- arroja_ind_col_MG("Profesor")##2
  vec_aux <- unique(m_grande_total[,num_col_Profesor])#1389
  # mat_nom_prof_total <- data.frame(Profesor = vec_aux,Tiempo_Completo = 0)
  mat_nom_prof_total <- matrix(0,nrow = length(vec_aux),ncol = 2)
  mat_nom_prof_total[,1] <- vec_aux
  
  #Se quitan los renglones sin información o NA's
  mat_nom_prof_total <- mat_nom_prof_total[mat_nom_prof_total[,1]!="",]
  mat_nom_prof_total <- mat_nom_prof_total[!is.na(mat_nom_prof_total[,1]),]
  
  #Recorre renglones de "mat_nom_prof_total"
  for(d in 1:dim(mat_nom_prof_total)[1]){
    nom_prof <- mat_nom_prof_total[d,1]
    if(any(vec_prof_TC==nom_prof)){
      mat_nom_prof_total[d,2] <- 1
    }
  }#Fin for(d)
  
  #' Hasta aquí hay 83 profesores de tiempo completo, se verán los
  #' casos faltantes (11):
  mat_aux <- mat_nom_prof_total[mat_nom_prof_total[,2]==1,]
  ind_aux <- 0
  for(d in 1:length(vec_prof_TC)){#Recorre los profesores de Tc
    if(any(vec_prof_TC[d]==mat_aux[,1])){
      ind_aux <- c(ind_aux,d)
    }
  }
  #Se quita el cero inicial
  ind_aux <- ind_aux[-1]
  
  #Vemos los casos faltantes
  vec_prof_TC_aux <- vec_prof_TC[-ind_aux]
  # ind_faltantes <- 1:length(vec_prof_TC)
  # ind_faltantes <- ind_faltantes[-ind_aux]
  #' 1) "Alejandro Ricardo Garciadiego Dantán": Diferencia de acentos (778)
  #' 2) "Ana Luisa Solís González Cosío": No se encuentra en los horarios
  #' 3) "Edith Corina Sáenz Valadéz": Diferencia de acentos (107)
  #' 4) "Emilio Lluis Puebla":  No se encuentra en los horarios
  #' 5) "Guillermo Sienra Loera": No se encuentra en los horarios
  #' 6) "Isabel Puga Espinosa": No se encuentra en los horarios
  #' 7) "Ma. Asunción Begoña Fernández Fernández": Diferencia en Ma.-María (667)
  #' 8) "María de Lourdes Velasco Arregui": No se encuentra en los horarios
  #' 9) "Mucuy-kak del Carmen Guevara Aguirre": Diferencia en Kak y kak (127)
  #' 10) "Óscar Alfredo Palmas Velasco": Diferencia de acentos (172)
  #' 11) "Úrsula Iturrarán Viveros": Diferencia en nombre. (351,871)
  #' 
  #' De estos 11 casos, vamos a modificar a mano: 1,3,7,9,10,11
  mat_nom_prof_total[c(778,107,667,127,172,351),2] <- 1
  colnames(mat_nom_prof_total) <- c("Profesor","Tiempo_Completo")
  save(mat_nom_prof_total, file = "mat_nom_prof_total.RData")
  
  return(mat_nom_prof_total)
}



# Ej. ---------------------------------------------------------------------
mat_nom_prof_total <- gen_mat_nom_prof_total()
View(mat_nom_prof_total)
