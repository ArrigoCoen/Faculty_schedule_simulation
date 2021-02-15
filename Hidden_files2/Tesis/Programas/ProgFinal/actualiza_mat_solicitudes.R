##########################################################################
#' En este programa se encuentra la función que actualiza la matriz
#' "mat_solicitudes". Le quitamos las horas que ya dieron los profesores.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")


# actualiza_mat_solicitudes -----------------------------------------------
#' Title actualiza_mat_solicitudes: Función que actualiza la matriz
#' "mat_solicitudes". Le quitamos las horas que ya dieron los profesores.
#'
#' @param mat_a_actualizar: Matriz de solictudes. Puede ser de los
#' profesores de tiempo completo o de asignatura.
#' @param renglon: Vector con la información del profesor elegido para
#' asignarle un grupo.
#' @param prof_mas_2: Variable binaria que vale 1 si el profesor ya tiene
#' 2 o más materias asignadas y 0 si no.
#'
#' @return mat_solicitudes_act: Matriz de solicitudes actualizada.
#'
#' @examples
#' mat_solicitudes_act <- actualiza_mat_solicitudes(mat_a_actualizar,renglon,
#' prof_mas_2)
#' 
actualiza_mat_solicitudes <- function(mat_a_actualizar,renglon,prof_mas_2){
  # Se definen las variables que se van a utilizar
  profesor <- which(mat_a_actualizar[,1]==renglon[1])
  
  if(prof_mas_2 == 1){
    mat_solicitudes_act <- mat_a_actualizar[-profesor,]
  }else{
    hora <- which(mat_a_actualizar[,5]==renglon[5])
    #' Se intersectan los conjuntos para tener los índices que se
    #' deben retirar de la matriz
    indices <- intersect(profesor,hora)
    if(length(indices) > 0){
      mat_solicitudes_act <- mat_a_actualizar[-indices,]
    }else{
      mat_solicitudes_act <- mat_a_actualizar
    }
  }
  
  return(mat_solicitudes_act)
}

