##########################################################################
#' En este programa se encuentra la función que elimina la información del
#' profesor del gen elegido en la matriz de solicitudes.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")



# ajusta_mat_solicitudes --------------------------------------------------
#' Title ajusta_mat_solicitudes: Función que elimina la información del
#' profesor del gen elegido en la matriz de solicitudes. Se quitan los grupos
#' con la misma hora y materia del profesor del gen elegido.
#'
#' @param mat_solicitudes_restantes: Matriz de 5 columnas (Profesor,TC,
#' Materia,Num_Materia,Horario) que tiene la información de las solicitudes
#' pseudo-reales de los profesores. Es una submatriz de "mat_solicitudes_real".
#' @param gen_elegido: Vector de 4 entradas (Materia,Profesor,TC,Horario)
#' con la información del gen del padre elegido.
#'
#' @return mat_solicitudes_restantes: Submatriz de "mat_solicitudes_real".
#' Matriz de 5 columnas (Profesor,TC,Materia,Num_Materia,Horario) que tiene
#' la información de las solicitudes pseudo-reales de los profesores.
#'
#' @examples
#' mat_solicitudes_restantes <- ajusta_mat_solicitudes(
#' mat_solicitudes_restantes,gen_elegido)
#' 
ajusta_mat_solicitudes <- function(mat_solicitudes_restantes,gen_elegido){
  #Se definen las variables que se van a utilizar
  (prof <- as.character(gen_elegido[2]))
  (hora <- as.character(gen_elegido[4]))
  (materia <- as.character(gen_elegido[1]))
  (ind_prof <- which(mat_solicitudes_restantes[,1] == prof))
  (ind_hora <- which(mat_solicitudes_restantes[,5] == hora))
  (ind_materia <- which(mat_solicitudes_restantes[,3] == materia))
  (elim_hora_prof <- intersect(ind_prof,ind_hora))
  (elim_materia_prof <- intersect(ind_prof,ind_materia))
  (ind_elim <- union(elim_hora_prof,elim_materia_prof))
  
  mat_solicitudes_restantes <- mat_solicitudes_restantes[-ind_elim,]
  
  return(mat_solicitudes_restantes)
}

