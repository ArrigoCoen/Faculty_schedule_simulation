##########################################################################
#' En este programa se encuentra la función que genera la asignación
#' completa de todas las materias.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")


# gen_asignacion_completa -------------------------------------------------
gen_asignacion_completa <- function(param,param_sim){

  #' 1) Extracción de datos y simulación de alumnos de t+1
  mat_demanda_alumnos <- gen_mat_demanda_alumnos(param,param_sim)#47.48 seg
  View(mat_demanda_alumnos)
  
  #' 2) Simulación de solicitudes de profesores de t+1 (oculta)
  mat_solicitudes <- gen_solicitudes(param)#8.47 seg
  View(mat_solicitudes)
  
  #' 3) Simulación de esqueletos: Aquí ya va a salir el mejor esqueleto
  #' (ya calificado y con AG)
  gen_esqueleto(directorio_info,param)
  
  #' 4) Simulación de solicitudes de profesores (pseudo-real)
  mat_solicitudes <- gen_solicitudes(param)#8.56 seg
  
  #' 5) Asignación
  gen_asignacion(mat_esqueleto,mat_solicitudes,param)
    
  #' 6) Calificación de asignación
  califica_asignaciones()
  
  return(mat_asignaciones)
}

