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

  #' 1-3) Extracción de datos y simulación de alumnos de t+1
  mat_demanda_alumnos <- gen_mat_demanda_alumnos(param,param_sim)#46.19 seg
  View(mat_demanda_alumnos)
  
  #' 4a) Simulación de solicitudes de profesores de t+1 (oculta)
  mat_solicitudes <- gen_solicitudes(param)#8.47 seg
  View(mat_solicitudes)
  
  #' 5) Simulación de esqueletos
  lista_info_esqueleto <- gen_esqueleto(mat_demanda_alumnos,mat_solicitudes,
                                        param)#19.48seg
  mat_esqueleto <- lista_info_esqueleto[[1]]
  mat_prof_TC <- lista_info_esqueleto[[2]]
  mat_prof_asig <- lista_info_esqueleto[[3]]
  
  #' 6) Calificación de esqueletos
  califica_esqueleto()
  
  #' 7) AG aplicado a esqueletos: Aquí ya va a salir un buen esqueleto
  AG_esqueleto()
  
  #' 4b) Simulación de solicitudes de profesores (pseudo-real)
  mat_solicitudes <- gen_solicitudes(param)#8.56 seg
  
  #' 8) Asignación
  gen_asignacion(mat_esqueleto,mat_solicitudes,param)
    
  #' 9) Calificación de asignación
  califica_asignaciones()
  
  #' 10) AG aplicado a asignaciones: Aquí ya va a salir una buena asignación
  AG_asignaciones()
  
  return(mat_asignaciones)
}

