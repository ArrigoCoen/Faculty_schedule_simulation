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
  # set.seed(8654)
  set.seed(1806)
  mat_demanda_alumnos <- gen_mat_demanda_alumnos(param,param_sim)#39.95 seg
  View(mat_demanda_alumnos)
  
  #' 4a) Simulación de solicitudes de profesores del siguiente semestre (oculta)
  # set.seed(8654)
  set.seed(1806)
  mat_solicitudes <- gen_solicitudes(param)#8.14 seg
  View(mat_solicitudes)
  
  #' 5) Simulación de esqueletos
  # set.seed(8654)
  # set.seed(9293)#5.76seg
  # set.seed(0802)#5.66seg
  # set.seed(236776)#4.82seg
  # set.seed(132934)#4.9seg
  # set.seed(0.1806)#5.39seg
  set.seed(1806)
  lista_info_esqueleto <- gen_esqueleto(mat_demanda_alumnos,mat_solicitudes,
                                        param)#16.73/10.27/13.34/9.25/8.65seg
  
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

