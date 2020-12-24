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
  # set.seed(1806)
  # mat_demanda_alumnos <- gen_mat_demanda_alumnos(param,param_sim)#39.95 seg
  # View(mat_demanda_alumnos)
  
  #' 4a) Simulación de solicitudes de profesores del siguiente semestre (oculta)
  # set.seed(8654)
  # set.seed(1806)
  # mat_solicitudes <- gen_solicitudes(param)#7.78 seg
  # View(mat_solicitudes)
  
  #' 5) Simulación de esqueletos
  # set.seed(8654)
  # set.seed(9293)#5.76seg
  # set.seed(0802)#5.66seg
  # set.seed(236776)#4.82seg
  # set.seed(132934)#4.9seg
  # set.seed(0.1806)#5.39seg
  # n_rep <- 10 #12.45 min
  n_rep <- 5 #5.97/5.59 min
  set.seed(1806)
  lista_esq_D_prima <- metodo_B(n_rep,param,param_sim)#5.79 min
  mat_esqueleto <- lista_esq_D_prima[[1]]
  View(mat_esqueleto)
  
  #' 6) Calificación de esqueletos
  # califica_esqueleto()
  
  #' 7) AG aplicado a esqueletos: Aquí ya va a salir un buen esqueleto
  # AG_esqueleto()
  
  #' 4b) Simulación de solicitudes de profesores (pseudo-real)
  set.seed(1806)
  mat_solicitudes_real <- gen_solicitudes_real(mat_esqueleto,param)#8.3 seg
  View(mat_solicitudes_real)
  
  #' 8) Asignación
  # set.seed(1806)
  # lista_asignacion <- gen_asignacion(mat_esqueleto,mat_solicitudes_real,
  #                                    param)#22.66 seg
  # mat_asignacion <- lista_asignacion[[1]]
  # mat_esqueleto_aux <- lista_asignacion[[2]]
  # # mat_solicitud_aux <- lista_asignacion[[3]]
  # View(mat_asignacion)
  # View(mat_esqueleto_aux)
  # View(mat_solicitud_aux)
  
  #' 9) Calificación de asignación
  # set.seed(1806)
  # lista_calif_asignacion <- califica_asignacion(mat_solicitudes_real,
  #                                               lista_asignacion,
  #                                               param)#5.48 seg
  # mat_calif_asig_x_gpo <- lista_calif_asignacion[[1]]
  # (calif_asignacion <- lista_calif_asignacion[[2]])#-1083.836
  # View(mat_calif_asig_x_gpo)
  
  #' 10) AG aplicado a asignaciones: Aquí ya va a salir una buena asignación
  AG_asignaciones()
  
  return(mat_asignaciones)
}

