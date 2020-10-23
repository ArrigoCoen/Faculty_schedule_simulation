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
  #Se definen las variables que vamos a utilizar
  mat_demanda_alumnos <- matrix(0,nrow = length(param$nombre_hrs),
                                ncol = length(param$vec_nom_materias_total))
  
  #' 1) Extracción de datos y simulación de alumnos de t+1
  vec_s_sem_k_info <- gen_vec_s_sem_k_info(param_sim$vec_sem_sig,
                                           param_sim$k_sem_ant,param)
  
  # Start the clock!
  ptm <- proc.time()
  for(d in 1:length(param$vec_nom_materias_total)){
    materia <- param$vec_nom_materias_total[d]
    cat("\n materia ",d,":",materia)
    param_sim$Materias = materia
    param_sim$m_filtrada <- gen_mat_m_filtrada(param,param_sim)
    mat_alumnos_corregidos <- gen_mat_alumnos_corregidos(vec_s_sem_k_info,
                                                         param,param_sim)
    vec_alum_sim <- simula_alumnos(mat_alumnos_corregidos,param)
    mat_demanda_alumnos[,d] <- vec_alum_sim
  }
  cat("\nEl proceso tardó: ",(proc.time()-ptm)[3]," segundos\n")##45.91
  rownames(mat_demanda_alumnos) <- param$nombre_hrs
  colnames(mat_demanda_alumnos) <- param$vec_nom_materias_total
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

