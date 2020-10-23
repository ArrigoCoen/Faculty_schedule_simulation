##########################################################################
#' En este programa se encuentra la función que genera la matriz
#' "mat_demanda_alumnos" con 15 renglones (horas) y 333 columnas
#' (materias). En la entrada (i,j) se tiene el número de alumnos simulados
#' para la hora i, y la materia j.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")



# gen_mat_demanda_alumnos -------------------------------------------------
#' Title gen_mat_demanda_alumnos: Función que genera la matriz
#' "mat_demanda_alumnos" con 15 renglones (horas) y 333 columnas (materias).
#' En la entrada (i,j) se tiene el número de alumnos simulados para la hora
#' i, y la materia j.
#'
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @param param_sim: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se encargan de la simulación.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' @example param_sim <- list(vec_sem_sig = c(20191,20192,20201),k_sem_ant = 5,
#' materia = "Estadística III", num_sim = 10, m_filtrada = matrix(0),
#' sub_m_filtrada = matrix(0,ncol = length(param$nom_cols_MG)))
#'
#' @return mat_demanda_alumnos: Matriz de 15 renglones (horas) y 333
#' columnas (materias). En la entrada (i,j) se tiene el número de alumnos
#' simulados para la hora i, y la materia j.
#'
#' @examples
#' mat_demanda_alumnos <- gen_mat_demanda_alumnos(param,param_sim)
#' 
gen_mat_demanda_alumnos <- function(param,param_sim){
  ptm <- proc.time()# Start the clock!
  
  #Se definen las variables que vamos a utilizar
  mat_demanda_alumnos <- matrix(0,nrow = length(param$nombre_hrs),
                                ncol = length(param$vec_nom_materias_total))
  vec_s_sem_k_info <- gen_vec_s_sem_k_info(param_sim$vec_sem_sig,
                                           param_sim$k_sem_ant,param)
  
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
  
  return(mat_demanda_alumnos)
}


# Ej. ---------------------------------------------------------------------
mat_demanda_alumnos <- gen_mat_demanda_alumnos(param,param_sim)
View(mat_demanda_alumnos)
