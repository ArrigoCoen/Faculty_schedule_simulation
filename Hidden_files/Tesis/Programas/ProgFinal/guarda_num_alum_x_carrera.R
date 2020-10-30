##########################################################################
#' En este programa se encuentra la función que guarda el vector con el
#' número de alumnos por grupo de cada carrera.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")


# guarda_num_alum_x_carrera -----------------------------------------------
#' Title guarda_num_alum_x_carrera: Función que guarda el vector con el
#' número de alumnos por grupo de cada carrera.
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
#' @examples
#' guarda_num_alum_x_carrera(param,param_sim)
#' 
guarda_num_alum_x_carrera <- function(param,param_sim){
  #Se carga la lista con la infomración de las materias de cada carrera
  load("lista_mat_materias_x_carrera.RData")
  
  #Se definen las variables que se van a utilizar
  lista_num_alum_x_carrera <- list()
  param_sim$k_sem_ant = 25
  num_col_Alumnos <- arroja_ind_col_MG("Alumnos")##6
  # m_grande_total <- param$m_grande_total
  # mat_materias_act <- lista_mat_materias_x_carrera[[1]]
  # mat_materias_CdC <- lista_mat_materias_x_carrera[[2]]
  # mat_materias_mate <- lista_mat_materias_x_carrera[[3]]
  # mat_materias_mateAp <- lista_mat_materias_x_carrera[[4]]
  # View(mat_materias_act)
  # View(mat_materias_CdC)
  # View(mat_materias_mate)
  # View(mat_materias_mateAp)
  
  #Guardamos el vector con el número de alumnos para cada carrera
  for(d in 1:length(lista_mat_materias_x_carrera)){
    param_sim$Materias = lista_mat_materias_x_carrera[[d]][,1]
    m_filtrada <- gen_mat_m_filtrada(param,param_sim)
    lista_num_alum_x_carrera[[d]] <- m_filtrada[,num_col_Alumnos]
  }
  names(lista_num_alum_x_carrera) <- c("num_alum_actuaria","num_alum_CdC",
                                       "num_alum_mate","num_alum_mateAp")
  save(lista_num_alum_x_carrera,file = "lista_num_alum_x_carrera.RData")
}


# Ej. ---------------------------------------------------------------------
guarda_num_alum_x_carrera(param,param_sim)
