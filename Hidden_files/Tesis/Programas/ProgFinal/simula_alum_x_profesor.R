##########################################################################
#' En este programa se encuentra la función que simula el número de alumnos
#' para un profesor y una materia. Se obtiene la información del número de
#' alumnos que ha tenido el profesor (del 2015-1 al 2020-1), se toma el
#' mín y el máx, se simula una uniforme en ese intervalo, se redondea el
#' valor con la función ceiling y así se obtiene el valor simulado.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")


# simula_alum_x_profesor --------------------------------------------------
#' Title simula_alum_x_profesor: Función que simula el número de alumnos
#' para un profesor y una materia. Se obtiene la información del número de
#' alumnos que ha tenido el profesor (del 2015-1 al 2020-1), se toma el
#' mín y el máx, se simula una uniforme en ese intervalo, se redondea el
#' valor con la función ceiling y así se obtiene el valor simulado.
#'
#' @param renglon: Vector con la información del profesor elegido para
#' asignarle un grupo.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' 
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return num_alum_x_profesor: Número de alumnos simulados de un profesor
#' y una materia.
#'
#' @examples
#' num_alum_x_profesor <- simula_alum_x_profesor(renglon,param)
#' 
simula_alum_x_profesor <- function(renglon,param){
  #Se definen las variables que se van a utilizar
  nom_prof <- as.character(renglon[1])
  num_materia <- as.numeric(renglon[4])
  num_col_Profesor <- arroja_ind_col_MG("Profesor")##2
  num_col_Alumnos <- arroja_ind_col_MG("Alumnos")##6
  num_col_NumMateria <- arroja_ind_col_MG("Num_materia")##37
  m_grande_2015 <- param$m_grande_2015
  m_grande_2015 <- m_grande_2015[!is.na(m_grande_2015[,num_col_Profesor]),]
  sub_mat_prof <- m_grande_2015[m_grande_2015[,num_col_Profesor]==nom_prof,]
  sub_mat <- sub_mat_prof[sub_mat_prof[,num_col_NumMateria]==num_materia,]
  
  num_Alumnos <- as.numeric(sub_mat[,num_col_Alumnos])
  num_alum_x_profesor <- ceiling(runif(1,min = min(num_Alumnos),
                                       max = max(num_Alumnos)))
  return(num_alum_x_profesor)
}


# Ej. ---------------------------------------------------------------------
simula_alum_x_profesor(renglon,param)
