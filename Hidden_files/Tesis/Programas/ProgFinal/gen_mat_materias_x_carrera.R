##########################################################################
#' En este programa se encuentra la función que genera la matriz que
#' contiene los nombres de las materias de cada carrera y el número de 
#' cada materia.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")


# gen_mat_materias_x_carrera ----------------------------------------------
#' Title gen_mat_materias_x_carrera: Función que genera la lista con las
#' matrices que tienen los nombres de las materias de cada carrera y el
#' número de materia para cada materia.
#'
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
#' @return lista_mat_materias_x_carrera: Lista con las matrices que tienen
#' los nombres de las materias de cada carrera y el número de cada materia.
#'
#' @examples
#' lista_mat_materias_x_carrera <- gen_mat_materias_x_carrera(param)
#' 
gen_mat_materias_x_carrera <- function(param){
  #Se definen las variables que se van a utilizar
  lista_materias_x_carrera <- carga_info_materias_x_carrera()
  materias_act <- lista_materias_x_carrera[[1]]
  materias_CdC <- lista_materias_x_carrera[[2]]
  materias_mate <- lista_materias_x_carrera[[3]]
  materias_mateAp <- lista_materias_x_carrera[[4]]
  mat_materias_act <- data.frame(Materia = materias_act,Num_materia = 0)
  mat_materias_CdC <- data.frame(Materia = materias_CdC,Num_materia = 0)
  mat_materias_mate <- data.frame(Materia = materias_mate,Num_materia = 0)
  mat_materias_mateAp <- data.frame(Materia = materias_mateAp,Num_materia = 0)
  
  vec_carrera <- c("act","CdC","mate","mateAp")
  for(d in 1:length(vec_carrera)){
    # cat("\n d = ",d)
    carrera <- vec_carrera[d]
    switch(carrera,
           'act' = {cota = length(materias_act)
           mat_aux <- mat_materias_act},
           'CdC' = {cota = length(materias_CdC)
           mat_aux <- mat_materias_CdC},
           'mate' = {cota = length(materias_mate)
           mat_aux <- mat_materias_mate},
           'mateAp' = {cota = length(materias_mateAp)
           mat_aux <- mat_materias_mateAp}
    )
    for(r in 1:cota){
      # cat("\n r = ",r)
      materia <- mat_aux[r,1]
      num_materia <- which(materia == param$vec_nom_materias_total)
      if(length(num_materia) > 0){
        mat_aux[r,2] <- num_materia
      }else{
        mat_aux[r,2] <- 0
      }
    }
    switch(carrera,
           'act' = {mat_materias_act <- mat_aux},
           'CdC' = {mat_materias_CdC <- mat_aux},
           'mate' = {mat_materias_mate <- mat_aux},
           'mateAp' = {mat_materias_mateAp <- mat_aux}
    )
  }#Fin for(d)
  
  mat_materias_act[33,2] <- 297
  mat_materias_mateAp[65,2] <- 333
  
  lista_mat_materias_x_carrera <- list()
  lista_mat_materias_x_carrera[[1]] <- mat_materias_act
  lista_mat_materias_x_carrera[[2]] <- mat_materias_CdC
  lista_mat_materias_x_carrera[[3]] <- mat_materias_mate
  lista_mat_materias_x_carrera[[4]] <- mat_materias_mateAp
  
  save(lista_mat_materias_x_carrera,
       file = paste0("lista_mat_materias_x_carrera.RData"))
}

# Ej. ---------------------------------------------------------------------

gen_mat_materias_x_carrera(param)
