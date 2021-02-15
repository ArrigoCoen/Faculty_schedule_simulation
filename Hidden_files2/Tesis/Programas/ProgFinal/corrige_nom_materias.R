##########################################################################
#' En este programa se encuentra las funciones encargadas de corregir los
#' nombres de las materias para que no haya repeticiones.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")



# arroja_nom_correcto -----------------------------------------------------
#' Title arroja_nom_correcto: Función que arroja un vector con el nombre
#' correcto de la materia ingresada y su número de materia.
#'
#' @param nom_materia: Nombre de alguna materia impartida en la FC.
#'
#' @return vec_info_nombre: Vector con el nombre correcto de la materia
#' ingresada y su número de materia.
#'
#' @examples
#' vec_info_nombre <- arroja_nom_correcto("Estadística I")
#' vec_info_nombre <- arroja_nom_correcto(nom_materia)
#' 
arroja_nom_correcto <- function(nom_materia){
  load("mat_nom_materias_total.RData")
  #Se definen las variables que se van a utilizar
  # vec_info_nombre <- rep(0,2)
  mat_nom_materias_total
  which(nom_materia == mat_nom_materias_total[,3:dim(mat_nom_materias_total)[2]])
  
  for(d in 1:dim(mat_nom_materias_total)[1]){
    ind <- which(nom_materia == mat_nom_materias_total[d,3:dim(mat_nom_materias_total)[2]])
    if(length(ind) > 0){
      vec_info_nombre <- c(mat_nom_materias_total[d,1],d)
    }
  }
  
  cat("\n La materia ",nom_materia," se llama: ",vec_info_nombre[1],
      ". Su número de materia es: ",vec_info_nombre[2])
  return(vec_info_nombre)
}


# Ej. ---------------------------------------------------------------------
nom_materia <- "Administración"
vec_info_nombre <- arroja_nom_correcto(nom_materia)

nom_materia <- "Estadística I"
vec_info_nombre <- arroja_nom_correcto(nom_materia)
nom_materia <- "Inferencia Estadística"
vec_info_nombre <- arroja_nom_correcto(nom_materia)




# actualiza_col_num_materia -----------------------------------------------
#' Title actualiza_col_num_materia: Función que actualiza las matrices
#' "m_grande" con el nombre correcto para las materias y también actualiza
#' el número de materia en caso de ser necesario.
#'
#' se deben de tomar en cuenta al crear "m_grande".
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @examples
#' actualiza_col_num_materia(param)
#' 
actualiza_col_num_materia <- function(param){
  #Se definen las variables que se van a utilizar
  # semestres <- param$sem_totales
  semestres <- param$sem_totales[20:length(param$sem_totales)]
  num_col_Materia <- arroja_ind_col_MG("Materia")##1
  num_col_Profesor <- arroja_ind_col_MG("Profesor")##2
  num_col_Cambios <- arroja_ind_col_MG("Cambios")##12
  num_col_NumMateria <- arroja_ind_col_MG("Num_materia")##37
  
  for(s in 1:(length(semestres)-1)){
    sem_info <- semestres[s]
    nom_m_grande <- paste0("m_grande por semestre/m_grande_",sem_info,".RData")
    load(nom_m_grande)
    m_grande <- m_grande[m_grande[,num_col_Profesor]!=0,]
    m_grande <- m_grande[!is.na(m_grande[,num_col_Materia]),]
    
    for(r in 1:dim(m_grande)[1]){#Recorre renglones
      nom_materia <- m_grande[r,num_col_Materia]
      vec_info_nombre <- arroja_nom_correcto(nom_materia)
      if(vec_info_nombre[1] != 0){
        #Cuando si se encuentra el nombre de
        m_grande[r,num_col_Materia] <- vec_info_nombre[1]
        
        if(m_grande[r,num_col_NumMateria] != vec_info_nombre[2]){
          #' En caso de que se haya cambiado el número de materia se registra
          #' en la columna de cambios.
          m_grande[r,num_col_Cambios] <- paste0(m_grande[r,num_col_Cambios],"/5")
        }
      }
      m_grande[r,num_col_NumMateria] <- vec_info_nombre[2]
    }#Fin for(r)
    save(m_grande,file = nom_m_grande)
  }#Fin for(d)
}



# Ej. ---------------------------------------------------------------------
actualiza_col_num_materia(param)


