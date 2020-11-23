##########################################################################
#' En este programa se encuentra la función que corrige los seminarios
#' para cada m_grande.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")



# corrige_sem_CdC --------------------------------------------------------
#' Title corrige_sem_CdC: Función que corrige los renglones de cada 
#' "m_grande" que tienen las materias: "Animación por Computadora" y
#' "Computación Distribuida".
#'
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @examples
#' corrige_sem_CdC(param)
#' 
corrige_sem_CdC <- function(param){
  #Se definen las variables que se van a utilizar
  semestres <- param$sem_totales
  num_col_Materia <- arroja_ind_col_MG("Materia")##1
  num_col_Cambios <- arroja_ind_col_MG("Cambios")##12
  num_col_NomMat_Act2000 <- arroja_ind_col_MG("NomMat_Act2000")##23
  num_col_NomMat_MAp2017 <- arroja_ind_col_MG("NomMat_MAp2017")##29
  num_col_NumMateria <- arroja_ind_col_MG("Num_materia")##37
  materia <- "Seminario de Ciencias de la Computación"
  vec_info_anim_comp <- arroja_nom_correcto("Animación por Computadora")
  vec_info_comp_distrib <- arroja_nom_correcto("Computación Distribuida")
  vec_comp_distrib <- c("Principios de Computación Distribuida",
                        "Computación Concurrente",
                        vec_info_comp_distrib[1])
  
  for(s in 1:(length(semestres)-1)){
    sem_info <- semestres[s]
    nom_m_grande <- paste0("m_grande por semestre/m_grande_",sem_info,".RData")
    load(nom_m_grande)
    
    indices <- checa_ind_materia(materia,m_grande)
    
    for(r in indices){#Recorre sólo los índices de los renglones que queremos cambiar
      vec_m_grande <- m_grande[r,num_col_NomMat_Act2000:num_col_NomMat_MAp2017]
      
      #Verificamos para "Animación por Computadora"
      if(any(vec_info_anim_comp[1] == vec_m_grande)){
        m_grande[r,c(num_col_Materia,num_col_Cambios,
                     num_col_NumMateria)] <- c(vec_info_anim_comp[1],
                                               paste0(m_grande[r,num_col_Cambios],"/1"),
                                               vec_info_anim_comp[2])
      }
      
      #Verificamos para "Computación Distribuida"
      if(any(vec_comp_distrib == vec_m_grande)){
        m_grande[r,c(num_col_Materia,num_col_Cambios,
                     num_col_NumMateria)] <- c(vec_info_comp_distrib[1],
                                               paste0(m_grande[r,num_col_Cambios],"/1"),
                                               vec_info_comp_distrib[2])
      }
    }#Fin for(r)
    save(m_grande,file = nom_m_grande)
  }#Fin for(s)
}



# Ej. ---------------------------------------------------------------------

corrige_sem_CdC(param)
