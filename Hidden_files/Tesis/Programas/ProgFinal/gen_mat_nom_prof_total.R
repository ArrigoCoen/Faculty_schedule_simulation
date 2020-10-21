##########################################################################
#' En este programa se encuentran la función que carga la matriz
#' "m_grande_total" de los semestres 2008-1 a 2020-1 de la cual se va
#' obtiene la lista de nombres de los profesores sin repetición. La matriz
#' "mat_nom_prof_total" tiene 2 columnas, en la primera se tiene el nombre
#' de los profesores y en la segunda se tiene un 1 si el profesor es de
#' tiempo completo y 0 si no.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Programas/ProgFinal V01")
source("Fn_Asignacion.R")

# gen_mat_nom_prof_total --------------------------------------------------
#' Title gen_mat_nom_prof_total: Función que carga la matriz
#' "m_grande_total" de los semestres 2008-1 a 2020-1 de la cual se va
#' obtiene la lista de nombres de los profesores sin repetición. La matriz
#' "mat_nom_prof_total" tiene 2 columnas, en la primera se tiene el nombre
#' de los profesores y en la segunda se tiene un 1 si el profesor es de
#' tiempo completo y 0 si no.
#'
#' @return mat_nom_prof_total: Matriz de 2 columnas, en la primera se
#' tiene el nombre de los profesores y en la segunda se tiene un 1 si
#' el profesor es de tiempo completo y 0 si no.
#'
gen_mat_nom_prof_total <- function(){
  #Se definen las variables que se van a utilizar:
  mat_nom_prof_total <- data.frame(Profesor = 0,Tiempo_Completo = 0)
  num_col_Profesor <- arroja_ind_col_MG("Profesor")##2
  
  #' Se carga la matriz m_grande_total de 2008-1 a 2020-1 de la cual
  #' se va a obtener la lista de nombres que se desea
  load("Matrices m_grande_total/m_grande_total_20151_20201.RData")
  # View(m_grande_total)
  
  
  
  
  
  
  
  for(r in 1:dim(m_grande_total)[1]){#Se recorren los renglones
    renglon <- m_grande_total[r,num_col_Act2000:num_col_MAp2017]
    # cat("\n renglon = ",renglon)
    # print(renglon)
    if(sum(as.numeric(renglon)) == 1){##Se toman las materias que tengan un sólo nombre
      mat_nom_prof_total <- c(mat_nom_prof_total,m_grande_total[r,num_col_Materia])
    }else if(sum(as.numeric(renglon)) > 1){
      ##Se toman las materias que tengan más de un nombre, conservando el más reciente
      # Se reordenan las columnas para que queden los datos ordenados de acuerdo
      #al plan de estudios, del más antiguo al más reciente.
      renglon_aux <- c(m_grande_total[r,num_col_Mat1983],
                       m_grande_total[r,num_col_CdC1994],
                       m_grande_total[r,num_col_Act2000],
                       m_grande_total[r,num_col_Act2006],
                       m_grande_total[r,num_col_CdC2013],
                       m_grande_total[r,num_col_Act2015],
                       m_grande_total[r,num_col_MAp2017])
      reng_aux_nom <- c(m_grande_total[r,num_col_NomMat_Mat1983],
                        m_grande_total[r,num_col_NomMat_CdC1994],
                        m_grande_total[r,num_col_NomMat_Act2000],
                        m_grande_total[r,num_col_NomMat_Act2006],
                        m_grande_total[r,num_col_NomMat_CdC2013],
                        m_grande_total[r,num_col_NomMat_Act2015],
                        m_grande_total[r,num_col_NomMat_MAp2017])
      var_aux <- 0
      for(c in length(renglon_aux):1){
        # cat("\n c = ",c)
        if(var_aux==0 && renglon_aux[c]==1){
          mat_nom_prof_total <- c(mat_nom_prof_total,reng_aux_nom[c])
          var_aux <- 1
        }#if
      }#for c
    }#if else
  }#for r
  
  #Se toman los nombres sin repetición
  mat_nom_prof_total <- unique(mat_nom_prof_total)
  
  #Se quitan los ceros
  mat_nom_prof_total <- mat_nom_prof_total[mat_nom_prof_total!=0]
  save(mat_nom_prof_total, file = "mat_nom_prof_total.RData")
  
  return(mat_nom_prof_total)
}
