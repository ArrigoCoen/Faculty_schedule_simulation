##########################################################################
#' En este programa se encuentra la función que agrega materias al vector
#' "vec_nom_materias_total".
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")


# agrega_nom_1_materia_en_vec ---------------------------------------------
#' Title agrega_nom_1_materia_en_vec: Función que recibe como parámetros el
#' nombre de una materia y el vector con los nombres de las materias y
#' guarda la materia en caso de dar la opción de "SI". También imprime una
#' lista con los diferentes nombres que pudiera tener "materia" en
#' "vec_nom_materias_total"
#' Por ejemplo "Estadística III" y "Modelos de Supervivencia y de Series
#' de Tiempo".
#'
#' @param materia: Nombre de algún curso impartido en la FC.
#' @param vec_nom_materias_total: Vector que contiene el nombre de las
#' materias sin repetición, conservando los nombres más recientes. 
#'
#' @examples
#' agrega_nom_1_materia_en_vec(materia,vec_nom_materias_total)
#'
agrega_nom_1_materia_en_vec <- function(materia,vec_nom_materias_total){
  #' Se carga la matriz m_grande_total de 2008-1 a 2020-1 de la cual
  #' se va a obtener la lista de nombres que se desea
  load("Matrices m_grande_total/m_grande_total_20081_20201.RData")
  
  #Se definen las variables que se van a utilizar
  num_col_Materia <- arroja_ind_col_MG("Materia")##1
  num_col_NomMat_Act2000 <- arroja_ind_col_MG("NomMat_Act2000")##23
  num_col_NomMat_MAp2017 <- arroja_ind_col_MG("NomMat_MAp2017")##29
  vec_materias <- unique(m_grande_total[,num_col_Materia])##531
  
  var_aux <- vec_nom_materias_total[vec_nom_materias_total == materia]
  # var_aux <- vec_nom_materias_total[vec_nom_materias_total == "materia"]
  if(length(var_aux) != 0){#Si el nombre de "materia" se encuentra en el vector
    cat("\n\n La materia ",materia," se encuentra en el vector como ",materia)
  }else{
    ind_materia <- checa_ind_materia(materia,m_grande_total)
    
    if(length(ind_materia) != 0){#Si "materia" se encuentra en el vector con otro nombre
      mat_aux <- m_grande_total[ind_materia,c(num_col_Materia,
                                              num_col_NomMat_Act2000:num_col_NomMat_MAp2017)]
      mat_aux <- unique(mat_aux)
      vec_aux <- mat_aux[mat_aux != 0]
      nom_aux <- 0
      
      for(k in 1:length(vec_aux)){
        nom_aux <- c(nom_aux,vec_nom_materias_total[vec_nom_materias_total == vec_aux[k]])
      }
      if(length(nom_aux) > 1){
        #Se quita el cero inicial
        nom_aux <- nom_aux[-1]
        cat("\n\n La materia ",materia," se encuentra en el vector como \n",nom_aux) 
      }
    }else{#Si "materia" NO se encuentra en el vector
      cat("\n\n La materia ",materia," NO se encuentra en el vector")
      cat("\nAgregar la materia al vector: \n (1) SI \n (0) NO")
      agrega_materia = scan(file = "", what = numeric(), n = 1)
      
      if(agrega_materia == 1){
        vec_nom_materias_total <- c(vec_nom_materias_total,materia)
        save(vec_nom_materias_total, file = "vec_nom_materias_total.RData")
        cat("\n La materia ",materia," se agregó al vector")
      }else{
        cat("\n La materia ",materia," NO se agregó al vector")}}}
}



# Ej. ---------------------------------------------------------------------
materia <- "Taller de Redacción"
vec_nom_materias_total <- param$vec_nom_materias_total
agrega_nom_1_materia_en_vec(materia,vec_nom_materias_total)

load(file = "vec_nom_materias_total.RData")
param$vec_nom_materias_total = vec_nom_materias_total
vec_nom_materias_total <- param$vec_nom_materias_total
materia <- "Solución Numérica de Ecuaciones Diferenciales Ordinarias"
agrega_nom_1_materia_en_vec(materia,vec_nom_materias_total)

load(file = "vec_nom_materias_total.RData")
param$vec_nom_materias_total = vec_nom_materias_total
vec_nom_materias_total <- param$vec_nom_materias_total



