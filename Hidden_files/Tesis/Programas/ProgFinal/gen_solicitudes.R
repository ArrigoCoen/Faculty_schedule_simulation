##########################################################################
#' En este programa se encuentran las funciones encargadas de generar la
#' matriz con las solicitudes de los profesores.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")



# gen_solicitudes_1_profesor ----------------------------------------------
#' Title gen_solicitudes_1_profesor: Función que genera la solicitud de 
#' un solo profesor. Arroja la matriz "mat_1_solicitud" de 4 columnas
#' (Profesor,TC,Materia, Horario) y 6 renglones que tiene la información de
#' la solicitud de "nom_prof". Se eligen 2 materias y hasta 3 diferentes
#' horarios.
#'
#' @param nom_prof: Nombre del profesor del que se va a obtener la solicitud.
#' @param tipo_prof: Variable binaria que vale 1 si el profesor es de
#' tiempo completo y cero si no.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' 
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return mat_1_solicitud: Matriz de 4 columnas (Profesor,TC,Materia,
#' Horario) y 6 renglones que tiene la información de la solicitud de
#' "nom_prof". Se eligen 2 materias y hasta 3 diferentes horarios.
#'
#' @examples
#' mat_1_solicitud <- gen_solicitudes_1_profesor("Arrigo Coen",0,param)
#' mat_1_solicitud <- gen_solicitudes_1_profesor("Margarita Chávez",1,param)
#' 
gen_solicitudes_1_profesor <- function(nom_prof,tipo_prof,param){
  #Se definen las variables que se van a utilizar
  num_col_Profesor <- arroja_ind_col_MG("Profesor")
  num_col_horario_num <- arroja_ind_col_MG("horario_num")##4
  num_col_NumMateria <- arroja_ind_col_MG("Num_materia")##37
  vec_nom_materias_total <- param$vec_nom_materias_total#333
  m_grande_2015 <- param$m_grande_2015#8393 37
  mat_1_solicitud <- data.frame(Profesor = 0,TC = 0, Materia = rep(0,6),
                                Horario = 0)
  
  #Se definen las variables con la información de "nom_prof"
  mat_1_prof <- m_grande_2015[m_grande_2015[,num_col_Profesor]==nom_prof,]
  materias_num_prof <- unique(mat_1_prof[,num_col_NumMateria])
  horas_prof <- unique(mat_1_prof[,num_col_horario_num])
  
  #Se llena las primeras 2 columnas
  mat_1_solicitud[,1] <- nom_prof
  mat_1_solicitud[,2] <- tipo_prof
  
  #Se llena la columna "Materia"
  if(length(materias_num_prof)==2){
    mat_1_solicitud[1:3,3] <- vec_nom_materias_total[materias_num_prof[1]]
    mat_1_solicitud[4:6,3] <- vec_nom_materias_total[materias_num_prof[2]]
  }else if(length(materias_num_prof)==1){
    mat_1_solicitud[,3] <- vec_nom_materias_total[materias_num_prof]
  }else if(length(materias_num_prof)>2){
    muestra_materias <- sample(materias_num_prof,size = 2)
    mat_1_solicitud[1:3,3] <- vec_nom_materias_total[muestra_materias[1]]
    mat_1_solicitud[4:6,3] <- vec_nom_materias_total[muestra_materias[2]]
  }
  
  #Se llena la columna "Horario"
  #' A lo más van a tener 3 horas diferentes
  if(length(horas_prof)==3){
    mat_1_solicitud[c(1,4),4] <- horas_prof[1]
    mat_1_solicitud[c(2,5),4] <- horas_prof[2]
    mat_1_solicitud[c(3,6),4] <- horas_prof[3]
  }else if(length(horas_prof)==2){
    mat_1_solicitud[c(1:2,4:5),4] <- horas_prof[1]
    mat_1_solicitud[c(3,6),4] <- horas_prof[2]
  }else if(length(horas_prof)==1){
    mat_1_solicitud[,4] <- horas_prof
  }else if(length(horas_prof)>3){
    muestra_horas <- sample(horas_prof,size = 3)
    mat_1_solicitud[c(1,4),4] <- muestra_horas[1]
    mat_1_solicitud[c(2,5),4] <- muestra_horas[2]
    mat_1_solicitud[c(3,6),4] <- muestra_horas[3]
  }
  
  return(mat_1_solicitud)
}


# gen_solicitudes ---------------------------------------------------------
#' Title gen_solicitudes: Función que genera la solicitud de todos los
#' profesores en la matriz "mat_nom_prof_total". Arroja la matriz
#' "mat_solicitudes" de 4 columnas (Profesor,TC,Materia, Horario). Tiene la
#' información de las solicitudes de los profesores. Se eligen hasta dos
#' materias y hasta 3 diferentes horarios. Se quitan los renglones repetidos.
#'
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' 
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return mat_solicitudes: Matriz de 4 columnas (Profesor,TC,Materia,
#' Horario). Tiene la información de las solicitudes de los profesores. Se
#' eligen hasta dos materias y hasta 3 diferentes horarios. Se quitan los
#' renglones repetidos.
#'
#' @examples
#' mat_solicitudes <- gen_solicitudes(param)
#' 
gen_solicitudes <- function(param){
  # Start the clock!
  ptm <- proc.time()
  
  #Se definen las variables que se van a utilizar
  num_col_Profesor <- arroja_ind_col_MG("Profesor")
  mat_nom_prof_total <- param$mat_nom_prof_total#1387 2
  m_grande_2015 <- param$m_grande_2015#8409 37
  mat_solicitudes <- data.frame(Profesor = 0,TC = 0, Materia = 0,
                                Horario = 0)
  
  #' Se quitan los renglones de ceros, con NA o vaciós en la
  #' columna de "Profesor"
  m_grande_2015 <- m_grande_2015[m_grande_2015[,num_col_Profesor]!="",]
  m_grande_2015 <- m_grande_2015[m_grande_2015[,num_col_Profesor]!=0,]
  m_grande_2015 <- m_grande_2015[!is.na(m_grande_2015[,num_col_Profesor]),]
  # dim(m_grande_2015)#8393 37
  param$m_grande_2015 = m_grande_2015#8393 37 ############
  
  
  #Recorre el nombre de los profesores de la matriz "mat_nom_prof_total"
  for(p in 1:dim(mat_nom_prof_total)[1]){
    nom_prof <- mat_nom_prof_total[p,1]
    tipo_prof <- mat_nom_prof_total[p,2]
    mat_1_solicitud <- gen_solicitudes_1_profesor(nom_prof,tipo_prof,param)
    mat_solicitudes <- rbind(mat_solicitudes,mat_1_solicitud)
  }
  #Se quita el renglón inicial de ceros
  mat_solicitudes <- mat_solicitudes[mat_solicitudes[,1]!=0,]#8322
  #Se deja la matriz sin repeticiones
  mat_solicitudes <- unique(mat_solicitudes)#4792
  
  cat("La función gen_solicitudes se tardó: ",(proc.time()-ptm)[3]/60," minutos\n\n\n" )
  
  return(mat_solicitudes)
}


