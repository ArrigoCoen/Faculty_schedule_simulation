##########################################################################
#' En este programa se encuentra la función que califica asignaciones.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")




# califica_asignacion ------------------------------------------------------
#' Title califica_asignacion: Función que califica un asignacion, se penaliza
#' en los siguientes casos:
#' - Penalización por no tener en el asignacion una materia que necesitamos.
#' Se resta 1 por cada materia no impartida.
#' -Penalización por cada alumno faltante: Se suma el número de alumnos que
#' quedaron en la matriz "mat_demanda_aux". Se multiplica alfa por el número
#' de alumnos faltantes
#' -Penalización por cada alumno sobrante: El número de alumnos sobrantes
#' es el número de alumnos simulados menos el número de alumnos asignados
#' (alumnos requeridos - alumnos faltantes). Se multiplica el número de
#' alumnos sobrantes por beta.
#' -Si algún profesor de tiempo completo pidió alguna materia y no se la
#' dieron. Se penaliza con uno por cada materia.
#' Notas:
#' 1) Se cuenta por materia solicitada, no por materia con
#' horario. Ej. Si se pidió Proba I a las 10hrs y a las 11hrs,
#' sólo se cuenta una penalización.
#' 2) Se penaliza por cada materia con tope a 2 asignaciones, i.e. si un
#' profesor pidió 3 o más  materias y sólo le dieron una, entonces se penaliza
#' con -1; si le dieron 2 entonces no hay penalización.
#' - Penalización por cada profesor que pueda impartir la materia j en la
#' hora i y esa entrada de la matriz "mat_demanda_aux" aún tenga alumnos. Se
#' penaliza por cada grupo que no se le asignó un profesor que si podía dar
#' clase.
#'
#' @param mat_demanda_alumnos: Matriz de 15 renglones (horas) y 201
#' columnas (materias). En la entrada (i,j) se tiene el número de alumnos
#' simulados para la hora i, y la materia j.
#' @param lista_info_esqueleto: Lista con las matrices:
#' 1) mat_esqueleto: Matriz de 15 renglones (horas) y 203 columnas
#' (materias). En la entrada (i,j) se tiene el número de grupos simulados
#' para la hora i, y la materia j.
#' 2) mat_prof_TC: Matriz de 2 columnas con el nombre de los profesores de
#' tiempo completo y el número de materias asignadas.
#' 3) mat_prof_asig: Matriz de 2 columnas con el nombre de los profesores
#' de asignatura y el número de materias asignadas.
#' 4) lista_ciclo_asig: Lista 
#' 5) mat_solicitudes_TC: Matriz de solicitudes de los profesores de tiempo
#' completo.
#' 6) mat_solicitudes_asignatura: Matriz de solicitudes de los profesores
#' de asignatura.
#' 7) num_alum_simulados: Variable tipo numeric, con el número de alumnos
#' simulados totales.
#' 8) mat_E: Matriz de  15 renglones (horas) y 203 columnas (materias).
#' En la entrada (i,j) se tiene el número de alumnos simulados
#' para la hora i, y la materia j.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return calif_asignacion: Variable tipo numeric que indica la calificación
#' del asignacion.
#'
#' @examples
#' calif_asignacion <- califica_asignacion(mat_demanda_alumnos,
#' lista_info_esqueleto,param)
#' 
califica_asignacion <- function(mat_demanda_alumnos,lista_info_esqueleto,
                               param){
  #Se definen las variables que se van a utilizar
  vec_nom_materias_total <- param$vec_nom_materias_total
  mat_esqueleto <- lista_info_esqueleto[[1]]
  mat_prof_TC <- lista_info_esqueleto[[2]]
  # mat_prof_asig <- lista_info_esqueleto[[3]]
  mat_demanda_aux <- lista_info_esqueleto[[4]]
  mat_solicitudes_TC <- lista_info_esqueleto[[5]]
  mat_solicitudes_asignatura <- lista_info_esqueleto[[6]]
  (num_alum_simulados <- lista_info_esqueleto[[7]])#34750
  calif_esqueleto <- 0
  
  #' Penalización por no tener en el esqueleto una materia que necesitamos.
  #' Se resta 1 por cada materia no impartida.
  materias_no_impartidas <- 0
  nom_materias_no_impartidas <- 0
  
  for(c in 1:dim(mat_esqueleto)[2]){#Se recorren las columnas
    suma_col <- sum(mat_demanda_alumnos[,c])
    if(suma_col!=0 && sum(mat_demanda_aux[,c])>=suma_col){
      materias_no_impartidas <- materias_no_impartidas + 1
      nom_materias_no_impartidas <- c(nom_materias_no_impartidas,
                                      vec_nom_materias_total[c])
      # cat("\n No fue impartida la materia: ",vec_nom_materias_total[c])
    }
  }
  materias_no_impartidas#4
  #Quitamos el cero inicial
  nom_materias_no_impartidas <- nom_materias_no_impartidas[-1]
  nom_materias_no_impartidas
  
  
  #' Penalización por cada alumno faltante: Se suma el número de alumnos
  #' que quedaron en la matriz "mat_demanda_aux". Se multiplica alfa
  #' por el número de alumnos faltantes
  alfa <- 0.5
  (num_alum_faltantes <- sum(mat_demanda_aux[mat_demanda_aux>0]))#10,930
  (pena_faltantes <- alfa*num_alum_faltantes)#5465
  
  
  #' Penalización por cada alumno sobrante: El número de alumnos
  #' sobrantes es el número de alumnos simulados menos el número de
  #' alumnos asignados (alumnos requeridos - alumnos faltantes)
  #' Se multiplica el número de alumnos sobrantes por beta.
  beta <- 0.8
  # (num_alum_requeridos <- sum(mat_demanda_alumnos))#34,955
  # (num_alum_asignados <- num_alum_requeridos-num_alum_faltantes)#13,000
  # (num_alum_sobrantes <- max(0,num_alum_simulados-num_alum_asignados))#10,354
  (num_alum_sobrantes <- -sum(mat_demanda_aux[mat_demanda_aux<0]))#10187
  (pena_sobrantes <- beta*num_alum_sobrantes)#8149.6
  
  
  #' Si algún profesor de tiempo completo pidió alguna materia y
  #' no se la dieron. Se penaliza con uno por cada materia.
  #' 
  #' Notas:
  #' 1) Se cuenta por materia solicitada, no por materia con
  #' horario. Ej. Si se pidió Proba I a las 10hrs y a las 11hrs,
  #' sólo se cuenta una penalización.
  #' 2) Se penaliza por cada materia con tope a 2 asignaciones,
  #' i.e. si un profesor pidió 3 o más  materias y sólo le dieron 1,
  #' entonces se penaliza 1; si le dieron 2 entonces no hay
  #' penalización.
  # mat_prof_TC_menor_2 <- mat_prof_TC[mat_prof_TC[,2]<=1,]
  # mat_solicitudes_TC_aux <- unique(mat_solicitudes_TC[,c(1,3,4)])
  pena_x_materia <- 0
  mat_prof_TC_igual_1 <- mat_prof_TC[mat_prof_TC[,2]==1,]
  mat_prof_TC_igual_0 <- mat_prof_TC[mat_prof_TC[,2]==0,]
  
  ## Una materia
  for(r in 1:dim(mat_prof_TC_igual_1)[1]){#Recorre los renglones
    nom_prof <- mat_prof_TC_igual_1[r,1]
    if(any(nom_prof == mat_solicitudes_TC[,1])){
      pena_x_materia <- pena_x_materia + 1
    }
  }
  
  ## Dos materias
  for(r in 1:dim(mat_prof_TC_igual_0)[1]){#Recorre los renglones
    nom_prof <- mat_prof_TC_igual_0[r,1]
    if(any(nom_prof == mat_solicitudes_TC[,1])){
      pena_x_materia <- pena_x_materia + 2
    }
  }
  pena_x_materia##72
  
  
  #' Penalización por cada profesor que pueda impartir la materia j en
  #' la hora i y esa entrada de la matriz "mat_demanda_aux" aún tenga
  #' alumnos. Se penaliza por cada grupo que no se le asignó un profesor
  #' que si podía dar clase.
  # media_alum <- 34.18746
  mat_solicitudes <- rbind(mat_solicitudes_TC,mat_solicitudes_asignatura)
  mat_solicitudes <- mat_solicitudes[mat_solicitudes[,4] > 0,]
  mat_i_j <- matrix(0,nrow = dim(mat_solicitudes)[1],ncol = 2)
  pena_gpos_sin_prof <- 0
  
  for(r in 1:dim(mat_solicitudes)[1]){#Recorre los renglones
    #' Se llenan los índices en los que un profesor puede dar
    #' la materia j en la hora i
    ind_hora <- which(7:21 == mat_solicitudes[r,5])
    mat_i_j[r,] <- c(ind_hora,mat_solicitudes[r,4])
  }
  colnames(mat_i_j) <- c("i","j")
  
  for(r in 1:dim(mat_i_j)[1]){#Recorre renglones
    # cat("\n r = ",r)
    i <- as.numeric(mat_i_j[r,1])
    j <- as.numeric(mat_i_j[r,2])
    if(mat_demanda_aux[i,j] > 0){
      pena_gpos_sin_prof <- pena_gpos_sin_prof + 1
    }
  }
  pena_gpos_sin_prof#148
  
  #' Si hay alumnos que necesitan una clase a alguna hora y no
  #' existe profesor que la imparta.
  
  
  calif_asignacion <- -sum(materias_no_impartidas,pena_faltantes,pena_sobrantes,
                          pena_x_materia,pena_gpos_sin_prof)#-13854.6
  return(calif_asignacion)
}
