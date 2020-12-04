##########################################################################
#' En este programa se encuentran la función que genera el esqueleto
#' final, después d ehaber aplicado el modelo de mezcla de normales al
#' número de alumnos y utilizado la función gen_esqueleto.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")



# gen_lista_info_esqueleto ------------------------------------------------
#' Title gen_lista_info_esqueleto: Función que arroja la lista
#' "lista_info_esqueleto" con el esqueleto que será utilizado para la
#' asignación de profesores y horarios.
#' Primero se aplicael modelo de mezcla de normales al número de alumnos y
#' utilizando la función gen_esqueleto se obtiene la lista.
#'
#' @param D_inicial: Matriz "mat_demanda_alumnos" de 15 renglones (horas)
#' y 203 columnas (materias). En la entrada (i,j) se tiene el número de
#' alumnos simulados para la hora i, y la materia j.
#' @param n_rep: Número de veces que se va a generar la matriz D con la
#' demanda de alumnos para el siguiente semestre.
#' 
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
#' @return lista_info_esqueleto: Lista con las matrices:
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
#'
#' @examples
#' lista_info_esqueleto <- gen_lista_info_esqueleto(D_inicial,n_rep,param)
#' 
gen_lista_info_esqueleto <- function(D_inicial,n_rep,param){
  ptm <- proc.time()# Start the clock!
  #' Definimos la lista en las que vamos a guardar el número de alumnos
  #' por materia
  num_alum_x_materia <- list()
  num_alum_x_materia[[1]] <- colSums(D_inicial)
  
  ##Convertimos los datos para obtener la distribución por horas
  wait_alumnos <- 0
  Horas <- param$Horas
  for(h in 1:length(Horas)){
    suma_x_hra <- sum(D_inicial[h,])
    if(suma_x_hra > 0){
      wait_alumnos <- c(wait_alumnos,rep(Horas[h],suma_x_hra))
    }
  }
  #Quitamos el cero inicial
  wait_alumnos <- wait_alumnos[-1]
  
  #' Definimos el modelo inicial
  mixmdl_1_D <- normalmixEM(wait_alumnos,k = 4)
  
  #' Obtenemos "n_rep" veces la información de la matriz con el número
  #' de alumnos simulados (D). Vamos guardando le número de alumnos
  #' por materia.
  for(d in 2:n_rep){
    cat("d = ",d)
    ### Obtener D
    D <- gen_mat_demanda_alumnos(param,param_sim)
    num_alum_x_materia[[d]] <- colSums(D)
    ##Convertimos los datos para obtener la distribución por horas
    for(h in 1:length(Horas)){
      suma_x_hra <- sum(D[h,])
      if(suma_x_hra > 0){
        wait_alumnos <- c(wait_alumnos,rep(Horas[h],suma_x_hra))
      }
    }#Fin for(h)
  }#Fin for(d)
  
  #'Definimos las matrices finales para calificar el esqueleto final
  ### Primero obtenemos el número promedio de grupos por materia
  mat_alum_x_materia <- matrix(0,nrow = n_rep,
                               ncol = length(param$vec_nom_materias_total))
  
  for(r in 1:n_rep){#Recorre las listas
    mat_alum_x_materia[r,] <- num_alum_x_materia[[r]]
  }
  prom_alum_x_materia <- ceiling(colMeans(mat_alum_x_materia))
  
  #Generamos la matriz D final
  D_final <- matrix(0,nrow = length(param$Horas),
                    ncol = length(param$vec_nom_materias_total))
  mixmdl_D <- normalmixEM(wait_alumnos,mixmdl_1_D$mu)#Modelo final
  
  for(c in 1:length(param$vec_nom_materias_total)){
    num_alum_1_materia <- prom_alum_x_materia[c]
    (rand_num <- sort(round(rnorm(num_alum_1_materia,mixmdl_D$mu,mixmdl_D$sigma))))
    ind_7 <- which(rand_num < 8)
    ind_22 <- which(rand_num >= 21)
    
    if(length(ind_7) > 0){
      rand_num[ind_7] <- 7
    }
    if(length(ind_22) > 0){
      rand_num[ind_22] <- 21
    }
    
    for(r in 1:length(param$Horas)){
      ind_hrs <- which(rand_num == param$Horas[r])
      if(length(ind_hrs) > 0){
        D_final[r,c] <- length(ind_hrs)
      }
    }
  }#Fin for(c)
  rownames(D_final) <- param$Horas
  colnames(D_final) <- param$vec_nom_materias_total
  View(D_final)
  
  
  ##Calificamos D_final
  mat_calif_x_gpo <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])
  ind_materias <- 1:dim(D)[2]
  calif_D <- actualiza_calif_D(D_inicial,D_final,mat_calif_x_gpo,ind_materias)
  mat_calif_x_gpo <- calif_D[[1]]
  vec_calif_x_materia <- calif_D[[2]]
  # View(mat_calif_x_gpo)
  # View(vec_calif_x_materia)
  (calif_esqueleto_2 <- sum(vec_calif_x_materia))#-1980.377/-1832.852
  cat("\n La suma de las calificaciones por materia es: ",calif_esqueleto_2)
  
  ##Generar esqueleto
  mat_solicitudes <- gen_solicitudes(param)#7.97 seg
  lista_info_esqueleto <- gen_esqueleto(D_final,mat_solicitudes,param)#10.76 seg
  
  cat("\n La función gen_lista_info_esqueleto tardó: ",(proc.time()-ptm)[3]/60,
      " minutos\n")
  return(lista_info_esqueleto)
}


# Ej. ---------------------------------------------------------------------
D_inicial <- gen_mat_demanda_alumnos(param,param_sim)#45.27 seg
# View(D_inicial)
n_rep <- 10

lista_info_esqueleto <- gen_lista_info_esqueleto(D_inicial,n_rep,param)#8.11 min









