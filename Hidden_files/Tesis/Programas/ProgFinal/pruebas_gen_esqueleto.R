##########################################################################
#' En este programa se encuentra la función que genera la matriz
#' "mat_esqueleto" la cual tiene 15 renglones (horas) y 333 columnas
#' (materias). En la entrada (i,j) se tiene el número de grupos simulados
#' para la hora i, y la materia j.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")



# ciclo_esqueleto ---------------------------------------------------------
#' Title ciclo_esqueleto: Función auxiliar de "gen_esqueleto" encargada de
#' realizar el ciclo. Recibe las matrices dependiendo si se está en el caso
#' de profesores de tiempo completo o de asignatura.
#'
#' @param cota: Cota para que el ciclo no sea infinito.
#' @param mat_solicitudes: Matriz de 5 columnas (Profesor,TC,Materia,
#' Num_Materia,Horario) que tiene la información de las solicitudes de los
#' profesores.
#' @param mat_prof: Matriz de 2 columnas con el nombre de los profesores y
#' el número de materias que se le han asignado.
#' @param mat_demanda: Matriz de 15 renglones (horas) y 333 columnas
#' (materias). En la entrada (i,j) se tiene el número de alumnos simulados
#' para la hora i, y la materia j.
#'
#' @return lista_ciclo: Lista con la matriz "mat_esqueleto", la matriz
#' "mat_prof" y la matriz "mat_demanda".
#'
#' @examples
#' lista_ciclo <- ciclo_esqueleto(1000,mat_solicitudes_TC,mat_prof_TC,mat_esqueleto)
#' lista_ciclo <- ciclo_esqueleto(6500,mat_solicitudes_asig,mat_prof_asig,mat_esqueleto)
#' 
ciclo_esqueleto <- function(cota,mat_solicitudes,mat_prof,mat_demanda){
  #Se definen las variables que se van a utilizar
  mat_esqueleto <- matrix(0,nrow = length(param$Horas),
                          ncol = length(param$vec_nom_materias_total))
  rownames(mat_esqueleto) <- 1:15
  colnames(mat_esqueleto) <- 1:333
  
  for(n in 1:cota){#Cota para que el ciclo no sea infinito
    # cat("\n Iteración: ",n)
    if(sum(mat_demanda)>0 && dim(mat_solicitudes)[1]>0){
      #Número aleatorio para elegir profesor
      (num_al <- round(runif(1,min = 1,max = dim(mat_prof)[1])))
      (profesor <- mat_prof[num_al,1])
      mat_aux <- mat_solicitudes[mat_solicitudes[,1]==profesor,]
      
      if(dim(mat_aux)[1]>0){#Si hay información de "profesor"
        #Número aleatorio para elegir materia
        (num_al_2 <- round(runif(1,min = 1,max = dim(mat_aux)[1])))
        (materia_al <- mat_aux[num_al_2,3])
        (num_materia_al <- mat_aux[num_al_2,4])
        
        #Número aleatorio para elegir horario
        (num_al_3 <- round(runif(1,min = 1,max = dim(mat_aux)[1])))
        (hora_al <- mat_aux[num_al_3,5])
        
        (renglon <- c(profesor,1,materia_al,num_materia_al,hora_al))
        
        #Índices de renglón y columna para "mat_esqueleto"
        (M_i <- which(param$Horas == as.numeric(renglon[5])))
        (M_j <- as.numeric(renglon[4]))
        
        #Se verifica si la demanda ha sido cubierta o no
        (sobran_alum_1si_0no <- verifica_demanda_cubierta(mat_demanda,renglon))
        
        if(sobran_alum_1si_0no == 1){#Aún hay alumnos sin clase para esa materia
          #Simulamos el número de alumnos para este grupo
          (num_alum_x_profesor <- simula_alum_x_profesor(renglon,param))
          
          #Se actualizan las entradas de las matrices auxiliares
          mat_demanda[M_i,M_j] <- max(0,mat_demanda[M_i,M_j]-num_alum_x_profesor)
          mat_esqueleto[M_i,M_j] <- mat_esqueleto[M_i,M_j] + 1
          
          mat_prof[num_al,2] <- as.numeric(mat_prof[num_al,2]) + 1
          prof_mas_2 <- 0
          if(mat_prof[num_al,2] >= 2){
            prof_mas_2 <- 1
          }
          mat_solicitudes <- actualiza_mat_solicitudes(mat_solicitudes,
                                                          renglon,prof_mas_2)
        }#Fin if(dim(mat_aux)[1]>0)
      }#Fin if Demanda existente
    }#Fin if Condiciones de paro
  }#Fin for(n)
  
  lista_ciclo <- list()
  lista_ciclo[[1]] <- mat_esqueleto
  lista_ciclo[[2]] <- mat_prof
  lista_ciclo[[3]] <- mat_demanda
  names(lista_ciclo) <- c("mat_esqueleto","mat_prof","mat_demanda")
  return(lista_ciclo)
}



# gen_esqueleto -----------------------------------------------------------
#' Title gen_esqueleto: Función que arroja una lista con las matrices:
#' 1) mat_esqueleto: Matriz de 15 renglones (horas) y 333 columnas
#' (materias). En la entrada (i,j) se tiene el número de grupos simulados
#' para la hora i, y la materia j.
#' 2) mat_prof_TC: Matriz de 2 columnas con el nombre de los profesores de
#' tiempo completo y el número de materias asignadas.
#' 3) mat_prof_asig: Matriz de 2 columnas con el nombre de los profesores de
#' asignatura y el número de materias asignadas.
#'
#' @param mat_demanda_alumnos: Matriz de 15 renglones (horas) y 333
#' columnas (materias). En la entrada (i,j) se tiene el número de alumnos
#' simulados para la hora i, y la materia j.
#' @param mat_solicitudes: Matriz de 4 columnas (Profesor,TC,Materia,
#' Horario). Tiene la información de las solicitudes de los profesores. Se
#' eligen hasta dos materias y hasta 3 diferentes horarios. Se quitan los
#' renglones repetidos.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' 
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return lista_info_esqueleto: Lista con las matrices:
#' 1) mat_esqueleto: Matriz de 15 renglones (horas) y 333 columnas
#' (materias). En la entrada (i,j) se tiene el número de grupos simulados
#' para la hora i, y la materia j.
#' 2) mat_prof_TC: Matriz de 2 columnas con el nombre de los profesores de
#' tiempo completo y el número de materias asignadas.
#' 3) mat_prof_asig: Matriz de 2 columnas con el nombre de los profesores
#' de asignatura y el número de materias asignadas.
#'
#' @examples
#' lista_info_esqueleto <- gen_esqueleto(mat_demanda_alumnos,mat_solicitudes,param)
#' 
gen_esqueleto <- function(mat_demanda_alumnos,mat_solicitudes,param){
  ptm <- proc.time()# Start the clock!
  #Se definen las variables que se van a utilizar
  mat_demanda_aux <- mat_demanda_alumnos
  mat_solicitudes_aux <- mat_solicitudes[as.numeric(mat_solicitudes[,5])>0,]
  mat_esqueleto <- matrix(0,nrow = length(param$Horas),
                          ncol = length(param$vec_nom_materias_total))
  rownames(mat_esqueleto) <- 1:15
  colnames(mat_esqueleto) <- 1:333
  
  ##### Profesores de tiempo completo #####
  mat_solicitudes_TC <- mat_solicitudes_aux[mat_solicitudes_aux[,2]==1,]
  mat_prof_TC <- cbind(unique(mat_solicitudes_TC[,1]),
                       rep(0,length(unique(mat_solicitudes_TC[,1]))))
  lista_ciclo_TC <- ciclo_esqueleto(1000,mat_solicitudes_TC,mat_prof_TC,
                                    mat_demanda_aux)
  mat_prof_TC <- lista_ciclo_TC[[2]]
  colnames(mat_prof_TC) <- c("Profesor","Materias_Asig")

  ##### Profesores de asignatura #####
  mat_solicitudes_asignatura <- mat_solicitudes_aux[mat_solicitudes_aux[,2]==0,]
  mat_prof_asig <- cbind(unique(mat_solicitudes_asignatura[,1]),
                         rep(0,length(unique(mat_solicitudes_asignatura[,1]))))
  
  lista_ciclo_asig <- ciclo_esqueleto(6500,mat_solicitudes_asignatura,
                                      mat_prof_asig,lista_ciclo_TC[[3]])
  mat_prof_asig <- lista_ciclo_asig[[2]]
  colnames(mat_prof_asig) <- c("Profesor","Materias_Asig")
  
  #Se suman los grupos de los profesores de tiempo completo y los de asignatura
  mat_esqueleto <- lista_ciclo_TC[[1]] + lista_ciclo_asig[[1]]
  rownames(mat_esqueleto) <- param$nombre_hrs
  colnames(mat_esqueleto) <- param$vec_nom_materias_total
  
  lista_info_esqueleto <- list()
  lista_info_esqueleto[[1]] <- mat_esqueleto
  lista_info_esqueleto[[2]] <- mat_prof_TC
  lista_info_esqueleto[[3]] <- mat_prof_asig
  names(lista_info_esqueleto) <- c("mat_esqueleto","mat_prof_TC",
                                   "mat_prof_asig")
  
  cat("\nLa función gen_esqueleto tardó: ",(proc.time()-ptm)[3],
      " segundos\n")#19.48seg
  return(lista_info_esqueleto)
}



# Ej. ---------------------------------------------------------------------

lista_info_esqueleto <- gen_esqueleto(mat_demanda_alumnos,mat_solicitudes,param)







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
#' @param mat_solicitudes: Matriz de 12 columnas que contiene la
#' información de las solicitudes de materia y de horario de todos los
#' profesores, en las primeras 6 columnas se tiene la información
#' de la simulación de elección de materias y en las últimas 6 columnas
#' se tiene la información de la simulación de elección de horarios,
#' la matriz puede no estar completamente llena),tiene como renglones
#' los nombres de los profesores.
#' @param n_rep: Número de veces que se va a generar la matriz D con la
#' demanda de alumnos para el siguiente semestre.
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
#' lista_info_esqueleto <- gen_lista_info_esqueleto(D_inicial,
#' mat_solicitudes,n_rep,param)
#' 
gen_lista_info_esqueleto <- function(D_inicial,mat_solicitudes,
                                     n_rep,param){
  ptm <- proc.time()# Start the clock!
  cota <- 100*n_rep
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
    mat_calif_x_gpo <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])
    ind_materias <- 1:dim(D)[2]
    calif_D <- actualiza_calif_D(D_inicial,D,mat_calif_x_gpo,
                                 ind_materias)
    vec_calif_x_materia <- calif_D[[2]]
    ind_1 <- which(vec_calif_x_materia< -20)
    ind_2 <- which(vec_calif_x_materia> 10)
    ind_materias <- union(ind_1,ind_2)
    D <- actualiza_D_prima(cota,D_inicial,D,mixmdl_1_D,
                           calif_D,ind_materias)
    
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
  
  ##Calificamos D_final
  mat_calif_x_gpo <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])
  ind_materias <- 1:dim(D)[2]
  calif_D <- actualiza_calif_D(D_inicial,D_final,mat_calif_x_gpo,ind_materias)
  mat_calif_x_gpo <- calif_D[[1]]
  vec_calif_x_materia <- calif_D[[2]]
  (calif_esqueleto_2 <- sum(vec_calif_x_materia))#-1980.377/-1832.852
  cat("\n La suma de las calificaciones por materia es: ",calif_esqueleto_2)
  
  ##Generar esqueleto
  # mat_solicitudes <- gen_solicitudes(param)#7.97 seg
  lista_info_esqueleto <- gen_esqueleto(D_final,mat_solicitudes,param)#10.76 seg
  
  cat("\n La función gen_lista_info_esqueleto tardó: ",(proc.time()-ptm)[3]/60,
      " minutos\n")
  return(lista_info_esqueleto)
}