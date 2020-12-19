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
#' @param mat_demanda: Matriz de 15 renglones (horas) y 203 columnas
#' (materias). En la entrada (i,j) se tiene el número de alumnos restantes
#' para la hora i, y la materia j.
#' @param num_max_asig: Número máximo de materias que se le pueden asignar
#' a un profesor.
#' @param mat_demanda_alumnos: Matriz de 15 renglones (horas) y 203 columnas
#' (materias). En la entrada (i,j) se tiene el número de alumnos simulados
#' para la hora i, y la materia j.
#'
#' @return lista_ciclo: Lista con la matriz "mat_esqueleto" y la matriz
#' "mat_demanda" actualizados.
#'
#' @examples
#' lista_ciclo <- ciclo_esqueleto(1000,mat_solicitudes_TC,mat_prof_TC,
#' mat_esqueleto,mat_demanda_alumnos)
#' lista_ciclo <- ciclo_esqueleto(6500,mat_solicitudes_asig,mat_prof_asig,
#' mat_esqueleto,mat_demanda_alumnos)
#' 
ciclo_esqueleto <- function(cota,mat_solicitudes,mat_prof,mat_demanda,
                            num_max_asig,mat_demanda_alumnos){
  #Se definen las variables que se van a utilizar
  mat_esqueleto <- matrix(0,nrow = length(param$Horas),
                          ncol = length(param$vec_nom_materias_total))
  rownames(mat_esqueleto) <- param$Horas
  colnames(mat_esqueleto) <- param$vec_nom_materias_total
  
  for(n in 1:cota){#Cota para que el ciclo no sea infinito
    # cat("\n Iteración: ",n)
    #Se suman los valores positivos de la demanda de alumnos
    if(sum(mat_demanda[mat_demanda>0])>0 && dim(mat_solicitudes)[1]>0){
      #Número aleatorio para elegir profesor
      (num_al <- sample(x = 1:dim(mat_prof)[1], size = 1))
      (profesor <- mat_prof[num_al,1])
      mat_aux <- mat_solicitudes %>% filter(Profesor == profesor)
      
      if(dim(mat_aux)[1]>0){#Si hay información de "profesor"
        #Número aleatorio para elegir materia
        (num_al_2 <- sample(x = 1:dim(mat_aux)[1], size = 1))
        (materia_al <- mat_aux[num_al_2,3])
        (num_materia_al <- mat_aux[num_al_2,4])
        
        #Número aleatorio para elegir horario
        (num_al_3 <- sample(x = 1:dim(mat_aux)[1], size = 1))
        (hora_al <- mat_aux[num_al_3,5])
        (TC <- mat_aux[num_al_3,2])
        
        (renglon <- c(profesor,TC,materia_al,num_materia_al,hora_al))
        
        #Índices de renglón y columna para "mat_esqueleto"
        (M_i <- which(param$Horas == as.numeric(renglon[5])))
        (M_j <- as.numeric(renglon[4]))
        
        #Se verifica si la demanda ha sido cubierta o no
        (sobran_alum_1si_0no <- verifica_demanda_cubierta(mat_demanda,
                                                          renglon,
                                                          mat_demanda_alumnos))
        
        if(sobran_alum_1si_0no == 1){
          #' Aún hay alumnos sin clase para esa materia. Simulamos el
          #' número de alumnos para este grupo
          (num_alum_x_profesor <- simula_alum_x_profesor(renglon,param))
          # num_alum_simulados <- num_alum_simulados + num_alum_x_profesor
          # mat_E[M_i,M_j] <- mat_E[M_i,M_j]+num_alum_x_profesor
          
          #Se actualizan las entradas de las matrices auxiliares
          # mat_demanda[M_i,M_j] <- max(0,mat_demanda[M_i,M_j]-num_alum_x_profesor)
          #' Permitimos los negativos que nos muestran los alumnos sobrantes
          mat_demanda[M_i,M_j] <- mat_demanda[M_i,M_j]-num_alum_x_profesor
          mat_esqueleto[M_i,M_j] <- mat_esqueleto[M_i,M_j] + 1
          
          mat_prof[num_al,2] <- as.numeric(mat_prof[num_al,2]) + 1
          prof_max_asig <- 0
          if(mat_prof[num_al,2] >= num_max_asig){
            prof_max_asig <- 1
          }
          mat_solicitudes <- actualiza_mat_solicitudes(mat_solicitudes,
                                                       renglon,prof_max_asig)
        }#Fin if(dim(mat_aux)[1]>0)
      }#Fin if Demanda existente
    }#Fin if Condiciones de paro
  }#Fin for(n)
  
  lista_ciclo <- list()
  lista_ciclo[[1]] <- mat_esqueleto
  lista_ciclo[[2]] <- mat_demanda
  names(lista_ciclo) <- c("mat_esqueleto","mat_demanda")
  return(lista_ciclo)
}



# gen_esqueleto -----------------------------------------------------------
#' Title gen_esqueleto: Función que arroja la matriz "mat_esqueleto". Matriz
#' de 15 renglones (horas) y 203 columnas (materias). En la entrada (i,j)
#' se tiene el número de grupos simulados para la hora i, y la materia j.
#'
#' @param mat_demanda_alumnos: Matriz de 15 renglones (horas) y 201
#' columnas (materias). En la entrada (i,j) se tiene el número de alumnos
#' simulados para la hora i, y la materia j.
#' @param mat_solicitudes: Matriz de 4 columnas (Profesor,TC,Materia,
#' Horario). Tiene la información de las solicitudes de los profesores. Se
#' eligen hasta dos materias y hasta 3 diferentes horarios. Se quitan los
#' renglones repetidos.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return mat_esqueleto: Matriz de 15 renglones (horas) y 203 columnas
#' (materias). En la entrada (i,j) se tiene el número de grupos simulados
#' para la hora i, y la materia j.
#'
#' @examples
#' lista_info_esqueleto <- gen_esqueleto(mat_demanda_alumnos,mat_solicitudes,
#' param)
#' 
gen_esqueleto <- function(mat_demanda_alumnos,mat_solicitudes,param){
  ptm <- proc.time()# Start the clock!
  #Se definen las variables que se van a utilizar
  num_max_asig <- param$num_max_asig
  mat_solicitudes_aux <- mat_solicitudes[as.numeric(mat_solicitudes[,5])>0,]
  
  ##### Profesores de tiempo completo
  mat_solicitudes_TC <- mat_solicitudes_aux %>% filter(TC == 1)
  nom_prof_TC <- unique(mat_solicitudes_TC[,1])
  mat_prof_TC <- data.frame(Profesor = nom_prof_TC,Materias_Asig = 0)
  lista_ciclo_TC <- ciclo_esqueleto(param$cota_TC,mat_solicitudes_TC,
                                    mat_prof_TC,mat_demanda_alumnos,
                                    num_max_asig,mat_demanda_alumnos)
  
  ##### Profesores de asignatura
  mat_solicitudes_asig <- mat_solicitudes_aux %>% filter(TC == 0)
  nom_prof_asig <- unique(mat_solicitudes_asig[,1])
  mat_prof_asig <- data.frame(Profesor = nom_prof_asig,Materias_Asig = 0)
  
  lista_ciclo_asig <- ciclo_esqueleto(param$cota_asig,
                                      mat_solicitudes_asig,
                                      mat_prof_asig,lista_ciclo_TC[[2]],
                                      num_max_asig,mat_demanda_alumnos)
  
  #' Se suman los grupos de los profesores de tiempo completo y los
  #' de asignatura.
  mat_esqueleto <- lista_ciclo_TC[[1]] + lista_ciclo_asig[[1]]
  rownames(mat_esqueleto) <- param$nombre_hrs
  colnames(mat_esqueleto) <- param$vec_nom_materias_total
  
  cat("\nLa función gen_esqueleto tardó: ",(proc.time()-ptm)[3],
      " segundos\n")#19.48seg
  return(mat_esqueleto)
}

