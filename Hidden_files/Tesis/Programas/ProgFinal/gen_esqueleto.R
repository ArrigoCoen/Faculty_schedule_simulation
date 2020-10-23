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



# verifica_demanda_cubierta -----------------------------------------------
#' Title verifica_demanda_cubierta: Función que arroja un 1 si aún existen
#' alumnos en la i-ésima hora y en la materia j, cero si no.
#'
#' @param mat_demanda_aux: Matriz con la demanda de alumnos que se le están
#' constantemente restando los alumnos ya simulados.
#' @param renglon: Vector con la información del profesor elegido para
#' asignarle un grupo.
#'
#' @return sobran_alum_1si_0no: Variable binaria que vale 1 si aún existen
#' alumnos en la i-ésima hora y en la materia j, cero si no.
#'
#' @examples
#' sobran_alum_1si_0no <- verifica_demanda_cubierta(mat_demanda_aux,renglon)
#' 
verifica_demanda_cubierta <- function(mat_demanda_aux,renglon){
  ind_hora <- which(7:21 == as.numeric(renglon[5]))
  
  #' La primera condición evita errores cuando no hay hora en la
  #' columna "Horario"
  if(length(ind_hora>0) && 
     mat_demanda_aux[ind_hora,as.numeric(renglon[4])] > 0){
    sobran_alum_1si_0no <- 1
  }else{
    sobran_alum_1si_0no <- 0
  }
  
  return(sobran_alum_1si_0no)
}



# gen_esqueleto -----------------------------------------------------------
#' Title gen_esqueleto: Función que arroja la matriz "mat_esqueleto" que
#' tiene 15 renglones (horas) y 333 columnas (materias). En la entrada (i,j)
#' se tiene el número de grupos simulados para la hora i, y la materia j.
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
#' @return mat_esqueleto: Matriz de 15 renglones (horas) y 333 columnas
#' (materias). En la entrada (i,j) se tiene el número de grupos simulados
#' para la hora i, y la materia j.
#'
#' @examples
#' mat_esqueleto <- gen_esqueleto(mat_demanda_alumnos,mat_solicitudes,param)
#' 
gen_esqueleto <- function(mat_demanda_alumnos,mat_solicitudes,param){
  #Se definen las variables que se van a utilizar
  mat_demanda_aux <- mat_demanda_alumnos
  mat_solicitudes_aux <- mat_solicitudes[as.numeric(mat_solicitudes[,5])>0,]
  mat_solicitudes_TC <- mat_solicitudes_aux[mat_solicitudes_aux[,2]==1,]
  mat_prof_TC <- cbind(unique(mat_solicitudes_TC[,1]),
                       rep(0,length(unique(mat_solicitudes_TC[,1]))))
  mat_solicitudes_asignatura <- mat_solicitudes_aux[mat_solicitudes_aux[,2]==0,]
  mat_esqueleto <- matrix(0,nrow = length(param$Horas),
                          ncol = length(param$vec_nom_materias_total))
  rownames(mat_esqueleto) <- param$nombre_hrs
  colnames(mat_esqueleto) <- param$vec_nom_materias_total
  
  ### Profesores de tiempo completo ###
  for(n in 1:(dim(mat_prof_TC)[1])*2){#Se asignan 2 materias por profesor
    cat("\n Iteración: ",n)
    if(sum(mat_demanda_aux)>0 && dim(mat_solicitudes_TC)[1]>0){
      num_al <- round(runif(1,min = 1,max = dim(mat_solicitudes_TC)[1]))
      renglon <- mat_solicitudes_TC[num_al,]
      M_i <- which(param$Horas == as.numeric(renglon[5]))
      M_j <- as.numeric(renglon[4])
      
      #Se verifica si la demanda ha sido cubierta o no
      (sobran_alum_1si_0no <- verifica_demanda_cubierta(mat_demanda_aux,renglon))
      
      if(sobran_alum_1si_0no == 1){#Aún hay alumnos sin clase para esa materia
        #Simulamos el número de alumnos para este grupo
        num_alum_x_profesor <- simula_alum_x_profesor(renglon,param)
        
        #Se actualizan las entradas de las matrices auxiliares
        mat_demanda_aux[M_i,M_j] <- max(0,mat_demanda_aux[M_i,M_j]-num_alum_x_profesor)
        mat_esqueleto[M_i,M_j] <- mat_esqueleto[M_i,M_j] + 1
        mat_solicitudes_TC <- mat_solicitudes_TC[-num_al,]
        
        cat("\n Se eligió la materia ",as.character(renglon[3]),
            " a las ",as.character(renglon[5])," hrs. \n Se simularon ",
            num_alum_x_profesor," alumnos.\n Antes se tenían ",
            mat_demanda_alumnos[M_i,M_j]," alumnos y ahora se tienen ",
            mat_demanda_aux[M_i,M_j]," alumnos.\n Se tienen ",mat_esqueleto[M_i,M_j],
            " grupos para la materia.")
      }#Fin if: Demanda existente
    }#Fin if: Condiciones de paro
  }#Fin for(n)
  
  
  
  
  
    
  
  
  
  
  
  
  
  
  
  
  ### Profesores de asignatura
  
  
  
  
  return(mat_esqueleto)
}
