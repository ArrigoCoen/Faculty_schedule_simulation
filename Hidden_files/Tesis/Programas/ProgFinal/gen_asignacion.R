##########################################################################
#' En este programa se encuentra la función que genera una matriz con
#' las asignaciones de Materia-Profesor-Horario.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")

# cuenta_asignaciones -----------------------------------------------------
#' Title cuenta_asignaciones: Función encargada de contar el número de
#' asignaciones que tiene cada profesor.
#'
#' @param mat_aux_solicitud: Matriz de 6 columnas (Profesor,TC,Materia,
#' Num_Materia,Horario,Num_Asig) que tiene la información de la solicitud
#' de los profesores y el número de asignaciones que tiene cada profesor.
#' @param mat_asig: Matriz de 4 columnas (Materia, Profesor,TC,Horario) la
#' cual contiene en el i-ésimo renglón la asignación por materia, profesor
#' y horario. La columna TC indica si el profesor es o no de tiempo completo.
#'
#' @return mat_aux_solicitud: Matriz de 6 columnas (Profesor,TC,Materia,
#' Num_Materia,Horario,Num_Asig) que tiene la información de la solicitud
#' de los profesores y el número de asignaciones que tiene cada profesor.
#'
#' @examples
#' mat_aux_solicitud <- cuenta_asignaciones(mat_aux_solicitud,mat_asig)
#' 
cuenta_asignaciones <- function(mat_aux_solicitud,mat_asig){
  for(d in 1:dim(mat_asig)[1]){
    #Sumamos a las solicitudes del profesor
    (ind_prof <- which(mat_aux_solicitud[,1] == mat_asig[d,2]))
    mat_aux_solicitud[ind_prof,6] <- as.numeric(mat_aux_solicitud[ind_prof,
                                                                  6]) + 1
    #Sumamos para no repetir materia ni hora
    (ind_hora <- which(mat_aux_solicitud[ind_prof,5] == mat_asig[d,4]))
    (ind_materia <- which(mat_aux_solicitud[ind_prof,3] == mat_asig[d,1]))
    ind <- union(ind_hora,ind_materia)
    mat_aux_solicitud[ind_prof[ind],6] <- mat_aux_solicitud[ind_prof[ind],
                                                            6] + 11
  }
  
  return(mat_aux_solicitud)
}


# gen_asignacion ----------------------------------------------------------
#' Title gen_asignacion: Función que genera asignaciones de materia con
#' profesor por hora, dependiendo del número de grupos simulados para el
#' siguiente semestre. Tenemos de información "mat_esqueleto" y
#' "mat_solicitudes_real".
#'
#' @param mat_esqueleto: Matriz de 15 renglones con las horas (7-8,8-9,...,
#' 21-22) y tantas columnas como materias impartidas en el semestre actual.
#' @param mat_solicitudes_real: Matriz de 5 columnas (Profesor,TC,Materia,
#' Num_Materia,Horario) que tiene la información de la solicitud de los
#' profesores. Se hace una "intersección" con los grupos simulados en la
#' matriz "mat_esqueleto" y así se obtienen las solicitudes pseudo-reales
#' de los profesores.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return mat_asignacion: Matriz de cuatro columnas (Materia, Profesor,
#' TC,Horario) la cual contiene en el i-ésimo renglón la asignación
#' por materia, profesor y horario. La columna TC indica si el profesor
#' es o no de tiempo completo.
#' 
#' @examples
#' lista_asignacion <- gen_asignacion(mat_esqueleto,mat_solicitudes_real,param)
#'
gen_asignacion <- function(mat_esqueleto,mat_solicitudes_real,param){
  ptm <- proc.time()# Start the clock!
  mat_asignacion <- data.frame(Materia = 0, Profesor = 0,TC = 0, Horario = 0)
  Materias <- param$vec_nom_materias_total
  Num_Asig <- rep(0,dim(mat_solicitudes_real)[1])
  m_sol_aux <- cbind(mat_solicitudes_real,Num_Asig)
  m_esq_aux <- mat_esqueleto
  
  # m <- 5
  # m <- 48
  for(m in 1:length(Materias)){
    materia <- Materias[m]
    cat("\nMateria ",m,": ",materia)
    (vec_aux_esq <- mat_esqueleto[,m])
    for(h in 1:length(param$Horas)){
      mat_aux_solicitud <- m_sol_aux %>% filter(Materia == materia)
      (num_gpos <- vec_aux_esq[h])
      if(num_gpos > 0){#Si el # de gpos. simulados > 0
        m_aux <- mat_aux_solicitud%>% filter(
          as.numeric(Num_Asig)<param$num_max_asig) %>% filter(
            Horario==param$Horas[h])
        
        if(dim(m_aux)[1] >= num_gpos){
          mat_asig <- m_aux[sample(1:dim(m_aux)[1],size = num_gpos),
                            c(3,1,2,5)]
          mat_asignacion <- rbind(mat_asignacion,mat_asig)
          m_esq_aux[h,m] <- m_esq_aux[h,m] - num_gpos
        }else{
          mat_asig <- m_aux[,c(3,1,2,5)]
          mat_asignacion <- rbind(mat_asignacion,mat_asig)
          m_esq_aux[h,m] <- m_esq_aux[h,m] - dim(m_aux)[1]
        }
      }#Fin if(num_gpos>0)
      m_sol_aux <- cuenta_asignaciones(m_sol_aux,mat_asig)
    }#Fin for(h)
  }
  mat_asignacion <- mat_asignacion %>% filter(Materia != 0)
  # View(mat_asignacion)
  
  cat("\nLa matriz mat_esqueleto tiene: ", sum(mat_esqueleto)," grupos" )
  cat("\nSe generaron: ", sum(mat_esqueleto) - sum(m_esq_aux),
      " grupos en la asignación" )
  cat("\nLa función gen_asignacion tomó: ", (proc.time()-ptm)[3],
      " segundos\n\n\n" )
  lista_asignacion <- list()
  lista_asignacion[[1]] <- mat_asignacion
  lista_asignacion[[2]] <- m_esq_aux
  lista_asignacion[[3]] <- m_sol_aux
  return(lista_asignacion)
}



# Ej. ---------------------------------------------------------------------

lista_asignacion <- gen_asignacion(mat_esqueleto,mat_solicitudes_real,
                                   param)#22.66 seg
mat_asignacion <- lista_asignacion[[1]]
m_esq_aux <- lista_asignacion[[2]]
m_sol_aux <- lista_asignacion[[3]]
View(mat_asignacion)
View(m_esq_aux)
View(m_sol_aux)








