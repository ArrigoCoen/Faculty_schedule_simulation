##########################################################################
#' En este programa se encuentra la función que genera la matriz de
#' solicitudes de los profesores que depende del esqueleto.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")


# gen_solicitudes_real ----------------------------------------------------
#' Title gen_solicitudes_real: Función que genera la matriz de
#' solicitudes de los profesores que depende del esqueleto. Se genera
#' primero la matriz "mat_solicitudes" con la función gen_solicitudes()
#' y después se hace una "intersección" con los grupos simulados en la
#' matriz "mat_esqueleto" y así se obtienen las solicitudes pseudo-reales
#' de los profesores.
#'
#' @param mat_esqueleto: Matriz de 15 renglones (horas) y 203 columnas
#' (materias). En la entrada (i,j) se tiene el número de grupos simulados
#' para la hora i, y la materia j.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
#' @return mat_solicitudes_real: Matriz de 5 columnas (Profesor,TC,Materia,
#' Num_Materia,Horario) y 6 renglones que tiene la información de la
#' solicitud de "nom_prof". Se eligen 2 materias y hasta 3 diferentes
#' horarios. Se quitan los renglones repetidos. Se hace una "intersección"
#' con los grupos simulados en la matriz "mat_esqueleto" y así se obtienen
#' las solicitudes pseudo-reales de los profesores.
#'
#' @examples
#' mat_solicitudes_real <- gen_solicitudes_real(mat_esqueleto,param)
#' 
gen_solicitudes_real <- function(mat_esqueleto,param){
  ptm <- proc.time()# Start the clock!
  #Se definen las variables que se van a utilizar
  mat_solicitudes_real <- data.frame(Profesor = 0,TC = 0, Materia = 0,
                                     Num_Materia = 0,Horario = 0)
  # set.seed(1806)
  mat_solicitudes <- gen_solicitudes(param)#7.98 seg
  #' Quitamos las materias y el horario = 0
  mat_solicitudes <- mat_solicitudes %>% filter(Materia != 0)
  mat_solicitudes <- mat_solicitudes %>% filter(Horario != 0)
  
  for(r in 1:dim(mat_solicitudes)[1]){#Recorre renglones de "mat_solicitudes"
    renglon <- mat_solicitudes[r,]
    ind_hora <- which(7:21 == as.numeric(renglon[5]))
    ind_materia <- as.numeric(mat_solicitudes[r,4])
    
    if(mat_esqueleto[ind_hora,ind_materia] > 0){
      mat_solicitudes_real <- rbind(mat_solicitudes_real,renglon)
    }
  }#Fin for(r)
  #Quitamos el renglón de ceros inicial
  mat_solicitudes_real <- mat_solicitudes_real %>% filter(Profesor != 0)
  
  cat("\nLa función gen_solicitudes_real tardó: ",(proc.time()-ptm)[3],
      " segundos\n")
  return(mat_solicitudes_real)
}



# Ej. ---------------------------------------------------------------------
mat_solicitudes_real <- gen_solicitudes_real(mat_esqueleto,param)#11.64/10.06seg
View(mat_solicitudes_real)








