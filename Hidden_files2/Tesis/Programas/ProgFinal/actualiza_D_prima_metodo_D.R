##########################################################################
#' En este programa se encuentra la función "actualiza_D_prima" de la
#' metodología A, pero modificada. Ésto para usarla en la metodología D.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")


# actualiza_D_prima_metodo_D ------------------------------------------------
#' Title actualiza_D_prima_metodo_D: Función encargada de actualizar D_prima.
#'
#' @param cota: Cota para que el ciclo no sea infinito.
#' @param D: Matriz mat_demanda_alumnos, de 15 renglones (horas) y 203
#' columnas (materias). En la entrada (i,j) se tiene el número de alumnos
#' simulados para la hora i, y la materia j.
#' @param D_prima: Matriz de 15 renglones (horas) y 203 columnas (materias).
#' En la entrada (i,j) se tiene el número de alumnos simulados para la hora
#' i, y la materia j. 
#' @param mixmdl: Lista con "m" elementos. Cada elemento es el modelo de
#' mezcla de Normales para una materia.
#' @param calif_D: Lista con 2 elementos: "mat_calif_x_gpo" y
#' "vec_calif_x_materia". La matriz "mat_calif_x_gpo" (15*203) contiene las
#' calificaciones por grupo. El vector "vec_calif_x_materia"
#' @param ind_materias: Vector con los índices de las materias que deben
#' de modificarse.
#'
#' @return D_prima: Matriz de 15x203 actualizada. En la entrada (i,j) se
#' tiene el nuevo número de alumnos simulados para la hora i, y la materia j.
#'
#' @examples
#' actualiza_D_prima_metodo_D(500,D,D_prima,mixmdl,calif_D,c(5,182))
#' actualiza_D_prima_metodo_D(cota,D,D_prima,mixmdl,calif_D,ind_materias)
#' 
actualiza_D_prima_metodo_D <- function(cota,D,D_prima,mixmdl,calif_D,ind_materias){
  #' Para este punto ya comparamos D y D_prima. Se redefine D_prima.
  #' Recibe a D_prima como parámetro para que en caso de que no haya
  #' modificaciones, se regrese la misma matriz y no una llena de ceros.
  
  mat_calif_x_gpo <- calif_D[[1]]
  vec_calif_x_materia <- calif_D[[2]]
  for(c in ind_materias){#Recorre columnas
    cont_1 <- 1
    cont_2 <- 1
    if(sum(vec_calif_x_materia[c])>10 || 
       sum(vec_calif_x_materia[c]) < -20){#Sólo modificamos si
      #' la califición total de la materia está fuera de [-20,10]
      for(h in 1:length(param$Horas)){#Recorre las horas (renglones)
        # cat("\n h = ",h)
        (rand_num <- ceiling(rnorm(1,mixmdl$mu,mixmdl$sigma)))
        if(mat_calif_x_gpo[h,c] > 10){#Si faltan alumnos
          while(rand_num <= D[h,c]){
            (rand_num <- ceiling(rnorm(1,mixmdl$mu,mixmdl$sigma)))
            cont_1 <- cont_1 + 1#Para no tener ciclo infinito
            if(cont_1 >= cota){
              break;
            }
          }
          cont_1 <- 1#Reiniciamos el contador
          D_prima[h,c] <- max(0,rand_num)
        }
        if(mat_calif_x_gpo[h,c] < -10 && D[h,c]>0){#Si sobran alumnos
          #'La 2° cond. es para que no haya simulación si no hay alumnos en D
          #'Aquí la calificación debe ser menor a -10 porque es por
          #'grupo no por materia (ver gráficas de diferencias relativas
          #'entre D y E)
          while(rand_num > D[h,c]){
            (rand_num <- ceiling(rnorm(1,mixmdl$mu,mixmdl$sigma)))
            cont_2 <- cont_2 + 1#Para no tener ciclo infinito
            if(cont_2 >= cota){
              break;
            }
          }
          cont_2 <- 1#Reiniciamos el contador
          D_prima[h,c] <- max(0,rand_num)
        }
      }#Fin for(h)
    }#Fin if(calificación)
  }#Fin for(c)
  return(D_prima)
}

