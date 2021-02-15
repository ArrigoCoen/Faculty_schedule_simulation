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
califica_asignacion <- function(mat_esqueleto,mat_solicitudes_real,
                                lista_asignacion,param){
  ptm <- proc.time()# Start the clock!
  #Se definen las variables que se van a utilizar
  mat_asignacion <- lista_asignacion[[1]]
  mat_esqueleto_aux <- lista_asignacion[[2]]
  mat_calif_asig_x_gpo <- data.frame(mat_asignacion,calif = 0, Prob_Ac = 0)
  
  #' Penalización por grupos sobrantes o faltantes:
  #' Se resta de acuerdo a la diferencia relativa por grupo.
  mat_diferencia <- mat_esqueleto - mat_esqueleto_aux
  # (gpos_sin_prof <- sum(!is.nan()))
  dif_relativas <- mat_diferencia/mat_esqueleto
  vec_dif_rel <- dif_relativas[!is.nan(dif_relativas)]
  (gpos_sobrantes <- sum(vec_dif_rel[vec_dif_rel<0]))
  (gpos_faltantes <- sum(vec_dif_rel[vec_dif_rel>0]))
  
  #' Si algún profesor de tiempo completo pidió alguna materia y
  #' no se la dieron. Se penaliza con -10 por cada materia.
  mat_info_prof <- data.frame(Profesor = param$mat_nom_prof_total[,1],
                              TC = param$mat_nom_prof_total[,2],
                              Materias_solicitadas = 0,
                              Materias_asignadas = 0)
  
  for(p in 1:length(param$mat_nom_prof_total[,1])){
    (prof <- mat_info_prof[p,1])
    solicitudes <- mat_solicitudes_real %>% filter(Profesor == prof)
    mat_info_prof[p,3] <- dim(solicitudes)[1]
    
    asignaciones <- mat_asignacion %>% filter(Profesor == prof)
    mat_info_prof[p,4] <- dim(asignaciones)[1]
  }
  
  #' Nota:
  #' Se penaliza por cada materia con tope a "num_max_asig",
  #' Ej. si num_max_asig = 2 y un profesor pidió 3 o más  materias
  #' pero sólo le dieron 1, entonces se penaliza 1; si le dieron 2
  #' no hay penalización.
  pena_x_solicitud_negada <- 0
  mat_prof_TC <- mat_info_prof %>% filter(TC == 1)
  
  for(r in 1:dim(mat_prof_TC)[1]){#Recorre los renglones
    if(mat_prof_TC[r,3]>0 && mat_prof_TC[r,4]<2){
      num_sols <- min(mat_prof_TC[r,3],param$num_max_asig)
      num_neg <- num_sols - mat_prof_TC[r,4]
      pena_x_solicitud_negada <- pena_x_solicitud_negada + (10*num_neg)
    }
  }
  pena_x_solicitud_negada##680
  
  
  ### CALIFICACIÓN POR GRUPO ###
  
  #' Se pone un +5 si el profesor asignado es de TC
  ind_TC <- which(mat_calif_asig_x_gpo[,3] == 1)
  mat_calif_asig_x_gpo[ind_TC,5] <- 5
  
  #' Se penaliza con -1 por cada asignación que pudo haber tenido
  #' un profesor de TC y tiene un profesor de asignatura.
  mat_prof_TC <- data.frame(mat_prof_TC,Materias_negadas = 0)
  
  for(r in 1:dim(mat_prof_TC)[1]){#Recorre los renglones
    (num_sols <- min(mat_prof_TC[r,3],param$num_max_asig))
    mat_prof_TC[r,5] <- num_sols - mat_prof_TC[r,4]
  }
  TC_falta_asig <- mat_prof_TC %>% filter(Materias_negadas > 0)
  materias_no_asignadas <- data.frame(Profesor = 0,TC = 0, Materia = 0,
                                      Num_Materia = 0,Horario = 0)
  
  for(r in 1:dim(TC_falta_asig)[1]){#Recorre renglones de "TC_falta_asig"
    indices <- which(mat_solicitudes_real[,1] == TC_falta_asig[r,1])
    materias_no_asignadas <- rbind(materias_no_asignadas,
                                   mat_solicitudes_real[indices,])
  }
  materias_no_asignadas <- materias_no_asignadas %>% filter(Profesor != 0)
  
  for(r in 1:dim(mat_calif_asig_x_gpo)[1]){#Recorre renglones de "mat_calif_asig_x_gpo"
    materia <- mat_calif_asig_x_gpo[r,1]
    hora <- mat_calif_asig_x_gpo[r,4]
    mat_aux <- materias_no_asignadas %>% filter(Materia == materia) %>% filter(
      Horario == hora)
    
    mat_calif_asig_x_gpo[r,5] <- mat_calif_asig_x_gpo[r,5] - dim(mat_aux)[1]
  }
  
  #' Para tener una calificación diferente para cada grupo, sumamos
  #' a cada renglón una épsilon:
  for(r in 1:dim(mat_calif_asig_x_gpo)[1]){#Recorre renglones de "mat_calif_asig_x_gpo"
    (num_al <- round(runif(1,0,0.1),4))
    if(mat_calif_asig_x_gpo[r,5] >= 0){
      mat_calif_asig_x_gpo[r,5] <- mat_calif_asig_x_gpo[r,5] + num_al
    }else{
      mat_calif_asig_x_gpo[r,5] <- mat_calif_asig_x_gpo[r,5] - num_al
    }
  }
  mat_calif_asig_x_gpo <- mat_calif_asig_x_gpo[order(mat_calif_asig_x_gpo$calif),]
  
  #' Agregamos una columna con la probabilidad acumulada de elegir cada
  #' grupo.
  (n_gpos <- dim(mat_calif_asig_x_gpo)[1])
  mat_calif_asig_x_gpo[1,6] <- (2*1)/(n_gpos*(n_gpos+1))
  for(r in 2:dim(mat_calif_asig_x_gpo)[1]){#Recorre renglones de "mat_calif_asig_x_gpo"
    prob <- (2*r)/(n_gpos*(n_gpos+1))
    mat_calif_asig_x_gpo[r,6] <- mat_calif_asig_x_gpo[(r-1),6] + prob
  }
  
  (calif_asignacion <- gpos_sobrantes -sum(gpos_sin_prof,
                                           pena_x_solicitud_negada,
                                           -mean(mat_calif_asig_x_gpo[,5]),
                                           gpos_faltantes))#-1624
  
  lista_calif_asignacion <- list()
  lista_calif_asignacion[[1]] <- mat_calif_asig_x_gpo
  lista_calif_asignacion[[2]] <- calif_asignacion
  
  cat("\nLa función califica_asignacion tardó: ",(proc.time()-ptm)[3],
      " segundos\n")
  return(lista_calif_asignacion)
}



# Ej. ---------------------------------------------------------------------

lista_calif_asignacion <- califica_asignacion(mat_solicitudes_real,
                                              lista_asignacion,
                                              param)#6.2/5.58/5.45 seg

mat_calif_asig_x_gpo <- lista_calif_asignacion[[1]]
(calif_asignacion <- lista_calif_asignacion[[2]])#-1083.836
View(mat_calif_asig_x_gpo)







