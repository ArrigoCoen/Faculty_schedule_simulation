##########################################################################
#' En este programa se encuentran las pruebas para calificaa asignaciones.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")



# Prueba 1 ----------------------------------------------------------------
#' Tenemos la información de "mat_esqueleto", "mat_solicitudes_real" y
#' "lista_asignacion" que contiene: "mat_asignacion", "mat_esqueleto_aux" y
#' "mat_solicitud_aux".


#' Penalización por grupo en esqueleto sin profesor:
#' Se resta 1 por cada grupo sin profesor.
(gpos_sin_prof <- sum(mat_esqueleto_aux))


#' Si algún profesor de tiempo completo pidió alguna materia y
#' no se la dieron. Se penaliza con 10 por cada materia.
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
# View(mat_info_prof)

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
    pena_x_solicitud_negada <- pena_x_solicitud_negada + (10*num_sols)
  }
}
pena_x_solicitud_negada##1220



(calif_asignacion <- -sum(gpos_sin_prof,pena_x_solicitud_negada))#-1624


# Prueba 2 ----------------------------------------------------------------
#' Tenemos la información de "mat_esqueleto", "mat_solicitudes_real" y
#' "lista_asignacion" que contiene: "mat_asignacion", "mat_esqueleto_aux" y
#' "mat_solicitud_aux".

#' La asignación se califica primero por grupo y después se obtiene
#' el promedio de todas las calificaciones para obtener la calificación
#' de toda la asignación.

#' Al momento de elegir las asignaciones en el AG, se quiere elegir la
#' mejor (para formar la mejor generación posible). Cuando se eligen los
#' esqueletos en la mutación, se quiere elegir el peor (para eliminarlo).
#' Por lo que por grupo se va a calificar con números negativos a los
#' peores grupos y con números positivos a los mejores grupos.
#' La calificación de la asignación va a contemplar el promedio de la
#' calificación de los grupos x -1, más otros criterios vistos a
#' continuación.

### CALIFICACIÓN POR GRUPO ###
#' Se pone un +5 si el profesor asignado es de TC
calif_asig_x_gpo <- data.frame(mat_asignacion,calif = 0)
ind_TC <- which(calif_asig_x_gpo[,3] == 1)
calif_asig_x_gpo[ind_TC,5] <- 5

#' Se pone un -1 por cada asignación que pudo haber tenido un
#' profesor de TC y tiene un profesor de asignatura.
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
# View(mat_info_prof)

mat_prof_TC <- mat_info_prof %>% filter(TC == 1)
mat_prof_TC <- data.frame(mat_prof_TC,Materias_negadas = 0)

# mat <- rbind(mat_prof_TC[,3],
#              rep(param$num_max_asig,dim(mat_prof_TC)[1]))
# minimos <- sapply(mat,min)
# mat_prof_TC[,5] <- minimos - mat_prof_TC[,4]

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
# View(materias_no_asignadas)

for(r in 1:dim(calif_asig_x_gpo)[1]){#Recorre renglones de "calif_asig_x_gpo"
  materia <- calif_asig_x_gpo[r,1]
  hora <- calif_asig_x_gpo[r,4]
  mat_aux <- materias_no_asignadas %>% filter(Materia == materia) %>% filter(
    Horario == hora)
  
  calif_asig_x_gpo[r,5] <- calif_asig_x_gpo[r,5] - dim(mat_aux)[1]
}
# View(calif_asig_x_gpo)


#' Para tener una calificación diferente para cada grupo, sumamos
#' a cada renglón una épsilon:
for(r in 1:dim(calif_asig_x_gpo)[1]){#Recorre renglones de "calif_asig_x_gpo"
  (num_al <- round(runif(1,0,0.1),4))
  if(calif_asig_x_gpo[r,5] >= 0){
    calif_asig_x_gpo[r,5] <- calif_asig_x_gpo[r,5] + num_al
  }else{
    calif_asig_x_gpo[r,5] <- calif_asig_x_gpo[r,5] - num_al
  }
}
# View(calif_asig_x_gpo)


# View(calif_asig_x_gpo[order(-calif_asig_x_gpo$calif),])
calif_asig_x_gpo <- calif_asig_x_gpo[order(-calif_asig_x_gpo$calif),]
# View(calif_asig_x_gpo)


#' Agregamos una columna con la probabilidad acumulada de elegir cada
#' grupo.
(n_gpos <- dim(calif_asig_x_gpo)[1])
calif_asig_x_gpo[1,6] <- (2*1)/(n_gpos*(n_gpos+1))
for(r in 2:dim(calif_asig_x_gpo)[1]){#Recorre renglones de "calif_asig_x_gpo"
  prob <- (2*r)/(n_gpos*(n_gpos+1))
  calif_asig_x_gpo[r,6] <- calif_asig_x_gpo[(r-1),6] + prob
}
View(calif_asig_x_gpo)



# califica_asig_x_gpo -----------------------------------------------------
#' Title califica_asig_x_gpo: Función que califica a la matriz de
#' asignaciones por grupo.
#'
#' @param mat_solicitudes_real: Matriz de 5 columnas (Profesor,TC,Materia,
#' Num_Materia,Horario) que tiene la información de la solicitud de los
#' profesores. Se hace una "intersección" con los grupos simulados en la
#' matriz "mat_esqueleto" y así se obtienen las solicitudes pseudo-reales
#' de los profesores.
#' @param mat_asignacion: Matriz de cuatro columnas (Materia, Profesor,
#' TC,Horario) la cual contiene en el i-ésimo renglón la asignación
#' por materia, profesor y horario. La columna TC indica si el profesor
#' es o no de tiempo completo.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return calif_asig_x_gpo: Matriz de 6 columnas: Materia, Profesor,TC,
#' Horario, calif, Prob_Ac.
#'
#' @examples
#' calif_asig_x_gpo <- califica_asig_x_gpo(mat_solicitudes_real,
#'                                        mat_asignacion,param)
#' 
califica_asig_x_gpo <- function(mat_solicitudes_real,mat_asignacion,param){
  ptm <- proc.time()# Start the clock!
  #Se definen las variables que se van a utilizar
  calif_asig_x_gpo <- data.frame(mat_asignacion,calif = 0, Prob_Ac = 0)
  
  #' Se pone un +5 si el profesor asignado es de TC
  ind_TC <- which(calif_asig_x_gpo[,3] == 1)
  calif_asig_x_gpo[ind_TC,5] <- 5
  
  #' Se pone un -1 por cada asignación que pudo haber tenido un
  #' profesor de TC y tiene un profesor de asignatura.
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
  
  mat_prof_TC <- mat_info_prof %>% filter(TC == 1)
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
  
  for(r in 1:dim(calif_asig_x_gpo)[1]){#Recorre renglones de "calif_asig_x_gpo"
    materia <- calif_asig_x_gpo[r,1]
    hora <- calif_asig_x_gpo[r,4]
    mat_aux <- materias_no_asignadas %>% filter(Materia == materia) %>% filter(
      Horario == hora)
    
    calif_asig_x_gpo[r,5] <- calif_asig_x_gpo[r,5] - dim(mat_aux)[1]
  }
  
  #' Para tener una calificación diferente para cada grupo, sumamos
  #' a cada renglón una épsilon:
  for(r in 1:dim(calif_asig_x_gpo)[1]){#Recorre renglones de "calif_asig_x_gpo"
    (num_al <- round(runif(1,0,0.1),4))
    if(calif_asig_x_gpo[r,5] >= 0){
      calif_asig_x_gpo[r,5] <- calif_asig_x_gpo[r,5] + num_al
    }else{
      calif_asig_x_gpo[r,5] <- calif_asig_x_gpo[r,5] - num_al
    }
  }
  calif_asig_x_gpo <- calif_asig_x_gpo[order(-calif_asig_x_gpo$calif),]
  
  #' Agregamos una columna con la probabilidad acumulada de elegir cada
  #' grupo.
  (n_gpos <- dim(calif_asig_x_gpo)[1])
  calif_asig_x_gpo[1,6] <- (2*1)/(n_gpos*(n_gpos+1))
  for(r in 2:dim(calif_asig_x_gpo)[1]){#Recorre renglones de "calif_asig_x_gpo"
    prob <- (2*r)/(n_gpos*(n_gpos+1))
    calif_asig_x_gpo[r,6] <- calif_asig_x_gpo[(r-1),6] + prob
  }
  cat("\nLa función califica_asig_x_gpo tardó: ",(proc.time()-ptm)[3],
      " segundos\n")
  return(calif_asig_x_gpo)
}



# Ej. ---------------------------------------------------------------------

calif_asig_x_gpo <- califica_asig_x_gpo(mat_solicitudes_real,
                                        mat_asignacion,param)#6.33/5.41 seg
View(calif_asig_x_gpo)


