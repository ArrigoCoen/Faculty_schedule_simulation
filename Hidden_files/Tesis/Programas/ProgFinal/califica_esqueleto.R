##########################################################################
#' En este programa se encuentra la función que califica esqueletos.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")



# califica_esqueleto ------------------------------------------------------

califica_esqueleto <- function(mat_demanda_alumnos,lista_info_esqueleto){
# califica_esqueleto <- function(mat_demanda_alumnos,lista_info_esqueleto,
# param){
  #Se definen las variables que se van a utilizar
  # vec_nom_materias_total <- param$vec_nom_materias_total
  mat_esqueleto <- lista_info_esqueleto[[1]]
  mat_prof_TC <- lista_info_esqueleto[[2]]
  # mat_prof_asig <- lista_info_esqueleto[[3]]
  mat_demanda_aux <- lista_info_esqueleto[[4]]
  mat_solicitudes_TC <- lista_info_esqueleto[[5]]
  mat_solicitudes_asignatura <- lista_info_esqueleto[[6]]
  num_alum_simulados <- lista_info_esqueleto[[7]]
  calif_esqueleto <- 0
  
  #Penalización por no tener en el esqueleto una materia que necesitamos
  materias_no_impartidas <- 0
  nom_materias_no_impartidas <- 0
  
  for(c in 1:dim(mat_esqueleto)[2]){#Se recorren las columnas
    suma_col <- sum(mat_demanda_alumnos[,c])
    if(suma_col!=0 && sum(mat_demanda_aux[,c])>=suma_col){
      materias_no_impartidas <- materias_no_impartidas + 1
      # nom_materias_no_impartidas <- c(nom_materias_no_impartidas,
      #                                 vec_nom_materias_total[c])
      # cat("\n No fue impartida la materia: ",vec_nom_materias_total[c])
    }
  }
  # Lmateria <- -materias_no_impartidas
  #Quitamos el cero inicial
  # nom_materias_no_impartidas <- nom_materias_no_impartidas[-1]
  # cat("\n Las materias no impartidas son: ",nom_materias_no_impartidas)
  
  
  #Penalización por cada alumno faltante
  alfa <- 0.5
  num_alum_faltantes <- sum(mat_demanda_aux)
  pena_faltantes <- alfa*num_alum_faltantes
  
  
  #Penalización por cada alumno sobrante
  beta <- 0.8
  alum_con_gpo <- sum(mat_demanda_alumnos)-sum(mat_demanda_aux)
  num_alum_sobrantes <- max(0,num_alum_simulados-alum_con_gpo)
  pena_sobrantes <- beta*num_alum_sobrantes
  
  
  #' Si algún profesor de tiempo completo pidió alguna materia y
  #' no se la dieron.
  #' 
  #' Notas:
  #' 1) Se cuenta por materia solicitada, no por materia con
  #' horario. Ej. Si se pidió Proba I a las 10hrs y a las 11hrs,
  #' sólo se cuenta una penalización.
  #' 2) Se penaliza por cada materia con tope a 2 asignaciones,
  #' i.e. si un profesor pidió 3 materias y sólo le dieron 1,
  #' entonces se penaliza 1; si le dieron 2 entonces no hay
  #' penalización.
  # mat_prof_TC_menor_2 <- mat_prof_TC[mat_prof_TC[,2]<=1,]
  # mat_solicitudes_TC_aux <- unique(mat_solicitudes_TC[,c(1,3,4)])
  pena_x_materia <- 0
  # pena_2_materias <- 0
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
  # pena_x_materia <- -pena_x_materia
  
  
  #' Si hay alumnos que necesitan una clase a alguna hora y no
  #' existe profesor que la imparta
  # media_alum <- 34.18746
  mat_solicitudes <- rbind(mat_solicitudes_TC,mat_solicitudes_asignatura)
  mat_i_j <- matrix(0,nrow = dim(mat_solicitudes)[1],ncol = 2)
  pena_alum_sin_clase <- 0
  
  for(r in 1:dim(mat_solicitudes)[1]){#Recorre los renglones
    ind_hora <- which(7:21 == mat_solicitudes[r,5])
    mat_i_j[r,] <- c(ind_hora,mat_solicitudes[r,4])
  }
  colnames(mat_i_j) <- c("i","j")
  
  for(r in 1:dim(mat_i_j)[1]){
    if(mat_demanda_aux[mat_i_j[r,1],mat_i_j[r,2]] > 0){
      pena_alum_sin_clase <- pena_alum_sin_clase + 1
    }
  }
  # pena_alum_sin_clase <- -pena_alum_sin_clase
  
  calif_esqueleto <- -sum(materias_no_impartidas,pena_faltantes,pena_sobrantes,
                         pena_x_materia,pena_alum_sin_clase)
  return(calif_esqueleto)
}


# Ej. ---------------------------------------------------------------------


(calif_esqueleto <- califica_esqueleto(mat_demanda_alumnos,
                                       lista_info_esqueleto))



# sdfg --------------------------------------------------------------------
View(mat_esqueleto)
View(mat_prof_TC)
View(mat_prof_asig)
View(mat_demanda_alumnos)
View(mat_demanda_aux)
View(mat_solicitudes_TC)
View(mat_prof_TC_igual_1)
View(mat_prof_TC_igual_0)

sum(mat_demanda_alumnos)#35,305
sum(mat_demanda_aux)#11,352


param$vec_nom_materias_total[c(41,45,53,75,100,135,150,152,156,159,174,
                               200,210,219,220,222,234,240,241,243,247,
                               275,284,285,288,289,300,303,304,307,317,
                               322,323)]

