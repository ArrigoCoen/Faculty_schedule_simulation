##########################################################################
#' En este programa se encuentran las funciones que se encargan de hacer
#' las pruebas para calificar esqueletos.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")



# pruebas_calificar_esqueletos --------------------------------------------
pruebas_calificar_esqueletos <- function(lista_info_esqueleto,
                                         mat_demanda_alumnos,param){
  #Se definen las variables que se van a utilizar
  E <- lista_info_esqueleto[[8]]#Matriz con el número de alumnos simulados
  #en los grupos hechos para "mat_esqueleto"
  D <- mat_demanda_alumnos#Matriz con el número de alumnos simulados
  #para el siguiente semestre
  bin_DUE <- matrix(0,nrow = dim(E)[1],ncol = dim(E)[2])#Matriz binaria que
  #tiene 1 en la entrada (i,j) si E o D tienen información distinta de
  #cero en la entrada (i,j). Tiene 0 en la entrada (i,j) si E y D tienen
  #un cero en la entrada (i,j).
  rownames(E) <- param$nombre_hrs
  colnames(E) <- param$vec_nom_materias_total
  rownames(bin_DUE) <- param$nombre_hrs
  colnames(bin_DUE) <- param$vec_nom_materias_total
  
  for(c in 1:dim(E)[2]){
    for(r in 1:dim(E)[1]){
      if(E[r,c]!=0 || D[r,c]!=0){
        bin_DUE[r,c] <- 1
      }
    }#Fin for(r)
  }#Fin for(c)
  
  
  #Se definen los vectores con información por materia
  D_x_materia <- colSums(D)
  E_x_materia <- colSums(E)
  
  #Se definen las matrices de errores
  mat_errores <- D-E
  mat_errores_relativos <- matrix(0,nrow = dim(E)[1],ncol = dim(E)[2])
  
  for(c in 1:dim(E)[2]){
    for(r in 1:dim(E)[1]){
      if(bin_DUE[r,c] == 1){
        mat_errores_relativos[r,c] <- (D[r,c]-E[r,c])/D[r,c]
      }
    }#Fin for(r)
  }#Fin for(c)
  rownames(mat_errores_relativos) <- param$nombre_hrs
  colnames(mat_errores_relativos) <- param$vec_nom_materias_total
  
  ### Histogramas por grupo
  #Ponemos todos los datos en un vector
  vec_x_gpo_mat_errores <- as.vector(mat_errores)
  vec_x_gpo_mat_errores <- vec_x_gpo_mat_errores[vec_x_gpo_mat_errores!=0]
  
  vec_x_gpo_mat_errores_relativos <- as.vector(mat_errores_relativos)
  vec_x_gpo_mat_errores_relativos <- vec_x_gpo_mat_errores_relativos[
    vec_x_gpo_mat_errores_relativos!=0]
  vec_x_gpo_mat_errores_relativos <- vec_x_gpo_mat_errores_relativos[
    vec_x_gpo_mat_errores_relativos>-Inf]
  
  min(vec_x_gpo_mat_errores)#-151
  max(vec_x_gpo_mat_errores)#361
  min(vec_x_gpo_mat_errores_relativos)#-67
  max(vec_x_gpo_mat_errores_relativos)#1
  
  hist(vec_x_gpo_mat_errores,col=param_graficas$col1_hist,
       breaks = seq(-160,370,by = 10),
       ylab = "Frecuencia relativa",
       # xlab = "Diferencia entre el número de alumnos esperados menos el número de alumnos simulados",
       xlab = "Diferencia por grupo",
       freq = F,main="Histograma de D-E")
  
  hist(vec_x_gpo_mat_errores_relativos,col=param_graficas$col1_hist,
       breaks = seq(-70,10,by = 10),
       ylab = "Frecuencia relativa",xlab = "Diferencia relativa por grupo",freq = F,
       main="Histograma de (D-E)/D")
  
  
  ### Histogramas por materia
  vec_errores_x_materia <- colSums(mat_errores)
  vec_errores_x_materia <- vec_errores_x_materia[vec_errores_x_materia!=0]
  
  vec_errores_relativos_x_materia <- colSums(mat_errores_relativos)
  vec_errores_relativos_x_materia <- vec_errores_relativos_x_materia[
    vec_errores_relativos_x_materia!=0]
  vec_errores_relativos_x_materia <- vec_errores_relativos_x_materia[
    vec_errores_relativos_x_materia>-Inf]
  
  min(vec_errores_x_materia)#-340
  max(vec_errores_x_materia)#274
  min(vec_errores_relativos_x_materia)#-94.87168
  max(vec_errores_relativos_x_materia)#3.33
  
  hist(vec_errores_x_materia,col=param_graficas$col1_hist,
       breaks = seq(-340,280,by = 10),
       ylab = "Frecuencia relativa",
       xlab = "Diferencia por materia",
       freq = F,main="Histograma de D-E")
  
  hist(vec_errores_relativos_x_materia,col=param_graficas$col1_hist,
       breaks = seq(-100,10,by = 10),
       ylab = "Frecuencia relativa",xlab = "Diferencia relativa por materia",freq = F,
       main="Histograma de (D-E)/D")
  
  
  
  # vec_errores_x_materia <- D_x_materia-E_x_materia
  # vec_errores_x_materia <- vec_errores_x_materia[vec_errores_x_materia!=0]
  # 
  # vec_errores_relativos_x_materia <- (D_x_materia-E_x_materia)/D_x_materia
  # vec_errores_relativos_x_materia <- vec_errores_relativos_x_materia[
  #   vec_errores_relativos_x_materia!=0]
  # vec_errores_relativos_x_materia <- vec_errores_relativos_x_materia[
  #   vec_errores_relativos_x_materia>-Inf]
  # vec_errores_relativos_x_materia <- vec_errores_relativos_x_materia[
  #   !is.nan(vec_errores_relativos_x_materia)]
  # vec_errores_relativos_x_materia <- vec_errores_relativos_x_materia[
  #   !is.na(vec_errores_relativos_x_materia)]
  # 
  # 
  # hist(vec_errores_relativos_x_materia,col=param_graficas$col1_hist,
  #      breaks = seq(-90,10,by = 10),
  #      ylab = "Frecuencia",xlab = "Diferencia relativa",freq = T,
  #      main="Histograma de (D-E)/D")
  
  
  
  
  
  
}




# sdfgb -------------------------------------------------------------------
View(E)
View(D)
View(bin_DUE)

