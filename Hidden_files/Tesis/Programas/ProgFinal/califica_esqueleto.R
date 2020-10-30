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

# califica_esqueleto <- function(mat_demanda_alumnos,lista_info_esqueleto){
califica_esqueleto <- function(mat_demanda_alumnos,lista_info_esqueleto,
                               param){
  #Se definen las variables que se van a utilizar
  vec_nom_materias_total <- param$vec_nom_materias_total
  mat_esqueleto <- lista_info_esqueleto[[1]]
  mat_prof_TC <- lista_info_esqueleto[[2]]
  # mat_prof_asig <- lista_info_esqueleto[[3]]
  mat_demanda_aux <- lista_info_esqueleto[[4]]
  mat_solicitudes_TC <- lista_info_esqueleto[[5]]
  mat_solicitudes_asignatura <- lista_info_esqueleto[[6]]
  (num_alum_simulados <- lista_info_esqueleto[[7]])#34750
  calif_esqueleto <- 0
  
  #' Penalización por no tener en el esqueleto una materia que necesitamos.
  #' Se resta 1 por cada materia no impartida.
  materias_no_impartidas <- 0
  nom_materias_no_impartidas <- 0
  
  for(c in 1:dim(mat_esqueleto)[2]){#Se recorren las columnas
    suma_col <- sum(mat_demanda_alumnos[,c])
    if(suma_col!=0 && sum(mat_demanda_aux[,c])>=suma_col){
      materias_no_impartidas <- materias_no_impartidas + 1
      nom_materias_no_impartidas <- c(nom_materias_no_impartidas,
                                      vec_nom_materias_total[c])
      # cat("\n No fue impartida la materia: ",vec_nom_materias_total[c])
    }
  }
  materias_no_impartidas#31
  #Quitamos el cero inicial
  nom_materias_no_impartidas <- nom_materias_no_impartidas[-1]
  nom_materias_no_impartidas
  
  
  #' Penalización por cada alumno faltante: Se suma el número de alumnos
  #' que quedaron en la matriz "mat_demanda_aux". Se multiplica alfa
  #' por el número de alumnos faltantes
  alfa <- 0.5
  (num_alum_faltantes <- sum(mat_demanda_aux[mat_demanda_aux>0]))#10,930
  (pena_faltantes <- alfa*num_alum_faltantes)#5465
  
  
  #' Penalización por cada alumno sobrante: El número de alumnos
  #' sobrantes es el número de alumnos simulados menos el número de
  #' alumnos asignados (alumnos requeridos - alumnos faltantes)
  #' Se multiplica el número de alumnos sobrantes por beta.
  beta <- 0.8
  # (num_alum_requeridos <- sum(mat_demanda_alumnos))#34,955
  # (num_alum_asignados <- num_alum_requeridos-num_alum_faltantes)#13,000
  # (num_alum_sobrantes <- max(0,num_alum_simulados-num_alum_asignados))#10,354
  (num_alum_sobrantes <- -sum(mat_demanda_aux[mat_demanda_aux<0]))#10187
  (pena_sobrantes <- beta*num_alum_sobrantes)#8149.6
  
  
  #' Si algún profesor de tiempo completo pidió alguna materia y
  #' no se la dieron. Se penaliza con uno por cada materia.
  #' 
  #' Notas:
  #' 1) Se cuenta por materia solicitada, no por materia con
  #' horario. Ej. Si se pidió Proba I a las 10hrs y a las 11hrs,
  #' sólo se cuenta una penalización.
  #' 2) Se penaliza por cada materia con tope a 2 asignaciones,
  #' i.e. si un profesor pidió 3 o más  materias y sólo le dieron 1,
  #' entonces se penaliza 1; si le dieron 2 entonces no hay
  #' penalización.
  # mat_prof_TC_menor_2 <- mat_prof_TC[mat_prof_TC[,2]<=1,]
  # mat_solicitudes_TC_aux <- unique(mat_solicitudes_TC[,c(1,3,4)])
  pena_x_materia <- 0
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
  pena_x_materia##61
  
  
  #' Penalización por cada profesor que pueda impartir la materia j en
  #' la hora i y esa entrada de la matriz "mat_demanda_aux" aún tenga
  #' alumnos. Se penaliza por cada grupo que no se le asignó un profesor
  #' que si podía dar clase.
  # media_alum <- 34.18746
  mat_solicitudes <- rbind(mat_solicitudes_TC,mat_solicitudes_asignatura)
  mat_i_j <- matrix(0,nrow = dim(mat_solicitudes)[1],ncol = 2)
  pena_gpos_sin_prof <- 0
  
  for(r in 1:dim(mat_solicitudes)[1]){#Recorre los renglones
    #' Se llenan los índices en los que un profesor puede dar
    #' la materia j en la hora i
    ind_hora <- which(7:21 == mat_solicitudes[r,5])
    mat_i_j[r,] <- c(ind_hora,mat_solicitudes[r,4])
  }
  colnames(mat_i_j) <- c("i","j")
  
  for(r in 1:dim(mat_i_j)[1]){
    i <- as.numeric(mat_i_j[r,1])
    j <- as.numeric(mat_i_j[r,2])
    if(mat_demanda_aux[i,j] > 0){
      pena_gpos_sin_prof <- pena_gpos_sin_prof + 1
    }
  }
  pena_gpos_sin_prof#148
  
  #' Si hay alumnos que necesitan una clase a alguna hora y no
  #' existe profesor que la imparta.
  
  
  calif_esqueleto <- -sum(materias_no_impartidas,pena_faltantes,pena_sobrantes,
                         pena_x_materia,pena_gpos_sin_prof)#-13854.6
  return(calif_esqueleto)
}



# Tabla -------------------------------------------------------------------
tabla_info <- data.frame(Materia = vec_nom_materias_total,Num_Al_Sobra = 0,
                         Num_Al_Falta = 0,Num_Exactas = 0)
## El número de alumnos sobrantes se representa con números negativos.
for(d in 1:dim(tabla_info)[1]){
  #Recorre renglones de la tabla, columnas de la matriz
  tabla_info[d,2] <- sum(mat_demanda_aux[mat_demanda_aux[,d]<0,d])
  tabla_info[d,3] <- sum(mat_demanda_aux[mat_demanda_aux[,d]>0,d])
  ceros_antes <- length(mat_demanda_alumnos[mat_demanda_alumnos[,d]==0,d])
  ceros_despues <- length(mat_demanda_aux[mat_demanda_aux[,d]==0,d])
  tabla_info[d,4] <- ceros_despues - ceros_antes
}
View(tabla_info)


# Gráficas ----------------------------------------------------------------
sobran_mas_faltan <- as.numeric(tabla_info[,2] + tabla_info[,3])
# mat_sobran_mas_faltan <- cbind(tabla_info[,1],sobran_mas_faltan)
mat_sobran_mas_faltan <- data.frame(Materias = vec_nom_materias_total,
                                    Suma_Sobran_Faltan = sobran_mas_faltan)
mat_sobran_mas_faltan <- mat_sobran_mas_faltan[order(mat_sobran_mas_faltan[,2]),]
View(mat_sobran_mas_faltan)
mean(mat_sobran_mas_faltan[,2])

plot(as.numeric(sort(tabla_info[,2])))#Sobrantes
plot(as.numeric(sort(tabla_info[,3])))#Faltantes
plot(as.numeric(mat_sobran_mas_faltan[,2]))#Suma



# Histogramas -------------------------------------------------------------
vec_sobrantes <- as.numeric(sort(tabla_info[,2]))
min(vec_sobrantes)#-401
max(vec_sobrantes)#0

hist(vec_sobrantes,col=param_graficas$col1_hist,breaks = seq(-410,0,by = 10),
     freq = T,ylab = "Frecuencia",#ylim=c(0,0.025),
     main="Histograma alumnos sobrantes",xlab = "Número alumnos")


vec_faltantes <- as.numeric(sort(tabla_info[,3]))
min(vec_faltantes)#0
max(vec_faltantes)#755

hist(vec_faltantes,col=param_graficas$col1_hist,breaks = seq(0,780,by = 10),
     freq = T,ylab = "Frecuencia",#ylim=c(0,0.025),
     main="Histograma alumnos faltantes",xlab = "Número alumnos")

vec_exactos <- as.numeric(sort(tabla_info[,4]))
min(vec_exactos)#0
max(vec_exactos)#2

hist(vec_exactos,col=param_graficas$col1_hist,breaks = seq(0,10,by = 1),
     freq = T,ylab = "Frecuencia",#ylim=c(0,0.025),
     main="Histograma alumnos exactos",xlab = "Número alumnos")



hist(vec_sobrantes, col=param_graficas$col1_hist,
     breaks = seq(-410,780,by = 10),freq = F,ylim=c(0,0.07),
     ylab = "Frecuencia relativa",xlab = "Número alumnos",
     # ylab = "Densidad",xlab = "Número alumnos",
     main="Alumnos sobrantes y faltantes")
# lines(density(vec_sobrantes),col=param_graficas$col1_linea,
#       lwd=param_graficas$lwd_dens)
hist(vec_faltantes,col=param_graficas$col2_hist,
     breaks = seq(0,780,by = 10),freq = F,add=TRUE)
# lines(density(vec_faltantes),col=param_graficas$col2_linea,
#       lwd=param_graficas$lwd_dens)


mat_porcentajes <- mat_demanda_aux/mat_demanda_alumnos
colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
heatmap(mat_porcentajes, Colv = NA, Rowv = NA, scale="none",col=colMain,
        main = "Porcentajes alumnos sobrantes y faltantes")

mat_porcentajes <- matrix(0,dim(mat_demanda_aux)[1],dim(mat_demanda_aux)[2])
for(c in 1:dim(mat_demanda_aux)[2]){
  for(r in 1:dim(mat_demanda_aux)[1]){
    if(mat_demanda_alumnos[r,c]!=0){
      # mat_porcentajes <- mat_demanda_aux/
      mat_porcentajes[r,c] <- mat_demanda_aux[r,c]/mat_demanda_alumnos[r,c]
    }else{
      mat_porcentajes[r,c] <- 0
    }
  }
}

colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
heatmap(mat_porcentajes, Colv = NA, Rowv = NA, scale="none",col=colMain,
        main = "Porcentajes alumnos sobrantes y faltantes")

min(mat_porcentajes)#-74
max(mat_porcentajes)#1
hist(as.vector(mat_porcentajes),col=param_graficas$col1_hist,breaks = seq(-80,10,by = 10),
     freq = F,ylab = "Frecuencia",#ylim=c(0,0.025),
     main="Histograma mat_porcentajes",xlab = "Número alumnos")




mat_diferencias <- mat_demanda_alumnos - mat_demanda_aux
colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
heatmap(mat_diferencias, Colv = NA, Rowv = NA, scale="none",col=colMain,
        main = "Porcentajes alumnos sobrantes y faltantes")

mat_dif_rel <- (mat_demanda_alumnos - mat_demanda_aux)/mat_demanda_alumnos
colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
heatmap(mat_dif_rel, Colv = NA, Rowv = NA, scale="none",col=colMain,
        main = "Porcentajes alumnos sobrantes y faltantes")






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

