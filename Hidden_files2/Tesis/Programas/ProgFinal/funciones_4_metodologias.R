##########################################################################
#' En este programa se encuentran las funciones de las 4 metodologías
#' para generar un esqueleto. Se califican y se elige una de ellas.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")


# Metodología A -----------------------------------------------------------
#' Title metodo_A: Función que genera un esqueleto con la metodología A, la
#' cual implementa la mezcla de normales por materia y modifica el número de
#' alumnos en D si la calificación por materia está fuera de [-20,10] y 
#' si la calificación por grupo está fuera de [-10,10].
#' En esta metodología en la función "gen_normalmixEM_inicial" el modelo
#' se genera con el promedio de los datos, no se utiliza el parámatro k
#' que es el número de normales que tiene el modelo. El esqueleto se simula
#' con la función "gen_esqueleto" utilizando la matriz D' actualizada.
#'
#' @param cota: Cota para que el ciclo no sea infinito.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @param param_sim: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se encargan de la simulación.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' @example param_sim <- list(vec_sem_sig = c(20191,20192,20201),k_sem_ant = 5,
#' materia = "Estadística III", num_sim = 10, m_filtrada = matrix(0),
#' sub_m_filtrada = matrix(0,ncol = length(param$nom_cols_MG)))
#'
#' @return lista_esq_D_prima: Lista que contiene 2 matrices, la matriz
#' "mat_esqueleto" con el esqueleto generado y la matriz "D_prima" la
#' cual contiene la matriz de demanda de alumnos con la que se generó el
#' esqueleto.
#'
#' @examples
#' lista_esq_D_prima <- metodo_A(cota,param,param_sim)
#' 
metodo_A <- function(cota,param,param_sim){
  ptm <- proc.time()# Start the clock!
  #' Simulamos D0 (matriz de demanda con la que vamos a calificar a la
  #' matriz D').
  D0 <- gen_mat_demanda_alumnos(param,param_sim)#45.94 seg
  
  ### Obtener D'0 = E
  mat_solicitudes <- gen_solicitudes(param)#7.21 seg
  lista_info_esqueleto <- gen_esqueleto(D0,mat_solicitudes,param)#13.58 seg
  E <- lista_info_esqueleto[[8]]#Matriz con el número de alumnos simulados
  
  ### Aplicar mezcla de normales inicial
  vec_s_sem_k_info <- gen_vec_s_sem_k_info(param_sim$vec_sem_sig,
                                           param_sim$k_sem_ant,param)
  lista_mod_y_wait <- gen_normalmixEM_inicial(vec_s_sem_k_info,E,
                                              param,param_sim)
  
  ### Obtener D' para generar esqueleto
  D_prima <-  gen_D_prima(D0,E,lista_mod_y_wait,cota)
  
  ##Calificamos D_prima
  # mat_calif_x_gpo <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])
  # ind_materias <- 1:dim(D)[2]
  # calif_D <- actualiza_calif_D(D0,D_prima,mat_calif_x_gpo,ind_materias)
  # vec_calif_x_materia <- calif_D[[2]]
  # View(vec_calif_x_materia)
  
  ##Generar esqueleto
  mat_solicitudes <- gen_solicitudes(param)#8.94 seg
  lista_info_esqueleto <- gen_esqueleto(D_prima,mat_solicitudes,param)#14.05 seg
  mat_esqueleto <- lista_info_esqueleto[[1]]
  rownames(mat_esqueleto) <- param$Horas
  colnames(mat_esqueleto) <- param$vec_nom_materias_total
  
  
  lista_esq_D_prima <- list()
  lista_esq_D_prima[[1]] <- mat_esqueleto
  lista_esq_D_prima[[2]] <- D_prima
  
  cat("\nLa metodología A tardó: ",(proc.time()-ptm)[3]/60,
      " minutos\n")
  return(lista_esq_D_prima)
}


# Ej. ---------------------------------------------------------------------
cota <- 1000
lista_esq_D_prima <- metodo_A(cota,param,param_sim)#2.136833
mat_esqueleto <- lista_esq_D_prima[[1]]
D_prima <- lista_esq_D_prima[[2]]
View(mat_esqueleto)
View(D_prima)


##Calificamos D_prima
D <- gen_mat_demanda_alumnos(param,param_sim)#57.92 seg
View(D)
# D_prima <- D_prima
mat_calif_x_gpo <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])
ind_materias <- 1:dim(D)[2]
calif_D <- actualiza_calif_D(D,D_prima,mat_calif_x_gpo,ind_materias)
mat_calif_x_gpo <- calif_D[[1]]
vec_calif_x_materia <- calif_D[[2]]
# View(mat_calif_x_gpo)
View(vec_calif_x_materia)

(calif_esqueleto_2 <- sum(vec_calif_x_materia))#-453.9001



# Gráfica de metodología A ------------------------------------------------
cota <- 1000
n_calif <- 10
mat_calif_A <- matrix(0,nrow = n_calif,ncol = dim(D)[2])
# D0 <- gen_mat_demanda_alumnos(param,param_sim)#57.92 seg
# View(D)
ind_materias <- 1:dim(D0)[2]
ptm <- proc.time()# Start the clock!
for(d in 10:n_calif){
  cat("\n***RENGLÓN ",d," ***")
  lista_esq_D_prima <- metodo_A(cota,param,param_sim)#2.136833
  D_prima <- lista_esq_D_prima[[2]]
  mat_calif_x_gpo <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])
  calif_D <- actualiza_calif_D(D0,D_prima,mat_calif_x_gpo,ind_materias)
  vec_calif_x_materia <- calif_D[[2]]
  
  mat_calif_A[d,] <- vec_calif_x_materia
}
cat("\nEl ciclo tardó: ",(proc.time()-ptm)[3]/60," minutos\n")
## 7.41 min ERROR d = 1:4
## 7.11 min ERROR d = 4:7
## 2.92 min ERROR d = 7:8
## 5.57 min ERROR d = 8:10

## 11.26 min ERROR d = 1:6
## 3.13 min ERROR d = 6:7
## 4.69 min ERROR d = 7:9
## 2.95 min ERROR d = 9
## 1.78 min ERROR d = 10

colnames(mat_calif_A) <- param$vec_nom_materias_total
View(mat_calif_A)
matplot(mat_calif_A, type = "l",main = "Metodología A",xlab = "Iteraciones",
        ylab = "Calificación",ylim = c(-5,1))

save(mat_calif_A,file = "mat_calif_A.RData")


# Metodología B -----------------------------------------------------------
#' Title metodo_B: Función que genera un esqueleto con la metodología B, la
#' cual implementa la mezcla de normales por esqueleto. Se genera un modelo
#' inicial con k = 3. Valor elegido al ver el histograma de los datos
#' en un esqueleto. En el modelo final se utiliza la media del modelo inical
#' como parámetro.
#'
#' @param n_rep: Número de veces que se generarán los esqueletos para
#' obtener información.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @param param_sim: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se encargan de la simulación.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' @example param_sim <- list(vec_sem_sig = c(20191,20192,20201),k_sem_ant = 5,
#' materia = "Estadística III", num_sim = 10, m_filtrada = matrix(0),
#' sub_m_filtrada = matrix(0,ncol = length(param$nom_cols_MG)))
#'
#' @return lista_esq_D_prima: Lista que contiene 2 matrices, la matriz
#' "mat_esqueleto" con el esqueleto generado y la matriz "D_prima" la
#' cual contiene el primedio de los esqueletos generados "n_rep" veces.
#'
#' @examples
#' lista_esq_D_prima <- metodo_B(n_rep,param,param_sim)
#' 
metodo_B <- function(n_rep,param,param_sim){
  ptm <- proc.time()# Start the clock!
  ### Obtener D_prima
  D_prima_inicial <- gen_mat_demanda_alumnos(param,param_sim)#46.41 seg
  prom_D <- D_prima_inicial
  
  ##Generar esqueleto inicial
  mat_solicitudes <- gen_solicitudes(param)#8.07 seg
  lista_info_esqueleto <- gen_esqueleto(D_prima_inicial,mat_solicitudes,param)#13.35 seg
  mat_esqueleto <- lista_info_esqueleto[[1]]
  
  ##Convertimos los datos para obtener la distribución por horas
  wait_mat_esqueleto <- 0
  Horas <- param$Horas
  for(h in 1:length(Horas)){
    suma_x_hra <- sum(mat_esqueleto[h,])
    if(suma_x_hra > 0){
      wait_mat_esqueleto <- c(wait_mat_esqueleto,rep(Horas[h],suma_x_hra))
    }
  }
  #Quitamos el cero inicial
  wait_mat_esqueleto <- wait_mat_esqueleto[-1]
  
  #' Definimos las listas en las que vamos a guardar el número de grupos por materia
  #' y los modelos de mezcla de normales para cada esqueleto
  mat_gpos_x_materia <- matrix(0,nrow = n_rep,ncol = dim(mat_esqueleto)[2])
  mat_gpos_x_materia[1,] <- colSums(mat_esqueleto)
  mixmdl_1_esqueleto <- normalmixEM(wait_mat_esqueleto,k = 3)
  
  # hist(wait_mat_esqueleto,freq = F,breaks = seq(7,22,by = 1),)
  # lines(density(rnorm(1000,mean = mixmdl_1_esqueleto$mu,sd = mixmdl_1_esqueleto$sigma)),
  #       lty=1,lwd=2,col = "blue")
  # lines(density(wait_mat_esqueleto), lty=1,lwd=2,col = "green")
  # legend(15,0.15,c("GMM","density()"),bty = "n",
  #        col=c("blue","green"),lty=c(1,1),
  #        cex=1.1,lwd=2)
  
  #Hacemos "n_rep" veces el proceso
  for(d in 2:n_rep){
    cat("d = ",d)
    ### Obtener D
    D <- gen_mat_demanda_alumnos(param,param_sim)
    prom_D <- prom_D + D
    
    ##Generar esqueleto
    mat_solicitudes <- gen_solicitudes(param)
    lista_info_esqueleto <- gen_esqueleto(D,mat_solicitudes,param)
    mat_esqueleto <- lista_info_esqueleto[[1]]
    mat_gpos_x_materia[d,] <- colSums(mat_esqueleto)
    # lista_de_lista_info_esqueleto[[d]] <- lista_info_esqueleto
    
    ##Convertimos los datos para obtener la distribución por horas
    for(h in 1:length(Horas)){
      suma_x_hra <- sum(mat_esqueleto[h,])
      if(suma_x_hra > 0){
        wait_mat_esqueleto <- c(wait_mat_esqueleto,rep(Horas[h],suma_x_hra))
      }
    }
  }#8.963333 min
  mixmdl_esqueleto <- normalmixEM(wait_mat_esqueleto,mean=mixmdl_1_esqueleto$mu)
  
  prom_gpos_x_materia <- ceiling(colMeans(mat_gpos_x_materia))
  
  #Generamos el esqueleto final
  mat_esqueleto_final <- matrix(0,nrow = length(param$Horas),
                                ncol = length(param$vec_nom_materias_total))
  for(c in 1:length(param$vec_nom_materias_total)){
    num_gpos_1_materia <- prom_gpos_x_materia[c]
    (rand_num <- sort(round(rnorm(num_gpos_1_materia,mixmdl_esqueleto$mu,
                                  mixmdl_esqueleto$sigma))))
    ind_7 <- which(rand_num < 8)
    ind_22 <- which(rand_num >= 21)
    
    if(length(ind_7) > 0){
      rand_num[ind_7] <- 7
    }
    if(length(ind_22) > 0){
      rand_num[ind_22] <- 21
    }
    
    for(r in 1:length(param$Horas)){
      ind_hrs <- which(rand_num == param$Horas[r])
      if(length(ind_hrs) > 0){
        mat_esqueleto_final[r,c] <- length(ind_hrs)
      }
    }
  }#Fin for(c)
  # View(mat_esqueleto_final)
  rownames(mat_esqueleto_final) <- param$Horas
  colnames(mat_esqueleto_final) <- param$vec_nom_materias_total
  
  lista_esq_D_prima <- list()
  lista_esq_D_prima[[1]] <- mat_esqueleto_final
  lista_esq_D_prima[[2]] <- ceiling(prom_D/n_rep)
  cat("\nLa función metodo_B tardó: ",(proc.time()-ptm)[3]/60," minutos\n")
  return(lista_esq_D_prima)
}
# Ej. ---------------------------------------------------------------------
n_rep <- 10

lista_esq_D_prima <- metodo_B(n_rep,param,param_sim)#10.71983


# Gráfica de metodología B ------------------------------------------------
n_rep <- 5
n_calif <- 10
mat_calif_B <- matrix(0,nrow = n_calif,ncol = dim(D)[2])
# D <- gen_mat_demanda_alumnos(param,param_sim)#39.72 seg
# View(D)
ind_materias <- 1:dim(D0)[2]
ptm <- proc.time()# Start the clock!
for(d in 1:n_calif){
  cat("\n***RENGLÓN ",d," ***")
  lista_esq_D_prima <- metodo_B(n_rep,param,param_sim)
  D_prima <- lista_esq_D_prima[[2]]
  mat_calif_x_gpo <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])
  calif_D <- actualiza_calif_D(D0,D_prima,mat_calif_x_gpo,ind_materias)
  vec_calif_x_materia <- calif_D[[2]]
  
  mat_calif_B[d,] <- vec_calif_x_materia
}
cat("\nEl ciclo tardó: ",(proc.time()-ptm)[3]/60," minutos\n")
## 52.31567 min d = 1:10
## 51.88 min d = 1:10
colnames(mat_calif_B) <- param$vec_nom_materias_total
View(mat_calif_B)
matplot(mat_calif_B, type = "l",main = "Metodología B",xlab = "Iteraciones",
        ylab = "Calificación",ylim = c(-0.5,1))

save(mat_calif_B,file = "mat_calif_B.RData")


# Metodología C -----------------------------------------------------------

#' Title metodo_B2: Función que genera un esqueleto con la metodología B2, la
#' cual implementa la mezcla de normales por esqueleto. Se genera un modelo
#' inicial con k = 3. Valor elegido al ver el histograma de los datos
#' en un esqueleto. En el modelo final se utiliza la media del modelo inical
#' como parámetro.
#'
#' @param n_rep: Número de veces que se generarán los esqueletos para
#' obtener información.
#' @param D_prima_inicial: Matriz de 15 renglones (horas) y 203 columnas
#' (materias). En la entrada (i,j) se tiene el número de alumnos simulados
#' para la hora i, y la materia j.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @param param_sim: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se encargan de la simulación.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' @example param_sim <- list(vec_sem_sig = c(20191,20192,20201),k_sem_ant = 5,
#' materia = "Estadística III", num_sim = 10, m_filtrada = matrix(0),
#' sub_m_filtrada = matrix(0,ncol = length(param$nom_cols_MG)))
#'
#' @return lista_esq_D_prima: Lista que contiene 2 matrices, la matriz
#' "mat_esqueleto" con el esqueleto generado y la matriz "D_prima" la
#' cual contiene el primedio de los esqueletos generados "n_rep" veces.
#'
#' @examples
#' lista_esq_D_prima <- metodo_B2(n_rep,D_prima_inicial,param,param_sim)
#' 
metodo_B2 <- function(n_rep,D_prima_inicial,param,param_sim){
  ptm <- proc.time()# Start the clock!
  ##Generar esqueleto inicial
  prom_D <- D_prima_inicial
  mat_solicitudes <- gen_solicitudes(param)#8.07 seg
  lista_info_esqueleto <- gen_esqueleto(D_prima_inicial,mat_solicitudes,param)#13.35 seg
  mat_esqueleto <- lista_info_esqueleto[[1]]
  
  ##Convertimos los datos para obtener la distribución por horas
  wait_mat_esqueleto <- 0
  Horas <- param$Horas
  for(h in 1:length(Horas)){
    suma_x_hra <- sum(mat_esqueleto[h,])
    if(suma_x_hra > 0){
      wait_mat_esqueleto <- c(wait_mat_esqueleto,rep(Horas[h],suma_x_hra))
    }
  }
  #Quitamos el cero inicial
  wait_mat_esqueleto <- wait_mat_esqueleto[-1]
  
  #' Definimos las listas en las que vamos a guardar el número de grupos por materia
  #' y los modelos de mezcla de normales para cada esqueleto
  mat_gpos_x_materia <- matrix(0,nrow = n_rep,ncol = dim(mat_esqueleto)[2])
  mat_gpos_x_materia[1,] <- colSums(mat_esqueleto)
  mixmdl_1_esqueleto <- normalmixEM(wait_mat_esqueleto,k = 3)
  
  #Hacemos "n_rep" veces el proceso
  for(d in 2:n_rep){
    cat("d = ",d)
    ### Obtener D
    # D <- gen_mat_demanda_alumnos(param,param_sim)
    # prom_D <- prom_D + D
    ##Generar esqueleto
    mat_solicitudes <- gen_solicitudes(param)
    lista_info_esqueleto <- gen_esqueleto(D_prima_inicial,
                                          mat_solicitudes,param)
    mat_esqueleto <- lista_info_esqueleto[[1]]
    mat_gpos_x_materia[d,] <- colSums(mat_esqueleto)
    
    ##Convertimos los datos para obtener la distribución por horas
    for(h in 1:length(Horas)){
      suma_x_hra <- sum(mat_esqueleto[h,])
      if(suma_x_hra > 0){
        wait_mat_esqueleto <- c(wait_mat_esqueleto,rep(Horas[h],suma_x_hra))
      }
    }
  }#8.963333 min
  mixmdl_esqueleto <- normalmixEM(wait_mat_esqueleto,
                                  mean=mixmdl_1_esqueleto$mu)
  
  prom_gpos_x_materia <- ceiling(colMeans(mat_gpos_x_materia))
  
  #Generamos el esqueleto final
  mat_esqueleto_final <- matrix(0,nrow = length(param$Horas),
                                ncol = length(param$vec_nom_materias_total))
  for(c in 1:length(param$vec_nom_materias_total)){
    num_gpos_1_materia <- prom_gpos_x_materia[c]
    (rand_num <- sort(round(rnorm(num_gpos_1_materia,mixmdl_esqueleto$mu,
                                  mixmdl_esqueleto$sigma))))
    ind_7 <- which(rand_num < 8)
    ind_22 <- which(rand_num >= 21)
    
    if(length(ind_7) > 0){
      rand_num[ind_7] <- 7
    }
    if(length(ind_22) > 0){
      rand_num[ind_22] <- 21
    }
    
    for(r in 1:length(param$Horas)){
      ind_hrs <- which(rand_num == param$Horas[r])
      if(length(ind_hrs) > 0){
        mat_esqueleto_final[r,c] <- length(ind_hrs)
      }
    }
  }#Fin for(c)
  # View(mat_esqueleto_final)
  rownames(mat_esqueleto_final) <- param$Horas
  colnames(mat_esqueleto_final) <- param$vec_nom_materias_total
  
  lista_esq_D_prima <- list()
  lista_esq_D_prima[[1]] <- mat_esqueleto_final
  lista_esq_D_prima[[2]] <- D_prima_inicial
  # lista_esq_D_prima[[2]] <- ceiling(prom_D/n_rep)
  cat("\nLa función metodo_B2 tardó: ",(proc.time()-ptm)[3]/60," minutos\n")
  return(lista_esq_D_prima)
}


#' Title metodo_C: Función que genera un esqueleto con la metodología C, la
#' cual implementa la mezcla de normales por número de alumnos y por
#' esqueleto. Para el número de alumnos se genera un modelo inicial con k = 4.
#' Valor elegido al ver el histograma de los datos del número de alumnos. Para
#' el esqueleto se aplica la metodología B.
#'
#' @param n_rep: Número de veces que se generarán los esqueletos para
#' obtener información.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @param param_sim: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se encargan de la simulación.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' @example param_sim <- list(vec_sem_sig = c(20191,20192,20201),k_sem_ant = 5,
#' materia = "Estadística III", num_sim = 10, m_filtrada = matrix(0),
#' sub_m_filtrada = matrix(0,ncol = length(param$nom_cols_MG)))
#'
#' @return lista_esq_D_prima: Lista que contiene 2 matrices, la matriz
#' "mat_esqueleto" con el esqueleto generado y la matriz "D_prima" la
#' cual contiene el primedio de los esqueletos generados "n_rep" veces.
#'
#' @examples
#' lista_esq_D_prima <- metodo_C(n_rep,param,param_sim)
#' 
metodo_C <- function(n_rep,param,param_sim){
  ptm <- proc.time()# Start the clock!
  ### Obtener D
  D_inicial <- gen_mat_demanda_alumnos(param,param_sim)#42.96 seg
  
  #' Definimos las listas en las que vamos a guardar el número de alumnos
  #' por materia
  num_alum_x_materia <- list()
  num_alum_x_materia[[1]] <- colSums(D_inicial)
  D <- D_inicial
  ##Convertimos los datos para obtener la distribución por horas
  wait_alumnos <- 0
  Horas <- param$Horas
  for(h in 1:length(Horas)){
    suma_x_hra <- sum(D[h,])
    if(suma_x_hra > 0){
      wait_alumnos <- c(wait_alumnos,rep(Horas[h],suma_x_hra))
    }
  }
  #Quitamos el cero inicial
  wait_alumnos <- wait_alumnos[-1]
  # save(wait_alumnos,file = "wait_alumnos.RData")
  
  #' Definimos la lista en las que vamos a guardar el número de alumnos
  #' por materia
  num_alum_x_materia <- list()
  num_alum_x_materia[[1]] <- colSums(D)
  mixmdl_1_D <- normalmixEM(wait_alumnos,k = 4)#Modelo inicial
  
  # hist(wait_alumnos,freq = F,breaks = seq(6,22,by = 1),)
  # lines(density(rnorm(1000,mean = mixmdl_1_D$mu,sd = mixmdl_1_D$sigma)),
  #       lty=1,lwd=2,col = "blue")
  # lines(density(wait_alumnos), lty=1,lwd=2,col = "green")
  # legend(15,0.14,c("GMM","density()"),bty = "n",
  #        col=c("blue","green"),lty=c(1,1),
  #        cex=1.1,lwd=2)
  
  #Hacemos "n_rep" veces el proceso
  for(d in 2:n_rep){
    cat("d = ",d)
    ### Obtener D
    D <- gen_mat_demanda_alumnos(param,param_sim)
    num_alum_x_materia[[d]] <- colSums(D)
    ##Convertimos los datos para obtener la distribución por horas
    for(h in 1:length(Horas)){
      suma_x_hra <- sum(D[h,])
      if(suma_x_hra > 0){
        wait_alumnos <- c(wait_alumnos,rep(Horas[h],suma_x_hra))
      }
    }
  }#2.7805 min
  
  ### Obtenemos el número promedio de grupos por materia
  mat_alum_x_materia <- matrix(0,nrow = n_rep,
                               ncol = length(param$vec_nom_materias_total))
  
  for(r in 1:n_rep){#Recorre las listas
    mat_alum_x_materia[r,] <- num_alum_x_materia[[r]]
  }
  prom_alum_x_materia <- ceiling(colMeans(mat_alum_x_materia))
  
  #Generamos la matriz D final
  D_final <- matrix(0,nrow = length(param$Horas),
                    ncol = length(param$vec_nom_materias_total))
  mixmdl_D <- normalmixEM(wait_alumnos,mixmdl_1_D$mu)#Modelo final
  
  # wait_alumnos_final <- wait_alumnos
  # save(wait_alumnos_final,file = "wait_alumnos_final.RData")
  # hist(wait_alumnos_final,freq = F,breaks = seq(6,22,by = 1),)
  # lines(density(rnorm(1000,mean = mixmdl_D$mu,sd = mixmdl_D$sigma)),
  #       lty=1,lwd=2,col = "blue")
  # lines(density(wait_alumnos_final), lty=1,lwd=2,col = "green")
  # legend(15,0.12,c("GMM","density()"),bty = "n",
  #        col=c("blue","green"),lty=c(1,1),
  #        cex=1.1,lwd=2)
  
  for(c in 1:length(param$vec_nom_materias_total)){
    num_alum_1_materia <- prom_alum_x_materia[c]
    (rand_num <- sort(round(rnorm(num_alum_1_materia,mixmdl_D$mu,mixmdl_D$sigma))))
    ind_7 <- which(rand_num < 8)
    ind_22 <- which(rand_num >= 21)
    
    if(length(ind_7) > 0){
      rand_num[ind_7] <- 7
    }
    if(length(ind_22) > 0){
      rand_num[ind_22] <- 21
    }
    
    for(r in 1:length(param$Horas)){
      ind_hrs <- which(rand_num == param$Horas[r])
      if(length(ind_hrs) > 0){
        D_final[r,c] <- length(ind_hrs)
      }
    }
  }#Fin for(c)
  rownames(D_final) <- param$Horas
  colnames(D_final) <- param$vec_nom_materias_total
  
  ### Metodología B
  lista_esq_D_prima <- metodo_B2(n_rep,D_final,param,param_sim)
  
  cat("\nLa función metodo_C tardó: ",(proc.time()-ptm)[3]/60," minutos\n")
  return(lista_esq_D_prima)
}

# Ej. ---------------------------------------------------------------------
n_rep <- 5
lista_esq_D_prima <- metodo_C(n_rep,param,param_sim)

# Gráfica de metodología C ------------------------------------------------
n_rep <- 5
n_calif <- 10
mat_calif_C <- matrix(0,nrow = n_calif,ncol = dim(D)[2])
# D <- gen_mat_demanda_alumnos(param,param_sim)#48.03/45.6 seg
# View(D)
ind_materias <- 1:dim(D0)[2]
ptm <- proc.time()# Start the clock!
for(d in 1:n_calif){
  cat("\n***RENGLÓN ",d," ***")
  lista_esq_D_prima <- metodo_C(n_rep,param,param_sim)
  D_prima <- lista_esq_D_prima[[2]]
  mat_calif_x_gpo <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])
  calif_D <- actualiza_calif_D(D0,D_prima,mat_calif_x_gpo,ind_materias)
  vec_calif_x_materia <- calif_D[[2]]
  
  mat_calif_C[d,] <- vec_calif_x_materia
}
cat("\nEl ciclo tardó: ",(proc.time()-ptm)[3]/60," minutos\n")
## 81.89567 min d = 1:10
## 51.45683 min d = 1:10
## 27.91067 min d = 1:4
## 54.64 min d = 1:10
## 58.26 min d = 1:10

colnames(mat_calif_C) <- param$vec_nom_materias_total
View(mat_calif_C)

matplot(mat_calif_C, type = "l",main = "Metodología C",xlab = "Iteraciones",
        ylab = "Calificación",ylim = c(-0.5,1))

save(mat_calif_C,file = "mat_calif_C.RData")






# Metodología D -----------------------------------------------------------

#' Title actualiza_D_prima_metodo_D: Función encargada de actualizar
#' D_prima para la metodología D.
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

#' Title metodo_C2: Función que arroja la matriz "D_final" a la cual se le
#' aplicó la mezcla de normales por número de alumnos. Para el número de 
#' alumnos se genera un modelo inicial con k = 4.
#' Valor elegido al ver el histograma de los datos del número de alumnos. Para
#' el esqueleto se aplica la metodología B.
#'
#' @param n_rep: Número de veces que se generarán los esqueletos para
#' obtener información.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @param param_sim: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se encargan de la simulación.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' @example param_sim <- list(vec_sem_sig = c(20191,20192,20201),k_sem_ant = 5,
#' materia = "Estadística III", num_sim = 10, m_filtrada = matrix(0),
#' sub_m_filtrada = matrix(0,ncol = length(param$nom_cols_MG)))
#'
#' @return D_final: Matriz de 15 renglones (horas) y 203 columnas
#' (materias). En la entrada (i,j) se tiene el número de alumnos simulados
#' para la hora i, y la materia j.
#'
#' @examples
#' D_final <- metodo_C2(n_rep,param,param_sim)
#' 
metodo_C2 <- function(n_rep,param,param_sim){
  ptm <- proc.time()# Start the clock!
  ### Obtener D0 y D_inicial
  D0 <- gen_mat_demanda_alumnos(param,param_sim)#42.96 seg
  D_inicial <- gen_mat_demanda_alumnos(param,param_sim)#42.96 seg
  ind_materias <- 1:dim(D0)[2]
  cota <- 100*n_rep
  
  #' Definimos las listas en las que vamos a guardar el número de alumnos
  #' por materia
  num_alum_x_materia <- list()
  num_alum_x_materia[[1]] <- colSums(D_inicial)
  
  ##Convertimos los datos para obtener la distribución por horas
  wait_alumnos <- 0
  Horas <- param$Horas
  for(h in 1:length(Horas)){
    suma_x_hra <- sum(D[h,])
    if(suma_x_hra > 0){
      wait_alumnos <- c(wait_alumnos,rep(Horas[h],suma_x_hra))
    }
  }
  #Quitamos el cero inicial
  wait_alumnos <- wait_alumnos[-1]
  
  #' Definimos la lista en las que vamos a guardar el número de alumnos
  #' por materia
  num_alum_x_materia <- list()
  num_alum_x_materia[[1]] <- colSums(D)
  mixmdl_1_D <- normalmixEM(wait_alumnos,k = 4)#Modelo inicial
  
  #Hacemos "n_rep" veces el proceso
  for(d in 2:n_rep){
    cat("d = ",d)
    cont <- 1
    ### Obtener D
    D <- gen_mat_demanda_alumnos(param,param_sim)
    ### Obtener D' para generar esqueleto
    mat_calif_x_gpo <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])
    calif_D <- actualiza_calif_D(D0,D,mat_calif_x_gpo,ind_materias)
    mat_calif_x_gpo <- calif_D[[1]]
    vec_calif_x_materia <- calif_D[[2]]
    #' Actualizo mientras se cumplan las siguientes condiciones:
    while(any(vec_calif_x_materia< -20) || any(vec_calif_x_materia> 10)){
      ind_1 <- which(vec_calif_x_materia< -20)
      ind_2 <- which(vec_calif_x_materia> 10)
      ind_materias <- union(ind_1,ind_2)
      if(length(ind_materias) > 0){
        D <- actualiza_D_prima_metodo_D(cota,D0,D,mixmdl_1_D,
                                        calif_D,ind_materias)
        calif_D <- actualiza_calif_D(D0,D,mat_calif_x_gpo,
                                     ind_materias)
        mat_calif_x_gpo <- calif_D[[1]]
        vec_calif_x_materia <- calif_D[[2]]
      }else{
        cat("\n Todas las calificaciones están dentro del intervalo [-20,10]")
        break;
      }
      if(cont >= cota){
        break;
      }
      cont <- cont + 1
    }#Fin while()
    
    num_alum_x_materia[[d]] <- colSums(D)
    ##Convertimos los datos para obtener la distribución por horas
    for(h in 1:length(Horas)){
      suma_x_hra <- sum(D[h,])
      if(suma_x_hra > 0){
        wait_alumnos <- c(wait_alumnos,rep(Horas[h],suma_x_hra))
      }
    }
  }#2.7805 min
  
  ### Obtenemos el número promedio de grupos por materia
  mat_alum_x_materia <- matrix(0,nrow = n_rep,
                               ncol = length(param$vec_nom_materias_total))
  
  for(r in 1:n_rep){#Recorre las listas
    mat_alum_x_materia[r,] <- num_alum_x_materia[[r]]
  }
  prom_alum_x_materia <- ceiling(colMeans(mat_alum_x_materia))
  
  #Generamos la matriz D final
  D_final <- matrix(0,nrow = length(param$Horas),
                    ncol = length(param$vec_nom_materias_total))
  mixmdl_D <- normalmixEM(wait_alumnos,mixmdl_1_D$mu)#Modelo final
  
  for(c in 1:length(param$vec_nom_materias_total)){
    num_alum_1_materia <- prom_alum_x_materia[c]
    (rand_num <- sort(round(rnorm(num_alum_1_materia,mixmdl_D$mu,mixmdl_D$sigma))))
    ind_7 <- which(rand_num < 8)
    ind_22 <- which(rand_num >= 21)
    
    if(length(ind_7) > 0){
      rand_num[ind_7] <- 7
    }
    if(length(ind_22) > 0){
      rand_num[ind_22] <- 21
    }
    
    for(r in 1:length(param$Horas)){
      ind_hrs <- which(rand_num == param$Horas[r])
      if(length(ind_hrs) > 0){
        D_final[r,c] <- length(ind_hrs)
      }
    }
  }#Fin for(c)
  rownames(D_final) <- param$Horas
  colnames(D_final) <- param$vec_nom_materias_total
  
  cat("\nLa función metodo_C2 tardó: ",(proc.time()-ptm)[3]/60," minutos\n")
  return(D_final)
}



#' Title metodo_D: Función que genera un esqueleto con la metodología D, la
#' cual implementa la mezcla de normales por número de alumnos. Se combina
#' el inicio de la metodología C con la función "gen_esqueleto".
#'
#' @param n_rep: Número de veces que se generarán los esqueletos para
#' obtener información.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @param param_sim: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se encargan de la simulación.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' @example param_sim <- list(vec_sem_sig = c(20191,20192,20201),k_sem_ant = 5,
#' materia = "Estadística III", num_sim = 10, m_filtrada = matrix(0),
#' sub_m_filtrada = matrix(0,ncol = length(param$nom_cols_MG)))
#'
#' @return lista_esq_D_prima: Lista que contiene 2 matrices, la matriz
#' "mat_esqueleto" con el esqueleto generado y la matriz "D_prima" la
#' cual contiene el primedio de los esqueletos generados "n_rep" veces.
#'
#' @examples
#' lista_esq_D_prima <- metodo_D(n_rep,param,param_sim)
#' 
metodo_D <- function(n_rep,param,param_sim){
  ptm <- proc.time()# Start the clock!
  #Definimos D_inicial
  D_inicial <- metodo_C2(n_rep,param,param_sim)
  
  ##Generar esqueleto
  mat_solicitudes <- gen_solicitudes(param)#7.97 seg
  lista_info_esqueleto <- gen_esqueleto(D_inicial,mat_solicitudes,param)#10.76 seg
  mat_esqueleto <- lista_info_esqueleto[[1]]
  
  lista_esq_D_prima <- list()
  lista_esq_D_prima[[1]] <- mat_esqueleto
  lista_esq_D_prima[[2]] <- D_inicial
  
  cat("\nLa función metodo_D tardó: ",(proc.time()-ptm)[3]/60," minutos\n")
  return(lista_esq_D_prima)
}

# Ej. ---------------------------------------------------------------------

lista_esq_D_prima <- metodo_D(n_rep,param,param_sim)

# Gráfica de metodología D ------------------------------------------------
n_rep <- 5
n_calif <- 10
mat_calif_D <- matrix(0,nrow = n_calif,ncol = dim(D)[2])
# D <- gen_mat_demanda_alumnos(param,param_sim)#41.03 seg
# View(D)
ind_materias <- 1:dim(D0)[2]
ptm <- proc.time()# Start the clock!
for(d in 1:n_calif){
  cat("\n***RENGLÓN ",d," ***")
  lista_esq_D_prima <- metodo_D(n_rep,param,param_sim)
  D_prima <- lista_esq_D_prima[[2]]
  mat_calif_x_gpo <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])
  calif_D <- actualiza_calif_D(D0,D_prima,mat_calif_x_gpo,ind_materias)
  vec_calif_x_materia <- calif_D[[2]]
  
  mat_calif_D[d,] <- vec_calif_x_materia
}
cat("\nEl ciclo tardó: ",(proc.time()-ptm)[3]/60," minutos\n")
# 7.831 min d = 1
# 41.90367 min d = 2:10
# 49.19217 min d = 1:10
# 49.35 min d = 1:10

colnames(mat_calif_D) <- param$vec_nom_materias_total
View(mat_calif_D)

matplot(mat_calif_D, type = "l",main = "Metodología D",
        xlab = "Iteraciones",
        ylab = "Calificación")

save(mat_calif_D,file = "mat_calif_D.RData")








# Generamos D0 para calificar los métodos ---------------------------------
n_rep <- 5
D <- gen_mat_demanda_alumnos(param,param_sim)#41.03 seg
D_prima <- metodo_C2(n_rep,param,param_sim)
Y <- cbind(D,D_prima)
Y <- array(Y, dim=c(dim(D), 2))#2 = son 2 matrices (a y b)

D0 <- ceiling(apply(Y, c(1, 2), mean, na.rm = TRUE))

# a <- matrix(3,3,3)
# b <- matrix(c(1,2,3,4,5,6,7,8,9),nrow = 3,ncol = 3,byrow = T)
# Y <- cbind(a,b)
# Y <- array(Y, dim=c(dim(a), 2))#2 = son 2 matrices (a y b)
# promedio <- ceiling(apply(Y, c(1, 2), mean, na.rm = TRUE))




# grafica_calificaciones --------------------------------------------------
# grafica_calificaciones <- function(D0,n_rep,n_calif,lista_mat_calif){
#   ind_materias <- 1:dim(D0)[2]
#   for(m in 2:4){
#     ptm <- proc.time()# Start the clock!
#     cat("\n*** METODOLOGÍA ",m," ***")
#     mat_calif <- matrix(0,nrow = n_calif,ncol = dim(D)[2])
#     for(d in 1:n_calif){
#       cat("\n***RENGLÓN ",d," ***")
#       switch(m,
#              '2' = {lista_esq_D_prima <- metodo_B(n_rep,param,param_sim)},
#              '3' = {lista_esq_D_prima <- metodo_C(n_rep,param,param_sim)},
#              '4' = {lista_esq_D_prima <- metodo_D(n_rep,param,param_sim)}
#       )
#       D_prima <- lista_esq_D_prima[[2]]
#       mat_calif_x_gpo <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])
#       calif_D <- actualiza_calif_D(D0,D_prima,mat_calif_x_gpo,ind_materias)
#       vec_calif_x_materia <- calif_D[[2]]
#       mat_calif[d,] <- vec_calif_x_materia
#     }
#     colnames(mat_calif) <- param$vec_nom_materias_total
#     lista_mat_calif[[m]] <- mat_calif
#     cat("\nEl ciclo para el método ",m,
#         " tardó: ",(proc.time()-ptm)[3]/60," minutos\n")
#   }#Fin for(m)
#   
#   return(lista_mat_calif)
# }
# 
# mat_calif_A <- lista_mat_calif[[1]]
# mat_calif_B <- lista_mat_calif[[2]]
# mat_calif_C <- lista_mat_calif[[3]]
# mat_calif_D <- lista_mat_calif[[4]]
# 
# save(mat_calif_A,file = "mat_calif_A.RData")
# save(mat_calif_B,file = "mat_calif_B.RData")
# save(mat_calif_C,file = "mat_calif_C.RData")
# save(mat_calif_D,file = "mat_calif_D.RData")


# Ej. ---------------------------------------------------------------------
n_rep <- 5
n_calif <- 10

matplot(mat_calif, type = "l",main = "Metodología D",xlab = "Iteraciones",
        ylab = "Calificación")

lista_mat_calif <- list()
lista_mat_calif[[1]] <- mat_calif_A

# Gráficas a escala -------------------------------------------------------
# matplot(mat_calif_A, type = "l",main = "Metodología A",xlab = "Iteraciones",
#         ylab = "Calificación")
# 
# matplot(mat_calif_B, type = "l",main = "Metodología B",xlab = "Iteraciones",
#         ylab = "Calificación")
# 
# matplot(mat_calif_C, type = "l",main = "Metodología C",xlab = "Iteraciones",
#         ylab = "Calificación")
# 
# matplot(mat_calif_D, type = "l",main = "Metodología D",xlab = "Iteraciones",
#         ylab = "Calificación")



matplot(mat_calif_A, type = "l",main = "Metodología A",xlab = "Iteraciones",
        ylab = "Calificación",ylim = c(-5,1))

matplot(mat_calif_B, type = "l",main = "Metodología B",xlab = "Iteraciones",
        ylab = "Calificación",ylim = c(-5,1))

matplot(mat_calif_C, type = "l",main = "Metodología C",xlab = "Iteraciones",
        ylab = "Calificación",ylim = c(-5,1))

matplot(mat_calif_D, type = "l",main = "Metodología D",xlab = "Iteraciones",
        ylab = "Calificación",ylim = c(-5,1))



# heatmaps ----------------------------------------------------------------
colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
# par(mfrow=c(2,2),cex=1) # set the plotting area into a 2*2 array
heatmap(mat_calif_A, Colv = NA, Rowv = NA, scale="none",col=colMain,
        main = "Métodología A")
legend(x="bottomright", legend=c(paste0("mín = ",round(min(mat_calif_A),2)),
                                 paste0("media = ",round(mean(mat_calif_A),2)),
                                 paste0("máx = ",round(max(mat_calif_A),2))), 
       fill=colorRampPalette(brewer.pal(8, "Blues"))(3))

heatmap(mat_calif_B, Colv = NA, Rowv = NA, scale="none",col=colMain,
        main = "Métodología B")
legend(x="bottomright", legend=c(paste0("mín = ",round(min(mat_calif_B),2)),
                                 paste0("media = ",round(mean(mat_calif_B),2)),
                                 paste0("máx = ",round(max(mat_calif_B),2))),
       fill=colorRampPalette(brewer.pal(8, "Blues"))(3))

heatmap(mat_calif_C, Colv = NA, Rowv = NA, scale="none",col=colMain,
        main = "Métodología C")
legend(x="bottomright", legend=c(paste0("mín = ",round(min(mat_calif_C),2)),
                                 paste0("media = ",round(mean(mat_calif_C),2)),
                                 paste0("máx = ",round(max(mat_calif_C),2))), 
       fill=colorRampPalette(brewer.pal(8, "Blues"))(3))

heatmap(mat_calif_D, Colv = NA, Rowv = NA, scale="none",col=colMain,
        main = "Métodología D")
legend(x="bottomright", legend=c(paste0("mín = ",round(min(mat_calif_D),2)),
                                 paste0("media = ",round(mean(mat_calif_D),2)),
                                 paste0("máx = ",round(max(mat_calif_D),2))), 
       fill=colorRampPalette(brewer.pal(8, "Blues"))(3))

# dev.off()#Para salir de la función par()

# Esqueletos --------------------------------------------------------------
cota <- 1000
lista_esq_D_prima_A <- metodo_A(cota,param,param_sim)#1.9 min
mat_esqueleto_A <- lista_esq_D_prima_A[[1]]
D_prima_A <- lista_esq_D_prima_A[[2]]
View(mat_esqueleto_A)
View(D_prima_A)


n_rep <- 5
lista_esq_D_prima_B <- metodo_B(n_rep,param,param_sim)#5.32/5.13 min
mat_esqueleto_B <- lista_esq_D_prima_B[[1]]
D_prima_B <- lista_esq_D_prima_B[[2]]
View(mat_esqueleto_B)
View(D_prima_B)


n_rep <- 5
lista_esq_D_prima_C <- metodo_C(n_rep,param,param_sim)#5.73 min
mat_esqueleto_C <- lista_esq_D_prima_C[[1]]
D_prima_C <- lista_esq_D_prima_C[[2]]
View(mat_esqueleto_C)
View(D_prima_C)


n_rep <- 5
lista_esq_D_prima_D <- metodo_D(n_rep,param,param_sim)#4.74 min
mat_esqueleto_D <- lista_esq_D_prima_D[[1]]
D_prima_D <- lista_esq_D_prima_D[[2]]
View(mat_esqueleto_D)
View(D_prima_D)









############################################################################
##### METODOLOGÍAS #####
#' Funciones que simulan un esqueleto. Se probaron 4 metodologías distintas.
############################################################################

# Metodología A -----------------------------------------------------------
#' Title metodo_A: Función que genera un esqueleto con la metodología A, la
#' cual implementa la mezcla de normales por materia y modifica el número de
#' alumnos en D si la calificación por materia está fuera de [-20,10] y 
#' si la calificación por grupo está fuera de [-10,10].
#' En esta metodología en la función "gen_normalmixEM_inicial" el modelo
#' se genera con el promedio de los datos, no se utiliza el parámatro k
#' que es el número de normales que tiene el modelo. El esqueleto se simula
#' con la función "gen_esqueleto" utilizando la matriz D' actualizada.
#'
#' @param cota: Cota para que el ciclo no sea infinito.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @param param_sim: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se encargan de la simulación.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' @example param_sim <- list(vec_sem_sig = c(20191,20192,20201),k_sem_ant = 5,
#' materia = "Estadística III", num_sim = 10, m_filtrada = matrix(0),
#' sub_m_filtrada = matrix(0,ncol = length(param$nom_cols_MG)))
#'
#' @return lista_esq_D_prima: Lista que contiene 2 matrices, la matriz
#' "mat_esqueleto" con el esqueleto generado y la matriz "D_prima" la
#' cual contiene la matriz de demanda de alumnos con la que se generó el
#' esqueleto.
#'
#' @examples
#' lista_esq_D_prima <- metodo_A(cota,param,param_sim)
#' 
metodo_A <- function(cota,param,param_sim){
  ptm <- proc.time()# Start the clock!
  #' Simulamos D0 (matriz de demanda con la que vamos a calificar a la
  #' matriz D').
  D0 <- gen_mat_demanda_alumnos(param,param_sim)#45.94 seg
  
  ### Obtener D'0 = E
  mat_solicitudes <- gen_solicitudes(param)#7.21 seg
  lista_info_esqueleto <- gen_esqueleto(D0,mat_solicitudes,param)#13.58 seg
  E <- lista_info_esqueleto[[8]]#Matriz con el número de alumnos simulados
  
  ### Aplicar mezcla de normales inicial
  vec_s_sem_k_info <- gen_vec_s_sem_k_info(param_sim$vec_sem_sig,
                                           param_sim$k_sem_ant,param)
  lista_mod_y_wait <- gen_normalmixEM_inicial(vec_s_sem_k_info,E,
                                              param,param_sim)
  
  ### Obtener D' para generar esqueleto
  D_prima <-  gen_D_prima(D0,E,lista_mod_y_wait,cota)
  
  ##Calificamos D_prima
  # mat_calif_x_gpo <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])
  # ind_materias <- 1:dim(D)[2]
  # calif_D <- actualiza_calif_D(D0,D_prima,mat_calif_x_gpo,ind_materias)
  # vec_calif_x_materia <- calif_D[[2]]
  # View(vec_calif_x_materia)
  
  ##Generar esqueleto
  mat_solicitudes <- gen_solicitudes(param)#8.94 seg
  mat_esqueleto <- gen_esqueleto(D_prima,mat_solicitudes,param)#14.05 seg
  rownames(mat_esqueleto) <- param$Horas
  colnames(mat_esqueleto) <- param$vec_nom_materias_total
  
  
  lista_esq_D_prima <- list()
  lista_esq_D_prima[[1]] <- mat_esqueleto
  lista_esq_D_prima[[2]] <- D_prima
  
  cat("\nLa metodología A tardó: ",(proc.time()-ptm)[3]/60,
      " minutos\n")
  return(lista_esq_D_prima)
}


# Metodología B -----------------------------------------------------------
#' Title metodo_B: Función que genera un esqueleto con la metodología B, la
#' cual implementa la mezcla de normales por esqueleto. Se genera un modelo
#' inicial con k = 3. Valor elegido al ver el histograma de los datos
#' en un esqueleto. En el modelo final se utiliza la media del modelo inical
#' como parámetro.
#'
#' @param n_rep: Número de veces que se generarán los esqueletos para
#' obtener información.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @param param_sim: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se encargan de la simulación.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' @example param_sim <- list(vec_sem_sig = c(20191,20192,20201),k_sem_ant = 5,
#' materia = "Estadística III", num_sim = 10, m_filtrada = matrix(0),
#' sub_m_filtrada = matrix(0,ncol = length(param$nom_cols_MG)))
#'
#' @return lista_esq_D_prima: Lista que contiene 2 matrices, la matriz
#' "mat_esqueleto" con el esqueleto generado y la matriz "D_prima" la
#' cual contiene el primedio de los esqueletos generados "n_rep" veces.
#'
#' @examples
#' lista_esq_D_prima <- metodo_B(n_rep,param,param_sim)
#' 
metodo_B <- function(n_rep,param,param_sim){
  ptm <- proc.time()# Start the clock!
  ### Obtener D_prima
  D_prima_inicial <- gen_mat_demanda_alumnos(param,param_sim)#46.41 seg
  prom_D <- D_prima_inicial
  
  ##Generar esqueleto inicial
  mat_solicitudes <- gen_solicitudes(param)#8.07 seg
  mat_esqueleto <- gen_esqueleto(D_prima_inicial,mat_solicitudes,param)#13.35 seg
  
  ##Convertimos los datos para obtener la distribución por horas
  wait_mat_esqueleto <- 0
  Horas <- param$Horas
  for(h in 1:length(Horas)){
    # cat("\n h = ",h)
    suma_x_hra <- sum(mat_esqueleto[h,])
    # cat("\n suma_x_hra = ",suma_x_hra)
    if(suma_x_hra > 0){
      wait_mat_esqueleto <- c(wait_mat_esqueleto,rep(Horas[h],suma_x_hra))
    }
    # cat("\n wait_mat_esqueleto = ",wait_mat_esqueleto)
  }
  #Quitamos el cero inicial
  wait_mat_esqueleto <- wait_mat_esqueleto[-1]
  
  #' Definimos las listas en las que vamos a guardar el número de grupos por materia
  #' y los modelos de mezcla de normales para cada esqueleto
  mat_gpos_x_materia <- matrix(0,nrow = n_rep,ncol = dim(mat_esqueleto)[2])
  mat_gpos_x_materia[1,] <- colSums(mat_esqueleto)
  mixmdl_1_esqueleto <- normalmixEM(wait_mat_esqueleto,k = 4)
  
  # wait_mat_esqueleto_inicial <- wait_mat_esqueleto
  # hist(wait_mat_esqueleto_inicial,freq = F,
  #      breaks = seq(5,22,by = 1),
  #      main = "Histograma de grupos en un esqueleto",
  #      xlab = "Horas",ylab = "Frecuencia relativa",
  #      ylim = c(0,0.15))
  # lines(density(rnorm(1000,mean = mixmdl_1_esqueleto$mu,
  #                     sd = mixmdl_1_esqueleto$sigma)),
  #       lty=1,lwd=2,col = "blue")
  # lines(density(wait_mat_esqueleto_inicial), lty=1,lwd=2,col = "green")
  # legend(15,0.15,c("GMM","density()"),bty = "n",
  #        col=c("blue","green"),lty=c(1,1),
  #        cex=1.1,lwd=2)
  
  #Hacemos "n_rep" veces el proceso
  for(d in 2:n_rep){
    cat("d = ",d)
    ### Obtener D
    D <- gen_mat_demanda_alumnos(param,param_sim)
    prom_D <- prom_D + D
    
    ##Generar esqueleto
    mat_solicitudes <- gen_solicitudes(param)
    lista_info_esqueleto <- gen_esqueleto(D,mat_solicitudes,param)
    mat_esqueleto <- lista_info_esqueleto[[1]]
    mat_gpos_x_materia[d,] <- colSums(mat_esqueleto)
    # lista_de_lista_info_esqueleto[[d]] <- lista_info_esqueleto
    
    ##Convertimos los datos para obtener la distribución por horas
    for(h in 1:length(Horas)){
      suma_x_hra <- sum(mat_esqueleto[h,])
      if(suma_x_hra > 0){
        wait_mat_esqueleto <- c(wait_mat_esqueleto,rep(Horas[h],suma_x_hra))
      }
    }
  }#Fin for(d)
  mixmdl_esqueleto <- normalmixEM(wait_mat_esqueleto,
                                  k=length(mixmdl_1_esqueleto$mu),
                                  mean=mixmdl_1_esqueleto$mu)
  prom_gpos_x_materia <- ceiling(colMeans(mat_gpos_x_materia))
  
  # wait_mat_esqueleto_final <- wait_mat_esqueleto
  # hist(wait_mat_esqueleto_final,freq = F,
  #      breaks = seq(5,22,by = 1),
  #      main = "Histograma de grupos en un esqueleto",
  #      xlab = "Horas",ylab = "Frecuencia relativa",
  #      ylim = c(0,0.15))
  # lines(density(rnorm(1000,mean = mixmdl_esqueleto$mu,
  #                     sd = mixmdl_esqueleto$sigma)),
  #       lty=1,lwd=2,col = "blue")
  # legend(5,0.17,
  #        c("Densidad ajustada por modelo de mezcla de normales"),
  #        bty = "n",
  #        col=c("blue"),lty=1,
  #        cex=1.1,lwd=2)
  # lines(density(wait_mat_esqueleto_final), lty=1,lwd=2,col = "green")
  # legend(15,0.15,c("GMM","density()"),bty = "n",
  #        col=c("blue","green"),lty=c(1,1),
  #        cex=1.1,lwd=2)
  
  #Generamos el esqueleto final
  mat_esqueleto_final <- matrix(0,nrow = length(param$Horas),
                                ncol = length(param$vec_nom_materias_total))
  for(c in 1:length(param$vec_nom_materias_total)){
    num_gpos_1_materia <- prom_gpos_x_materia[c]
    (rand_num <- sort(round(rnorm(num_gpos_1_materia,mixmdl_esqueleto$mu,
                                  mixmdl_esqueleto$sigma))))
    ind_7 <- which(rand_num < 8)
    ind_22 <- which(rand_num >= 21)
    
    if(length(ind_7) > 0){
      rand_num[ind_7] <- 7
    }
    if(length(ind_22) > 0){
      rand_num[ind_22] <- 21
    }
    
    for(r in 1:length(param$Horas)){
      ind_hrs <- which(rand_num == param$Horas[r])
      if(length(ind_hrs) > 0){
        mat_esqueleto_final[r,c] <- length(ind_hrs)
      }
    }
  }#Fin for(c)
  # View(mat_esqueleto_final)
  rownames(mat_esqueleto_final) <- param$Horas
  colnames(mat_esqueleto_final) <- param$vec_nom_materias_total
  
  lista_esq_D_prima <- list()
  lista_esq_D_prima[[1]] <- mat_esqueleto_final
  lista_esq_D_prima[[2]] <- ceiling(prom_D/n_rep)
  cat("\nLa función metodo_B tardó: ",(proc.time()-ptm)[3]/60," minutos\n")
  return(lista_esq_D_prima)
}


# Metodología C -----------------------------------------------------------

#' Title metodo_B2: Función que genera un esqueleto con la metodología B2, la
#' cual implementa la mezcla de normales por esqueleto. Se genera un modelo
#' inicial con k = 3. Valor elegido al ver el histograma de los datos
#' en un esqueleto. En el modelo final se utiliza la media del modelo inical
#' como parámetro.
#'
#' @param n_rep: Número de veces que se generarán los esqueletos para
#' obtener información.
#' @param D_prima_inicial: Matriz de 15 renglones (horas) y 203 columnas
#' (materias). En la entrada (i,j) se tiene el número de alumnos simulados
#' para la hora i, y la materia j.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @param param_sim: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se encargan de la simulación.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' @example param_sim <- list(vec_sem_sig = c(20191,20192,20201),k_sem_ant = 5,
#' materia = "Estadística III", num_sim = 10, m_filtrada = matrix(0),
#' sub_m_filtrada = matrix(0,ncol = length(param$nom_cols_MG)))
#'
#' @return lista_esq_D_prima: Lista que contiene 2 matrices, la matriz
#' "mat_esqueleto" con el esqueleto generado y la matriz "D_prima" la
#' cual contiene el primedio de los esqueletos generados "n_rep" veces.
#'
#' @examples
#' lista_esq_D_prima <- metodo_B2(n_rep,D_prima_inicial,param,param_sim)
#' 
metodo_B2 <- function(n_rep,D_prima_inicial,param,param_sim){
  ptm <- proc.time()# Start the clock!
  ##Generar esqueleto inicial
  prom_D <- D_prima_inicial
  mat_solicitudes <- gen_solicitudes(param)#8.07 seg
  lista_info_esqueleto <- gen_esqueleto(D_prima_inicial,mat_solicitudes,param)#13.35 seg
  mat_esqueleto <- lista_info_esqueleto[[1]]
  
  ##Convertimos los datos para obtener la distribución por horas
  wait_mat_esqueleto <- 0
  Horas <- param$Horas
  for(h in 1:length(Horas)){
    suma_x_hra <- sum(mat_esqueleto[h,])
    if(suma_x_hra > 0){
      wait_mat_esqueleto <- c(wait_mat_esqueleto,rep(Horas[h],suma_x_hra))
    }
  }
  #Quitamos el cero inicial
  wait_mat_esqueleto <- wait_mat_esqueleto[-1]
  
  #' Definimos las listas en las que vamos a guardar el número de grupos por materia
  #' y los modelos de mezcla de normales para cada esqueleto
  mat_gpos_x_materia <- matrix(0,nrow = n_rep,ncol = dim(mat_esqueleto)[2])
  mat_gpos_x_materia[1,] <- colSums(mat_esqueleto)
  mixmdl_1_esqueleto <- normalmixEM(wait_mat_esqueleto,k = 4)
  
  #Hacemos "n_rep" veces el proceso
  for(d in 2:n_rep){
    cat("d = ",d)
    ### Obtener D
    # D <- gen_mat_demanda_alumnos(param,param_sim)
    # prom_D <- prom_D + D
    ##Generar esqueleto
    mat_solicitudes <- gen_solicitudes(param)
    lista_info_esqueleto <- gen_esqueleto(D_prima_inicial,
                                          mat_solicitudes,param)
    mat_esqueleto <- lista_info_esqueleto[[1]]
    mat_gpos_x_materia[d,] <- colSums(mat_esqueleto)
    
    ##Convertimos los datos para obtener la distribución por horas
    for(h in 1:length(Horas)){
      suma_x_hra <- sum(mat_esqueleto[h,])
      if(suma_x_hra > 0){
        wait_mat_esqueleto <- c(wait_mat_esqueleto,rep(Horas[h],suma_x_hra))
      }
    }
  }#8.963333 min
  mixmdl_esqueleto <- normalmixEM(wait_mat_esqueleto,
                                  k = length(mixmdl_1_esqueleto$mu),
                                  mean=mixmdl_1_esqueleto$mu)
  
  prom_gpos_x_materia <- ceiling(colMeans(mat_gpos_x_materia))
  
  #Generamos el esqueleto final
  mat_esqueleto_final <- matrix(0,nrow = length(param$Horas),
                                ncol = length(param$vec_nom_materias_total))
  for(c in 1:length(param$vec_nom_materias_total)){
    num_gpos_1_materia <- prom_gpos_x_materia[c]
    (rand_num <- sort(round(rnorm(num_gpos_1_materia,mixmdl_esqueleto$mu,
                                  mixmdl_esqueleto$sigma))))
    ind_7 <- which(rand_num < 8)
    ind_22 <- which(rand_num >= 21)
    
    if(length(ind_7) > 0){
      rand_num[ind_7] <- 7
    }
    if(length(ind_22) > 0){
      rand_num[ind_22] <- 21
    }
    
    for(r in 1:length(param$Horas)){
      ind_hrs <- which(rand_num == param$Horas[r])
      if(length(ind_hrs) > 0){
        mat_esqueleto_final[r,c] <- length(ind_hrs)
      }
    }
  }#Fin for(c)
  # View(mat_esqueleto_final)
  rownames(mat_esqueleto_final) <- param$Horas
  colnames(mat_esqueleto_final) <- param$vec_nom_materias_total
  
  lista_esq_D_prima <- list()
  lista_esq_D_prima[[1]] <- mat_esqueleto_final
  lista_esq_D_prima[[2]] <- D_prima_inicial
  # lista_esq_D_prima[[2]] <- ceiling(prom_D/n_rep)
  cat("\nLa función metodo_B2 tardó: ",(proc.time()-ptm)[3]/60," minutos\n")
  return(lista_esq_D_prima)
}


#' Title metodo_C: Función que genera un esqueleto con la metodología C, la
#' cual implementa la mezcla de normales por número de alumnos y por
#' esqueleto. Para el número de alumnos se genera un modelo inicial con k = 4.
#' Valor elegido al ver el histograma de los datos del número de alumnos. Para
#' el esqueleto se aplica la metodología B.
#'
#' @param n_rep: Número de veces que se generarán los esqueletos para
#' obtener información.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @param param_sim: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se encargan de la simulación.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' @example param_sim <- list(vec_sem_sig = c(20191,20192,20201),k_sem_ant = 5,
#' materia = "Estadística III", num_sim = 10, m_filtrada = matrix(0),
#' sub_m_filtrada = matrix(0,ncol = length(param$nom_cols_MG)))
#'
#' @return lista_esq_D_prima: Lista que contiene 2 matrices, la matriz
#' "mat_esqueleto" con el esqueleto generado y la matriz "D_prima" la
#' cual contiene el primedio de los esqueletos generados "n_rep" veces.
#'
#' @examples
#' lista_esq_D_prima <- metodo_C(n_rep,param,param_sim)
#' 
metodo_C <- function(n_rep,param,param_sim){
  ptm <- proc.time()# Start the clock!
  ### Obtener D
  D_inicial <- gen_mat_demanda_alumnos(param,param_sim)#42.96 seg
  
  #' Definimos las listas en las que vamos a guardar el número de alumnos
  #' por materia
  num_alum_x_materia <- list()
  num_alum_x_materia[[1]] <- colSums(D_inicial)
  
  ##Convertimos los datos para obtener la distribución por horas
  wait_alumnos <- 0
  Horas <- param$Horas
  for(h in 1:length(Horas)){
    suma_x_hra <- sum(D[h,])
    if(suma_x_hra > 0){
      wait_alumnos <- c(wait_alumnos,rep(Horas[h],suma_x_hra))
    }
  }
  #Quitamos el cero inicial
  wait_alumnos <- wait_alumnos[-1]
  
  #' Definimos la lista en las que vamos a guardar el número de alumnos
  #' por materia
  num_alum_x_materia <- list()
  num_alum_x_materia[[1]] <- colSums(D)
  mixmdl_1_D <- normalmixEM(wait_alumnos,k = 4)#Modelo inicial
  
  # hist(wait_alumnos,freq = F,breaks = seq(6,22,by = 1),)
  # lines(density(rnorm(1000,mean = mixmdl_1_D$mu,sd = mixmdl_1_D$sigma)),
  #       lty=1,lwd=2,col = "blue")
  # lines(density(wait_alumnos), lty=1,lwd=2,col = "green")
  # legend(15,0.14,c("GMM","density()"),bty = "n",
  #        col=c("blue","green"),lty=c(1,1),
  #        cex=1.1,lwd=2)
  
  #Hacemos "n_rep" veces el proceso
  for(d in 2:n_rep){
    cat("d = ",d)
    ### Obtener D
    D <- gen_mat_demanda_alumnos(param,param_sim)
    num_alum_x_materia[[d]] <- colSums(D)
    ##Convertimos los datos para obtener la distribución por horas
    for(h in 1:length(Horas)){
      suma_x_hra <- sum(D[h,])
      if(suma_x_hra > 0){
        wait_alumnos <- c(wait_alumnos,rep(Horas[h],suma_x_hra))
      }
    }
  }#2.7805 min
  
  ### Obtenemos el número promedio de grupos por materia
  mat_alum_x_materia <- matrix(0,nrow = n_rep,
                               ncol = length(param$vec_nom_materias_total))
  
  for(r in 1:n_rep){#Recorre las listas
    mat_alum_x_materia[r,] <- num_alum_x_materia[[r]]
  }
  prom_alum_x_materia <- ceiling(colMeans(mat_alum_x_materia))
  
  #Generamos la matriz D final
  D_final <- matrix(0,nrow = length(param$Horas),
                    ncol = length(param$vec_nom_materias_total))
  mixmdl_D <- normalmixEM(wait_alumnos,mixmdl_1_D$mu)#Modelo final
  
  # wait_alumnos_final <- wait_alumnos
  # hist(wait_alumnos_final,freq = F,breaks = seq(6,22,by = 1),)
  # lines(density(rnorm(1000,mean = mixmdl_D$mu,sd = mixmdl_D$sigma)),
  #       lty=1,lwd=2,col = "blue")
  # lines(density(wait_alumnos_final), lty=1,lwd=2,col = "green")
  # legend(15,0.12,c("GMM","density()"),bty = "n",
  #        col=c("blue","green"),lty=c(1,1),
  #        cex=1.1,lwd=2)
  
  for(c in 1:length(param$vec_nom_materias_total)){
    num_alum_1_materia <- prom_alum_x_materia[c]
    (rand_num <- sort(round(rnorm(num_alum_1_materia,mixmdl_D$mu,mixmdl_D$sigma))))
    ind_7 <- which(rand_num < 8)
    ind_22 <- which(rand_num >= 21)
    
    if(length(ind_7) > 0){
      rand_num[ind_7] <- 7
    }
    if(length(ind_22) > 0){
      rand_num[ind_22] <- 21
    }
    
    for(r in 1:length(param$Horas)){
      ind_hrs <- which(rand_num == param$Horas[r])
      if(length(ind_hrs) > 0){
        D_final[r,c] <- length(ind_hrs)
      }
    }
  }#Fin for(c)
  rownames(D_final) <- param$Horas
  colnames(D_final) <- param$vec_nom_materias_total
  
  ### Metodología B
  lista_esq_D_prima <- metodo_B2(n_rep,D_final,param,param_sim)
  
  cat("\nLa función metodo_C tardó: ",(proc.time()-ptm)[3]/60," minutos\n")
  return(lista_esq_D_prima)
}


# Metodología D -----------------------------------------------------------

#' Title actualiza_D_prima_metodo_D: Función encargada de actualizar
#' D_prima para la metodología D.
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
    if(sum(vec_calif_x_materia[c])>1 || 
       sum(vec_calif_x_materia[c]) < -1){#Sólo modificamos si
      #' la califición total de la materia está fuera de [-20,10]
      for(h in 1:length(param$Horas)){#Recorre las horas (renglones)
        # cat("\n h = ",h)
        (rand_num <- ceiling(rnorm(1,mixmdl$mu,mixmdl$sigma)))
        if(mat_calif_x_gpo[h,c] > 0.5){#Si faltan alumnos
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
        if(mat_calif_x_gpo[h,c] < -0.5 && D[h,c]>0){#Si sobran alumnos
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

#' Title metodo_C2: Función que arroja la matriz "D_final" a la cual se le
#' aplicó la mezcla de normales por número de alumnos. Para el número de 
#' alumnos se genera un modelo inicial con k = 4.
#' Valor elegido al ver el histograma de los datos del número de alumnos. Para
#' el esqueleto se aplica la metodología B.
#'
#' @param n_rep: Número de veces que se generarán los esqueletos para
#' obtener información.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @param param_sim: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se encargan de la simulación.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' @example param_sim <- list(vec_sem_sig = c(20191,20192,20201),k_sem_ant = 5,
#' materia = "Estadística III", num_sim = 10, m_filtrada = matrix(0),
#' sub_m_filtrada = matrix(0,ncol = length(param$nom_cols_MG)))
#'
#' @return D_final: Matriz de 15 renglones (horas) y 203 columnas
#' (materias). En la entrada (i,j) se tiene el número de alumnos simulados
#' para la hora i, y la materia j.
#'
#' @examples
#' D_final <- metodo_C2(n_rep,param,param_sim)
#' 
metodo_C2 <- function(n_rep,param,param_sim){
  ptm <- proc.time()# Start the clock!
  ### Obtener D0 y D_inicial
  D0 <- gen_mat_demanda_alumnos(param,param_sim)#42.96 seg
  D_inicial <- gen_mat_demanda_alumnos(param,param_sim)#42.96 seg
  ind_materias <- 1:dim(D0)[2]
  cota <- 100*n_rep
  
  #' Definimos las listas en las que vamos a guardar el número de alumnos
  #' por materia
  num_alum_x_materia <- list()
  num_alum_x_materia[[1]] <- colSums(D_inicial)
  
  ##Convertimos los datos para obtener la distribución por horas
  wait_alumnos <- 0
  Horas <- param$Horas
  for(h in 1:length(Horas)){
    suma_x_hra <- sum(D[h,])
    if(suma_x_hra > 0){
      wait_alumnos <- c(wait_alumnos,rep(Horas[h],suma_x_hra))
    }
  }
  #Quitamos el cero inicial
  wait_alumnos <- wait_alumnos[-1]
  
  #' Definimos la lista en las que vamos a guardar el número de alumnos
  #' por materia
  num_alum_x_materia <- list()
  num_alum_x_materia[[1]] <- colSums(D)
  mixmdl_1_D <- normalmixEM(wait_alumnos,k = 4)#Modelo inicial
  
  #Hacemos "n_rep" veces el proceso
  for(d in 2:n_rep){
    cat("d = ",d)
    cont <- 1
    ### Obtener D
    D <- gen_mat_demanda_alumnos(param,param_sim)
    ### Obtener D' para generar esqueleto
    mat_calif_x_gpo <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])
    calif_D <- actualiza_calif_D(D0,D,mat_calif_x_gpo,ind_materias)
    mat_calif_x_gpo <- calif_D[[1]]
    vec_calif_x_materia <- calif_D[[2]]
    #' Actualizo mientras se cumplan las siguientes condiciones:
    while(any(vec_calif_x_materia< -20) || any(vec_calif_x_materia> 10)){
      ind_1 <- which(vec_calif_x_materia< -20)
      ind_2 <- which(vec_calif_x_materia> 10)
      ind_materias <- union(ind_1,ind_2)
      if(length(ind_materias) > 0){
        D <- actualiza_D_prima_metodo_D(cota,D0,D,mixmdl_1_D,
                                        calif_D,ind_materias)
        calif_D <- actualiza_calif_D(D0,D,mat_calif_x_gpo,
                                     ind_materias)
        mat_calif_x_gpo <- calif_D[[1]]
        vec_calif_x_materia <- calif_D[[2]]
      }else{
        cat("\n Todas las calificaciones están dentro del intervalo [-20,10]")
        break;
      }
      if(cont >= cota){
        break;
      }
      cont <- cont + 1
    }#Fin while()
    
    num_alum_x_materia[[d]] <- colSums(D)
    ##Convertimos los datos para obtener la distribución por horas
    for(h in 1:length(Horas)){
      suma_x_hra <- sum(D[h,])
      if(suma_x_hra > 0){
        wait_alumnos <- c(wait_alumnos,rep(Horas[h],suma_x_hra))
      }
    }
  }#2.7805 min
  
  ### Obtenemos el número promedio de grupos por materia
  mat_alum_x_materia <- matrix(0,nrow = n_rep,
                               ncol = length(param$vec_nom_materias_total))
  
  for(r in 1:n_rep){#Recorre las listas
    mat_alum_x_materia[r,] <- num_alum_x_materia[[r]]
  }
  prom_alum_x_materia <- ceiling(colMeans(mat_alum_x_materia))
  
  #Generamos la matriz D final
  D_final <- matrix(0,nrow = length(param$Horas),
                    ncol = length(param$vec_nom_materias_total))
  mixmdl_D <- normalmixEM(wait_alumnos,mixmdl_1_D$mu)#Modelo final
  
  for(c in 1:length(param$vec_nom_materias_total)){
    num_alum_1_materia <- prom_alum_x_materia[c]
    (rand_num <- sort(round(rnorm(num_alum_1_materia,mixmdl_D$mu,mixmdl_D$sigma))))
    ind_7 <- which(rand_num < 8)
    ind_22 <- which(rand_num >= 21)
    
    if(length(ind_7) > 0){
      rand_num[ind_7] <- 7
    }
    if(length(ind_22) > 0){
      rand_num[ind_22] <- 21
    }
    
    for(r in 1:length(param$Horas)){
      ind_hrs <- which(rand_num == param$Horas[r])
      if(length(ind_hrs) > 0){
        D_final[r,c] <- length(ind_hrs)
      }
    }
  }#Fin for(c)
  rownames(D_final) <- param$Horas
  colnames(D_final) <- param$vec_nom_materias_total
  
  cat("\nLa función metodo_C2 tardó: ",(proc.time()-ptm)[3]/60," minutos\n")
  return(D_final)
}



#' Title metodo_D: Función que genera un esqueleto con la metodología D, la
#' cual implementa la mezcla de normales por número de alumnos. Se combina
#' el inicio de la metodología C con la función "gen_esqueleto".
#'
#' @param n_rep: Número de veces que se generarán los esqueletos para
#' obtener información.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @param param_sim: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se encargan de la simulación.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' @example param_sim <- list(vec_sem_sig = c(20191,20192,20201),k_sem_ant = 5,
#' materia = "Estadística III", num_sim = 10, m_filtrada = matrix(0),
#' sub_m_filtrada = matrix(0,ncol = length(param$nom_cols_MG)))
#'
#' @return lista_esq_D_prima: Lista que contiene 2 matrices, la matriz
#' "mat_esqueleto" con el esqueleto generado y la matriz "D_prima" la
#' cual contiene el primedio de los esqueletos generados "n_rep" veces.
#'
#' @examples
#' lista_esq_D_prima <- metodo_D(n_rep,param,param_sim)
#' 
metodo_D <- function(n_rep,param,param_sim){
  ptm <- proc.time()# Start the clock!
  #Definimos D_inicial
  D_inicial <- metodo_C2(n_rep,param,param_sim)
  
  ##Generar esqueleto
  mat_solicitudes <- gen_solicitudes(param)#7.97 seg
  lista_info_esqueleto <- gen_esqueleto(D_inicial,mat_solicitudes,param)#10.76 seg
  mat_esqueleto <- lista_info_esqueleto[[1]]
  
  lista_esq_D_prima <- list()
  lista_esq_D_prima[[1]] <- mat_esqueleto
  lista_esq_D_prima[[2]] <- D_inicial
  
  cat("\nLa función metodo_D tardó: ",(proc.time()-ptm)[3]/60," minutos\n")
  return(lista_esq_D_prima)
}