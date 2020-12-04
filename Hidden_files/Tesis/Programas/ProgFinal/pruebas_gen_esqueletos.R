##########################################################################
#' En este programa se encuentran las funciones de las 3 metodologías
#' para generar un esqueleto.
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
### Obtener D
set.seed(1806)
D <- gen_mat_demanda_alumnos(param,param_sim)#42.25 seg
View(D)

### Obtener D'0 = E
set.seed(1806)
mat_solicitudes <- gen_solicitudes(param)#7.97 seg
View(mat_solicitudes)
set.seed(1806)
lista_info_esqueleto <- gen_esqueleto(D,mat_solicitudes,param)#11.36 seg
E <- lista_info_esqueleto[[8]]#Matriz con el número de alumnos simulados
D_prima_inicial <- E

### Calificar E
# mat_calif_x_gpo <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])
# calif_D <- actualiza_calif_D(D,E,mat_calif_x_gpo,1:dim(D)[2])
# View(calif_D)

### Aplicar mezcla de normales inicial
# param_sim$vec_sem_sig = 20202
# param_sim$k_sem_ant = 5
vec_s_sem_k_info <- gen_vec_s_sem_k_info(param_sim$vec_sem_sig,
                                         param_sim$k_sem_ant,param)
lista_mod_y_wait <- gen_normalmixEM_inicial(vec_s_sem_k_info,E,param,param_sim)
# hist(mixmdl_1_materia$x,freq = F,breaks = seq(7,22,by = 1),)
# lines(density(rnorm(1000,mean = mixmdl_1_materia$mu,sd = mixmdl_1_materia$sigma)),
#       lty=1,lwd=2,col = "blue")
# lines(density(mixmdl_1_materia$x), lty=1,lwd=2,col = "green")
# legend(15,0.4,c("GMM","density()"),bty = "n",
#        col=c("blue","green"),lty=c(1,1),
#        cex=1.1,lwd=2)
# hist(wait_1_materia,freq = F,breaks = seq(7,22,by = 1),)
# lines(density(wait_1_materia), lty=1,lwd=2,col = "green")
# legend(15,0.4,c("GMM","density()"),bty = "n",
#        col=c("blue","green"),lty=c(1,1),
#        cex=1.1,lwd=2)

### Obtener D' para generar esqueleto
cota <- 1000
ptm <- proc.time()# Start the clock!
D_prima <-  gen_D_prima(D,E,lista_mod_y_wait,cota)
cat("\nLa función gen_D_prima tardó: ",(proc.time()-ptm)[3],
    " segundos\n")##3.14 seg/ 4.41 seg
View(D_prima)

##Generar esqueleto
set.seed(8654)
mat_solicitudes <- gen_solicitudes(param)#7.97 seg
# View(mat_solicitudes)
set.seed(8654)
lista_info_esqueleto <- gen_esqueleto(D_prima,mat_solicitudes,param)#10.76 seg
mat_esqueleto <- lista_info_esqueleto[[1]]
View(mat_esqueleto)

##Calificamos el esqueleto
(calif_esqueleto <- califica_esqueleto(D_prima,lista_info_esqueleto,param))#-7594.3/-7685.7

##Calificamos D_prima
# D <- D
# D_prima <- D_prima
mat_calif_x_gpo <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])
ind_materias <- 1:dim(D)[2]
calif_D <- actualiza_calif_D(D,D_prima,mat_calif_x_gpo,ind_materias)
mat_calif_x_gpo <- calif_D[[1]]
vec_calif_x_materia <- calif_D[[2]]

View(mat_calif_x_gpo)
View(vec_calif_x_materia)

calif_esqueleto_2 <- sum(vec_calif_x_materia)#-453.9001


# Metodología B -----------------------------------------------------------
### Obtener D
set.seed(1806)
D <- gen_mat_demanda_alumnos(param,param_sim)#42.44 seg
D_inicial <- D
# View(D)

##Generar esqueleto
set.seed(8654)
mat_solicitudes <- gen_solicitudes(param)#8.33 seg
# View(mat_solicitudes)
set.seed(8654)
lista_info_esqueleto <- gen_esqueleto(D_inicial,mat_solicitudes,param)#11.91 seg
mat_esqueleto <- lista_info_esqueleto[[1]]
View(mat_esqueleto)
lista_de_lista_info_esqueleto <- list()
lista_de_lista_info_esqueleto[[1]] <- lista_info_esqueleto

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
num_gpos_x_materia <- list()
modelos_x_esqueleto <- list()
num_gpos_x_materia[[1]] <- colSums(mat_esqueleto)
mixmdl_1_esqueleto <- normalmixEM(wait_mat_esqueleto,mean=mean(wait_mat_esqueleto))
modelos_x_esqueleto[[1]] <- mixmdl_1_esqueleto

# hist(wait_mat_esqueleto,freq = F,breaks = seq(7,22,by = 1),)
# lines(density(rnorm(1000,mean = mixmdl_1_esqueleto$mu,sd = mixmdl_1_esqueleto$sigma)),
#       lty=1,lwd=2,col = "blue")
# lines(density(wait_mat_esqueleto), lty=1,lwd=2,col = "green")
# legend(15,0.2,c("GMM","density()"),bty = "n",
#        col=c("blue","green"),lty=c(1,1),
#        cex=1.1,lwd=2)

#Hacemos "n_rep" veces el proceso
n_rep <- 50
ptm <- proc.time()# Start the clock!
for(d in 2:n_rep){
  cat("d = ",d)
  ### Obtener D
  D <- gen_mat_demanda_alumnos(param,param_sim)
  
  ##Generar esqueleto
  mat_solicitudes <- gen_solicitudes(param)
  lista_info_esqueleto <- gen_esqueleto(D,mat_solicitudes,param)
  mat_esqueleto <- lista_info_esqueleto[[1]]
  num_gpos_x_materia[[d]] <- colSums(mat_esqueleto)
  lista_de_lista_info_esqueleto[[d]] <- lista_info_esqueleto
  
  ##Convertimos los datos para obtener la distribución por horas
  for(h in 1:length(Horas)){
    suma_x_hra <- sum(mat_esqueleto[h,])
    if(suma_x_hra > 0){
      wait_mat_esqueleto <- c(wait_mat_esqueleto,rep(Horas[h],suma_x_hra))
    }
  }
  mu_esq <- modelos_x_esqueleto[[(d-1)]]$mu
  mixmdl_1_esqueleto <- normalmixEM(wait_mat_esqueleto,mean=mu_esq)
  modelos_x_esqueleto[[d]] <- mixmdl_1_esqueleto
}
cat("\nEl ciclo tardó: ",(proc.time()-ptm)[3]/60," minutos\n")
#38.63933 min - d = 1:37
#59.476 min - d = 1:50
#52.23 min - d = 1:50
#53.23 min - d = 1:50

View(num_gpos_x_materia)
# View(lista_info_esqueleto)
View(modelos_x_esqueleto)
View(lista_de_lista_info_esqueleto)

#'Definimos las matrices finales para calificar el esqueleto final
### Primero obtenemos el número promedio de grupos por materia
mat_gpos_x_materia <- matrix(0,nrow = n_rep,
                             ncol = length(param$vec_nom_materias_total))
# lista_mat_prof_TC <- list()
lista_mat_demanda_aux <- list()
# lista_mat_solicitudes_TC <- list()
# lista_mat_solicitudes_asignatura <- list()
lista_num_alum_simulados <- list()
matrices_E <- list()

for(r in 1:n_rep){#Recorre las listas
  mat_gpos_x_materia[r,] <- num_gpos_x_materia[[r]]
  lista_mat_demanda_aux[[r]] <- lista_de_lista_info_esqueleto[[r]][[4]]
  lista_num_alum_simulados[[r]] <- lista_de_lista_info_esqueleto[[r]][[7]]
  matrices_E[[r]] <- lista_de_lista_info_esqueleto[[r]][[8]]
}
prom_gpos_x_materia <- ceiling(colMeans(mat_gpos_x_materia))

#Generamos el esqueleto final
mat_esqueleto_final <- matrix(0,nrow = length(param$Horas),
                              ncol = length(param$vec_nom_materias_total))
mixmdl <- modelos_x_esqueleto[[n_rep]]
for(c in 1:length(param$vec_nom_materias_total)){
  num_gpos_1_materia <- prom_gpos_x_materia[c]
  (rand_num <- sort(round(rnorm(num_gpos_1_materia,mixmdl$mu,mixmdl$sigma))))
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
lista_info_esqueleto[[1]] <- mat_esqueleto_final


### mat_demanda_aux
X <- lista_mat_demanda_aux
Y <- do.call(cbind, X)
Y <- array(Y, dim=c(dim(X[[1]]), length(X)))
mat_demanda_aux <- ceiling(apply(Y, c(1, 2), mean, na.rm = TRUE))

### num_alum_simulados
X <- lista_num_alum_simulados
Y <- do.call(cbind, X)
# Y <- array(Y, dim=c(dim(X[[1]]), length(X)))
num_alum_simulados <- ceiling(mean(Y))

### E
X <- matrices_E
Y <- do.call(cbind, X)
Y <- array(Y, dim=c(dim(X[[1]]), length(X)))
E <- ceiling(apply(Y, c(1, 2), mean, na.rm = TRUE))


# mat_esqueleto <- lista_info_esqueleto[[1]]
lista_info_esqueleto[[2]] <- lista_de_lista_info_esqueleto[[n_rep]][[2]]
lista_info_esqueleto[[3]] <- lista_de_lista_info_esqueleto[[n_rep]][[3]]
lista_info_esqueleto[[4]] <- mat_demanda_aux
lista_info_esqueleto[[5]] <- lista_de_lista_info_esqueleto[[n_rep]][[5]]
lista_info_esqueleto[[6]] <- lista_de_lista_info_esqueleto[[n_rep]][[6]]
lista_info_esqueleto[[7]] <- num_alum_simulados
lista_info_esqueleto[[8]] <- E #Matriz con el número de alumnos simulados



##Calificamos el esqueleto
(calif_esqueleto <- califica_esqueleto(D_inicial,lista_info_esqueleto,param))#-8546.1


##Calificamos D_prima
D <- D_inicial
D_prima <- E
mat_calif_x_gpo <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])
ind_materias <- 1:dim(D)[2]
calif_D <- actualiza_calif_D(D,D_prima,mat_calif_x_gpo,ind_materias)
mat_calif_x_gpo <- calif_D[[1]]
vec_calif_x_materia <- calif_D[[2]]

View(mat_calif_x_gpo)
View(vec_calif_x_materia)

(calif_esqueleto_2 <- sum(vec_calif_x_materia))#-1650.392




# Metodología C -----------------------------------------------------------
### Obtener D
set.seed(1806)
D <- gen_mat_demanda_alumnos(param,param_sim)#42.44 seg
D_inicial <- D
# View(D)

#' Definimos las listas en las que vamos a guardar el número de alumnos por materia
#' y la lista de listas con los modelos de mezcla de normales para cada materia.
num_alum_x_materia <- list()
lista_modelos <- list()
mixmdl_m_materias <- list()

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


#' Definimos las listas en las que vamos a guardar el número de grupos por materia
#' y los modelos de mezcla de normales para cada esqueleto
num_alum_x_materia <- list()
modelos_x_D <- list()
num_alum_x_materia[[1]] <- colSums(D)
mixmdl_1_D <- normalmixEM(wait_alumnos,mean=mean(wait_alumnos))
modelos_x_D[[1]] <- mixmdl_1_D

# hist(wait_alumnos,freq = F,breaks = seq(7,22,by = 1),)
# lines(density(rnorm(1000,mean = mixmdl_1_D$mu,sd = mixmdl_1_D$sigma)),
#       lty=1,lwd=2,col = "blue")
# lines(density(wait_alumnos), lty=1,lwd=2,col = "green")
# legend(15,0.2,c("GMM","density()"),bty = "n",
#        col=c("blue","green"),lty=c(1,1),
#        cex=1.1,lwd=2)

#Hacemos "n_rep" veces el proceso
n_rep <- 50
ptm <- proc.time()# Start the clock!
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
  mu_alum <- modelos_x_D[[(d-1)]]$mu
  mixmdl_1_D <- normalmixEM(wait_alumnos,mean=mu_alum)
  modelos_x_D[[d]] <- mixmdl_1_D
}
cat("\nEl ciclo tardó: ",(proc.time()-ptm)[3]/60," minutos\n")
#43.15 min - d = 1:50


View(num_alum_x_materia)
View(modelos_x_D)

#'Definimos las matrices finales para calificar el esqueleto final
### Primero obtenemos el número promedio de grupos por materia
mat_alum_x_materia <- matrix(0,nrow = n_rep,
                             ncol = length(param$vec_nom_materias_total))

for(r in 1:n_rep){#Recorre las listas
  mat_alum_x_materia[r,] <- num_alum_x_materia[[r]]
}
prom_alum_x_materia <- ceiling(colMeans(mat_alum_x_materia))

#Generamos la matriz D final
D_final <- matrix(0,nrow = length(param$Horas),
                              ncol = length(param$vec_nom_materias_total))
mixmdl_D <- modelos_x_D[[n_rep]]
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
View(D_final)




### Metodología 2
D_inicial <- D_final

##Generar esqueleto
set.seed(8654)
mat_solicitudes <- gen_solicitudes(param)#7.91 seg
# View(mat_solicitudes)
set.seed(8654)
lista_info_esqueleto <- gen_esqueleto(D_inicial,mat_solicitudes,param)#16.73 seg
mat_esqueleto <- lista_info_esqueleto[[1]]
View(mat_esqueleto)
lista_de_lista_info_esqueleto <- list()
lista_de_lista_info_esqueleto[[1]] <- lista_info_esqueleto

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
num_gpos_x_materia <- list()
modelos_x_esqueleto <- list()
num_gpos_x_materia[[1]] <- colSums(mat_esqueleto)
mixmdl_1_esqueleto <- normalmixEM(wait_mat_esqueleto,mean=mean(wait_mat_esqueleto))
modelos_x_esqueleto[[1]] <- mixmdl_1_esqueleto

# hist(wait_mat_esqueleto,freq = F,breaks = seq(7,22,by = 1),)
# lines(density(rnorm(1000,mean = mixmdl_1_esqueleto$mu,sd = mixmdl_1_esqueleto$sigma)),
#       lty=1,lwd=2,col = "blue")
# lines(density(wait_mat_esqueleto), lty=1,lwd=2,col = "green")
# legend(15,0.2,c("GMM","density()"),bty = "n",
#        col=c("blue","green"),lty=c(1,1),
#        cex=1.1,lwd=2)

#Hacemos "n_rep" veces el proceso
n_rep <- 50
ptm <- proc.time()# Start the clock!
for(d in 2:n_rep){
  cat("d = ",d)
  ### Obtener D
  D <- gen_mat_demanda_alumnos(param,param_sim)
  
  ##Generar esqueleto
  mat_solicitudes <- gen_solicitudes(param)
  lista_info_esqueleto <- gen_esqueleto(D,mat_solicitudes,param)
  mat_esqueleto <- lista_info_esqueleto[[1]]
  num_gpos_x_materia[[d]] <- colSums(mat_esqueleto)
  lista_de_lista_info_esqueleto[[d]] <- lista_info_esqueleto
  
  ##Convertimos los datos para obtener la distribución por horas
  for(h in 1:length(Horas)){
    suma_x_hra <- sum(mat_esqueleto[h,])
    if(suma_x_hra > 0){
      wait_mat_esqueleto <- c(wait_mat_esqueleto,rep(Horas[h],suma_x_hra))
    }
  }
  mu_esq <- modelos_x_esqueleto[[(d-1)]]$mu
  mixmdl_1_esqueleto <- normalmixEM(wait_mat_esqueleto,mean=mu_esq)
  modelos_x_esqueleto[[d]] <- mixmdl_1_esqueleto
}
cat("\nEl ciclo tardó: ",(proc.time()-ptm)[3]/60," minutos\n")
#53.79633 min - d = 1:50

View(num_gpos_x_materia)
# View(lista_info_esqueleto)
View(modelos_x_esqueleto)
View(lista_de_lista_info_esqueleto)

#'Definimos las matrices finales para calificar el esqueleto final
### Primero obtenemos el número promedio de grupos por materia
mat_gpos_x_materia <- matrix(0,nrow = n_rep,
                             ncol = length(param$vec_nom_materias_total))
# lista_mat_prof_TC <- list()
lista_mat_demanda_aux <- list()
# lista_mat_solicitudes_TC <- list()
# lista_mat_solicitudes_asignatura <- list()
lista_num_alum_simulados <- list()
matrices_E <- list()

for(r in 1:n_rep){#Recorre las listas
  mat_gpos_x_materia[r,] <- num_gpos_x_materia[[r]]
  lista_mat_demanda_aux[[r]] <- lista_de_lista_info_esqueleto[[r]][[4]]
  lista_num_alum_simulados[[r]] <- lista_de_lista_info_esqueleto[[r]][[7]]
  matrices_E[[r]] <- lista_de_lista_info_esqueleto[[r]][[8]]
}
prom_gpos_x_materia <- ceiling(colMeans(mat_gpos_x_materia))

#Generamos el esqueleto final
mat_esqueleto_final <- matrix(0,nrow = length(param$Horas),
                              ncol = length(param$vec_nom_materias_total))
mixmdl <- modelos_x_esqueleto[[n_rep]]
for(c in 1:length(param$vec_nom_materias_total)){
  num_gpos_1_materia <- prom_gpos_x_materia[c]
  (rand_num <- sort(round(rnorm(num_gpos_1_materia,mixmdl$mu,mixmdl$sigma))))
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
lista_info_esqueleto[[1]] <- mat_esqueleto_final


### mat_demanda_aux
X <- lista_mat_demanda_aux
Y <- do.call(cbind, X)
Y <- array(Y, dim=c(dim(X[[1]]), length(X)))
mat_demanda_aux <- ceiling(apply(Y, c(1, 2), mean, na.rm = TRUE))

### num_alum_simulados
X <- lista_num_alum_simulados
Y <- do.call(cbind, X)
# Y <- array(Y, dim=c(dim(X[[1]]), length(X)))
num_alum_simulados <- ceiling(mean(Y))

### E
X <- matrices_E
Y <- do.call(cbind, X)
Y <- array(Y, dim=c(dim(X[[1]]), length(X)))
E <- ceiling(apply(Y, c(1, 2), mean, na.rm = TRUE))


# mat_esqueleto <- lista_info_esqueleto[[1]]
lista_info_esqueleto[[2]] <- lista_de_lista_info_esqueleto[[n_rep]][[2]]
lista_info_esqueleto[[3]] <- lista_de_lista_info_esqueleto[[n_rep]][[3]]
lista_info_esqueleto[[4]] <- mat_demanda_aux
lista_info_esqueleto[[5]] <- lista_de_lista_info_esqueleto[[n_rep]][[5]]
lista_info_esqueleto[[6]] <- lista_de_lista_info_esqueleto[[n_rep]][[6]]
lista_info_esqueleto[[7]] <- num_alum_simulados
lista_info_esqueleto[[8]] <- E #Matriz con el número de alumnos simulados



##Calificamos el esqueleto
(calif_esqueleto <- califica_esqueleto(D_inicial,lista_info_esqueleto,param))#-9194


##Calificamos D_prima
D <- D_inicial
D_prima <- E
mat_calif_x_gpo <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])
ind_materias <- 1:dim(D)[2]
calif_D <- actualiza_calif_D(D,D_prima,mat_calif_x_gpo,ind_materias)
mat_calif_x_gpo <- calif_D[[1]]
vec_calif_x_materia <- calif_D[[2]]

View(mat_calif_x_gpo)
View(vec_calif_x_materia)

calif_esqueleto_2 <- sum(vec_calif_x_materia)#-877.9684



# Metodología 4 -----------------------------------------------------------
#' Modificar (A) para generar el cambio con la distribución y el promedio
#' de alumnos.

### Obtener D
set.seed(1806)
D <- gen_mat_demanda_alumnos(param,param_sim)#42.25 seg
View(D)

### Obtener D'0 = E
set.seed(8654)
D_prima_inicial <- gen_mat_demanda_alumnos(param,param_sim)#42.25 seg
View(D_prima_inicial)

### Aplicar mezcla de normales inicial
# param_sim$vec_sem_sig = 20202
# param_sim$k_sem_ant = 5
vec_s_sem_k_info <- gen_vec_s_sem_k_info(param_sim$vec_sem_sig,
                                         param_sim$k_sem_ant,param)
lista_mod_y_wait <- gen_normalmixEM_inicial(vec_s_sem_k_info,D_prima_inicial,
                                            param,param_sim)
# hist(lista_mod_y_wait[[1]][[5]]$x,freq = F,breaks = seq(7,22,by = 1),
#      main = "Probabilidad I")
# lines(density(rnorm(1000,mean = lista_mod_y_wait[[1]][[5]]$mu,
#                     sd = lista_mod_y_wait[[1]][[5]]$sigma)),
#       lty=1,lwd=2,col = "blue")
# lines(density(lista_mod_y_wait[[1]][[5]]$x), lty=1,lwd=2,col = "green")
# legend(15,0.4,c("GMM","density()"),bty = "n",
#        col=c("blue","green"),lty=c(1,1),
#        cex=1.1,lwd=2)
# mean(lista_mod_y_wait[[1]][[5]]$x)#11.97263


### Obtener D' para generar esqueleto
cota <- 1000

#Se definen las variables que se van a utilizar
mat_calif_x_gpo <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])
cont <- 1
D_prima <- D_prima_inicial
mixmdl <- lista_mod_y_wait[[1]]
# wait <- lista_mod_y_wait[[2]]
prom_alum_x_materia <- lista_mod_y_wait[[3]]

#Calificación inicial
calif_D <- actualiza_calif_D(D,D_prima,mat_calif_x_gpo,
                             1:dim(D)[2])
mat_calif_x_gpo <- calif_D[[1]]
vec_calif_x_materia <- calif_D[[2]]

#' Actualizo mientras se cumplan las siguientes condiciones:
while(any(vec_calif_x_materia< -20) || any(vec_calif_x_materia> 10)){
  ind_1 <- which(vec_calif_x_materia< -20)
  ind_2 <- which(vec_calif_x_materia> 10)
  ind_materias <- union(ind_1,ind_2)
  if(length(ind_materias) > 0){
    #' Para este punto ya comparamos D y D_prima. Se redefine D_prima.
    #' Recibe a D_prima como parámetro para que en caso de que no haya
    #' modificaciones, se regrese la misma matriz y no una llena de ceros.
    for(c in ind_materias){#Recorre columnas
      cont_1 <- 1
      cont_2 <- 1
      
      num_alum_1_materia <- prom_alum_x_materia[c]
      mixmdl <- lista_mod_y_wait[[1]][[c]]
      (rand_num <- sort(round(rnorm(num_alum_1_materia,mixmdl$mu,mixmdl$sigma))))
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
          D_prima[r,c] <- length(ind_hrs)
        }
      }
    }#Fin for(c)
    
    calif_D <- actualiza_calif_D(D,D_prima,mat_calif_x_gpo,
                                 ind_materias)
    mat_calif_x_gpo <- calif_D[[1]]
    vec_calif_x_materia <- calif_D[[2]]
    sum(vec_calif_x_materia)#-358.1767/-395.7022/-429.7916#La calificación va empeorando
    
    lista_mod_y_wait <- actualiza_mixmdl(lista_mod_y_wait,D_prima,
                                         ind_materias,param,param_sim)
    mixmdl <- lista_mod_y_wait[[1]]
    
  }else{
    cat("\n Todas las calificaciones están dentro del intervalo [-20,10]")
    break;
  }
  if(cont >= cota){
    break;
  }
  cont <- cont + 1
}#Fin while()


# Metodología 5 -----------------------------------------------------------
###Combinación de (A) con (B)

### Obtener D
set.seed(1806)
D <- gen_mat_demanda_alumnos(param,param_sim)#42.25 seg
View(D)

# metodo_A_gen_D_prima <- function(cota){
#   ### Obtener D'0 = E
#   mat_solicitudes <- gen_solicitudes(param)#7.97 seg
#   lista_info_esqueleto <- gen_esqueleto(D,mat_solicitudes,param)#11.36 seg
#   E <- lista_info_esqueleto[[8]]#Matriz con el número de alumnos simulados
#   
#   ### Aplicar mezcla de normales inicial
#   vec_s_sem_k_info <- gen_vec_s_sem_k_info(param_sim$vec_sem_sig,
#                                            param_sim$k_sem_ant,param)
#   lista_mod_y_wait <- gen_normalmixEM_inicial(vec_s_sem_k_info,E,param,param_sim)
#   
#   ### Obtener D' para generar esqueleto
#   D_prima <-  gen_D_prima(D,E,lista_mod_y_wait,cota)
#   
#   return(D_prima)
# }
# param_sim$vec_sem_sig = 20202
# param_sim$k_sem_ant = 5
cota <- 1000
ptm <- proc.time()# Start the clock!
D_prima <- metodo_A_gen_D_prima(cota)
cat("\nLa función metodo_A_gen_D_prima tardó: ",(proc.time()-ptm)[3],
    " segundos\n")##
View(D_prima)

##Calificamos D_prima
# D <- D_inicial
# D_prima <- E
mat_calif_x_gpo <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])
ind_materias <- 1:dim(D)[2]
calif_D <- actualiza_calif_D(D,D_prima,mat_calif_x_gpo,ind_materias)
mat_calif_x_gpo <- calif_D[[1]]
vec_calif_x_materia <- calif_D[[2]]

# View(mat_calif_x_gpo)
# View(vec_calif_x_materia)

(calif_esqueleto_2 <- sum(vec_calif_x_materia))#-440.0572


##Generar esqueleto inicial
set.seed(8654)
mat_solicitudes <- gen_solicitudes(param)#8.33 seg
# View(mat_solicitudes)
D_inicial <- D_prima
set.seed(8654)
lista_info_esqueleto <- gen_esqueleto(D_inicial,mat_solicitudes,param)#11.91 seg
mat_esqueleto <- lista_info_esqueleto[[1]]
View(mat_esqueleto)
# lista_de_lista_info_esqueleto <- list()
# lista_de_lista_info_esqueleto[[1]] <- lista_info_esqueleto

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
num_gpos_x_materia <- list()
modelos_x_esqueleto <- list()
num_gpos_x_materia[[1]] <- colSums(mat_esqueleto)
# mixmdl_1_esqueleto <- normalmixEM(wait_mat_esqueleto,mean=mean(wait_mat_esqueleto))
mixmdl_1_esqueleto <- normalmixEM(wait_mat_esqueleto,k = 3)
modelos_x_esqueleto[[1]] <- mixmdl_1_esqueleto

# hist(wait_mat_esqueleto,freq = F,breaks = seq(6,22,by = 1),)
# lines(density(rnorm(1000,mean = mixmdl_1_esqueleto$mu,sd = mixmdl_1_esqueleto$sigma)),
#       lty=1,lwd=2,col = "blue")
# lines(density(wait_mat_esqueleto), lty=1,lwd=2,col = "green")
# legend(15,0.135,c("GMM","density()"),bty = "n",
#        col=c("blue","green"),lty=c(1,1),
#        cex=1.1,lwd=2)

#Hacemos "n_rep" veces el proceso
n_rep <- 10
ptm <- proc.time()# Start the clock!
for(d in 2:n_rep){
  cat("d = ",d)
  ### Obtener D
  D <- gen_mat_demanda_alumnos(param,param_sim)
  
  ##Generar esqueleto
  mat_solicitudes <- gen_solicitudes(param)
  lista_info_esqueleto <- gen_esqueleto(D,mat_solicitudes,param)
  mat_esqueleto <- lista_info_esqueleto[[1]]
  num_gpos_x_materia[[d]] <- colSums(mat_esqueleto)
  # lista_de_lista_info_esqueleto[[d]] <- lista_info_esqueleto
  
  ##Convertimos los datos para obtener la distribución por horas
  for(h in 1:length(Horas)){
    suma_x_hra <- sum(mat_esqueleto[h,])
    if(suma_x_hra > 0){
      wait_mat_esqueleto <- c(wait_mat_esqueleto,rep(Horas[h],suma_x_hra))
    }
  }
  mu_esq <- modelos_x_esqueleto[[(d-1)]]$mu
  mixmdl_1_esqueleto <- normalmixEM(wait_mat_esqueleto,mean=mu_esq)
  modelos_x_esqueleto[[d]] <- mixmdl_1_esqueleto
}
cat("\nEl ciclo tardó: ",(proc.time()-ptm)[3]/60," minutos\n")
#38.63933 min - d = 1:37
#59.476 min - d = 1:50
#52.23 min - d = 1:50
#53.23 min - d = 1:50
#10.32 min - d = 1:10
#10.40267 min - d = 1:10

View(num_gpos_x_materia)
# View(lista_info_esqueleto)
View(modelos_x_esqueleto)
# View(lista_de_lista_info_esqueleto)

#' Obtenemos el número promedio de grupos por materia
mat_gpos_x_materia <- matrix(0,nrow = n_rep,
                             ncol = length(param$vec_nom_materias_total))
for(r in 1:n_rep){#Recorre las listas
  mat_gpos_x_materia[r,] <- num_gpos_x_materia[[r]]
}
prom_gpos_x_materia <- ceiling(colMeans(mat_gpos_x_materia))

#Generamos el esqueleto final
mat_esqueleto_final <- matrix(0,nrow = length(param$Horas),
                              ncol = length(param$vec_nom_materias_total))
mixmdl <- modelos_x_esqueleto[[n_rep]]
for(c in 1:length(param$vec_nom_materias_total)){
  num_gpos_1_materia <- prom_gpos_x_materia[c]
  (rand_num <- sort(round(rnorm(num_gpos_1_materia,mixmdl$mu,mixmdl$sigma))))
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
# mat_esqueleto_final <- as.data.frame(mat_esqueleto_final)
rownames(mat_esqueleto_final) <- param$Horas
colnames(mat_esqueleto_final) <- param$vec_nom_materias_total
View(mat_esqueleto_final)

##Convertimos los datos para obtener la gráfica de los resultados finales
wait_mat_esqueleto_final <- 0
for(h in 1:length(Horas)){
  suma_x_hra <- sum(mat_esqueleto[h,])
  if(suma_x_hra > 0){
    wait_mat_esqueleto_final <- c(wait_mat_esqueleto_final,rep(Horas[h],suma_x_hra))
  }
}
#Quitamos el cero inicial
wait_mat_esqueleto_final <- wait_mat_esqueleto_final[-1]

hist(wait_mat_esqueleto_final,freq = F,breaks = seq(7,22,by = 1),)
lines(density(rnorm(1000,mean = mixmdl$mu,mixmdl$sigma)),
      lty=1,lwd=2,col = "blue")
lines(density(wait_mat_esqueleto_final), lty=1,lwd=2,col = "green")
legend(15,0.2,c("GMM","density()"),bty = "n",
       col=c("blue","green"),lty=c(1,1),
       cex=1.1,lwd=2)


# Metodología 6 -----------------------------------------------------------
#' Generar D_prima con (A) y con ella generar 100 mat_esqueleto con la
#' función gen_esqueleto sin GMM. Generar wait_mat_esqueleto_final con GMM
#' y la información por materia de los 100 esqueletos.

### Obtener D
set.seed(1806)
D <- gen_mat_demanda_alumnos(param,param_sim)#42.25 seg
View(D)
Horas <- param$Horas

metodo_A_gen_D_prima <- function(D,cota,param){
  ### Obtener D'0 = E
  mat_solicitudes <- gen_solicitudes(param)#7.97 seg
  lista_info_esqueleto <- gen_esqueleto(D,mat_solicitudes,param)#11.36 seg
  E <- lista_info_esqueleto[[8]]#Matriz con el número de alumnos simulados
  
  ### Aplicar mezcla de normales inicial
  vec_s_sem_k_info <- gen_vec_s_sem_k_info(param_sim$vec_sem_sig,
                                           param_sim$k_sem_ant,param)
  lista_mod_y_wait <- gen_normalmixEM_inicial(vec_s_sem_k_info,E,param,param_sim)
  
  ### Obtener D' para generar esqueleto
  D_prima <-  gen_D_prima(D,E,lista_mod_y_wait,cota)
  
  return(D_prima)
}
# param_sim$vec_sem_sig = 20202
# param_sim$k_sem_ant = 5
cota <- 1000
ptm <- proc.time()# Start the clock!
D_prima <- metodo_A_gen_D_prima(cota)
cat("\nLa función metodo_A_gen_D_prima tardó: ",(proc.time()-ptm)[3],
    " segundos\n")##61.82 seg = 1.03 min
View(D_prima)

##Calificamos D_prima
# D <- D_inicial
# D_prima <- E
mat_calif_x_gpo <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])
ind_materias <- 1:dim(D)[2]
calif_D <- actualiza_calif_D(D,D_prima,mat_calif_x_gpo,ind_materias)
mat_calif_x_gpo <- calif_D[[1]]
vec_calif_x_materia <- calif_D[[2]]
# View(mat_calif_x_gpo)
# View(vec_calif_x_materia)
(calif_esqueleto_2 <- sum(vec_calif_x_materia))#-440.0572


##Generamos "n_rep" esqueletos y guardamos su información
lista_mat_esqueleto <- list()
n_rep <- 100
#' Cada esqueleto se genera con la misma D_prima y con diferente
#' matriz de solicitudes.
ptm <- proc.time()# Start the clock!
for(d in 1:n_rep){
  mat_solicitudes <- gen_solicitudes(param)#8.33 seg
  lista_info_esqueleto <- gen_esqueleto(D_prima,mat_solicitudes,param)#11.91 seg
  mat_esqueleto <- lista_info_esqueleto[[1]]
  lista_mat_esqueleto[[d]] <- mat_esqueleto
}
cat("\nEl ciclo tardó: ",(proc.time()-ptm)[3]/60," minutos\n")##36.6495 min


#' Juntamos todas las matrices de los esqueletos para tener
#' toda la información por materias (columnas).
mat_n_esqueletos <- do.call(rbind,lista_mat_esqueleto)
# prom_gpos_x_materia <- ceiling(colMeans(mat_n_esqueletos))#vector
prom_gpos_x_materia <- rep(0,length(param$vec_nom_materias_total))
# mat_aux <- matrix(0,nrow = n_rep,ncol = 2)
# for(d in 1:n_rep){
#   mat_aux[d,1] <- 1+(d-1)*15
#   mat_aux[d,2] <- 15+(d-1)*15
# }

for(c in 1:length(param$vec_nom_materias_total)){
  vec_1_materia <- mat_n_esqueletos[,c]
  vec_aux <- rep(0,n_rep)
  for(d in 1:n_rep){
    vec_aux[d] <- mean(vec_1_materia[(1+(d-1)*15):(15+(d-1)*15)])
  }
  prom_gpos_x_materia[c] <- ceiling(mean(vec_aux))#vector
}

#' El promedio de los grupos no es adecuado

 

# Metodología 7 -----------------------------------------------------------
#Combinar (C) con la función gen_esqueleto

### Obtener D
set.seed(1806)
D <- gen_mat_demanda_alumnos(param,param_sim)#42.44 seg
# View(D)
D_inicial <- D


#' Definimos las listas en las que vamos a guardar el número de alumnos por materia
#' y la lista de listas con los modelos de mezcla de normales para cada materia.
num_alum_x_materia <- list()
lista_modelos <- list()
mixmdl_m_materias <- list()

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


#' Definimos las listas en las que vamos a guardar el número de grupos por materia
#' y los modelos de mezcla de normales para cada esqueleto
num_alum_x_materia <- list()
modelos_x_D <- list()
num_alum_x_materia[[1]] <- colSums(D)
# mixmdl_1_D <- normalmixEM(wait_alumnos,mean=mean(wait_alumnos),k = 5)
# mixmdl_1_D <- normalmixEM(wait_alumnos,k = 5)#Modelo inicial
mixmdl_1_D <- normalmixEM(wait_alumnos,k = 4)#Modelo inicial
# mixmdl_1_D <- normalmixEM(wait_alumnos,k = 3)#Modelo inicial
modelos_x_D[[1]] <- mixmdl_1_D

# hist(wait_alumnos,freq = F,breaks = seq(6,22,by = 1),)
# lines(density(rnorm(1000,mean = mixmdl_1_D$mu,sd = mixmdl_1_D$sigma)),
#       lty=1,lwd=2,col = "blue")
# lines(density(wait_alumnos), lty=1,lwd=2,col = "green")
# legend(15,0.14,c("GMM","density()"),bty = "n",
#        col=c("blue","green"),lty=c(1,1),
#        cex=1.1,lwd=2)

#Hacemos "n_rep" veces el proceso
n_rep <- 10
ptm <- proc.time()# Start the clock!
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
  mu_alum <- modelos_x_D[[(d-1)]]$mu
  mixmdl_1_D <- normalmixEM(wait_alumnos,mean=mu_alum)
  modelos_x_D[[d]] <- mixmdl_1_D
}
cat("\nEl ciclo tardó: ",(proc.time()-ptm)[3]/60," minutos\n")
#43.15 min - d = 1:50
#6.655 min - d = 1:10


View(num_alum_x_materia)
View(modelos_x_D)

#'Definimos las matrices finales para calificar el esqueleto final
### Primero obtenemos el número promedio de grupos por materia
mat_alum_x_materia <- matrix(0,nrow = n_rep,
                             ncol = length(param$vec_nom_materias_total))

for(r in 1:n_rep){#Recorre las listas
  mat_alum_x_materia[r,] <- num_alum_x_materia[[r]]
}
prom_alum_x_materia <- ceiling(colMeans(mat_alum_x_materia))

#Generamos la matriz D final
D_final <- matrix(0,nrow = length(param$Horas),
                  ncol = length(param$vec_nom_materias_total))
mixmdl_D <- modelos_x_D[[n_rep]]
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
View(D_final)


##Calificamos D_prima
# D <- D
D_prima <- D_final
mat_calif_x_gpo <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])
ind_materias <- 1:dim(D)[2]
calif_D <- actualiza_calif_D(D,D_prima,mat_calif_x_gpo,ind_materias)
mat_calif_x_gpo <- calif_D[[1]]
vec_calif_x_materia <- calif_D[[2]]
# View(mat_calif_x_gpo)
# View(vec_calif_x_materia)
(calif_esqueleto_2 <- sum(vec_calif_x_materia))#-1980.377


##Generar esqueleto
# set.seed(8654)
mat_solicitudes <- gen_solicitudes(param)#7.97 seg
# View(mat_solicitudes)
# set.seed(8654)
lista_info_esqueleto <- gen_esqueleto(D_final,mat_solicitudes,param)#10.76 seg
mat_esqueleto <- lista_info_esqueleto[[1]]
View(mat_esqueleto)

# Metodología 8 -----------------------------------------------------------
#Modificar un poco (C) y hacer esqueleto con la función gen_esqueleto
### Obtener D
# set.seed(1806)
D <- gen_mat_demanda_alumnos(param,param_sim)#40.64 seg
# View(D)
D_inicial <- D


#' Definimos las listas en las que vamos a guardar el número de alumnos
#' por materia
num_alum_x_materia <- list()
num_alum_x_materia[[1]] <- colSums(D_inicial)
# lista_modelos <- list()

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


#' Definimos las listas en las que vamos a guardar el número de grupos por materia
#' y los modelos de mezcla de normales para cada esqueleto
num_alum_x_materia <- list()
# modelos_x_D <- list()
num_alum_x_materia[[1]] <- colSums(D)
# mixmdl_1_D <- normalmixEM(wait_alumnos,mean=mean(wait_alumnos),k = 5)
# mixmdl_1_D <- normalmixEM(wait_alumnos,k = 5)#Modelo inicial
mixmdl_1_D <- normalmixEM(wait_alumnos,k = 4)#Modelo inicial
# mixmdl_1_D <- normalmixEM(wait_alumnos,k = 3)#Modelo inicial
# modelos_x_D[[1]] <- mixmdl_1_D

# hist(wait_alumnos,freq = F,breaks = seq(6,22,by = 1),)
# lines(density(rnorm(1000,mean = mixmdl_1_D$mu,sd = mixmdl_1_D$sigma)),
#       lty=1,lwd=2,col = "blue")
# lines(density(wait_alumnos), lty=1,lwd=2,col = "green")
# legend(15,0.14,c("GMM","density()"),bty = "n",
#        col=c("blue","green"),lty=c(1,1),
#        cex=1.1,lwd=2)

#Hacemos "n_rep" veces el proceso
n_rep <- 10
ptm <- proc.time()# Start the clock!
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
  # mu_alum <- modelos_x_D[[(d-1)]]$mu
  # mixmdl_1_D <- normalmixEM(wait_alumnos,mean=mu_alum)
  # modelos_x_D[[d]] <- mixmdl_1_D
}
cat("\nEl ciclo tardó: ",(proc.time()-ptm)[3]/60," minutos\n")
#43.15 min - d = 1:50
#6.655 min - d = 1:10
#6.35 min - d = 1:10

View(num_alum_x_materia)
# View(modelos_x_D)

#'Definimos las matrices finales para calificar el esqueleto final
### Primero obtenemos el número promedio de grupos por materia
mat_alum_x_materia <- matrix(0,nrow = n_rep,
                             ncol = length(param$vec_nom_materias_total))

for(r in 1:n_rep){#Recorre las listas
  mat_alum_x_materia[r,] <- num_alum_x_materia[[r]]
}
prom_alum_x_materia <- ceiling(colMeans(mat_alum_x_materia))

#Generamos la matriz D final
D_final <- matrix(0,nrow = length(param$Horas),
                  ncol = length(param$vec_nom_materias_total))
# mixmdl_D <- normalmixEM(wait_alumnos,mixmdl_1_D$mu,mixmdl_1_D$sigma)#Modelo final
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
View(D_final)


##Calificamos D_prima
# D <- D
D_prima <- D_final
mat_calif_x_gpo <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])
ind_materias <- 1:dim(D)[2]
calif_D <- actualiza_calif_D(D,D_prima,mat_calif_x_gpo,ind_materias)
mat_calif_x_gpo <- calif_D[[1]]
vec_calif_x_materia <- calif_D[[2]]
# View(mat_calif_x_gpo)
# View(vec_calif_x_materia)
(calif_esqueleto_2 <- sum(vec_calif_x_materia))#-1980.377/-1832.852


##Generar esqueleto
# set.seed(8654)
mat_solicitudes <- gen_solicitudes(param)#7.97 seg
# View(mat_solicitudes)
# set.seed(8654)
lista_info_esqueleto <- gen_esqueleto(D_final,mat_solicitudes,param)#10.76 seg
mat_esqueleto <- lista_info_esqueleto[[1]]
# View(mat_esqueleto)



