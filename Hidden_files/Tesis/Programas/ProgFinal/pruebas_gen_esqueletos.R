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



# Metodología 1 -----------------------------------------------------------
### Obtener D
set.seed(1806)
D <- gen_mat_demanda_alumnos(param,param_sim)#55.61 seg
View(D)

### Obtener D'0 = E
set.seed(1806)
mat_solicitudes <- gen_solicitudes(param)#8.33 seg
View(mat_solicitudes)
set.seed(1806)
lista_info_esqueleto <- gen_esqueleto(D,mat_solicitudes,param)#14.56 seg
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
    " segundos\n")##3.14
View(D_prima)

##Generar esqueleto
set.seed(8654)
mat_solicitudes <- gen_solicitudes(param)#8.33 seg
# View(mat_solicitudes)
set.seed(8654)
lista_info_esqueleto <- gen_esqueleto(D_prima,mat_solicitudes,param)#13.03 seg
mat_esqueleto <- lista_info_esqueleto[[1]]
View(mat_esqueleto)

##Calificamos el esqueleto
(calif_esqueleto <- califica_esqueleto(D_prima,lista_info_esqueleto,param))#-7594.3



# Metodología 2 -----------------------------------------------------------
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
lista_info_esqueleto <- gen_esqueleto(D,mat_solicitudes,param)#16.73 seg
mat_esqueleto <- lista_info_esqueleto[[1]]
View(mat_esqueleto)
lista_de_lista_info_esqueleto <- list()
lista_de_lista_info_esqueleto[[1]] <- lista_info_esqueleto

##Convertimos los datos para obtener la distribución por horas
wait_mat_esqueleto <- 0

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
  mixmdl_1_esqueleto <- normalmixEM(wait_mat_esqueleto,mean=mean(wait_mat_esqueleto))
  modelos_x_esqueleto[[d]] <- mixmdl_1_esqueleto
}
cat("\nEl ciclo tardó: ",(proc.time()-ptm)[3]/60," minutos\n")
#38.63933 min - d = 1:37
#59.476 min - d = 1:50

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
Y <- array(Y, dim=c(dim(X[[1]]), length(X)))
num_alum_simulados <- ceiling(apply(Y, c(1, 2), mean, na.rm = TRUE))

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



# Metodología 3 -----------------------------------------------------------


