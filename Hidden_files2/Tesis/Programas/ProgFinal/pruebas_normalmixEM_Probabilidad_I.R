##########################################################################
#' En este programa se encuentran algunas pruebas de la función
#' normalmixEM() para los datos de "Probabilidad I".
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")


# normalmixEM en Proba I --------------------------------------------------
vec_s_sem_k_info <- gen_vec_s_sem_k_info(param_sim$vec_sem_sig,
                                         param_sim$k_sem_ant,param)
materia <- "Probabilidad I"
param_sim$Materias = materia
param_sim$m_filtrada <- gen_mat_m_filtrada(param,param_sim)
mat_alumnos_corregidos <- gen_mat_alumnos_corregidos(vec_s_sem_k_info,
                                                     param,param_sim)
set.seed(1806)
(d_prima <- simula_alumnos(mat_alumnos_corregidos,param))



wait = c(as.vector(mat_alumnos_corregidos),d_prima)
# hist(wait,freq = F)
# mixmdl = normalmixEM(wait)#Error in normalmixEM(wait) : Too many tries!
# mixmdl = normalmixEM(wait,k = 1)#arbmean and arbvar cannot both be FALSE
mixmdl = normalmixEM(wait,mean = mean(wait))
plot(mixmdl,density = TRUE)# gives both plots
plot(mixmdl,which=1)#displays the log likelihood plot (this is the default)
plot(mixmdl,which = 2)#displays the density components/histogram plot
lines(density(wait), lty=2, lwd=2)
lines(density(rnorm(1000,mean = mixmdl$mu,sd = mixmdl$sigma)), lty=1,
      lwd=2,col = "blue")


mixmdl$x #Datos que se le pasaron como parámetro a la función
hist(mixmdl$x,freq = F)
lines(density(rnorm(1000,mean = mixmdl$mu,sd = mixmdl$sigma)), lty=1,
      lwd=2,col = "blue")
lines(density(mixmdl$x), lty=1,lwd=2,col = "green")

mixmdl$lambda #The final mixing proportions
mixmdl$mu #The final mean parameters
mixmdl$sigma #The final standard deviations. If arbmean = FALSE,
#then only the smallest standard deviation is returned.

mixmdl$loglik#Último valor de la máxima verosimilitud
mixmdl$all.loglik#Todos los valores de la máxima verosimilitud
#' (debe de ser cada vez mejor). A vector of each iteration's log-likelihood.
#' This vector includes both the initial and the final values; thus, the
#' number of iterations is one less than its length.

mixmdl$posterior #An nxk matrix of posterior probabilities for observations
#' En la columna 1 de la tabla anterior se tiene la probabilidad
#' de que el dato del renglón i pertenezca a la primera componente
#' (a la normal 1). En la segunda columna se tiene la probabilidad
#' de que el dato del renglón i pertenezca a la segunda componente
#' (a la normal 2).

mixmdl$restarts #The number of times the algorithm restarted due to
#unacceptable choice of initial values.

mixmdl$ft #A character vector giving the name of the function.



# Prueba de for -----------------------------------------------------------
### Inicialización
vec_s_sem_k_info <- gen_vec_s_sem_k_info(param_sim$vec_sem_sig,
                                         param_sim$k_sem_ant,param)
materia <- "Probabilidad I"
param_sim$Materias = materia
param_sim$m_filtrada <- gen_mat_m_filtrada(param,param_sim)
mat_alumnos_corregidos <- gen_mat_alumnos_corregidos(vec_s_sem_k_info,
                                                     param,param_sim)
set.seed(1806)
(d_prima <- simula_alumnos(mat_alumnos_corregidos,param))

wait = c(as.vector(mat_alumnos_corregidos),d_prima)
mixmdl = normalmixEM(wait,mean = mean(wait))
mixmdl$loglik#-507.4657


# mixmdl$x #Datos que se le pasaron como parámetro a la función
hist(mixmdl$x,freq = F)
lines(density(rnorm(1000,mean = mixmdl$mu,sd = mixmdl$sigma)), lty=1,
      lwd=2,col = "blue")
lines(density(mixmdl$x), lty=1,lwd=2,col = "green")
legend(300,0.006,c("GMM","density()"),bty = "n",
       col=c("blue","green"),lty=c(1,1),
       cex=1.1,lwd=2)


for(d in 1:3){
  cat("\n d = ",d)
  set.seed(1806+d)
  (d_prima <- simula_alumnos(mat_alumnos_corregidos,param))
  
  wait = c(wait,d_prima)#Actualizamos los datos
  
  mixmdl = normalmixEM(wait,mu = unique(mixmdl$mu))
  # mixmdl = normalmixEM(wait,mu = mixmdl$mu)
  # mixmdl = normalmixEM(wait,lambda = mixmdl$lambda, mu = mixmdl$mu,
  #                      sigma = mixmdl$sigma)
  # mixmdl = normalmixEM(wait,mu = mixmdl$mu,sigma = mixmdl$sigma)
  # mixmdl = normalmixEM(wait,mu = mixmdl$mu, k = length(mixmdl$mu))
  # mixmdl = normalmixEM(wait,sigma = mixmdl$sigma, k = length(mixmdl$mu))
  cat("\n loglik = ",mixmdl$loglik)
  # mixmdl$x #Datos que se le pasaron como parámetro a la función
  hist(mixmdl$x,freq = F)
  lines(density(rnorm(1000,mean = mixmdl$mu,sd = mixmdl$sigma)), lty=1,
        lwd=2,col = "blue")
  lines(density(mixmdl$x), lty=1,lwd=2,col = "green")
  legend(300,0.006,c("GMM","density()"),bty = "n",
         col=c("blue","green"),lty=c(1,1),
         cex=1.1,lwd=2)
}





# Prueba num aleatorio ----------------------------------------------------
vec_s_sem_k_info <- gen_vec_s_sem_k_info(param_sim$vec_sem_sig,
                                         param_sim$k_sem_ant,param)
materia <- "Probabilidad I"
param_sim$Materias = materia
param_sim$m_filtrada <- gen_mat_m_filtrada(param,param_sim)
mat_alumnos_corregidos <- gen_mat_alumnos_corregidos(vec_s_sem_k_info,
                                                     param,param_sim)
set.seed(1806)
(d_prima <- simula_alumnos(mat_alumnos_corregidos,param))

wait = c(as.vector(mat_alumnos_corregidos),d_prima)
mixmdl = normalmixEM(wait,mean = mean(wait))

mixmdl$mu #The final mean parameters
mixmdl$sigma #The final standard deviations. If arbmean = FALSE,
#then only the smallest standard deviation is returned.
mixmdl$loglik#Último valor de la máxima verosimilitud


#' Con el siguiente ciclo se pretende obtener una nueva d_prima
#' con el número de alumnos simulados para cada hora, dependiendo
#' de si faltan o sobran alumnos.
d_prima_2 <- rep(0,length(d_prima))
# falta_alum <- 1
# sobra_alum <- 1
for(h in 1:length(param$Horas)){
  cat("\n h = ",h)
  (rand_num <- ceiling(rnorm(1,mixmdl$mu,mixmdl$sigma)))
  falta_alum <- round(runif(1,0,1))
  cat("\n falta_alum = ",falta_alum)
  sobra_alum <- 1 - falta_alum
  if(falta_alum == 1){
    while(rand_num <= d_prima[h]){
      (rand_num <- ceiling(rnorm(1,mixmdl$mu,mixmdl$sigma)))
    }
    d_prima_2[h] <- max(0,rand_num)
  }
  if(sobra_alum == 1 && d_prima[h]>0){
    while(rand_num > d_prima[h]){
      (rand_num <- ceiling(rnorm(1,mixmdl$mu,mixmdl$sigma)))
    }
    d_prima_2[h] <- max(0,rand_num)
  }
}#fin for(h)



# Segunda prueba de for ---------------------------------------------------
wait = c(as.vector(mat_alumnos_corregidos),d_prima_2)
mixmdl = normalmixEM(wait,mean = mean(wait))
mixmdl$loglik#-510.7405

wait = c(as.vector(mat_alumnos_corregidos),d_prima_2)
mixmdl = normalmixEM(wait,mean = mixmdl$mu)
mixmdl$loglik#-552.1499/-524.0673/-510.7405




# mixmdl$x #Datos que se le pasaron como parámetro a la función
hist(mixmdl$x,freq = F)
lines(density(rnorm(1000,mean = mixmdl$mu,sd = mixmdl$sigma)), lty=1,
      lwd=2,col = "blue")
lines(density(mixmdl$x), lty=1,lwd=2,col = "green")
legend(300,0.006,c("GMM","density()"),bty = "n",
       col=c("blue","green"),lty=c(1,1),
       cex=1.1,lwd=2)


for(d in 1:3){
  cat("\n d = ",d)
  set.seed(1806+d)
  (d_prima <- simula_alumnos(mat_alumnos_corregidos,param))
  
  wait = c(wait,d_prima)#Actualizamos los datos
  
  mixmdl = normalmixEM(wait,mu = unique(mixmdl$mu))
  # mixmdl = normalmixEM(wait,mu = mixmdl$mu)
  # mixmdl = normalmixEM(wait,lambda = mixmdl$lambda, mu = mixmdl$mu,
  #                      sigma = mixmdl$sigma)
  # mixmdl = normalmixEM(wait,mu = mixmdl$mu,sigma = mixmdl$sigma)
  # mixmdl = normalmixEM(wait,mu = mixmdl$mu, k = length(mixmdl$mu))
  # mixmdl = normalmixEM(wait,sigma = mixmdl$sigma, k = length(mixmdl$mu))
  cat("\n loglik = ",mixmdl$loglik)
  # mixmdl$x #Datos que se le pasaron como parámetro a la función
  hist(mixmdl$x,freq = F)
  lines(density(rnorm(1000,mean = mixmdl$mu,sd = mixmdl$sigma)), lty=1,
        lwd=2,col = "blue")
  lines(density(mixmdl$x), lty=1,lwd=2,col = "green")
  legend(300,0.006,c("GMM","density()"),bty = "n",
         col=c("blue","green"),lty=c(1,1),
         cex=1.1,lwd=2)
}





# sdfgh -------------------------------------------------------------------

#' Sea X ~ N(-5,4)
mu_X <- -5
sd_X <- sqrt(4)

#' P(X<0) = P(X<=0)
pnorm(0,mu_X,sd_X)#0.9937903
pnorm(((0-mu_X)/sd_X),0,1)#0.9937903

dnorm(0,mu_X,sd_X)#0.00876415
dnorm(((0-mu_X)/sd_X),0,1)#0.0175283
qnorm(0,mu_X,sd_X)#-Inf
qnorm(0.5,mu_X,sd_X)#mu_X
qnorm(1,mu_X,sd_X)#Inf
rnorm(10,mu_X,sd_X)

#' P(???7<X<???3) = P(X<=-3) - P(X<=-7)
pnorm(-3,mu_X,sd_X) - pnorm(-7,mu_X,sd_X)#0.6826895

#' P(X>???3|X>???5) = [1 - P(X<=-3)]/[1 - P(X<=mu_X)]
(1 - pnorm(-3,mu_X,sd_X))/(1 - pnorm(mu_X,mu_X,sd_X))#0.3173105
























