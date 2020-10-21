##########################################################################
#' En este programa se encuentra un ejemplo de la obtención de los valores
#' de la prueba de Kolmogorov-Smirnov.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Programas/ProgFinal V01")




# Sn(xi) ------------------------------------------------------------------
Sn <- function(i,xi,X){
  #Se definen las variables que se van a utilizar
  n <- length(X)
  
  if(i <= 0){
    Sn_x <- 0
  }else{
    if(xi < X[1]){
      Sn_x <- 0
    }else if(xi >= X[n]){
      Sn_x <- 1
    }else{##i = 1,2,...,n-1
      for(i in 1:(n-1)){
        if(xi>=X[i] && xi<X[i+1]){
          Sn_x <- i/n
        }
      }
    }
  }

  return(Sn_x)
}



# F(xi) --------------------------------------------------------------------
F_x <- function(i,xi,EMV_lambda){
  if(i <= 0){
    F_xi <- 0
  }else{
    F_xi <- sum(dpois(x = 0:xi, lambda = EMV_lambda))
  }
  
  return(F_xi)
}




# Kolmogorov-Smirnov ------------------------------------------------------

# Poisson(97.7619) --------------------------------------------------------
#' Se cargan los datos del semestre 2008-1 al 2020-1 del número de alumnos
#' por grupo.
load("vec_al_x_gpo_todos_sem.RData")

##Se definen las variables que se van a utilizar
datos <- as.data.frame(table(vec_al_x_gpo_todos_sem))
frec_datos <- data.frame(NumAlum = as.numeric(as.character(datos$vec_al_x_gpo_todos_sem)),
                         Freq = datos$Freq)
X <- frec_datos[,1]
tabla_KS <- matrix(0,nrow = length(X),ncol = 6)
Sn_x <- rep(0,length(X))
EMV_lambda <- mean(frec_datos[,1])#97.7619
n <- sum(frec_datos[,2])#17246

tabla_KS[,1] <- frec_datos[,1]
tabla_KS[,2] <- frec_datos[,2]

for(i in 1:length(X)){
  xi <- X[i]
  tabla_KS[i,3] <- Sn(i,xi,X)
  tabla_KS[i,4] <- F_x(i,xi,EMV_lambda)
  tabla_KS[i,5] <- abs(F_x(i,xi,EMV_lambda)-Sn(i-1,X[i-1],X))
  tabla_KS[i,6] <- abs(F_x(i,xi,EMV_lambda)-Sn(i,xi,X))
}
colnames(tabla_KS) <- c("NumAlum","Freq","Sn","Fx","Fi-Si-1","Fi-Si")
View(tabla_KS)

##Se rechaza cuando Dn >= Dn_alpha
Dn <- max(c(tabla_KS[,5],tabla_KS[,6]))#0.3952665
#alpha = 0.05
Dn_alpha <- 1.36/sqrt(n)#0.01035606
#alpha = 0.01
Dn_alpha <- 1.63/sqrt(n)#0.01241205

#' Dn = 0.3952665 > 0.01241205 = Dn_alpha por lo tanto
#' se rechaza H0, por lo tanto los datos no siguen una distribución
#' Poisson con media 97.7619



# Poisson(34.18746) --------------------------------------------------------
#' Se cargan los datos del semestre 2008-1 al 2020-1 del número de alumnos
#' por grupo.
load("vec_al_x_gpo_todos_sem.RData")

##Se definen las variables que se van a utilizar
datos <- as.data.frame(table(vec_al_x_gpo_todos_sem))
frec_datos <- data.frame(NumAlum = as.numeric(as.character(datos$vec_al_x_gpo_todos_sem)),
                         Freq = datos$Freq)
X <- frec_datos[,1]
tabla_KS <- matrix(0,nrow = length(X),ncol = 6)
Sn_x <- rep(0,length(X))
EMV_lambda <- sum(frec_datos[,1]*frec_datos[,2])/n#34.18746
n <- sum(frec_datos[,2])#17246

tabla_KS[,1] <- frec_datos[,1]
tabla_KS[,2] <- frec_datos[,2]

for(i in 1:length(X)){
  xi <- X[i]
  tabla_KS[i,3] <- Sn(i,xi,X)
  tabla_KS[i,4] <- F_x(i,xi,EMV_lambda)
  tabla_KS[i,5] <- abs(F_x(i,xi,EMV_lambda)-Sn(i-1,X[i-1],X))
  tabla_KS[i,6] <- abs(F_x(i,xi,EMV_lambda)-Sn(i,xi,X))
}
colnames(tabla_KS) <- c("NumAlum","Freq","Sn","Fx","Fi-Si-1","Fi-Si")
View(tabla_KS)

##Se rechaza cuando Dn >= Dn_alpha
(Dn <- max(c(tabla_KS[,5],tabla_KS[,6])))#0.7365395
#alpha = 0.05
(Dn_alpha <- 1.36/sqrt(n))#0.01035606
#alpha = 0.01
(Dn_alpha <- 1.63/sqrt(n))#0.01241205

#' Dn = 0.7365395 > 0.01241205 = Dn_alpha por lo tanto
#' se rechaza H0, por lo tanto los datos no siguen una distribución
#' Poisson con media 34.18746



# F_x Normal --------------------------------------------------------------
F_x <- function(i,xi,EMV_mu,EMV_sigma){
  if(i <= 0){
    F_xi <- 0
  }else{
    F_xi <- pnorm(xi,EMV_mu,EMV_sigma)
    # dnorm(xi,EMV_mu,EMV_sigma)
    # dnorm(frec_datos[,2],mean = 97.761905,sd = 61.530922)
    # sum(dpois(x = 0:xi, lambda = EMV_lambda))
  }
  
  return(F_xi)
}



# Normal(97.761905,61.530922) ----------------------------------------------------
#' Se cargan los datos del semestre 2008-1 al 2020-1 del número de alumnos
#' por grupo.
load("vec_al_x_gpo_todos_sem.RData")

##Se definen las variables que se van a utilizar
datos <- as.data.frame(table(vec_al_x_gpo_todos_sem))
frec_datos <- data.frame(NumAlum = as.numeric(as.character(datos$vec_al_x_gpo_todos_sem)),
                         Freq = datos$Freq)
X <- frec_datos[,1]
tabla_KS <- matrix(0,nrow = length(X),ncol = 6)
Sn_x <- rep(0,length(X))

fitdistr(frec_datos[,1], densfun="normal")#numAL
# fitdistr(frec_datos[,2], densfun="normal")#freq
EMV_mu <- 97.761905
EMV_sigma <- 61.530922
# mean = 91.248677,sd = 119.878892

n <- sum(frec_datos[,2])#17246

tabla_KS[,1] <- frec_datos[,1]
tabla_KS[,2] <- frec_datos[,2]

for(i in 1:length(X)){
  xi <- X[i]
  tabla_KS[i,3] <- Sn(i,xi,X)
  tabla_KS[i,4] <- F_x(i,xi,EMV_mu,EMV_sigma)
  tabla_KS[i,5] <- abs(F_x(i,xi,EMV_mu,EMV_sigma)-Sn(i-1,X[i-1],X))
  tabla_KS[i,6] <- abs(F_x(i,xi,EMV_mu,EMV_sigma)-Sn(i,xi,X))
}
colnames(tabla_KS) <- c("NumAlum","Freq","Sn","Fx","Fi-Si-1","Fi-Si")
View(tabla_KS)

##Se rechaza cuando Dn >= Dn_alpha
(Dn <- max(c(tabla_KS[,5],tabla_KS[,6])))#0.05604988
#alpha = 0.05
(Dn_alpha <- 1.36/sqrt(n))#0.01035606
#alpha = 0.01
(Dn_alpha <- 1.63/sqrt(n))#0.01241205

#' Dn = 0.05604988 > 0.01241205 = Dn_alpha por lo tanto
#' se rechaza H0, por lo tanto los datos no siguen una distribución
#' Normal con mu 97.761905 y sd 61.530922


# Normal(91.248677,119.878892) ----------------------------------------------------
#' Se cargan los datos del semestre 2008-1 al 2020-1 del número de alumnos
#' por grupo.
load("vec_al_x_gpo_todos_sem.RData")

##Se definen las variables que se van a utilizar
datos <- as.data.frame(table(vec_al_x_gpo_todos_sem))
frec_datos <- data.frame(NumAlum = as.numeric(as.character(datos$vec_al_x_gpo_todos_sem)),
                         Freq = datos$Freq)
X <- frec_datos[,1]
tabla_KS <- matrix(0,nrow = length(X),ncol = 6)
Sn_x <- rep(0,length(X))

# fitdistr(frec_datos[,1], densfun="normal")#numAL
fitdistr(frec_datos[,2], densfun="normal")#freq
EMV_mu <- 91.248677
EMV_sigma <- 119.878892

n <- sum(frec_datos[,2])#17246

tabla_KS[,1] <- frec_datos[,1]
tabla_KS[,2] <- frec_datos[,2]

for(i in 1:length(X)){
  xi <- X[i]
  tabla_KS[i,3] <- Sn(i,xi,X)
  tabla_KS[i,4] <- F_x(i,xi,EMV_mu,EMV_sigma)
  tabla_KS[i,5] <- abs(F_x(i,xi,EMV_mu,EMV_sigma)-Sn(i-1,X[i-1],X))
  tabla_KS[i,6] <- abs(F_x(i,xi,EMV_mu,EMV_sigma)-Sn(i,xi,X))
}
colnames(tabla_KS) <- c("NumAlum","Freq","Sn","Fx","Fi-Si-1","Fi-Si")
# View(tabla_KS)

##Se rechaza cuando Dn >= Dn_alpha
(Dn <- max(c(tabla_KS[,5],tabla_KS[,6])))#0.2232766
#alpha = 0.05
(Dn_alpha <- 1.36/sqrt(n))#0.01035606
#alpha = 0.01
(Dn_alpha <- 1.63/sqrt(n))#0.01241205

#' Dn = 0.2232766 > 0.01241205 = Dn_alpha por lo tanto
#' se rechaza H0, por lo tanto los datos no siguen una distribución
#' Normal con mu 91.248677 y sd 119.878892


# F_x Geom --------------------------------------------------------------
F_x <- function(i,xi,EMV_p){
  if(i <= 0){
    F_xi <- 0
  }else{
    F_xi <- pgeom(xi,EMV_p)
  }
  
  return(F_xi)
}



# Geom(0.0101253616) ------------------------------------------------------
#' Se cargan los datos del semestre 2008-1 al 2020-1 del número de alumnos
#' por grupo.
load("vec_al_x_gpo_todos_sem.RData")

##Se definen las variables que se van a utilizar
datos <- as.data.frame(table(vec_al_x_gpo_todos_sem))
frec_datos <- data.frame(NumAlum = as.numeric(as.character(datos$vec_al_x_gpo_todos_sem)),
                         Freq = datos$Freq)
X <- frec_datos[,1]
tabla_KS <- matrix(0,nrow = length(X),ncol = 6)
Sn_x <- rep(0,length(X))

fitdistr(frec_datos[,1], densfun="geometric")#numAL
EMV_p <- 0.0101253616
# fitdistr(frec_datos[,2], densfun="geometric")#freq
# EMV_p <- 0.0108402638 


n <- sum(frec_datos[,2])#17246

tabla_KS[,1] <- frec_datos[,1]
tabla_KS[,2] <- frec_datos[,2]

for(i in 1:length(X)){
  xi <- X[i]
  tabla_KS[i,3] <- Sn(i,xi,X)
  tabla_KS[i,4] <- F_x(i,xi,EMV_p)
  tabla_KS[i,5] <- abs(F_x(i,xi,EMV_p)-Sn(i-1,X[i-1],X))
  tabla_KS[i,6] <- abs(F_x(i,xi,EMV_p)-Sn(i,xi,X))
}
colnames(tabla_KS) <- c("NumAlum","Freq","Sn","Fx","Fi-Si-1","Fi-Si")
# View(tabla_KS)

##Se rechaza cuando Dn >= Dn_alpha
(Dn <- max(c(tabla_KS[,5],tabla_KS[,6])))#0.1453128
#alpha = 0.05
(Dn_alpha <- 1.36/sqrt(n))#0.01035606
#alpha = 0.01
(Dn_alpha <- 1.63/sqrt(n))#0.01241205

#' Dn = 0.1453128 > 0.01241205 = Dn_alpha por lo tanto
#' se rechaza H0, por lo tanto los datos no siguen una distribución
#' Normal con p = 0.0101253616


# Geom(0.0108402638) ------------------------------------------------------
#' Se cargan los datos del semestre 2008-1 al 2020-1 del número de alumnos
#' por grupo.
load("vec_al_x_gpo_todos_sem.RData")

##Se definen las variables que se van a utilizar
datos <- as.data.frame(table(vec_al_x_gpo_todos_sem))
frec_datos <- data.frame(NumAlum = as.numeric(as.character(datos$vec_al_x_gpo_todos_sem)),
                         Freq = datos$Freq)
X <- frec_datos[,1]
tabla_KS <- matrix(0,nrow = length(X),ncol = 6)
Sn_x <- rep(0,length(X))

fitdistr(frec_datos[,2], densfun="geometric")#freq
EMV_p <- 0.0108402638


n <- sum(frec_datos[,2])#17246

tabla_KS[,1] <- frec_datos[,1]
tabla_KS[,2] <- frec_datos[,2]

for(i in 1:length(X)){
  xi <- X[i]
  tabla_KS[i,3] <- Sn(i,xi,X)
  tabla_KS[i,4] <- F_x(i,xi,EMV_p)
  tabla_KS[i,5] <- abs(F_x(i,xi,EMV_p)-Sn(i-1,X[i-1],X))
  tabla_KS[i,6] <- abs(F_x(i,xi,EMV_p)-Sn(i,xi,X))
}
colnames(tabla_KS) <- c("NumAlum","Freq","Sn","Fx","Fi-Si-1","Fi-Si")
# View(tabla_KS)

##Se rechaza cuando Dn >= Dn_alpha
(Dn <- max(c(tabla_KS[,5],tabla_KS[,6])))#0.1690225
#alpha = 0.05
(Dn_alpha <- 1.36/sqrt(n))#0.01035606
#alpha = 0.01
(Dn_alpha <- 1.63/sqrt(n))#0.01241205

#' Dn = 0.1690225 > 0.01241205 = Dn_alpha por lo tanto
#' se rechaza H0, por lo tanto los datos no siguen una distribución
#' Normal con p = 0.0108402638



# ks.test() ---------------------------------------------------------------
#' Se cargan los datos del semestre 2008-1 al 2020-1 del número de alumnos
#' por grupo.
load("vec_al_x_gpo_todos_sem.RData")

##Se definen las variables que se van a utilizar
datos <- as.data.frame(table(vec_al_x_gpo_todos_sem))
frec_datos <- data.frame(NumAlum = as.numeric(as.character(datos$vec_al_x_gpo_todos_sem)),
                         Freq = datos$Freq)
X <- frec_datos[,1]
n <- sum(frec_datos[,2])#17246
y <- rpois(n,EMV_lambda)
# Do x and y come from the same distribution?
ks.test(X, y)

y <- runif(n)#Unif(0,1)
# Do x and y come from the same distribution?
ks.test(X, y)

fitdistr(frec_datos[,1], densfun="exponential")#numAL
y <- rexp(n,0.0102289333)
# Do x and y come from the same distribution?
ks.test(X, y)#Dn = 0.14876

fitdistr(frec_datos[,1], densfun="cauchy")#numAL
y <- rcauchy(n,91.860712,41.899013)
# Do x and y come from the same distribution?
ks.test(X, y)#Dn = 0.13534

fitdistr(frec_datos[,1], densfun="geometric")#numAL
y <- rgeom(n,0.0101253616)
# Do x and y come from the same distribution?
ks.test(X, y)#Dn = 0.14098

fitdistr(frec_datos[,1], densfun="logistic")#numAL
y <- rlogis(n,95.479765,36.006770)
# Do x and y come from the same distribution?
ks.test(X, y)#Dn = 0.065001


fitdistr(frec_datos[,1], densfun="negative binomial")#numAL
y <- rnbinom(n,size = 1.7455436,mu=97.7619048)
# Do x and y come from the same distribution?
ks.test(X, y)#Dn = 0.08091

fitdistr(frec_datos[,1], densfun="normal")#numAL
y <- rnorm(n,97.761905,61.530922)
# Do x and y come from the same distribution?
ks.test(X, y)#Dn = 0.055781
#' Dn = 0.055781 > 0.01241205 = Dn_alpha por lo tanto
#' se rechaza H0, por lo tanto los datos no siguen una distribución
#' Normal con mu = 97.761905 y sd = 61.530922


fitdistr(frec_datos[,1], densfun="Poisson")#numAL
y <- rpois(n,97.761905)
# Do x and y come from the same distribution?
ks.test(X, y)#Dn = 0.39525

fitdistr(frec_datos[,1], densfun="t")#numAL
y <- rt(n,df=46.925867)
# Do x and y come from the same distribution?
ks.test(X, y)#Dn = 0.98117


y <- round(runif(n,0,100))#Unif(0,1)
# Do x and y come from the same distribution?
ks.test(X, y)#Dn = 0.46591

y <- runif(n,0,100)#Unif(0,1)
# Do x and y come from the same distribution?
ks.test(X, y)#Dn = 0.4709

y <- runif(n,10,80)#Unif(0,1)
# Do x and y come from the same distribution?
ks.test(X, y)#Dn = 0.57672

y <- runif(n,10,100)#Unif(0,1)
# Do x and y come from the same distribution?
ks.test(X, y)#Dn = 0.4709

y <- runif(n,0,360)#Unif(0,1)
# Do x and y come from the same distribution?
ks.test(X, y)#Dn = 0.4064
