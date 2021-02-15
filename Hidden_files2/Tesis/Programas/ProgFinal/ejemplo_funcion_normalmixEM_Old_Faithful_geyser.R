##########################################################################
#' En este programa se encuentra el código con el ejemplo del modelo con
#' mezcla de Normales. El ejemplo mostrado es para el uso de la función:
#' normalmixEM
#' https://www.r-bloggers.com/2011/08/fitting-mixture-distributions-with-the-r-package-mixtools/
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")



# Ejemplo -----------------------------------------------------------------

wait = faithful$waiting
mixmdl = normalmixEM(wait)
# plot(mixmdl,which=2)
plot(mixmdl,which = 2)
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



rnorm(1,mean = mixmdl$mu, sd = mixmdl$sigma)
#' NOTA: mixmdl$mu y mixmdl$sigma son VECTORES





# Analyzing the Old Faithful geyser ---------------------------------------
#' Data with a 2-component mixture of normals.
#' https://www.rdocumentation.org/packages/mixtools/versions/1.0.4/topics/normalmixEM

data(faithful)
attach(faithful)
set.seed(100)
system.time(out<-normalmixEM(waiting, arbvar = FALSE, epsilon = 1e-03))
out
set.seed(100)
system.time(out2<-normalmixEM(waiting, arbvar = FALSE, epsilon = 1e-03, fast=TRUE))
out2 # same thing but much faster