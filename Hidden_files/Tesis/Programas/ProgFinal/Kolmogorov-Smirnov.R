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
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")

#' Se cargan los datos del semestre 2008-1 al 2020-1 del número de alumnos
#' por grupo.
load("vec_al_x_gpo_todos_sem.RData")

# Pruebas Kolmogorov-Smirnov ----------------------------------------------

##Se rechaza cuando Dn >= Dn_alpha
#alpha = 0.01
(Dn_alpha <- 1.63/sqrt(n))#0.01241205

X <- vec_al_x_gpo_todos_sem
n <- length(X)#17246

### Poisson
EMV_lambda <- mean(X)#34.18746
y <- rpois(n,EMV_lambda)
# Do x and y come from the same distribution?
ks.test(X, y)#Dn = 0.4005
#' Dn = 0.4005 > 0.01241205 = Dn_alpha por lo tanto
#' se rechaza H0, por lo tanto los datos no siguen una distribución
#' Poisson con lambda = 34.18746

### Exponencial
fitdistr(X, densfun="exponential")
y <- rexp(n,0.0292504880 )
# Do x and y come from the same distribution?
ks.test(X, y)#Dn = 0.11701
#' Dn = 0.11701 > 0.01241205 = Dn_alpha por lo tanto
#' se rechaza H0, por lo tanto los datos no siguen una distribución
#' Exponencial con lambda = 0.0292504880

### Geométrica
fitdistr(X, densfun="geometric")#numAL
y <- rgeom(n,0.0284192122 )
# Do x and y come from the same distribution?
ks.test(X, y)#Dn = 0.10646
#' Dn = 0.10646 > 0.01241205 = Dn_alpha por lo tanto
#' se rechaza H0, por lo tanto los datos no siguen una distribución
#' Geométrica con p = 0.0284192122

### Binomial negativa
fitdistr(X, densfun="negative binomial")
y <- rnbinom(n,size = 1.80547812,mu=34.18745219)
# Do x and y come from the same distribution?
ks.test(X, y)#Dn = 0.019599
#' Dn = 0.019599 > 0.01241205 = Dn_alpha por lo tanto
#' se rechaza H0, por lo tanto los datos no siguen una distribución
#' Binomial negativa con size = 1.80547812 y mu = 34.18745219

### Normal
fitdistr(X, densfun="normal")
y <- rnorm(n,34.18745219,26.5768345)
# Do x and y come from the same distribution?
ks.test(X, y)#Dn = 0.10501
#' Dn = 0.10501 > 0.01241205 = Dn_alpha => NO se rechaza H0,
#' por lo tanto los datos siguen una distribución
#' Normal con mu = 34.18745219 y sd = 26.5768345


# Histograma --------------------------------------------------------------
#' La distribución que se ajusta a los datos es una
#' Normal(mu = 34.18745219, sd = 26.5768345). Graficamos el histograma con
#' las frecuencias relativas de los datos. La línea azul es la densidad
#' ajustada generada por R y la línea roja es la densidad de "n" números
#' aleatorios con distribución  Normal(34.18745219,26.5768345).
hist(X,col=param_graficas$col1_hist,breaks = seq(0,360,by = 10),
     ylab = "Frecuencia relativa",freq = F,ylim = c(0,0.025),
     main="Histograma del número de alumnos",xlab = "Número alumnos")
lines(density(X),col=param_graficas$col1_linea,
      lwd=param_graficas$lwd_dens)
lines(density(rnorm(n,34.18745219,26.5768345)),col=param_graficas$col2_linea,
      lwd=param_graficas$lwd_dens)
