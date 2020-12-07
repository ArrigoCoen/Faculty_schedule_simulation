##########################################################################
#' En este programa se encuentran ejemplos de las funciones bptest() y
#' jarque.bera.test() que prueban la heterocedasticidad y la normalidad de
#' los residuales de los datos dados, respectivamente.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis")


# Se cargan los datos -----------------------------------------------------
#' Los datos del semestre 2008-1 al 2020-1 del número total de
#' alumnos por semestre
load("Programas/Figuras PDF/fig_descomposicion_ts_total_alumnos/vec_num_total_alum.RData")

#' Se convierten los datos en serie de tiempo
num_total_alum.ts <- ts(vec_num_total_alum,frequency = 2, start = c(2008, 1))
num_total_alum.ts

#' Se descompone la serie
num_total_alum.Comp <- decompose(num_total_alum.ts)
num_total_alum.Comp
# num_total_alum.Comp$random


#' Now lets plot the components.
plot(num_total_alum.Comp)


# Prueba de Normalidad ----------------------------------------------------
# install.packages('tseries')
# install.packages('lmtest')
# install.packages('het.test')
library(tseries)
library(lmtest)
library(het.test)

datos <- num_total_alum.Comp$random[!is.na(num_total_alum.Comp$random)]
jarque.bera.test(datos)#p-value = 0.3297 > 0.01 = alfa => NO se rechaza H0
#' por lo tanto los residuales provienen de una distribución normal

# x <- rnorm(100)  # null
# jarque.bera.test(x)
# 
# x <- runif(100)  # alternative
# jarque.bera.test(x)



# heterocedasticidad ------------------------------------------------------
t <- 2:24#"Tiempo" CHECAR SI ESTÁ BIEN LA PRUEBA
bptest(lm(datos~t))#p-value = 0.2561 > 0.01 = alfa => NO se rechaza H0
#' por lo tanto la varianza de los residuales es constante







