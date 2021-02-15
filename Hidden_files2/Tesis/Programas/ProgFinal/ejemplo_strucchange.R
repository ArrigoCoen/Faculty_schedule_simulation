##########################################################################
#' En este programa se encuentran ejemplos de cómo enocntrar cambios
#' estructurales en los datos.
#' https://www.youtube.com/watch?v=57NecP8FoF0&ab_channel=Lic.LourdesCuellar
#' https://www.youtube.com/watch?v=UHIrD-IPuiI&ab_channel=Lic.LourdesCuellar
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")

install.packages('strucchange')
library(strucchange)

# Cambios estructurales ---------------------------------------------------
load("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/Figuras PDF/fig_descomposicion_ts_total_alumnos/vec_num_total_alum.RData")

#' Se convierten los datos en serie de tiempo
num_total_alum.ts <- ts(vec_num_total_alum,frequency = 2, start = c(2008, 1))
num_total_alum.ts

?Fstats
modelo <- Fstats(num_total_alum.ts~1,from = 0.01)
# modelo <- Fstats(num_total_alum.ts~1)


#' Prueba F de Chao
#' H0: No hay cambio estructural en la serie
#' H1: Hay cambio estructural en la serie
sctest(modelo)#p-value = 2.22e-16
#' Como p-value < 0.01 = alfa => se rechaza H0, por
#' lo tant hay cambio estructural en la serie


# strucchange::breakpoints(num_total_alum.ts~1)
BP <- strucchange::breakpoints(num_total_alum.ts~1)
BP
summary(BP)
plot(BP)

plot(num_total_alum.ts)
lines(BP)
int_conf <- confint(BP)
lines(int_conf)




# sdfg --------------------------------------------------------------------
# install.packages('aTSA')
# library(aTSA)
# 
# x <- rnorm(100)
# trend.test(x,plot = TRUE) # no trend
# 
# x <- 5*(1:100)/100
# x <- x + arima.sim(list(order = c(1,0,0),ar = 0.4),n = 100)
# trend.test(x,plot = TRUE) # increasing trend
# 
# 
# 
# 
















