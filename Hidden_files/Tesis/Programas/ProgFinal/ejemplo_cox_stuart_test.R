##########################################################################
#' En este programa se encuentran ejemplos de la función cox.stuart.test()
#' que prueba la aleatoriedad de los datos dados.
#' https://www.rdocumentation.org/packages/randtests/versions/1.0/topics/cox.stuart.test
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola

# install.packages("randtests")
# install.packages('stats')
library(stats)
library(randtests)

# Ej. internet ------------------------------------------------------------
#https://www.rdocumentation.org/packages/randtests/versions/1.0/topics/cox.stuart.test
## Conover (1999)
## The total annual precipitation recorded each year, for 19 years.
##
precipitation <- c(45.25, 45.83, 41.77, 36.26, 45.37, 52.25, 35.37, 57.16, 35.37, 58.32, 
                   41.05, 33.72, 45.73, 37.90, 41.72, 36.07, 49.83, 36.24, 39.90)
plot(precipitation)
cox.stuart.test(precipitation)

##
## Example 2
## Sweet potato production, harvested in the United States, between 1868 and 1937.
##
data(sweetpotato)
plot(sweetpotato$production)
cox.stuart.test(sweetpotato$production)


# Ej. datos tesis ---------------------------------------------------------
#' Se cargan los datos del semestre 2008-1 al 2020-1 del promedio de
#' alumnos por semestre
load("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/Figuras PDF/fig_prom_alum_total_x_sem_ts/vec_prom_total_alum.RData")
vec_prom_total_alum

plot(vec_prom_total_alum)
cox.stuart.test(vec_prom_total_alum)

prueba <- cox.stuart.test(vec_prom_total_alum)
prueba$statistic
prueba$alternative
prueba$p.value#0.0004882812
prueba$parameter
prueba$data.name

alfa <- 0.01

#Como
prueba$p.value < alfa
#' entonces se rechaza H0, por lo tanto la muestra no proviene de
#' datos aleatorios, tiene una tendencia.

#Gráfica que muestra que la tendencia de los datos es lineal y creciente
plot(vec_prom_total_alum)
t <- 1:25
abline(lm(vec_prom_total_alum~t),col = "red")
