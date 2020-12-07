##########################################################################
#' En este programa se encuentran las pruebas del método Holt-Winters.
#' https://www.rdocumentation.org/packages/randtests/versions/1.0/topics/cox.stuart.test
#' https://www.rdocumentation.org/packages/seastests/versions/0.14.2/topics/wo
#' https://www.rdocumentation.org/packages/seastests/versions/0.14.2/topics/isSeasonal
#' https://www.rdocumentation.org/packages/tsoutliers/versions/0.3/topics/jarque.bera.test
#' https://rpubs.com/henriquesantos/560324
#' https://cran.r-project.org/web/packages/olsrr/vignettes/heteroskedasticity.html
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")



# Tendencia ---------------------------------------------------------------
#' Se cargan los datos del semestre 2008-1 al 2020-1 del promedio de
#' alumnos por semestre
load("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/Figuras PDF/fig_prom_alum_total_x_sem_ts/vec_prom_total_alum.RData")
vec_prom_total_alum

plot(vec_prom_total_alum)
cox.stuart.test(vec_prom_total_alum)
cox.stuart.test(vec_prom_total_alum,alternative = "left.sided")
cox.stuart.test(vec_prom_total_alum,alternative = "right.sided")

# summary(cox.stuart.test(vec_prom_total_alum))

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
plot(vec_prom_total_alum,ylim=c(600,1200),
     main = "Media de alumnos por semestre",axes = F,
     type="p",xlab="Semestres",ylab="Número de alumnos")
t <- 1:25
abline(lm(vec_prom_total_alum~t),col = param_graficas$col2_linea)

axis(side=1, at = seq(from = 1, to = 25, by = 2),
     labels=c("2008-1","2009-1","2010-1","2011-1","2012-1","2013-1","2014-1",
              "2015-1","2016-1","2017-1","2018-1","2019-1","2020-1"))
axis(2)
box() #- To make it look like "usual" plot

legend(15,800,"Ajuste de tendencia lineal",bty = "n",
       col=param_graficas$col2_linea,
       lty=1)#,cex=1.1



# Estacionalidad ----------------------------------------------------------
load("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/Figuras PDF/fig_descomposicion_ts_total_alumnos/vec_num_total_alum.RData")

#' Se convierten los datos en serie de tiempo
num_total_alum.ts <- ts(vec_num_total_alum,frequency = 2, start = c(2008, 1))
num_total_alum.ts

alumnos.fit <- hw(num_total_alum.ts,seasonal = "additive")
alumnos.fit$model
alumnos.fit$fitted
alumnos.fit$method
alumnos.fit$series
alumnos.fit$residuals
alumnos.fit$x
alumnos.fit$mean#Media de las estimaciones
alumnos.fit$level#Intervalos de confianza
plot(alumnos.fit)



summary(wo(num_total_alum.ts))#The WO-test does not identify  seasonality
isSeasonal(num_total_alum.ts)#FALSE
summary(wo(alumnos.fit$fitted))#The WO-test does not identify  seasonality
isSeasonal(alumnos.fit$fitted)#FALSE
summary(wo(alumnos.fit$residuals))#The WO-test does not identify  seasonality
isSeasonal(alumnos.fit$residuals)#FALSE

#' Se descompone la serie
num_total_alum.Comp <- decompose(num_total_alum.ts)
num_total_alum.Comp

num_total_alum.Comp$seasonal
num_total_alum.Comp$trend
num_total_alum.Comp$random
num_total_alum.Comp$figure#The estimated seasonal figure only.
summary(wo(num_total_alum.Comp$seasonal))#The WO-test identifies  seasonality
isSeasonal(num_total_alum.Comp$seasonal)#TRUE
isSeasonal(num_total_alum.Comp$trend)#FALSE
isSeasonal(num_total_alum.Comp$random)#FALSE

de_trend <- num_total_alum.Comp$x - num_total_alum.Comp$trend
plot(de_trend)
isSeasonal(de_trend)#FALSE
isSeasonal(ts(de_trend[2:12],frequency = 2,
              start = c(2008, 2)))#FALSE
isSeasonal(ts(de_trend[2:11],frequency = 2,
              start = c(2008, 2)))#TRUE
isSeasonal(ts(de_trend[2:20],frequency = 2,
              start = c(2008, 2)))#FALSE
plot(de_trend[2:11],type = "l")
plot(de_trend[2:20],type = "l")

# Prueba de Normalidad ----------------------------------------------------
num_total_alum.random <- num_total_alum.Comp$random[!is.na(num_total_alum.Comp$random)]
jarque.bera.test(num_total_alum.random)
#' p-value = 0.3297 > 0.01 = alfa => NO se rechaza H0 por lo
#' tanto los residuales provienen de una distribución normal


# Homocedasticidad --------------------------------------------------------
t <- 2:24#"Tiempo" CHECAR SI ESTÁ BIEN LA PRUEBA
bptest(lm(num_total_alum.random~t))

#p-value = 0.2561 > 0.01 = alfa => NO se rechaza H0
#' por lo tanto la varianza de los residuales es constante




