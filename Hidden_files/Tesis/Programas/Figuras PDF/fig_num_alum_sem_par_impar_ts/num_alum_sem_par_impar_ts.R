##########################################################################
#' En este programa se encuentra el código que genera la gráfica del
#' número de alumnos totales de semestres pares e impares.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
# rm(list=ls())  # Borra variables
# cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri")

# Figura ------------------------------------------------------------------
#' Se cargan los datos del semestre 2008-1 al 2020-1 del promedio de
#' alumnos por semestre y de la desviación estándar.
load("Programas/Figuras PDF/fig_num_alum_sem_par_impar_ts/vec_num_total_alum.RData")

# Histograma doble --------------------------------------------------------
min(vec_num_total_alum)#17796
max(vec_num_total_alum)#28497
ind_impar <- seq(from=1,to=length(vec_num_total_alum),by=2)
ind_par <- seq(from=2,to=length(vec_num_total_alum),by=2)

#' Se convierten los datos en serie de tiempo
alum_sem_impar.ts <- ts(vec_num_total_alum[ind_impar],frequency = 2,
                        start = c(2008, 1))
alum_sem_impar.ts
min(alum_sem_impar.ts)#19251
max(alum_sem_impar.ts)#28497

alum_sem_par.ts <- ts(vec_num_total_alum[ind_par],frequency = 1,
                      start = c(2008, 1))
alum_sem_par.ts
min(alum_sem_par.ts)#18759
max(alum_sem_par.ts)#26156

alum_sem_par_impar.ts <- ts(vec_num_total_alum,frequency = 2,
                            start = c(2008, 1))
alum_sem_par_impar.ts
min(alum_sem_par_impar.ts)#17796
max(alum_sem_par_impar.ts)#28497


## Se grafica la serie de tiempo
semestres <- (20081:20201)[(20081:20201)%% 10>0 &(20081:20201)%% 10<3]
sem_impar <- semestres[ind_impar]
sem_par <- semestres[ind_par]

plot(semestres,alum_sem_par_impar.ts,ylim=c(15000,30000),
     main = "Número de alumnos de semestres pares e impares",
     type="n",xlab="Semestres",ylab="Número de alumnos")

points(sem_impar, alum_sem_impar.ts, col=param_graficas$col1_linea,pch="o")
lines(sem_impar, alum_sem_impar.ts, col=param_graficas$col1_linea)
sem_par_aux <- c("20085","20095","20105","20115","20125","20135","20145",
                 "20155","20165","20175","20185","20195")
points(sem_par_aux, alum_sem_par.ts, col=param_graficas$col2_linea,pch="o")
lines(sem_par_aux, alum_sem_par.ts, col=param_graficas$col2_linea)

legend(20150,23000,c("Semestres impares",
                   "Semestres pares"),bty = "n",
       col=c(param_graficas$col1_linea,param_graficas$col2_linea),
       pch=c("o","o"),lty=c(1,1),cex=1.1)

# Se guarda la imagen -----------------------------------------------------
nom_plot <- "num_alum_sem_par_impar_ts.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)


