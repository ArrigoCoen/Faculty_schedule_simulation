##########################################################################
#' En este programa se encuentra el código que genera la gráfica del
#' número de alumnos de semestres pares e impares.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
# rm(list=ls())  # Borra variables
# cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri")


# Figura ------------------------------------------------------------------
#' Se cargan los datos
load("Programas/Figuras PDF/fig_num_alum_sem_par_impar_Proba_I/mat_alum_sem_impar_proba_I.Rdata")
load("Programas/Figuras PDF/fig_num_alum_sem_par_impar_Proba_I/mat_alum_sem_par_proba_I.Rdata")
load("Programas/Figuras PDF/fig_num_alum_sem_par_impar_Proba_I/mat_alum_x_sem_proba_I.Rdata")

#' Se convierten los datos en serie de tiempo
alum_sem_impar.ts <- ts(mat_alum_sem_impar_proba_I[,2],frequency = 1,
                        start = c(2015, 1))
alum_sem_impar.ts
min(alum_sem_impar.ts)#682
max(alum_sem_impar.ts)#944

alum_sem_par.ts <- ts(mat_alum_sem_par_proba_I[,2],frequency = 1,
                      start = c(2015, 1))
alum_sem_par.ts
min(alum_sem_par.ts)#366
max(alum_sem_par.ts)#528

alum_sem_par_impar.ts <- ts(mat_alum_x_sem_proba_I[,2],frequency = 2,
                            start = c(2015, 1))
alum_sem_par_impar.ts
min(alum_sem_par_impar.ts)#366
max(alum_sem_par_impar.ts)#944


## Se grafica la serie de tiempo
semestres <- mat_alum_x_sem_proba_I[,1]
sem_impar <- mat_alum_sem_impar_proba_I[,1]
sem_par <- mat_alum_sem_par_proba_I[,1]


# plot --------------------------------------------------------------------
plot(semestres,alum_sem_par_impar.ts,ylim=c(0,1000),main = "Probabilidad I",
     type="n",xlab="Semestres",ylab="Número de alumnos")

points(sem_impar, alum_sem_impar.ts, col=param_graficas$col1_linea,pch="o")
lines(sem_impar, alum_sem_impar.ts, col=param_graficas$col1_linea)
sem_par_aux <- c("20155","20165","20175","20185","20195")
points(sem_par_aux, alum_sem_par.ts, col=param_graficas$col2_linea,pch="o")
lines(sem_par_aux, alum_sem_par.ts, col=param_graficas$col2_linea)

legend(20168,400,c("Número de alumnos de semestres impares",
                   "Número de alumnos de semestres pares"),bty = "n",
       col=c(param_graficas$col1_linea,param_graficas$col2_linea),
       pch=c("o","o"),lty=c(1,1),cex=1.1)


# Se guarda la imagen -----------------------------------------------------
nom_plot <- "num_alum_sem_par_impar_Proba_I.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)
