##########################################################################
#' En este programa se encuentra el código que genera la gráfica del
#' número de alumnos por grupo del turno matutino y vespertino.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
# rm(list=ls())  # Borra variables
# cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis")


# Figura ------------------------------------------------------------------
#' Se cargan los datos
load("Programas/Figuras PDF/fig_num_alum_x_turno_Proba_I/mat_alum_matu_vesp_proba_I.Rdata")

#' Se convierten los datos en serie de tiempo
alum_matutino.ts <- ts(mat_alum_matu_vesp_proba_I[,2],frequency = 2, start = c(2015, 1))
alum_matutino.ts
min(alum_matutino.ts)#233
max(alum_matutino.ts)#706

alum_vespertino.ts <- ts(mat_alum_matu_vesp_proba_I[,3],frequency = 2, start = c(2015, 1))
alum_vespertino.ts
min(alum_vespertino.ts)#72
max(alum_vespertino.ts)#255

## Se define un vector auxiliar para tener bien los semestres
semestres_aux <- sem_par_aux <- c("20150","20155",
                                  "20160","20165",
                                  "20170","20175",
                                  "20180","20185",
                                  "20190","20195","20200")


# plot --------------------------------------------------------------------
plot(semestres_aux,alum_matutino.ts,ylim=c(-100,750),main = "Probabilidad I",
     type="o",xlab="Semestres",ylab="Número de alumnos",axes = FALSE,
     col=param_graficas$col1_linea,pch="o")
points(semestres_aux, alum_vespertino.ts, col=param_graficas$col2_linea,pch="o")
lines(semestres_aux, alum_vespertino.ts, col=param_graficas$col2_linea)
axis(side=1, at = c(20150,20160,20170,20180,20190,20200),
     labels=c("2015-1","2016-1","2017-1","2018-1","2019-1","2020-1"))
axis(2)
box() #- To make it look like "usual" plot
legend(20155,120,c("Número de alumnos en el turno matutino",
                   "Número de alumnos en el turno vespertino"),bty = "n",
       col=c(param_graficas$col1_linea,param_graficas$col2_linea),
       pch=c("o","o"),lty=c(1,1),#cex=1.1
       )


# Se guarda la imagen -----------------------------------------------------
nom_plot <- "num_alum_x_turno_Proba_I.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)
