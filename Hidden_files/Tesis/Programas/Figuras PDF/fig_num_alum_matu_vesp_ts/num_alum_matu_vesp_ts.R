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
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis")

#' Se cargan los datos
load("Programas/Figuras PDF/fig_num_alum_matu_vesp_ts/mat_num_alum_x_sem_matutino.RData")
load("Programas/Figuras PDF/fig_num_alum_matu_vesp_ts/mat_num_alum_x_sem_vespertino.RData")

# Figura ------------------------------------------------------------------
mat_aux_matu <- mat_num_alum_x_sem_matutino[,2:dim(mat_num_alum_x_sem_matutino)[2]]
mat_aux_vesp <- mat_num_alum_x_sem_vespertino[,2:dim(mat_num_alum_x_sem_vespertino)[2]]
#' Se convierten los datos en serie de tiempo
alum_matutino.ts <- ts(colSums(mat_aux_matu),frequency = 2, start = c(2008, 1))
alum_matutino.ts
min(alum_matutino.ts)#13696
max(alum_matutino.ts)#22503

alum_vespertino.ts <- ts(colSums(mat_aux_vesp),frequency = 2, start = c(2008, 1))
alum_vespertino.ts
min(alum_vespertino.ts)#4048
max(alum_vespertino.ts)#10166

## Se define un vector auxiliar para tener bien los semestres
semestres_aux <- sem_par_aux <- c("20080","20085",
                                  "20090","20095",
                                  "20100","20105",
                                  "20110","20115",
                                  "20120","20125",
                                  "20130","20135",
                                  "20140","20145",
                                  "20150","20155",
                                  "20160","20165",
                                  "20170","20175",
                                  "20180","20185",
                                  "20190","20195","20200")
# semestres_aux <- as.character((20081:20201)[(20081:20201)%% 10>0 &(20081:20201)%% 10<3])

plot(semestres_aux,alum_matutino.ts,ylim=c(0,25000),
     main = "Número de alumnos por turno",axes = F,
     type="o",xlab="Semestres",ylab="Número de alumnos",
     col=param_graficas$col1_linea,pch="o")
points(semestres_aux, alum_vespertino.ts, col=param_graficas$col2_linea,pch="o")
lines(semestres_aux, alum_vespertino.ts, col=param_graficas$col2_linea)

axis(side=1, at = c(20080,20090,20100,20110,20120,20130,20140,
                    20150,20160,20170,20180,20190,20200),
     labels=c("2008-1","2009-1","2010-1","2011-1","2012-1","2013-1","2014-1",
              "2015-1","2016-1","2017-1","2018-1","2019-1","2020-1"))
axis(2)
box() #- To make it look like "usual" plot

legend(20163,9000,c("Turno matutino",
                   "Turno vespertino"),bty = "n",
       col=c(param_graficas$col1_linea,param_graficas$col2_linea),
       pch=c("o","o"),lty=c(1,1))#,cex=1.1


# Se guarda la imagen -----------------------------------------------------
nom_plot <- "num_alum_matu_vesp_ts.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)
