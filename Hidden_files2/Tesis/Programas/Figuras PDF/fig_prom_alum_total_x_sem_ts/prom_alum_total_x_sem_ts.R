##########################################################################
#' En este programa se encuentra el código que genera la gráfica del
#' promedio del número total de alumnos por semestre.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
# rm(list=ls())  # Borra variables
# cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis")

#' Se cargan los datos del semestre 2008-1 al 2020-1 del promedio de
#' alumnos por semestre
load("Programas/Figuras PDF/fig_prom_alum_total_x_sem_ts/vec_prom_total_alum.RData")

# Figura ------------------------------------------------------------------
#' Se convierten los datos en serie de tiempo
prom_total_alum.ts <- ts(vec_prom_total_alum,frequency = 2, start = c(2008, 1))
prom_total_alum.ts
min(prom_total_alum.ts)#711.84
max(prom_total_alum.ts)#1139.88

## Se grafica la serie de tiempo
par(mfrow=c(1,2),cex=1) # set the plotting area into a 1*2 array
plot.ts(prom_total_alum.ts,main = "Media de alumnos por semestre",
        type = "l",axes = F,
        ylim = c(600,1200),xlab="Semestres",ylab="Promedio de alumnos")

axis(side=1, at = c(2008,2010,2012,2014,2016,2018,2020),
     labels=c("2018-1","2010-1","2012-1","2014-1","2016-1","2018-1","2020-1"))
axis(2)
box() #- To make it look like "usual" plot



#Gráfica que muestra que la tendencia de los datos es lineal y creciente
plot(vec_prom_total_alum,ylim=c(600,1200),
     main = "Media de alumnos por semestre",axes = F,
     type="p",xlab="Semestres",ylab="Promedio de alumnos")
t <- 1:25
abline(lm(vec_prom_total_alum~t),col = param_graficas$col2_linea)

axis(side=1, at = seq(from = 1, to = 25, by = 2),
     labels=c("2008-1","2009-1","2010-1","2011-1","2012-1","2013-1","2014-1",
              "2015-1","2016-1","2017-1","2018-1","2019-1","2020-1"))
axis(2)
box() #- To make it look like "usual" plot

legend(1,700,"Ajuste de tendencia lineal",bty = "n",
       col=param_graficas$col2_linea,
       lty=1)#,cex=1.1


# Se guarda la imagen -----------------------------------------------------
nom_plot <- "prom_alum_total_x_sem_ts.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)
