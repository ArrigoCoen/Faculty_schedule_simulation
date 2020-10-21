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
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri")


# Figura ------------------------------------------------------------------
#' Se cargan los datos del semestre 2008-1 al 2020-1 del promedio de
#' alumnos por semestre
load("Programas/Figuras PDF/fig_prom_alum_total_x_sem_ts/vec_prom_total_alum.RData")
#' Se convierten los datos en serie de tiempo
prom_total_alum.ts <- ts(vec_prom_total_alum,frequency = 2, start = c(2008, 1))
prom_total_alum.ts
min(prom_total_alum.ts)#711.84
max(prom_total_alum.ts)#1139.88

## Se grafica la serie de tiempo
plot.ts(prom_total_alum.ts,main = "Media de alumnos por semestre",type = "l",
        ylim = c(600,1200),xlab="Semestres",ylab="Número de alumnos")


##Se guarda la imagen
nom_plot <- "prom_alum_total_x_sem_ts.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)
