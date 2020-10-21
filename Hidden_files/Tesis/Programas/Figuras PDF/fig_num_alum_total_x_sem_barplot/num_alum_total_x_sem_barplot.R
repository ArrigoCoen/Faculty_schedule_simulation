##########################################################################
#' En este programa se encuentra el código que genera la gráfica de barras
#' del número total de alumnos por semestre de todas las materias.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
# rm(list=ls())  # Borra variables
# cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri")


# Figura ------------------------------------------------------------------
#' Se cargan los datos del semestre 2008-1 al 2020-1 del número total de
#' alumnos por semestre
load("Programas/Figuras PDF/fig_num_alum_total_x_sem_barplot/vec_num_total_alum.RData")

barplot(vec_num_total_alum,main = "Número de alumnos totales",ylim = c(0,30000),
        xlab="Semestres",ylab="Número de alumnos",col = param_graficas$col_barras,
        axis.lty=1,las=2,cex.names = 0.82,cex.axis=0.84,
        names.arg=c("2008-1","","","","2010-1",
                    "","","","2012-1","",
                    "","","2014-1","","",
                    "","2016-1","","","",
                    "2018-1","","","","2020-1"))

# Se guarda la imagen -----------------------------------------------------
nom_plot <- "num_alum_total_x_sem_barplot.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)
