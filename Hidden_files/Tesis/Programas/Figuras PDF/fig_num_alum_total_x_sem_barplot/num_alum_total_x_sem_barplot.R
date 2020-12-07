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
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis")

#' Se cargan los datos del semestre 2008-1 al 2020-1 del número total de
#' alumnos por semestre
load("Programas/Figuras PDF/fig_num_alum_total_x_sem_barplot/vec_num_total_alum.RData")

# Figura ------------------------------------------------------------------
barplot(vec_num_total_alum,main = "Número de alumnos totales",
        ylim = c(0,30000),col = param_graficas$col_barras,
        xlab="Semestres",ylab="Número de alumnos",axes = FALSE,
        axis.lty=1,las=2,#cex.names = 0.82,#expansion factor for *X names*
        cex.axis=0.8,#expansion factor for *Y labels*
        )
pos <- c(0.8,
         3.1,
         5.5,
         7.9,
         10.3,
         12.7,
         15.2,
         17.5,
         19.9,
         22.3,
         24.7,
         27.1,
         29.5)
nombres <- c("2008-1","2009-1","2010-1",
             "2011-1","2012-1",
             "2013-1","2014-1","2015-1",
             "2016-1","2017-1",
             "2018-1","2019-1","2020-1")
axis(side=1, at = pos,labels=nombres, las = 2,
     # cex = 0.2,## Label size
     cex.axis=0.9,
     tck = -0.01,
     ## Adjust the label position.
     mgp = c(3, 0.3, 0))
axis(side=2,las = 2,
     cex.axis=0.8,
     ## Adjust the label position.
     mgp = c(3, 0.5, 0)
     )

# Se guarda la imagen -----------------------------------------------------
nom_plot <- "num_alum_total_x_sem_barplot.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)
