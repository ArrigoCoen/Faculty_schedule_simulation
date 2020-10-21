##########################################################################
#' En este programa se encuentra el código que genera la gráfica de barras
#' del promedio de alumnos por hora.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
# rm(list=ls())  # Borra variables
# cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis")

#' Se cargan los datos
load("Programas/Figuras PDF/fig_prom_alum_x_hora_barplot/mat_num_alum_x_hora.RData")

# Figura ------------------------------------------------------------------
vec_prom_alum_x_hora <- as.numeric(mat_num_alum_x_hora[,2])/25
min(vec_prom_alum_x_hora)#239.12
max(vec_prom_alum_x_hora)#3586.24

barplot(vec_prom_alum_x_hora,main = "Promedio de alumnos por hora",
        ylim = c(0,3800),xlab="Horario",ylab="Número de alumnos",
        col = param_graficas$col_barras,
        axis.lty=1,las=2,cex.axis=0.9,#cex.names = 0.9,
        names.arg=c("7-8","8-9","9-10","10-11","11-12",
                    "12-13","13-14","14-15","15-16",
                    "16-17","17-18","18-19","19-20",
                    "20-21","21-22"))

# Se guarda la imagen -----------------------------------------------------
nom_plot <- "prom_alum_x_hora_barplot.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)
