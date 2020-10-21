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
#' Se cargan los datos
load("Programas/Figuras PDF/fig_num_prom_gpos_x_hora_barplot/mat_num_total_gpos_x_hora.RData")

vec_prom_materias_x_hora <- as.numeric(mat_num_total_gpos_x_hora[,2])/25
min(vec_prom_materias_x_hora)#8.36
max(vec_prom_materias_x_hora)#50

# barplot(vec_prom_materias_x_hora,main = "Promedio de grupos por hora",
#         ylim = c(0,60),xlab="Horario",ylab="Número de grupos",
#         axis.lty=1,#cex.names = 1.1,cex.axis=0.84,#las=2,
#         col = param_graficas$col_barras,names.arg=7:21)

barplot(vec_prom_materias_x_hora,main = "Promedio de grupos por hora",
        ylim = c(0,60),xlab="Horario",ylab="Número de grupos",
        axis.lty=1,las=3,#cex.names = 1.1,cex.axis=0.84,
        col = param_graficas$col_barras,
        names.arg=c("7-8","8-9","9-10","10-11","11-12",
                    "12-13","13-14","14-15","15-16",
                    "16-17","17-18","18-19","19-20",
                    "20-21","21-22"))


# Se guarda la imagen -----------------------------------------------------
nom_plot <- "num_prom_gpos_x_hora_barplot.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)

