##########################################################################
#' En este programa se encuentra el código que genera la gráfica de la
#' desviación estandar de alumnos por semestre.
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
load("Programas/Figuras PDF/fig_sd_alum_x_gpo_x_sem_ts/vec_sd_total_alum.RData")
#' Se convierten los datos en serie de tiempo
sd_total_alum.ts <- ts(vec_sd_total_alum,frequency = 2, start = c(2008, 1))
sd_total_alum.ts
min(sd_total_alum.ts)#24.20968
max(sd_total_alum.ts)#28.87565


#' Se grafica la serie de tiempo
plot.ts(sd_total_alum.ts,type = "l",
        main = "Desviación estandar de alumnos por semestre",
        ylim = c(19,35),xlab="Semestres",ylab="Número de alumnos")


# Se guarda la imagen -----------------------------------------------------
nom_plot <- "sd_alum_x_gpo_x_sem_ts.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)
