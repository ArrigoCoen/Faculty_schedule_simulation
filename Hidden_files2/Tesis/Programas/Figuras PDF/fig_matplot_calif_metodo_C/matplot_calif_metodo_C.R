##########################################################################
#' En este programa se encuentra el código que genera el matplot para
#' las calificaciones de la metodología C.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
# rm(list=ls())  # Borra variables
# cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis")

#' Se cargan los datos
load("Programas/Figuras PDF/fig_matplot_calif_metodo_C/mat_calif_C.RData")


# Figura ------------------------------------------------------------------
# View(mat_calif_C)

matplot(mat_calif_C, type = "l",main = "Metodología C",xlab = "Iteraciones",
        ylab = "Calificación",ylim = c(-0.5,0.8))


# Se guarda la imagen -----------------------------------------------------
nom_plot <- "matplot_metodo_C.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)

