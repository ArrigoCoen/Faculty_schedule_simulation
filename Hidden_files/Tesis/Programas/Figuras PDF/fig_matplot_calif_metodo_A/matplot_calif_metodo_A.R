##########################################################################
#' En este programa se encuentra el código que genera el matplot para
#' las calificaciones de la metodología A.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
# rm(list=ls())  # Borra variables
# cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis")

#' Se cargan los datos
load("Programas/Figuras PDF/fig_matplot_calif_metodo_A/mat_calif_A.RData")


# Figura ------------------------------------------------------------------
# View(mat_calif_A)

matplot(mat_calif_A, type = "l",main = "Metodología A",xlab = "Iteraciones",
        ylab = "Calificación",ylim = c(-5,1))


# Se guarda la imagen -----------------------------------------------------
nom_plot <- "matplot_metodo_A.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)

