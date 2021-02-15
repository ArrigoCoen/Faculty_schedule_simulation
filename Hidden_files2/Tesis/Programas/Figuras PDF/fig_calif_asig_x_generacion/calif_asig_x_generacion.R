##########################################################################
#' En este programa se encuentra el código que genera la gráfica con las
#' calificaciones de los hijos. Hay una línea por cada generación.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
# rm(list=ls())  # Borra variables
# cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis")

#' Se cargan los datos
load("Programas/Figuras PDF/fig_calif_asig_x_generacion/mat_calif_generaciones.RData")

# Figura ------------------------------------------------------------------
matplot(mat_calif_generaciones,type = "l",
        main = "Calificaciones de las asignaciones por generación",
        xlab = "Asignaciones",ylab = "Calificaciones")


# Se guarda la imagen -----------------------------------------------------
nom_plot <- "calif_asig_x_generacion.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)
