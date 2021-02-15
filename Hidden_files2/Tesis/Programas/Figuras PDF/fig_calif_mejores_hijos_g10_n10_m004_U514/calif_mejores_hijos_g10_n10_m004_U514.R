##########################################################################
#' En este programa se encuentra el código que genera la gráfica con los
#' las calificaciones de los mejores elementos. Con 10 generaciones y el
#' tamaño de la población es 10.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
# rm(list=ls())  # Borra variables
# cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis")

#' Se cargan los datos
load("Programas/Figuras PDF/fig_calif_mejores_hijos_g10_n10_m004_U514/calif_mejor_elem.RData")


# Figura: Media calificaciones --------------------------------------------
min(calif_mejor_elem)#-1258.83
max(calif_mejor_elem)#-514.413

plot(calif_mejor_elem,#ylim = c(-1300,-400),
     main = "Calificación del mejor elemento por generación",
     xlab = "Generación",ylab = "Calificación")

# Se guarda la imagen -----------------------------------------------------
nom_plot <- "calif_mejores_hijos_g10_n10_m004_U514.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)
