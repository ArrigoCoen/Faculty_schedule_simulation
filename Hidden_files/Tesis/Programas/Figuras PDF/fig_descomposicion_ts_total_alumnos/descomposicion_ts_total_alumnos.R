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
load("Programas/Figuras PDF/fig_descomposicion_ts_total_alumnos/vec_num_total_alum.RData")

#' Se convierten los datos en serie de tiempo
num_total_alum.ts <- ts(vec_num_total_alum,frequency = 2, start = c(2008, 1))
num_total_alum.ts

#' Se descompone la serie
num_total_alum.Comp <- decompose(num_total_alum.ts)
num_total_alum.Comp

#' Now lets plot the components.
plot(num_total_alum.Comp)

# Se guarda la imagen -----------------------------------------------------
nom_plot <- "descomposicion_ts_total_alumnos.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)

