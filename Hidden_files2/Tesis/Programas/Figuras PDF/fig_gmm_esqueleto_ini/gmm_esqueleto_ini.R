##########################################################################
#' En este programa se encuentra el código que genera la gráfica con 
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
# rm(list=ls())  # Borra variables
# cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis")

#' Se cargan los datos
load("Programas/Figuras PDF/fig_gmm_esqueleto_ini/wait_mat_esqueleto_inicial.RData")

# Figura ------------------------------------------------------------------
hist(wait_mat_esqueleto_inicial,freq = F,breaks = seq(6,22,by = 1),
     main = "Histograma de grupos en un esqueleto",
     ylab = "Frecuencia reativa",
     xlab = "Horas")

# Se guarda la imagen -----------------------------------------------------
nom_plot <- "gmm_esqueleto_ini.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)
