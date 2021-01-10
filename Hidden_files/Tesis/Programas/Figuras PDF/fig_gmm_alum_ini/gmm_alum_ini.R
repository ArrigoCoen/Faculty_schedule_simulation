##########################################################################
#' En este programa se encuentra el código que genera el histograma con 
#' los datos del número de alumnos por hora. Se le ajusta la densidad con
#' el modelo GMM (modelo inicial).
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
# rm(list=ls())  # Borra variables
# cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis")
library(mixtools)

#' Se cargan los datos
load("Programas/Figuras PDF/fig_gmm_alum_ini/wait_alumnos.RData")

# Modelo GMM --------------------------------------------------------------
mixmdl_1_D <- normalmixEM(wait_alumnos,k = 4)#Modelo inicial

# Figura ------------------------------------------------------------------
hist(wait_alumnos,freq = F,breaks = seq(6,22,by = 1),
     main = "Histograma de la demanda de alumnos por hora",
     ylab = "Frecuencia reativa",
     xlab = "Número de alumnos esperados para el 2020-1")
lines(density(rnorm(1000,mean = mixmdl_1_D$mu,sd = mixmdl_1_D$sigma)),
      lty=1,lwd=2,col = param_graficas$col1_linea)
legend(10,0.145,"Modelo inicial de mezcla de normales",bty = "n",
       col=param_graficas$col1_linea,lty=c(1,1),
       cex=1.1,lwd=2)

# Se guarda la imagen -----------------------------------------------------
nom_plot <- "gmm_alum_ini.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)
