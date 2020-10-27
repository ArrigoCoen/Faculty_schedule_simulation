##########################################################################
#' En este programa se encuentra el código que genera el histograma que
#' muestra el resultado de la prueba de Kolmogorov-Smirnov.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
# rm(list=ls())  # Borra variables
# cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis")

#' Se cargan los datos del semestre 2008-1 al 2020-1 del número de alumnos
#' por grupo.
load("Programas/Figuras PDF/fig_histograma_FR_prueba_KS/vec_al_x_gpo_todos_sem.RData")

# Histograma --------------------------------------------------------------
X <- vec_al_x_gpo_todos_sem
n <- length(X)#17246

#' La distribución que se ajusta a los datos es una
#' Normal(mu = 34.18745219, sd = 26.5768345). Graficamos el histograma con
#' las frecuencias relativas de los datos. La línea azul es la densidad
#' ajustada generada por R y la línea roja es la densidad de "n" números
#' aleatorios con distribución  Normal(34.18745219,26.5768345).
hist(X,col=param_graficas$col1_hist,breaks = seq(0,360,by = 10),
     ylab = "Frecuencia relativa",freq = F,ylim = c(0,0.075),
     main="Histograma del número de alumnos",xlab = "Número alumnos")
lines(density(X),col=param_graficas$col1_linea,
      lwd=param_graficas$lwd_dens)
lines(density(rnorm(n,34.18745219,26.5768345)),col=param_graficas$col2_linea,
      lwd=param_graficas$lwd_dens)
lines(density(rpois(n,34.18745219)),col=param_graficas$col3_linea,
      lwd=param_graficas$lwd_dens)


legend(100,0.035,c("Densidad ajustada a los datos",
                   "Densidad de una Poisson(34.18)",
                   "Densidad de una Normal(34.18,26.57)"),
       bty = "n",col=c(param_graficas$col1_linea,param_graficas$col3_linea,
                       param_graficas$col2_linea),
       lty=c(1,1),cex=1.1,lwd=param_graficas$lwd_dens)

# Se guarda la imagen -----------------------------------------------------
nom_plot <- "histograma_FR_prueba_KS.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)

