##########################################################################
#' En este programa se encuentra el código que genera el histograma del
#' número de alumnos de semestres pares e impares.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
# rm(list=ls())  # Borra variables
# cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis")

# Figura ------------------------------------------------------------------
#' Se cargan los datos del semestre 2008-1 al 2020-1 del promedio de
#' alumnos por semestre y de la desviación estándar.
load("Programas/Figuras PDF/fig_histograma_FR_num_alum_sem_par_impar_Proba_I/vec_alum_x_gpo_par_proba_I.Rdata")
load("Programas/Figuras PDF/fig_histograma_FR_num_alum_sem_par_impar_Proba_I/vec_alum_x_gpo_impar_proba_I.Rdata")


# Histograma doble --------------------------------------------------------
min(c(vec_alum_x_gpo_impar_proba_I,vec_alum_x_gpo_par_proba_I))
max(c(vec_alum_x_gpo_impar_proba_I,vec_alum_x_gpo_par_proba_I))

hist(vec_alum_x_gpo_impar_proba_I, col=param_graficas$col1_hist,
     breaks = seq(0,200,by = 10),freq = F,ylim=c(0,0.02),
     ylab = "Frecuencia relativa",xlab = "Número alumnos",
     # ylab = "Densidad",xlab = "Número alumnos",
     main="Probabilidad I semestres par e impar")
lines(density(vec_alum_x_gpo_impar_proba_I),col=param_graficas$col1_linea,
      lwd=param_graficas$lwd_dens)
hist(vec_alum_x_gpo_par_proba_I,col=param_graficas$col2_hist,
     breaks = seq(0,200,by = 10),freq = F,add=TRUE)
lines(density(vec_alum_x_gpo_par_proba_I),col=param_graficas$col2_linea,
      lwd=param_graficas$lwd_dens)

legend(130,0.02,c("Semestres impares","Semestres pares"),bty = "n",
       col=c(param_graficas$col1_linea,param_graficas$col2_linea),lty=c(1,1),
       cex=1.1,lwd=param_graficas$lwd_dens)

# Se guarda la imagen -----------------------------------------------------
nom_plot <- "histograma_FR_num_alum_sem_par_impar_Proba_I.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)
