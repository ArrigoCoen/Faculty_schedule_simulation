##########################################################################
#' En este programa se encuentra el código que genera el histograma del
#' número de alumnos totales de semestres pares e impares.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
# rm(list=ls())  # Borra variables
# cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis")

#' Se cargan los datos
load("Programas/Figuras PDF/fig_histograma_FR_num_alum_matu_vesp/vec_num_alum_x_sem_matutino.RData")
load("Programas/Figuras PDF/fig_histograma_FR_num_alum_matu_vesp/vec_num_alum_x_sem_vespertino.RData")

# Histograma doble --------------------------------------------------------
min(c(vec_num_alum_x_sem_matutino,vec_num_alum_x_sem_vespertino))#47
max(c(vec_num_alum_x_sem_matutino,vec_num_alum_x_sem_vespertino))#4514

hist(vec_num_alum_x_sem_matutino,col=param_graficas$col1_hist,
     breaks = seq(0,5000,by = 100),
     freq = F,ylab = "Frecuencia relativa",ylim=c(0,0.0015),
     main="Histograma turnos matutino y vespertino",xlab = "Número alumnos")
lines(density(vec_num_alum_x_sem_matutino),col=param_graficas$col1_linea,lwd=param_graficas$lwd_dens)
hist(vec_num_alum_x_sem_vespertino, col=param_graficas$col2_hist,
     breaks = seq(0,4600,by = 100),freq = F,add=TRUE)
lines(density(vec_num_alum_x_sem_vespertino),col=param_graficas$col2_linea,lwd=param_graficas$lwd_dens)

legend(3500,0.0006,c("Turno matutino","Turno vespertino"),bty = "n",
       col=c(param_graficas$col1_linea,param_graficas$col2_linea),lty=c(1,1),
       cex=1.1,lwd=param_graficas$lwd_dens)

# Se guarda la imagen -----------------------------------------------------
nom_plot <- "histograma_FR_num_alum_matu_vesp.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)
