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

#' Se cargan los datos del semestre 2008-1 al 2020-1 del promedio de
#' alumnos por semestre y de la desviación estándar.
load("Programas/Figuras PDF/fig_histograma_FR_num_alum_sem_par_impar/vec_num_al_x_gpo_sem_impar.RData")
load("Programas/Figuras PDF/fig_histograma_FR_num_alum_sem_par_impar/vec_num_al_x_gpo_sem_par.RData")

# Figura ------------------------------------------------------------------
min(vec_num_al_x_gpo_sem_impar)#0
max(vec_num_al_x_gpo_sem_impar)#203
min(vec_num_al_x_gpo_sem_par)#0
max(vec_num_al_x_gpo_sem_par)#353

hist(vec_num_al_x_gpo_sem_impar, col=param_graficas$col1_hist,
     breaks = seq(0,360,by=10),
     xlim = c(0,200),
     freq = F,ylab = "Frecuencia relativa",ylim=c(0,0.025),
     main="Histogramas por semestres pares e impares",xlab = "Número alumnos")
lines(density(vec_num_al_x_gpo_sem_impar),col=param_graficas$col1_linea,lwd=param_graficas$lwd_dens)
hist(vec_num_al_x_gpo_sem_par,col=param_graficas$col2_hist,breaks = seq(0,360,by = 10),
     freq = F,add=TRUE)
lines(density(vec_num_al_x_gpo_sem_par),col=param_graficas$col2_linea,lwd=param_graficas$lwd_dens)

legend(120,0.01,c("Semestres impares","Semestres pares"),bty = "n",
       col=c(param_graficas$col1_linea,param_graficas$col2_linea),
       lty=c(1,1),
       #cex=1.1,
       lwd=param_graficas$lwd_dens)

# Se guarda la imagen -----------------------------------------------------
nom_plot <- "histograma_FR_num_alum_sem_par_impar.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)
