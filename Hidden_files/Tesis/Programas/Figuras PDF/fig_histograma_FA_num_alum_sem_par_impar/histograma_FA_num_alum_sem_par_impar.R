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

#' Se cargan los datos del semestre 2008-1 al 2020-1
load("Programas/Figuras PDF/fig_histograma_FA_num_alum_sem_par_impar/vec_num_al_x_gpo_sem_impar.RData")
load("Programas/Figuras PDF/fig_histograma_FA_num_alum_sem_par_impar/vec_num_al_x_gpo_sem_par.RData")

# Histograma doble --------------------------------------------------------
min(vec_num_al_x_gpo_sem_impar)#0
max(vec_num_al_x_gpo_sem_impar)#203
min(vec_num_al_x_gpo_sem_par)#0
max(vec_num_al_x_gpo_sem_par)#353

#### Gráfica con frecuencia = T
hist(vec_num_al_x_gpo_sem_impar,col=param_graficas$col1_hist,breaks = seq(0,360,by = 5),
     freq = T,ylab = "Frecuencia",#ylim=c(0,0.025),
     main="Histograma semestres par e impar",xlab = "Número alumnos por grupo")
hist(vec_num_al_x_gpo_sem_par,col=param_graficas$col2_hist,breaks = seq(0,360,by = 5),
     freq = T,add=TRUE)

legend(180,800,c("Semestres impares","Semestres pares"),bty = "n",
       col=c(param_graficas$col1_linea,param_graficas$col2_linea),
       lty=c(1,1),cex=1.1,lwd=param_graficas$lwd_dens)

# Se guarda la imagen -----------------------------------------------------
nom_plot <- "histograma_FA_num_alum_sem_par_impar.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)
