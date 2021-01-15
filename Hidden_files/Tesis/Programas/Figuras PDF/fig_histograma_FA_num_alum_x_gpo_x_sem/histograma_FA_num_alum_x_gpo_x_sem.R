##########################################################################
#' En este programa se encuentra el código que genera el histograma del
#' número de alumnos por grupo por semestre.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
# rm(list=ls())  # Borra variables
# cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis")

#' Se cargan los datos del semestre 2008-1 al 2020-1
load("Programas/Figuras PDF/fig_histograma_FA_num_alum_x_gpo_x_sem/lista_num_al_x_gpo_x_sem.RData")

# Histograma --------------------------------------------------------------
#Ponemos todos los datos en un vector
vec_alumnos <- lista_num_al_x_gpo_x_sem[[1]]
for(d in 2:length(lista_num_al_x_gpo_x_sem)){
  vec_alumnos <- c(vec_alumnos,lista_num_al_x_gpo_x_sem[[d]])
}
min(vec_alumnos)#0
max(vec_alumnos)#353

hist(vec_alumnos,col=param_graficas$col1_hist,
     breaks = seq(0,360,by = 10),xlim = c(0,200),
     ylab = "Frecuencia",xlab = "Número alumnos",freq = T,
     main="Histograma del número de alumnos por grupo")

# Se guarda la imagen -----------------------------------------------------
nom_plot <- "histograma_FA_num_alum_x_gpo_x_sem.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)
