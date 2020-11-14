##########################################################################
#' En este programa se encuentra el código que genera la gráfica con las
#' densidades aproximadas de los datos del número de alumnos por grupo. Se
#' genera una densidad por semestre.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
# rm(list=ls())  # Borra variables
# cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis")

#' Se cargan los datos
load("Programas/Figuras PDF/fig_densidades_num_alum_x_gpo_x_sem/lista_num_al_x_gpo_x_sem.Rdata")

# Figura ------------------------------------------------------------------
#Se crea la base de la gráfica con los datos del semestre 2008-1
plot(0:360,ylim=c(0,0.025),type="n",
     main = "Densidades",xlab="Número de alumnos",ylab="Densidad",
     col=param_graficas$col1_linea,pch="o")
lwd_dens <- 2
colores <- c("chartreuse","chartreuse1","chartreuse2","chartreuse3","chartreuse4",#Verdes
             "limegreen","olivedrab1","olivedrab2","olivedrab3","mediumspringgreen",#Verdes
             # "gold","gold1","gold2","gold3","goldenrod1",#Amarillos
             "deeppink","deeppink1","deeppink2","deeppink3","maroon1",#Rosas
             "hotpink","hotpink1","hotpink2","hotpink3","magenta",#Rosas
             # "firebrick","firebrick1","firebrick2","firebrick3","firebrick4",#Rojos
             # "darkorchid","darkorchid1","darkorchid2","darkorchid3","darkorchid4",#Morados
             # "dodgerblue","dodgerblue1","dodgerblue2","dodgerblue3","dodgerblue4")#Azules
             "deepskyblue","deepskyblue1","deepskyblue2","deepskyblue3","skyblue")#Azules

#Se generan las demás líneas correspondientes a cada semestre
for(d in 1:length(lista_num_al_x_gpo_x_sem)){
  # line_color <- randomColor(hue=param_graficas$col1_linea)
  line_color <- colores[d]
  lines(density(lista_num_al_x_gpo_x_sem[[d]]),col=line_color,lwd=lwd_dens)
}

# Se guarda la imagen -----------------------------------------------------
nom_plot <- "densidades_num_alum_x_gpo_x_sem.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)
