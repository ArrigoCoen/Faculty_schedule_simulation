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
plot(0:100,ylim=c(0,0.025),type="n",
     main = "Densidades",xlab="Número de alumnos",ylab="Densidad",
     col=param_graficas$col1_linea,pch="o")
lwd_dens <- 2
colores <- c(rep("green2",10),#Verdes
             rep("deeppink2",10),#Rosas
             # rep("limegreen",10),#Verdes
             rep("deepskyblue1",5))#Azules

#Se generan las demás líneas correspondientes a cada semestre
for(d in 1:length(lista_num_al_x_gpo_x_sem)){
  # line_color <- randomColor(hue=param_graficas$col1_linea)
  line_color <- colores[d]
  lines(density(lista_num_al_x_gpo_x_sem[[d]]),col=line_color,lwd=lwd_dens)
}

legend(45,0.028,c("Semestres del 2008-1 al 2012-2",
                   "Semestres del 2013-1 al 2017-2",
                   "Semestres del 2018-1 al 2020-1"),bty = "n",
       col=c("green2","deeppink2","deepskyblue1"),
       cex=1.1,lty=c(1,1,1),lwd=c(lwd_dens,lwd_dens,lwd_dens))


# Se guarda la imagen -----------------------------------------------------
nom_plot <- "densidades_num_alum_x_gpo_x_sem.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)
