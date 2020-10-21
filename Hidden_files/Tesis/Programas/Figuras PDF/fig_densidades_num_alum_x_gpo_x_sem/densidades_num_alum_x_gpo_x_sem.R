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
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri")


# Figura ------------------------------------------------------------------
#' Se cargan los datos
load("Programas/Figuras PDF/fig_densidades_num_alum_x_gpo_x_sem/lista_num_al_x_gpo_x_sem.Rdata")

#' Se convierten los datos en serie de tiempo
sem_20081 <- lista_num_al_x_gpo_x_sem[[1]]
sem_20081.ts <- ts(sem_20081,frequency = 1, start = c(7, 1))
sem_20081.ts

#Se crea la base de la gráfica con los datos del semestre 2008-1
plot(0:360,ylim=c(0,0.025),type="n",
     main = "Densidades",xlab="Número de alumnos",ylab="Densidad",
     col=param_graficas$col1_linea,pch="o")
lwd_dens <- 2
lines(density(sem_20081),col=param_graficas$col1_linea,lwd=lwd_dens)

#Se generan las demás líneas correspondientes a cada semestre
for(d in 2:length(lista_num_al_x_gpo_x_sem)){
  line_color <- randomColor(hue=param_graficas$col1_linea)
  lines(density(lista_num_al_x_gpo_x_sem[[d]]),col=line_color,lwd=lwd_dens)
}



# Gráfica de varios colores con los nombres de los semestres
# #Se crea la base de la gráfica con los datos del semestre 2008-1
# plot(0:360,ylim=c(0,0.05),type="n",
#      main = "Densidades",xlab="Número de alumnos",ylab="Densidad",
#      col=param_graficas$col1_linea,pch="o")
# lwd_dens <- 2
# lines(density(sem_20081),col=param_graficas$col1_linea,lwd=lwd_dens)
# colores <- 0
# nom_sem <- as.character((20081:20201)[(20081:20201)%% 10>0 &(20081:20201)%% 10<3])
# 
# #Se generan las demás líneas correspondientes a cada semestre
# for(d in 2:length(lista_num_al_x_gpo_x_sem)){
#   # line_color <- randomColor(hue=param_graficas$col1_linea)
#   line_color <- randomColor()
#   colores <- c(colores,line_color)
#   lines(density(lista_num_al_x_gpo_x_sem[[d]]),col=line_color,lwd=lwd_dens)
# }
# #Le quitamos el cero inicial
# colores <- colores[-1]
# 
# legend(80,0.05,nom_sem,bty = "n",col=colores,lty=c(1,1),ncol=5)#,cex=1.1


# Se guarda la imagen -----------------------------------------------------
nom_plot <- "densidades_num_alum_x_gpo_x_sem.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)
