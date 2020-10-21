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
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri")

# Figura ------------------------------------------------------------------
#' Se cargan los datos del semestre 2008-1 al 2020-1 del 
load("Programas/Figuras PDF/fig_densidad_num_alum_x_gpo_x_sem/lista_num_al_x_gpo_x_sem.RData")

# Plot --------------------------------------------------------------
#Ponemos todos los datos en un vector
vec_alumnos <- lista_num_al_x_gpo_x_sem[[1]]
for(d in 2:length(lista_num_al_x_gpo_x_sem)){
  vec_alumnos <- c(vec_alumnos,lista_num_al_x_gpo_x_sem[[d]])
}
min(vec_alumnos)#0
max(vec_alumnos)#353

lwd_dens <- 6
# par(mfrow=c(1,2),cex=1.1) # set the plotting area into a 1*2 array
# par(mfrow=c(1,2)) # set the plotting area into a 1*2 array
# hist(vec_alumnos,col=param_graficas$col1_hist,breaks = seq(0,360,by = 5),
hist(vec_alumnos,col=param_graficas$col1_hist,breaks = seq(0,360,by = 10),
     ylab = "Frecuencia relativa",freq = F,ylim = c(0,0.025),
     main="Histograma del número de alumnos",xlab = "Número alumnos")
lines(density(vec_alumnos),col=param_graficas$col1_linea,
      lwd=lwd_dens)
# lines(dpois(0:360,34.18746))
# lines(dpois(0:100,34.18746),col="red")

# distribucion <- fitdist(vec_alumnos, distr = "pois")
# summary(distribucion)
# summary(fitdist(vec_alumnos, distr = "pois"))


# Se guarda la imagen -----------------------------------------------------
nom_plot <- "densidad_num_alum_x_gpo_x_sem.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)
