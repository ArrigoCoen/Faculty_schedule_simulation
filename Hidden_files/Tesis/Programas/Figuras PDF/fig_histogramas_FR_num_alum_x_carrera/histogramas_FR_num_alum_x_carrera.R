##########################################################################
#' En este programa se encuentra el código que genera los histogramas del
#' número de alumnos totales para cada carrera del Depto. de Mate.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
# rm(list=ls())  # Borra variables
# cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis")

#' Se cargan los archivos que se van a utilizar
load("Programas/Figuras PDF/fig_histogramas_FR_num_alum_x_carrera/lista_num_alum_x_carrera.RData")


# Histograma doble --------------------------------------------------------
num_alum_actuaria <- as.numeric(lista_num_alum_x_carrera[[1]])#Azul
num_alum_CdC <- as.numeric(lista_num_alum_x_carrera[[2]])#Morado
num_alum_mate <- as.numeric(lista_num_alum_x_carrera[[3]])#Verde
num_alum_mateAp <- as.numeric(lista_num_alum_x_carrera[[4]])#Magenta

min(num_alum_actuaria)#0
max(num_alum_actuaria)#353
min(num_alum_CdC)#0
max(num_alum_CdC)#211
min(num_alum_mate)#0
max(num_alum_mate)#353
min(num_alum_mateAp)#0
max(num_alum_mateAp)#353


#### Gráfica con frecuencia = F
# hist(num_alum_mate,col=param_graficas$col3_hist,breaks = seq(0,360,by = 10),
#      freq = F,ylab = "Frecuencia",ylim=c(0,0.025),
#      main="Histograma semestres par e impar",xlab = "Número alumnos por grupo")
# hist(num_alum_CdC,col=param_graficas$col2_hist,breaks = seq(0,360,by = 10),
#      freq = F,add=TRUE)
# hist(num_alum_actuaria,col=param_graficas$col1_hist,breaks = seq(0,360,by = 10),
#      freq = F,add=TRUE)
# hist(num_alum_mateAp,col=param_graficas$col4_hist,breaks = seq(0,360,by = 10),
#      freq = F,add=TRUE)

#Se crea la base de la gráfica
plot(0:200,ylim=c(0,0.021),type="n",
     main = "Densidades del número de alumnos por carrera",
     xlab="Número de alumnos",ylab="Densidad",
     col=param_graficas$col1_linea,pch="o")
lwd_dens <- 2.5
lines(density(num_alum_mate),col=param_graficas$col5_hist,lwd=lwd_dens)
lines(density(num_alum_mateAp),col=param_graficas$col4_hist,lwd=lwd_dens)
lines(density(num_alum_actuaria),col=param_graficas$col1_hist,lwd=lwd_dens)
lines(density(num_alum_CdC),col=param_graficas$col3_hist,lwd=lwd_dens)

legend(110,0.01,c("Actuaría","Ciencias de la Computación","Matemáticas",
                 "Matemáticas Aplicadas"),bty = "n",
       col=c(param_graficas$col1_hist,param_graficas$col3_hist,
             param_graficas$col5_hist,param_graficas$col4_hist),
       lty=c(1,1,1,1),cex=1.1,lwd=c(lwd_dens,lwd_dens,lwd_dens,lwd_dens))

# Se guarda la imagen -----------------------------------------------------
nom_plot <- "histogramas_FR_num_alum_x_carrera.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)

