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
load("Programas/Figuras PDF/fig_histogramas_FA_num_alum_x_carrera/lista_num_alum_x_carrera.RData")


# Histograma doble --------------------------------------------------------
num_alum_actuaria <- as.numeric(lista_num_alum_x_carrera[[1]])#Azul
num_alum_CdC <- as.numeric(lista_num_alum_x_carrera[[2]])#Rojo
num_alum_mate <- as.numeric(lista_num_alum_x_carrera[[3]])#Verde
num_alum_mateAp <- as.numeric(lista_num_alum_x_carrera[[4]])#Morado

min(num_alum_actuaria)#0
max(num_alum_actuaria)#353
min(num_alum_CdC)#0
max(num_alum_CdC)#211
min(num_alum_mate)#0
max(num_alum_mate)#353
min(num_alum_mateAp)#0
max(num_alum_mateAp)#353


#### Gráfica con frecuencia = T
par(mfrow=c(2,2),cex=1) # set the plotting area into a 2*2 array
hist(num_alum_actuaria,col=param_graficas$col1_hist,breaks = seq(0,360,by = 10),
     freq = T,ylab = "Frecuencia absoluta",ylim=c(0,2500),
     xlim = c(0,200),
     main="Histograma Actuaría",xlab = "Número alumnos por grupo")
hist(num_alum_CdC,col=param_graficas$col3_hist,breaks = seq(0,360,by = 10),
     freq = T,ylab = "Frecuencia absoluta",ylim=c(0,2500),
     xlim = c(0,200),cex.main=0.98,# Tamaño de letra del título
     main="Histograma Ciencias de la Computación",xlab = "Número alumnos por grupo")
hist(num_alum_mate,col=param_graficas$col5_hist,breaks = seq(0,360,by = 10),
     freq = T,ylab = "Frecuencia absoluta",ylim=c(0,2500),
     xlim = c(0,200),
     main="Histograma Matemáticas",xlab = "Número alumnos por grupo")
hist(num_alum_mateAp,col=param_graficas$col4_hist,breaks = seq(0,360,by = 10),
     freq = T,ylab = "Frecuencia absoluta",ylim=c(0,2500),
     xlim = c(0,200),
     main="Histograma Matemáticas Aplicadas",xlab = "Número alumnos por grupo")


# hist(num_alum_mate,col=param_graficas$col3_hist,breaks = seq(0,360,by = 10),
#      freq = T,ylab = "Frecuencia",#ylim=c(0,0.025),
#      main="Histograma semestres par e impar",xlab = "Número alumnos por grupo")
# hist(num_alum_actuaria,col=param_graficas$col1_hist,breaks = seq(0,360,by = 10),
#      freq = T,add=TRUE)
# hist(num_alum_mateAp,col=param_graficas$col4_hist,breaks = seq(0,360,by = 10),
#      freq = T,add=TRUE)
# hist(num_alum_CdC,col=param_graficas$col2_hist,breaks = seq(0,360,by = 10),
#      freq = T,add=TRUE)


# Se guarda la imagen -----------------------------------------------------
nom_plot <- "histogramas_FA_num_alum_x_carrera.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)

