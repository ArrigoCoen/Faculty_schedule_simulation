##########################################################################
#' En este programa se encuentra el código que genera la gráfica con 
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
# rm(list=ls())  # Borra variables
# cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis")

#' Se cargan los datos
load("Programas/Figuras PDF/fig_gmm_esqueleto_fin/wait_mat_esqueleto_inicial.RData")
load("Programas/Figuras PDF/fig_gmm_esqueleto_fin/wait_mat_esqueleto_final.RData")
library(mixtools)

# Modelos GMM --------------------------------------------------------------
mixmdl_1_esq <- normalmixEM(wait_mat_esqueleto_inicial,k = 4)#Modelo inicial
mixmdl_esqueleto <- normalmixEM(wait_mat_esqueleto_final,mixmdl_1_esq$mu)#Modelo final


# Figura ------------------------------------------------------------------
hist(wait_mat_esqueleto_final,freq = F,breaks = seq(6,22,by = 1),
     main = "Histograma de grupos en un esqueleto",
     ylab = "Frecuencia reativa",
     xlab = "Horas")

lines(density(rnorm(1000,mean = mixmdl_esqueleto$mu,sd = mixmdl_esqueleto$sigma)),
      lty=1,lwd=2,col = param_graficas$col1_linea)
legend(10,0.11,"Modelo de mezcla de normales",
       bty = "n",
       col=param_graficas$col1_linea,lty=c(1,1),
       # cex=1.1,
       lwd=2)

# Se guarda la imagen -----------------------------------------------------
nom_plot <- "gmm_esqueleto_fin.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)
