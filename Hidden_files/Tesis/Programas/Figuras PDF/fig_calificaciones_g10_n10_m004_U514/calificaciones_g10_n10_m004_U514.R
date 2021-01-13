##########################################################################
#' En este programa se encuentra el código que genera las gráficas con los
#' datos de las calificaciones por generación. Con 10 generaciones y el
#' tamaño de la población es 10.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
# rm(list=ls())  # Borra variables
# cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis")

#' Se cargan los datos
load("Programas/Figuras PDF/fig_calificaciones_g10_n10_m004_U514/mat_calif_generaciones.RData")


# Figura: Boxplot calificaciones ------------------------------------------
#Definimos el data.frame que necesitamos para graficar los boxplots
df_calif_gen <- data.frame(Calificaciones = 0,
                           Generaciones = sort(rep(c(1:dim(mat_calif_generaciones)[2]),
                                                   dim(mat_calif_generaciones)[1])))
for(k in 1:dim(mat_calif_generaciones)[2]){
  vec <- (dim(mat_calif_generaciones)[1]*(k-1))+1:dim(mat_calif_generaciones)[1]
  # cat("\nk = ", k,"\n\n vec = ",vec)
  df_calif_gen[vec,1] <- mat_calif_generaciones[,k]
}

# Create basic boxplot
ggboxplot(df_calif_gen, x = "Generaciones",xlab = "Generación",
          main = "Calificación de asignaciones por generación",
          y = "Calificaciones",ylab = "Calificación",
          add = "jitter")

# Se guarda la imagen -----------------------------------------------------
nom_plot <- "boxplot_calif_g10_n10_m004_U514.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)


# Figura: Media calificaciones --------------------------------------------
(media_x_generacion <- colMeans(mat_calif_generaciones))

plot(media_x_generacion,#ylim = c(-1300,-400),
     main = "Promedio de calificaciones por generación",
     xlab = "Generación",ylab = "Media de calificación")

# Se guarda la imagen -----------------------------------------------------
nom_plot <- "media_calif_g10_n10_m004_U514.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)


# Figura: Varianza calificaciones -----------------------------------------
# install.packages('resample')
library(resample)
(var_x_generacion <- colVars(mat_calif_generaciones))

plot(var_x_generacion,main = "Varianza de calificaciones por generación",
     xlab = "Generación",ylab = "Varianza de calificación")


# Se guarda la imagen -----------------------------------------------------
nom_plot <- "varianza_g10_n10_m004_U514.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)

