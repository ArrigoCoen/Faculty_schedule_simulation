##########################################################################
#' En este programa se encuentra el código que genera las gráficas con los
#' datos de las calificaciones por generación.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
# rm(list=ls())  # Borra variables
# cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis")

#' Se cargan los datos
load("Programas/Figuras PDF/fig_media_calif_x_generacion/mat_calif_generaciones.RData")
load("Programas/Figuras PDF/fig_media_calif_x_generacion/mat_num_genes.RData")


# Figura: Media calificaciones --------------------------------------------
(media_x_generacion <- colMeans(mat_calif_generaciones))

plot(media_x_generacion,main = "Promedio de calificaciones por generación",
     xlab = "Generación",ylab = "Media de calificación")

# Se guarda la imagen -----------------------------------------------------
nom_plot <- "media_calif_x_generacion.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)


# Figura: Varianza calificaciones -----------------------------------------
# install.packages('resample')
library(resample)
(var_x_generacion <- colVars(mat_calif_generaciones))

plot(var_x_generacion,main = "Varianza de calificaciones por generación",
     xlab = "Generación",ylab = "Varianza de calificación")


# Se guarda la imagen -----------------------------------------------------
nom_plot <- "varianza_calif_x_generacion.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)



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
          main = "                            Calificación de asignaciones por generación",
          y = "Calificaciones",ylab = "Calificación",
          add = "jitter")

# Se guarda la imagen -----------------------------------------------------
nom_plot <- "boxplot_calif_x_generacion.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)



# Figura: Boxplot número de genes -----------------------------------------
#Definimos el data.frame que necesitamos para graficar los boxplots
df_num_gen <- data.frame(Num_genes = 0,
                         Generaciones = sort(rep(c(1:dim(mat_num_genes)[2]),
                                                 dim(mat_num_genes)[1])))
for(k in 1:dim(mat_num_genes)[2]){
  vec <- (dim(mat_num_genes)[1]*(k-1))+1:dim(mat_num_genes)[1]
  # cat("\nk = ", k,"\n\n vec = ",vec)
  df_num_gen[vec,1] <- mat_num_genes[,k]
}

# Create basic boxplot
ggboxplot(df_num_gen, x = "Generaciones",xlab = "Generación",
          main = "                  Número de genes en asignaciones por generación",
          y = "Num_genes",ylab = "Número de genes",
          add = "jitter")

# Se guarda la imagen -----------------------------------------------------
nom_plot <- "boxplot_num_genes_x_generacion.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
                    width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)


