##########################################################################
#' En este programa se encuentra el código que genera la gráfica con el
#' número de genes por generación. Con 8 generaciones y tamaño de la 
#' población igual a 8.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
# rm(list=ls())  # Borra variables
# cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis")

#' Se cargan los datos
load("Programas/Figuras PDF/fig_num_genes_g08_n08_m004_U510/mat_num_genes.RData")


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
          main = "Número de genes en asignaciones por generación",
          y = "Num_genes",ylab = "Número de genes",
          add = "jitter")

# Se guarda la imagen -----------------------------------------------------
nom_plot <- "boxplot_num_genes_g08_n08_m004_U510.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)

