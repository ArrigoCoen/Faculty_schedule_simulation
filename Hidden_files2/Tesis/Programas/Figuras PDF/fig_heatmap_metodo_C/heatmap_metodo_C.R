##########################################################################
#' En este programa se encuentra el código que genera el heatmap para
#' las calificaciones de la metodología C.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
# rm(list=ls())  # Borra variables
# cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis")

#' Se cargan los datos
load("Programas/Figuras PDF/fig_heatmap_metodo_C/mat_calif_C.RData")

# Figura ------------------------------------------------------------------
# View(mat_calif_C)

m <- mat_calif_C
colnames(m) <- rep("",dim(m)[2])
m_ordenada <- m

for(i in 1:nrow(m)){
  m_ordenada[i,] <- sort(m[i,])
}
colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
heatmap(m_ordenada, Colv = NA, Rowv = NA, scale="none",col=colMain,
        main = "Calificaciones de métodología C")
legend(x="bottomright", legend=c(paste0("mín = ",round(min(mat_calif_C),2)),
                                 paste0("media = ",round(mean(mat_calif_C),2)),
                                 paste0("máx = ",round(max(mat_calif_C),2))), 
       fill=colorRampPalette(brewer.pal(8, "Blues"))(3))


# Se guarda la imagen -----------------------------------------------------
nom_plot <- "heatmap_metodo_C.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)
