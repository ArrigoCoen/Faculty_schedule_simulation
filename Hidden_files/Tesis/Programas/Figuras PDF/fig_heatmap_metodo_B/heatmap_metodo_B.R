##########################################################################
#' En este programa se encuentra el código que genera el heatmap para
#' las calificaciones de la metodología B.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
# rm(list=ls())  # Borra variables
# cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis")

#' Se cargan los datos
load("Programas/Figuras PDF/fig_heatmap_metodo_B/mat_calif_B.RData")

# Figura ------------------------------------------------------------------
# View(mat_calif_B)

m <- mat_calif_B
m_ordenada <- m

for(i in 1:nrow(m)){
  m_ordenada[i,] <- sort(m[i,])
}
colnames(m_ordenada) <- rep("",dim(m_ordenada)[2])

colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
heatmap(m_ordenada, Colv = NA, Rowv = NA, scale="none",col=colMain,
        main = "Calificaciones de métodología B")
legend(x="bottomright", legend=c(paste0("mín = ",round(min(mat_calif_B),2)),
                                 paste0("media = ",round(mean(mat_calif_B),2)),
                                 paste0("máx = ",round(max(mat_calif_B),2))), 
       fill=colorRampPalette(brewer.pal(8, "Blues"))(3))

# Se guarda la imagen -----------------------------------------------------
nom_plot <- "heatmap_metodo_B.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)
