##########################################################################
#' En este programa se encuentra el código que genera el histograma del
#' número de alumnos totales de semestres pares e impares.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
# rm(list=ls())  # Borra variables
# cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis")

#' Se cargan los datos
load("Programas/Figuras PDF/fig_histograma_FA_num_alum_matu_vesp/mat_num_alum_x_hora_x_sem.RData")

# Histograma doble --------------------------------------------------------
n_cols <- dim(mat_num_alum_x_hora_x_sem)[2]
mat_num_alum_x_sem_matutino <- mat_num_alum_x_hora_x_sem[1:8,]
mat_num_alum_x_sem_vespertino <- mat_num_alum_x_hora_x_sem[9:15,]
# save(mat_num_alum_x_sem_matutino,file = "mat_num_alum_x_sem_matutino.RData")
# save(mat_num_alum_x_sem_vespertino,file = "mat_num_alum_x_sem_vespertino.RData")
min(mat_num_alum_x_hora_x_sem[,2:26])#47
max(mat_num_alum_x_hora_x_sem[,2:26])#4514
min(mat_num_alum_x_sem_matutino[,2:26])#425
max(mat_num_alum_x_sem_matutino[,2:26])#4514
min(mat_num_alum_x_sem_vespertino[,2:26])#47
max(mat_num_alum_x_sem_vespertino[,2:26])#2612

vec_num_alum_x_sem_matutino <- as.numeric(mat_num_alum_x_sem_matutino[,2])
vec_num_alum_x_sem_vespertino <- as.numeric(mat_num_alum_x_sem_vespertino[,2])
for(s in 3:n_cols){
  vec_num_alum_x_sem_matutino<- c(vec_num_alum_x_sem_matutino,
                                  as.numeric(mat_num_alum_x_sem_matutino[,s]))
  vec_num_alum_x_sem_vespertino <- c(vec_num_alum_x_sem_vespertino,
                                     as.numeric(mat_num_alum_x_sem_vespertino[,s]))
}
min(vec_num_alum_x_sem_matutino)#425
max(vec_num_alum_x_sem_matutino)#4515
min(vec_num_alum_x_sem_vespertino)#47
max(vec_num_alum_x_sem_vespertino)#2612

# save(vec_num_alum_x_sem_matutino,file = "vec_num_alum_x_sem_matutino.RData")
# save(vec_num_alum_x_sem_vespertino,file = "vec_num_alum_x_sem_vespertino.RData")

##Gráfica con freq = T
hist(vec_num_alum_x_sem_matutino,col=param_graficas$col1_hist,breaks = seq(0,4600,by = 100),
     freq = T,ylab = "Frecuencia",ylim=c(0,25),
     main="Histograma turnos matutino y vespertino",xlab = "Número alumnos")
hist(vec_num_alum_x_sem_vespertino, col=param_graficas$col2_hist,breaks = seq(0,4600,by = 100),
     freq = T,add=TRUE)

legend(2500,25,c("Turno matutino","Turno vespertino"),bty = "n",
       col=c(param_graficas$col1_linea,param_graficas$col2_linea),lty=c(1,1),
       cex=1.1,lwd=param_graficas$lwd_dens)

# Se guarda la imagen -----------------------------------------------------
nom_plot <- "histograma_FA_num_alum_matu_vesp.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)
