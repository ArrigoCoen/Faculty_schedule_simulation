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
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri")

# Figura ------------------------------------------------------------------
#' Se cargan los datos del semestre 2008-1 al 2020-1 del promedio de
#' alumnos por semestre y de la desviación estándar.
load("Programas/Figuras PDF/fig_histograma_num_alum_vec_num_al_x_gpo_sem_par_impar/lista_num_al_x_gpo_x_sem.RData")

# Histograma doble --------------------------------------------------------
#Ponemos todos los datos en dos vectores
vec_num_al_x_gpo_sem_impar <- lista_num_al_x_gpo_x_sem[[1]]
vec_num_al_x_gpo_sem_par <- lista_num_al_x_gpo_x_sem[[2]]

ind_impar <- seq(from = 3, to =length(lista_num_al_x_gpo_x_sem),by = 2)
ind_par <- seq(from = 4, to =length(lista_num_al_x_gpo_x_sem),by = 2)

for(d in ind_impar){
  vec_num_al_x_gpo_sem_impar <- c(vec_num_al_x_gpo_sem_impar,lista_num_al_x_gpo_x_sem[[d]])
}
save(vec_num_al_x_gpo_sem_impar,file = "vec_num_al_x_gpo_sem_impar.RData")
min(vec_num_al_x_gpo_sem_impar)#0
max(vec_num_al_x_gpo_sem_impar)#203

for(d in ind_par){
  vec_num_al_x_gpo_sem_par <- c(vec_num_al_x_gpo_sem_par,lista_num_al_x_gpo_x_sem[[d]])
}
save(vec_num_al_x_gpo_sem_par,file = "vec_num_al_x_gpo_sem_par.RData")
min(vec_num_al_x_gpo_sem_par)#0
max(vec_num_al_x_gpo_sem_par)#353

lwd_dens <- 6
hist(vec_num_al_x_gpo_sem_par,col=param_graficas$col2_hist,breaks = seq(0,360,by = 5),
     freq = F,ylab = "Densidad",ylim=c(0,0.025),
     main="Histograma semestres par e impar",xlab = "Número alumnos")
lines(density(vec_num_al_x_gpo_sem_par),col=param_graficas$col2_linea,lwd=lwd_dens)
hist(vec_num_al_x_gpo_sem_impar, col=param_graficas$col1_hist,breaks = seq(0,360,by = 5),
     freq = F,add=TRUE)
lines(density(vec_num_al_x_gpo_sem_impar),col=param_graficas$col1_linea,lwd=lwd_dens)

legend(150,0.025,c("Semestres impares","Semestres pares"),bty = "n",
       col=c(param_graficas$col1_linea,param_graficas$col2_linea),
       lty=c(1,1),cex=1.1,lwd=lwd_dens)

#### Gráfica con frecuencia = T
# hist(vec_num_al_x_gpo_sem_par,col=param_graficas$col2_hist,breaks = seq(0,360,by = 5),
#      freq = T,ylab = "Frecuencia",#ylim=c(0,0.025),
#      main="Histograma semestres par e impar",xlab = "Número alumnos")
# lines(vec_num_al_x_gpo_sem_par,col=param_graficas$col2_linea,lwd=lwd_dens)
# hist(vec_num_al_x_gpo_sem_impar, col=param_graficas$col1_hist,breaks = seq(0,360,by = 5),
#      freq = T,add=TRUE)
# lines(vec_num_al_x_gpo_sem_impar,col=param_graficas$col1_linea,lwd=lwd_dens)
# 
# legend(150,500,c("Semestres impares","Semestres pares"),bty = "n",
#        col=c(param_graficas$col1_linea,param_graficas$col2_linea),
#        lty=c(1,1),cex=1.1,lwd=lwd_dens)

# Se guarda la imagen -----------------------------------------------------
nom_plot <- "histograma_num_alum_vec_num_al_x_gpo_sem_par_impar.pdf"
dev.print(pdf,paste0(param_graficas$dir_TeX,nom_plot),
          width=param_graficas$ancho_pdf, height=param_graficas$altura_pdf)


