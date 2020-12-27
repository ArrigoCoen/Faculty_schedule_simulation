##########################################################################
#' En este programa se encuentra el código que se encarga de mandar llamar
#' a los archivos que generan las gráficas para el pdf
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis")

# install.packages('randomcoloR')
# install.packages('fitdistrplus')

library(randomcoloR)
library(fitdistrplus)

param_graficas <- list()
# param_graficas$color_barras = rgb(91,155,213)##Azul como excel
# param_graficas$col_barras = '#5b9bd5' ##Azul como excel
param_graficas$col_barras = "skyblue" ##Azul cielo
# param_graficas$col1_hist = rgb(0,0,1,1/4)##Azul histogramas
param_graficas$col1_hist = "skyblue" ##Azul cielo histogramas
param_graficas$col2_hist = rgb(1,0,0,1/4)##Rojo histogramas
param_graficas$col3_hist = "purple" ##Morado histogramas
param_graficas$col4_hist = "magenta" ##Magenta histogramas
param_graficas$col5_hist = "limegreen" ##Verde histogramas
# param_graficas$col5_hist = "green" ##Verde histogramas
param_graficas$col1_linea = "blue" ##Azul densidad
param_graficas$col2_linea = "red" ##Rojo densidad
param_graficas$col3_linea = "purple" ##Morado densidad
param_graficas$lwd_dens = 6 #Ancho de línea para densidad ajustada
param_graficas$ancho_pdf = 8 #Anchura para guardar imagen
param_graficas$altura_pdf = 6 #Altura para guardar imagen
param_graficas$dir_TeX = "TeX/LaTeX/Pictures/"

# Figuras -----------------------------------------------------------------

##### Número de alumnos totales por semestres pares e impares
source("Programas/Figuras PDF/fig_num_alum_sem_par_impar_Proba_I/num_alum_sem_par_impar_Proba_I.R")

##### Histograma del número de alumnos por semestre
source("Programas/Figuras PDF/fig_histograma_FR_num_alum_sem_par_impar_Proba_I/histograma_FR_num_alum_sem_par_impar_Proba_I.R")

##### Número de alumnos por turno
source("Programas/Figuras PDF/fig_num_alum_x_turno_Proba_I/num_alum_x_turno_Proba_I.R")

##### Histograma del número de alumnos por turno
source("Programas/Figuras PDF/fig_histograma_FR_num_alum_x_turno_Proba_I/histograma_FR_num_alum_x_turno_Proba_I.R")

##### Número total de alumnos por semestre
source("Programas/Figuras PDF/fig_num_alum_total_x_sem_barplot/num_alum_total_x_sem_barplot.R")

##### Media de alumnos por semestre
source("Programas/Figuras PDF/fig_prom_alum_total_x_sem_ts/prom_alum_total_x_sem_ts.R")

##### Desviación estándar del número de alumnos por semestre
source("Programas/Figuras PDF/fig_sd_alum_x_gpo_x_sem_ts/sd_alum_x_gpo_x_sem_ts.R")

##### Descomposición por el método aditivo de Holt-Winters: Total de alumnos por semestre
source("Programas/Figuras PDF/fig_descomposicion_ts_total_alumnos/descomposicion_ts_total_alumnos.R")

##### Número de alumnos de semestres pares e impares
source("Programas/Figuras PDF/fig_num_alum_sem_par_impar_ts/num_alum_sem_par_impar_ts.R")

##### Histograma del número de alumnos de semestres pares e impares
source("Programas/Figuras PDF/fig_histograma_FR_num_alum_sem_par_impar/histograma_FR_num_alum_sem_par_impar.R")

##### Número de alumnos por turno de todos los semestres
source("Programas/Figuras PDF/fig_num_alum_matu_vesp_ts/num_alum_matu_vesp_ts.R")

##### Histograma del número de alumnos de los turnos matutino y vespertino
source("Programas/Figuras PDF/fig_histograma_FR_num_alum_matu_vesp/histograma_FR_num_alum_matu_vesp.R")

##### Histograma del número de alumnos de todos los semestres
source("Programas/Figuras PDF/fig_histograma_FA_num_alum_x_gpo_x_sem/histograma_FA_num_alum_x_gpo_x_sem.R")

##### Densidades del número de alumnos por grupo de cada semestre
source("Programas/Figuras PDF/fig_densidades_num_alum_x_gpo_x_sem/densidades_num_alum_x_gpo_x_sem.R")

##### Número promedio de grupos por hora
source("Programas/Figuras PDF/fig_num_prom_gpos_x_hora_barplot/num_prom_gpos_x_hora_barplot.R")

##### Número promedio de alumnos por hora
source("Programas/Figuras PDF/fig_prom_alum_x_hora_barplot/prom_alum_x_hora_barplot.R")

##### Histograma con densidad ajustadad por prueba de Kolmogorov-Smirnov
source("Programas/Figuras PDF/fig_histograma_FR_prueba_KS/histograma_FR_prueba_KS.R")

##### Histograma del número de alumnos por carrera
source("Programas/Figuras PDF/fig_histogramas_FA_num_alum_x_carrera/histogramas_FA_num_alum_x_carrera.R")
dev.off()#Para salir de la función par()

##### Densidades del número de alumnos por carrera
source("Programas/Figuras PDF/fig_histogramas_FR_num_alum_x_carrera/histogramas_FR_num_alum_x_carrera.R")

#####
# source("Programas/Figuras PDF/fig_heatmap_metodo_B/heatmap_metodo_B.R")

#####
# source("Programas/Figuras PDF/fig_heatmap_metodo_C/heatmap_metodo_C.R")

#####
source("Programas/Figuras PDF/fig_calif_mejores_hijos/.R")

#####
# source("Programas/Figuras PDF//.R")

#####
# source("Programas/Figuras PDF//.R")

#####
# source("Programas/Figuras PDF//.R")




# Figuras que NO están en el pdf ------------------------------------------
##### Histograma del número de alumnos de semestres pares e impares
# source("Programas/Figuras PDF/fig_histograma_FA_num_alum_sem_par_impar/histograma_FA_num_alum_sem_par_impar.R")

##### Histograma del número de alumnos de los turnos matutino y vespertino
# source("Programas/Figuras PDF/fig_histograma_FA_num_alum_matu_vesp/histograma_FA_num_alum_matu_vesp.R")

##### Densidad ajustada del tamaño de grupo
# source("Programas/Figuras PDF/fig_histograma_FR_num_alum_x_gpo_x_sem/histograma_FR_num_alum_x_gpo_x_sem.R")
