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
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation\Hidden_files/Tesis/Programas/Figuras PDF")

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
param_graficas$col1_linea = "blue" ##Azul densidad
param_graficas$col2_linea = "red" ##Rojo densidad
param_graficas$ancho_pdf = 8 #Anchura para guardar imagen
param_graficas$altura_pdf = 6 #Altura para guardar imagen
param_graficas$dir_TeX = "TeX/LaTeX/Pictures/"

# Figuras -----------------------------------------------------------------

##### Número de alumnos totales por semestres pares e impares
source("fig_num_alum_sem_par_impar_Proba_I/num_alum_sem_par_impar_Proba_I.R")

##### Histograma del número de alumnos por semestre
source("fig_num_alum_x_turno_Proba_I/num_alum_x_turno_Proba_I.R")

##### Número de alumnos por turno
source("fig_histograma_FR_num_alum_x_turno_Proba_I/histograma_FR_num_alum_x_turno_Proba_I.R")

##### Histograma del número de alumnos por turno
source("fig_histograma_FR_num_alum_x_turno_Proba_I/histograma_FR_num_alum_x_turno_Proba_I.R")

##### Número total de alumnos por semestre
source("fig_num_alum_total_x_sem_barplot/num_alum_total_x_sem_barplot.R")

##### Media de alumnos por semestre
source("fig_prom_alum_total_x_sem_ts/prom_alum_total_x_sem_ts.R")

##### Desviación estándar del número de alumnos por semestre
source("fig_sd_alum_x_gpo_x_sem_ts/sd_alum_x_gpo_x_sem_ts.R")

##### Histograma del número de alumnos de todos los semestres
source("fig_histograma_num_alum_x_gpo_x_sem/histograma_num_alum_x_gpo_x_sem.R")
# graphics.off()#Código para limpiar las gráficas porque este histograma usa la función par()

##### Histograma del número de alumnos de semestres pares e impares
source("fig_histograma_FA_num_alum_sem_par_impar/histograma_FA_num_alum_sem_par_impar.R")

##### Histograma del número de alumnos de los turnos matutino y vespertino
source("fig_histograma_FA_num_alum_matu_vesp/histograma_FA_num_alum_matu_vesp.R")

##### Descomposición por el método aditivo de Holt-Winters: Total de alumnos por semestre
source("fig_descomposicion_ts_total_alumnos/descomposicion_ts_total_alumnos.R")

##### Número promedio de grupos por hora
source("fig_num_prom_gpos_x_hora_barplot/num_prom_gpos_x_hora_barplot.R")

##### Número promedio de alumnos por hora
source("fig_prom_alum_x_hora_barplot/prom_alum_x_hora_barplot.R")

##### Número de alumnos por grupo de todos los semestres
source("fig_num_alum_sem_par_impar_ts/num_alum_sem_par_impar_ts.R")

##### Número de alumnos por turno de todos los semestres
source("fig_num_alum_matu_vesp_ts/num_alum_matu_vesp_ts.R")

##### Densidades del número de alumnos por grupo de cada semestre
source("fig_densidades_num_alum_x_gpo_x_sem/densidades_num_alum_x_gpo_x_sem.R")

##### Densidad ajustada del tamaño de grupo
source("fig_densidad_num_alum_x_gpo_x_sem/densidad_num_alum_x_gpo_x_sem.R")

##### Histograma del número de alumnos de semestres pares e impares
source("fig_histograma_FR_num_alum_sem_par_impar/histograma_FR_num_alum_sem_par_impar.R")

##### Histograma del número de alumnos de los turnos matutino y vespertino
source("fig_histograma_FR_num_alum_matu_vesp/histograma_FR_num_alum_matu_vesp.R")

#####
# source("/.R")

#####
# source("/.R")

