##########################################################################
#' En este programa se encuentra el código que genera una asignación
#' completa de materias, profesores y horas.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo "Fn_schedule_simulation.R".
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Programs/R")
source("Fn_schedule_simulation.R")


# gen_asignacion_completa -------------------------------------------------
#' Title gen_asignacion_completa: Función que genera la asignación
#' completa de materias, profesores, horas. Utiliza el Algoritmo Genético.
#' Tiene la opción de leer un archivo de excel con grupos predefinidos.
#'
#' @param con_xlsx_1_sin_xlsx_0: Variable binaria que vale 1 si se debe
#' de leer el archivo xlsx y 0 si no.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @param param_sim: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se encargan de la simulación.
#'
#' @return list_asignacion_final: Lista de 12 elementos:
#' 1) mat_asignacion_final: Matriz de 3 columnas (Materia,Profesor,
#' Horario). Contiene la asignación final encontrada con el algoritmo
#' genético.
#' 2) calif_mejor_elem: Vector con calificaciones de los mejores elementos
#' por generación.
#' 3) mat_calif_generaciones: Matriz con calificaciones de todos los
#' elementos de todas las generaciones.
#' 4) matrices_calif_x_generacion: Lista de tamaño num_generaciones+1
#' con las matrices de calificaciones ordenadas por generación.
#' 5) mejores_asig: Lista de tamaño num_generaciones+1 con la información
#' de los mejores hijos de cada generación.
#' 6) mat_num_genes: Matriz con el número de genes de todos los elementos
#' por generación.
#' 7) mat_esqueleto
#' 8) mat_solicitudes_real
#' 9) param
#' 10) vec_info_AG: Vector con información del AG y sus resultados.
#' 11) esq_asig_final: Esqueleto de la asignación final.
#' 12) info_gpos_sin_asig: Matriz con las columnas: mat_esq (gpos. por
#' materia en mat_esqueleto), esq_asig_fin (gpos. x materia en
#' esq_asig_final), gpos_sin_asig (gpos. sin asignación x materia),
#' dif_rel (diferencia relativa x materia).
#'

con_xlsx_1_sin_xlsx_0 <- 1 #Leer excel
# con_xlsx_1_sin_xlsx_0 <- 0 #No leer excel

list_asignacion_final <- gen_asignacion_completa(con_xlsx_1_sin_xlsx_0,param,param_sim)
mat_asignacion_final <- list_asignacion_final[[1]]
View(mat_asignacion_final)

calif_mejor_elem <- list_asignacion_final[[2]]
mat_calif_generaciones <- list_asignacion_final[[3]]
matrices_calif_x_generacion <- list_asignacion_final[[4]]
mejores_asig <- list_asignacion_final[[5]]
mat_num_genes <- list_asignacion_final[[6]]
mat_esqueleto <- list_asignacion_final[[7]]
mat_solicitudes_real <- list_asignacion_final[[8]]
param <- list_asignacion_final[[9]]
vec_info_AG <- list_asignacion_final[[10]] #Vector con información del AG y sus resultados.
esq_asig_final <- list_asignacion_final[[11]] #Esqueleto de la asignación final.
info_gpos_sin_asig <- list_asignacion_final[[12]]


















