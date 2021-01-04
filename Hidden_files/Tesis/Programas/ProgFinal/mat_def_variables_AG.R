##########################################################################
#' En este programa se encuentra el código que guarda la matriz de 2
#' columnas: "mat_def_variables_AG". En la primera están los nombres de
#' las variables que se utilizan en el AG. En la segunda está su
#' explicación correspondiente.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")


# mat_def_variables_AG ----------------------------------------------------
#' Se guarda la matriz mat_def_variables_AG que tiene 2 columnas.En la
#' primera están los nombres de las variables que se utilizan en el AG. En
#' la segunda está su explicación correspondiente.




mat_def_variables_AG <- data.frame(Nom_variable = 0,Explicacion = 0)

mat_def_variables_AG <- rbind(mat_def_variables_AG,
                              c("mat_esqueleto",
                                "Matriz de txm con el número de grupos simulados para el siguiente semestre."))
mat_def_variables_AG <- rbind(mat_def_variables_AG,
                              c("mat_solicitudes_real",
                                "Matriz con las solicitudes pseudo-reales de los profesores."))
mat_def_variables_AG <- rbind(mat_def_variables_AG,
                              c("param",
                                "Lista con los parámetros generales utilizados al crear la matriz de asignaciones."))
mat_def_variables_AG <- rbind(mat_def_variables_AG,
                              c("tam_poblacion",
                                "Número de elementos que tiene cada generación. Se obtiene de param."))
mat_def_variables_AG <- rbind(mat_def_variables_AG,
                              c("num_generaciones",
                                "Número de generaciones a realizar en el AG. Se obtiene de param."))
mat_def_variables_AG <- rbind(mat_def_variables_AG,
                              c("prob_mutacion",
                                "Probabilidad de mutación de cada gen. Se obtiene de param."))
mat_def_variables_AG <- rbind(mat_def_variables_AG,
                              c("n_cols_mat_calif",
                                "Número de columnas para generar los heatmaps con las calificaciones ordenadas por generación. Se obtiene de param."))
mat_def_variables_AG <- rbind(mat_def_variables_AG,
                              c("matrices_calif_x_generacion",
                                "Lista de tamaño num_generaciones+1 con las matrices de calificaciones ordenadas por generación. Cada matriz de la lista tiene la calificación x gpo. de las asignaciones (como xiii de T45) y tiene n_cols_mat_calif columnas."))
mat_def_variables_AG <- rbind(mat_def_variables_AG,
                              c("vec_prob_asig",
                                "Vector de probabilidad para las asignaciones."))
mat_def_variables_AG <- rbind(mat_def_variables_AG,
                              c("mat_asignacion_final",
                                "Matriz con la asignación final."))
mat_def_variables_AG <- rbind(mat_def_variables_AG,
                              c("calif_mejor_elem",
                                "Vector con calificaciones de los mejores elementos por generación."))
mat_def_variables_AG <- rbind(mat_def_variables_AG,
                              c("mat_calif_generaciones",
                                "Matriz con calificaciones de todos los elementos de todas las generaciones."))
mat_def_variables_AG <- rbind(mat_def_variables_AG,
                              c("mejores_asig",
                                "Lista de tamaño num_generaciones+1 con la información de los mejores hijos de cada generación."))
mat_def_variables_AG <- rbind(mat_def_variables_AG,
                              c("mat_num_genes",
                                "Matriz con el número de genes de todos los elementos por generación."))
mat_def_variables_AG <- rbind(mat_def_variables_AG,
                              c("mat_info_AG",
                                "Tabla con información del AG y sus resultados."))
mat_def_variables_AG <- rbind(mat_def_variables_AG,
                              c("esq_asig_final",
                                "Esqueleto de la asignación final."))
mat_def_variables_AG <- rbind(mat_def_variables_AG,
                              c("info_gpos_sin_asig",
                                "Matriz con las columnas: mat_esq (gpos. x materia en mat_esqueleto), esq_asig_fin (gpos. x materia en esq_asig_final), gpos_sin_asig (gpos. sin asignación x materia), dif_rel (diferencia relativa x materia)."))
mat_def_variables_AG <- rbind(mat_def_variables_AG,
                              c("",
                                ""))



#Se quita el renglón inicial de ceros
mat_def_variables_AG <- mat_def_variables_AG[-1,]
View(mat_def_variables_AG)
save(mat_def_variables_AG, file = "mat_def_variables_AG.RData")

