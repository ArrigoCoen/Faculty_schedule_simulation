##########################################################################
#' En este programa se encuentra la función que genera la asignación
#' completa de todas las materias.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")


# gen_asignacion_completa -------------------------------------------------
#' Title gen_asignacion_completa: Función que genera la asignación
#' completa de todas las materias. Utiliz el AG. Tiene la opción de leer
#' un archivo de excel con grupos predefinidos.
#'
#' @param con_xlsx_1_sin_xlsx_0: Variable binaria que vale 1 si se debe
#' de leer el archivo xlsx y 0 si no.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @param param_sim: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se encargan de la simulación.
#' 
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' @example param_sim <- list(vec_sem_sig = c(20191,20192,20201),k_sem_ant = 5,
#' materia = "Estadística III", num_sim = 10, m_filtrada = matrix(0),
#' sub_m_filtrada = matrix(0,ncol = length(param$nom_cols_MG)))
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
#' @examples
#' list_asignacion_final <- gen_asignacion_completa(con_xlsx_1_sin_xlsx_0,
#'                                                  param,param_sim)
#' 
gen_asignacion_completa <- function(con_xlsx_1_sin_xlsx_0,param,param_sim){
  ptm <- proc.time()# Start the clock!
  #' 1-3) Extracción de datos y simulación de alumnos de t+1
  # set.seed(8654)
  # set.seed(1806)
  # mat_demanda_alumnos <- gen_mat_demanda_alumnos(param,param_sim)#39.95 seg
  # View(mat_demanda_alumnos)
  
  #' 4a) Simulación de solicitudes de profesores del siguiente semestre (oculta)
  # set.seed(8654)
  # set.seed(1806)
  # mat_solicitudes <- gen_solicitudes(param)#7.78 seg
  # View(mat_solicitudes)
  
  #' 5) Simulación de esqueletos
  # set.seed(8654)
  # set.seed(9293)#5.76seg
  # set.seed(0802)#5.66seg
  # set.seed(236776)#4.82seg
  # set.seed(132934)#4.9seg
  # set.seed(0.1806)#5.39seg
  # n_rep <- 10 #12.45 min
  n_rep <- 5 #5.97/5.59 min
  set.seed(1806)
  lista_esq_D_prima <- metodo_B(n_rep,param,param_sim)#5.79 min
  mat_esqueleto <- lista_esq_D_prima[[1]]
  View(mat_esqueleto)
  
  #' 6) Calificación de esqueletos
  # califica_esqueleto()
  
  #' 7) AG aplicado a esqueletos: Aquí ya va a salir un buen esqueleto
  # AG_esqueleto()
  
  #' 4b) Simulación de solicitudes de profesores (pseudo-real)
  set.seed(1806)
  mat_solicitudes_real <- gen_solicitudes_real(mat_esqueleto,param)#8.3 seg
  View(mat_solicitudes_real)
  
  #' 8) Asignación
  # set.seed(1806)
  # lista_asignacion <- gen_asignacion(mat_esqueleto,mat_solicitudes_real,
  #                                    param)#22.66 seg
  # mat_asignacion <- lista_asignacion[[1]]
  # mat_esqueleto_aux <- lista_asignacion[[2]]
  # # mat_solicitud_aux <- lista_asignacion[[3]]
  # View(mat_asignacion)
  # View(mat_esqueleto_aux)
  # View(mat_solicitud_aux)
  
  #' 9) Calificación de asignación
  # set.seed(1806)
  # lista_calif_asignacion <- califica_asignacion(mat_solicitudes_real,
  #                                               lista_asignacion,
  #                                               param)#5.48 seg
  # mat_calif_asig_x_gpo <- lista_calif_asignacion[[1]]
  # (calif_asignacion <- lista_calif_asignacion[[2]])#-1083.836
  # View(mat_calif_asig_x_gpo)
  
  #' 10) AG aplicado a asignaciones: Aquí ya va a salir una buena asignación
  # n_rep <- 5 #5.97/5.59 min
  # set.seed(8654)
  # lista_esq_D_prima <- metodo_B(n_rep,param,param_sim)#5.79 min
  # mat_esqueleto_cotas <- lista_esq_D_prima[[1]]
  list_asignacion_final <- AG_asignaciones_con_xlsx(mat_esqueleto,
                                                    mat_solicitudes_real,
                                                    con_xlsx_1_sin_xlsx_0,
                                                    param)
  cat("\nLa función gen_asignacion_completa tardó: ",
      (proc.time()-ptm_generaciones)[3]/60," minutos. \n")
  return(list_asignacion_final)
}




# Ej. ---------------------------------------------------------------------
# con_xlsx_1_sin_xlsx_0 <- 1 #Leer excel
con_xlsx_1_sin_xlsx_0 <- 0 #No leer excel

list_asignacion_final <- gen_asignacion_completa(con_xlsx_1_sin_xlsx_0,
                                                 param,param_sim)
mat_asignacion_final <- list_asignacion_final[[1]]

View(mat_asignacion_final)