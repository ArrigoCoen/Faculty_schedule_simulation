##########################################################################
##### PARÁMETROS INICIALES #####
## En este programa se encuentran las funciones que se requieren para la
##asignación de horarios y profesores para cada materia.
##########################################################################

# Inicio ------------------------------------------------------------------
# rm(list=ls())  # Borra variables
# cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
# setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Programas/Prog PRUEBAS V02")

# load("mat_def_columnas_MG.RData")
# View(mat_def_columnas_MG)
# load("mat_def_variables.RData")
# View(mat_def_variables)

# Se cargan los paquetes necesarios para algunas funciones que se van
# a utilizar.
# install.packages('zoo')
# install.packages('rvest')
# install.packages('dplyr')
# install.packages('purrr')
# install.packages('forecast')
# install.packages('xml2')
# install.packages('miceadds')
# install.packages('rJava')
# install.packages('xlsx')
# install.packages('astsa')
# install.packages('ggplot2')
# install.packages('knitr')
# install.packages('printr')
# install.packages('plyr')
# install.packages('lubridate')
# install.packages('gridExtra')
# install.packages('reshape2')
# install.packages('TTR')
# install.packages('randomcoloR')
# install.packages('manipulate')


#Loading packages
library('zoo')
library('rvest')
# library('dplyr')
library('purrr')
library('forecast')
library('xml2')
# library('miceadds')
library('stringr')
# library('xlsx')
library(RColorBrewer)
library(astsa, quietly=TRUE, warn.conflicts=FALSE)
library(ggplot2)
library(knitr)
library(printr)
library(plyr)
# library(dplyr)
library(lubridate)
library(gridExtra)
library(reshape2)
library(TTR)
library(randomcoloR)
library(manipulate)


# param -------------------------------------------------------------------
param <- list()
param$sem_ini = 20081##Inicio de información real
param$sem_fin = 20201##Fin de información real
param$sem_sig = 20202##Semestre de simulación
param$sem_totales = (20081:20202)[(20081:20202)%% 10>0 &(20081:20202)%% 10<3]
param$Semestres = (param$sem_ini:param$sem_fin)[(param$sem_ini:param$sem_fin) 
                                                %% 10>0 &(param$sem_ini:param$sem_fin) %% 10<3]
param$nombre_sem = as.character(param$Semestres)
param$n_semestres_anteriores = length(param$Semestres)
param$Horas = c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)
param$nombre_hrs = c("7-8","8-9","9-10","10-11","11-12",
                     "12-13","13-14","14-15","15-16",
                     "16-17","17-18","18-19","19-20",
                     "20-21","21-22")
param$q1 = 80
param$q2 = 90
param$num_simula_eleccion_materia = 6
param$num_simula_eleccion_horario = 6
param$nom_cols_m14 = c("Materia","Profesor","Horario","Lugares",
                       "Alumnos","Salon","Grupo","Carrera","Plan",
                       "Semestre","Cambios","Turno",
                       "Semestre_de_materia","url")
param$nom_cols_MG = c("Materia","Profesor","Horario","horario_num",
                      "Lugares","Alumnos","Salon","Grupo","Carrera",
                      "Plan","Semestre","Cambios","Turno",
                      "Semestre_de_materia","url","Act2000","Act2006",
                      "Act2015","CdC1994","CdC2013","Mat1983","MAp2017",
                      "NomMat_Act2000","NomMat_Act2006","NomMat_Act2015",
                      "NomMat_CdC1994","NomMat_CdC2013","NomMat_Mat1983",
                      "NomMat_MAp2017","URL_Act2000","URL_Act2006",
                      "URL_Act2015","URL_CdC1994","URL_CdC2013",
                      "URL_Mat1983","URL_MAp2017","Num_materia")
param$m_grande_total = matrix(0,ncol = length(param$nom_cols_MG))
param$vec_nom_materias_total = 0


param_sim <- list()
param_sim$vec_sem_sig = c(20191,20192,20201)
param_sim$k_sem_ant = 5##Se inicia en 2016-2, 2017-1, 2017-2
# param_sim$vec_k_sem_info = rep(0,param_sim$k_sem_ant)
param_sim$Materias = "Estadística III" ##Puede ser una o más materias
param_sim$num_sim = 10
param_sim$m_filtrada = matrix(0,ncol = length(param$nom_cols_MG))
param_sim$sub_m_filtrada = matrix(0,ncol = length(param$nom_cols_MG))
# param_sim$vec_q1 = c(80,90,99)
# param_sim$vec_q2 = c(85,95,99)
# param_sim$vec_q = c(80,85,95,99)
param_sim$posibles_comb_q = matrix(c(80,85,95,99),ncol = 2,byrow = T)

load(file = paste0("Matrices m_grande_total/m_grande_total_",
                   param$sem_ini,"_",param$sem_fin,".RData"))
param$m_grande_total = m_grande_total

load(file = "vec_nom_materias_total.RData")
param$vec_nom_materias_total = vec_nom_materias_total

# param_graficas <- list()
# # param_graficas$color_barras = rgb(91,155,213)##Azul como excel
# # param_graficas$col_barras = '#5b9bd5' ##Azul como excel
# param_graficas$col_barras = "skyblue" ##Azul cielo
# param_graficas$col1_hist = rgb(0,0,1,1/4)##Azul histogramas
# param_graficas$col2_hist = rgb(1,0,0,1/4)##Rojo histogramas
# param_graficas$col1_linea = "blue" ##Azul densidad
# param_graficas$col2_linea = "red" ##Rojo densidad
# param_graficas$ancho_pdf = 8 #Anchura para guardar imagen
# param_graficas$altura_pdf = 6 #Altura para guardar imagen
# param_graficas$dir_TeX = "TeX/TeX V05/Pictures/"



# mat_def_columnas_MG -----------------------------------------------------
## Se guarda la matriz mat_def_columnas_MG que tiene 3 columnas (Nombre,
##Número, Descripción) y tantos renglones como columnas tenga la matriz
##m_grande. Contiene la información de las columnas de m_grande.

mat_def_columnas_MG <- matrix(0,ncol = 3,nrow = 37)
colnames(mat_def_columnas_MG) <- c("Nombre","Número","Descripción")
mat_def_columnas_MG[,1] <- c("Materia","Profesor","Horario","horario_num","Lugares",
                             "Alumnos","Salon","Grupo","Carrera","Plan","Semestre",
                             "Cambios","Turno","Semestre_de_materia","url","Act2000",
                             "Act2006","Act2015","CdC1994","CdC2013","Mat1983","MAp2017",
                             "NomMat_Act2000","NomMat_Act2006","NomMat_Act2015",
                             "NomMat_CdC1994","NomMat_CdC2013","NomMat_Mat1983",
                             "NomMat_MAp2017","URL_Act2000","URL_Act2006",
                             "URL_Act2015","URL_CdC1994","URL_CdC2013",
                             "URL_Mat1983","URL_MAp2017","Num_materia")
mat_def_columnas_MG[,2] <- 1:37
mat_def_columnas_MG[,3] <- c("Nombre del curso impartido",
                             "Nombre de la persona que va a impartir alguna materia",
                             "Hora en la que se imparte alguna materia",
                             "Valores de la columna Horario en variables tipo numeric",
                             "Espacios disponibles por salón",
                             "Número de estudiantes inscritos por grupo",
                             "Espacio físico en el que se imparte alguna materia",
                             "Clave con la que se identifica una asignación",
                             "Nombre de alguna carrera de FC",
                             "Año en el que se implemento un nuevo plan de estudios",
                             "Semestre al que pertenece la materia (Año y semestre)",
                             "Clave que indica los cambios que se le han hecho al grupo",
                             "Matutino: 7:00-14:00hrs, Vespertino: 15:00-21:00",
                             "Semestre en el que el plan de estudios dicta que se lleva esa materia",
                             "Nombre de la página de los horarios de FC correspondiente al grupo",
                             "Columna binaria, tiene un 1 si el grupo pertenece a la carrera de Actuaría, plan 2000 y hay un 0 e.o.c.",
                             "Columna binaria, tiene un 1 si el grupo pertenece a la carrera de Actuaría, plan 2006 y hay un 0 e.o.c.",
                             "Columna binaria, tiene un 1 si el grupo pertenece a la carrera de Actuaría, plan 2015 y hay un 0 e.o.c.",
                             "Columna binaria, tiene un 1 si el grupo pertenece a la carrera de Ciencias de la Computación, plan 1994 y hay un 0 e.o.c.",
                             "Columna binaria, tiene un 1 si el grupo pertenece a la carrera de Ciencias de la Computación, plan 2013 y hay un 0 e.o.c.",
                             "Columna binaria, tiene un 1 si el grupo pertenece a la carrera de Matemáticas, plan 1983 y hay un 0 e.o.c.",
                             "Columna binaria, tiene un 1 si el grupo pertenece a la carrera de Matemáticas Aplicadas, plan 2017 y hay un 0 e.o.c.",
                             "Indica el nombre de las materia correspondiente a la carrera de Actuaría plan 2000",
                             "Indica el nombre de las materia correspondiente a la carrera de Actuaría plan 2006",
                             "Indica el nombre de las materia correspondiente a la carrera de Actuaría plan 2015",
                             "Indica el nombre de las materia correspondiente a la carrera de Ciencias de la Computación plan 1994",
                             "Indica el nombre de las materia correspondiente a la carrera de Ciencias de la Computación plan 2013",
                             "Indica el nombre de las materia correspondiente a la carrera de Matemáticas plan 1983",
                             "Indica el nombre de las materia correspondiente a la carrera de Matemáticas Aplicadas plan 2017",
                             "Indica la URL correspondiente a la carrera de Actuaría plan 2000",
                             "Indica la URL correspondiente a la carrera de Actuaría plan 2006",
                             "Indica la URL correspondiente a la carrera de Actuaría plan 2015",
                             "Indica la URL correspondiente a la carrera de Ciencias de la Computación plan 1994",
                             "Indica la URL correspondiente a la carrera de Ciencias de la Computación plan 2013",
                             "Indica la URL correspondiente a la carrera de Matemáticas plan 1983",
                             "Indica la URL correspondiente a la carrera de Matemáticas Aplicadas plan 2017",
                             "Número de materia con respecto al vector que contiene el nombre de las materias")
# View(mat_def_columnas_MG)
save(mat_def_columnas_MG, file = "mat_def_columnas_MG.RData")


# guarda_mat_def_grupos_simulados ----------------------------------------------
# Se guarda la matriz "mat_def_grupos_simulados" la cual contiene los nombres de
#las columnas de la matriz "mat_simula_grupos", en su primer columna, en la
#segunda columna se tiene el número de columna y en la tercera tiene sus
#respectivas descripciones.
mat_def_grupos_simulados <- matrix(0,ncol = 3,nrow = 6)
colnames(mat_def_grupos_simulados) <- c("Nombre","Número","Descripción")
mat_def_grupos_simulados[,1] <- c("Materia","Horario","Grupos_Simulados",
                                  "Alumnos_Simulados_Totales","col_1er_grupo",
                                  "col_ult_grupo")
mat_def_grupos_simulados[,2] <- c(1:5,24)
mat_def_grupos_simulados[,3] <- c("Nombre del curso impartido",
                                  "Hora en la que se imparte alguna materia",
                                  "Número de grupos que se simularon de acuerdo al número de alumnos simulados",
                                  "Número de alumnos simulados por materia y por hora",
                                  "Número de columna en la que se encuentra la información del número de alumnos simulados del primer grupo",
                                  "Número de columna en la que se encuentra la información del número de alumnos simulados del último grupo")
# View(mat_def_grupos_simulados)
save(mat_def_grupos_simulados, file = "mat_def_grupos_simulados.RData")


# guarda_mat_def_grupos_reales ----------------------------------------------
# Se guarda la matriz "mat_def_grupos_reales" la cual contiene los nombres de
#las columnas de la matriz "mat_real_grupos", en su primer columna, en la
#segunda columna se tiene el número de columna y en la tercera tiene sus
#respectivas descripciones.
mat_def_grupos_reales <- matrix(0,ncol = 3,nrow = 6)
colnames(mat_def_grupos_reales) <- c("Nombre","Número","Descripción")

mat_def_grupos_reales[,1] <- c("Materia","Horario","Grupos_Reales",
                               "Alumnos_Reales_Totales","col_1er_grupo",
                               "col_ult_grupo")

mat_def_grupos_reales[,2] <- c(1:5,24)

mat_def_grupos_reales[,3] <- c("Nombre del curso impartido",
                               "Hora en la que se imparte alguna materia",
                               "Número de grupos reales que se tuvieron por materia y por hora, en cada semestre",
                               "Número de alumnos reales por materia y por hora",
                               "Número de columna en la que se encuentra la información del número de alumnos reales del primer grupo",
                               "Número de columna en la que se encuentra la información del número de alumnos reales del último grupo")

# View(mat_def_grupos_reales)
save(mat_def_grupos_reales, file = "mat_def_grupos_reales.RData")



# mat_def_variables -----------------------------------------------------
## Se guarda la matriz mat_def_variables que tiene 2 columnas (Nombre,
##Descripción) y tantos renglones como variables se utilicen para la 
##asignación, contiene la información de dichas variables.

mat_def_variables <- matrix(0,ncol = 2,nrow = 15)
colnames(mat_def_variables) <- c("Nombre","Descripción")
mat_def_variables[,1] <- c("sem_ini","sem_fin","list_url","sem_info",
                           "n_semestres_anteriores","directorio_info","param",
                           "m_grande","m_grande_total","mat_esqueleto",
                           "mat_solicitudes","url","mat_info_k_pag","materia",
                           "profesor")
mat_def_variables[,2] <- c("Semestre en el que se inicia la búsqueda de información.",
                           "Semestre en el que se finaliza la búsqueda de información para generar la asignación del siguiente semestre.",
                           "Lista con las variables globales del programa.",
                           "Semestre del que se desea obtener información.",
                           "Variable tipo integer que indica el número de semestres anteriores al semestre actual para poder generar el esqueleto.",
                           "Vector que contiene la ubicación y el nombre de los archivos tipo .Rdata en donde se hayan guardado las matrices que contienen la información de los semestres que se requieren para obtener el esqueleto.",
                           "Lista con los diferentes parámetros que se utilizan en las funciones que se mandan llamar.",
                           "Matriz de 22 columnas (Materia,Profesor,Horario,horario_num,Lugares,Alumnos,Salon,Grupo,Carrera,Plan,Semestre,Cambios,Turno,Semestre_de_materia,url,Act2000,Act2006,Act2015,CdC1994,CdC2013,Mat1983,MAp2017), con la información de cada semestre. Las últimas 7 columnas son columnas binarias las cuales indican con un 1 si el grupo del i-ésimo renglón pertenece a la carrera y plan correspondiente al nombre de cada columna, hay un 0 e.o.c.",
                           "Matriz m_grande con la información de todos los semestres",
                           "Matriz de 15 renglones con las horas (7-8,8-9,...,21-22) y tantas columnas como materias.",
                           "Matriz de 12 columnas que contiene la información de las solicitudes de materia y de horario de todos los profesores, en las primeras 6 columnas se tiene la información de la simulación de elección de materias y en las últimas 6 columnas se tiene la información de la simulación de elección de horarios, la matriz puede no estar completamente llena),tiene como renglones los nombres de los profesores.",
                           "Página de internet correspondiente a algún horario de la FC",
                           "Matriz de 14 columnas (Materia,Profesor,Horario,Lugares,Alumnos,Salon,Grupo,Carrera,Plan,Semestre,Cambios,Turno,Semestre_de_materia,url), con la información de cada semestre.",
                           "Nombre de algún curso impartido en la FC",
                           "Nombre de algún profesor de la Facultad de Ciencias")
# View(mat_def_variables)
save(mat_def_variables, file = "mat_def_variables.RData")


# Valida_list_url ---------------------------------------------------------
#' Validador de list_url
#' @param list_url lista con las variables globales
#' @param sem_ini semestre inicial de información; eg. 20192
#' @param sem_fin semestre final de información; eg. 20201
#' @param sem_actual semestre actual de información; eg. 20201
#' @param Actualiza_RAW_url indicadora si se actualiza la matriz mat_RAW_url;
#'   utlizado por función Actualiza_list_url
#' @param Actualiza_limpia_base_url indicadora de limpiar la matriz
#'   mat_posibles_url; utlizado por función Actualiza_list_url par llamar a la
#'   función limpia_base_url
#' @param Actualiza_elimina_grupos_con_0
#' @param Salvar_URL_RData
#' @param usar_vec_corto_num_materia
#' @param planes_estudio
#' @param file_name
#' @param file_name_RAW
#' @param nombres_carrera_plan
#' @param mat_ubicaciones_url matriz con las ubicaciones de cada elemento que se
#'   extrae de una url
#' @param colnames_mat_posibles_url
#' @param ncol_mat_posibles_url número de columnas de matriz mat_posibles_url
#' @param nrow_mat_posibles_url número de renglones de matriz mat_posibles_url
#' @param mat_RAW_url
#' @param mat_posibles_url
#' @param utilizar_RAW_anterior indicadora si se utiliza un archivo ya
#'   existente; utilizada por función posibles_url
#' @param mat_Grande
#' @param semestres_reales
#' @param num_grupos
#' @param url_con_salon
#' @param plan_reales
#' @param num_mat_reales
#' @param indicadoras_actualiza_col_j_mat_Grande indicadora que utiliza la
#'   función XXX para saber que columnas de mat_Grande se deben de actualizar
#' @param mat_paginas_error matriz que guarda el historial de póginas con errores
#' @param elimina_pags_con_0_grupos se borran las páginas que no tienen grupos
#' @param usa_grupos_salvados indicadora de usar el archivo .RData con grupos salvados de cada página
#' @param usa_vec_con_salon indicadora de usar el archivo .RData con variable vec_con_salon
#' @param Carpeta_RData 
#' @param usa_vec_con_info_salvados indicadora de usar el archivo .RData con variable vec_con_info_salvados
#'
#' @return
#'
#' @examples
Valida_list_url <- function(list_url,
                            sem_ini = c(20151,20081,20172,20192)[4], # 1 = Super GRANDE,..., 4 = chica
                            sem_fin = 20201,
                            sem_actual = 20201,
                            Actualiza_RAW_url = TRUE,
                            Actualiza_limpia_base_url = TRUE,
                            Actualiza_elimina_grupos_con_0 = TRUE,
                            Salvar_URL_RData = TRUE,
                            usar_vec_corto_num_materia = TRUE,
                            elimina_pags_con_0_grupos = TRUE,
                            Carpeta_RData = "Archivos RData V01",
                            usa_grupos_salvados = TRUE,
                            usa_vec_con_salon = TRUE,
                            usa_vec_con_info_salvados = TRUE,
                            planes_estudio = c(119,1176,2017,218,1556,217,2055),
                            file_name = paste0("Dat_URL_",list_url$sem_ini,"_",list_url$sem_fin,".RData"),
                            file_name_RAW = paste0("Lista_RAW_",list_url$file_name),
                            nombres_carrera_plan = c("Actuaría (plan 2000)",
                                                     "Actuaría (plan 2006)",
                                                     "Actuaría (plan 2015)",
                                                     "Ciencias de la Computación (plan 1994)",
                                                     "Ciencias de la Computación (plan 2013)",
                                                     "Matemáticas (plan 1983)",
                                                     "Matemáticas Aplicadas (plan 2017)"),
                            mat_ubicaciones_url = matrix(c("Materia"            ,'#info-contenido h2', T,F,
                                                           "Profesor"           ,'tr:nth-child(1) td:nth-child(2) a',F,F,
                                                           "Horario"            ,'tr:nth-child(1) td:nth-child(4)',F,T,
                                                           "Lugares"            ,'#info-contenido div',F,F,
                                                           "Alumnos"            ,'#info-contenido div',F,F,
                                                           "Salon"              ,'tr:nth-child(1) td~ td+ td a , td:nth-child(4) a',F,F,
                                                           "Grupo"              ,'#info-contenido div',F,F,
                                                           "Carrera"            ,'h1',T,F,
                                                           "Plan"               ,'h1',T,F,
                                                           "Semestre"           ,-1,-1,-1,### FALTA POR HACER
                                                           "Cambios"            ,-1,-1,-1,### FALTA POR HACER
                                                           "Turno"              ,-1,-1,-1,### FALTA POR HACER
                                                           "Semestre_de_materia",'#info-contenido h2',T,F,
                                                           "Grupos_x_pag"       ,'strong',F,F,
                                                           "Grupo_paralelo"     ,'em',F,F),ncol=4,byrow = T),
                            # colnames(list_url$mat_ubicaciones_url) = c("Nombre columna","Ubicacion en pagina","Repetir","Elimina salto"),
                            colnames_mat_posibles_url = c("Semestre","Plan","Materia","URL","Grupos x pag","url_con_salon"),
                            ncol_mat_posibles_url = length(list_url$colnames_mat_posibles_url),
                            nrow_mat_posibles_url = 20000,
                            mat_RAW_url = matrix(0,list_url$nrow_mat_posibles_url,list_url$ncol_mat_posibles_url),
                            mat_posibles_url = list_url$mat_RAW_url,
                            utilizar_RAW_anterior = T,
                            mat_Grande = matrix(0,sum(as.numeric(list_url$mat_posibles_url[,5])),13),
                            indicadoras_actualiza_col_j_mat_Grande = rep(T,13),
                            semestres_reales = NA,
                            num_grupos = NA,
                            url_con_salon = NA,
                            # colnames(list_url$mat_Grande) = c("Materia", "Profesor","Horario","Lugares","Alumnos","Salon",
                            #                                    "Grupo","Carrera","Plan","Semestre","Cambios","Turno","Semestre_de_materia"),
                            plan_reales = NA,
                            num_mat_reales = NA,
                            mat_paginas_error = matrix(0,1,4)) {
  # EJEMPLO DE list_url
  list_url_EJEMPLO <- list()
  list_url_EJEMPLO$sem_ini = 20192 # Datos Chica 
  list_url_EJEMPLO$sem_fin = 20201
  list_url_EJEMPLO$sem_actual = 20201
  list_url_EJEMPLO$Actualiza_RAW_url = TRUE
  list_url_EJEMPLO$Actualiza_limpia_base_url = TRUE
  list_url_EJEMPLO$Actualiza_elimina_grupos_con_0 = TRUE
  list_url_EJEMPLO$Salvar_URL_RData = TRUE
  list_url_EJEMPLO$usar_vec_corto_num_materia = TRUE
  list_url_EJEMPLO$elimina_pags_con_0_grupos = TRUE
  list_url_EJEMPLO$Carpeta_RData = "Archivos RData V01"
  list_url_EJEMPLO$usa_grupos_salvados = TRUE
  list_url_EJEMPLO$usa_vec_con_salon = TRUE
  list_url_EJEMPLO$usa_vec_con_info_salvados = TRUE
  list_url_EJEMPLO$planes_estudio = c(119,1176,2017,218,1556,217,2055)
  list_url_EJEMPLO$file_name <- paste0("Dat_URL_",list_url$sem_ini,"_",list_url$sem_fin,".RData")
  list_url_EJEMPLO$file_name_RAW <- paste0(list_url$Carpeta_RData,"/Lista_RAW_Dat_URL_",list_url$sem_ini,"_",list_url$sem_fin,".RData")
  list_url_EJEMPLO$nombres_carrera_plan <- c("Actuaría (plan 2000)",
                                             "Actuaría (plan 2006)",
                                             "Actuaría (plan 2015)",
                                             "Ciencias de la Computación (plan 1994)",
                                             "Ciencias de la Computación (plan 2013)",
                                             "Matemáticas (plan 1983)",
                                             "Matemáticas Aplicadas (plan 2017)")
  list_url_EJEMPLO$mat_ubicaciones_url <- matrix(c("Materia"            ,'#info-contenido h2', T,F,
                                                   "Profesor"           ,'tr:nth-child(1) td:nth-child(2) a',F,F,
                                                   "Horario"            ,'tr:nth-child(1) td:nth-child(4)',F,T,
                                                   "Lugares"            ,'#info-contenido div',F,F,
                                                   "Alumnos"            ,'#info-contenido div',F,F,
                                                   "Salon"              ,'tr:nth-child(1) td~ td+ td a , td:nth-child(4) a',F,F,
                                                   "Grupo"              ,'#info-contenido div',F,F,
                                                   "Carrera"            ,'h1',T,F,
                                                   "Plan"               ,'h1',T,F,
                                                   "Semestre"           ,-1,-1,-1,### FALTA POR HACER
                                                   "Cambios"            ,-1,-1,-1,### FALTA POR HACER
                                                   "Turno"              ,-1,-1,-1,### FALTA POR HACER
                                                   "Semestre_de_materia",'#info-contenido h2',T,F,
                                                   "Grupos_x_pag"       ,'strong',F,F,
                                                   "Grupo_paralelo"     ,'em',F,F),ncol=4,byrow = T)
  list_url_EJEMPLO$colnames_mat_posibles_url <- c("Semestre","Plan","Materia","URL","Grupos x pag","url_con_salon")
  list_url_EJEMPLO$ncol_mat_posibles_url <- length(list_url_EJEMPLO$colnames_mat_posibles_url)
  list_url_EJEMPLO$nrow_mat_posibles_url <- 20000
  list_url_EJEMPLO$mat_RAW_url <- matrix(0,list_url_EJEMPLO$nrow_mat_posibles_url,list_url_EJEMPLO$ncol_mat_posibles_url)
  list_url_EJEMPLO$mat_posibles_url <- list_url_EJEMPLO$mat_RAW_url
  list_url_EJEMPLO$utilizar_RAW_anterior <- T
  
  list_url_EJEMPLO$ncol_mat_Grande <- 13
  list_url_EJEMPLO$mat_Grande <- matrix(0,1,list_url_EJEMPLO$ncol_mat_Grande)
  list_url_EJEMPLO$mat_Grande_con_url <- matrix(0,1,list_url_EJEMPLO$ncol_mat_Grande+1)
  
  list_url_EJEMPLO$semestres_reales <- NA
  list_url_EJEMPLO$plan_reales <- NA
  list_url_EJEMPLO$num_mat_reales <- NA
  list_url_EJEMPLO$num_grupos <- NA
  list_url_EJEMPLO$url_con_salon <- NA
  
  list_url_EJEMPLO$mat_paginas_error <- matrix(0,1,4)
  list_url_EJEMPLO$indicadoras_actualiza_col_j_mat_Grande = rep(T,13)
  
  #  Nombres de columnas
  colnames(list_url_EJEMPLO$mat_ubicaciones_url) = c("Nombre columna","Ubicacion en pagina","Repetir","Elimina salto")
  colnames(list_url_EJEMPLO$mat_Grande) = c("Materia", "Profesor","Horario","Lugares","Alumnos","Salon",
                                            "Grupo","Carrera","Plan","Semestre","Cambios","Turno","Semestre_de_materia")
  colnames(list_url_EJEMPLO$mat_paginas_error) <- c("Columna","length(vec)","num_gpo","Pagina")
  
  # INICIO DE VALIDACIONES
  error1_bien0 <- 0
  # if(length(list_url)!=length(list_url_EJEMPLO)){
  if(length(setdiff(names(list_url_EJEMPLO),names(list_url)))>0 || length(setdiff(names(list_url),names(list_url_EJEMPLO))>0)) {
    error1_bien0 <- error1_bien0 +1
    cat("Error",error1_bien0,":\n\t","¡ERROR EN VARIABLES DE list_url!\n")
    cat("Faltan las variables:\n\t",setdiff(names(list_url_EJEMPLO),names(list_url)),"\n",
        "Sobran las variables:\n\t",setdiff(names(list_url),names(list_url_EJEMPLO)),"\n\n")
  }
  
  if(list_url$ncol_mat_Grande != list_url_EJEMPLO$ncol_mat_Grande) {
    error1_bien0 <- error1_bien0 +1
    cat("Error",error1_bien0,":\n\t variable ncol_mat_Grande incorrecta \n",
        "Deberían ser ",list_url_EJEMPLO$ncol_mat_Grande," y vale ",
        list_url$ncol_mat_Grande,"\n")
  } 
  
  if(is.null(list_url$mat_Grande)) {
    error1_bien0 <- error1_bien0 +1
    cat("Error: la matriz mat_Grande es NULL \n")
  } else if(ncol(list_url$mat_Grande) != ncol(list_url_EJEMPLO$mat_Grande)) {
    error1_bien0 <- error1_bien0 +1
    cat("Error en el número de columnas de list_url$mat_Grande \n")
  }
  
  if(any(dim(list_url$mat_ubicaciones_url) != dim(list_url_EJEMPLO$mat_ubicaciones_url))) {
    error1_bien0 <- error1_bien0 +1
    cat("Error",error1_bien0,":\n\t números incorrectos en las dimensiones de mat_ubicaciones_url \n",
        "Deberían ser ",dim(list_url_EJEMPLO$mat_ubicaciones_url)," y son",
        dim(list_url$mat_ubicaciones_url),"\n")
  } else if(!all.equal(list_url$mat_ubicaciones_url,list_url_EJEMPLO$mat_ubicaciones_url)){
    cat("¡ERROR EN ENTRADAS DE mat_ubicaciones_url \n")
    error1_bien0 <- error1_bien0 +1
  }
  
  
  if(list_url$file_name != paste0(list_url$Carpeta_RData,"/Dat_URL_",list_url$sem_ini,"_",list_url$sem_fin,".RData")) {
    error1_bien0 <- error1_bien0 +1
    cat("Error",error1_bien0,":\n\t file_name incorrecto, debería ser:\n\t",
        paste0(list_url$Carpeta_RData,"/Dat_URL_",list_url$sem_ini,"_",list_url$sem_fin,".RData"),
        "\n pero es:\n\t",
        list_url$file_name,"\n")
  }
  
  
  if(list_url$file_name_RAW != paste0(list_url$Carpeta_RData,"/Lista_RAW_Dat_URL_",list_url$sem_ini,"_",list_url$sem_fin,".RData")) {
    error1_bien0 <- error1_bien0 +1
    cat("Error",error1_bien0,":\n\t file_name_RAW incorrecto, debería ser:\n\t",
        paste0(list_url$Carpeta_RData,"/Lista_RAW_Dat_URL_",list_url$sem_ini,"_",list_url$sem_fin,".RData"),
        "\n pero es:\n\t",
        list_url$file_name_RAW,"\n")
  }
  
  if(!all(colnames(list_url$mat_Grande)==colnames(list_url_EJEMPLO$mat_Grande))) {
    error1_bien0 <- error1_bien0 +1
    cat("Error",error1_bien0,":\n\t error en nombres de las columnas de list_url$mat_Grande \n")
  }
  
  if(nrow(list_url$mat_Grande)!=sum(as.numeric(list_url$mat_posibles_url[,5]))) {
    error1_bien0 <- error1_bien0 +1
    cat("Error",error1_bien0,":\n\t el número de columnas de mat_Grande no coincide con la suma de todos los grupos:\n",
        "nrow(list_url$mat_Grande) = ",nrow(list_url$mat_Grande)," y debería ser ",
        sum(as.numeric(list_url$mat_posibles_url[,5]))," (la suma de la columna 5 de mat_posibles_url)\n")
  }
  
  if(nrow(list_url$mat_Grande_con_url)!=sum(as.numeric(list_url$mat_posibles_url[,5]))) {
    error1_bien0 <- error1_bien0 +1
    cat("Error",error1_bien0,":\n\t el número de columnas de mat_Grande_con_url no coincide con la suma de todos los grupos:\n",
        "nrow(list_url$mat_Grande) = ",nrow(list_url$mat_Grande_con_url)," y debería ser ",
        sum(as.numeric(list_url$mat_posibles_url[,5]))," (la suma de la columna 5 de mat_posibles_url)\n")
  }
  
  if(nrow(list_url$mat_posibles_url)!=length(list_url$num_grupos)) {
    error1_bien0 <- error1_bien0 +1
    cat("Error",error1_bien0,":\n\t número de renglones de mat_posibles_url no coincide con longitud de num_grupos\n",
        "nrow(list_url$mat_posibles_url) = ",nrow(list_url$mat_posibles_url)," y \n",
        "length(list_url$num_grupos)=",length(list_url$num_grupos),"\n")
  }
  
  if(error1_bien0 >0) {
    cat(" *** EXISTEN",error1_bien0,"ERRORES EN list_url *** \n")
    return(F)
  } else {
      cat(" *** La variable list_url es adecuada ***\n")
    return(T)
  }
}



# My_plot_progress --------------------------------------------------------
#' Plot de barras de progreso
#'
#' @param percentages vector de valores numericos entre cero y uno para graficar
#' @param names vector con nombres de cada barra
#' @param title_plot titulo del plot
#'
#' Ver https://www.r-bloggers.com/multiple-progress-bars/
#' @return
#' @export
#'
#' @examples
#' percentages <- runif(3)
#' names=c("uno","dos","tres")
#' title_plot <- "avance"
#' My_plot_progress(percentages,names,title_plot)
My_plot_progress <- function(percentages,names_percentages,title_plot)	{
  percentages <- rev(percentages)
  names_percentages <- rev(names_percentages)
  vectOfBar <- c(percentages)*100
  numOfBar <- length(vectOfBar)
  plot(c(0,100), c(0,numOfBar), type='n', xlab='', ylab='', yaxt='n', mar=c(3,3,3,3))
  for(i in 1:numOfBar) {
    rect(0, 0.1+i-1, vectOfBar[i], 0.9+i-1, col=rainbow(numOfBar)[i])
    # text(0.5, 0.5+i-1, paste('Status ', i, ': ', round(vectOfBar[i],2), '%', sep=''), adj=0)
    text(0.5, 0.5+i-1, paste(names_percentages[i],': ', round(vectOfBar[i],2), '%', sep=''), adj=0)
  }
  title(title_plot)
}

##########################################################################
##### MATRIZ CON POSIBLES URL #####
## Funciones que generan la lista de posibles URL de donde se va a extraer
##la información de las páginas de la facultad
##########################################################################


# posibles_url ------------------------------------------------------------
#' Title: posibles_url: Función que arroja la lista "list_url"  dentro de
#' la cual se encuentra la matriz con las posibles URL de las páginas de
#' horarios de la FC. Dicha matriz tiene 6 columnas: Semestre, Plan,
#' Materia, URL, Grupos por página, url_con_salon. Las últimas 2 columnas
#' se llenan con la función "Actualiza_list_url"
#'
#' Obs: para 20081-20201 tarda 41 minutos, 20151-20201 tarda 18 minutos,
#' 20172-20201 tarda 9 minutos y 20192-20201 tarda 3 min
#' 
#' @param sem_ini Número que representa el semestre en el cual se desea iniciar
#'   la búsqueda de información.
#' @param sem_fin  Número que representa el semestre en el cual se desea
#'   finalizar la búsqueda de información.

#' @param intervalo_num_materia indicador si se utiliza el vector corto o si si
#'   hace una búsqueda en un vector más grande
#'
#' @example sem_ini <- 20081
#' @example sem_fin <- 20201
#'
#' @return mat_posibles_url: Matriz con 4 columnas: 1) Semestre 2) Plan 3)
#'   Número de materia 4) Posibles url
posibles_url = function(list_url){
  
  if(!Valida_list_url(list_url)){
    return(list_url)
  }
  
  # En caso de ya existir el archivo con la matriz RAW
  if(file.exists(list_url$file_name_RAW) && list_url$utilizar_RAW_anterior){
    list_url_nueva <- list_url
    # Cargando matriz RAW ya existente
    load(list_url$file_name_RAW)
    Sys.sleep(1)
    # Actualizando variables que actualizaría esta función
    nrow_anterior <- nrow(list_url$mat_RAW_url[,1:4])
    list_url_nueva$mat_RAW_url = cbind(list_url$mat_RAW_url[,1:4],
                                       rep(0,nrow_anterior),
                                       rep(0,nrow_anterior))
    list_url_nueva$mat_posibles_url = cbind(list_url$mat_posibles_url[,1:4],
                                            rep(0,nrow_anterior),
                                            rep(0,nrow_anterior))
    list_url_nueva$semestres_reales = list_url$semestres_reales
    list_url_nueva$plan_reales = list_url$plan_reales
    list_url_nueva$num_mat_reales = list_url$num_mat_reales
    
    list_url_nueva$nrow_mat_posibles_url <- nrow(list_url_nueva$mat_posibles_url)
    
    if(length(list_url_nueva$num_grupos)!=nrow(list_url_nueva$mat_posibles_url)) {
      cat("Se borró la información de num_grupos por no ser de la longitud correcta:\n",
          "length(num_grupos) = ",length(list_url_nueva$num_grupos),
          "nrow(mat_posibles_url) = ",nrow(list_url_nueva$mat_posibles_url),"\n\n")
      list_url_nueva$num_grupos = rep(-1, nrow(list_url_nueva$mat_posibles_url))
      
    }
    
    # Guardamos la información 
    list_url  <- list_url_nueva
    # Corregimos mat_Grande
    if(nrow(list_url$mat_Grande) == 0) {
      list_url$mat_Grande <- matrix(-1,sum(as.numeric(list_url$mat_posibles_url[,5])),
                                    list_url$ncol_mat_Grande)
      colnames(list_url$mat_Grande) <- c("Materia", "Profesor","Horario","Lugares",
                                         "Alumnos","Salon","Grupo","Carrera","Plan",
                                         "Semestre","Cambios","Turno",
                                         "Semestre_de_materia")
    }
    
    cat("Se utilizará la matriz RAW ya existente del archivo:","\n\n",
        list_url$file_name_RAW,"\n\n")
    return(list_url)
  }
  
  sem_ini <- list_url$sem_ini
  sem_fin <- list_url$sem_fin
  # usar_vec_corto_num_materia <- list_url$usar_vec_corto_num_materia
  
  # Start the clock!
  ptm <- proc.time()
  ## Sólo se van a tomar en cuenta los planes de estudio vigentes
  planes_estudio = list_url$planes_estudio
  
  #Inicializamos las variables:
  
  if(list_url$usar_vec_corto_num_materia) {
    load(paste0(list_url$Carpeta_RData,"/Datos_INTERVALO_NUM_MATERIA.RData"))
  } else intervalo_num_materia = 1:2000 # Obs: el más grande encontrado fue 1841
  
  
  # intervalo_num_materia = 800:815
  mat_posibles_url = matrix(0, nrow = list_url$nrow_mat_posibles_url,
                            ncol = list_url$ncol_mat_posibles_url)
  colnames(mat_posibles_url) <- list_url$colnames_mat_posibles_url
  
  ## Se crea el vector para los semestres pares e impares
  (semestres = (list_url$sem_ini:list_url$sem_fin)[(list_url$sem_ini:list_url$sem_fin) 
                                                   %% 10>0 &
                                                     (list_url$sem_ini:list_url$sem_fin) 
                                                   %% 10<3])
  
  semestres_reales <- c(NULL)
  plan_reales <- c(NULL)
  num_mat_reales <- c(NULL)
  i_sem <- 1
  i_plan <- 1
  i_num_materia <- 1
  title_plot <- "Progreso de extracción url"
  
  i = 0
  for(i_sem in 1:length(semestres)){
    for(i_plan in 1:length(planes_estudio)) {
      for(i_num_materia in 1:length(intervalo_num_materia)) {
        # Graficamos el avance de la extracción
        if(i_num_materia/100==floor(i_num_materia/100)){
          # Sys.sleep(0.06)
          names_percentages <- c(paste0("Semestre ",semestres[i_sem]),
                                 paste0("Plan ",planes_estudio[i_plan]),"Materias")
          My_plot_progress(c(i_sem/length(semestres),i_plan/length(planes_estudio),
                             i_num_materia/length(intervalo_num_materia)),
                           names_percentages,title_plot)
        } # fi graficando
        url = paste0("http://www.fciencias.unam.mx/docencia/horarios/",semestres[i_sem],
                     "/",planes_estudio[i_plan],
                     "/",intervalo_num_materia[i_num_materia])
        
        #Probamos si la página existe:
        tryCatch({
          read_html(url)
          i <- i+1
          mat_posibles_url[i,1:4] <- c(semestres[i_sem],
                                       planes_estudio[i_plan],
                                       intervalo_num_materia[i_num_materia],
                                       url)
          semestres_reales <- c(semestres_reales,semestres[i_sem])
          plan_reales <- c(plan_reales,planes_estudio[i_plan])
          num_mat_reales <- c(num_mat_reales,intervalo_num_materia[i_num_materia])
        }, 
        error=function(e){})
      } # fin for materias
    } # fin for planes
  } # fin de for semestres
  
  semestres_reales <- unique(na.omit(semestres_reales))
  plan_reales <- unique(na.omit(plan_reales))
  num_mat_reales <- unique(na.omit(num_mat_reales))
  
  names_percentages <- c(paste0("Semestre ",semestres[i_sem]),
                         paste0("Plan ",planes_estudio[i_plan]),"Materias")
  My_plot_progress(c(1,1,1),names_percentages,title_plot)
  
  ##Quitamos los renglones vacíos de la matriz:
  # mat_posibles_url = mat_posibles_url[1:i,]
  n_renglones<- dim(mat_posibles_url)[1]
  mat_posibles_url = matrix(mat_posibles_url[mat_posibles_url[1:n_renglones,1]!=0],ncol = 6)
  
  # Regresamos una lista con los valores adecuados
  
  list_url$mat_RAW_url=mat_posibles_url
  list_url$mat_posibles_url=mat_posibles_url
  list_url$semestres_reales=semestres_reales
  list_url$plan_reales=plan_reales
  list_url$num_mat_reales=num_mat_reales
  
  if(nrow(list_url$mat_Grande) == 0) {
    list_url$mat_Grande <- matrix(-1,sum(as.numeric(list_url$mat_posibles_url[,5])),
                                  list_url$ncol_mat_Grande)
    
    colnames(list_url$mat_Grande) <- c("Materia", "Profesor","Horario","Lugares",
                                       "Alumnos","Salon","Grupo","Carrera","Plan",
                                       "Semestre","Cambios","Turno",
                                       "Semestre_de_materia")
  }
  
  # Salvando intervalo de materia para futuras corridas
  intervalo_num_materia <- sort(list_url$num_mat_reales)
  archivo_intervalo_num_materia <- paste0(list_url$Carpeta_RData,
                                          "/Datos_INTERVALO_NUM_MATERIA.RData")
  save(intervalo_num_materia, file = archivo_intervalo_num_materia)
  
  # Stop the clock
  cat("La función posibles_url tardó: ",(proc.time()-ptm)[3]/60," minutos\n")
  
  return(list_url)
}



# url_con1_sin0_info ------------------------------------------------------
#' Title: url_con1_sin0_info
#' @param url: página de internet correspondiente a algún horario de la FC
#'
#' @return 1 si la página tiene información, 0 si no
#'
#' @example url = paste0("http://www.fciencias.unam.mx/docencia/horarios/",sem,"/",
#' plan,"/",num_materia)
url_con1_sin0_info = function(url){
  valor_url = 0
  
  tryCatch({
    webpage <- read_html(url)
    profesor_data_html <- html_nodes(webpage,'tr:nth-child(1) td:nth-child(1)')
    (profesor <- html_text(profesor_data_html))
    if(length(profesor) > 0){
      valor_url = 1
    }
  }, error=function(e){})
  
  return(valor_url)
}


# url_con_info ------------------------------------------------------------
#' Title: url_con_info
#' @param mat_posibles_url: Matriz con 4 columnas:
#' 1) Semestre
#' 2) Plan
#' 3) Número de materia
#' 4) Posibles url
#'
#' @return vec_con_info: Vector binario que indica si cada url tiene o no información
#'
#' @example mat_posibles_url[16407,] = c(20182,217,991,
#' http://www.fciencias.unam.mx/docencia/horarios/20182/217/991)
url_con_info = function(list_url){
  if(file.exists(paste0(list_url$Carpeta_RData,"/Datos_vec_con_info_",list_url$sem_ini,"_",list_url$sem_fin,".RData")) && 
     list_url$usa_vec_con_info_salvados) {
    cat("Se utilizara del archivo \n\t",
        paste0(list_url$Carpeta_RData,"/Datos_vec_con_info_",list_url$sem_ini,"_",list_url$sem_fin,".RData"),"\n\n")
    load(paste0(list_url$Carpeta_RData,"/Datos_vec_con_info_",list_url$sem_ini,"_",list_url$sem_fin,".RData"))
    Sys.sleep(1)
  } else {
    mat_posibles_url <- list_url$mat_posibles_url
    vec_posibles_url = mat_posibles_url[,4] # la col 4 contiene URL
    vec_con_info = rep(-1,length(vec_posibles_url))
    
    vec_para_for <- 1:length(vec_posibles_url)
    pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
    cat("Limpiando mat_posibles_url:\n")
    for(i in vec_para_for){
      setTxtProgressBar(pb, i)
      vec_con_info[i] = url_con1_sin0_info(as.character(mat_posibles_url[i,4]))
    }
    close(pb)
    print("\n")
    cat("Se encontraron ",mean(vec_con_info==1)*100,"% url con información\n")
  }
  save(vec_con_info,
       file = paste0(list_url$Carpeta_RData,"/Datos_vec_con_info_",list_url$sem_ini,"_",list_url$sem_fin,".RData"))
  return(vec_con_info)
}



# limpia_base_url ---------------------------------------------------------
#' Title: limpia_base_url
#' @param mat_posibles_url: Matriz con 4 columnas:
#' 1) Semestre
#' 2) Plan
#' 3) Número de materia
#' 4) Posibles url
#'
#' @return mat_limpia1_url: Matriz "mat_posibles_url" sólo con páginas que
#' tienen información
#'
#' @example mat_posibles_url[16407,] = c(20182,217,991,
#' http://www.fciencias.unam.mx/docencia/horarios/20182/217/991)
limpia_base_url = function(list_url){
  # mat_posibles_url <- list_url$mat_posibles_url
  longitud_original <- nrow(list_url$mat_posibles_url)
  
  # Primero borramos las que no tengan información
  vec_con_info = url_con_info(list_url)
  # mat_limpia1_url = mat_posibles_url[vec_con_info>0,] # ANTES
  list_url$mat_posibles_url = list_url$mat_posibles_url[vec_con_info>0,] # AHORA
  list_url$num_grupos = list_url$num_grupos[vec_con_info>0] # AHORA
  
  longitud_nueva <- nrow(list_url$mat_posibles_url)
  
  cat("La longitud original era ",longitud_original," la nueva longitud es ",
      longitud_nueva,":\n\t se borraron ",longitud_original-longitud_nueva,
      " un ",(longitud_original-longitud_nueva)*100/longitud_original,"% \n")
  
  return(list_url) 
}



# elimina_grupos_con_0 ----------------------------------------------------
#' Eliminando páginas sin grupos
#'
#' @param list_url 
#'
#' @return
#' @export
#'
#' @examples
elimina_grupos_con_0 <- function(list_url) {
  # Logitud original sin borrar páginas con 0 en su número de grupos
  (longitud_original <- nrow(list_url$mat_posibles_url))
  i_poginas_CON_grupos <- which(list_url$mat_posibles_url[,5]!="0")
  i_poginas_SIN_grupos <- which(list_url$mat_posibles_url[,5]=="0")
  (longitud_nueva <- length(i_poginas_CON_grupos))
  if(longitud_nueva!=longitud_original) {
    list_url$mat_posibles_url = list_url$mat_posibles_url[i_poginas_CON_grupos,]
    list_url$num_grupos = list_url$num_grupos[i_poginas_CON_grupos]
  }
  cat("La longitud original era ",longitud_original," la nueva longitud es ",
      longitud_nueva,":\n se borraron ",longitud_original-longitud_nueva,
      " un ",(longitud_original-longitud_nueva)*100/longitud_original,"% \n")
  return(list_url)
}



# genera_num_grupos -------------------------------------------------------
#' Genera el vector de números de grupos de cada página web
#'
#' @param list_url lista con información de url
#'
#' @return
#' @export
#'
#' @examples
genera_num_grupos <- function(list_url){
  if(file.exists(paste0(list_url$Carpeta_RData,"/Datos_num_grupos_x_pag_",list_url$sem_ini,"_",list_url$sem_fin,".RData")) && 
     list_url$usa_grupos_salvados) {
    cat("Se utilizara del archivo \n\t",
        paste0(list_url$Carpeta_RData,"/Datos_num_grupos_x_pag_",list_url$sem_ini,"_",list_url$sem_fin,".RData"),"\n\n")
    load(paste0(list_url$Carpeta_RData,"/Datos_num_grupos_x_pag_",list_url$sem_ini,"_",list_url$sem_fin,".RData"))
    Sys.sleep(1)
  } else {
    m4_posibles_url <- list_url$mat_posibles_url
    dim_matriz <- dim(m4_posibles_url)
    num_grupos <- rep(0,dim_matriz[1])
    cat("Extrayendo número de grupos de cada página web:\n")
    ubicacion_grupos <- list_url$mat_ubicaciones_url[list_url$mat_ubicaciones_url[,1]=="Grupos_x_pag",2]
    vec_para_for <- 1:dim_matriz[1]
    pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
    for(i in 1:dim_matriz[1]){
      setTxtProgressBar(pb, i)
      url <- m4_posibles_url[i,4]
      tryCatch({
        webpage <- read_html(url)
        grupo_data_html <- html_nodes(webpage,ubicacion_grupos)
        grupo <- html_text(grupo_data_html)
        num_grupos[i] <- length(grupo)
      }, error=function(e){})
      # Antes estaba fuera del tryCatch
      # num_grupos[i] <- length(grupo)
    }
    close(pb)
    print("\n")
  }
  list_url$num_grupos <- num_grupos
  
  list_url$mat_posibles_url[,5] <- list_url$num_grupos
  colnames(list_url$mat_posibles_url) <- list_url$colnames_mat_posibles_url
  
  save(num_grupos,
       file = paste0(list_url$Carpeta_RData,"/Datos_num_grupos_x_pag_",list_url$sem_ini,"_",list_url$sem_fin,".RData"))
  
  return(list_url)
}



# url_con1_sin0_salon -----------------------------------------------------
#' Title: url_con1_sin0_salon
#' Esta función recibe como parámetro una url, la cual se checa si tiene o no
#' el salón del grupo, en caso de que lo tenga, la función regresa un 1 y en caso 
#' contrario regresa un cero
#' 
#' @param url: página de internet correspondiente a algún horario de la FC
#'
#' @return 1 si la página tiene información, 0 si no
#'
#' @example url = paste0("http://www.fciencias.unam.mx/docencia/horarios/",sem,"/",
#' plan,"/",num_materia)
url_con1_sin0_salon <- function(url){
  valor_url <- 0
  tryCatch({
    webpage <- read_html(url)
    salon_data_html <- html_nodes(webpage,'tr:nth-child(1) td~ td+ td a')
    salon <- html_text(salon_data_html)
    if(length(salon) > 0){
      valor_url <- 1
    }
  }, error=function(e){})
  return(valor_url)
}



# actualiza_list_url_con_salon --------------------------------------------
#' Title: actualiza_list_url_con_salon
#' Esta función recibe como parámetro la matriz de 4 columnas generada por la
#' función "posibles_url", se crea un vector con las url que se encuentran en la 
#' cuarta columna de dicha matriz y se manda a llamar a la función "url_con1_sin0_salon"
#' dentro de un "for" que recorre las páginas que están en el vector para regresar
#' un vector binario, en el cual los 1 indican que la página correspondiente a ese
#' renglón si tiene información del salón, 0 si no.
#' @param m4_posibles_url: Matriz con 4 columnas:
#' 1) Semestre
#' 2) Plan
#' 3) Número de materia
#' 4) Posibles url
#'
#' @return vec_con_salon: Vector binario que indica si cada url tiene o no información
#' del salón
#'
#' @example m4_posibles_url[16407,] = c(20182,217,991,
#' http://www.fciencias.unam.mx/docencia/horarios/20182/217/991)
actualiza_list_url_con_salon <- function(list_url){
  if(file.exists(paste0(list_url$Carpeta_RData,"/Datos_vec_con_salon",
                        list_url$sem_ini,"_",list_url$sem_fin,".RData")) && 
     list_url$usa_vec_con_salon){
    cat("Se utilizara del archivo \n\t",
        paste0(list_url$Carpeta_RData,"/Datos_vec_con_salon",list_url$sem_ini,"_",list_url$sem_fin,".RData"),"\n\n")
    load(paste0(list_url$Carpeta_RData,"/Datos_vec_con_salon",list_url$sem_ini,"_",list_url$sem_fin,".RData"))
    Sys.sleep(1)
  } else {
    m4_posibles_url <- list_url$mat_posibles_url
    #En la 4° columna de "m4_posibles_url" están las url
    vec_posibles_url <- m4_posibles_url[,4]
    vec_con_salon <- rep(-1,length(vec_posibles_url))
    vec_para_for <- 1:length(vec_posibles_url)
    pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
    for(i in 1:length(vec_posibles_url)){
      setTxtProgressBar(pb, i)
      vec_con_salon[i] <- url_con1_sin0_salon(vec_posibles_url[i])
    }
    close(pb)
    print("\n")
  }
  
  list_url$url_con_salon <- vec_con_salon
  list_url$mat_posibles_url[,6] <- list_url$url_con_salon
  save(vec_con_salon,
       file = paste0(list_url$Carpeta_RData,"/Datos_vec_con_salon",
                     list_url$sem_ini,"_",list_url$sem_fin,".RData"))
  
  return(list_url)
}



# Actualiza_list_url ------------------------------------------------------
#' Generador de archivo .RData con información de semestres sacada de url.
#' Llena las columnas "Grupos por página" y "url_con_salon" de la matriz
#' "mat_posibles_url" para que tenga completas sus 6 columnas.
#'
#' @param sem_ini semestre inicial
#' @param sem_fin semestre final
#' @param file_name nombre del archivo que se quiere salvar
#'
#' @return list_url
#'
#' @examples
#' sem_ini = 20151
#' sem_fin = 20201
#' file_name <- "Posibles_URL_V01_10_no.RData"
#' salva_limpia_genera_url(sem_ini,sem_fin,file_name)
#'
Actualiza_list_url <- function(list_url) {
  
  if(!Valida_list_url(list_url)){
    return(list_url)
  }
  # sem_ini <- list_url$sem_ini
  # sem_fin <- list_url$sem_fin
  # file_name <- list_url$file_name
  # Start the clock!
  ptm <- proc.time()
  
  # Extrayendo posibles paginas url
  if(list_url$Actualiza_RAW_url) {
    cat("\n ---- Extrayendo posibles páginas url\n")
    list_url <- posibles_url(list_url)
    save(list_url, file = list_url$file_name_RAW)
    cat("\n ---- Extrayendo grupos url\n")
    list_url <- genera_num_grupos(list_url)
    cat("\n ---- Actualizando url con salon url\n")
    list_url <- actualiza_list_url_con_salon(list_url)
    cat("OK --- Fin de extracción de posibles páginas url\n")
    save(list_url, file = list_url$file_name_RAW)
  }
  
  if(list_url$Actualiza_limpia_base_url) {
    cat("---- Actualizando limpia_base_url\n")
    list_url <- limpia_base_url(list_url)
    cat("OK --- Fin de limpieza de base url\n")
  }
  
  if(list_url$Actualiza_elimina_grupos_con_0) {
    cat("------- Función elimina_grupos_con_0\n")
    list_url <- elimina_grupos_con_0(list_url)
    cat("OK --- Fin de elimina_grupos_con_0\n")
  }
  
  if(nrow(list_url$mat_Grande) == 0) {
    list_url$nrow_mat_posibles_url <- nrow(list_url$mat_posibles_url)
    list_url$mat_Grande <- matrix(-1,sum(as.numeric(list_url$mat_posibles_url[,5])),
                                  list_url$ncol_mat_Grande)
    colnames(list_url$mat_Grande) <- c("Materia", "Profesor","Horario","Lugares","Alumnos","Salon",
                                       "Grupo","Carrera","Plan","Semestre","Cambios","Turno","Semestre_de_materia")
    list_url$mat_Grande_con_url <- matrix(-1,sum(as.numeric(list_url$mat_posibles_url[,5])),
                                          list_url$ncol_mat_Grande+1)
    
    colnames(list_url$mat_Grande_con_url) <- c("Materia", "Profesor","Horario","Lugares","Alumnos","Salon",
                                               "Grupo","Carrera","Plan","Semestre","Cambios","Turno","Semestre_de_materia","url")
  }
  
  if(list_url$Salvar_URL_RData) {
    cat("---- Salvando variables\n")
    save(list_url, file = list_url$file_name)
    cat("Se guardó el archivo:\n\t", list_url$file_name,"\n")
    cat("OK --- Fin de salvado de variables\n")
  }
  
  cat("La función Actualiza_list_url tomó: ", (proc.time()-ptm)[3]/60," minutos\n\n\n" )
  return(list_url)
}


##########################################################################
##### M_GRANDE / M_GRANDE_TOTAL #####
## Funciones que generan la matriz "m_grande" con la información de las
##páginas de la facultad.
##########################################################################


# borra_i_posible_grupo ---------------------------------------------------
#' Borra renglones de vec que contengan string_a_buscar
#'
#' @param string_a_buscar 
#' @param vec 
#'
#' @return
#' @export
#'
#' @examples
borra_i_posible_grupo <- function(string_a_buscar,vec){
  i_a_borrar <- grep(string_a_buscar, vec)
  if(length(i_a_borrar)>0) {
    vec <- vec[-(c(i_a_borrar,i_a_borrar-1))]
  }
  return(vec)
}



# corrige_i_posible_grupo -------------------------------------------------
#' Junta la entrada anterior y la que contiene a string_a_buscar
#'
#' @param string_a_buscar 
#' @param vec 
#'
#' @return
#' @export
#'
#' @examples
corrige_i_posible_grupo <- function(string_a_buscar,vec){
  i_corrige <- grep(string_a_buscar, vec)
  if(length(i_corrige)>0) {
    aux <- vec
    for(ii in 1:length(i_corrige)){
      aux[i_corrige[ii]-1] <- paste(vec[i_corrige[ii]-1],vec[i_corrige[ii]],collapse = "")
    }
    aux <- aux[-i_corrige]
    vec <- aux
  }
  return(vec)
}


# imprime_errores_mat_info_k_pag ------------------------------------------
#' Title: imprime_errores_mat_info_k_pag
#'
#' @param mat_info_k_pag 
#' @param list_url 
#'
#' @return
#' @export
#'
#' @examples
imprime_errores_mat_info_k_pag <- function(mat_info_k_pag,list_url) {
  
  col_names_tabla <- c("Materia","Profesor","Horario","Lugares","Alumnos","Salon  ",
                       "Grupo  ","Carrera","Plan  ", "Semestre","Cambios","Turno  ",
                       "Sem_de_mater","url    ")
  cat(" --- La dimension de mat_info_k_pag es ",dim(mat_info_k_pag)," ---\n",
      "------------------------------------------------------------------------------\n",
      "\t","Nombre","\t# vacios","\t% vacios","\t# NA","\t% NA","\n",
      "------------------------------------------------------------------------------\n")
  for(i in 1:ncol(mat_info_k_pag)) {
    porciento_vacios <- as.character(round(sum(mat_info_k_pag[,i]=="",na.rm = T)*100/nrow(mat_info_k_pag),
                                           digits = 2))
    # porciento_vacios <- paste(rep(" ",5-nchar(porciento_vacios)),porciento_vacios,collapse = "")
    n_vacios <- sum(mat_info_k_pag[,i]=="",na.rm = T)
    if(n_vacios==0) porciento_vacios <- 0
    
    cat(i,"\t", col_names_tabla[i],"\t",
        n_vacios,"\t\t", 
        porciento_vacios,
        "%\t\t",
        sum(is.na(mat_info_k_pag[,i])),"\t",
        round(sum(is.na(mat_info_k_pag[,i]))*100/nrow(mat_info_k_pag), digits = 2),
        "%\n")
  }
  cat(" --- Se revisaron ",nrow(mat_info_k_pag)," páginas distintas ---\n")
}



# extrae_info_1_pag -------------------------------------------------------
#' Extrae la información de una página web
#'
#' @param url 
#'
#' @return
#' @export
#'
#' @examples
#' load("Archivos RData V01/Dat_URL_20172_20201.RData") # Mediana
#'ii <- sample(1:nrow(list_url$mat_posibles_url),1)
#'url <- list_url$mat_posibles_url[ii,4]
#'extrae_info_1_pag(url)
#'
extrae_info_1_pag <- function(url) {
  cat("\nLeyendo url: ",url)
  # Sacamos toda la información de una página
  webpage <- read_html(url)
  data_html <- html_nodes(read_html(url),'#info-contenido')
  vec <- html_text(data_html)
  
  vec <- strsplit(vec,"Grupo ")[[1]] # [1] "a" "b" "c" "c" "b" "a"
  vec <- vec[-1]
  vec
  
  vec_strings_a_borrar <- c("especial","Especial","paralelo a ","xtraordinario por etapas")
  vec_strings_a_corrige <- c("exclusivo para ","semipresencial","nuevo aprobado por el CT")
  
  for(string_a_buscar in vec_strings_a_borrar)
    vec <- borra_i_posible_grupo(string_a_buscar,vec)
  
  ##En este ciclo "vec" puede quedar vacío por lo que se pone una
  ##condición en el siguiente for para que el programa no se detenga
  
  if(identical(vec,character(0))){
    vec <- 0
  }else{
    for(string_a_buscar in vec_strings_a_corrige)
      vec <- corrige_i_posible_grupo(string_a_buscar,vec)
  }
  
  if(identical(vec,character(0))){
    vec <- 0
  }
  
  vec
  
  # if(length(vec)>1){
  if(vec!=0){##Puede tener longitud 1 y si tener información (con esta condición)
    ##se generan "warnings" cuando si hay información en "vec"
    mat_info_un_pag <- matrix(0,length(vec),14)
    # colnames(mat_info) <- c("grupo","lugares","alumnos","profesor","horario","salon")
    
    colnames(mat_info_un_pag) <- c("Materia","Profesor","Horario","Lugares",
                                   "Alumnos","Salon","Grupo","Carrera","Plan",
                                   "Semestre","Cambios","Turno","Semestre_de_materia","url")
    
    # Extraemos de la página materia y semestre_de_materia
    materia_y_semestre_de_materia <- html_text(html_nodes(webpage,'#info-contenido h2'))
    
    # Extraemos variables constantes entre grupos: 
    # 1  Materia
    # 8  Carrera
    # 9  Plan
    # 10 Semestre
    # 13 Semestre_de_materia
    # 14 url
    
    # 1  Materia
    mat_info_un_pag[,1] <- substr(materia_y_semestre_de_materia,1,
                                  regexpr(',', materia_y_semestre_de_materia)[1]-1)  # Materia
    
    # Extraemos de la página la carrera y el plan
    carrera_plan <- html_text(html_nodes(webpage,'h1'))
    
    # 8  Carrera
    mat_info_un_pag[,8] <- substr(carrera_plan,1,nchar(carrera_plan)-12) # Carrera
    # 9  Plan
    mat_info_un_pag[,9] <- substr(carrera_plan,nchar(carrera_plan)-4,nchar(carrera_plan)-1)#Plan
    # 10 Semestre
    mat_info_un_pag[,10] <- substr(url,48,52) # Semestre
    # 13 Semestre_de_materia
    mat_info_un_pag[,13] <- substr(materia_y_semestre_de_materia,
                                   regexpr(',',materia_y_semestre_de_materia)[1]+2,
                                   nchar(materia_y_semestre_de_materia)) # Semestre_de_materia
    # 14 url
    mat_info_un_pag[,14] <- url
    
    i <- 2
    i <- 1
    # Con este for extraemos variables que cambian por grupo:
    # 2 Profesor
    # 3 Horario
    # 4 Lugares
    # 5 Alumnos
    # 6 Salón
    # 7 Grupo 
    for(i in 1:nrow(mat_info_un_pag)){
      x <- vec[i]
      x <- strsplit(x,"\n")[[1]] 
      x
      
      texto <- vec[i]
      texto
      
      # Separamos con respecto a \n
      texto <- strsplit(texto, split='\n', fixed=TRUE)[[1]]
      texto
      # Encontramos los lugares de profesor y ayudante
      i_profesores <- which(unlist(gregexpr('Profesor', texto))>0)+1
      i_ayudantess <- which(unlist(gregexpr('Ayudante', texto))>0)+1
      
      # Posibles mejoras utilizando los horarios de ayudantes
      # i_horariosss <- c(i_profesores,i_ayudantess)+1
      
      as.vector(regexpr(' a ', texto))
      
      aux_encuentra_a <- which(as.vector(regexpr(' a ', texto))>-1)
      aux_encuentra_a <- aux_encuentra_a[aux_encuentra_a>2]
      aux_encuentra_a
      
      i_horariosss <- aux_encuentra_a
      i_horariosss
      
      # Hacemos los vectores de prof y ayudante
      vec_i_profesores <- sort(unique(texto[i_profesores]))
      vec_i_ayudantess <- sort(unique(texto[i_ayudantess]))
      vec_i_horariosss <- sort(unique(texto[i_horariosss]))
      
      vec_i_profesores
      vec_i_ayudantess
      vec_i_horariosss
      
      # Eliminamos na
      vec_i_profesores <- paste(vec_i_profesores[!is.na(vec_i_profesores)],collapse = " / ")
      vec_i_ayudantess <- paste(vec_i_ayudantess[!is.na(vec_i_ayudantess)],collapse = " / ")
      vec_i_horariosss <- paste(vec_i_horariosss[!is.na(vec_i_horariosss)],collapse = " / ")
      
      vec_i_profesores
      vec_i_ayudantess
      vec_i_horariosss
      
      # mat_info_un_pag[i,3] <- x[8] # horario
      mat_info_un_pag[i,2] <- vec_i_profesores
      mat_info_un_pag[i,3] <- vec_i_horariosss # horario
      
      # Obs: se podría guardar la informacion de vec_i_ayudantess
      
      if(regexpr('lugares', x[1])[1]>0) {
        mat_info_un_pag[i,4] <- substr(x[1],7,regexpr('lugares', x[1])[1]-2) # lugares
        mat_info_un_pag[i,5] <- substr(x[1],regexpr('lugares', x[1])[1]+9,
                                       regexpr('alumno', x[1])[1]-2) # alumnos
      } else {
        mat_info_un_pag[i,4] <- "" # lugares
        mat_info_un_pag[i,5] <- substr(x[1],6,regexpr('alumno', x[1])[1]-2) # alumnos
      }
      if(mat_info_un_pag[i,5]=="Un") mat_info_un_pag[i,5] <- "1" # alumnos
      mat_info_un_pag[i,6] <- x[9] # salon
      mat_info_un_pag[i,7] <- substr(x[1],1,4) # grupo
    }
  }else{
    mat_info_un_pag <- matrix(0,length(vec),14)
  }##Fin de if(vec!=0)
    
  return(mat_info_un_pag)
}


# limpia_m_grande ---------------------------------------------------------
#' Title limpia_m_grande: Genera la matriz "m_grande" para cada semestre,
#' sin grupos repetidos.
#'
#' @param sem_info: Semestre del que se desea obtener información.
#' @param mat_info_k_pag: Matriz de 14 columnas (Materia,Profesor,Horario,
#' Lugares,Alumnos,Salon,Grupo,Carrera,Plan,Semestre,Cambios,Turno,
#' Semestre_de_materia,url), con la información de cada semestre.
#'
#' @example sem_info <- 20182
#' @example mat_info_k_pag[378,] = c(Problemas Socio-Económicos de México,
#' Silvia Alonso Reyes,7 a 8,"",38,O122,6007,Actuaría,2000,20101,0,0,Primer
#' Semestre,http://www.fciencias.unam.mx/docencia/horarios/20101/119/1109)
#' 
#' @return m_grande: Matriz de 36 columnas (Materia,Profesor,Horario,
#' horario_num,Lugares,Alumnos,Salon,Grupo,Carrera,Plan,Semestre,Cambios,
#' Turno,Semestre_de_materia,url,Act2000,Act2006,Act2015,CdC1994,CdC2013,
#' Mat1983,MAp2017,NomMat_Act2000,NomMat_Act2006,NomMat_Act2015,
#' NomMat_CdC1994,NomMat_CdC2013,NomMat_Mat1983,NomMat_MAp2017,URL_Act2000,
#' URL_Act2006,URL_Act2015,URL_CdC1994,URL_CdC2013,URL_Mat1983,URL_MAp2017),
#' con la información de "sem_info". Las columnas 16-22 son columnas
#' binarias las cuales indican con un 1 si el grupo del i-ésimo renglón
#' pertenece a la carrera y plan correspondiente al nombre de cada columna,
#' hay un 0 e.o.c. Las columnas 23-29 indican el nombre de las materias
#' dependiendo del plan y carrera al que pertenecen. Las columnas 30-36 
#' indican las URLs dependiendo del plan y carrera al que pertenecen.
#' 
limpia_m_grande <- function(sem_info,mat_info_k_pag,param){
  #Se eliminan los renglones con información repetida
  mat_info_k_pag = unique(mat_info_k_pag)
  
  ##Se quitan los renglones vacíos de la matriz:
  mat_info_k_pag <- matrix(mat_info_k_pag[mat_info_k_pag[1:(nrow(mat_info_k_pag)),
                                                         1]!=0,],ncol = dim(mat_info_k_pag)[2])
  
  #Se generan las matrices con la información de los grupos repetidos:
  i <- 1
  matriz_con_rep <- matrix(0,ncol = ncol(mat_info_k_pag),nrow = nrow(mat_info_k_pag))
  mat_resumen_rep <- matrix(0,nrow = nrow(mat_info_k_pag),ncol = 5)
  Profesores <- unique(mat_info_k_pag[,2])
  Horarios <- unique(mat_info_k_pag[,3])##String, no números
  
  for(p in 1:length(Profesores)){
    prof_iguales <- matrix(mat_info_k_pag[mat_info_k_pag[,2] == Profesores[p] , ],
                           ncol = 14)
    for(h in 1:length(Horarios)){
      horarios_iguales <- matrix(prof_iguales[prof_iguales[,3] == Horarios[h] , ],
                                 ncol = 14)
      n_rep <- nrow(horarios_iguales)
      if(n_rep>1){
        num_alumnos <- horarios_iguales[,5]
        alum_diferentes <- 0 #Suponemos que el número de alumnos es igual
        for(k in 2:n_rep){
          if(num_alumnos[1]!=num_alumnos[k]){
            alum_diferentes <- 1
          }
        }
        
        matriz_con_rep[i:(i+n_rep-1),] <- matrix(horarios_iguales,ncol = 14)
        mat_resumen_rep[i,] <- c(sem_info,Profesores[p],Horarios[h],
                                 n_rep,alum_diferentes)
        i <- i + n_rep
      }
    }##Fin de for de Horarios
  }##Fin de for de Profesores
  
  ##Quitamos los renglones vacíos de la matriz:
  matriz_con_rep <- matrix(matriz_con_rep[matriz_con_rep[1:(nrow(matriz_con_rep)),
                                                         1]!=0,],ncol = 14)
  colnames(matriz_con_rep) <- param$nom_cols_m14
  save(matriz_con_rep, file = paste0("mat_aux de limpia_m_grande/matriz_con_rep_",
                                     sem_info,".RData"))
  mat_resumen_rep <- matrix(mat_resumen_rep[mat_resumen_rep[1:(nrow(mat_resumen_rep)),1]!=0,],
                            ncol = 5)
  colnames(mat_resumen_rep) <- c("Semestre","Profesor","Horario","Repeticiones","AlumDif")
  save(mat_resumen_rep, file = paste0("mat_aux de limpia_m_grande/mat_resumen_rep_",
                                      sem_info,".RData"))
  
  n_gpos_alum_dif <- sum(as.numeric(mat_resumen_rep[,5]))
  cat("\nHay ",n_gpos_alum_dif,
      " grupos iguales con diferente número de alumnos para el semestre ",
      sem_info," de ",dim(mat_resumen_rep)[1]," grupos repetidos")
  
  
  #Se define "m_grande"
  m_grande <- as.data.frame(matrix(0,nrow = nrow(mat_info_k_pag),
                                   ncol = length(param$nom_cols_MG)))
  
  num_col_Materia <- arroja_ind_col_MG("Materia")##1
  num_col_Profesor <- arroja_ind_col_MG("Profesor")##2
  num_col_Horario <- arroja_ind_col_MG("Horario")##3
  num_col_horario_num <- arroja_ind_col_MG("horario_num")##4
  num_col_Lugares <- arroja_ind_col_MG("Lugares")##5
  num_col_Alumnos <- arroja_ind_col_MG("Alumnos")##6
  # num_col_Salon <- arroja_ind_col_MG("Salon")##7
  num_col_Grupo <- arroja_ind_col_MG("Grupo")##8
  num_col_Carrera <- arroja_ind_col_MG("Carrera")##9
  num_col_Plan <- arroja_ind_col_MG("Plan")##10
  # num_col_Semestre <- arroja_ind_col_MG("Semestre")##11
  # num_col_Cambios <- arroja_ind_col_MG("Cambios")##12
  num_col_Turno <- arroja_ind_col_MG("Turno")##13
  num_col_Semestre_de_materia <- arroja_ind_col_MG("Semestre_de_materia")##14
  num_col_url <- arroja_ind_col_MG("url")##15
  num_col_Act2000 <- arroja_ind_col_MG("Act2000")##16
  num_col_Act2006 <- arroja_ind_col_MG("Act2006")##17
  num_col_Act2015 <- arroja_ind_col_MG("Act2015")##18
  num_col_CdC1994 <- arroja_ind_col_MG("CdC1994")##19
  num_col_CdC2013 <- arroja_ind_col_MG("CdC2013")##20
  num_col_Mat1983 <- arroja_ind_col_MG("Mat1983")##21
  num_col_MAp2017 <- arroja_ind_col_MG("MAp2017")##22
  num_col_NomMat_Act2000 <- arroja_ind_col_MG("NomMat_Act2000")##23
  num_col_NomMat_Act2006 <- arroja_ind_col_MG("NomMat_Act2006")##24
  num_col_NomMat_Act2015 <- arroja_ind_col_MG("NomMat_Act2015")##25
  num_col_NomMat_CdC1994 <- arroja_ind_col_MG("NomMat_CdC1994")##26
  num_col_NomMat_CdC2013 <- arroja_ind_col_MG("NomMat_CdC2013")##27
  num_col_NomMat_Mat1983 <- arroja_ind_col_MG("NomMat_Mat1983")##28
  num_col_NomMat_MAp2017 <- arroja_ind_col_MG("NomMat_MAp2017")##29
  num_col_URL_Act2000 <- arroja_ind_col_MG("URL_Act2000")##30
  num_col_URL_Act2006 <- arroja_ind_col_MG("URL_Act2006")##31
  num_col_URL_Act2015 <- arroja_ind_col_MG("URL_Act2015")##32
  num_col_URL_CdC1994 <- arroja_ind_col_MG("URL_CdC1994")##33
  num_col_URL_CdC2013 <- arroja_ind_col_MG("URL_CdC2013")##34
  num_col_URL_Mat1983 <- arroja_ind_col_MG("URL_Mat1983")##35
  num_col_URL_MAp2017 <- arroja_ind_col_MG("URL_MAp2017")##36
  
  
  names(m_grande) <- param$nom_cols_MG
  m_grande[,num_col_Materia:num_col_Horario] <- mat_info_k_pag[,1:3]
  m_grande[,num_col_Lugares:num_col_url] <- mat_info_k_pag[,4:14]
  
  vec_indices_rep <- 0
  ##Se eliminan los renglones con información repetida
  vec_para_for <- 1:dim(m_grande)[1]
  pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
  for(k in vec_para_for){#Se recorre sobre los renglones de los grupos repetidos
    setTxtProgressBar(pb, k)
    for(r in 1:dim(mat_resumen_rep)[1]){
      profesor <- mat_resumen_rep[r,2]
      horario <- mat_resumen_rep[r,3]
      if(m_grande[k,num_col_Profesor]==profesor && 
         m_grande[k,num_col_Horario]==horario){
        vec_indices_rep <- c(vec_indices_rep,k)
      }
    }
  }
  close(pb)
  
  vec_indices_rep <- vec_indices_rep[-1]
  
  if(length(vec_indices_rep)>0){
    m_grande <- m_grande[-vec_indices_rep,]
  }
  
  ##Se llenan las últimas 7 columnas
  for(j in 1:dim(m_grande)[1]){
    switch(m_grande[j,num_col_Plan],
           '2000' = {m_grande[j,num_col_Act2000] = 1},
           '2006' = {m_grande[j,num_col_Act2006] = 1},
           '2015' = {m_grande[j,num_col_Act2015] = 1},
           '1994' = {m_grande[j,num_col_CdC1994] = 1},
           '2013' = {m_grande[j,num_col_CdC2013] = 1},
           '1983' = {m_grande[j,num_col_Mat1983] = 1},
           '2017' = {m_grande[j,num_col_MAp2017] = 1}
    )
  }
  
  ## Se agregan los renglones únicos (de la información repetida) a "m_grande"
  ##para tener completa la información
  d <- 1##Contador para la matriz "matriz_con_rep"
  cambio_materia <- 0
  cambio_grupo <- 0
  cambio_carrera <- 0
  cambio_plan <- 0
  
  for(k in 1:dim(mat_resumen_rep)[1]){
    cat("\nk = ",k)
    renglon <- c(matriz_con_rep[d,1:3],0,matriz_con_rep[d,4:14],
                 rep(0,(length(param$nom_cols_MG)-15)))
    num_repeticiones <- as.numeric(mat_resumen_rep[k,4])
    rango <- d:(d+num_repeticiones-1)
    
    ##Materia:
    if(length(unique(matriz_con_rep[rango,1]))>1){
      renglon[num_col_Materia] <- ""
      cambio_materia <- 1}
    
    ##Lugares:
    if(is.na(as.numeric(unique(matriz_con_rep[rango,4])))){
      renglon[num_col_Lugares] <- 0
    }else{
      renglon[num_col_Lugares] <- max(as.numeric(matriz_con_rep[rango,4]))
    }
    
    ##Alumnos:
    renglon[num_col_Alumnos] <- sum(as.numeric(unique(matriz_con_rep[rango,5])))
    
    ##Grupo:
    if(length(unique(matriz_con_rep[rango,7]))>1){
      renglon[num_col_Grupo] <- ""
      cambio_grupo <- 1}
    
    ##Carrera
    if(length(unique(matriz_con_rep[rango,8]))>1){
      renglon[num_col_Carrera] <- ""
      cambio_carrera <- 1}
    
    ##Plan
    if(length(unique(matriz_con_rep[rango,9]))>1){
      renglon[num_col_Plan] <- ""
      cambio_plan <- 1}
    
    ##Semestre de materia
    renglon[num_col_Semestre_de_materia] <- ""
    
    ##URL
    renglon[num_col_url] <- ""
    
    for(r in 1:num_repeticiones){
      ##Materia:
      if(cambio_materia == 1){
        renglon[num_col_Materia] <- paste(renglon[num_col_Materia],matriz_con_rep[d,1],sep = "/")}
      
      ##Grupo:
      if(cambio_grupo == 1){
        renglon[num_col_Grupo] <- paste(renglon[num_col_Grupo],matriz_con_rep[d,7],sep = "/")}
      
      ##Carrera
      if(cambio_carrera == 1){
        renglon[num_col_Carrera] <- paste(renglon[num_col_Carrera],matriz_con_rep[d,8],sep = "/")}
      
      ##Plan
      if(cambio_plan == 1){
        renglon[num_col_Plan] <- paste(renglon[num_col_Plan],matriz_con_rep[d,9],sep = "/")}
      
      ##Semestre de materia
      renglon[num_col_Semestre_de_materia] <- paste(renglon[num_col_Semestre_de_materia],matriz_con_rep[d,13],sep = "/")
      
      ##URL
      renglon[num_col_url] <- paste(renglon[num_col_url],matriz_con_rep[d,14],sep = "/")
      
      switch(matriz_con_rep[d,9],
             '2000' = {renglon[num_col_Act2000] = 1;
             renglon[num_col_NomMat_Act2000] = matriz_con_rep[d,1];
             renglon[num_col_URL_Act2000] = matriz_con_rep[d,14]},
             '2006' = {renglon[num_col_Act2006] = 1;
             renglon[num_col_NomMat_Act2006] = matriz_con_rep[d,1];
             renglon[num_col_URL_Act2006] = matriz_con_rep[d,14]},
             '2015' = {renglon[num_col_Act2015] = 1;
             renglon[num_col_NomMat_Act2015] = matriz_con_rep[d,1];
             renglon[num_col_URL_Act2015] = matriz_con_rep[d,14]},
             '1994' = {renglon[num_col_CdC1994] = 1;
             renglon[num_col_NomMat_CdC1994] = matriz_con_rep[d,1];
             renglon[num_col_URL_CdC1994] = matriz_con_rep[d,14]},
             '2013' = {renglon[num_col_CdC2013] = 1;
             renglon[num_col_NomMat_CdC2013] = matriz_con_rep[d,1];
             renglon[num_col_URL_CdC2013] = matriz_con_rep[d,14]},
             '1983' = {renglon[num_col_Mat1983] = 1;
             renglon[num_col_NomMat_Mat1983] = matriz_con_rep[d,1];
             renglon[num_col_URL_Mat1983] = matriz_con_rep[d,14]},
             '2017' = {renglon[num_col_MAp2017] = 1;
             renglon[num_col_NomMat_MAp2017] = matriz_con_rep[d,1];
             renglon[num_col_URL_MAp2017] = matriz_con_rep[d,14]}
      )
      d <- d + 1
    }
    m_grande <- rbind(m_grande,renglon)
  }##Fin de for(k)
  
  ## Se llena la columna 4 "horario_num" con los datos de los horarios
  ##en variables de tipo "numeric"
  ##Se agrega la columna que tiene los horarios como números y no como string
  horario_string <- m_grande[,num_col_Horario]
  ## Se toman los primeros 2 caracteres de cada hora y se convierte en
  ##número, al encontrar ":" toma sólo el primer caracter y al encontrar
  ## "" en la entrada se pone un cero
  horario_numeros <- as.numeric(substr(horario_string,1,2))
  # View(horario_numeros)
  indices_NA <- rep(0,length(horario_numeros))
  k <- 1
  for(i in 1:length(horario_numeros)){
    cat("\n i =",i)
    if(is.na(horario_numeros[i])){
      indices_NA[k] <- i
      k <- k + 1}}
  indices_NA <- indices_NA[indices_NA>0]
  
  if(length(indices_NA) > 0){
    for(i in 1:length(indices_NA)){
      if(substr(horario_string[indices_NA[i]],2,2) == ":"){
        horario_numeros[indices_NA[i]] <- 
          as.numeric(substr(horario_string[indices_NA[i]],1,1))
      }else if(substr(horario_string[indices_NA[i]],2,2) == ""){
        horario_numeros[indices_NA[i]] <- 0
      }
    }
  }
  # View(horario_numeros)
  m_grande[,num_col_horario_num] <- horario_numeros
  
  ##Se ponen ceros en la columna de "Lugares" en caso de tenerlos
  lugares <- as.numeric(m_grande[,num_col_Lugares])
  for(i in 1:length(lugares)){
    if(is.na(lugares[i])){
      lugares[i] <- 0
    }
  }
  # View(lugares)
  m_grande[,num_col_Lugares] <- lugares
  
  ##Se ponen ceros en la columna de "Alumnos" en caso de tenerlos
  num_alumnos <- as.numeric(m_grande[,num_col_Alumnos])
  for(i in 1:length(num_alumnos)){
    if(is.na(num_alumnos[i])){
      num_alumnos[i] <- 0
    }
  }
  # View(num_alumnos)
  m_grande[,num_col_Alumnos] <- num_alumnos
  
  ##Se llena la columna 13 "Turno"
  ##Turno matutino: 7am - 14:00hrs (incluyendo la clase de 14-15hrs)
  ##Turno vespertino: 15-21 hrs (incluyendo la clase de 21-22hrs)
  Turno <- m_grande[,num_col_Turno]
  for(i in 1:length(horario_numeros)){
    if(is.na(horario_numeros[i])){
      # Turno[i] <- "NA"
      Turno[i] <- 0
    }else if(horario_numeros[i] < 15){
      Turno[i] <- "M" ##Matutino
    }else if(horario_numeros[i] >= 15){
      Turno[i] <- "V" ##Vespertino
    }
  }
  # View(Turno)
  m_grande[,num_col_Turno] <- Turno
  
  # View(m_grande)
  return(m_grande)
}


# gen_m_grande_SIN_MOD -------------------------------------------------------
#' Title gen_m_grande_SIN_MOD: Función que recibe como parámetro el semestre
#' del que se desea obtener la información y genera un archivo de tipo
#' ".Rdata" con la matriz "m_grande" de dicho semestre. Regresa la matriz
#' "m_grande_SIN_MOD" de "sem_info".
#'
#' @param sem_info: Semestre del que se desea obtener información.
#' @param list_url: Lista con parámetros utilizados
#' 
#' @example sem_info <- 20182
#' @example list_url <-list()
#' 
#' @return m_grande: Matriz de 36 columnas (Materia,Profesor,Horario,
#' horario_num,Lugares,Alumnos,Salon,Grupo,Carrera,Plan,Semestre,Cambios,
#' Turno,Semestre_de_materia,url,Act2000,Act2006,Act2015,CdC1994,CdC2013,
#' Mat1983,MAp2017,NomMat_Act2000,NomMat_Act2006,NomMat_Act2015,
#' NomMat_CdC1994,NomMat_CdC2013,NomMat_Mat1983,NomMat_MAp2017,URL_Act2000,
#' URL_Act2006,URL_Act2015,URL_CdC1994,URL_CdC2013,URL_Mat1983,URL_MAp2017),
#' con la información de "sem_info". Las columnas 16-22 son columnas
#' binarias las cuales indican con un 1 si el grupo del i-ésimo renglón
#' pertenece a la carrera y plan correspondiente al nombre de cada columna,
#' hay un 0 e.o.c. Las columnas 23-29 indican el nombre de las materias
#' dependiendo del plan y carrera al que pertenecen. Las columnas 30-36 
#' indican las URLs dependiendo del plan y carrera al que pertenecen.
#'
gen_m_grande_SIN_MOD <- function(sem_info,list_url,param){
  mat_posibles_url <- list_url$mat_posibles_url
  # direccion_info <- paste0("m_grande por semestre/m_grande_",sem_info,".RData")
  direccion_info <- paste0("m_grande por semestre SIN MODIFICAR/m_grande_SIN_MOD_",
                           sem_info,".RData")
  
  #Este vector contiene las url del "sem_info"
  vec_url_sem <- mat_posibles_url[mat_posibles_url[,1]==sem_info,4]
  
  #Factor por pagina: Es un estimado de cuantos grupos hay por página (a ojo)
  media_grupos_por_pagina <- 10
  mat_info_k_pag <- matrix(0,length(vec_url_sem)*media_grupos_por_pagina,14)
  
  colnames(mat_info_k_pag) <- c("Materia","Profesor","Horario","Lugares","Alumnos","Salon",
                                "Grupo","Carrera","Plan","Semestre","Cambios","Turno",
                                "Semestre_de_materia","url")
  
  if(!file.exists(direccion_info)){
    ##En caso de que el archivo de la matriz "m_grande" no exista:
    vec_para_for <- 1:length(vec_url_sem)
    pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
    
    indice_grupos <- 1
    for(i in vec_para_for){
      # cat("\ni = ",i)
      setTxtProgressBar(pb, i)
      url <- vec_url_sem[i]
      mat_info_un_pag <- extrae_info_1_pag(url)
      # mat_info_un_pag <- matrix(-1,sample(2:10,1),13)
      mat_info_k_pag[indice_grupos:(indice_grupos+nrow(mat_info_un_pag)-1),] <- mat_info_un_pag
      # cat(indice_grupos,(indice_grupos+nrow(mat_info_un_pag)-1),"\n")
      indice_grupos <- indice_grupos + nrow(mat_info_un_pag)
    }
    close(pb)
    
    mat_info_k_pag <- mat_info_k_pag[1:(indice_grupos-1),]
    nom_archivo <- paste0("mat_info_k_pag por semestre/mat_info_k_pag_",sem_info,".RData")
    save(mat_info_k_pag, file = nom_archivo)
    
    # nombre_m_grande <- paste0("m_grande_",sem_info)
    # assign(nombre_m_grande,mat_info_k_pag)
    # nom_matriz <- as.name(nombre_m_grande)
    ### REVISAR CÓMO GUARDAR LA MATRIZ CON EL NOMBRE ADECUADO ###
    ### EXCEPCIONES ###
    # vec_excepciones <- 1##Caso de inglés
    # m_grande <- limpia_m_grande(sem_info,mat_info_k_pag,param,vec_excepciones)
    m_grande <- limpia_m_grande(sem_info,mat_info_k_pag,param)
    
    # Salvando archivo
    save(m_grande, file = direccion_info)
  }else{
    load(direccion_info)
  }
  imprime_errores_mat_info_k_pag(m_grande,list_url)
  
  # return(direccion_info)
  return(m_grande)
}


# actualiza_m_grande_1_sem ------------------------------------------------
#' Title actualiza_m_grande_1_sem: Función que actualiza la matriz
#' "m_grande_SIN_ING" de "sem_info".
#' - Se unen las materias iguales en un solo renglón
#' - Se conserva el nombre más reciente de la materia repetida
#' - Se verifica que las matrices sólo tengan datos del semestre
#' correspondiente.
#'
#' @param sem_info: Semestre del que se desea obtener información.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' 
#' @example sem_info <- 20182
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
#' @return m_grande: Matriz de 36 columnas (Materia,Profesor,Horario,
#' horario_num,Lugares,Alumnos,Salon,Grupo,Carrera,Plan,Semestre,Cambios,
#' Turno,Semestre_de_materia,url,Act2000,Act2006,Act2015,CdC1994,CdC2013,
#' Mat1983,MAp2017,NomMat_Act2000,NomMat_Act2006,NomMat_Act2015,
#' NomMat_CdC1994,NomMat_CdC2013,NomMat_Mat1983,NomMat_MAp2017,URL_Act2000,
#' URL_Act2006,URL_Act2015,URL_CdC1994,URL_CdC2013,URL_Mat1983,URL_MAp2017),
#' con la información de "sem_info". Las columnas 16-22 son columnas
#' binarias las cuales indican con un 1 si el grupo del i-ésimo renglón
#' pertenece a la carrera y plan correspondiente al nombre de cada columna,
#' hay un 0 e.o.c. Las columnas 23-29 indican el nombre de las materias
#' dependiendo del plan y carrera al que pertenecen. Las columnas 30-36 
#' indican las URLs dependiendo del plan y carrera al que pertenecen.
#'
actualiza_m_grande_1_sem <- function(sem_info,param){
  #Se carga la matriz "m_grande" que se va a actualizar
  nom_archivo <- paste0("m_grande por semestre SIN MODIFICAR/m_grande_SIN_MOD_",
                        sem_info,".RData")
  load(nom_archivo)
  m_grande_SIN_MOD <- m_grande
  
  ##Se definen las variables que se van a utilizar:
  m_grande <- as.data.frame(matrix(0,ncol = length(param$nom_cols_MG)))
  names(m_grande) <- param$nom_cols_MG
  mat_aux_rep <- as.data.frame(matrix(0,ncol = length(param$nom_cols_MG)))
  names(mat_aux_rep) <- param$nom_cols_MG
  num_col_Materia <- arroja_ind_col_MG("Materia")##1
  num_col_Profesor <- arroja_ind_col_MG("Profesor")##2
  num_col_Horario <- arroja_ind_col_MG("Horario")##3
  num_col_horario_num <- arroja_ind_col_MG("horario_num")##4
  num_col_Lugares <- arroja_ind_col_MG("Lugares")##5
  num_col_Alumnos <- arroja_ind_col_MG("Alumnos")##6
  num_col_Salon <- arroja_ind_col_MG("Salon")##7
  num_col_Grupo <- arroja_ind_col_MG("Grupo")##8
  num_col_Carrera <- arroja_ind_col_MG("Carrera")##9
  num_col_Plan <- arroja_ind_col_MG("Plan")##10
  num_col_Semestre <- arroja_ind_col_MG("Semestre")##11
  num_col_Cambios <- arroja_ind_col_MG("Cambios")##12
  num_col_Turno <- arroja_ind_col_MG("Turno")##13
  num_col_Sem_de_mat <- arroja_ind_col_MG("Semestre_de_materia")##14
  num_col_url <- arroja_ind_col_MG("url")##15
  num_col_Act2000 <- arroja_ind_col_MG("Act2000")##16
  num_col_MAp2017 <- arroja_ind_col_MG("MAp2017")##22
  num_col_NomMat_Act2006 <- arroja_ind_col_MG("NomMat_Act2006")##24
  num_col_NomMat_CdC1994 <- arroja_ind_col_MG("NomMat_CdC1994")##26
  num_col_URL_Act2006 <- arroja_ind_col_MG("URL_Act2006")##31
  num_col_URL_CdC1994 <- arroja_ind_col_MG("URL_CdC1994")##33
  num_col_URL_MAp2017 <- arroja_ind_col_MG("URL_MAp2017")##36
  
  #Se carga el archivo con las materias repetidas desde el 2008-1 al 2020-1
  load("mat_materias_rep_20081_20201.RData")
  
  #Se define una matriz auxiliar con la información repetida del "sem_info"
  materias_rep_aux <- mat_materias_rep[mat_materias_rep[,1]==sem_info,]
  
  #Se verifica que la matriz sólo tenga datos del semestre "sem_info"
  m_grande_SIN_MOD <- m_grande_SIN_MOD[m_grande_SIN_MOD[,num_col_Semestre]==sem_info,]
  
  if(dim(materias_rep_aux)[1]>1){##Si hay repeticiones
    profesores_unique <- unique(materias_rep_aux[,2])
    horarios_unique <- unique(materias_rep_aux[,4])
    num_profesores <- length(profesores_unique)
    if(length(horarios_unique) < length(profesores_unique)){
      horarios_unique <- c(horarios_unique,
                           rep(0,(length(profesores_unique)-length(horarios_unique))))}
    
    #Se eliminan los renglones repetidos
    ind_aux <- 0
    for(d in 1:num_profesores){
      # cat("\n d = ",d)
      for(r in 1:dim(m_grande_SIN_MOD)[1]){
        # cat("\n r = ",r)
        # cat("\n Profesor: ",m_grande_SIN_MOD[r,num_col_Profesor])
        if(m_grande_SIN_MOD[r,num_col_Profesor]==profesores_unique[d] && 
           m_grande_SIN_MOD[r,num_col_horario_num] == horarios_unique[d]){
          ind_aux <- c(ind_aux,r)}}}
    ## Se quita el cero inicial
    ind_aux <- ind_aux[-1]
    ind_aux <- unique(ind_aux)
    
    mat_aux <- m_grande_SIN_MOD[ind_aux,]
    m_grande <- m_grande_SIN_MOD[-ind_aux,]
    
    #Se agrega la información combinada por casos:
    switch(sem_info,
           #' Las materias "Seminario de Aplicaciones de Cómputo II" y
           #' "Biología Matemática I", son la misma, así se se combinan
           #' los renglones se agrega a "m_grande"
           '20101' = {renglon <- c("Biología Matemática I",
                                   mat_aux[1,num_col_Profesor:num_col_Lugares],
                                   sum(as.numeric(mat_aux[,num_col_Alumnos])),
                                   "Taller de Análisis Numérico",
                                   "7040/4207",
                                   paste0(mat_aux[1,num_col_Carrera],
                                          mat_aux[2,num_col_Carrera]),
                                   paste0(mat_aux[1,num_col_Plan],mat_aux[2,num_col_Plan]),
                                   mat_aux[1,num_col_Semestre:num_col_Turno],
                                   paste0(mat_aux[1,num_col_Sem_de_mat],
                                          mat_aux[2,num_col_Sem_de_mat]),
                                   paste0(mat_aux[1,num_col_url],mat_aux[2,num_col_url]),
                                   0,0,0,1,0,1,1,0,0,0,
                                   "Seminario de Aplicaciones de Cómputo II",0,
                                   "Biología Matemática I","Biología Matemática I",0,0,0,
                                   "http://www.fciencias.unam.mx/docencia/horarios/20101/218/424",
                                   0,
                                   "http://www.fciencias.unam.mx/docencia/horarios/20101/217/275",
                                   "http://www.fciencias.unam.mx/docencia/horarios/20101/2055/275")
           renglon[num_col_Cambios] <- "1/"
           names(renglon) <- param$nom_cols_MG  
           m_grande <- rbind(m_grande,renglon)
           },##Fin 20101
           '20111' = {
             #' Las materias "Ecuaciones Diferenciales I" y "Cálculo Diferencial e
             #' Integral I", son diferentes, pero las clases si comienzan a la misma
             #' hora, Ecuaciones de 18-19hrs y Cálculo de 18-20hrs, dado que se tiene
             #' la misma ayudante pudiera ser que se intercambien las horas, pero
             #' no se puede asignar más de una clase a la misma hora al mismo profesor.
             renglon_1 <- c("Ecuaciones Diferenciales I",
                            mat_aux[1,num_col_Profesor:num_col_Alumnos],
                            "O123","4172",
                            mat_aux[1,num_col_Carrera:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/"
             names(renglon_1) <- param$nom_cols_MG  
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c("Cálculo Diferencial e Integral I",
                            mat_aux[2,num_col_Profesor:num_col_Alumnos],
                            "Taller Interdisciplinario de Física y Biomedicina I",
                            "4039",
                            mat_aux[2,num_col_Carrera:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/"
             names(renglon_2) <- param$nom_cols_MG  
             m_grande <- rbind(m_grande,renglon_2)
           },##Fin 20111
           '20121' = {
             #' Las materias "Inteligencia Artificial" y "Lingüística Computacional",
             #' son diferentes, y los horarios también, coinciden por las clases de los
             #' ayudantes.
             renglon_1 <- c("Inteligencia Artificial",
                            mat_aux[1,num_col_Profesor:num_col_Horario],
                            18,##Empieza a las 18:30hrs y termina a las 20hrs
                            mat_aux[1,num_col_Lugares:num_col_Alumnos],
                            "Taller de Lenguajes de Programación","7016",
                            mat_aux[1,num_col_Carrera:num_col_Cambios],
                            "V",
                            mat_aux[1,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/"
             names(renglon_1) <- param$nom_cols_MG  
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c("Lingüística Computacional",
                            mat_aux[2,num_col_Profesor:num_col_Horario],
                            17,##Empieza a las 17hrs y termina a las 18:30hrs
                            mat_aux[2,num_col_Lugares:num_col_Alumnos],
                            "Taller de Ingeniería de Software","7027",
                            mat_aux[2,num_col_Carrera:num_col_Cambios],
                            "V",
                            mat_aux[2,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/"
             names(renglon_2) <- param$nom_cols_MG  
             m_grande <- rbind(m_grande,renglon_2)
             
             #' Las materias "Introducción a Ciencias de la Computación" y "Teoría
             #' de la Computación", son diferentes, y los horarios también, coinciden
             #' por las clases de los ayudantes.
             renglon_3 <- c(mat_aux[3,num_col_Materia:num_col_Horario],
                            16,##Empieza a las 16hrs y termina a las 17:30hrs
                            mat_aux[3,num_col_Lugares:num_col_Alumnos],
                            "O218",
                            mat_aux[3,num_col_Grupo:num_col_Cambios],
                            "V",
                            mat_aux[3,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_3[num_col_Cambios] <- "1/"
             names(renglon_3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_3)
             
             renglon_4 <- c("Teoría de la Computación",
                            mat_aux[4,num_col_Profesor:num_col_Alumnos],
                            "P210","7014",
                            mat_aux[4,num_col_Carrera:num_col_URL_MAp2017])
             renglon_4[num_col_Cambios] <- "1/"
             names(renglon_4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_4)
           },##Fin 20121
           '20122' = {
             ind_aux <- 0
             for(r in 1:dim(m_grande)[1]){
               if(m_grande[r,num_col_Profesor]=="Antonio Francisco Menéndez Leonel de Cervantes" && 
                  m_grande[r,num_col_horario_num] == "14"){
                 ind_aux <- c(ind_aux,r)}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             #' Las materias "Programación I" e "Introducción a Ciencias
             #' de la Computación I" son diferentes, los horarios coinciden
             #' por las horas de los ayudantes.
             renglon_1 <- c("Programación I",
                            mat_aux[1,num_col_Profesor:num_col_Alumnos],
                            "Laboratorio de Enseñanza de Cómputo de Actuaría","6019",
                            mat_aux[1,num_col_Carrera:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/"
             names(renglon_1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c("Introducción a Ciencias de la Computación I",
                            mat_aux[2,num_col_Profesor:num_col_Horario],
                            16,##Empieza a las 16hrs y termina a las 17:30hrs
                            mat_aux[2,num_col_Lugares:num_col_Alumnos],
                            "Laboratorio de Ciencias de la Computación 3",
                            mat_aux[2,num_col_Grupo:num_col_Cambios],
                            "V",
                            mat_aux[2,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/"
             names(renglon_2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_2)
             
             #' Las materias "Seminario de Computación Teórica" y "Teoría de Códigos"
             #' son diferentes, los horarios coinciden por las horas de los ayudantes.
             renglon_3 <- c(mat_aux[3,num_col_Materia:num_col_Horario],
                            18,##Empieza a las 18:30hrs y termina a las 20hrs
                            mat_aux[3,num_col_Lugares:num_col_Alumnos],
                            "Laboratorio de Ciencias de la Computación 2",
                            mat_aux[3,num_col_Grupo:num_col_Cambios],
                            "V",
                            mat_aux[3,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_3[num_col_Cambios] <- "1/"
             names(renglon_3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_3)
             
             renglon_4 <- c(mat_aux[4,num_col_Materia:num_col_Horario],
                            17,##Empieza a las 17hrs y termina a las 18:30hrs
                            mat_aux[4,num_col_Lugares:num_col_Alumnos],
                            "P104",
                            mat_aux[4,num_col_Grupo:num_col_Cambios],
                            "V",
                            mat_aux[4,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_4[num_col_Cambios] <- "1/"
             names(renglon_4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_4)
             
             
             #' Las materias "Proceso Digital de Imagenes" y "Redes de Computadoras"
             #' son diferentes, los horarios coinciden por las horas de los ayudantes.
             renglon_5 <- c("Proceso Digital de Imagenes",
                            mat_aux[5,num_col_Profesor:num_col_Horario],
                            8,
                            mat_aux[5,num_col_Lugares:num_col_Alumnos],
                            "Laboratorio de Innovación Tecnológica","7037",
                            "Ciencias de la Computación/Ciencias de la Computación",
                            mat_aux[5,num_col_Plan:num_col_URL_MAp2017])
             renglon_5[num_col_Cambios] <- "1/"
             names(renglon_5) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_5)
             
             renglon_6 <- c("Redes de Computadora",
                            mat_aux[6,num_col_Profesor:num_col_Horario],
                            7,
                            mat_aux[6,num_col_Lugares:num_col_Alumnos],
                            "Laboratorio de Innovación Tecnológica","7069",
                            mat_aux[6,num_col_Carrera:num_col_URL_MAp2017])
             renglon_6[num_col_Cambios] <- "1/"
             names(renglon_6) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_6)
           },##Fin 20122
           '20131' = {
             #' Las materias "Redes de Computadoras" e "Introducción a Ciencias
             #' de la Computación I" son diferentes, los horarios coinciden
             #' por las horas de los ayudantes.
             renglon_1 <- c("Redes de Computadoras",
                            mat_aux[1,num_col_Profesor:num_col_Salon],
                            "7016",
                            mat_aux[1,num_col_Carrera:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/"
             names(renglon_1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c("Introducción a Ciencias de la Computación I",
                            mat_aux[2,num_col_Profesor:num_col_Horario],
                            16,##Empieza a las 16hrs y termina a las 17:30hrs
                            39,40,
                            "Laboratorio de Ciencias de la Computación 2",
                            mat_aux[2,num_col_Grupo:num_col_Cambios],
                            "V",
                            mat_aux[2,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/"
             names(renglon_2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_2)
             
             
             #' Las materias "Probabilidad y Estadística" y "Estadística I"
             #' son diferentes, los horarios coinciden por las horas de los ayudantes.
             renglon_3 <- c(mat_aux[3,num_col_Materia:num_col_Horario],
                            17,##Empieza a las 17hrs y termina a las 18:30hrs
                            mat_aux[3,num_col_Lugares:num_col_URL_MAp2017])
             renglon_3[num_col_Cambios] <- "1/"
             names(renglon_3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_3)
             
             renglon_4 <- c("Estadística I",
                            mat_aux[4,num_col_Profesor:num_col_Salon],
                            "6218",
                            mat_aux[4,num_col_Carrera:num_col_URL_MAp2017])
             renglon_4[num_col_Cambios] <- "1/"
             names(renglon_4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_4)
           },##Fin 20131
           '20132' = {
             ind_aux <- 0
             for(r in 1:dim(m_grande)[1]){
               if(m_grande[r,num_col_Profesor]=="José de Jesús Galaviz Casas" && 
                  m_grande[r,num_col_horario_num] == "10"){
                 ind_aux <- c(ind_aux,r)}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             ind_aux <- 0
             for(r in 1:dim(m_grande)[1]){
               if(m_grande[r,num_col_Profesor]=="Manuel Cristobal López Michelone" && 
                  m_grande[r,num_col_horario_num] == "14"){
                 ind_aux <- c(ind_aux,r)}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             #' Las materias "	Matemáticas para las Ciencias Aplicadas IV" y
             #' "Cálculo Diferencial e Integral II" son diferentes, los horarios
             #' coinciden por las horas de los ayudantes.
             renglon_1 <- c(mat_aux[1,num_col_Materia:num_col_Alumnos],
                            "P208",
                            mat_aux[1,num_col_Grupo:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/"
             names(renglon_1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c(mat_aux[2,num_col_Materia:num_col_Horario],
                            9,##Empieza a las 9hrs y termina a las 11hrs
                            mat_aux[2,num_col_Lugares:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/"
             names(renglon_2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_2)
             
             
             #' Las materias "Fundamentos de Bases de Datos" y "Modelado y Programación"
             #' son diferentes, los horarios coinciden por las horas de los ayudantes.
             renglon_3 <- c("Fundamentos de Bases de Datos",
                            mat_aux[3,num_col_Profesor:num_col_Horario],
                            8,53,51,"102 (Yelizcalli)",
                            mat_aux[3,num_col_Grupo:num_col_URL_MAp2017])
             renglon_3[num_col_Cambios] <- "1/"
             names(renglon_3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_3)
             
             renglon_4 <- c("Modelado y Programación",
                            mat_aux[4,num_col_Profesor:num_col_Horario],
                            16,##Empieza a las 16hrs y termina a las 17:30hrs
                            mat_aux[4,num_col_Lugares:num_col_Salon],
                            "7041",
                            mat_aux[4,num_col_Carrera:num_col_Cambios],
                            "V",
                            mat_aux[4,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_4[num_col_Cambios] <- "1/"
             names(renglon_4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_4)
             
             
             #' Las materias "Seminario de Computación Teórica" y "Organización y
             #' Arquitectura de Computadoras" son diferentes, los horarios coinciden
             #' por las horas de los ayudantes.
             renglon_5 <- c(mat_aux[5,num_col_Materia:num_col_URL_MAp2017])
             renglon_5[num_col_Cambios] <- "1/"
             names(renglon_5) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_5)
             
             renglon_6 <- c("Organización y Arquitectura de Computadoras",
                            mat_aux[6,num_col_Profesor:num_col_Horario],
                            16,##Empieza a las 16hrs y termina a las 17:30hrs
                            31,30,"P211",
                            mat_aux[6,num_col_Grupo:num_col_Cambios],
                            "V",
                            mat_aux[6,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_6[num_col_Cambios] <- "1/"
             names(renglon_6) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_6)
             
             
             #' Las materias "Proceso Digital de Imagenes" e "Inteligencia
             #' Artificial" son diferentes, los horarios coinciden por las
             #' horas de los ayudantes.
             renglon_7 <- c(mat_aux[7,num_col_Materia:num_col_URL_MAp2017])
             renglon_7[num_col_Cambios] <- "1/"
             names(renglon_7) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_7)
             
             renglon_8 <- c("Inteligencia Artificial",
                            mat_aux[8,num_col_Profesor:num_col_Horario],
                            9,
                            mat_aux[8,num_col_Lugares:num_col_Salon],
                            "7001",
                            mat_aux[8,num_col_Carrera:num_col_URL_MAp2017])
             renglon_8[num_col_Cambios] <- "1/"
             names(renglon_8) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_8)
           },##Fin 20132
           '20141' = {
             #' Las materias "Introducción a Ciencias de la Computación I" y
             #' "Modelado y Programación" son diferentes, coinciden en horario
             #' pero se imparten en días distintos
             renglon_1 <- c("Introducción a Ciencias de la Computación I",
                            mat_aux[1,num_col_Profesor],
                            "10 a 12 / 13 a 14 / 16 a 17:30	Lu Mi",
                            16,##Empieza a las 16hrs y termina a las 17:30hrs
                            mat_aux[1,num_col_Lugares:num_col_Cambios],
                            "V",
                            mat_aux[1,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/2/"
             names(renglon_1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c("Modelado y Programación",
                            mat_aux[2,num_col_Profesor],
                            "10 a 12 / 16 a 17:30	Ma Ju",
                            16,##Empieza a las 16hrs y termina a las 17:30hrs
                            mat_aux[2,num_col_Lugares:num_col_Salon],
                            "7045",
                            mat_aux[2,num_col_Carrera:num_col_Cambios],
                            "V",
                            mat_aux[1,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/2/"
             names(renglon_2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_2)
             
             
             #' Las materias "Diseño de interfaces" y "Programación I" son
             #' diferentes, los horarios coinciden por las horas de los ayudantes.
             renglon_3 <- c(mat_aux[3,num_col_Materia:num_col_Horario],
                            17,##Empieza a las 17hrs y termina a las 18:30hrs
                            mat_aux[3,num_col_Lugares:num_col_Cambios],
                            "V",
                            mat_aux[3,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_3[num_col_Cambios] <- "1/"
             names(renglon_3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_3)
             
             renglon_4 <- c("Programación I",
                            mat_aux[4,num_col_Profesor:num_col_Salon],
                            "9285",
                            mat_aux[4,num_col_Carrera:num_col_URL_MAp2017])
             renglon_4[num_col_Cambios] <- "1/"
             names(renglon_4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_4)
           },##Fin 20141
           '20142' = {
             ind_aux <- 0
             prof_aux_1 <- c("José de Jesús Galaviz Casas","Baruch Demian Gaxiola Valles")
             for(r in 1:dim(m_grande)[1]){
               for(p in 1:length(prof_aux_1)){
                 if(m_grande[r,num_col_Profesor]==prof_aux_1[p] && 
                    m_grande[r,num_col_horario_num] == "10"){
                   ind_aux <- c(ind_aux,r)}}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             ind_aux <- 0
             prof_aux_2 <- c("Selene Marisol Martínez Ramírez","Manuel Cristobal López Michelone")
             for(r in 1:dim(m_grande)[1]){
               for(p in 1:length(prof_aux_2)){
                 if(m_grande[r,num_col_Profesor]==prof_aux_2[p] && 
                    m_grande[r,num_col_horario_num] == "14"){
                   ind_aux <- c(ind_aux,r)}}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             #' Las materias "Algoritmos Paralelos" y "Estructuras de Datos" son
             #' diferentes, los horarios coinciden por las horas de los ayudantes.
             renglon_1 <- c("Algoritmos Paralelos",
                            mat_aux[1,num_col_Profesor:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/"
             names(renglon_1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c("Estructuras de Datos",
                            mat_aux[2,num_col_Profesor:num_col_Horario],
                            16,##Empieza a las 16hrs y termina a las 17:30hrs
                            mat_aux[2,num_col_Lugares:num_col_Cambios],
                            "V",
                            mat_aux[2,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/"
             names(renglon_2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_2)
             
             
             #' Las materias "Lógica Computacional" y "Semántica y Verificación" son
             #' diferentes, los horarios coinciden por las horas de los ayudantes.
             renglon_3 <- c("Lógica Computacional",
                            mat_aux[3,num_col_Profesor:num_col_Horario],
                            16,##Empieza a las 16hrs y termina a las 17:30hrs
                            mat_aux[3,num_col_Lugares:num_col_Cambios],
                            "V",
                            mat_aux[3,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_3[num_col_Cambios] <- "1/"
             names(renglon_3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_3)
             
             renglon_4 <- c("Semántica y Verificación",
                            ##Empieza a las 13hrs y termina a las 14:30hrs
                            mat_aux[4,num_col_Profesor:num_col_Alumnos],
                            "Salón 301 del IIMAS","7016",
                            mat_aux[4,num_col_Carrera:num_col_URL_MAp2017])
             renglon_4[num_col_Cambios] <- "1/"
             names(renglon_4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_4)
             
             
             #' Las materias "Criptografía y Seguridad" y "Organización y
             #' Arquitectura de Computadoras" son diferentes, los horarios coinciden
             #' por las horas de los ayudantes.
             renglon_5 <- c("Criptografía y Seguridad",
                            mat_aux[5,num_col_Profesor:num_col_horario_num],
                            30,
                            mat_aux[5,num_col_Alumnos:num_col_URL_MAp2017])
             renglon_5[num_col_Cambios] <- "1/"
             names(renglon_5) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_5)
             
             renglon_6 <- c("Organización y Arquitectura de Computadoras",
                            mat_aux[6,num_col_Profesor:num_col_Horario],
                            12,##12-13 //Un ay. imparte los jueves de 10-12
                            mat_aux[6,num_col_Lugares:num_col_URL_MAp2017])
             renglon_6[num_col_Cambios] <- "1/"
             names(renglon_6) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_6)
             
             
             #' Las materias "Seminario de Aplicaciones de Cómputo II" y "Visión
             #' Por Computadora" es la misma pero con diferente nombre de acuerdo
             #' al plan de estudios.
             renglon_7 <- c(mat_aux[8,num_col_Materia:num_col_Lugares],
                            9,#Se suman los alumnos de ambos nombres
                            mat_aux[8,num_col_Salon],
                            "7084/7087",
                            "Ciencias de la Computación/Ciencias de la Computación",
                            "/1994/2013",
                            mat_aux[8,num_col_Semestre:num_col_Turno],
                            "/Optativas/Optativas",
                            "/http://www.fciencias.unam.mx/docencia/horarios/20142/218/424/http://www.fciencias.unam.mx/docencia/horarios/20142/1556/794",
                            as.numeric(mat_aux[7,num_col_Act2000:num_col_MAp2017])+as.numeric(mat_aux[8,num_col_Act2000:num_col_MAp2017]),
                            0,0,0,"Seminario de Aplicaciones de Cómputo II",
                            "Visión Por Computadora",0,0,0,0,0,
                            "http://www.fciencias.unam.mx/docencia/horarios/20142/218/424",
                            "http://www.fciencias.unam.mx/docencia/horarios/20142/1556/794",
                            0,0)
             renglon_7[num_col_Cambios] <- "1/"
             names(renglon_7) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_7)
             
             
             #' Las materias "Recuperación y Búsqueda de Información en Textos" y
             #' "Proceso Digital de Imagenes" son diferentes, los horarios coinciden
             #' por las horas de los ayudantes.
             renglon_8 <- c("Recuperación y Búsqueda de Información en Textos",
                            mat_aux[9,num_col_Profesor:num_col_Horario],
                            9,##9-10hrs
                            mat_aux[9,num_col_Lugares:num_col_URL_MAp2017])
             renglon_8[num_col_Cambios] <- "1/"
             names(renglon_8) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_8)
             
             renglon_9 <- c("Proceso Digital de Imagenes",
                            mat_aux[10,num_col_Profesor:num_col_Horario],
                            8,##8-9hrs
                            mat_aux[10,num_col_Lugares:num_col_URL_MAp2017])
             renglon_9[num_col_Cambios] <- "1/"
             names(renglon_9) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_9)
             
             
             #' Las materias "Tecnologías para Desarrollos en Internet" y
             #' "Diseño de Interfaces de Usuario" son diferentes, los horarios coinciden
             #' por las horas de los ayudantes.
             renglon_10 <- c("Tecnologías para Desarrollos en Internet",
                             mat_aux[11,num_col_Profesor:num_col_Horario],
                             17,##Empieza a las 13hrs y termina a las 14:30hrs
                             30,
                             mat_aux[11,num_col_Alumnos:num_col_Cambios],
                             "V",
                             mat_aux[11,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_10[num_col_Cambios] <- "1/"
             names(renglon_10) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_10)
             
             renglon_11 <- c("Diseño de Interfaces de Usuario",
                             mat_aux[12,num_col_Profesor:num_col_Horario],
                             9,##9-10hrs
                             mat_aux[12,num_col_Lugares:num_col_URL_MAp2017])
             renglon_11[num_col_Cambios] <- "1/"
             names(renglon_11) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_11)
           },##Fin 20142
           '20151' = {
             #' Las materias "Computación Distribuida" e "Introducción a Ciencias
             #' de la Computación", son diferentes, los horarios coinciden por
             #' las clases de los ayudantes.
             renglon_1 <- c("Computación Distribuida",
                            mat_aux[1,num_col_Profesor:num_col_horario_num],
                            50,
                            mat_aux[1,num_col_Alumnos:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/"
             names(renglon_1) <- param$nom_cols_MG  
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c("Lingüística Computacional",
                            mat_aux[2,num_col_Profesor:num_col_Horario],
                            16,##Empieza a las 16hrs y termina a las 17:30hrs
                            mat_aux[2,num_col_Lugares:num_col_Cambios],
                            "V",
                            mat_aux[2,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/"
             names(renglon_2) <- param$nom_cols_MG  
             m_grande <- rbind(m_grande,renglon_2)
           },##Fin 20151
           '20152' = {
             #' Las materias "Programación" y "Diseño de Interfaces de Usuario" son
             #' diferentes, los horarios coinciden por las horas de los ayudantes.
             renglon_1 <- c("Programación",
                            mat_aux[1,num_col_Profesor:num_col_Salon],
                            "9796",
                            mat_aux[1,num_col_Carrera:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/"
             names(renglon_1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c("Diseño de Interfaces de Usuario",
                            mat_aux[2,num_col_Profesor:num_col_Horario],
                            17,##Empieza a las 17hrs y termina a las 18:30hrs
                            mat_aux[2,num_col_Lugares:num_col_Cambios],
                            "V",
                            mat_aux[2,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/"
             names(renglon_2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_2)
             
             
             #' Las materias "Probabilidad y Estadística" y "Probabilidad II" son
             #' diferentes, los horarios coinciden por las horas de los ayudantes.
             renglon_3 <- c(mat_aux[3,num_col_Materia:num_col_Horario],
                            17,##Empieza a las 17hrs y termina a las 18:30hrs
                            mat_aux[3,num_col_Lugares:num_col_URL_MAp2017])
             renglon_3[num_col_Cambios] <- "1/"
             names(renglon_3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_3)
             
             renglon_4 <- c("Probabilidad II",
                            mat_aux[4,num_col_Profesor:num_col_Salon],
                            "9607",
                            mat_aux[4,num_col_Carrera:num_col_URL_MAp2017])
             renglon_4[num_col_Cambios] <- "1/"
             names(renglon_4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_4)
           },##Fin 20152
           '20161' = {
             ind_aux <- 0
             for(r in 1:dim(m_grande)[1]){
               if(m_grande[r,num_col_Profesor]=="Pilar Selene Linares Arévalo" && 
                  m_grande[r,num_col_horario_num] == "13"){
                 ind_aux <- c(ind_aux,r)}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             ind_aux <- 0
             for(r in 1:dim(m_grande)[1]){
               if(m_grande[r,num_col_Profesor]=="Jonathan Banfi Vázquez" && 
                  m_grande[r,num_col_horario_num] == "14"){
                 ind_aux <- c(ind_aux,r)}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             
             #' Las materias "Ecuaciones Diferenciales I" y "Variable Compleja I" son
             #' diferentes, los horarios coinciden por las horas de los ayudantes.
             renglon_1 <- c("Ecuaciones Diferenciales I",
                            mat_aux[1,num_col_Profesor:num_col_Salon],
                            "4331",
                            mat_aux[1,num_col_Carrera:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/"
             names(renglon_1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c("Variable Compleja I",
                            mat_aux[2,num_col_Profesor:num_col_Horario],
                            14,##Empieza a las 14hrs y termina a las 15hrs
                            mat_aux[2,num_col_Lugares:num_col_Salon],
                            "4333",
                            mat_aux[2,num_col_Carrera:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/"
             names(renglon_2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_2)
             
             
             #' Las materias "Estructuras Discretas" y "Lógica Computacional II" son
             #' diferentes, los horarios coinciden por las horas de los ayudantes.
             renglon_3 <- c("Estructuras Discretas",
                            mat_aux[3,num_col_Profesor:num_col_Horario],
                            16,##Empieza a las 16hrs y termina a las 17:30hrs
                            mat_aux[3,num_col_Lugares:num_col_Cambios],
                            "V",
                            mat_aux[3,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_3[num_col_Cambios] <- "1/"
             names(renglon_3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_3)
             
             renglon_4 <- c("Lógica Computacional II",
                            mat_aux[4,num_col_Profesor:num_col_URL_MAp2017])
             renglon_4[num_col_Cambios] <- "1/"
             names(renglon_4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_4)
             
             
             #' Las materias "Seminario de Ciencias de la Computación A" y
             #' "Criptografía y Seguridad" son diferentes, los horarios coinciden
             #' por las horas de los ayudantes.
             renglon_5 <- c("Seminario de Ciencias de la Computación A",
                            mat_aux[5,num_col_Profesor:num_col_Horario],
                            19,##Empieza a las 19hrs y termina a las 20hrs
                            mat_aux[5,num_col_Lugares:num_col_Cambios],
                            "V",
                            mat_aux[5,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_5[num_col_Cambios] <- "1/"
             names(renglon_5) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_5)
             
             renglon_6 <- c("Criptografía y Seguridad",
                            mat_aux[6,num_col_Profesor:num_col_Horario],
                            8,##8-9 //Un ay. imparte los viernes de 14-16
                            mat_aux[6,num_col_Lugares:num_col_URL_MAp2017])
             renglon_6[num_col_Cambios] <- "1/"
             names(renglon_6) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_6)
           },##Fin 20161
           '20162' = {
             #' Las materias "Estructuras de Datos" y "Redes Neuronales"
             #' son diferentes, los horarios coinciden por las horas de
             #' los ayudantes.
             renglon_1 <- c("Estructuras de Datos",
                            mat_aux[1,num_col_Profesor:num_col_Horario],
                            16,##Empieza a las 16hrs y termina a las 17:30hrs
                            mat_aux[1,num_col_Lugares:num_col_Cambios],
                            "V",
                            mat_aux[1,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/"
             names(renglon_1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c("Redes Neuronales",
                            mat_aux[2,num_col_Profesor:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/"
             names(renglon_2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_2)
             
             
             #' Las materias "Inteligencia Artificial" y "Modelado y Programación" son
             #' diferentes, los horarios coinciden por las horas de los ayudantes.
             renglon_3 <- c("Inteligencia Artificial",
                            mat_aux[3,num_col_Profesor:num_col_Horario],
                            9,##Empieza a las 9hrs y termina a las 10hrs
                            mat_aux[3,num_col_Lugares:num_col_Cambios],
                            "M",
                            mat_aux[3,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_3[num_col_Cambios] <- "1/"
             names(renglon_3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_3)
             
             renglon_4 <- c("Modelado y Programación",
                            mat_aux[4,num_col_Profesor:num_col_Alumnos],
                            "Aula del Futuro, CECADET","7032",
                            mat_aux[4,num_col_Carrera:num_col_URL_MAp2017])
             renglon_4[num_col_Cambios] <- "1/"
             names(renglon_4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_4)
             
             
             #' Las materias "Inglés III" e "	Inglés II" son diferentes,
             #' las clases presenciales se imparten en días distintos.
             renglon_5 <- c(mat_aux[5,num_col_Materia:num_col_Profesor],
                            "13 a 15 Vi",
                            mat_aux[5,num_col_horario_num:num_col_URL_MAp2017])
             renglon_5[num_col_Cambios] <- "1/2/"
             names(renglon_5) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_5)
             
             renglon_6 <- c(mat_aux[6,num_col_Materia:num_col_Profesor],
                            "13 a 14 Lu Ma Mi Ju",
                            mat_aux[6,num_col_horario_num:num_col_URL_MAp2017])
             renglon_6[num_col_Cambios] <- "1/2/"
             names(renglon_6) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_6)
           },##Fin 20162
           '20171' = {
             ind_aux <- 0
             for(r in 1:dim(m_grande)[1]){
               if(m_grande[r,num_col_Profesor]=="Arturo García Miranda" && 
                  m_grande[r,num_col_horario_num] == "14"){
                 ind_aux <- c(ind_aux,r)}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             ind_aux <- 0
             for(r in 1:dim(m_grande)[1]){
               for(h in 13:14){
                 if(m_grande[r,num_col_Profesor]=="Lidia Fabiola Quevedo Rojas" && 
                    m_grande[r,num_col_horario_num] == h){
                   ind_aux <- c(ind_aux,r)}}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             
             #' Las materias "Seminario de Apoyo a la Titulación en
             #' Matemáticas A" y "Álgebra Lineal I" son diferentes, los
             #' horarios coinciden porque la profesora imparte el seminario
             #' a la misma hora los lunes y jueves, se le da prioridad a la
             #' materia de "Álgebra Lineal I" (en cuanto al horario) debido
             #' a que es una materia obligatoria.
             renglon_1 <- c(mat_aux[1,num_col_Materia:num_col_Profesor],
                            "16 a 18 Lu/ 17 a 18 Ju",
                            17,##Lunes: 16-18, Jueves: 17-18
                            mat_aux[1,num_col_Lugares:num_col_Alumnos],
                            "Cubículo 224, Departamento de Matemáticas",
                            mat_aux[1,num_col_Grupo:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/2/"
             names(renglon_1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- mat_aux[2,num_col_Materia:num_col_URL_MAp2017]
             renglon_2[num_col_Cambios] <- "1/"
             names(renglon_2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_2)
             
             
             #' Las materias "Inglés III" e "	Inglés II" son diferentes,
             #' las clases presenciales se imparten en días distintos.
             renglon_3 <- c(mat_aux[3,num_col_Materia:num_col_Profesor],
                            "14 a 16 Vi/ 15 a 16 Mi Ju Sesión AVE",
                            mat_aux[3,num_col_horario_num:num_col_Alumnos],
                            "002 (Yelizcalli)",
                            mat_aux[3,num_col_Grupo:num_col_URL_MAp2017])
             renglon_3[num_col_Cambios] <- "1/2/"
             names(renglon_3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_3)
             
             renglon_4 <- c(mat_aux[4,num_col_Materia:num_col_Profesor],
                            "14 a 15 Lu Ma Mi Ju",
                            mat_aux[4,num_col_horario_num:num_col_URL_MAp2017])
             renglon_4[num_col_Cambios] <- "1/2/"
             names(renglon_4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_4)
             
             
             #' Se encontró que el profesor "Arturo García Miranda" imparte varias
             #' clases de inglés en diferentes días que pueden o no ser del mismo
             #' nivel, pero son grupos diferentes.
             #' Se quitaron todos los renglones de "m_grande" de dicho profesor
             #' para hacer los cambios correspondientes.
             #' Nota: Se toma en cuenta el horario de las clases presenciales
             #' no de las clases en línea.
             # m_grande[m_grande[,num_col_Profesor]=="Arturo García Miranda",]
             ind_aux <- 0
             for(r in 1:dim(m_grande)[1]){
               if(m_grande[r,num_col_Profesor]=="Arturo García Miranda"){
                 ind_aux <- c(ind_aux,r)}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             mat_aux_2 <- mat_aux[mat_aux[,num_col_Profesor]=="Arturo García Miranda",]
             
             ## INGLÉS I ##
             renglon_A1 <- c(mat_aux_2[2,num_col_Materia:num_col_Profesor],
                             "13 a 14 Lu Ma Mi Ju",
                             mat_aux_2[2,num_col_horario_num:num_col_URL_MAp2017])
             renglon_A1[num_col_Cambios] <- "1/2/"
             names(renglon_A1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_A1)
             
             renglon_A2 <- c(mat_aux_2[3,num_col_Materia:num_col_Profesor],
                             "14 a 16 Ma",
                             mat_aux_2[3,num_col_horario_num:num_col_URL_MAp2017])
             renglon_A2[num_col_Cambios] <- "1/2/"
             names(renglon_A2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_A2)
             
             ## INGLÉS III ##
             renglon_A3 <- c(mat_aux_2[1,num_col_Materia:num_col_Profesor],
                             "13 a 15 Sa Sesión AVE/ 14 a 16 Vi",
                             14,##14-16hrs Viernes
                             mat_aux_2[1,num_col_Lugares:num_col_Alumnos],
                             "003 (Yelizcalli)",
                             mat_aux_2[1,num_col_Grupo:num_col_URL_MAp2017])
             renglon_A3[num_col_Cambios] <- "1/2/"
             names(renglon_A3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_A3)
             
             renglon_A4 <- c(mat_aux_2[4,num_col_Materia:num_col_Profesor],
                             "14 a 16 Lu / 18 a 20 Ma Sesión AVE",
                             mat_aux_2[4,num_col_horario_num:num_col_Alumnos],
                             "003 (Yelizcalli)",
                             mat_aux_2[4,num_col_Grupo:num_col_URL_MAp2017])
             renglon_A4[num_col_Cambios] <- "1/2/"
             names(renglon_A4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_A4)
             
             renglon_A5 <- c(mat_aux_2[5,num_col_Materia:num_col_Profesor],
                             "11 a 13 Sa Sesión AVE/ 14 a 16 Ju",
                             14,##14-16hrs Jueves
                             mat_aux_2[5,num_col_Lugares:num_col_Alumnos],
                             "003 (Yelizcalli)",
                             mat_aux_2[5,num_col_Grupo:num_col_URL_MAp2017])
             renglon_A5[num_col_Cambios] <- "1/2/"
             names(renglon_A5) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_A5)
             
             ## INGLÉS IV ##
             renglon_A6 <- c(mat_aux_2[6,num_col_Materia:num_col_Profesor],
                             "18 a 20 Mi Sesión AVE/ 9 a 11 Sa",
                             9,##9-11hrs Sábado
                             mat_aux_2[6,num_col_Lugares:num_col_Alumnos],
                             "204 (Yelizcalli)",
                             mat_aux_2[6,num_col_Grupo:num_col_Cambios],
                             "M",
                             mat_aux_2[6,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_A6[num_col_Cambios] <- "1/2/"
             names(renglon_A6) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_A6)
             
             
             #' Se encontró que la profesora "Lidia Fabiola Quevedo Rojas" imparte varias
             #' clases de inglés en diferentes días que pueden o no ser del mismo
             #' nivel, pero son grupos diferentes.
             #' Se quitaron todos los renglones de "m_grande" de dicho profesor
             #' para hacer los cambios correspondientes.
             #' Nota: Se toma en cuenta el horario de las clases presenciales
             #' no de las clases en línea.
             # m_grande[m_grande[,num_col_Profesor]=="Lidia Fabiola Quevedo Rojas",]
             ind_aux <- 0
             for(r in 1:dim(m_grande)[1]){
               if(m_grande[r,num_col_Profesor]=="Lidia Fabiola Quevedo Rojas"){
                 ind_aux <- c(ind_aux,r)}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             mat_aux_3 <- mat_aux[mat_aux[,num_col_Profesor]=="Lidia Fabiola Quevedo Rojas",]
             
             ## INGLÉS I ##
             renglon_L1 <- c(mat_aux_3[1,num_col_Materia:num_col_Profesor],
                             "14 a 16 Ju",
                             mat_aux_3[1,num_col_horario_num:num_col_URL_MAp2017])
             renglon_L1[num_col_Cambios] <- "1/2/"
             names(renglon_L1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_L1)
             
             
             ## INGLÉS IV ##
             renglon_L2 <- c(mat_aux_3[3,num_col_Materia:num_col_Profesor],
                             "13 a 14 Lu Ma Mi Ju",
                             mat_aux_3[3,num_col_horario_num:num_col_URL_MAp2017])
             renglon_L2[num_col_Cambios] <- "1/2/"
             names(renglon_L2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_L2)
             
             
             renglon_L3 <- c(mat_aux_3[5,num_col_Materia:num_col_Profesor],
                             "11 a 13 Sa Sesión AVE/ 14 a 16 Vi",
                             14,##14-16hrs Viernes
                             mat_aux_3[5,num_col_Lugares:num_col_Alumnos],
                             "O216",
                             mat_aux_3[5,num_col_Grupo:num_col_URL_MAp2017])
             renglon_L3[num_col_Cambios] <- "1/2/"
             names(renglon_L3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_L3)
             
             
             ## INGLÉS V ##
             renglon_L4 <- c(mat_aux_3[2,num_col_Materia:num_col_Profesor],
                             "13 a 15 Mi Sesión AVE/ 14 a 16 Lu",
                             14,##14-16hrs Lunes
                             mat_aux_3[2,num_col_Lugares:num_col_Alumnos],
                             "O216",
                             mat_aux_3[2,num_col_Grupo:num_col_URL_MAp2017])
             renglon_L4[num_col_Cambios] <- "1/2/"
             names(renglon_L4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_L4)
             
             
             renglon_L5 <- c(mat_aux_3[2,num_col_Materia:num_col_Profesor],
                             #Se toman los nombres de Materia y de Profesor del
                             #renglón 2
                             "14 a 16 Ma / 16 a 18 Mi Sesión AVE",
                             mat_aux_3[4,num_col_horario_num:num_col_Lugares],
                             32,"003 (Yelizcalli)","9386","Actuaría","2015",
                             mat_aux_3[4,num_col_Semestre:num_col_Turno],
                             "Sexto Semestre",
                             "http://www.fciencias.unam.mx/docencia/horarios/20171/2017/1640",
                             0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
             renglon_L5[num_col_Cambios] <- "1/2/"
             names(renglon_L5) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_L5)
             
             renglon_L6 <- c(mat_aux_3[2,num_col_Materia:num_col_Profesor],
                             #Se toman los nombres de Materia y de Profesor del
                             #renglón 2
                             "14 a 16 Mi / 16 a 18 Ju Sesión AVE",
                             mat_aux_3[4,num_col_horario_num:num_col_Lugares],
                             40,"003 (Yelizcalli)","9387","Actuaría","2015",
                             mat_aux_3[4,num_col_Semestre:num_col_Turno],
                             "Sexto Semestre",
                             "http://www.fciencias.unam.mx/docencia/horarios/20171/2017/1640",
                             0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
             renglon_L6[num_col_Cambios] <- "1/2/"
             names(renglon_L6) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_L6)
           },##Fin 20171
           '20172' = {
             ind_aux <- 0
             for(r in 1:dim(m_grande)[1]){
               if(m_grande[r,num_col_Profesor]=="Olga Nelly Sánchez Cárdenas" && 
                  m_grande[r,num_col_horario_num] == "14"){
                 ind_aux <- c(ind_aux,r)}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             
             #' Las materias "Inglés II", "Inglés II" e "Inglés III"
             #' son diferentes, las clases presenciales son únicamente un día
             #' a la semana y también hay sesiones virtuales, se tienen 4
             #' clases diferentes.
             renglon_1 <- c(mat_aux[1,num_col_Materia:num_col_Profesor],
                            "13 a 15 Vi / 16 a 18 Vi Sesión virtual",
                            mat_aux[1,num_col_horario_num:num_col_Alumnos],
                            "102 (Yelizcalli)",
                            mat_aux[1,num_col_Grupo:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/2/"
             names(renglon_1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c(mat_aux[2,num_col_Materia:num_col_Profesor],
                            "13 a 15 Sa Sesión virtual / 9 a 11 Sa",
                            9,##9-11hrs Sábado
                            mat_aux[2,num_col_Lugares:num_col_Alumnos],
                            "004 (Yelizcalli)",
                            mat_aux[2,num_col_Grupo:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/2/"
             names(renglon_2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_2)
             
             
             renglon_3 <- c(mat_aux[3,num_col_Materia:num_col_Profesor],
                            "13 a 14 Lu Ma Mi Ju",
                            mat_aux[3,num_col_horario_num:num_col_URL_MAp2017])
             renglon_3[num_col_Cambios] <- "1/2/"
             names(renglon_3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_3)
             
             
             #' Las materias "Inglés VI" e "/Inglés II/Inglés II/Inglés IV/Inglés V"
             #' son diferentes, las clases presenciales son únicamente un día
             #' a la semana y también hay sesiones virtuales, se tienen 5
             #' clases diferentes.
             renglon_4 <- c(mat_aux[4,num_col_Materia:num_col_Profesor],
                            "11 a 13 Vi Sesión virtual / 14 a 16 Vi",
                            14,##14-16 Viernes
                            mat_aux[4,num_col_Lugares:num_col_Alumnos],
                            "002 (Yelizcalli)",
                            mat_aux[4,num_col_Grupo:num_col_URL_MAp2017])
             renglon_4[num_col_Cambios] <- "1/2/"
             names(renglon_4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_4)
             
             renglon_5 <- c("Inglés II",
                            mat_aux[5,num_col_Profesor],
                            "11 a 13 Ju Sesión virtual / 13 a 15 Ju",
                            13,##13-15 Jueves
                            50,46,"102 (Yelizcalli)","9343",
                            "Actuaría","2015",
                            mat_aux[5,num_col_Semestre:num_col_Turno],
                            "Segundo Semestre",
                            "http://www.fciencias.unam.mx/docencia/horarios/20172/2017/1235",
                            mat_aux[5,num_col_Act2000:num_col_NomMat_Act2006],0,
                            mat_aux[5,num_col_NomMat_CdC1994:num_col_URL_Act2006],0,
                            mat_aux[5,num_col_URL_CdC1994:num_col_URL_MAp2017])
             renglon_5[num_col_Cambios] <- "1/2/"
             names(renglon_5) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_5)
             
             renglon_6 <- c("Inglés II",
                            mat_aux[5,num_col_Profesor],
                            "11 a 13 Lu Sesión virtual / 13 a 15 Lu",
                            13,##13-15 Lunes
                            50,50,"004 (Yelizcalli)","9344",
                            "Actuaría","2015",
                            mat_aux[5,num_col_Semestre:num_col_Turno],
                            "Segundo Semestre",
                            "http://www.fciencias.unam.mx/docencia/horarios/20172/2017/1235",
                            mat_aux[5,num_col_Act2000:num_col_NomMat_Act2006],0,
                            mat_aux[5,num_col_NomMat_CdC1994:num_col_URL_Act2006],0,
                            mat_aux[5,num_col_URL_CdC1994:num_col_URL_MAp2017])
             names(renglon_6) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_6)
             
             renglon_7 <- c("Inglés IV",
                            mat_aux[5,num_col_Profesor],
                            "11 a 13 Ma Sesión virtual / 13 a 15 Ma",
                            13,##13-15 Martes
                            38,37,"004 (Yelizcalli)","9350","Actuaría","2015",
                            mat_aux[5,num_col_Semestre:num_col_Turno],
                            "Cuarto Semestre",
                            "http://www.fciencias.unam.mx/docencia/horarios/20172/2017/1436",
                            mat_aux[5,num_col_Act2000:num_col_NomMat_Act2006],0,
                            mat_aux[5,num_col_NomMat_CdC1994:num_col_URL_Act2006],0,
                            mat_aux[5,num_col_URL_CdC1994:num_col_URL_MAp2017])
             renglon_7[num_col_Cambios] <- "1/2/"
             names(renglon_7) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_7)
             
             renglon_8 <- c("Inglés V",
                            mat_aux[5,num_col_Profesor],
                            "11 a 13 Mi Sesión virtual / 13 a 15 Mi",
                            13,##13-15 Miércoles
                            44,44,"003 (Yelizcalli)","9353","Actuaría","2015",
                            mat_aux[5,num_col_Semestre:num_col_Turno],
                            "Sexto Semestre",
                            "http://www.fciencias.unam.mx/docencia/horarios/20172/2017/1640",
                            mat_aux[5,num_col_Act2000:num_col_NomMat_Act2006],0,
                            mat_aux[5,num_col_NomMat_CdC1994:num_col_URL_Act2006],0,
                            mat_aux[5,num_col_URL_CdC1994:num_col_URL_MAp2017])
             renglon_8[num_col_Cambios] <- "1/2/"
             names(renglon_8) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_8)
             
             
             #' Las materias "Seminario de Ciencias de la Computación A" y "Robótica"
             #' son diferentes, los horarios coinciden por las horas de los ayudantes.
             renglon_9 <- c(mat_aux[6,num_col_Materia:num_col_Horario],
                            17,##Empieza a las 17hrs y termina a las 18:30hrs
                            mat_aux[6,num_col_Lugares:num_col_Cambios],
                            "V",
                            mat_aux[6,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_9[num_col_Cambios] <- "1/"
             names(renglon_9) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_9)
             
             renglon_10 <- c("Robótica",
                             mat_aux[7,num_col_Profesor:num_col_Horario],
                             18,##Empieza a las 18:30hrs y termina a las 20hrs
                             mat_aux[7,num_col_Lugares:num_col_Salon],
                             "7014",
                             "Ciencias de la Computación/Ciencias de la Computación",
                             # mat_aux[6,num_col_Plan:num_col_Cambios],
                             mat_aux[7,num_col_Plan:num_col_Cambios],
                             "V",
                             mat_aux[7,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_10[num_col_Cambios] <- "1/"
             names(renglon_10) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_10)
             
             
             #' Las materias "Inglés III" e "/Inglés III/Inglés V	" son diferentes,
             #' las clases presenciales son únicamente un día a la semana y
             #' también hay sesiones virtuales, se tienen 3 clases diferentes.
             renglon_11 <- c(mat_aux[8,num_col_Materia:num_col_Profesor],
                             "14 a 16 Ma Ju",
                             mat_aux[8,num_col_horario_num:num_col_URL_MAp2017])
             names(renglon_11) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_11)
             
             renglon_12 <- c("Inglés III",
                             mat_aux[9,num_col_Profesor],
                             "14 a 16 Mi / 16 a 18 Mi Sesión virtual",
                             mat_aux[9,num_col_horario_num],
                             32,31,mat_aux[9,num_col_Salon],
                             "9348","Actuaría","2015",
                             mat_aux[9,num_col_Semestre:num_col_Turno],
                             "Tercer Semestre",
                             "http://www.fciencias.unam.mx/docencia/horarios/20172/2017/1336",
                             mat_aux[9,num_col_Act2000:num_col_NomMat_Act2006],0,
                             mat_aux[9,num_col_NomMat_CdC1994:num_col_URL_Act2006],0,
                             mat_aux[9,num_col_URL_CdC1994:num_col_URL_MAp2017])
             renglon_12[num_col_Cambios] <- "1/2/"
             names(renglon_12) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_12)
             
             renglon_13 <- c("Inglés V",
                             mat_aux[9,num_col_Profesor],
                             "14 a 16 Lu / 16 a 18 Lu Sesión virtual",
                             mat_aux[9,num_col_horario_num],
                             40,38,"003 (Yelizcalli)","9354","Actuaría","2015",
                             mat_aux[9,num_col_Semestre:num_col_Turno],
                             "Sexto Semestre",
                             "http://www.fciencias.unam.mx/docencia/horarios/20172/2017/1640",
                             mat_aux[9,num_col_Act2000:num_col_NomMat_Act2006],0,
                             mat_aux[9,num_col_NomMat_CdC1994:num_col_URL_Act2006],0,
                             mat_aux[9,num_col_URL_CdC1994:num_col_URL_MAp2017])
             renglon_13[num_col_Cambios] <- "1/2/"
             names(renglon_13) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_13)
           },##Fin 20172
           '20191' = {
             ind_aux <- 0
             for(r in 1:dim(m_grande)[1]){
               if(m_grande[r,num_col_Profesor]=="Canek Peláez Valdés" && 
                  m_grande[r,num_col_horario_num] == "10"){
                 ind_aux <- c(ind_aux,r)}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             ind_aux <- 0
             for(r in 1:dim(m_grande)[1]){
               if(m_grande[r,num_col_Profesor]=="Maria Teresa Saavedra Sordo" && 
                  m_grande[r,num_col_horario_num] == "14"){
                 ind_aux <- c(ind_aux,r)}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             #' Las materias "Seminario de Apoyo a la Titulación en Actuaría B" y
             #' "Taller de Modelación I" son diferentes, los horarios
             #' coinciden pero los días que son impartidas difieren.
             renglon_1 <- c(mat_aux[1,num_col_Materia:num_col_Profesor],
                            "13 a 14:30	Lu Vi",
                            mat_aux[1,num_col_horario_num:num_col_Alumnos],
                            "Cubículo 028, Departamento de Matemáticas",
                            mat_aux[1,num_col_Grupo:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/2/"
             names(renglon_1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c(mat_aux[2,num_col_Materia:num_col_Profesor],
                            "13 a 15 Ma Ju",
                            mat_aux[2,num_col_horario_num:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/2/"
             names(renglon_2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_2)
             
             
             #' Las materias "Redes de Computadoras" e "Introducción a Ciencias de
             #' la Computación I" son diferentes, los horarios coinciden por las
             #' horas de los ayudantes.
             renglon_3 <- c("Redes de Computadoras",
                            mat_aux[3,num_col_Profesor:num_col_URL_MAp2017])
             renglon_3[num_col_Cambios] <- "1/"
             names(renglon_3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_3)
             
             renglon_4 <- c("Introducción a Ciencias de la Computación I",
                            mat_aux[4,num_col_Profesor:num_col_Horario],
                            13,##Empieza a las 13hrs y termina a las 14:30hrs
                            mat_aux[4,num_col_Lugares:num_col_URL_MAp2017])
             renglon_4[num_col_Cambios] <- "1/"
             names(renglon_4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_4)
             
             
             #' La materia "Fundamentos de Bases de Datos" tiene 2 horarios distintos,
             #' hay una coincidencia por las horas de los ayudantes.
             renglon_5 <- c("Fundamentos de Bases de Datos",
                            mat_aux[5,num_col_Profesor:num_col_Horario],
                            8,##8-9 en el salón 103 del Tlahuizcalpan
                            mat_aux[5,num_col_Lugares:num_col_URL_MAp2017])
             renglon_5[num_col_Cambios] <- "1/"
             names(renglon_5) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_5)
             
             renglon_6 <- c("Fundamentos de Bases de Datos",
                            mat_aux[6,num_col_Profesor:num_col_Horario],
                            9,##9-10
                            mat_aux[6,num_col_Lugares:num_col_URL_MAp2017])
             renglon_6[num_col_Cambios] <- "1/"
             names(renglon_6) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_6)
             
             
             #' Las materias "Mecánica Vectorial" y "Relatividad" son
             #' diferentes, los horarios coinciden pero los días en
             #' los que se imparten difieren.
             renglon_7 <- c(mat_aux[7,num_col_Materia:num_col_Profesor],
                            "8 a 10	Lu Mi Vi",
                            mat_aux[7,num_col_horario_num:num_col_URL_MAp2017])
             renglon_7[num_col_Cambios] <- "1/2/"
             names(renglon_7) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_7)
             
             renglon_8 <- c(mat_aux[8,num_col_Materia:num_col_Profesor],
                            "8 a 9:30	Ma Ju",
                            mat_aux[8,num_col_horario_num:num_col_URL_MAp2017])
             renglon_8[num_col_Cambios] <- "1/2/"
             names(renglon_8) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_8)
             
             
             #' Las materias "Seminario de Ciencias de la Computación B" e
             #' "Introducción a Ciencias de la Computación I" son diferentes,
             #' los horarios coinciden por las horas de los ayudantes.
             renglon_9 <- mat_aux[9,num_col_Materia:num_col_URL_MAp2017]
             renglon_9[num_col_Cambios] <- "1/"
             names(renglon_9) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_9)
             
             renglon_10 <- c("Introducción a Ciencias de la Computación I",
                             mat_aux[10,num_col_Profesor:num_col_Horario],
                             13,##Empieza a las 13hrs y termina a las 14:30hrs
                             mat_aux[10,num_col_Lugares:num_col_URL_MAp2017])
             renglon_10[num_col_Cambios] <- "1/"
             names(renglon_10) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_10)
             
             
             #' La materia "Inglés I" es impartida en días distintos de la
             #' semana por la profesora "Maria Teresa Saavedra Sordo" en el
             #' mismo horario.
             renglon_11 <- c(mat_aux[11,num_col_Materia:num_col_Profesor],
                             "14 a 16 Ma / 9 a 11 Sa Sesión virtual",
                             mat_aux[11,num_col_horario_num:num_col_Alumnos],
                             "O215",
                             mat_aux[11,num_col_Grupo:num_col_URL_MAp2017])
             renglon_11[num_col_Cambios] <- "1/2/"
             names(renglon_11) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_11)
             
             renglon_12 <- c(mat_aux[12,num_col_Materia:num_col_Profesor],
                             "14 a 16 Ju / 7 a 9 Vi Sesión virtual",
                             mat_aux[12,num_col_horario_num:num_col_URL_MAp2017])
             renglon_12[num_col_Cambios] <- "1/2/"
             names(renglon_12) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_12)
           },##Fin 20191
           '20192' = {
             ind_aux <- 0
             prof_aux_1 <- c("Lidia Fabiola Quevedo Rojas",
                             "María Azucena Rivera Vidal",
                             "Maria Teresa Saavedra Sordo")
             for(r in 1:dim(m_grande)[1]){
               for(p in 1:length(prof_aux_1)){
                 if(m_grande[r,num_col_Profesor]==prof_aux_1[p] && 
                    m_grande[r,num_col_horario_num] == "13"){
                   ind_aux <- c(ind_aux,r)}}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             
             #' Las materias de inglés sólo se imparten un día a la semana, los
             #' días en que se imparten las clases se van a colocar en la
             #' columna "Horario" para que sólo haya números en la columna
             #' "horario_num"
             
             #' Las materias "Inglés III" e "Inglés IV" son diferentes,
             #' las clases presenciales son únicamente un día a la semana
             #' y también hay sesiones virtuales.
             renglon_1 <- c(mat_aux[1,num_col_Materia:num_col_Profesor],
                            "13 a 14 Lu Mi/ 9 a 11	Sa Sesión virtual",
                            mat_aux[1,num_col_horario_num:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/2/"
             names(renglon_1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c(mat_aux[2,num_col_Materia:num_col_Profesor],
                            "13 a 15 Sa Sesión virtual/ 14 a 16 Vi",
                            14,##14-16 Viernes
                            mat_aux[2,num_col_Lugares:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/2/"
             names(renglon_2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_2)
             
             #' Las materias "Inglés II", "Inglés III" e "Inglés IV" son diferentes,
             #' las clases presenciales son únicamente un día a la semana
             #' y también hay sesiones virtuales. Se tienen 4 grupos distintos.
             mat_aux_2 <- mat_aux[mat_aux[,num_col_Profesor]=="Lidia Fabiola Quevedo Rojas",]
             
             ## INGLÉS II ##
             renglon_A1 <- c(mat_aux_2[1,num_col_Materia:num_col_Profesor],
                             "14 a 15 Ma Ju / 7 a 9	Sa Sesión virtual",
                             mat_aux_2[1,num_col_horario_num:num_col_URL_MAp2017])
             renglon_A1[num_col_Cambios] <- "1/2/"
             names(renglon_A1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_A1)
             
             ## INGLÉS III ##
             renglon_A2 <- c(mat_aux_2[3,num_col_Materia:num_col_Profesor],
                             "13 a 14 Lu Mi / 9 a 11 Sa Sesión virtual",
                             mat_aux_2[3,num_col_horario_num:num_col_URL_MAp2017])
             renglon_A2[num_col_Cambios] <- "1/2/"
             names(renglon_A2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_A2)
             
             ## INGLÉS IV ##
             renglon_A3 <- c(mat_aux_2[4,num_col_Materia:num_col_Profesor],
                             "13 a 15 Sa Sesión virtual / 8 a 10 Ma",
                             8,##8-10 Martes
                             mat_aux_2[4,num_col_Lugares:num_col_URL_MAp2017])
             renglon_A3[num_col_Cambios] <- "1/2/"
             names(renglon_A3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_A3)
             
             ## INGLÉS V ##
             renglon_A4 <- c("Inglés V",
                             mat_aux_2[2,num_col_Profesor],
                             "14 a 16 Lu / 18 a 20 Ju Sesión virtual",
                             mat_aux_2[2,num_col_horario_num:num_col_Lugares],
                             15,
                             mat_aux_2[2,num_col_Salon],
                             "7096","Ciencias de la Computación","2013",
                             mat_aux_2[2,num_col_Semestre:num_col_Turno],
                             "Quinto Semestre",
                             "http://www.fciencias.unam.mx/docencia/horarios/20192/1556/1535",
                             mat_aux_2[2,num_col_Act2000:num_col_NomMat_CdC1994],0,0,0,0,
                             mat_aux_2[2,num_col_URL_Act2006:num_col_URL_CdC1994],0,0,0)
             renglon_A4[num_col_Cambios] <- "1/2/"
             names(renglon_A4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_A4)
             
             renglon_A5 <- c("Inglés V",
                             mat_aux_2[2,num_col_Profesor],
                             "14 a 16 Mi / 18 a 20 Vi Sesión virtual",
                             mat_aux_2[2,num_col_horario_num:num_col_Lugares],
                             14,
                             mat_aux_2[2,num_col_Salon],
                             "7097","Ciencias de la Computación","2013",
                             mat_aux_2[2,num_col_Semestre:num_col_Turno],
                             "Quinto Semestre",
                             "http://www.fciencias.unam.mx/docencia/horarios/20192/1556/1535",
                             mat_aux_2[2,num_col_Act2000:num_col_NomMat_CdC1994],0,0,0,0,
                             mat_aux_2[2,num_col_URL_Act2006:num_col_URL_CdC1994],0,0,0)
             renglon_A5[num_col_Cambios] <- "1/2/"
             names(renglon_A5) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_A5)
             
             
             #' Las materias "Inglés IV" de la profesora "María Azucena Rivera Vidal"
             #' son diferentes, los horarios son distintos.
             renglon_3 <- c(mat_aux[6,num_col_Materia:num_col_Profesor],
                            "13 a 14 Ma Ju / 9 a 11	Sa Sesión virtual",
                            mat_aux[6,num_col_horario_num:num_col_URL_MAp2017])
             renglon_3[num_col_Cambios] <- "1/2/"
             names(renglon_3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_3)
             
             renglon_4 <- c(mat_aux[10,num_col_Materia:num_col_Profesor],
                            "13 a 15 Sa Sesión virtual / 16 a 18 Ju",
                            16,##16-18 Jueves
                            mat_aux[10,num_col_Lugares:num_col_Cambios],
                            "V",
                            mat_aux[10,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_4[num_col_Cambios] <- "1/2/"
             names(renglon_4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_4)
             
             
             #' Las materias "Inglés IV" e "Inglés II" son diferentes,
             #' las clases presenciales son diferentes días a la semana,
             #' también hay sesiones virtuales.
             renglon_5 <- c(mat_aux[7,num_col_Materia:num_col_Profesor],
                            "13 a 14 Lu Mi/ 9 a 11 Sa Sesión virtual",
                            mat_aux[7,num_col_horario_num:num_col_URL_MAp2017])
             renglon_5[num_col_Cambios] <- "1/2/"
             names(renglon_5) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_5)
             
             renglon_6 <- c(mat_aux[8,num_col_Materia:num_col_Profesor],
                            "13 a 15 Sa Sesión virtual / 14 a 16 Ma",
                            14,##14-16 Martes
                            mat_aux[8,num_col_Lugares:num_col_URL_MAp2017])
             renglon_6[num_col_Cambios] <- "1/2/"
             names(renglon_6) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_6)
           },##Fin 20192
           '20201' = {
             ind_aux <- 0
             for(r in 1:dim(m_grande)[1]){
               if(m_grande[r,num_col_Profesor]=="Silvia Loera Rivera" && 
                  m_grande[r,num_col_horario_num] == "14"){
                 ind_aux <- c(ind_aux,r)}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             #' Las materias "Redes de Computadoras" e "Introducción a Ciencias
             #' de la Computación" son diferentes, los horarios coinciden
             #' por las horas de los ayudantes.
             renglon_1 <- c("Redes de Computadoras",
                            mat_aux[1,num_col_Profesor:num_col_Salon],
                            "7004",
                            mat_aux[1,num_col_Carrera:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/"
             names(renglon_1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c("Introducción a Ciencias de la Computación",
                            mat_aux[2,num_col_Profesor:num_col_Horario],
                            13,
                            mat_aux[2,num_col_Lugares:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/"
             names(renglon_2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_2)
             
             #' Las materias de inglés sólo se imparten un día a la semana, los
             #' días en que se imparten las clases se van a colocar en la
             #' columna "Horario" para que sólo haya números en la columna
             #' "horario_num"
             
             #' Las materias "Inglés I", "Inglés III", "/Inglés V/Inglés VI"
             #' son diferentes, las clases presenciales son únicamente un día
             #' a la semana y también hay sesiones virtuales, se tienen 4
             #' clases diferentes.
             renglon_3 <- c(mat_aux[3,num_col_Materia:num_col_Profesor],
                            "13 a 15 Ma",
                            13,##Se imparte los martes
                            mat_aux[3,num_col_Lugares:num_col_URL_MAp2017])
             renglon_3[num_col_Cambios] <- "1/2/"
             names(renglon_3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_3)
             
             renglon_4 <- c(mat_aux[4,num_col_Materia:num_col_Profesor],
                            "13 a 15 Vi",
                            13,##Se imparte los viernes
                            mat_aux[4,num_col_Lugares:num_col_Alumnos],
                            "P104",
                            mat_aux[4,num_col_Grupo:num_col_URL_MAp2017])
             renglon_4[num_col_Cambios] <- "1/2/"
             names(renglon_4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_4)
             
             renglon_5 <- c("Inglés V",
                            mat_aux[5,num_col_Profesor],
                            "15 a 17	Lu Sesión virtual / 13 a 15 Lu",13,25,25,"P105","9325",
                            "Actuaría","2015",
                            mat_aux[5,num_col_Semestre:num_col_Turno],
                            "Sexto Semestre",
                            "http://www.fciencias.unam.mx/docencia/horarios/20201/2017/1640",
                            mat_aux[5,num_col_Act2000:num_col_NomMat_Act2006],0,
                            mat_aux[5,num_col_NomMat_CdC1994:num_col_URL_Act2006],0,
                            mat_aux[5,num_col_URL_CdC1994:num_col_URL_MAp2017])
             renglon_5[num_col_Cambios] <- "1/2/"
             names(renglon_5) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_5)
             
             renglon_6 <- c("Inglés VI",
                            mat_aux[5,num_col_Profesor],
                            "13 a 15 Mi / 15 a 17 Mi Sesión virtual",13,25,26,
                            "P105","9327","Actuaría","2015",
                            mat_aux[5,num_col_Semestre:num_col_Turno],
                            "Séptimo Semestre",
                            "http://www.fciencias.unam.mx/docencia/horarios/20201/2017/1740",
                            mat_aux[5,num_col_Act2000:num_col_NomMat_Act2006],0,
                            mat_aux[5,num_col_NomMat_CdC1994:num_col_URL_Act2006],0,
                            mat_aux[5,num_col_URL_CdC1994:num_col_URL_MAp2017])
             renglon_6[num_col_Cambios] <- "1/2/"
             names(renglon_6) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_6)
             
             
             #' Las materias "Taller de Herramientas Computacionales" y "Manejo de
             #' Datos" son diferentes, el horario es el mismo, pero se imparten
             #' en días distintos
             renglon_7 <- c(mat_aux[6,num_col_Materia:num_col_Profesor],
                            "7 a 9 Ma Ju",7,##Se imparte los martes y jueves de 7-9hrs
                            mat_aux[6,num_col_Lugares:num_col_URL_MAp2017])
             renglon_7[num_col_Cambios] <- "1/2/"
             names(renglon_7) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_7)
             
             renglon_8 <- c("Manejo de Datos",
                            mat_aux[7,num_col_Profesor],
                            "7 a 8 Lu Mi Vi",7,
                            mat_aux[7,num_col_Lugares:num_col_Salon],
                            "9172",
                            mat_aux[7,num_col_Carrera:num_col_URL_MAp2017])
             renglon_8[num_col_Cambios] <- "1/2/"
             names(renglon_8) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_8)
             
             
             #' Las materias "Inglés III" e "/Inglés V/Inglés VI"
             #' son diferentes, las clases presenciales son únicamente un día
             #' a la semana y también hay sesiones virtuales, se tienen 3
             #' clases diferentes.
             renglon_9 <- c(mat_aux[8,num_col_Materia:num_col_Profesor],
                            "14 a 16 Vi Sesión virtual / 14 a 16 Lu",
                            14,#Se imparten los lunes y viernes
                            mat_aux[8,num_col_Lugares:num_col_Alumnos],
                            "P104",
                            mat_aux[8,num_col_Grupo:num_col_URL_MAp2017])
             renglon_9[num_col_Cambios] <- "1/2/"
             names(renglon_9) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_9)
             
             renglon_10 <- c("Inglés V",
                             mat_aux[9,num_col_Profesor],
                             "14 a 16 Ma / 16 a 18 Ma Sesión virtual",
                             14,#Se imparte los martes
                             25,28,"P105","9326","Actuaría","2015",
                             mat_aux[9,num_col_Semestre:num_col_Turno],
                             "Sexto Semestre",
                             "http://www.fciencias.unam.mx/docencia/horarios/20201/2017/1640",
                             mat_aux[9,num_col_Act2000:num_col_NomMat_Act2006],0,
                             mat_aux[9,num_col_NomMat_CdC1994:num_col_URL_Act2006],0,
                             mat_aux[9,num_col_URL_CdC1994:num_col_URL_MAp2017])
             renglon_10[num_col_Cambios] <- "1/2/"
             names(renglon_10) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_10)
             
             renglon_11 <- c("Inglés VI",
                             mat_aux[9,num_col_Profesor],
                             "14 a 16 Ju / 16 a 18 Ju Sesión virtual",
                             14,#Se imparten los jueves
                             25,25,"P105","9328","Actuaría","2015",
                             mat_aux[9,num_col_Semestre:num_col_Turno],
                             "Séptimo Semestre",
                             "http://www.fciencias.unam.mx/docencia/horarios/20201/2017/1740",
                             mat_aux[9,num_col_Act2000:num_col_NomMat_Act2006],0,
                             mat_aux[9,num_col_NomMat_CdC1994:num_col_URL_Act2006],0,
                             mat_aux[9,num_col_URL_CdC1994:num_col_URL_MAp2017])
             renglon_11[num_col_Cambios] <- "1/2/"
             names(renglon_11) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_11)
             
             
             #' Las materias "Inglés I" e "Inglés VI" son diferentes, las
             #' clases presenciales son únicamente un día a la semana y
             #' también hay sesiones virtuales, se tienen 2 clases diferentes.
             renglon_12 <- c(mat_aux[10,num_col_Materia:num_col_Profesor],
                             "14 a 16	Mi",14,#Se imparte los miércoles
                             mat_aux[10,num_col_Lugares:num_col_URL_MAp2017])
             renglon_12[num_col_Cambios] <- "1/2/"
             names(renglon_12) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_12)
             
             renglon_13 <- c(mat_aux[11,num_col_Materia:num_col_Profesor],
                             "14 a 16 Sesión presencial / 16 a 18	Sesión virtual",14,
                             mat_aux[11,num_col_Lugares:num_col_URL_MAp2017])
             renglon_13[num_col_Cambios] <- "1/2/"
             names(renglon_13) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_13)
           }##Fin 20201
    )##Fin switch(sem_info)
  }else{##Si NO hay repeticiones
    m_grande <- m_grande_SIN_MOD
  }
  
  # save(m_grande, file = paste0("m_grande por semestre/m_grande_",sem_info,".RData"))
  save(m_grande, file = paste0("m_grande por semestre SIN INGLES/m_grande_SIN_ING_",sem_info,".RData"))
  return(m_grande)
}



# imprime_info_idiomas_1_sem ----------------------------------------------
#' Title imprime_info_idiomas_1_sem: Función que imprime la información de
#' "idioma". Arroja una variable binaria que indica si la matriz "m_grande"
#' del semestre "sem_info" requiere modificación.
#'
#' @param nom_archivo : Nombre del archivo que se va a cargar con la
#' matriz "m_grande".
#' @param sem_info: Semestre del que se desea obtener información.
#' @param idioma: Variable de tipo "char" la cual indica el idioma del
#' que se debe tomar la información.
#'
#' @example nom_archivo <- "m_grande por semestre SIN INGLES/m_grande_SIN_ING_20182.RData"
#' @example sem_info <- 20182
#' @example idioma <- "Inglés"
#'
#' @return mod_1si_0no: Variable binaria que indica si la matriz "m_grande"
#' del semestre "sem_info" requiere modificación.
imprime_info_idiomas_1_sem <- function(nom_archivo,sem_info,idioma){
  ##Se definen las variables que se van a utilizar:
  num_col_Materia <- arroja_ind_col_MG("Materia")##1
  num_col_NomMat_Act2000 <- arroja_ind_col_MG("NomMat_Act2000")##23
  num_col_NomMat_MAp2017 <- arroja_ind_col_MG("NomMat_MAp2017")##29
  mod_1si_0no <- 0
  
  load(nom_archivo)#Se carga "m_grande" de "idioma" del semestre "sem_info"
  vec_ind_aux <- 0
  
  #Se obtienen los renglones que contienen grupos de "idioma"
  for(d in 1:dim(m_grande)[1]){##Recorre renglones
    texto_1 <- substr(m_grande[d,num_col_Materia],1,6)
    texto_2 <- substr(m_grande[d,num_col_Materia],1,7)
    # if(texto_1=="Inglés" || texto_2=="/Inglés"){
    if(texto_1==idioma || texto_2==paste0("/",idioma)){
      vec_ind_aux <- c(vec_ind_aux,d)}}
  
  if(length(vec_ind_aux) > 1){#Si hay clases de "idioma"
    vec_ind_aux <- vec_ind_aux[-1]#Se quita el cero inicial
    
    #' El vector "vec_ind_aux_2" contiene los índices de los renglones
    #' de "m_grande" que tienen más de una materia de inglés
    vec_ind_aux_2 <- 0
    for(d in 1:length(vec_ind_aux)){##Recorre los índices de los renglones
      if(!all(m_grande[vec_ind_aux[d],
                       num_col_NomMat_Act2000:num_col_NomMat_MAp2017]==rep(0,7))){
        vec_ind_aux_2 <- c(vec_ind_aux_2,vec_ind_aux[d])}}
    if(length(vec_ind_aux_2) > 1){#En caso de que haya clases repetidas
      vec_ind_aux_2 <- vec_ind_aux_2[-1]#Se quita el cero inicial
      
      cat("\n En el semestre ",sem_info," se tienen ",length(vec_ind_aux_2),
          " clases repetidas de ",idioma)
      mod_1si_0no <- 1
      
    }else{#En caso de que en "m_grande" no haya clases repetidas
      cat("\n La matriz m_grande del semestre ",sem_info,
          " no tiene clases repetidas de ",idioma)
    }
  }else{#En caso de que en "m_grande" no haya clases de "idioma"
    cat("\n La matriz m_grande del semestre ",sem_info,
        " no tiene clases de ",idioma)
  }
  
  return(mod_1si_0no)
}


# imprime_info_idiomas ----------------------------------------------------
#' Title imprime_info_idiomas: Función que imprime la información de los
#' idiomas de los semestres 2008-1 al 2020-1. Arroja un vector con los
#' semestres que requieren modificación de las materias de "idioma".
#'
#' @param idioma: Variable de tipo "char" la cual indica el idioma del
#' que se debe tomar la información.
#'
#' @example idioma <- "Inglés"
#'
#' @return vec_sem_idiomas: Vector con los semestres que requieren
#' modificación en "m_grande".
imprime_info_idiomas <- function(idioma){
  semestres <- (20081:20201)[(20081:20201)%% 10>0 &(20081:20201)%% 10<3]
  vec_sem_idiomas <- 0
  for(s in 1:length(semestres)){
    sem_info <- semestres[s]
    nom_archivo <- paste0("m_grande por semestre SIN INGLES/m_grande_SIN_ING_",
                          sem_info,".RData")
    if(file.exists(nom_archivo)){
      mod_1si_0no <- imprime_info_idiomas_1_sem(nom_archivo,sem_info,idioma)}
    
    if(mod_1si_0no == 1){
      vec_sem_idiomas <- c(vec_sem_idiomas,sem_info)}
  }#fin for(s)
  if(length(vec_sem_idiomas) > 1){
    vec_sem_idiomas <- vec_sem_idiomas[-1]
  }
  return(vec_sem_idiomas)
}


# revisa_gpos_idiomas_1_sem -----------------------------------------------
#' Title revisa_gpos_idiomas_1_sem: Función en la que se identifican y
#' separan los grupos de idiomas que se imparten en días distintos pero en
#' el mismo horario.
#'
#' @param sem_info: Semestre del que se desea obtener información.
#' @param idioma: Variable de tipo "char" la cual indica el idioma del
#' que se debe tomar la información.
#'
#' @example sem_info <- 20182
#' @example idioma <- "Inglés"
#' 
#' @return m_grande: Matriz de 36 columnas (Materia,Profesor,Horario,
#' horario_num,Lugares,Alumnos,Salon,Grupo,Carrera,Plan,Semestre,Cambios,
#' Turno,Semestre_de_materia,url,Act2000,Act2006,Act2015,CdC1994,CdC2013,
#' Mat1983,MAp2017,NomMat_Act2000,NomMat_Act2006,NomMat_Act2015,
#' NomMat_CdC1994,NomMat_CdC2013,NomMat_Mat1983,NomMat_MAp2017,URL_Act2000,
#' URL_Act2006,URL_Act2015,URL_CdC1994,URL_CdC2013,URL_Mat1983,URL_MAp2017),
#' con la información de "sem_info". Las columnas 16-22 son columnas
#' binarias las cuales indican con un 1 si el grupo del i-ésimo renglón
#' pertenece a la carrera y plan correspondiente al nombre de cada columna,
#' hay un 0 e.o.c. Las columnas 23-29 indican el nombre de las materias
#' dependiendo del plan y carrera al que pertenecen. Las columnas 30-36 
#' indican las URLs dependiendo del plan y carrera al que pertenecen.
#'
revisa_gpos_idiomas_1_sem <- function(sem_info,idioma){
  ##Se definen las variables que se van a utilizar:
  num_col_Materia <- arroja_ind_col_MG("Materia")##1
  num_col_Profesor <- arroja_ind_col_MG("Profesor")##2
  num_col_horario_num <- arroja_ind_col_MG("horario_num")##4
  num_col_Semestre <- arroja_ind_col_MG("Semestre")##11
  num_col_Cambios <- arroja_ind_col_MG("Cambios")##12
  num_col_Turno <- arroja_ind_col_MG("Turno")##13
  num_col_Act2000 <- arroja_ind_col_MG("Act2000")##16
  num_col_NomMat_Act2000 <- arroja_ind_col_MG("NomMat_Act2000")##23
  num_col_NomMat_Act2006 <- arroja_ind_col_MG("NomMat_Act2006")##24
  num_col_NomMat_CdC1994 <- arroja_ind_col_MG("NomMat_CdC1994")##26
  num_col_NomMat_MAp2017 <- arroja_ind_col_MG("NomMat_MAp2017")##29
  num_col_URL_Act2006 <- arroja_ind_col_MG("URL_Act2006")##31
  num_col_URL_CdC1994 <- arroja_ind_col_MG("URL_CdC1994")##33
  num_col_URL_MAp2017 <- arroja_ind_col_MG("URL_MAp2017")##36
  
  #Se define el vector que contiene los semestres que requieren modificación
  vec_sem_idiomas <- imprime_info_idiomas(idioma)
  
  switch(idioma,
         'Inglés' = {
           #Se carga la matriz "m_grande" de "sem_info"
           nom_archivo <- paste0("m_grande por semestre SIN INGLES/m_grande_SIN_ING_",
                                 sem_info,".RData")
           load(nom_archivo)
           # View(m_grande)
           m_grande_SIN_ING <- m_grande
           
           if(any(sem_info == vec_sem_idiomas)){#En caso de que se requieran modificaciones
             #Se obtienen los índices de las materias de inglés
             vec_ind_aux <- 0
             for(d in 1:dim(m_grande_SIN_ING)[1]){##Recorre renglones
               texto_1 <- substr(m_grande_SIN_ING[d,num_col_Materia],1,6)
               texto_2 <- substr(m_grande_SIN_ING[d,num_col_Materia],1,7)
               if(texto_1=="Inglés" || texto_2=="/Inglés"){
                 vec_ind_aux <- c(vec_ind_aux,d)}}
             vec_ind_aux <- vec_ind_aux[-1]#Se quita el cero inicial
             
             #' El vector "vec_ind_aux_2" contiene los índices de los renglones
             #' de "m_grande_SIN_ING" que tienen más de una materia de inglés
             vec_ind_aux_2 <- 0
             for(d in 1:length(vec_ind_aux)){##Recorre los índices de los renglones
               if(!all(m_grande_SIN_ING[vec_ind_aux[d],
                                        num_col_NomMat_Act2000:num_col_NomMat_MAp2017]==rep(0,7))){
                 vec_ind_aux_2 <- c(vec_ind_aux_2,vec_ind_aux[d])}}
             vec_ind_aux_2 <- vec_ind_aux_2[-1]#Se quita el cero inicial
             
             #Se modifica "m_grande"
             mat_aux <- m_grande_SIN_ING[vec_ind_aux_2,]
             m_grande <- m_grande_SIN_ING[-vec_ind_aux_2,]
             
             switch(sem_info,
                    '20152' = {
                      #' Las materias "/Inglés I/Inglés II" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      renglon_1 <- c("Inglés I",
                                     mat_aux[1,num_col_Profesor],
                                     "14 a 16	Ma Ju",
                                     14,##14-16hrs Martes Jueves
                                     40,40,"P102","7347",
                                     "Ciencias de la Computación","2013",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Primer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20152/1556/1124",
                                     0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_1[num_col_Cambios] <- "1/2/"
                      names(renglon_1) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_1)
                      
                      renglon_2 <- c("Inglés II",
                                     mat_aux[1,num_col_Profesor],
                                     "14 a 16 Lu Mi",
                                     14,##14-16hrs Lunes Miércoles
                                     40,38,"O122","7348",
                                     "Ciencias de la Computación","2013",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Segundo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20152/1556/1223",
                                     0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_2[num_col_Cambios] <- "1/2/"
                      names(renglon_2) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_2)
                      
                      
                      #' Las materias "/Inglés II/Inglés III" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      renglon_3 <- c("Inglés II",
                                     mat_aux[2,num_col_Profesor],
                                     "14 a 16 Lu Mi",
                                     14,##14-16hrs Lunes Miércoles
                                     42,41,"P201","7349",
                                     "Ciencias de la Computación","2013",
                                     mat_aux[2,num_col_Semestre:num_col_Turno],
                                     "Segundo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20152/1556/1223",
                                     0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_3[num_col_Cambios] <- "1/2/"
                      names(renglon_3) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_3)
                      
                      renglon_4 <- c("Inglés III",
                                     mat_aux[2,num_col_Profesor],
                                     "14 a 16 Ma Ju",
                                     14,##14-16hrs Martes Jueves
                                     40,25,"P201","7350",
                                     "Ciencias de la Computación","2013",
                                     mat_aux[2,num_col_Semestre:num_col_Turno],
                                     "Tercer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20152/1556/1322",
                                     0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_4[num_col_Cambios] <- "1/2/"
                      names(renglon_4) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_4)
                    },##Fin 20152
                    '20161' = {
                      #' Las materias "/Inglés II/Inglés I/Inglés III" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      renglon_1 <- c("Inglés II",
                                     mat_aux[1,num_col_Profesor],
                                     "14 a 16	Vi",
                                     14,##14-16hrs Viernes
                                     77,74,"201 (Yelizcalli)","9253","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Segundo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20161/2017/1235",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_1[num_col_Cambios] <- "1/2/"
                      names(renglon_1) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_1)
                      
                      renglon_2 <- c("Inglés I",
                                     mat_aux[1,num_col_Profesor],
                                     "14 a 16 Ma Ju",
                                     14,##14-16hrs Martes Jueves
                                     60,54,"204 (Yelizcalli)","7096",
                                     "Ciencias de la Computación","2013",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Primer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20161/1556/1124",
                                     0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_2[num_col_Cambios] <- "1/2/"
                      names(renglon_2) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_2)
                      
                      renglon_3 <- c("Inglés III",
                                     mat_aux[1,num_col_Profesor],
                                     "14 a 16 Lu Mi",
                                     14,##14-16hrs Lunes Miércoles
                                     30,26,"201 (Yelizcalli)","7099",
                                     "Ciencias de la Computación","2013",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Tercer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20161/1556/1322",
                                     0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_3[num_col_Cambios] <- "1/2/"
                      names(renglon_3) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_3)
                      
                      
                      #' Las materias "/Inglés III/Inglés I/Inglés II" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      renglon_4 <- c("Inglés III",
                                     mat_aux[2,num_col_Profesor],
                                     "9 a 11 Sa",
                                     9,##9-11hrs Sábado
                                     40,39,"011",##Tlahuizcalpan
                                     "9261","Actuaría","2015",
                                     mat_aux[2,num_col_Semestre:num_col_Turno],
                                     "Tercer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20161/2017/1336",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_4[num_col_Cambios] <- "1/2/"
                      names(renglon_4) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_4)
                      
                      renglon_5 <- c("Inglés III",
                                     mat_aux[2,num_col_Profesor],
                                     "11 a 13 Sa",
                                     11,##11-13hrs Sábado
                                     40,33,"002 (Yelizcalli)","9263","Actuaría","2015",
                                     mat_aux[2,num_col_Semestre:num_col_Turno],
                                     "Tercer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20161/2017/1336",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_5[num_col_Cambios] <- "1/2/"
                      names(renglon_5) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_5)
                      
                      renglon_6 <- c("Inglés III",
                                     mat_aux[2,num_col_Profesor],
                                     "14 a 16 Vi",
                                     14,##14-16hrs Viernes
                                     40,40,"106 (Yelizcalli)","9265","Actuaría","2015",
                                     mat_aux[2,num_col_Semestre:num_col_Turno],
                                     "Tercer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20161/2017/1336",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_6[num_col_Cambios] <- "1/2/"
                      names(renglon_6) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_6)
                      
                      renglon_7 <- c("Inglés I",
                                     mat_aux[2,num_col_Profesor],
                                     "14 a 16 Ma Ju",
                                     14,##14-16hrs Martes Jueves
                                     60,53,"203 (Yelizcalli)","7095",
                                     "Ciencias de la Computación","2013",
                                     mat_aux[2,num_col_Semestre:num_col_Turno],
                                     "Primer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20161/1556/1124",
                                     0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_7[num_col_Cambios] <- "1/2/"
                      names(renglon_7) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_7)
                      
                      renglon_8 <- c("Inglés II",
                                     mat_aux[2,num_col_Profesor],
                                     "14 a 16 Lu Mi",
                                     14,##14-16hrs Lunes Miércoles
                                     45,40,"202 (Yelizcalli)","7097",
                                     "Ciencias de la Computación","2013",
                                     mat_aux[2,num_col_Semestre:num_col_Turno],
                                     "Segundo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20161/1556/1223",
                                     0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_8[num_col_Cambios] <- "1/2/"
                      names(renglon_8) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_8)
                      
                      
                      #' Las materias "/Inglés III/Inglés IV" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      renglon_9 <- c("Inglés III",
                                     mat_aux[3,num_col_Profesor],
                                     "14 a 16 Ma Ju",
                                     14,##14-16hrs Martes Jueves
                                     40,16,"O129","7098",
                                     "Ciencias de la Computación","2013",
                                     mat_aux[3,num_col_Semestre:num_col_Turno],
                                     "Tercer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20161/1556/1322",
                                     0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_9[num_col_Cambios] <- "1/2/"
                      names(renglon_9) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_9)
                      
                      renglon_10 <- c("Inglés IV",
                                      mat_aux[3,num_col_Profesor],
                                      "14 a 16 Lu Mi",
                                      14,##14-16hrs Lunes Miércoles
                                      40,20,"204 (Yelizcalli)","7100",
                                      "Ciencias de la Computación","2013",
                                      mat_aux[3,num_col_Semestre:num_col_Turno],
                                      "Cuarto Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20161/1556/1426",
                                      0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_10[num_col_Cambios] <- "1/2/"
                      names(renglon_10) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_10)
                    },##Fin 20161
                    '20162' = {
                      #' Las materias "/Inglés I/Inglés I/Inglés II/Inglés II" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      #' Manuel Enrique Camargo Coronel	
                      renglon_1 <- c("Inglés I",
                                     mat_aux[1,num_col_Profesor],
                                     "13 a 15 Lu",
                                     13,##13-15hrs Lunes
                                     55,24,"P201","9343","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Primer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20162/2017/1135",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_1[num_col_Cambios] <- "1/2/"
                      names(renglon_1) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_1)
                      
                      
                      renglon_2 <- c("Inglés I",
                                     mat_aux[1,num_col_Profesor],
                                     "13 a 15 Mi",
                                     13,##13-15hrs Miércoles
                                     55,20,"P201","9344","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Primer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20162/2017/1135",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_2[num_col_Cambios] <- "1/2/"
                      names(renglon_2) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_2)
                      
                      renglon_3 <- c("Inglés II",
                                     mat_aux[1,num_col_Profesor],
                                     "13 a 15 Ma",
                                     13,##13-15hrs Martes
                                     50,48,"P201","9345","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Segundo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20162/2017/1235",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_3[num_col_Cambios] <- "1/2/"
                      names(renglon_3) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_3)
                      
                      renglon_4 <- c("Inglés II",
                                     mat_aux[1,num_col_Profesor],
                                     "13 a 15 Ju",
                                     13,##13-15hrs Jueves
                                     50,36,"P201","9347","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Segundo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20162/2017/1235",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_4[num_col_Cambios] <- "1/2/"
                      names(renglon_4) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_4)
                      
                      
                      #' Las materias "/Inglés II/Inglés IV" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      #' Lidia Fabiola Quevedo Rojas
                      renglon_L1 <- c("Inglés II",
                                      mat_aux[2,num_col_Profesor],
                                      "14 a 16 Ma",
                                      14,##14-16hrs Martes
                                      50,47,"O219","9346","Actuaría","2015",
                                      mat_aux[2,num_col_Semestre:num_col_Turno],
                                      "Segundo Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20162/2017/1235",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_L1[num_col_Cambios] <- "1/2/"
                      names(renglon_L1) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_L1)
                      
                      renglon_L2 <- c("Inglés II",
                                      mat_aux[2,num_col_Profesor],
                                      "13 a 15 Vi",
                                      13,##13-15hrs Viernes
                                      50,31,"P201","9348","Actuaría","2015",
                                      mat_aux[2,num_col_Semestre:num_col_Turno],
                                      "Segundo Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20162/2017/1235",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_L2[num_col_Cambios] <- "1/2/"
                      names(renglon_L2) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_L2)
                      
                      
                      renglon_L3 <- c("Inglés IV",
                                      mat_aux[2,num_col_Profesor],
                                      "9 a 11 Sa",
                                      9,##9-11hrs Sábado
                                      45,45,"O125","9354","Actuaría","2015",
                                      mat_aux[2,num_col_Semestre:num_col_Turno],
                                      "Cuarto Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20162/2017/1436",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_L3[num_col_Cambios] <- "1/2/"
                      names(renglon_L3) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_L3)
                      
                      renglon_L4 <- c("Inglés IV",
                                      mat_aux[2,num_col_Profesor],
                                      "11 a 13 Sa",
                                      11,##11-13hrs Sábado
                                      45,40,"O125","9355","Actuaría","2015",
                                      mat_aux[2,num_col_Semestre:num_col_Turno],
                                      "Cuarto Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20162/2017/1436",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_L4[num_col_Cambios] <- "1/2/"
                      names(renglon_L4) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_L4)
                      
                      renglon_L5 <- c("Inglés IV",
                                      mat_aux[2,num_col_Profesor],
                                      "13 a 15 Ju",
                                      13,##13-15hrs Jueves
                                      45,45,"203 (Yelizcalli)","9356","Actuaría","2015",
                                      mat_aux[2,num_col_Semestre:num_col_Turno],
                                      "Cuarto Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20162/2017/1436",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_L5[num_col_Cambios] <- "1/2/"
                      names(renglon_L5) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_L5)
                      
                      renglon_L6 <- c("Inglés V",
                                      mat_aux[3,num_col_Profesor],
                                      "14 a 16 Lu Mi",
                                      14,##14-16hrs Lunes Miércoles
                                      35,27,"O219","7096",
                                      "Ciencias de la Computación","2013",
                                      mat_aux[3,num_col_Semestre:num_col_Turno],
                                      "Quinto Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20162/1556/1535",
                                      0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_L6[num_col_Cambios] <- "1/2/"
                      names(renglon_L6) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_L6)
                      
                      
                      #' Las materias "/Inglés III/Inglés IV" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      #' Martha del Carmen Riveroll Alvarez
                      renglon_M1 <- c("Inglés III",
                                      mat_aux[4,num_col_Profesor],
                                      "14 a 16 Ma Ju",
                                      14,##14-16hrs Martes Jueves
                                      40,18,"P210","7094",
                                      "Ciencias de la Computación","2013",
                                      mat_aux[4,num_col_Semestre:num_col_Turno],
                                      "Tercer Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20162/1556/1322",
                                      0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_M1[num_col_Cambios] <- "1/2/"
                      names(renglon_M1) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_M1)
                      
                      renglon_M2 <- c("Inglés IV",
                                      mat_aux[4,num_col_Profesor],
                                      "14 a 16 Lu Mi",
                                      14,##14-16hrs Lunes Martes
                                      45,41,"P210","7095",
                                      "Ciencias de la Computación","2013",
                                      mat_aux[4,num_col_Semestre:num_col_Turno],
                                      "Cuarto Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20162/1556/1426",
                                      0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_M2[num_col_Cambios] <- "1/2/"
                      names(renglon_M2) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_M2)
                    },##Fin 20162
                    '20171' = {
                      #' Las materias "Inglés V/Inglés V/Inglés V" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      renglon_1 <- c("Inglés V",
                                     mat_aux[1,num_col_Profesor],
                                     "14 a 16 Lu / 13 a 15 Sa Sesión AVE",
                                     14,##14-16hrs Lunes
                                     0,10,"O216","9385","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Sexto Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20171/2017/1640",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_1[num_col_Cambios] <- "1/2/"
                      names(renglon_1) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_1)
                      
                      
                      renglon_2 <- c("Inglés V",
                                     mat_aux[1,num_col_Profesor],
                                     "14 a 16 Ma / 16 a 18 Mi Sesión AVE",
                                     14,##14-16hrs Martes
                                     0,32,"003 (Yelizcalli)","9386","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Sexto Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20171/2017/1640",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_2[num_col_Cambios] <- "1/2/"
                      names(renglon_2) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_2)
                      
                      renglon_3 <- c("Inglés V",
                                     mat_aux[1,num_col_Profesor],
                                     "14 a 16 Mi / 16 a 18 Ju Sesión AVE",
                                     14,##14-16hrs Miércoles
                                     0,40,"003 (Yelizcalli)","9387","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Sexto Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20171/2017/1640",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_3[num_col_Cambios] <- "1/2/"
                      names(renglon_3) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_3)
                    },##Fin 20171
                    '20172' = {
                      #' Las materias "/Inglés II/Inglés IV/Inglés VI" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      renglon_1 <- c("Inglés II",
                                     mat_aux[1,num_col_Profesor],
                                     "13 a 15 Ma / 15 a 17 Ma Sesión virtual",
                                     13,##13-15hrs Martes
                                     50,50,"102 (Yelizcalli)","9341","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Segundo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20172/2017/1235",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_1[num_col_Cambios] <- "1/2/"
                      names(renglon_1) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_1)
                      
                      renglon_2 <- c("Inglés IV",
                                     mat_aux[1,num_col_Profesor],
                                     "13 a 15 Lu / 15 a 17 Lu Sesión virtual",
                                     13,##13-15hrs Lunes
                                     45,45,"102 (Yelizcalli)","9349","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Cuarto Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20172/2017/1436",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_2[num_col_Cambios] <- "1/2/"
                      names(renglon_2) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_2)
                      
                      renglon_3 <- c("Inglés VI",
                                     mat_aux[1,num_col_Profesor],
                                     "13 a 15 Mi / 15 a 17 Mi Sesión virtual",
                                     13,##13-15hrs Miércoles
                                     48,48,"002 (Yelizcalli)","9355","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Séptimo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20172/2017/1740",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_3[num_col_Cambios] <- "1/2/"
                      names(renglon_3) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_3)
                      
                      
                      #' Las materias "/Inglés III/Inglés V" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      renglon_4 <- c("Inglés III",
                                     mat_aux[2,num_col_Profesor],
                                     "14 a 16 Mi / 16 a 18 Mi Sesión virtual",
                                     14,##14-16hrs Miércoles
                                     32,31,"102 (Yelizcalli)","9348","Actuaría","2015",
                                     mat_aux[2,num_col_Semestre:num_col_Turno],
                                     "Tercer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20172/2017/1336",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_4[num_col_Cambios] <- "1/2/"
                      names(renglon_4) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_4)
                      
                      renglon_5 <- c("Inglés V",
                                     mat_aux[2,num_col_Profesor],
                                     "14 a 16 Lu / 16 a 18 Lu Sesión virtual",
                                     14,##14-16hrs Lunes
                                     40,38,"003 (Yelizcalli)","9354","Actuaría","2015",
                                     mat_aux[2,num_col_Semestre:num_col_Turno],
                                     "Sexto Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20172/2017/1640",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_5[num_col_Cambios] <- "1/2/"
                      names(renglon_5) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_5)
                    },##Fin 20172
                    '20181' = {
                      #' Las materias "/Inglés I/Inglés I/Inglés III/Inglés III/Inglés VI"
                      #' son diferentes, las clases presenciales se imparten en días
                      #' distintos.
                      #' Arturo García Miranda	
                      renglon_A1 <- c("Inglés I",
                                      mat_aux[1,num_col_Profesor],
                                      "13 a 15 Lu / 11 a 13 Lu Sesión virtual",
                                      13,##13-15hrs Lunes
                                      50,57,"001 (Yelizcalli)","9337","Actuaría","2015",
                                      mat_aux[1,num_col_Semestre:num_col_Turno],
                                      "Primer Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20181/2017/1135",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_A1[num_col_Cambios] <- "1/2/"
                      names(renglon_A1) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_A1)
                      
                      renglon_A2 <- c("Inglés I",
                                      mat_aux[1,num_col_Profesor],
                                      "13 a 15 Ju / 11 a 13 Ju Sesión virtual",
                                      13,##13-15hrs Jueves
                                      50,57,"008",##Tlahuizcalpan
                                      "9340","Actuaría","2015",
                                      mat_aux[1,num_col_Semestre:num_col_Turno],
                                      "Primer Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20181/2017/1135",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_A2[num_col_Cambios] <- "1/2/"
                      names(renglon_A2) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_A2)
                      
                      renglon_A3 <- c("Inglés III",
                                      mat_aux[1,num_col_Profesor],
                                      "13 a 15 Mi / 15 a 17 Mi Sesión virtual",
                                      13,##13-15hrs Miércoles
                                      45,43,"P201","9346","Actuaría","2015",
                                      mat_aux[1,num_col_Semestre:num_col_Turno],
                                      "Tercer Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20181/2017/1336",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_A3[num_col_Cambios] <- "1/2/"
                      names(renglon_A3) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_A3)
                      
                      renglon_A4 <- c("Inglés III",
                                      mat_aux[1,num_col_Profesor],
                                      "13 a 15 Vi / 11 a 13 Vi Sesión virtual",
                                      13,##13-15hrs Viernes
                                      45,31,"001 (Yelizcalli)","9348","Actuaría","2015",
                                      mat_aux[1,num_col_Semestre:num_col_Turno],
                                      "Tercer Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20181/2017/1336",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_A4[num_col_Cambios] <- "1/2/"
                      names(renglon_A4) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_A4)
                      
                      renglon_A5 <- c("Inglés VI",
                                      mat_aux[1,num_col_Profesor],
                                      "13 a 15 Ma / 15 a 17 Ma Sesión virtual",
                                      13,##13-15hrs Martes
                                      50,55,"001 (Yelizcalli)","9356","Actuaría","2015",
                                      mat_aux[1,num_col_Semestre:num_col_Turno],
                                      "Séptimo Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20181/2017/1740",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_A5[num_col_Cambios] <- "1/2/"
                      names(renglon_A5) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_A5)
                      
                      
                      #' Las materias "Inglés I/Inglés I/Inglés I" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      #' Lidia Fabiola Quevedo Rojas
                      renglon_L1 <- c("Inglés I",
                                      mat_aux[2,num_col_Profesor],
                                      "14 a 16 Ma / 16 a 18 Ma Sesión virtual",
                                      14,##14-16hrs Martes
                                      50,57,"O216","9338","Actuaría","2015",
                                      mat_aux[2,num_col_Semestre:num_col_Turno],
                                      "Primer Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20181/2017/1135",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_L1[num_col_Cambios] <- "1/2/"
                      names(renglon_L1) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_L1)
                      
                      renglon_L2 <- c("Inglés I",
                                      mat_aux[2,num_col_Profesor],
                                      "14 a 16 Mi / 15 a 17 Lu Sesión virtual",
                                      14,##14-16hrs Miércoles
                                      50,58,"O216","9343","Actuaría","2015",
                                      mat_aux[2,num_col_Semestre:num_col_Turno],
                                      "Primer Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20181/2017/1135",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_L2[num_col_Cambios] <- "1/2/"
                      names(renglon_L2) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_L2)
                      
                      
                      #' Las materias "/Inglés I/Inglés II/Inglés V" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      #' Martha del Carmen Riveroll Alvarez	
                      renglon_M1 <- c("Inglés I",
                                      mat_aux[3,num_col_Profesor],
                                      "13 a 15 Vi / 11 a 13 Vi Sesión virtual",
                                      13,##13-15hrs Viernes
                                      53,41,"008",##Tlahuizcalpan
                                      "9341","Actuaría","2015",
                                      mat_aux[3,num_col_Semestre:num_col_Turno],
                                      "Primer Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20181/2017/1135",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_M1[num_col_Cambios] <- "1/2/"
                      names(renglon_M1) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_M1)
                      
                      renglon_M2 <- c("Inglés I",
                                      mat_aux[3,num_col_Profesor],
                                      "13 a 15 Lu / 15 a 17 Lu Sesión virtual",
                                      13,##13-15hrs Lunes
                                      50,53,"008",##Tlahuizcalpan
                                      "9342","Actuaría","2015",
                                      mat_aux[3,num_col_Semestre:num_col_Turno],
                                      "Primer Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20181/2017/1135",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_M2[num_col_Cambios] <- "1/2/"
                      names(renglon_M2) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_M2)
                      
                      renglon_M3 <- c("Inglés II",
                                      mat_aux[3,num_col_Profesor],
                                      "14 a 16 Ma / 16 a 18 Ma Sesión virtual",
                                      14,##14-16hrs Martes
                                      40,25,"004 (Yelizcalli)","9345","Actuaría","2015",
                                      mat_aux[3,num_col_Semestre:num_col_Turno],
                                      "Segundo Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20181/2017/1235",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_M3[num_col_Cambios] <- "1/2/"
                      names(renglon_M3) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_M3)
                      
                      renglon_M4 <- c("Inglés V",
                                      mat_aux[4,num_col_Profesor],
                                      "14 a 16 Ju / 16 a 18 Ju Sesión virtual",
                                      14,##14-16hrs Jueves
                                      40,36,"004 (Yelizcalli)","9352","Actuaría","2015",
                                      mat_aux[4,num_col_Semestre:num_col_Turno],
                                      "Sexto Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20181/2017/1640",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_M4[num_col_Cambios] <- "1/2/"
                      names(renglon_M4) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_M4)
                    },##Fin 20181
                    '20182' = {
                      #' Las materias "/Inglés II/Inglés V/Inglés VI" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      renglon_1 <- c("Inglés II",
                                     mat_aux[1,num_col_Profesor],
                                     "13 a 15 Lu / 15 a 17	Lu Sesión virtual",
                                     13,##13-15hrs Lunes
                                     45,35,"O219","9353","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Segundo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20182/2017/1235",
                                     mat_aux[1,num_col_Act2000:num_col_NomMat_Act2006],0,
                                     mat_aux[1,num_col_NomMat_CdC1994:num_col_URL_Act2006],0,
                                     mat_aux[1,num_col_URL_CdC1994:num_col_URL_MAp2017])
                      renglon_1[num_col_Cambios] <- "1/2/"
                      names(renglon_1) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_1)
                      
                      renglon_2 <- c("Inglés V",
                                     mat_aux[1,num_col_Profesor],
                                     "13 a 15 Vi / 15 a 17	Vi Sesión virtual",
                                     13,##13-15hrs Viernes
                                     55,48,"O219","9363","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Sexto Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20182/2017/1640",
                                     mat_aux[1,num_col_Act2000:num_col_NomMat_Act2006],0,
                                     mat_aux[1,num_col_NomMat_CdC1994:num_col_URL_Act2006],0,
                                     mat_aux[1,num_col_URL_CdC1994:num_col_URL_MAp2017])
                      renglon_2[num_col_Cambios] <- "1/2/"
                      names(renglon_2) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_2)
                      
                      renglon_3 <- c("Inglés VI",
                                     mat_aux[1,num_col_Profesor],
                                     "13 a 15 Mi / 15 a 17	Mi Sesión virtual",
                                     13,##13-15hrs Lunes
                                     40,34,"004 (Yelizcalli)","9366","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Séptimo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20182/2017/1740",
                                     mat_aux[1,num_col_Act2000:num_col_NomMat_Act2006],0,
                                     mat_aux[1,num_col_NomMat_CdC1994:num_col_URL_Act2006],0,
                                     mat_aux[1,num_col_URL_CdC1994:num_col_URL_MAp2017])
                      renglon_3[num_col_Cambios] <- "1/2/"
                      names(renglon_3) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_3)
                      
                      
                      #' Las materias "/Inglés II/Inglés VI/Inglés II" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      renglon_4 <- c("Inglés II",
                                     mat_aux[2,num_col_Profesor],
                                     "14 a 16 Ma / 16 a 18	Ma Sesión virtual",
                                     14,##14-16hrs Martes
                                     50,48,"P201","9354","Actuaría","2015",
                                     mat_aux[2,num_col_Semestre:num_col_Turno],
                                     "Segundo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20182/2017/1235",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_4[num_col_Cambios] <- "1/2/"
                      names(renglon_4) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_4)
                      
                      renglon_5 <- c("Inglés VI",
                                     mat_aux[2,num_col_Profesor],
                                     "14 a 16 Ju / 16 a 18 Ju Sesión virtual",
                                     14,##14-16hrs Jueves
                                     45,40,"102 (Yelizcalli)","9367","Actuaría","2015",
                                     mat_aux[2,num_col_Semestre:num_col_Turno],
                                     "Séptimo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20182/2017/1740",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_5[num_col_Cambios] <- "1/2/"
                      names(renglon_5) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_5)
                      
                      renglon_6 <- c("Inglés VI",
                                     mat_aux[2,num_col_Profesor],
                                     "13 a 15 Lu / 15 a 17 Lu Sesión virtual",
                                     13,##13-15hrs Jueves
                                     45,27,"P107","9390","Actuaría","2015",
                                     mat_aux[2,num_col_Semestre:num_col_Turno],
                                     "Séptimo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20182/2017/1740",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_6[num_col_Cambios] <- "1/2/"
                      names(renglon_6) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_6)
                      
                      renglon_7 <- c("Inglés II",
                                     mat_aux[2,num_col_Profesor],
                                     "14 a 16 Mi / 16 a 18	Mi Sesión virtual",
                                     14,##14-16hrs Miércoles
                                     45,44,"P201","7095",
                                     "Ciencias de la Computación","2013",
                                     mat_aux[2,num_col_Semestre:num_col_Turno],
                                     "Segundo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20182/1556/1223",
                                     0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_7[num_col_Cambios] <- "1/2/"
                      names(renglon_7) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_7)
                      
                      
                      #' Las materias "/Inglés II/Inglés II/Inglés IV" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      renglon_8 <- c("Inglés II",
                                     mat_aux[3,num_col_Profesor],
                                     "11 a 13 Mi Sesión virtual / 13 a 15 Mi",
                                     13,##13-15hrs Miércoles
                                     45,43,"O219","9355","Actuaría","2015",
                                     mat_aux[3,num_col_Semestre:num_col_Turno],
                                     "Segundo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20182/2017/1235",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_8[num_col_Cambios] <- "1/2/"
                      names(renglon_8) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_8)
                      
                      renglon_9 <- c("Inglés II",
                                     mat_aux[3,num_col_Profesor],
                                     "11 a 13 Vi Sesión virtual / 13 a 15 Vi",
                                     13,##13-15hrs Viernes
                                     45,26,"O213","9357","Actuaría","2015",
                                     mat_aux[3,num_col_Semestre:num_col_Turno],
                                     "Segundo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20182/2017/1235",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_9[num_col_Cambios] <- "1/2/"
                      names(renglon_9) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_9)
                      
                      renglon_10 <- c("Inglés IV",
                                      mat_aux[3,num_col_Profesor],
                                      "11 a 13 Lu Sesión virtual / 13 a 15 Lu",
                                      13,##13-15hrs Lunes
                                      45,43,"O134","9360","Actuaría","2015",
                                      mat_aux[3,num_col_Semestre:num_col_Turno],
                                      "Cuarto Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20182/2017/1436",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_10[num_col_Cambios] <- "1/2/"
                      names(renglon_10) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_10)
                    },##Fin 20182
                    '20192' = {
                      #' Las materias "Inglés I/Inglés VI" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      renglon_1 <- c("Inglés I",
                                     mat_aux[1,num_col_Profesor],
                                     "12 a 14 Mi / 18 a 20 Ma Sesión virtual",
                                     12,##12-14hrs Miércoles
                                     15,11,"P204","7088",
                                     "Ciencias de la Computación","2013",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Primer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20192/1556/1124",
                                     0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_1[num_col_Cambios] <- "1/2/"
                      names(renglon_1) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_1)
                      
                      renglon_2 <- c("Inglés VI",
                                     mat_aux[1,num_col_Profesor],
                                     "12 a 14 Ju / 18 a 20 Vi Sesión virtual",
                                     12,##12-14hrs Jueves
                                     20,15,"P109","7099",
                                     "Ciencias de la Computación","2013",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Octavo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20192/1556/1829",
                                     0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_2[num_col_Cambios] <- "1/2/"
                      names(renglon_2) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_2)
                    },##Fin 20192
                    '20201' = {
                      #' Las materias "Inglés I/Inglés I" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      renglon_1 <- c("Inglés I",
                                     mat_aux[1,num_col_Profesor],
                                     "14 a 16	Vi",
                                     14,##14-16hrs Viernes
                                     45,32,"O215","9313","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Primer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20201/2017/1135",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_1[num_col_Cambios] <- "1/2/"
                      names(renglon_1) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_1)
                      
                      renglon_2 <- c("Inglés I",
                                     mat_aux[1,num_col_Profesor],
                                     "14 a 16	Lu",
                                     14,##14-16hrs Lunes
                                     45,29,"O215","9314","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Primer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20201/2017/1135",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_2[num_col_Cambios] <- "1/2/"
                      names(renglon_2) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_2)
                    },##Fin 20201
             )##Fin de switch(sem_info)
           }else{#En caso de que la matriz no requiera modificaciones
             m_grande <- m_grande_SIN_ING
           }
         },##Fin "Inglés"
         'Francés' = {},##En caso de que algún día haya clases de francés
         'Alemán' = {},##En caso de que algún día haya clases de alemán
         'Mandarín' = {},##En caso de que algún día haya clases de mandarín
  )##Fin de switch(idioma)
  
  save(m_grande, file = paste0("m_grande por semestre/m_grande_",sem_info,".RData"))
  return(m_grande)
}


# actualiza_col_cambios ---------------------------------------------------
#' Title actualiza_col_cambios: Función que se encarga de actualizar la
#' columna "Cambios" de "m_grande".
#'
#' @param m_grande: Matriz de 36 columnas (Materia,Profesor,Horario,
#' horario_num,Lugares,Alumnos,Salon,Grupo,Carrera,Plan,Semestre,Cambios,
#' Turno,Semestre_de_materia,url,Act2000,Act2006,Act2015,CdC1994,CdC2013,
#' Mat1983,MAp2017,NomMat_Act2000,NomMat_Act2006,NomMat_Act2015,
#' NomMat_CdC1994,NomMat_CdC2013,NomMat_Mat1983,NomMat_MAp2017,URL_Act2000,
#' URL_Act2006,URL_Act2015,URL_CdC1994,URL_CdC2013,URL_Mat1983,URL_MAp2017),
#' con la información de "sem_info". Las columnas 16-22 son columnas
#' binarias las cuales indican con un 1 si el grupo del i-ésimo renglón
#' pertenece a la carrera y plan correspondiente al nombre de cada columna,
#' hay un 0 e.o.c. Las columnas 23-29 indican el nombre de las materias
#' dependiendo del plan y carrera al que pertenecen. Las columnas 30-36 
#' indican las URLs dependiendo del plan y carrera al que pertenecen. 
#' @example m_grande[155,] <- c("Álgebra Moderna II",...,0)
#'
#' @return m_grande
#'
actualiza_col_cambios <- function(m_grande){
  num_col_Salon <- arroja_ind_col_MG("Salon")##7
  num_col_Cambios <- arroja_ind_col_MG("Cambios")##12
  num_col_NomMat_Act2000 <- arroja_ind_col_MG("NomMat_Act2000")##23
  num_col_NomMat_MAp2017 <- arroja_ind_col_MG("NomMat_MAp2017")##29
  # col_cambios_aux <- m_grande[,num_col_Cambios]
  
  for(d in 1:dim(m_grande)[1]){
    ##Cambio (3): Se eliminaron los grupos repetidos
    renlon_aux <- m_grande[d,num_col_NomMat_Act2000:num_col_NomMat_MAp2017]
    if(!all(renlon_aux == rep(0,7))){
      m_grande[d,num_col_Cambios] <- paste(m_grande[d,num_col_Cambios],3,
                                           sep = "/")}
    
    ##Cambio (4): Páginas que no tienen información del salón
    if(m_grande[d,num_col_Salon]=="" || is.na(m_grande[d,num_col_Salon])){
      m_grande[d,num_col_Cambios] <- paste(m_grande[d,num_col_Cambios],4,
                                           sep = "/")}
  }#Fin for(d)
  return(m_grande)
}


# gen_m_grande_SIN_Num_Materia --------------------------------------------
#' Title gen_m_grande_SIN_Num_Materia: Función que recibe como parámetro el
#' semestre del que se desea obtener la información y genera un archivo de
#' tipo ".Rdata" con la matriz "m_grande" de dicho semestre. Regresa el
#' nombre de la ubicación en la que se encuentra guardado el archivo.
#' La matriz "m_grande" que se guarda está limpia y actualizada.
#'
#' @param sem_info: Semestre del que se desea obtener información.
#' @param vec_excepciones: Vector que contiene las posibles excepciones que
#' se deben de tomar en cuenta al crear "m_grande".
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' 
#' @example sem_info <- 20182
#' @example vec_excepciones <- c("Inglés")
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' 
#' @return m_grande_SNM: Matriz de 36 columnas (Materia,Profesor,Horario,
#' horario_num,Lugares,Alumnos,Salon,Grupo,Carrera,Plan,Semestre,Cambios,
#' Turno,Semestre_de_materia,url,Act2000,Act2006,Act2015,CdC1994,CdC2013,
#' Mat1983,MAp2017), con la información de sem_info. Las columnas 16-22
#' son columnas binarias las cuales indican con un 1 si el grupo del
#' i-ésimo renglón pertenece a la carrera y plan correspondiente al
#' nombre de cada columna, hay un 0 e.o.c. Las columnas 23-29 indican
#' el nombre de las materias dependiendo del plan y carrera al que
#' pertenecen. Las columnas 30-36 indican las URLs dependiendo del plan
#' y carrera al que pertenecen.
#'
gen_m_grande_SIN_Num_Materia <- function(sem_info,vec_excepciones,param){
  ##Se definen las variables que se van a utilizar:
  nom_arch_SIN_MOD <- paste0("m_grande por semestre SIN MODIFICAR/m_grande_SIN_MOD_",
                             sem_info,".RData")
  nom_arch_SIN_ING <- paste0("m_grande por semestre SIN INGLES/m_grande_SIN_ING_",
                             sem_info,".RData")
  direccion_info <- paste0("m_grande por semestre SIN NUM MATERIA/m_grande_SNM_",
                           sem_info,".RData")
  
  #' Se hace la revisión por etapas para que se actualice la matriz
  #' hasta el punto necesario.
  if(file.exists(direccion_info)){
    load(direccion_info)
  }else if(!file.exists(direccion_info) && file.exists(nom_arch_SIN_ING)){
    ##En caso de que el archivo de la matriz "m_grande" no exista:
    m_grande <- revisa_gpos_idiomas_1_sem(sem_info,vec_excepciones)
  }else if(!file.exists(nom_arch_SIN_ING) && file.exists(nom_arch_SIN_MOD)){
    ##En caso de que el archivo de la matriz "m_grande_SIN_ING" no exista:
    m_grande <- actualiza_m_grande_1_sem(sem_info,param)
    m_grande <- revisa_gpos_idiomas_1_sem(sem_info,vec_excepciones)
  }else if(!file.exists(nom_arch_SIN_MOD)){
    ##En caso de que el archivo de la matriz "m_grande_SIN_MOD" no exista:
    m_grande <- gen_m_grande_SIN_MOD(sem_info,list_url,param)
    m_grande <- actualiza_m_grande_1_sem(sem_info,param)
    m_grande <- revisa_gpos_idiomas_1_sem(sem_info,vec_excepciones)
  }
  
  #Se actualiza la columna "Cambios"
  m_grande_SNM <- actualiza_col_cambios(m_grande)
  
  save(m_grande_SNM, file = direccion_info)
  return(m_grande_SNM)
}


# gen_vec_nom_materias_total ----------------------------------------------
#' Title gen_vec_nom_materias_total: Función que carga la matriz
#' "m_grande_total" de los semestres 2008-1 a 2020-1 de la cual se va
#' obtiene la lista de nombres de las materias sin repetición, conservando
#' los nombres más recientes de las materias.
#'
#' @return vec_nom_materias_total: Vector que contiene el nombre de las
#' materias sin repetición, conservando los nombres más recientes.
#'
gen_vec_nom_materias_total <- function(){
  #Se definen las variables que se van a utilizar:
  vec_nom_materias_total <- 0
  num_col_Materia <- arroja_ind_col_MG("Materia")##1
  num_col_Act2000 <- arroja_ind_col_MG("Act2000")##16
  num_col_Act2006 <- arroja_ind_col_MG("Act2006")##17
  num_col_Act2015 <- arroja_ind_col_MG("Act2015")##18
  num_col_CdC1994 <- arroja_ind_col_MG("CdC1994")##19
  num_col_CdC2013 <- arroja_ind_col_MG("CdC2013")##20
  num_col_Mat1983 <- arroja_ind_col_MG("Mat1983")##21
  num_col_MAp2017 <- arroja_ind_col_MG("MAp2017")##22
  num_col_NomMat_Act2000 <- arroja_ind_col_MG("NomMat_Act2000")##23
  num_col_NomMat_Act2006 <- arroja_ind_col_MG("NomMat_Act2006")##24
  num_col_NomMat_Act2015 <- arroja_ind_col_MG("NomMat_Act2015")##25
  num_col_NomMat_CdC1994 <- arroja_ind_col_MG("NomMat_CdC1994")##26
  num_col_NomMat_CdC2013 <- arroja_ind_col_MG("NomMat_CdC2013")##27
  num_col_NomMat_Mat1983 <- arroja_ind_col_MG("NomMat_Mat1983")##28
  num_col_NomMat_MAp2017 <- arroja_ind_col_MG("NomMat_MAp2017")##29
  
  #' Se carga la matriz m_grande_total de 2008-1 a 2020-1 de la cual
  #' se va a obtener la lista de nombres que se desea
  load("Matrices m_grande_total/m_grande_total_20081_20201_VECTOR.RData")
  # View(m_grande_total)
  
  for(r in 1:dim(m_grande_total)[1]){#Se recorren los renglones
    renglon <- m_grande_total[r,num_col_Act2000:num_col_MAp2017]
    # cat("\n renglon = ",renglon)
    # print(renglon)
    if(sum(as.numeric(renglon)) == 1){##Se toman las materias que tengan un sólo nombre
      vec_nom_materias_total <- c(vec_nom_materias_total,m_grande_total[r,num_col_Materia])
    }else if(sum(as.numeric(renglon)) > 1){
      ##Se toman las materias que tengan más de un nombre, conservando el más reciente
      # Se reordenan las columnas para que queden los datos ordenados de acuerdo
      #al plan de estudios, del más antiguo al más reciente.
      renglon_aux <- c(m_grande_total[r,num_col_Mat1983],
                       m_grande_total[r,num_col_CdC1994],
                       m_grande_total[r,num_col_Act2000],
                       m_grande_total[r,num_col_Act2006],
                       m_grande_total[r,num_col_CdC2013],
                       m_grande_total[r,num_col_Act2015],
                       m_grande_total[r,num_col_MAp2017])
      reng_aux_nom <- c(m_grande_total[r,num_col_NomMat_Mat1983],
                        m_grande_total[r,num_col_NomMat_CdC1994],
                        m_grande_total[r,num_col_NomMat_Act2000],
                        m_grande_total[r,num_col_NomMat_Act2006],
                        m_grande_total[r,num_col_NomMat_CdC2013],
                        m_grande_total[r,num_col_NomMat_Act2015],
                        m_grande_total[r,num_col_NomMat_MAp2017])
      var_aux <- 0
      for(c in length(renglon_aux):1){
        # cat("\n c = ",c)
        if(var_aux==0 && renglon_aux[c]==1){
          vec_nom_materias_total <- c(vec_nom_materias_total,reng_aux_nom[c])
          var_aux <- 1
        }#if
      }#for c
    }#if else
  }#for r
  
  #Se toman los nombres sin repetición
  vec_nom_materias_total <- unique(vec_nom_materias_total)
  
  #Se quitan los ceros
  vec_nom_materias_total <- vec_nom_materias_total[vec_nom_materias_total!=0]
  save(vec_nom_materias_total, file = "vec_nom_materias_total.RData")
  
  return(vec_nom_materias_total)
}


# agrega_col_num_materia --------------------------------------------------
#' Title agrega_col_num_materia: Función que agrega otra columna a la
#' matriz m_grande con el número del nombre de materia de acuerdo al
#' vector "vec_nom_materias".
#'
#' @param m_grande_SNM: Matriz de 36 columnas (Materia,Profesor,Horario,
#' horario_num,Lugares,Alumnos,Salon,Grupo,Carrera,Plan,Semestre,Cambios,
#' Turno,Semestre_de_materia,url,Act2000,Act2006,Act2015,CdC1994,CdC2013,
#' Mat1983,MAp2017,NomMat_Act2000,NomMat_Act2006,NomMat_Act2015,
#' NomMat_CdC1994,NomMat_CdC2013,NomMat_Mat1983,NomMat_MAp2017,URL_Act2000,
#' URL_Act2006,URL_Act2015,URL_CdC1994,URL_CdC2013,URL_Mat1983,URL_MAp2017),
#' con la información de "sem_info". Las columnas 16-22 son columnas
#' binarias las cuales indican con un 1 si el grupo del i-ésimo renglón
#' pertenece a la carrera y plan correspondiente al nombre de cada columna,
#' hay un 0 e.o.c. Las columnas 23-29 indican el nombre de las materias
#' dependiendo del plan y carrera al que pertenecen. Las columnas 30-36 
#' indican las URLs dependiendo del plan y carrera al que pertenecen.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' 
#' @example m_grande_SNM <- c("Cálculo Diferencial e Integral I",...,0)
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
#' @return m_grande: Matriz de 37 columnas (las de "m_grande_SNM" más
#' la columna ), con la información de "sem_info".
#'
agrega_col_num_materia <- function(m_grande_SNM,param){
  ##Se definen las variables que se van a utilizar:
  num_col_Materia <- arroja_ind_col_MG("Materia")##1
  vec_nom_materias_total <- param$vec_nom_materias_total
  m_grande <- matrix(0,ncol = length(param$nom_cols_MG))
  colnames(m_grande) <- param$nom_cols_MG
  
  for(d in 1:length(vec_nom_materias_total)){#Recorre las materias
    if(dim(m_grande_SNM)[1] > 0){
      materia <- vec_nom_materias_total[d]
      ind_materia <- checa_ind_materia(materia,m_grande_SNM)
      if(length(ind_materia) > 0){
        Num_materia <- rep(d,length(ind_materia))
        mat_aux <- cbind(m_grande_SNM[ind_materia,],Num_materia)
        colnames(mat_aux) <- param$nom_cols_MG
        m_grande <- rbind(m_grande,mat_aux)
        
        m_grande_SNM <- m_grande_SNM[-ind_materia,]}}}
  
  ## Se quita el renglón de ceros inicial
  m_grande <- m_grande[m_grande[,num_col_Materia]!=0,]
  # View(m_grande)
  return(m_grande)
}


# gen_m_grande ------------------------------------------------------------
#' Title gen_m_grande: Función que recibe como parámetro el semestre del
#' que se desea obtener la información y genera un archivo de tipo ".Rdata"
#' con la matriz "m_grande" de dicho semestre. Regresa el nombre de la
#' ubicación en la que se encuentra guardado el archivo.
#' La matriz "m_grande" que se guarda está limpia y actualizada.
#' Se agrega la columna con el número del nombre de materia de acuerdo al
#' vector "vec_nom_materias".
#'
#' @param sem_info: Semestre del que se desea obtener información.
#' @param vec_excepciones: Vector que contiene las posibles excepciones que
#' se deben de tomar en cuenta al crear "m_grande".
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' 
#' @example sem_info <- 20182
#' @example vec_excepciones <- c("Inglés")
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' 
#' @return direccion_info: Nombre de la ubicación en la que se
#' encuentra guardada la matriz "m_grande".
#'
gen_m_grande <- function(sem_info,vec_excepciones,param){
  ##Se definen las variables que se van a utilizar:
  nom_arch_SIN_Num_Mat <- paste0("m_grande por semestre SIN NUM MATERIA/m_grande_SNM_",
                                 sem_info,".RData")
  direccion_info <- paste0("m_grande por semestre/m_grande_",sem_info,".RData")
  
  #' Se hace la revisión por etapas para que se actualice la matriz
  #' hasta el punto necesario.
  if(file.exists(direccion_info)){
    load(direccion_info)
  }else if(!file.exists(direccion_info) && !file.exists(nom_arch_SIN_Num_Mat)){
    # En caso de que el archivo de la matriz "m_grande" no exista ni tampoco
    #el archivo de "m_grande_SNM":
    m_grande_SNM <- gen_m_grande_SIN_Num_Materia(sem_info,vec_excepciones,param)
    m_grande <- agrega_col_num_materia(m_grande_SNM,param)
  }else if(!file.exists(direccion_info) && file.exists(nom_arch_SIN_Num_Mat)){
    ##En caso de que el archivo de la matriz "m_grande" no exista:
    load(nom_arch_SIN_Num_Mat)
    m_grande_SNM <- m_grande
    
    m_grande <- agrega_col_num_materia(m_grande_SNM,param)
  }
  
  save(m_grande, file = direccion_info)
  return(direccion_info)
}


# gen_m_grande_total ------------------------------------------------------
#' Title gen_m_grande_total: Función que genera la matriz "m_grande_total"
#' para un intervalo semestres.
#'
#' @param vec_excepciones: Vector que contiene las posibles excepciones que
#' se deben de tomar en cuenta al crear "m_grande".
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' 
#' @example vec_excepciones <- c("Inglés")
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return m_grande_total: Matriz de 37 columnas (Materia,Profesor,Horario,
#' horario_num,Lugares,Alumnos,Salon,Grupo,Carrera,Plan,Semestre,Cambios,
#' Turno,Semestre_de_materia,url,Act2000,Act2006,Act2015,CdC1994,CdC2013,
#' Mat1983,MAp2017), con la información de todos los semestres entre "sem_ini"
#' y "sem_fin." Las columnas 16-22 son columnas binarias las cuales indican
#' con un 1 si el grupo del i-ésimo renglón pertenece a la carrera y plan
#' correspondiente al nombre de cada columna, hay un 0 e.o.c. Las columnas
#' 23-29 indican el nombre de las materias dependiendo del plan y carrera
#' al que pertenecen. Las columnas 30-36 indican las URLs dependiendo del
#' plan y carrera al que pertenecen. La columna 37 tiene el número de materia
#' correspondiente al vector de materias.
#'
gen_m_grande_total <- function(vec_excepciones,param){
  ##Se definen las variables que se van a utilizar:
  semestres <- param$Semestres
  
  m_grande_total <- matrix(0,ncol = length(param$nom_cols_MG))
  colnames(m_grande_total) <- param$nom_cols_MG
  num_col_Materia <- arroja_ind_col_MG("Materia")##1
  num_col_sem <- arroja_ind_col_MG("Semestre")##11
  
  for(d in 1:length(semestres)){
    sem_info <- semestres[d]
    nom_archivo <- paste0("m_grande por semestre/m_grande_",sem_info,".RData")
    
    if(!file.exists(nom_archivo)){
      nom_archivo <- gen_m_grande(sem_info,vec_excepciones,param)
    }
    load(nom_archivo)
    m_grande_total <- rbind(m_grande_total,m_grande)
  }
  ## Se quita el renglón de ceros inicial
  m_grande_total <- m_grande_total[m_grande_total[,num_col_Materia]!=0,]
  save(m_grande_total, file = paste0("Matrices m_grande_total/m_grande_total_",
                                     param$sem_ini,"_",param$sem_fin,".RData"))
  # save(m_grande_total, file = paste0("Matrices m_grande_total/m_grande_total_PRUEBA_",
  #                                    semestres[1],"_",sem_fin,".RData"))
  param$m_grande_total = m_grande_total
  
  return(m_grande_total)
}


##########################################################################
##### ÍNDICES #####
#' Funciones que encuentran los índices de las columnas para las diferentes
#' matrices que contienen información.
##########################################################################

# arroja_ind_col_SG -------------------------------------------------------
#' Title arroja_ind_col_SG: Función que recibe el nombre de la columna que
#' se busca y devuelve el número de columna en "mat_simula_grupos" con ese
#' nombre.
#'
#' @param nombre_col: Nombre de la columna de "mat_simula_grupos" de la que
#' se busca conocer la columna en la que se encuentra en dicha matriz.
#' @example nombre_col <- "Grupos_Simulados"
#'
#' @return num_col: Número de columna en "mat_simula_grupos" con nombre
#' "nombre_col".
#' @example num_col <- 3
#'
arroja_ind_col_SG <- function(nombre_col){
  load("mat_def_grupos_simulados.RData")
  dim_mat <- dim(mat_def_grupos_simulados)
  
  for(d in 1:dim_mat[1]){
    if(nombre_col == mat_def_grupos_simulados[d,1]){
      num_col <- mat_def_grupos_simulados[d,2]
    }
  }
  num_col <- as.numeric(num_col)
  
  return(num_col)
}


# arroja_ind_col_RG -------------------------------------------------------
#' Title arroja_ind_col_RG: Función que recibe el nombre de la columna que
#' se busca y devuelve el número de columna en "mat_real_grupos" con ese
#' nombre.
#'
#' @param nombre_col: Nombre de la columna de "mat_real_grupos" de la que
#' se busca conocer la columna en la que se encuentra en dicha matriz.
#' @example nombre_col <- "Alumnos_Reales_Totales"
#'
#' @return num_col: Número de columna en "mat_real_grupos" con nombre
#' "nombre_col".
#' @example num_col <- 4
#'
arroja_ind_col_RG <- function(nombre_col){
  load("mat_def_grupos_reales.RData")
  dim_mat <- dim(mat_def_grupos_reales)
  
  for(d in 1:dim_mat[1]){
    if(nombre_col == mat_def_grupos_reales[d,1]){
      num_col <- mat_def_grupos_reales[d,2]
    }
  }
  num_col <- as.numeric(num_col)
  
  return(num_col)
}


# arroja_ind_col_MG -------------------------------------------------------
#' Title arroja_ind_col_MG: Función que recibe el nombre de la columna que
#' se busca y devuelve el número de columna en m_grande con ese nombre.
#'
#' @param nombre_col: Nombre de la columna de m_grande de la que se busca
#' conocer la columna en la que se encuentra en dicha matriz.
#' @example nombre_col <- "Plan"
#'
#' @return num_col: Número de columna en m_grande con nombre "nombre_col".
#' @example num_col <- 10
#'
arroja_ind_col_MG <- function(nombre_col){
  load("mat_def_columnas_MG.RData")
  dim_mat <- dim(mat_def_columnas_MG)
  
  for(d in 1:dim_mat[1]){
    if(nombre_col == mat_def_columnas_MG[d,1]){
      num_col <- mat_def_columnas_MG[d,2]
    }
  }
  num_col <- as.numeric(num_col)
  
  return(num_col)
}


# checa_ind_materia -------------------------------------------------------
#' Title checa_ind_materia: Función que revisa en qué índices de la matriz
#' coincide "materia" con los nombres que se encuentran en las columnas:
#'  Materia
#'  NomMat_Act2000
#'  NomMat_Act2006
#'  NomMat_Act2015
#'  NomMat_CdC1994
#'	NomMat_CdC2013
#'  NomMat_Mat1983
#'  NomMat_MAp2017
#' Arroja un vector con los índices en los que hay coincidencia para que
#' se cree la matriz con la información necesaria.
#'
#' @param materia: Nombre de algún curso impartido en la FC.
#' @param matriz: Puede ser "m_grande_total" o "m_grande" (de cada semestre)
#' 
#' @example materia <- "Probabilidad I"
#'
#' @return ind_materia: Vector con los índices en los que hay coincidencia
#' entre "materia" y alguna de las columnas correspondientes.
#'
checa_ind_materia <- function(materia,matriz){
  #Se definen las variables que se van a utlizar
  # lista_def_columnas_MG <- param$lista_def_columnas_MG
  # m_grande_total <- param$m_grande_total
  
  num_col_Materia <- arroja_ind_col_MG("Materia")
  num_col_NomMat_Act2000 <- arroja_ind_col_MG("NomMat_Act2000")
  num_col_NomMat_Act2006 <- arroja_ind_col_MG("NomMat_Act2006")
  num_col_NomMat_Act2015 <- arroja_ind_col_MG("NomMat_Act2015")
  num_col_NomMat_CdC1994 <- arroja_ind_col_MG("NomMat_CdC1994")
  num_col_NomMat_CdC2013 <- arroja_ind_col_MG("NomMat_CdC2013")
  num_col_NomMat_Mat1983 <- arroja_ind_col_MG("NomMat_Mat1983")
  num_col_NomMat_MAp2017 <- arroja_ind_col_MG("NomMat_MAp2017")
  
  Materia <- matriz[,num_col_Materia]
  NomMat_Act2000 <- matriz[,num_col_NomMat_Act2000]
  NomMat_Act2006 <- matriz[,num_col_NomMat_Act2006]
  NomMat_Act2015 <- matriz[,num_col_NomMat_Act2015]
  NomMat_CdC1994 <- matriz[,num_col_NomMat_CdC1994]
  NomMat_CdC2013 <- matriz[,num_col_NomMat_CdC2013]
  NomMat_Mat1983 <- matriz[,num_col_NomMat_Mat1983]
  NomMat_MAp2017 <- matriz[,num_col_NomMat_MAp2017]
  
  ind_materia <- 0
  
  for(k in 1:length(Materia)){
    if(materia == Materia[k] ||
       materia == NomMat_Act2000[k] ||
       materia == NomMat_Act2006[k] ||
       materia == NomMat_Act2015[k] ||
       materia == NomMat_CdC1994[k] ||
       materia == NomMat_CdC2013[k] ||
       materia == NomMat_Mat1983[k] ||
       materia == NomMat_MAp2017[k]){
      ind_materia <- c(ind_materia,k)
    }
  }
  ind_materia <- ind_materia[-1]
  
  ## No se pone "if" para evitar que el vector se quede con longitud
  ##cero porque cada materia se encuentra al menos una vez en la
  ##matriz
  
  return(ind_materia)
}


# arroja_num_materia ------------------------------------------------
#' Title arroja_num_materia: Función que recibe el nombre de la
#' materia que se busca y devuelve el número de materia en "vec_nom_materias"
#' con ese nombre.
#'
#' @param nom_materia: Nombre de la materia de "vec_nom_materias" de la
#' que se busca conocer el número de materia.
#' @example materia <- "Estadística I"
#'
#' @return num_materia: Número de materia
#' @example num_materia <- 42
#'
arroja_num_materia <- function(materia,param){
  vec_nom_materias_total <- param$vec_nom_materias_total
  
  for(d in 1:length(vec_nom_materias_total)){
    if(materia == vec_nom_materias_total[d]){
      num_materia <- d
    }
  }
  num_materia <- as.numeric(num_materia)
  
  return(num_materia)
}

##########################################################################
##### SIMULACIÓN #####
#'Funciones encargadas de extraer, estimar y simular el número de alumnos
#'totales y el tamaño de cada grupo.
##########################################################################

# gen_mat_alumnos_corregidos ----------------------------------------------
#' Title gen_mat_alumnos_corregidos: Función que regresa la matriz 
#' "mat_alumnos_corregidos" de 15 renglones (horas) y k+s-1 columnas, la
#' cual tiene el número total de alumnos por hora y por semestre. Se suma
#' el número de alumnos en los semestres repetidos por cada hora. Hay
#' ceros en los smestres y horas en donde no hay información.
#'
#' @param vec_s_sem_k_info: Vector con los "k_sem_ant + s - 1" semestres
#' de los que se quiere obtener la información para realizar la simulación
#' del vector "vec_sem_sig".
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @param param_sim: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se encargan de la simulación.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' @example param_sim <- list(vec_sem_sig = c(20191,20192,20201),k_sem_ant = 5,
#' materia = "Estadística III", num_sim = 10, m_filtrada = matrix(0),
#' sub_m_filtrada = matrix(0,ncol = length(param$nom_cols_MG)))
#' 
#' @return mat_alumnos_corregidos: Matriz de 15 renglones (horas) y
#' k+s-1 columnas, la cual tiene el número total de alumnos por hora
#' y por semestre. Se suma el número de alumnos en los semestres
#' repetidos por cada hora. Hay ceros en los smestres y horas en
#' donde no hay información.
#'
#' @examples gen_mat_alumnos_corregidos(c(20182,20191,20192,20201),param,param_sim)
gen_mat_alumnos_corregidos <- function(vec_s_sem_k_info,param,param_sim){
  ##Se definen las variables que se van a utilizar:
  num_col_horario_num <- arroja_ind_col_MG("horario_num")##4
  num_col_Alumnos <- arroja_ind_col_MG("Alumnos")##6
  num_col_Semestre <- arroja_ind_col_MG("Semestre")##11
  m_filtrada <- param_sim$m_filtrada
  horas_unicas <- sort(unique(m_filtrada[,num_col_horario_num]))##horas
  sem_con_info <- sort(unique(m_filtrada[,num_col_Semestre]))##semestres
  mat_alumnos_corregidos <- matrix(0,nrow = length(param$Horas),
                                   ncol = length(vec_s_sem_k_info))
  rownames(mat_alumnos_corregidos) <- param$nombre_hrs
  colnames(mat_alumnos_corregidos) <- vec_s_sem_k_info
  
  mat_aux <- data.frame(Semestre = 0,Hora = 0,
                        Alumnos_Totales = rep(0,dim(m_filtrada)[1]))
  mat_aux[,1] <- m_filtrada[,num_col_Semestre]
  mat_aux[,2] <- m_filtrada[,num_col_horario_num]
  mat_aux[,3] <- m_filtrada[,num_col_Alumnos]
  
  #Se suma el número de alumnos en los semestres repetidos por cada hora:
  mat_aux2 <- data.frame(Semestre = 0,Hora = 0,Alumnos_Totales = 0)
  j <- 1
  c <- 1
  while(c <= length(vec_s_sem_k_info)){
    # cat("\nc = ",c)
    # cat("\nj = ",j)
    #' Con la siguiente condición se verifica que hay información
    #' en el semestre y que el vector con información sea suficientemente
    #' largo para no caer en error.
    if(sem_con_info[j]==vec_s_sem_k_info[c] &&
       j<=length(vec_s_sem_k_info)){
      mat_hora_alum <- mat_aux[mat_aux[,1]==vec_s_sem_k_info[c],c(2,3)]
      for(r in 1:length(param$Horas)){
        # cat("\nr = ",r)
        vec_aux <- mat_hora_alum[mat_hora_alum[,1]==param$Horas[r],2]
        if(length(vec_aux)>0){
          renglon <- c(vec_s_sem_k_info[c],param$Horas[r],sum(vec_aux))
          mat_aux2 <- rbind(mat_aux2,renglon)
        }else{
          mat_aux2 <- rbind(mat_aux2,rep(0,3))
        }
      }#Fin for(r)
      c <- c + 1
    }else if(c >= length(vec_s_sem_k_info)){
      c <- length(vec_s_sem_k_info) + 1
    }
    if(j<length(sem_con_info)){
      j <- j + 1
    }else{
      c <- length(vec_s_sem_k_info) + 1
    }
  }#Fin while()
  
  ## Se quitan los renglones con ceros
  mat_aux2 <- mat_aux2[mat_aux2[,1]!=0,]
  
  #Se llenan las entradas con información
  i <- 1
  j <- 1
  for(c in 1:length(vec_s_sem_k_info)){
    # cat("\nc = ",c)
    if(sem_con_info[j] == vec_s_sem_k_info[c]){
      mat_sem <- mat_aux2[mat_aux2[,1]==vec_s_sem_k_info[c],c(2,3)]
      for(r in 1:length(param$nombre_hrs)){
        # cat("\nr = ",r)
        num_aux <- mat_sem[mat_sem[,1]==param$Horas[r],2]
        # if(horas_unicas[i] == param$Horas[r]){
        if(length(num_aux) > 0){
          mat_alumnos_corregidos[r,c] <- num_aux
        }
      }#Fin for(r)
      i <- i + 1
    }
    if(j<length(sem_con_info)){
      j <- j + 1
    }else{
      c <- length(vec_s_sem_k_info) + 1
    }
  }#Fin for(c)
  # View(mat_alumnos_corregidos)
  return(mat_alumnos_corregidos)
}


# gen_mat_1_sim_m_materias_1_sem ------------------------------------------
#' Title gen_mat_1_sim_m_materias_1_sem: Función que genera el vector
#' "vec_sim_1_sem", el cual tiene la simulación de un semestre.
#'
#' @param mat_al_1_sem: Submatriz de "mat_alumnos_corregidos" con la
#' información de los semestres anteriores al semestre del que se quiere
#' obtener la simulación.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return vec_sim_1_sem: Vector de longitud 15 (horas) el cual tiene la
#' simulación de un semestre.
#'
#' @examples
#' vec_sim_1_sem <- gen_mat_1_sim_m_materias_1_sem(mat_al_1_sem,param)
#' 
gen_mat_1_sim_m_materias_1_sem <- function(mat_al_1_sem,param){
  ##Se definen las variables que se van a utilizar:
  q1 <- param$q1
  q2 <- param$q2
  vec_sim_1_sem <- rep(0,length(param$Horas))
  
  for(r in 1:length(param$Horas)){
    vec_alumnos <- mat_al_1_sem[r,]
    if(sum(vec_alumnos) == 0){#En caso de que no haya información en el renglón
      vec_sim_1_sem[r] <- 0
    }else{
      tsData <- ts(vec_alumnos,frequency = 2)
      # Ajuste hw
      alumnos.fit.q <- hw(tsData,h=1,level = c(q1,q2),seasonal = "additive")
      cota1 <- max(0,alumnos.fit.q$lower[1])
      media <- max(0,alumnos.fit.q$mean[1])
      cota2 <- max(0,alumnos.fit.q$upper[2])
      # cota1 <- max(0,alumnos.fit.q$upper[2])
      # media <- max(0,alumnos.fit.q$mean[1])
      # cota2 <- max(0,alumnos.fit.q$upper[1])
      # cota1 <- max(0,alumnos.fit.q$lower[1])
      # media <- max(0,alumnos.fit.q$mean[1])
      # cota2 <- max(0,alumnos.fit.q$lower[2])
      # cota1 <- max(0,alumnos.fit.q$lower[2])
      # media <- max(0,alumnos.fit.q$mean[1])
      # cota2 <- max(0,alumnos.fit.q$upper[1])
      vec_sim_1_sem[r] <- sample(ceiling(cota1):ceiling(cota2),1)
      # vec_sim_1_sem[r] <- sample(ceiling(media):ceiling(cota2),1)
      # vec_sim_1_sem[r] <- sample(ceiling(cota1):ceiling(media),1)
    }
  }#Fin for(r)
  return(vec_sim_1_sem)
}


# gen_mat_1_sim_m_materias_s_sem ------------------------------------------
#' Title gen_mat_1_sim_m_materias_s_sem: Función que genera la matriz
#' "mat_1_sim_m_materias_s_sem", la cual tiene la simulación de s semestres.
#'
#' @param vec_s_sem_k_info: Vector con los "k_sem_ant + s - 1" semestres
#' de los que se quiere obtener la información para realizar la simulación
#' del vector "vec_sem_sig".
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @param param_sim: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se encargan de la simulación.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' @example param_sim <- list(vec_sem_sig = c(20191,20192,20201),k_sem_ant = 5,
#' materia = "Estadística III", num_sim = 10, m_filtrada = matrix(0),
#' sub_m_filtrada = matrix(0,ncol = length(param$nom_cols_MG)))
#'
#' @return mat_1_sim_m_materias_s_sem: Matriz de 15 renglones (horas) y "s"
#' columnas, cada una para los semestres en "vec_sem_sig".
#'
#' @examples
#' gen_mat_1_sim_m_materias_s_sem(vec_s_sem_k_info,param,param_sim)
#' 
gen_mat_1_sim_m_materias_s_sem <- function(vec_s_sem_k_info,param,param_sim){
  ##Se definen las variables que se van a utilizar:
  vec_sem_sig <- param_sim$vec_sem_sig
  k_sem_ant <- param_sim$k_sem_ant
  # m_filtrada <- param_sim$m_filtrada
  mat_alumnos_corregidos <- gen_mat_alumnos_corregidos(vec_s_sem_k_info,
                                                       param,param_sim)
  nom_cols <- colnames(mat_alumnos_corregidos)
  mat_1_sim_m_materias_s_sem <- matrix(0,nrow = length(param$nombre_hrs),
                                       ncol = length(vec_sem_sig))
  
  for(s in 1:length(vec_sem_sig)){
    sem_sig <- vec_sem_sig[s]
    vec_1_sem_k_info <- gen_vec_1_sem_k_info(sem_sig,k_sem_ant,param)
    
    #Se define la matriz auxiliar para cada "sem_sig"
    mat_aux <- matrix(0,nrow = dim(mat_alumnos_corregidos)[1],ncol = k_sem_ant)
    for(c in 1:length(vec_1_sem_k_info)){
      col_aux <- mat_alumnos_corregidos[,nom_cols == vec_1_sem_k_info[c]]
      mat_aux[,c] <- col_aux
    }#fin for(c)
    rownames(mat_aux) <- param$nombre_hrs
    colnames(mat_aux) <- vec_1_sem_k_info
    
    mat_1_sim_m_materias_s_sem[,s] <- gen_mat_1_sim_m_materias_1_sem(mat_aux,param)
  }#Fin for(s)
  rownames(mat_1_sim_m_materias_s_sem) <- param$nombre_hrs
  colnames(mat_1_sim_m_materias_s_sem) <- vec_sem_sig
  
  # View(mat_1_sim_m_materias_s_sem)
  return(mat_1_sim_m_materias_s_sem)
}


# gen_mat_real_gpos_m_materias_1_sem --------------------------------------
#' Title gen_mat_real_gpos_m_materias_1_sem: Función que genera la matriz 
#' "mat_real_gpos_m_materias" la cual contiene 24 columnas: Materia,
#' Horario, Número de grupos reales, Número de alumnos reales, las últimas
#' 20 columnas indican el número de simulaciones del tamaño de grupo, en sus
#' renglones se tiene el número real de alumnos de cada grupo por hora, para
#' "m" materias y un semestre.
#'
#' @param sem_info: Semestre del que se desea obtener información
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @param param_sim: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se encargan de la simulación.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' @example param_sim <- list(vec_sem_sig = c(20191,20192,20201),k_sem_ant = 5,
#' materia = "Estadística III", num_sim = 10, m_filtrada = matrix(0),
#' sub_m_filtrada = matrix(0,ncol = length(param$nom_cols_MG)))
#'
#' @return mat_real_gpos_m_materias: Matriz de 24 columnas: Materia,
#' Horario, Número de grupos reales, Número de alumnos reales, las últimas
#' 20 columnas indican el número de simulaciones del tamaño de grupo, en sus
#' renglones se tiene el número real de alumnos de cada grupo por hora, para
#' "m" materias de "sem_info".
#'
#' @examples
#' gen_mat_real_gpos_m_materias_1_sem(sem_info,param,param_sim)
#' 
gen_mat_real_gpos_m_materias_1_sem <- function(sem_info,param,param_sim){
  #Se definen las variables que se van a utilizar
  Materias <- param_sim$Materias
  n_materias <- length(Materias)
  col_grupos_reales <- arroja_ind_col_RG("Grupos_Reales") ##3
  
  ##Se define la matriz como data frame para agregar renglones más fácilmente.
  mat_real_gpos_m_materias <- data.frame(Materia = 0,Horario = 0,
                                         Núm.Gpos.Reales = 0,Núm.Al.Reales = 0,Grupo_1 = 0,
                                         Grupo_2 = 0,Grupo_3 = 0,Grupo_4 = 0,Grupo_5 = 0,
                                         Grupo_6 = 0,Grupo_7 = 0,Grupo_8 = 0,Grupo_9 = 0,
                                         Grupo_10 = 0,Grupo_11 = 0,Grupo_12 = 0,Grupo_13 = 0,
                                         Grupo_14 = 0,Grupo_15 = 0,Grupo_16 = 0,Grupo_17 = 0,
                                         Grupo_18 = 0,Grupo_19 = 0,Grupo_20 = 0)
  for(d in 1:n_materias){
    # cat("\nMateria ",d," de ",n_materias)
    materia <- Materias[d]
    mat_real_grupos_una_materia <- gen_mat_real_grupos_una_materia(materia,sem_info,param)
    mat_real_gpos_m_materias <- rbind(mat_real_gpos_m_materias,mat_real_grupos_una_materia)
  }
  ##Se quitan los renglones que no tienen información de grupos:
  mat_real_gpos_m_materias <- mat_real_gpos_m_materias[mat_real_gpos_m_materias[,col_grupos_reales]>0,]
  
  # View(mat_real_gpos_m_materias)
  # nom_mat_real_gpos <- paste0("mat_real_grupos por semestre/mat_real_grupos_",sem_info,".RData")
  # save(mat_real_grupos,file = nom_mat_real_gpos)
  return(mat_real_gpos_m_materias)
}


# gen_mat_esp_datos_real_s_sem --------------------------------------------
#' Title gen_mat_esp_datos_real_s_sem: Función que genera la matriz
#' "mat_real_alum" la cual contiene la esperanza del número de alumnos
#' totales reales por hora de s semestres.
#'
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @param param_sim: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se encargan de la simulación.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' @example param_sim <- list(vec_sem_sig = c(20191,20192,20201),k_sem_ant = 5,
#' materia = "Estadística III", num_sim = 10, m_filtrada = matrix(0),
#' sub_m_filtrada = matrix(0,ncol = length(param$nom_cols_MG)))
#'
#' @return mat_real_alum: Matriz de 15 renglones (horas) y tantas columnas
#' como semestres haya en "param_sim$vec_sem_sig".
#'
#' @examples
#' mat_real_alum <- gen_mat_esp_datos_real_s_sem(param,param_sim)
#' 
gen_mat_esp_datos_real_s_sem <- function(param,param_sim){
  ##Se definen las variables que se van a utilizar:
  nom_col_real_alum <- "Alumnos_Reales_Totales"
  vec_sem_sig <- param_sim$vec_sem_sig
  mat_real_alum <- matrix(0,nrow = length(param$Horas),
                          ncol = length(vec_sem_sig))
  ##Se obtienen las matrices que contienen los datos de alumnos totales
  ##reales por semestre
  
  for(s in 1:length(vec_sem_sig)){
    mat_real_gpos_m_materias <- gen_mat_real_gpos_m_materias_1_sem(vec_sem_sig[s],
                                                                   param,param_sim)
    mat_real_alum[,s] <- gen_vec_esp_datos_real_1_sem(mat_real_gpos_m_materias,
                                                      nom_col_real_alum,
                                                      param,param_sim)
  }#Fin for(s)
  return(mat_real_alum)
}



# pruebas_num_alum_total --------------------------------------------------
#' Title pruebas_num_alum_total: Función encargada de obtener las matrices
#' "mat_dif_relativas" y "mat_var_sim" del número total de alumnos para "s"
#' semestres. En la función se guardan las gráficas heatmap de cada matriz.
#'
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @param param_sim: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se encargan de la simulación.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' @example param_sim <- list(vec_sem_sig = c(20191,20192,20201),k_sem_ant = 5,
#' materia = "Estadística III", num_sim = 10, m_filtrada = matrix(0),
#' sub_m_filtrada = matrix(0,ncol = length(param$nom_cols_MG)))
#'
#' @return lista_mat_pruebas: Lista con las matrices "mat_dif_relativas"
#' y "mat_var_sim"
#'
#' @examples
#' lista_mat_pruebas <- pruebas_num_alum_total(param,param_sim)
#' 
pruebas_num_alum_total <- function(param,param_sim){
  ##Se definen las variables que se van a utilizar:
  vec_sem_sig <- param_sim$vec_sem_sig
  k_sem_ant <- param_sim$k_sem_ant
  num_sim <- param_sim$num_sim
  lista_n_sim_s_sem <- list()
  nombres_lista <- 0
  vec_s_sem_k_info <- gen_vec_s_sem_k_info(vec_sem_sig,k_sem_ant,param)
  lista_mat_pruebas <- list()
  
  #Se inicia con las n simulaciones
  for(k in 1:num_sim){
    # cat("\nSimulación número ",k)
    mat_1_sim <- gen_mat_1_sim_m_materias_s_sem(vec_s_sem_k_info,param,param_sim)
    lista_n_sim_s_sem[[k]] <- mat_1_sim
    nombres_lista <- c(nombres_lista,paste0("mat_",k,"_sim"))
  }
  #Se quita el cero que está al inicio del vector de nombres
  nombres_lista <- nombres_lista[-1]
  names(lista_n_sim_s_sem) <- nombres_lista
  
  arreglo_aux <- array(do.call(cbind,lista_n_sim_s_sem),
                       dim=c(dim(lista_n_sim_s_sem[[1]]),
                             length(lista_n_sim_s_sem)))
  ##Esperanza
  mat_sim_alum <- apply(arreglo_aux,c(1, 2),mean,na.rm = TRUE)
  
  ##Diferencia relativa
  mat_real_alum <- gen_mat_esp_datos_real_s_sem(param,param_sim)
  # mat_dif_total_alumnos_x_sem <- mat_real_alum - mat_sim_alum
  # mat_dif_relativas <- gen_mat_dif_relativas(mat_sim_alum,mat_real_alum,
  #                                            nom_archivo_dif_rel)
  nom_archivo_dif_abs <- paste0("dif_absoluta_total_de_alumnos_x_sem_",
                                vec_sem_sig[1],"-",vec_sem_sig[length(vec_sem_sig)])
  mat_dif_abs <- gen_mat_diferencias(mat_sim_alum,mat_real_alum,
                                     nom_archivo_dif_abs,0)
  rownames(mat_dif_abs) <- param$nombre_hrs
  colnames(mat_dif_abs) <- vec_sem_sig
  nom_archivo_dif_rel <- paste0("dif_relativa_total_de_alumnos_x_sem_",
                                vec_sem_sig[1],"-",vec_sem_sig[length(vec_sem_sig)])
  mat_dif_relativas <- gen_mat_diferencias(mat_sim_alum,mat_real_alum,
                                           nom_archivo_dif_rel,1)
  rownames(mat_dif_relativas) <- param$nombre_hrs
  colnames(mat_dif_relativas) <- vec_sem_sig
  # View(mat_dif_relativas)
  
  ##Varianza
  mat_var_sim <- apply(arreglo_aux,c(1, 2),var, na.rm = TRUE)
  rownames(mat_var_sim) <- param$nombre_hrs
  colnames(mat_var_sim) <- vec_sem_sig
  nom_archivo_var <- paste0("Figuras/Matrices Simuladas/heatmap_var_alum_total_",
                            vec_sem_sig[1],"-",vec_sem_sig[length(vec_sem_sig)],".jpeg")
  ## Para guardar gráficas de R como imagen .jpeg se manda llamar la función
  ## jpeg() antes de generar una gráfica. Al hacer esto, le indicamos a R
  ##que en lugar de mandar nuestro gráfico a una ventana del escritorio, lo
  ##mande a un dispositivo gráfico distinto.
  ## La función dev.off(), se utiliza para cerrar el dispositivo gráfico
  ##elegido y así poder crear más gráficos después.
  jpeg(filename = nom_archivo_var, width = 800, height = 700)
  colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
  heatmap(mat_var_sim, Colv = NA, Rowv = NA, scale="none",col=colMain,
          main = paste0(vec_sem_sig[1],"-",vec_sem_sig[length(vec_sem_sig)]))
  dev.off()
  
  lista_mat_pruebas[[1]] <- mat_dif_relativas
  lista_mat_pruebas[[2]] <- mat_dif_abs
  lista_mat_pruebas[[3]] <- mat_var_sim
  save(lista_mat_pruebas,file = "Listas mat_n_sim/lista_mat_pruebas.RData")
  
  return(lista_mat_pruebas)
}



# extrae_alumnos_1_materia ----------------------------------------------------------
#' Title: extrae_alumnos_1_materia: Se extrae el número de alumnos por hora y por
#' semestre de "materia".
#' 
#' @param materia: Nombre de la materia de la cual se obtendrá el número de
#' grupos por semestre.
#' @examples materia <- "Probabilidad I"
#' @return mat_alumnos_x_hora_sem: Matriz con 15 renglones (horas) y tantas columnas
#' como semestres de los que se desee obtener la información. Tiene como información
#' el número de alumnos por hora y semestre de "materia".
#' 
extrae_alumnos_1_materia <- function(materia,param){
  ##Se definen las variables que se van a utilizar
  m_grande_total <- param$m_grande_total
  ind_materia <- checa_ind_materia(materia,m_grande_total)
  num_col_Alumnos <- arroja_ind_col_MG("Alumnos")
  Alumnos <- m_grande_total[,num_col_Alumnos]
  num_col_horario_num <- arroja_ind_col_MG("horario_num")
  horario_num <- m_grande_total[,num_col_horario_num]
  num_col_Semestre <- arroja_ind_col_MG("Semestre")
  Semestre <- m_grande_total[,num_col_Semestre]
  
  ##Vector con el número de alumnos de "materia", en tipo "numeric"
  datos_alumnos <- Alumnos[ind_materia]
  
  ##Matriz con 2 columnas: Horas-Semestre de "materia"
  datos_horas_sem <- cbind(horario_num[ind_materia],Semestre[ind_materia])
  ##Matriz con 3 columnas: Horas-Semestre-Número de alumnos de "materia"
  mat_alumnos <- cbind(datos_horas_sem,datos_alumnos)
  ##Se eliminan los renglones repetidos
  mat_num_alumnos <- unique(mat_alumnos)
  colnames(mat_num_alumnos) <- c("Horas","Sem","Alumnos")
  
  mat_alumnos_x_hora_sem <- matrix(0,nrow = length(param$Horas),
                                   ncol = length(param$Semestres))
  rownames(mat_alumnos_x_hora_sem) <- param$nombre_hrs
  colnames(mat_alumnos_x_hora_sem) <- param$nombre_sem
  
  for(j in 1:length(param$Semestres)){
    # cat("\nj = ",j)
    ## Matriz con las columnas Horas-Alumnos con la información
    ##de cada semestre
    datos_x_sem <- mat_num_alumnos[mat_num_alumnos[,2]==param$Semestres[j],
                                   c(1,3)]
    for(k in 1:length(param$Horas)){
      # cat("\nk = ",k)
      if(anyNA(datos_x_sem) || length(datos_x_sem)<2){
        #En caso de que no haya información
        mat_alumnos_x_hora_sem[k,j] <- 0
      }else if(dim(datos_x_sem)[1]>1 && length(datos_x_sem)>2){
        ## Entra al if si se tiene más de un renglón en la matriz
        ##y se tiene más de 2 datos. La segunda condición se pone
        ##para evitar errores cuando la matriz sólo tiene un renglón.
        mat_alumnos_x_hora_sem[k,j] <- sum(as.numeric(datos_x_sem
                                                      [datos_x_sem[,1]==
                                                          param$Horas[k],2]))
        ##Se suma el número de alumnos del semestre por cada hora
      }else if(datos_x_sem[1]==param$Horas[k]){
        #En caso de que hay sólo un renglón con info
        mat_alumnos_x_hora_sem[k,j] <- sum(as.numeric(datos_x_sem[2]))
      }
    }##fin for(k) #Renglones
  }##fin for(j) #Columnas
  # View(mat_alumnos_x_hora_sem)
  return(mat_alumnos_x_hora_sem)
}


# estima_alumnos_1_materia -----------------------------------------------------------
#' Title: estima_alumnos_1_materia: Función que arroja una matriz de 15 renglones (horas)
#' y 3 columnas: cota1, media, cota2; los cuales corresponden a la estimación
#' del número de alumnos que se tendrán en el siguiente semestre por hora.
#' @param mat_alumnos: Matriz con 15 renglones (horas) y tantas columnas
#' como semestres de los que se desee obtener la información. Tiene como
#' información el número de alumnos por hora y semestre de "materia".
#' @example mat_alumnos[5,] <- c(74,39,...,0)
#' 
#' @return mat_alumnos_estimados: matriz de 15 renglones (horas) y 3 columnas:
#' cota1, media, cota2; los cuales corresponden a la estimación del número
#' de alumnos que se tendrán en el siguiente semestre por hora.
estima_alumnos_1_materia <- function(mat_alumnos,param){
  ##Se definen las variables que se van a utilizar
  q1 <- param$q1
  q2 <- param$q2
  mat_alumnos_estimados <- matrix(0,ncol = 3,nrow = length(param$Horas))
  
  ##A cada renglón se le aplica la función HoltWinters, la cual recibe
  ##un objeto de tipo "ts" (time series)
  for(i in 1:length(param$Horas)){
    tsData <- ts(mat_alumnos[i,],frequency = 2)
    # Ajuste hw
    alumnos.fit.q1 <- hw(tsData,h=1,level = q1,seasonal = "additive")
    alumnos.fit.q2 <- hw(tsData,h=1,level = q2,seasonal = "additive")
    
    #En caso de que haya valores negativos en las estimaciones se acota
    #por abajo con cero.
    cota1 <- max(0,alumnos.fit.q1$lower[1])
    media <- max(0,alumnos.fit.q1$mean[1])
    cota2 <- max(0,alumnos.fit.q2$upper[1])
    mat_alumnos_estimados[i,] <- c(cota1,media,cota2)
  }##Fin for(i)
  rownames(mat_alumnos_estimados) <- param$nombre_hrs
  colnames(mat_alumnos_estimados) <- c("Cota1","Media","Cota2")
  # View(mat_alumnos_estimados)???
  
  return(mat_alumnos_estimados)
}


# simula_alumnos_1_materia -----------------------------------------------------------
#' Title: simula_alumnos_1_materia: Se manda llamar la función "estima_alumnos_1_materia" para
#' simular una variable aleatoria discreta en el intervalo de las cotas
#' arrojadas por dicha función. La función simula_alumnos_1_materia regresa un vector
#' con el número total de alumnos simulados por cada hora para el siguiente
#' semestre.
#' @param materia: Nombre de algún curso impartido en la FC
#' @example materia <- "Probabilidad I"
#' 
#' @return vec_alumnos_simulados: Vector con el número total de alumnos
#' simulados por cada hora para el siguiente semestre.
simula_alumnos_1_materia <- function(materia,param){
  ##Extracción de información
  mat_alumnos_x_hora_sem <- extrae_alumnos_1_materia(materia,param)
  
  ##Estimación de alumnos
  mat_alumnos_estimados <- estima_alumnos_1_materia(mat_alumnos_x_hora_sem,param)
  
  ##Simulación
  n_rep <- length(param$Horas)
  vec_alumnos_simulados <- rep(0,n_rep)
  
  for(i in 1:n_rep){
    cota1 <- floor(mat_alumnos_estimados[i,1])
    cota2 <- ceiling(mat_alumnos_estimados[i,3])
    alum_sim <- runif(1,min = cota1,max = cota2)
    vec_alumnos_simulados[i] <- ceiling(alum_sim)
  }
  vec_alumnos_simulados <- matrix(vec_alumnos_simulados,nrow = n_rep)
  rownames(vec_alumnos_simulados) <- param$nombre_hrs
  
  return(vec_alumnos_simulados)
}


# simula_tam_gpo_1_materia -----------------------------------------------------
#' Title simula_tam_gpo_1_materia: Función que simula el tamaño de los grupos por
#' materia en cada hora dependiendo del número de alumnos que se han tenido.
#'
#' @param materia: Nombre de algún curso impartido en la FC
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' 
#' @example materia <- "Probabilidad I"
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
#' @return sim_tam_gpo_x_hora: Vector que indica el tamaño simulado de cada
#' grupo por hora.
#'
simula_tam_gpo_1_materia <- function(materia,param){
  ##Inicializamos las variables que se van a utilizar
  # lista_def_columnas_MG <- param$lista_def_columnas_MG
  m_grande_total <- param$m_grande_total
  ind_materia <- checa_ind_materia(materia,m_grande_total)
  num_col_horario_num <- arroja_ind_col_MG("horario_num")
  num_col_Alumnos <- arroja_ind_col_MG("Alumnos")
  horario_num <- m_grande_total[,num_col_horario_num]
  Alumnos <- m_grande_total[,num_col_Alumnos]
  
  ##Matriz con 2 columnas: Horas-Número de alumnos de "materia", en tipo "numeric"
  datos_horas_alumnos <- cbind(horario_num[ind_materia],Alumnos[ind_materia])
  colnames(datos_horas_alumnos) <- c("Horas","Alumnos")
  vec_horas <- 7:21
  sim_tam_gpo_x_hora <- rep(0,length(param$nombre_hrs))
  
  for(d in 1:length(param$nombre_hrs)){
    ##Se toman las cantidades > 0 de alumnos por hora
    vec_tam <- datos_horas_alumnos[datos_horas_alumnos[,1]==vec_horas[d],2]
    #' No aplicamos la función unique al vector para respetar la probabilidad
    #' de elegir tamaños de grupo entre más repetidos haya.
    # vec_tam <- unique(vec_tam[vec_tam>0])
    vec_tam <- vec_tam[vec_tam>0]
    if(length(vec_tam) > 1){
      sim_tam_gpo_x_hora[d] <- sample(vec_tam,size = 1)
    }else if(length(vec_tam) == 1){
      ## Cuando el vector sólo tiene una entrada la función sample
      ##toma una muestra aleatoria entre 1 y el número en esa entrada,
      ##es por ello que se separan en casos de acuerdo a la longitud
      ##del vector.
      sim_tam_gpo_x_hora[d] <- vec_tam
    }
  }#Fin for(d)
  
  return(sim_tam_gpo_x_hora)
}


# gen_mat_simula_gpos_1_materia --------------------------------------
#' Title gen_mat_simula_gpos_1_materia: Función que guarda la matriz 
#' "mat_simula_grupos_una_materia" por materia, la cual contiene 24 columnas:
#' Materia, Horario, Número de grupos simulados, Número de alumnos simulados,
#' las últimas 20 columnas indican el número de simulaciones del tamaño de
#' grupo, en sus renglones se tiene el número de alumnos de cada grupo
#' simulado por hora.
#'
#' @param materia: Nombre de algún curso impartido en la FC
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' 
#' @example materia <- "Probabilidad I"
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
#' @return mat_simula_grupos_una_materia: Matriz con 24 columnas: Materia, Horario,
#' Número de grupos simulados, Número de alumnos simulados, las últimas 20
#' columnas indican el número de simulaciones del tamaño de grupo, en sus
#' renglones se tiene el número de alumnos de cada grupo por hora.
#' 
gen_mat_simula_gpos_1_materia <- function(materia,param){
  # Se obtiene el índice de los renglones de la matriz "m_grande_total"
  #que tienen la información de "materia"
  m_grande_total <- param$m_grande_total
  ind_materia <- checa_ind_materia(materia,m_grande_total)
  
  ##Se define la matriz como data frame
  mat_simula_grupos_una_materia <- data.frame(Materia = 0,Horario = param$nombre_hrs,
                                              Núm.Gpos.Simulados = 0,
                                              Núm.Al.Simulados = 0,Sim_1 = 0,Sim_2 = 0,
                                              Sim_3 = 0,Sim_4 = 0,Sim_5 = 0,Sim_6 = 0,
                                              Sim_7 = 0,Sim_8 = 0,Sim_9 = 0,Sim_10 = 0,
                                              Sim_11 = 0,Sim_12 = 0,Sim_13 = 0,Sim_14 = 0,
                                              Sim_15 = 0,Sim_16 = 0,Sim_17 = 0,Sim_18 = 0,
                                              Sim_19 = 0,Sim_20 = 0)
  
  ## Números de columna de la matriz "mat_simula_grupos_una_materia"
  col_def_Materia <- arroja_ind_col_SG("Materia") ##1
  col_def_Horario <- arroja_ind_col_SG("Horario") ##2
  col_grupos_simulados <- arroja_ind_col_SG("Grupos_Simulados") ##3
  col_alum_sim_total <- arroja_ind_col_SG("Alumnos_Simulados_Totales") ##4
  col_1er_grupo <- arroja_ind_col_SG("col_1er_grupo") ##5
  col_ult_grupo <- arroja_ind_col_SG("col_ult_grupo") ##24
  rango_grupos <- col_1er_grupo:col_ult_grupo
  
  ##Se llenan las columnas: "Materia", "Horario" y "Núm.Al.Simulados" la cual 
  #tiene el número de alumnos simulados para "materia" en cada horario.
  # El número 15 es porque hay 15 horarios en total (7-8,...,21-22)
  vec_renglones <- 1:length(param$Horas)
  mat_simula_grupos_una_materia[vec_renglones,col_def_Materia] <- materia
  # mat_simula_grupos_una_materia[vec_renglones,col_def_Horario] <- param$nombre_hrs
  vec_alumnos_simulados <- simula_alumnos_1_materia(materia,param)
  mat_simula_grupos_una_materia[vec_renglones,col_alum_sim_total] <- vec_alumnos_simulados
  
  ##Se llenan las demás columnas de la matriz "mat_simula_grupos_una_materia",
  #en caso de que si haya habido grupos simulados para "materia"
  if(sum(vec_alumnos_simulados) > 0){
    for(d in rango_grupos){##Se recorren las columnas
      # cat("\n d =",d)
      alto <- 0
      while(alto == 0){
        #Se simula el tamaño de grupo por hora en cada columna
        sim_tam_gpo_x_hora <- simula_tam_gpo_1_materia(materia,param)
        for(j in 1:length(param$Horas)){ ##Se recorren los renglones
          # cat("\nj =",j)
          num_alumnos <- mat_simula_grupos_una_materia[j,col_alum_sim_total]
          
          ##Preguntamos si la suma de los alumnos sigue siendo menor
          #al número de alumnos simulados
          if(sum(mat_simula_grupos_una_materia[j,rango_grupos])<num_alumnos){
            mat_simula_grupos_una_materia[j,d] <- sim_tam_gpo_x_hora[j]
            # cat("\n Entra al primer if")
          }else if(sum(mat_simula_grupos_una_materia[j,rango_grupos])>=num_alumnos
                   || d==col_ult_grupo){
            # cat("\n Entra al segundo if")
            alto <- 1
          }
          # cat("\n alto =",alto)
        }##Fin de for(j) #Renglones
      }##Fin de while
    }##Fin de for(d) #Columnas
    
    ##Se llena la columna con el número de grupos simulados por materia y
    ##se restan tantos alumnos como sean necesarios en el último grupo
    ##para que la suma del número de alumnos por grupo sea igual al número
    ##de alumnos simulados. Después se acomodan de mayor a menor los grupos.
    for(k in 1:length(param$Horas)){
      renglon <- mat_simula_grupos_una_materia[k,rango_grupos]
      num_gpos_sim <- length(renglon[renglon>0])
      mat_simula_grupos_una_materia[k,col_grupos_simulados] <- num_gpos_sim
      num_alum_sim <- mat_simula_grupos_una_materia[k,col_alum_sim_total]
      suma_alum_x_gpo <- sum(mat_simula_grupos_una_materia[k,col_1er_grupo:col_ult_grupo])
      col_ult_gpo_sim <- col_alum_sim_total+num_gpos_sim
      
      mat_simula_grupos_una_materia[k,col_ult_gpo_sim] <- mat_simula_grupos_una_materia[k,col_ult_gpo_sim]-
        (suma_alum_x_gpo-num_alum_sim)
      
      rango <- col_1er_grupo:col_ult_gpo_sim
      mat_simula_grupos_una_materia[k,rango] <- sort(mat_simula_grupos_una_materia[k,rango],
                                                     decreasing = T)
    }
  }else{ #Si no hay grupos simulados de "materia"
    mat_simula_grupos_una_materia <- data.frame(Materia = 0,Horario = param$nombre_hrs,
                                                Núm.Gpos.Simulados = 0,
                                                Núm.Al.Simulados = 0,Sim_1 = 0,Sim_2 = 0,
                                                Sim_3 = 0,Sim_4 = 0,Sim_5 = 0,Sim_6 = 0,
                                                Sim_7 = 0,Sim_8 = 0,Sim_9 = 0,Sim_10 = 0,
                                                Sim_11 = 0,Sim_12 = 0,Sim_13 = 0,Sim_14 = 0,
                                                Sim_15 = 0,Sim_16 = 0,Sim_17 = 0,Sim_18 = 0,
                                                Sim_19 = 0,Sim_20 = 0)
    ##Se llena la columna: "Materia"
    mat_simula_grupos_una_materia[vec_renglones,col_def_Materia] <- materia
  }
  
  # View(mat_simula_grupos_una_materia)
  # nom_archivo <- paste0("mat_simula_grupos por materia 20202/mat_simula_grupos_",materia,".RData")
  # save(mat_simula_grupos_una_materia,file = nom_archivo)
  return(mat_simula_grupos_una_materia)
}


# gen_list_n_sim_1_materia --------------------------------------------------
#' Title gen_list_n_sim_1_materia: Función que hace "n" simulaciones fijando un
#' semestre y una materia; arroja una matriz con el número de alumnos por
#' grupo ordenados de mayor a menor por cada hora. Las "n" matrices generadas
#' se guardan en una lista llamada "lista_mat_n_sim". Cada matriz generada
#' tiene 24 columnas: Materia, Horario, Número total de grupos simulados,
#' Número total de alumos simulados, las siguientes 20 columnas contienen
#' el número de alumnos simulados, ordenados de mayor a menor. Las matrices
#' generadas se guardan en una lista llamada "lista_mat_n_sim".
#'
#' @param materia: Nombre de algún curso impartido en la FC.
#' @param num_materia: Número del índice de "materia" en "vec_nom_materias_total"
#' @param num_sim: Número de matrices simuladas.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' 
#' @example materia <- "Probabilidad I"
#' @example num_materia <- 60
#' @example num_sim <- 10
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
#' @return lista_mat_n_sim: Lista que contiene las "n" matrices generadas con
#' el número de alumnos simulados, ordenados de mayor a menor.
#'
gen_list_n_sim_1_materia <- function(materia,num_sim,param){
  #Se definen las variables que se van a utilizar
  sem_sig <- param$sem_sig
  num_materia <- arroja_num_materia(materia,param)
  m_grande_total <- param$m_grande_total
  
  #Se inicia con las n simulaciones
  lista_mat_n_sim <- list()
  nombres_lista <- 0
  for(k in 1:num_sim){
    # cat("\nSimulación número ",k)
    mat_1_sim <- gen_mat_simula_gpos_1_materia(materia,param)
    
    lista_mat_n_sim[[k]] <- mat_1_sim
    nombres_lista <- c(nombres_lista,paste0("mat_",k,"_sim"))
  }
  #Se quita el cero que está al inicio del vector de nombres
  nombres_lista <- nombres_lista[-1]
  names(lista_mat_n_sim) <- nombres_lista
  
  nom_lista_n_sim <- gen_nom_list_n_sim_1_materia(num_sim,num_materia,sem_sig)
  
  save(lista_mat_n_sim,file = nom_lista_n_sim)
  return(lista_mat_n_sim)
}


##########################################################################
##### PRUEBAS RÁPIDAS #####
#'Funciones para las pruebas del modelo para la asignación de horarios.
#'Las pruebas son prácticamente instantáneas.
##########################################################################


# gen_vec_1_sem_k_info ----------------------------------------------------
#' Title gen_vec_1_sem_k_info: Función que genera el vector
#' "vec_1_sem_k_info" con los "k_sem_ant" semestres de los que se quiere
#' obtener la información para realizar la simulación de "sem_sig".
#'
#' @param sem_sig: Semestre del que se obtienen las simulaciones. 
#' @param k_sem_ant: Número de semestres de información se quieren para
#' la simulación.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
#' @return vec_1_sem_k_info: Vector con los "k_sem_ant" semestres de los
#' que se quiere obtener la información para realizar la simulación de
#' "sem_sig".
#'
#' @examples
#' gen_vec_1_sem_k_info(20182,5,param)
#' 
gen_vec_1_sem_k_info <- function(sem_sig,k_sem_ant,param){
  for(d in 1:length(param$sem_totales)){
    # cat("\n d = ",d)
    if(param$sem_totales[d]==sem_sig){
      ind_sem_ini <- d-k_sem_ant
      # cat("\n sem_ini = ",param$sem_totales[ind_sem_ini])
      
      ind_sem_fin <- d-1
      # cat("\n sem_fin = ",param$sem_totales[ind_sem_fin])
    }
  }#Fin for(d)
  
  vec_1_sem_k_info = param$sem_totales[ind_sem_ini:ind_sem_fin]
  return(vec_1_sem_k_info)
}

# gen_vec_s_sem_k_info ----------------------------------------------------
#' Title gen_vec_s_sem_k_info: Función que genera el vector
#' "vec_s_sem_k_info" con los "k_sem_ant" semestres de los que se quiere
#' obtener la información para realizar la simulación de los semestres en
#' el vector "vec_sem_sig".
#'
#' @param vec_sem_sig: Vector con los semestres de los que se desean obtener
#' las simulaciones. Deben estar ordenados del más antiguo al más reciente.
#' @param k_sem_ant: Número de semestres de información se quieren para
#' la simulación.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
#' @return vec_s_sem_k_info: Vector con los "k_sem_ant + s - 1" semestres
#' de los que se quiere obtener la información para realizar la simulación
#' del vector "vec_sem_sig".
#'
#' @examples
#' gen_vec_s_sem_k_info(c(20181,20182,20191),5,param) 
#'
gen_vec_s_sem_k_info <- function(vec_sem_sig,k_sem_ant,param){
  ##Se definen las variables que se van a utilizar:
  vec_aux <- 0
  
  for(s in 1:length(vec_sem_sig)){
    sem_sig <- vec_sem_sig[s]
    vec_aux <- c(vec_aux,gen_vec_1_sem_k_info(sem_sig,k_sem_ant,param))
  }
  vec_s_sem_k_info <- sort(unique(vec_aux[-1]))
  
  return(vec_s_sem_k_info)
}


# gen_mat_m_filtrada ------------------------------------------------------
#' Title gen_mat_m_filtrada: Función que genera la matriz "m_filtrada" la
#' cual es una submatriz de "m_grande_total". La submatriz contiene
#' información de m materias de los semestres correspondientes a los
#' semestres de "vec_sem_sig".
#' Por ejemplo, si
#' vec_sem_sig <- c(20172,20181,20182) y k_sem_ant <- 5 entonces se toma
#' la información desde 2015-1 hasta 2018-1 (7 semestres) de las m materias.
#'SUPONEMOS QUE LA LISTA "param" YA TIENE CARGADA LA MATRIZ "m_grande_total"
#'DE 2008-1 HASTA 2020-1.
#'
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @param param_sim: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se encargan de la simulación.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' @example param_sim <- list(vec_sem_sig = c(20191,20192,20201),k_sem_ant = 5,
#' materia = "Estadística III", num_sim = 10, m_filtrada = matrix(0),
#' sub_m_filtrada = matrix(0,ncol = length(param$nom_cols_MG)))
#'
#' @return m_filtrada: Submatriz de "m_grande_total" que  contiene la
#' información de m materias de los semestres correspondientes a "vec_sem_sig".
#'
#' @examples
#' gen_mat_m_filtrada(param,param_sim)
#'
gen_mat_m_filtrada <- function(param,param_sim){
  ##Se definen las variables que se van a utilizar:
  Materias <- param_sim$Materias
  num_col_Materia <- arroja_ind_col_MG("Materia")##1
  num_col_sem <- arroja_ind_col_MG("Semestre")##11
  MGT <- param$m_grande_total
  mat_aux2 <- matrix(0,ncol = length(param$nom_cols_MG))
  colnames(mat_aux2) <- param$nom_cols_MG
  vec_sem_sig <- param_sim$vec_sem_sig
  vec_s_sem_k_info <- gen_vec_s_sem_k_info(vec_sem_sig,param_sim$k_sem_ant,param)
  ind_materia <- 0
  
  #' Se obtienen una matriz auxiliar sólo con los semestres que queremos:
  for(s in 1:length(vec_s_sem_k_info)){
    mat_aux <- MGT[MGT[,num_col_sem]==vec_s_sem_k_info[s],]
    colnames(mat_aux) <- param$nom_cols_MG
    mat_aux2 <- rbind(mat_aux2,mat_aux)
  }
  ## Se quita el renglón de ceros inicial y los renglones con NA
  mat_aux2 <- mat_aux2[mat_aux2[,num_col_Materia]!=0,]
  mat_aux2 <- mat_aux2[!is.na(mat_aux2[,num_col_Materia]),]
  
  #Se obtienen los índices de los renglones que corresponden a las materias
  for(m in 1:length(Materias)){
    materia <- Materias[m]
    ind_materia <- c(ind_materia,checa_ind_materia(materia,mat_aux2))
  }
  ind_materia <- ind_materia[-1]
  
  ### Se define m_filtrada ##
  m_filtrada <- mat_aux2[ind_materia,]
  colnames(m_filtrada) <- param$nom_cols_MG
  
  return(m_filtrada)
}


# gen_sub_m_filtrada ------------------------------------------------------
#' Title gen_sub_m_filtrada: Función que regresa la matriz 
#' "sub_m_filtrada" la cual es una submatriz de "m_filtrada" con la
#' información de "materia" y de los "k_sem_ant" de "sem_sig".
#' Si sem_sig = 20182 y k_sem_ant = 5 => sub_m_filtrada tendrá la información
#' de "materia" de los semestres 2016-1 al 2018-1.
#'
#' @param materia: Nombre de algún curso impartido en la FC.
#' @param sem_sig: Semestre del que se obtienen las simulaciones.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @param param_sim: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se encargan de la simulación.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' @example param_sim <- list(vec_sem_sig = c(20191,20192,20201),k_sem_ant = 5,
#' materia = "Estadística III", num_sim = 10, m_filtrada = matrix(0),
#' sub_m_filtrada = matrix(0,ncol = length(param$nom_cols_MG)))
#'
#' @return sub_m_filtrada: Matriz de 37 columnas, submatriz de
#' "m_filtrada" con la información de "materia" y de los "k_sem_ant" de
#' "sem_sig".
#'
#' @examples
#' gen_sub_m_filtrada("Estadística I",5,20182,param,param_sim)
#' 
gen_sub_m_filtrada <- function(materia,sem_sig,param,param_sim){
  #Se definen las variables que se van a utilizar
  k_sem_ant <- param_sim$k_sem_ant
  vec_1_sem_k_info <- gen_vec_1_sem_k_info(sem_sig,k_sem_ant,param)
  param_sim$vec_sem_sig = vec_1_sem_k_info
  param$m_grande_total = param_sim$m_filtrada
  
  #Se define la matriz sub_m_filtrada
  sub_m_filtrada <- gen_mat_m_filtrada(materia,param,param_sim)
  
  return(sub_m_filtrada)
}


# gen_list_n_sim_1_mat_n_sem ----------------------------------------------
#' Title gen_list_n_sim_1_mat_n_sem: Función utilizada para hacer pruebas
#' rápidas con una sola materia. Genera n simulaciones de varios semestres.
#'
#' @param vec_sem_sig: Vector con los semestres de los que se desean obtener
#' las simulaciones. Deben estar ordenados del más antiguo al más reciente.
#' @param vec_k_sem_info; Vector con el número de semestres anteriores para
#' cada "sem_sig" del vector "vec_sem_sig"
#' @param materia: Nombre de algún curso impartido en la FC.
#' @param num_sim: Número de matrices simuladas.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#'
#' @example vec_sem_sig <- c(20131,20152,20182,20201)
#' @example vec_k_sem_info <- c(10,15,21,24)##Se inicia en 2008-1
#' @example materia <- "Estadística III"
#' @example num_sim <- 20
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' 
gen_list_n_sim_1_mat_n_sem <- function(vec_sem_sig,vec_k_sem_info,materia,
                                       num_sim,param){
  ##Se define la matriz m_filtrada para que el proceso sea más eficiente
  m_filtrada <- gen_mat_m_filtrada(vec_sem_sig,vec_k_sem_info,materia,param)
  param$m_grande_total = m_filtrada
  
  ##Se realiza un ciclo para cada semestre
  for(s in 1:length(vec_sem_sig)){
    # cat("\n s = ",s)
    for(d in 1:length(param$sem_totales)){
      # cat("\n d = ",d)
      if(param$sem_totales[d]==vec_sem_sig[s]){
        ind_sem_ini <- d-vec_k_sem_info[s]
        # cat("\n sem_ini = ",param$sem_totales[ind_sem_ini])
        
        ind_sem_fin <- d-1
        # cat("\n sem_fin = ",param$sem_totales[ind_sem_fin])
      }
    }#Fin for(d)
    ##Se definen las variables de param que cambian:
    param$sem_ini = param$sem_totales[ind_sem_ini]
    param$sem_fin = param$sem_totales[ind_sem_fin]
    param$sem_sig = vec_sem_sig[s]
    param$Semestres = param$sem_totales[ind_sem_ini:ind_sem_fin]
    param$nombre_sem = as.character(param$Semestres)
    param$n_semestres_anteriores = length(param$Semestres)
    
    gen_list_n_sim_1_materia(materia,num_sim,param)
  }#Fin for(s)
}


# arroja_error_espacio_en_mat ---------------------------------------------
#' Title arroja_error_espacio_en_mat: Función que indica si se debe arrojar
#' un error o no al momento de revisar si hay espacio suficiente para guardar
#' la información de los grupos reales. Arroja un 1 si el número de columnas
#' para guardar la información es menor al número máximo de grupos reales.
#'
#' @param sem_info: Semestre del que se desea obtener información.
#' @example sem_info <- 20182
#'
#' @return error_1si_0no: Variable binaria la cual vale 1 si el número de
#' columnas para guardar la información es menor al número máximo de grupos
#' reales (i.e. si hay un error).
#'
arroja_error_espacio_en_mat <- function(sem_info){
  ##Se carga la matriz m_grande de "sem_info"
  dir_info <- paste0("mat_real_grupos por semestre/mat_real_grupos_",sem_info,".RData")
  load(dir_info)
  
  ## Números de columna de la matriz "mat_real_grupos"
  col_grupos_reales <- arroja_ind_col_RG("Grupos_Reales") ##3
  col_1er_grupo <- arroja_ind_col_RG("col_1er_grupo") ##5
  col_ult_grupo <- arroja_ind_col_RG("col_ult_grupo") ##24
  
  ##Se definen las variables que se van a utilizar:
  error_1si_0no <- 0
  num_max_gpos <- max(mat_real_grupos[,col_grupos_reales])
  
  if((col_ult_grupo - col_1er_grupo + 1) < num_max_gpos){
    error_1si_0no <- 1
  }
  
  return(error_1si_0no)
}


# gen_mat_real_grupos_una_materia ----------------------------------------
#' Title gen_mat_real_grupos_una_materia: Función que guarda la matriz 
#' "mat_real_grupos_una_materia" la cual contiene 24 columnas: Materia,
#' Horario, Número de grupos reales, Número de alumnos reales, las últimas
#' 20 columnas indican el número de simulaciones del tamaño de grupo, en sus
#' renglones se tiene el número real de alumnos de cada grupo por hora.
#' Fijando materia y semestre
#'
#' @param materia: Nombre de algún curso impartido en la FC.
#' @param sem_info: Semestre del que se desea obtener información
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example materia <- "Probabilidad I"
#' @example sem_info <- 20182
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return mat_real_grupos_una_materia: Matriz con 24 columnas: Materia, Horario,
#' Número de grupos reales, Número de alumnos reales, las últimas 20
#' columnas indican el número de simulaciones del tamaño de grupo, en sus
#' renglones se tiene el número real de alumnos de cada grupo por hora.
#' 
gen_mat_real_grupos_una_materia <- function(materia,sem_info,param){
  # error_1si_0no <- arroja_error_espacio_en_mat(sem_info)
  error_1si_0no <- 0
  
  if(error_1si_0no == 0){
    ##Se define el número de columna para "Materia", "Horario" y "Núm. de alumnos"
    num_col_Materia <- arroja_ind_col_MG("Materia")
    num_col_horario_num <- arroja_ind_col_MG("horario_num")
    num_col_alum <- arroja_ind_col_MG("Alumnos")
    
    ##Se carga la matriz m_grande de "sem_info"
    direccion_info <- paste0("m_grande por semestre/m_grande_",sem_info,".RData")
    load(direccion_info)
    
    ##Se define la matriz como data frame para agregar renglones más fácilmente.
    mat_real_grupos_una_materia <- data.frame(Materia = 0,Horario = 1:15,
                                              Núm.Gpos.Reales = 0,Núm.Al.Reales = 0,Grupo_1 = 0,
                                              Grupo_2 = 0,Grupo_3 = 0,Grupo_4 = 0,Grupo_5 = 0,
                                              Grupo_6 = 0,Grupo_7 = 0,Grupo_8 = 0,Grupo_9 = 0,
                                              Grupo_10 = 0,Grupo_11 = 0,Grupo_12 = 0,Grupo_13 = 0,
                                              Grupo_14 = 0,Grupo_15 = 0,Grupo_16 = 0,Grupo_17 = 0,
                                              Grupo_18 = 0,Grupo_19 = 0,Grupo_20 = 0)
    
    ## Números de columna de la matriz "mat_real_grupos_una_materia"
    col_def_Materia <- arroja_ind_col_RG("Materia") ##1
    col_def_Horario <- arroja_ind_col_RG("Horario") ##2
    col_grupos_reales <- arroja_ind_col_RG("Grupos_Reales") ##3
    col_alum_real_total <- arroja_ind_col_RG("Alumnos_Reales_Totales") ##4
    col_1er_grupo <- arroja_ind_col_RG("col_1er_grupo") ##5
    col_ult_grupo <- arroja_ind_col_RG("col_ult_grupo") ##24
    rango_grupos <- col_1er_grupo:col_ult_grupo
    
    ##Se llena la columna auxiliar de horario:
    col_aux_hora <- 7:21
    
    ##Se llenan las columnas: "Materia" y "Horario"
    # El número 15 es porque hay 15 horarios en total (7-8,...,21-22)
    mat_real_grupos_una_materia[1:15,col_def_Materia] <- materia
    mat_real_grupos_una_materia[1:15,col_def_Horario] <- param$nombre_hrs
    
    ##Se llena la información de los grupos
    ind_materia <- checa_ind_materia(materia,m_grande)
    
    if(length(ind_materia) != 0){#Si hay grupos de "materia" en "sem_info"
      mat_materia <- m_grande[ind_materia,]
      for(d in 1:dim(mat_real_grupos_una_materia)[1]){##Recorre los renglones de "mat_real_grupos"
        # cat("\nd = ",d)
        for(h in 7:21){##Recorre las horas
          # cat("\n   h = ",h)
          mat_horario <- mat_materia[mat_materia[,num_col_horario_num]==h,]
          if(length(mat_horario)>=dim(m_grande)[2] && dim(mat_horario)[1] !=0){
            for(j in 1:dim(mat_horario)[1]){
              # cat("\n      j = ",j)
              if(mat_horario[j,num_col_horario_num] == col_aux_hora[d]){
                mat_real_grupos_una_materia[d,col_grupos_reales] <- dim(mat_horario)[1]
                mat_real_grupos_una_materia[d,(col_1er_grupo+j-1)] <- mat_horario[j,num_col_alum]
              }
            }##Fin de for(j)
          }
        }##Fin de for(h)
      }##Fin de for(d)
      
      ##Se llena la columna con el número de alumnos totales y se ordenan de
      ##mayor a menor los grupos con respecto al número de alumnos que se tienen
      for(k in 1:dim(mat_real_grupos_una_materia)[1]){
        renglon <- mat_real_grupos_una_materia[k,rango_grupos]
        mat_real_grupos_una_materia[k,col_alum_real_total] <- sum(renglon)
        
        # col_ult_gpo_sim <- col_alum_real_total+mat_real_grupos_una_materia[k,col_grupos_reales]
        # rango <- col_1er_grupo:col_ult_gpo_sim
        mat_real_grupos_una_materia[k,rango_grupos] <- sort(mat_real_grupos_una_materia[k,rango_grupos],decreasing = T)
      }
      # View(mat_real_grupos_una_materia)
    }else{#Si no hay grupos de "materia" en "sem_info"
      mat_real_grupos_una_materia <- data.frame(Materia = 0,Horario = 1:15,
                                                Núm.Gpos.Reales = 0,Núm.Al.Reales = 0,Grupo_1 = 0,
                                                Grupo_2 = 0,Grupo_3 = 0,Grupo_4 = 0,Grupo_5 = 0,
                                                Grupo_6 = 0,Grupo_7 = 0,Grupo_8 = 0,Grupo_9 = 0,
                                                Grupo_10 = 0,Grupo_11 = 0,Grupo_12 = 0,Grupo_13 = 0,
                                                Grupo_14 = 0,Grupo_15 = 0,Grupo_16 = 0,Grupo_17 = 0,
                                                Grupo_18 = 0,Grupo_19 = 0,Grupo_20 = 0)
      ##Se llenan las columnas: "Materia" y "Horario"
      # El número 15 es porque hay 15 horarios en total (7-8,...,21-22)
      mat_real_grupos_una_materia[1:15,col_def_Materia] <- materia
      mat_real_grupos_una_materia[1:15,col_def_Horario] <- param$nombre_hrs
    }
    return(mat_real_grupos_una_materia)
  }else{
    cat("\n ***ERROR*** \nEl espacio para guardar la información es insuficiente")
  }
}


# gen_mat_esp_alum_x_materia_1_sem ------------------------------------------
#' Title gen_mat_esp_alum_x_materia_1_sem: Función que genera una matriz con
#' 20 columnas y 15 renglones que contiene la esperanza por cada entrada de
#' las "n" matrices generadas en la función "gen_mat_n_sim_1_sem".
#'
#' @param lista_mat_n_sim: Lista que contiene las matrices generadas con
#' el número de alumnos simulados, ordenados de mayor a menor.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' 
#' @example lista_mat_n_sim[[5]][1,] <- c(12-13,54,32,31,6,1,0,...,0)
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
#' @return mat_esp_alum_x_materia_1_sem: Matriz de 15 renglones (horas) y
#' 20 columnas que contiene la esperanza por cada entrada de las "n" matrices
#' generadas en la función "gen_mat_n_sim_1_sem".
#'
gen_mat_esp_alum_x_materia_1_sem <- function(lista_mat_n_sim,param){
  n_sim <- length(names(lista_mat_n_sim))
  mat_esp_alum_x_materia_1_sem <- matrix(0,nrow = 15,ncol = 20)
  lista_aux <- list()
  num_col_Horario <- arroja_ind_col_SG("Horario")
  num_col_1er_grupo <- arroja_ind_col_SG("col_1er_grupo")
  num_col_ult_grupo <- arroja_ind_col_SG("col_ult_grupo")
  
  for(k in 1:n_sim){
    lista_aux[[k]] <- lista_mat_n_sim[[k]][,c(num_col_Horario,
                                              num_col_1er_grupo:num_col_ult_grupo)]
  }
  
  vec_info <- list()
  for(c in 2:21){
    # cat("\n c = ",c)
    for(r in 1:15){
      # cat("\n r = ",r)
      for(i in 1:n_sim){
        ##Se toma la entrada (r,c) del i-ésimo elemento de "lista_aux"
        vec_info[i] <- lista_aux[[i]][r,c]
      }
      mat_esp_alum_x_materia_1_sem[r,(c-1)] <- mean(sapply(vec_info, mean))
    }
  }
  rownames(mat_esp_alum_x_materia_1_sem) <- param$nombre_hrs
  colnames(mat_esp_alum_x_materia_1_sem) <- c("E[Gpo_1]","E[Gpo_2]","E[Gpo_3]",
                                              "E[Gpo_4]","E[Gpo_5]","E[Gpo_6]",
                                              "E[Gpo_7]","E[Gpo_8]","E[Gpo_9]",
                                              "E[Gpo_10]","E[Gpo_11]","E[Gpo_12]",
                                              "E[Gpo_13]","E[Gpo_14]","E[Gpo_15]",
                                              "E[Gpo_16]","E[Gpo_17]","E[Gpo_18]",
                                              "E[Gpo_19]","E[Gpo_20]")
  
  # save(mat_esp_alum_x_materia_1_sem,file = "mat_esp_alum_x_materia_1_sem_Modelos de Supervivencia y de Series de Tiempo.RData")
  return(mat_esp_alum_x_materia_1_sem)
}


# gen_mat_dif_alum_x_materia_1_sem ----------------------------------------------------
#' Title gen_mat_dif_alum_x_materia_1_sem: Función que genera una matriz con 20 columnas
#' y 15 renglones que contiene la diferencia entre los valores reales menos
#' la esperanza, para cada entrada, fijando materia y semestre.
#'
#' @param mat_esp_alum_x_materia_1_sem: Matriz de 15 renglones (horas) y
#' 20 columnas que contiene la esperanza por cada entrada de las "n" matrices
#' generadas en la función "gen_mat_n_sim_1_sem".
#' @example mat_esp_alum_x_materia_1_sem[6,] <- c(121.0,0.2,3.2,6.8,1.8,0,...,0)
#'
#' @return mat_dif_alum_x_materia_1_sem: Matriz de 15 renglones (horas) y 20 columnas
#' que contiene la diferencia entre los valores reales menos la esperanza,
#' para cada entrada.
#'
gen_mat_dif_alum_x_materia_1_sem <- function(mat_esp_alum_x_materia_1_sem,
                                             mat_real_grupos_una_materia){
  mat_dif_alum_x_materia_1_sem <- matrix(0,nrow = 15,ncol = 20)
  col_1er_grupo_real <- arroja_ind_col_RG("col_1er_grupo") ##5
  col_ult_grupo_real <- arroja_ind_col_RG("col_ult_grupo") ##24
  mat_real_aux <- mat_real_grupos_una_materia[,col_1er_grupo_real:col_ult_grupo_real]
  
  for(c in 1:dim(mat_dif_alum_x_materia_1_sem)[2]){
    # cat("\n c = ",c)
    for(r in 1:dim(mat_dif_alum_x_materia_1_sem)[1]){
      # cat("\n r = ",r)
      mat_dif_alum_x_materia_1_sem[r,c] <- mat_real_aux[r,c] - mat_esp_alum_x_materia_1_sem[r,c]
    }
  }
  rownames(mat_dif_alum_x_materia_1_sem) <- param$nombre_hrs
  colnames(mat_dif_alum_x_materia_1_sem) <- c("dif_Gpo_1","dif_Gpo_2","dif_Gpo_3",
                                              "dif_Gpo_4","dif_Gpo_5","dif_Gpo_6",
                                              "dif_Gpo_7","dif_Gpo_8","dif_Gpo_9",
                                              "dif_Gpo_10","dif_Gpo_11","dif_Gpo_12",
                                              "dif_Gpo_13","dif_Gpo_14","dif_Gpo_15",
                                              "dif_Gpo_16","dif_Gpo_17","dif_Gpo_18",
                                              "dif_Gpo_19","dif_Gpo_20")
  
  # View(mat_dif_alum_x_materia_1_sem)
  # save(mat_dif_alum_x_materia_1_sem,file = "mat_dif_alum_x_materia_1_sem_Modelos de Supervivencia y de Series de Tiempo.RData")
  return(mat_dif_alum_x_materia_1_sem)
}


# gen_mat_var_alum_x_materia_1_sem ------------------------------------------
#' Title gen_mat_var_alum_x_materia_1_sem: Función que genera una matriz con
#' 20 columnas que contiene la esperanza por cada entrada de las "n" matrices
#' generadas en la función "gen_mat_n_sim_1_sem".
#'
#' @param lista_mat_n_sim: Lista que contiene las matrices generadas con
#' el número de alumnos simulados, ordenados de mayor a menor.
#' @example lista_mat_n_sim[[5]][1,] <- c(12-13,54,32,31,6,1,0,...,0)
#'
#' @return mat_var_alum_x_materia_1_sem: Matriz de 15 renglones (horas) y
#' 20 columnas que contiene la esperanza por cada entrada de las "n" matrices
#' generadas en la función "gen_mat_n_sim_1_sem".
#'
gen_mat_var_alum_x_materia_1_sem <- function(lista_mat_n_sim,param){
  #Se definen las variables que se van a utilizar
  n_sim <- length(names(lista_mat_n_sim))
  mat_var_alum_x_materia_1_sem <- matrix(0,nrow = 15,ncol = 20)
  lista_aux <- list()
  num_col_Horario <- arroja_ind_col_SG("Horario")
  num_col_1er_grupo <- arroja_ind_col_SG("col_1er_grupo")
  num_col_ult_grupo <- arroja_ind_col_SG("col_ult_grupo")
  
  for(k in 1:n_sim){
    lista_aux[[k]] <- lista_mat_n_sim[[k]][,c(num_col_Horario,
                                              num_col_1er_grupo:num_col_ult_grupo)]
  }
  
  vec_info <- list()
  for(c in 2:21){
    # cat("\n c = ",c)
    for(r in 1:15){
      # cat("\n r = ",r)
      for(d in 1:n_sim){
        vec_info[d] <- lista_aux[[d]][r,c]
      }
      mat_var_alum_x_materia_1_sem[r,(c-1)] <- var(sapply(vec_info, mean))
    }
  }
  rownames(mat_var_alum_x_materia_1_sem) <- param$nombre_hrs
  colnames(mat_var_alum_x_materia_1_sem) <- c("Var(Gpo_1)","Var(Gpo_2)",
                                              "Var(Gpo_3)","Var(Gpo_4)",
                                              "Var(Gpo_5)","Var(Gpo_6)",
                                              "Var(Gpo_7)","Var(Gpo_8)",
                                              "Var(Gpo_9)","Var(Gpo_10)",
                                              "Var(Gpo_11)","Var(Gpo_12)",
                                              "Var(Gpo_13)","Var(Gpo_14)",
                                              "Var(Gpo_15)","Var(Gpo_16)",
                                              "Var(Gpo_17)","Var(Gpo_18)",
                                              "Var(Gpo_19)","Var(Gpo_20)")
  
  # save(mat_var_alum_x_materia_1_sem,file = "mat_var_alum_x_materia_1_sem_Estadística III.RData")
  # View(mat_var_alum_x_materia_1_sem)
  return(mat_var_alum_x_materia_1_sem)
}


# gen_vec_esp_datos_real_1_sem ------------------------------------------------
#' Title gen_vec_esp_datos_real_1_sem: Función en la que se genera el vector
#' "vec_suma_x_sem_real" el cual contiene la esperanza de los datos reales
#' por hora y por materia de 1 semestre.
#'
#' @param mat_real_grupos: Matriz con 24 columnas: Materia, Horario,
#' Número de grupos reales, Número de alumnos reales, las últimas 20
#' columnas indican el número de simulaciones del tamaño de grupo, en sus
#' renglones se tiene el número real de alumnos de cada grupo por hora.
#' @param nom_col: Nombre de la columna de datos que se requieren
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' 
#' @example mat_real_grupos[1,] <- c("Probabilidad I","9-10",15,150,50,...,0)
#' @example nom_col <- "Alumnos_Reales_Totales"
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
#' @return vec_suma_x_sem_real: Vector que contiene la suma de los datos
#' reales por hora y por materia de 1 semestre.
#'
gen_vec_esp_datos_real_1_sem <- function(mat_real_grupos,nom_col,param,param_sim){
  #Se definen las variables que se van a utilizar
  num_col_horario <- arroja_ind_col_RG("Horario") ##2
  num_col <- arroja_ind_col_RG(nom_col) ##4
  horas <- param$nombre_hrs
  Materias <- param_sim$Materias
  vec_suma_x_sem_real <- matrix(0,nrow = 15,ncol = 1)
  
  for(h in 1:length(horas)){#Recorre los horarios
    vec_aux <- mat_real_grupos[mat_real_grupos[,num_col_horario]==horas[h],num_col]
    if(length(vec_aux) > 0){
      vec_suma_x_sem_real[h] <- mean(vec_aux)
    }
  }
  
  return(vec_suma_x_sem_real)
}


# gen_vec_esp_datos_sim_1_sem ------------------------------------------------
#' Title gen_vec_esp_datos_sim_1_sem: Función en la que se genera el vector
#' "vec_suma_x_sem" el cual contiene la suma de los datos de las simulaciones
#' por hora y por materia de 1 semestre ya sea del número total de grupos
#' o del número total de alumnos.
#'
#' @param lista_n_sim_por_sem: Lista que contiene las listas de "n" simulaciones
#' por cada materia obtenidas de la función "gen_mat_n_sim_1_sem" 
#' @param nom_col: Nombre de la columna de la que se desea obtener la información.
#' 
#' @example lista_n_sim_por_sem: 
#' @example nom_col <- "Alumnos_Simulados_Totales"
#'
#' @return vec_suma_x_sem: Vector que contiene la suma de los datos de las
#' simulaciones por hora y por materia de 1 semestre.
#'
gen_vec_esp_datos_sim_1_sem <- function(lista_n_sim_por_sem,nom_col){
  #Se definen las variables que se van a utilizar
  num_col <- arroja_ind_col_SG(nom_col)
  vec_esp_x_sem <- matrix(0,nrow = 15,ncol = 1)
  
  for(m in 1:length(lista_n_sim_por_sem)){#Recorre las materias
    lista_aux <- list()
    lista_aux <- lista_n_sim_por_sem[[m]]##Lista con "n" simulaciones
    mat_aux <- matrix(0,nrow = 15,ncol = length(lista_aux))
    vec_info <- matrix(0,nrow = 15,ncol = 1)
    for(i in 1:length(lista_aux)){
      mat_aux[,i] <- lista_aux[[i]][,num_col]
    }##Fin for i
    
    ##Se obtiene la esperanza de los datos de las "n" simulaciones por hora
    vec_info <- rowMeans(mat_aux)
    vec_esp_x_sem <- vec_esp_x_sem + vec_info
    # cat("\n vec_esp_x_sem: ",vec_esp_x_sem)
  }##Fin for m
  
  # vec_esp_x_sem <- round(vec_esp_x_sem)### Verificar se usa "ceiling" o "round"
  vec_esp_x_sem <- ceiling(vec_esp_x_sem)### Se debe usar "ceiling"
  return(vec_esp_x_sem)
}


# gen_mat_dif_relativas ---------------------------------------------------
#' Title gen_mat_dif_relativas: Función que recibe dos matrices, una
#' matriz de diferencias absolutas y una matriz de valores reales, arroja
#' una matriz con las diferencias relativas correspondientes y guarda la
#' gráfica "heatmap" de ella.
#'
#' @param mat_dif_abs: Matriz de diferencias absolutas (valores reales
#' menos simulados).
#' @param mat_real: Matriz con valores reales por semestre.
#' @param nom_archivo : Nombre del archivo con el que se va a guardar la
#' gráfica "heatmap" de la matriz "mat_dif_relativas".
#'
#' @return mat_dif_relativas: Matriz con las diferencias relativas
#' correspondientes a las matrices que se pasan como parámetro.
#'
#' @example nom_archivo: "dif_relativa_total_de_alumnos_x_sem" o
#' "dif_relativa_total_de_gpos_x_sem"
#' @examples gen_mat_dif_relativas(mat_dif_abs,mat_real,nom_archivo)
#' 
gen_mat_dif_relativas <- function(mat_dif_abs,mat_real,nom_archivo){
  ##Se definen las variables que se van a utilizar:
  mat_dif_relativas <- matrix(0,nrow = nrow(mat_dif_abs),
                              ncol = ncol(mat_dif_abs))
  
  for(c in 1:dim(mat_dif_abs)[2]){#Recorre columnas
    for(r in 1:dim(mat_dif_abs)[1]){#Recorre renglones
      if(mat_real[r,c]!=0){
        mat_dif_relativas[r,c] <- mat_dif_abs[r,c]/mat_real[r,c]
      }else if(mat_real[r,c]==0 && mat_dif_abs[r,c]==0){
        mat_dif_relativas[r,c] <- 0
      }else if(mat_real[r,c]==0){
        #' En caso de que se la diferencia absoluta sea != 0, se
        #' define el cociente como infinito.
        mat_dif_relativas[r,c] <- Inf
      }
    }#Fin for(r)
  }#Fin for(c)
  
  ## Para guardar gráficas de R como imagen .jpeg se manda llamar la función
  ## jpeg() antes de generar una gráfica. Al hacer esto, le indicamos a R
  ##que en lugar de mandar nuestro gráfico a una ventana del escritorio, lo
  ##mande a un dispositivo gráfico distinto.
  ## La función dev.off(), se utiliza para cerrar el dispositivo gráfico
  ##elegido y así poder crear más gráficos después.
  nombre_archivo <- paste0("Figuras/Matrices Simuladas/heatmap_",nom_archivo,".jpeg")
  jpeg(filename = nombre_archivo, width = 800, height = 700)
  colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
  heatmap(mat_dif_relativas, Colv = NA, Rowv = NA, scale="none",
          col=colMain,main = nom_archivo)
  dev.off()
  
  nom_arch_matriz <- paste0("mat_dif_relativas/",nom_archivo,".RData")
  save(mat_dif_relativas,file = nom_arch_matriz)
  
  return(mat_dif_relativas)
}


# gen_mat_diferencias -----------------------------------------------------
#' Title gen_mat_diferencias: Función en la que se tiene la opción de elegir
#' si se desea obtener la matriz de diferencias relativas o de diferencias
#' absolutas.
#'
#' @param mat_sim_alum: Matriz con la esperanza de los valores simulados.
#' @param mat_real: Matriz con valores reales por semestre.
#' @param nom_archivo : Nombre del archivo con el que se va a guardar la
#' gráfica "heatmap" de la matriz "mat_dif_relativas".
#' @param abs_0_rel_1: Variable binaria que vale 1 si se desea la matriz
#' de diferencias relativas y cero si se desea la matriz de diferencias
#' absolutas.
#'
#' @return mat_diferencias: Matriz de 15 renglones (horas) con la 
#' infomración de las diferencias relativas o absolutas.
#'
#' @examples
#' gen_mat_diferencias(mat_sim_alum,mat_real,nom_archivo,1)
#' gen_mat_diferencias(mat_sim_alum,mat_real,nom_archivo,0)
#' 
gen_mat_diferencias <- function(mat_sim_alum,mat_real,nom_archivo,
                                abs_0_rel_1){
  ##Se definen las variables que se van a utilizar:
  mat_dif_abs <- mat_real - mat_sim_alum
  
  if(abs_0_rel_1 == 0){
    mat_diferencias <- mat_dif_abs
  }else{
    mat_diferencias <- gen_mat_dif_relativas(mat_dif_abs,mat_real,nom_archivo)
    gen_mat_dif_relativas(mat_sim_alum,mat_real,nom_archivo)
  }
  return(mat_diferencias)
}


# gen_vec_esp_dat_sim_1_sem_1_materia ------------------------------------------------
#' Title gen_vec_esp_dat_sim_1_sem_1_materia: Función en la que se genera el vector
#' "vec_suma_x_sem" el cual contiene la suma de los datos de las simulaciones
#' por hora y por materia de 1 semestre ya sea del número total de grupos
#' o del número total de alumnos.
#'
#' @param lista_mat_n_sim: Lista que contiene las "n" simulaciones de una materia.
#' @param nom_col: Nombre de la columna de la que se desea obtener la información.
#' 
#' @example lista_mat_n_sim: 
#' @example nom_col <- "Alumnos_Simulados_Totales" o "Grupos_Simulados"
#'
#' @return vec_suma_x_sem: Vector que contiene la suma de los datos de las
#' simulaciones por hora de 1 materia y 1 semestre.
#'
gen_vec_esp_dat_sim_1_sem_1_materia <- function(lista_mat_n_sim,nom_col){
  #Se definen las variables que se van a utilizar
  num_col <- arroja_ind_col_SG(nom_col)
  vec_esp_x_sem <- matrix(0,nrow = 15,ncol = 1)
  
  mat_aux <- matrix(0,nrow = 15,ncol = length(lista_mat_n_sim))
  vec_info <- matrix(0,nrow = 15,ncol = 1)
  for(i in 1:length(lista_mat_n_sim)){
    mat_aux[,i] <- lista_mat_n_sim[[i]][,num_col]
  }##Fin for i
  
  ##Se obtiene la esperanza de los datos de las "n" simulaciones por hora
  vec_info <- rowMeans(mat_aux)
  vec_esp_x_sem <- vec_esp_x_sem + vec_info
  # cat("\n vec_esp_x_sem: ",vec_esp_x_sem)
  vec_esp_x_sem <- ceiling(vec_esp_x_sem)### Se debe usar "ceiling"
  return(vec_esp_x_sem)
}



# pruebas_aleatorias_1_materia --------------------------------------------
#' Title pruebas_aleatorias_1_materia: Función que manda a llamar a la
#' función encargada de realizar las pruebas rápidas para una materia.
#'
#' @param materia: Nombre de algún curso impartido en la FC.
#' @param k_sem_ant: Número de semestres de información se quieren para
#' la simulación.
#' @param num_sim: Número de matrices simuladas.
#' @param num_seg_sig: Número de semestres de los que se van a obtener las
#' simulaciones.
#' @param cont_1si_0no: Variable binaria que vale 1 si los semestres a 
#' simular son continuos y 0 sino.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' 
#' @example materia <- "Estadística III"
#' @example k_sem_ant <- 5
#' @example num_sim <- 10
#' @example num_seg_sig <- 3
#' @example cont_1si_0no <- 1 ó 0
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' 
pruebas_aleatorias_1_materia <- function(materia,k_sem_ant,num_sim,
                                         num_seg_sig,cont_1si_0no,param){
  #Se definen las variables que se van a utilizar
  vec_sem_aux <- param$sem_totales[15:(length(param$sem_totales)-1)]#20151 - 20201
  if(cont_1si_0no == 1){
    vec_ind_aux <- 1:(length(vec_sem_aux)-num_seg_sig)
    ind_1 <- sample(vec_ind_aux,1)
    vec_sem_sig <- vec_sem_aux[ind_1:(ind_1+num_seg_sig-1)]
  }else{
    vec_sem_sig <- sort(sample(vec_sem_aux,num_seg_sig))
  }
  print(vec_sem_sig)
  vec_k_sem_info <- rep(k_sem_ant,length(vec_sem_sig))
  guarda_heatmap_1_materia_n_sem(vec_sem_sig,vec_k_sem_info,materia,num_sim,param)
}



# pruebas_aleatorias ------------------------------------------------------
#' Title pruebas_aleatorias: Función que manda a llamar "n_materias" veces
#' a la función "pruebas_aleatorias_1_materia".
#'
#' @param n_materias: Número de materias de las cuales se van a obtener las
#' pruebas rápidas.
#' @param materias_con_error: Vector con los índices de las materias que
#' tienen algún error, por el momento no se toman en cuenta ya que se quieren
#' obtener resultados rápidos y no detenerse en los casos especiales. Se notó
#' que las materias con error son optativas y casi sin alumnos.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' 
#' @example n_materias <- 5
#' @example materias_con_error <- c(127,168,195,198,211,215,216,223,237,253)
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total) 
#'
pruebas_aleatorias <- function(n_materias,materias_con_error,param){
  #Se definen las variables que se van a utilizar
  vec_aux <- 1:length(param$vec_nom_materias_total)
  vec_ind_aux <- vec_aux[-materias_con_error]
  ind_materias <- sample(vec_ind_aux,n_materias)
  
  i <- 1
  for(d in ind_materias){
    cat("\nPRUEBA: ",i)
    # Start the clock!
    ptm <- proc.time()
    #Se definen las variables aleatorias que se van a utilizar
    k_sem_ant <- sample(5:10,1)
    num_sim <- sample(10:15,1)
    num_seg_sig <- sample(3:5,1)
    cont_1si_0no <- sample(0:1,1)
    
    materia <- param$vec_nom_materias_total[d]
    cat("\n Materia: ",d," = ",materia)
    pruebas_aleatorias_1_materia(materia,k_sem_ant,num_sim,
                                 num_seg_sig,cont_1si_0no,param)
    
    #Se imprime la información
    cat("\nSe probaron ",1," materias")
    cat("\n",num_seg_sig," semestres")
    cat("\n",k_sem_ant," semestres de información")
    cat("\n CON m_filtrada")
    cat("\n",num_sim," simulaciones por materia")
    
    # Stop the clock
    minutos <- (proc.time()-ptm)[3]/60
    cat("\nEl proceso tardó: ",minutos," minutos\n")
    
    i <- i + 1
  }
  
  # #Se imprime la información
  # cat("\nSe probaron ",n_materias," materias")
  # cat("\n",num_seg_sig," semestres")
  # cat("\n",k_sem_ant," semestres de información")
  # cat("\n CON m_filtrada")
  # cat("\n",num_sim," simulaciones por materia")
  # 
  # # Stop the clock
  # minutos <- (proc.time()-ptm)[3]/60
  # cat("\nEl proceso tardó: ",minutos," minutos\n")
}




##########################################################################
##### HEATMAP #####
#'Funciones encargadas de graficar las matrices con heatmaps y guardar
#'las imágenes.
##########################################################################

# guarda_una_fig_heatmap --------------------------------------------------
#' Title guarda_una_fig_heatmap: Función que genera una imagen de tipo "jpeg"
#' de las gráficas de heatmap para las matrices de datos generadas.
#'
#' @param matriz: Matriz de la cual se obtiene la gráfica de tipo heatmap
#' y se guarda la imagen "jpeg". La matriz de datos puede contener la 
#' diferencia entre los datos reales menos la esperanza de los simulados,
#' la varianza de los datos simulados, ...
#' @param num_materia: Índice de la materia de la cual se quiere obtener
#' la gráfica de tipo heatmap, del vector "Materias".
#' @param nom_archivo: Nombre con el que se guardará la imagen "jpeg".
#' 
#' @example matriz[6,] <- c(121.0,0.2,3.2,6.8,1.8,0,...,0)
#' @example num_materia <- 5
#' @example nom_archivo <- "Figuras/Matrices Simuladas/heatmap_var_materia_5_sem_20201.jpeg"
#'
#' @export jpeg: Imagen de las gráficas de heatmap obtenidas de alguna matriz.
#'
guarda_una_fig_heatmap <- function(matriz,num_materia,nom_archivo){
  load(file = "vec_nom_materias_total.RData")
  
  ## Para guardar gráficas de R como imagen .jpeg se manda llamar la función
  ## jpeg() antes de generar una gráfica. Al hacer esto, le indicamos a R
  ##que en lugar de mandar nuestro gráfico a una ventana del escritorio, lo
  ##mande a un dispositivo gráfico distinto.
  ## La función dev.off(), se utiliza para cerrar el dispositivo gráfico
  ##elegido y así poder crear más gráficos después.
  jpeg(filename = nom_archivo, width = 800, height = 700)
  colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
  heatmap(matriz, Colv = NA, Rowv = NA, scale="none",col=colMain,
          main = vec_nom_materias_total[num_materia])
  dev.off()
}


# guarda_heatmap_1_materia_1_sem ------------------------------------------
#' Title guarda_heatmap_1_materia_1_sem: Función que guarda las imágenes de
#' tipo "jpeg" de las gráficas heatmap para las pruebas hechas para un
#' semestre.
#'
#' @param lista_mat_n_sim: Lista que contiene las matrices generadas con
#' el número de alumnos simulados, ordenados de mayor a menor.
#' @param materia: Nombre de algún curso impartido en la FC.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' 
#' @example lista_mat_n_sim[[5]][1,] <- c(12-13,54,32,31,6,1,0,...,0)
#' @example materia <- "Estadística III"
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' 
guarda_heatmap_1_materia_1_sem <- function(lista_mat_n_sim,materia,sem_sig,param){
  #Se definen las variables que se van a utilizar
  num_materia <- arroja_num_materia(materia,param)
  num_col_1er_grupo <- arroja_ind_col_RG("col_1er_grupo") ##5
  num_col_ult_grupo <- arroja_ind_col_RG("col_ult_grupo") ##24
  
  #Se guardan las figuras correspondientes a la varianza
  mat_var_alum_x_materia_1_sem <- gen_mat_var_alum_x_materia_1_sem(lista_mat_n_sim,param)
  nom_heatmap_var <- gen_nom_heatmap("var_alum_materia",num_materia,sem_sig)
  guarda_una_fig_heatmap(mat_var_alum_x_materia_1_sem,num_materia,nom_heatmap_var)
  guarda_una_fig_heatmap(mat_var_alum_x_materia_1_sem,num_materia,nom_heatmap_var)
  
  # Se guardan las figuras correspondientes a la diferencia entre valores reales
  #y la esperanza.
  mat_esp_alum_x_materia_1_sem <- gen_mat_esp_alum_x_materia_1_sem(lista_mat_n_sim,param)
  mat_real_grupos_una_materia <- gen_mat_real_grupos_una_materia(materia,sem_sig,param)
  mat_dif_alum_x_materia_1_sem <- gen_mat_dif_alum_x_materia_1_sem(mat_esp_alum_x_materia_1_sem,
                                                                   mat_real_grupos_una_materia)
  
  mat_real_aux <- mat_real_grupos_una_materia[,num_col_1er_grupo:num_col_ult_grupo]
  mat_real_aux <- as.matrix(mat_real_aux,nrow=dim(mat_real_aux)[1],ncol=dim(mat_real_aux)[1])
  mat_dif_relativa_alum_x_materia_1_sem <- matrix(0,nrow = dim(mat_real_aux)[1],
                                                  ncol = dim(mat_real_aux)[2])
  if(sum(as.numeric(mat_real_aux)) != 0){###SUMAR RENGLONES Y COLS
    #Se hace la división en caso de que la matriz de datos reales sea
    #distinta de cero en todas sus entradas
    for(c in 1:dim(mat_real_aux)[2]){#Recorre columnas
      for(r in 1:dim(mat_real_aux)[1]){#Recorre renglones
        if(mat_real_aux[r,c]==0){
          #Esn caso de que el valor real sea cero, no se hace la división
          mat_dif_relativa_alum_x_materia_1_sem[r,c] <- 0
        }else{
          mat_dif_relativa_alum_x_materia_1_sem[r,c] <- mat_dif_alum_x_materia_1_sem[r,c]/mat_real_aux[r,c]
        }}#for r
    }#for c
  }else{
    #En caso de que toda la matriz de valores reales sea cero
    mat_dif_relativa_alum_x_materia_1_sem <- mat_real_aux
  }
  nom_heatmap_dif_rel <- gen_nom_heatmap("dif_relativa_alum_materia",num_materia,sem_sig)
  guarda_una_fig_heatmap(mat_dif_relativa_alum_x_materia_1_sem,num_materia,nom_heatmap_dif_rel)
}


# guarda_heatmap_1_materia_n_sem ------------------------------------------
#' Title guarda_heatmap_1_materia_n_sem: Función que guarda todas las
#' imágenes de tipo "jpeg" de las gráficas heatmap para las pruebas hechas.
#'
#' @param vec_sem_sig: Vector con los semestres de los que se desean obtener
#' las simulaciones. Deben estar ordenados del más antiguo al más reciente.
#' @param vec_k_sem_info: Vector con el número de semestres anteriores para
#' cada "sem_sig" del vector "vec_sem_sig"
#' @param materia: Nombre de algún curso impartido en la FC.
#' @param num_sim: Número de matrices simuladas.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#'
#' @example vec_sem_sig <- c(20131,20152,20182,20201)
#' @example vec_k_sem_info <- c(10,15,21,24)##Se inicia en 2008-1
#' @example materia <- "Estadística III"
#' @example num_sim <- 10
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' 
guarda_heatmap_1_materia_n_sem <- function(vec_sem_sig,vec_k_sem_info,materia,
                                           num_sim,param){
  #Se definen las variables que se van a utilizar
  num_materia <- arroja_num_materia(materia,param)
  nom_col_sim_alum <- "Alumnos_Simulados_Totales"
  nom_col_sim_gpos <- "Grupos_Simulados"
  nom_col_real_alum <- "Alumnos_Reales_Totales"
  nom_col_real_gpos <- "Grupos_Reales"
  
  mat_aux_real_alum <- matrix(0,nrow = 15,ncol = length(vec_sem_sig))
  mat_aux_real_gpos <- matrix(0,nrow = 15,ncol = length(vec_sem_sig))
  mat_aux_sim_alum <- matrix(0,nrow = 15,ncol = length(vec_sem_sig))
  mat_aux_sim_gpos <- matrix(0,nrow = 15,ncol = length(vec_sem_sig))
  
  mat_dif_total_alumnos_x_sem <- matrix(0,nrow = 15,ncol = length(vec_sem_sig))
  mat_dif_total_gpos_x_sem <- matrix(0,nrow = 15,ncol = length(vec_sem_sig))
  
  #Se generan los archivos de la simulación
  gen_list_n_sim_1_mat_n_sem(vec_sem_sig,vec_k_sem_info,materia,num_sim,param)
  
  for(s in 1:length(vec_sem_sig)){#Recorre los semestres
    sem_sig <- vec_sem_sig[s]
    nom_lista_n_sim <- gen_nom_list_n_sim_1_materia(num_sim,num_materia,sem_sig)
    load(nom_lista_n_sim)
    
    #Se guardan las gráficas de una materia
    guarda_heatmap_1_materia_1_sem(lista_mat_n_sim,materia,sem_sig,param)
    
    ##Se obtienen las matrices que contienen los datos de alumnos totales
    ##reales por semestre
    nom_archivo_real <- paste0("mat_real_grupos por semestre/mat_real_grupos_",
                               vec_sem_sig[s],".RData")
    load(nom_archivo_real)
    mat_aux_real_alum[,s] <- gen_vec_esp_datos_real_1_sem(mat_real_grupos,
                                                           nom_col_real_alum,
                                                           param)
    mat_aux_real_gpos[,s] <- gen_vec_esp_datos_real_1_sem(mat_real_grupos,
                                                           nom_col_real_gpos,
                                                           param)
    mat_aux_sim_alum[,s] <- gen_vec_esp_dat_sim_1_sem_1_materia(lista_mat_n_sim,
                                                                nom_col_sim_alum)
    mat_aux_sim_gpos[,s] <- gen_vec_esp_dat_sim_1_sem_1_materia(lista_mat_n_sim,
                                                                nom_col_sim_gpos)
  }#Fin for(s)
  rownames(mat_aux_real_alum) <- param$nombre_hrs
  colnames(mat_aux_real_alum) <- vec_sem_sig
  rownames(mat_aux_real_gpos) <- param$nombre_hrs
  colnames(mat_aux_real_gpos) <- vec_sem_sig
  rownames(mat_aux_sim_alum) <- param$nombre_hrs
  colnames(mat_aux_sim_alum) <- vec_sem_sig
  rownames(mat_aux_sim_gpos) <- param$nombre_hrs
  colnames(mat_aux_sim_gpos) <- vec_sem_sig
  
  #Se cargan las matrices con las diferencias de datos
  mat_dif_total_alumnos_x_sem <- mat_aux_real_alum - mat_aux_sim_alum
  rownames(mat_dif_total_alumnos_x_sem) <- param$nombre_hrs
  colnames(mat_dif_total_alumnos_x_sem) <- vec_sem_sig
  
  mat_dif_total_gpos_x_sem <- mat_aux_real_gpos - mat_aux_sim_gpos
  rownames(mat_dif_total_gpos_x_sem) <- param$nombre_hrs
  colnames(mat_dif_total_gpos_x_sem) <- vec_sem_sig
  
  
  nom_archivo <- paste0("dif_relativa_total_de_alumnos_x_sem_",
                        vec_sem_sig[1],"-",vec_sem_sig[length(vec_sem_sig)],
                        "_materia_",num_materia)
  mat_dif_relativas_alum <- gen_mat_dif_relativas(mat_dif_total_alumnos_x_sem,
                                                  mat_aux_real_alum,nom_archivo)
  colnames(mat_dif_relativas_alum) <- vec_sem_sig
  rownames(mat_dif_relativas_alum) <- param$nombre_hrs
  # View(mat_dif_relativas_alum)
  
  nom_archivo <- paste0("dif_relativa_total_de_gpos_x_sem_",
                        vec_sem_sig[1],"-",vec_sem_sig[length(vec_sem_sig)],
                        "_materia_",num_materia)
  mat_dif_relativas_gpos <- gen_mat_dif_relativas(mat_dif_total_gpos_x_sem,
                                                  mat_aux_real_gpos,nom_archivo)
  colnames(mat_dif_relativas_gpos) <- vec_sem_sig
  rownames(mat_dif_relativas_gpos) <- param$nombre_hrs
  # View(mat_dif_relativas_gpos)
}


##########################################################################
##### MEDIDAS DE DISPERSIÓN #####
#' Funciones que generan matrices con la información de las medidas de
#' dispersión que ayudan a decidir q1 y q2
##########################################################################


# gen_mat_min_mean_max_var ----------------------------------------------------
#' Title gen_mat_min_mean_max_var: Función que arroja y guarda una matriz de 6
#' columnas (Materia, mín, media, máx, var, segundos), en el renglón i se
#' tienen los datos de la matriz de diferencias relativas de i-ésima materia.
#'
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @param param_sim: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se encargan de la simulación.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' @example param_sim <- list(vec_sem_sig = c(20191,20192,20201),k_sem_ant = 5,
#' materia = "Estadística III", num_sim = 10, m_filtrada = matrix(0),
#' sub_m_filtrada = matrix(0,ncol = length(param$nom_cols_MG)))
#'
#' @return mat_min_mean_max_var: Matriz de 6 columnas (Materia,mín,
#' media, máx, var, segundos), en el renglón i se tienen los datos
#' de la matriz de diferencias relativas de i-ésima materia.
#'
#' @examples
#' mat_min_mean_max_var <- gen_mat_min_mean_max_var(param,param_sim)
#' 
gen_mat_min_mean_max_var <- function(param,param_sim){
  ##Se definen las variables que se van a utilizar:
  vec_nom_materias_total <- param$vec_nom_materias_total
  mat_min_mean_max_var <- data.frame(Materia = vec_nom_materias_total,Min = 0,
                                     Media = 0,Max = 0,Var = 0,Seg = 0)
  ##El tiempo está medido en segundos
  for(d in 1:length(vec_nom_materias_total)){
    materia <- vec_nom_materias_total[d]
    cat("\n Materia ",d,": ",materia)
    param_sim$Materias = materia
    m_filtrada <- gen_mat_m_filtrada(param,param_sim)
    param_sim$m_filtrada = m_filtrada
    
    # Start the clock!
    ptm <- proc.time()
    if(dim(m_filtrada)[1] == 0){
      tiempo <- (proc.time()-ptm)[3]
      mat_min_mean_max_var[d,2:5] <- 0
      mat_min_mean_max_var[d,6] <- tiempo
    }else{
      lista_mat_pruebas <- pruebas_num_alum_total(param,param_sim)
      tiempo <- (proc.time()-ptm)[3]
      mat_min_mean_max_var[d,2] <- min(lista_mat_pruebas[[1]]
                                       [!(lista_mat_pruebas[[1]]==Inf)])
      mat_min_mean_max_var[d,3] <- mean(lista_mat_pruebas[[1]]
                                        [!(lista_mat_pruebas[[1]]==Inf)])
      mat_min_mean_max_var[d,4] <- max(lista_mat_pruebas[[1]]
                                       [!(lista_mat_pruebas[[1]]==Inf)])
      mat_min_mean_max_var[d,5] <- var(lista_mat_pruebas[[1]]
                                       [!(lista_mat_pruebas[[1]]==Inf)])
      mat_min_mean_max_var[d,6] <- tiempo
    }
  }##Fin for(d)
  nom_archivo <- paste0("mat_min_mean_max_var/mat_min_mean_max_var_",
                        param_sim$vec_sem_sig[1],"-",
                        param_sim$vec_sem_sig[length(param_sim$vec_sem_sig)],
                        "_q1_",param$q1,"_q2_",param$q2,".RData")
  save(mat_min_mean_max_var,file = nom_archivo)
  return(mat_min_mean_max_var)
}


# gen_mat_min_mean_max_var_m_materias -------------------------------------
#' Title gen_mat_min_mean_max_var_m_materias: Función que arroja y guarda
#' una matriz de 6 columnas (Materia, mín, media, máx, var, segundos), en
#' el renglón i se tienen los datos de la matriz de diferencias relativas
#' de i-ésima materia. Para un subconjunto de "vec_nom_materias_total".
#'
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
#' @return mat_min_mean_max_var_m_materias: Matriz de 6 columnas
#' (Materia,mín,media,máx,var,segundos), en el renglón i se tienen los datos
#' de la matriz de diferencias relativas de i-ésima materia.
#'
#' @examples
#' gen_mat_min_mean_max_var_m_materias(param,param_sim)
#' 
gen_mat_min_mean_max_var_m_materias <- function(param,param_sim){
  ##Se definen las variables que se van a utilizar:
  Materias <- param_sim$Materias
  mat_min_mean_max_var_m_materias <- data.frame(Materia = Materias,Min = 0,
                                                Media = 0,Max = 0,Var = 0,Seg = 0)
  ##El tiempo está medido en segundos
  for(d in 1:length(Materias)){
    materia <- Materias[d]
    cat("\n Materia ",d,": ",materia)
    param_sim$Materias = materia
    m_filtrada <- gen_mat_m_filtrada(param,param_sim)
    param_sim$m_filtrada = m_filtrada
    
    # Start the clock!
    ptm <- proc.time()
    if(dim(m_filtrada)[1] == 0){
      tiempo <- (proc.time()-ptm)[3]
      mat_min_mean_max_var_m_materias[d,2:5] <- 0
      mat_min_mean_max_var_m_materias[d,6] <- tiempo
    }else{
      lista_mat_pruebas <- pruebas_num_alum_total(param,param_sim)
      tiempo <- (proc.time()-ptm)[3]
      mat_min_mean_max_var_m_materias[d,2] <- min(lista_mat_pruebas[[1]]
                                                  [!(lista_mat_pruebas[[1]]==Inf)])
      mat_min_mean_max_var_m_materias[d,3] <- mean(lista_mat_pruebas[[1]]
                                                   [!(lista_mat_pruebas[[1]]==Inf)])
      mat_min_mean_max_var_m_materias[d,4] <- max(lista_mat_pruebas[[1]]
                                                  [!(lista_mat_pruebas[[1]]==Inf)])
      mat_min_mean_max_var_m_materias[d,5] <- var(lista_mat_pruebas[[1]]
                                                  [!(lista_mat_pruebas[[1]]==Inf)])
      mat_min_mean_max_var_m_materias[d,6] <- tiempo
    }
  }##Fin for(d)
  nom_archivo <- paste0("mat_min_mean_max_var/mat_min_mean_max_var_",
                        length(Materias),"_materias_",
                        param_sim$vec_sem_sig[1],"-",
                        param_sim$vec_sem_sig[length(param_sim$vec_sem_sig)],
                        "_q1_",param$q1,"_q2_",param$q2,".RData")
  save(mat_min_mean_max_var_m_materias,file = nom_archivo)
  return(mat_min_mean_max_var_m_materias)
}


# imprime_tiempo ----------------------------------------------------------
#' Title imprime_tiempo: Función que imprime el tiempo que se tardó la
#' función "gen_mat_med_dispersion" para crear la matriz "mat_med_dispersion".
#' También se imprime la información del número de pruebas hechas y el número
#' de materias que se están probando.
#'
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
#' @return tabla_prom_sd: Matriz de 2 columnas (Intervalo,Promedio_sd),
#' con la información del promedio de la sd para cada intervalo de
#' confianza.
#'
#' @examples
#' imprime_tiempo(param,param_sim)
#' 
imprime_tiempo <- function(param,param_sim){
  ptm <- proc.time()# Start the clock!
  mat_med_dispersion <- gen_mat_med_dispersion(param,param_sim)
  View(mat_med_dispersion)
  tabla_prom_sd <- as.data.frame(matrix(0,nrow = dim(param_sim$posibles_comb_q)[1],
                                        ncol = 2))
  # tabla_prom_sd <- data.frame()
  cat("\nLa función gen_mat_med_dispersion tardó: ",(proc.time()-ptm)[3]/60,
      " minutos\n")
  cat("\nSe hicieron ",dim(param_sim$posibles_comb_q)[1]," pruebas")
  cat("\nNúmero de materias = ",length(param_sim$Materias))
  
  # cat("\n\n Los promedios de la sd para cada intervalo son:")
  for(d in 1:dim(param_sim$posibles_comb_q)[1]){
    intervalo <- paste0("L",param_sim$posibles_comb_q[d,1],",","U",
                        param_sim$posibles_comb_q[d,2])
    promedio <- mean(mat_med_dispersion[mat_med_dispersion[,2]==intervalo,6])
    # cat("\n Intervalo: ",intervalo," Promedio = ",promedio)
    # cat("\n",intervalo," -> ",promedio)
    tabla_prom_sd[d,1] <- intervalo
    tabla_prom_sd[d,2] <- promedio
  }
  colnames(tabla_prom_sd) <- c("Intervalo","Promedio_sd")
  return(tabla_prom_sd)
}


# gen_mat_med_disp_1_materia_1_prueba -------------------------------------
#' Title gen_mat_med_disp_1_materia_1_prueba: Función que arroja una matriz
#' de 7 columnas (Materia, Intervalo, mín, media, máx, sd, segundos). Se
#' tienen los datos de la matriz de diferencias relativas de "materia".
#'
#' @param materia: Nombre de algún curso impartido en la FC.
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
#' @return mat_med_disp_1_materia_1_prueba: Matriz de 7 columnas
#' (Materia,Intervalo,mín,media,máx,var,segundos), en el renglón i se
#' tienen los datos de la matriz de diferencias relativas de la
#' i-ésima materia.
#'
#' @examples
#' gen_mat_med_disp_1_materia_1_prueba(materia,param,param_sim)
#' 
gen_mat_med_disp_1_materia_1_prueba <- function(materia,param,param_sim){
  ##Se definen las variables que se van a utilizar:
  intervalo <- paste0("L",param$q1,",U",param$q2)
  mat_med_disp_1_materia_1_prueba <- data.frame(Materia = materia,
                                                Intervalo = intervalo,
                                                Min = 0,Media = 0,Max = 0,
                                                sd = 0,Seg = 0)
  param_sim$Materias = materia
  m_filtrada <- gen_mat_m_filtrada(param,param_sim)
  param_sim$m_filtrada = m_filtrada
  
  # Start the clock!
  ptm <- proc.time()
  if(dim(m_filtrada)[1] == 0){
    tiempo <- (proc.time()-ptm)[3]
    mat_med_disp_1_materia_1_prueba[1,3:6] <- 0
    mat_med_disp_1_materia_1_prueba[1,7] <- tiempo
  }else{
    lista_mat_pruebas <- pruebas_num_alum_total(param,param_sim)
    tiempo <- (proc.time()-ptm)[3]
    mat_med_disp_1_materia_1_prueba[1,3] <- min(lista_mat_pruebas[[1]]
                                                [!(lista_mat_pruebas[[1]]==Inf)])
    mat_med_disp_1_materia_1_prueba[1,4] <- mean(lista_mat_pruebas[[1]]
                                                 [!(lista_mat_pruebas[[1]]==Inf)])
    mat_med_disp_1_materia_1_prueba[1,5] <- max(lista_mat_pruebas[[1]]
                                                [!(lista_mat_pruebas[[1]]==Inf)])
    mat_med_disp_1_materia_1_prueba[1,6] <- sd(lista_mat_pruebas[[1]]
                                               [!(lista_mat_pruebas[[1]]==Inf)])
    mat_med_disp_1_materia_1_prueba[1,7] <- tiempo
  }
  return(mat_med_disp_1_materia_1_prueba)
}


# gen_mat_med_disp_1_materia_n_pruebas ------------------------------------
#' Title gen_mat_med_disp_1_materia_n_pruebas: Función que arroja y guarda
#' una matriz de 7 columnas (Materia, Intervalo, mín, media, máx, sd,
#' segundos), en el renglón i se tienen los datos de la matriz de diferencias
#' relativas de "materia" para cada prueba de q1 y q2.
#'
#' @param materia: Nombre de algún curso impartido en la FC.
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
#' @return mat_med_disp_1_materia_n_pruebas: Matriz de 7 columnas (Materia,
#' Intervalo, mín, media, máx, sd,segundos), con los datos de la matriz de
#' diferencias relativas de "materia" para cada prueba de q1 y q2.
#'
#' @examples
#' gen_mat_med_disp_1_materia_n_pruebas(materia,param,param_sim)
#' 
gen_mat_med_disp_1_materia_n_pruebas <- function(materia,param,param_sim){
  ##Se definen las variables que se van a utilizar:
  num_materia <- arroja_num_materia(materia,param)
  posibles_comb <- param_sim$posibles_comb_q
  mat_med_disp_1_materia_n_pruebas <- data.frame(Materia = 0,
                                                 Intervalo = 0,
                                                 Min = 0,Media = 0,Max = 0,
                                                 sd = 0,Seg = 0)
  
  for(r in 1:dim(posibles_comb)[1]){#Recorre las combinaciones de q1 y q2
    # cat("\n r = ",r," de ",dim(posibles_comb)[1])
    param$q1 = posibles_comb[r,1]
    param$q2 = posibles_comb[r,2]
    mat_med_disp_1_materia_1_prueba <- gen_mat_med_disp_1_materia_1_prueba(materia,
                                                                           param,
                                                                           param_sim)
    mat_med_disp_1_materia_n_pruebas <- rbind(mat_med_disp_1_materia_n_pruebas,
                                              mat_med_disp_1_materia_1_prueba)
  }
  ##Se quita el renglón de ceros:
  mat_med_disp_1_materia_n_pruebas <- mat_med_disp_1_materia_n_pruebas[
    mat_med_disp_1_materia_n_pruebas[,1]!=0,]
  
  nom_archivo <- paste0("mat_min_mean_max_sd/mat_med_disp_materia_",
                        num_materia,"_",param_sim$vec_sem_sig[1],"-",
                        param_sim$vec_sem_sig[length(param_sim$vec_sem_sig)],
                        ".RData")
  save(mat_med_disp_1_materia_1_prueba,file = nom_archivo)
  
  return(mat_med_disp_1_materia_n_pruebas)
}


# gen_mat_med_dispersion --------------------------------------------------
#' Title gen_mat_med_dispersion: Función que arroja una matriz de 7 columnas
#' (Materia, Intervalo, mín, media, máx, sd, segundos), en el renglón i se
#' tienen los datos de la matriz de diferencias relativas de la i-ésima
#' materia para cada prueba de q1 y q2.
#'
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
#' @return mat_med_dispersion: Matriz de 7 columnas (Materia, Intervalo,
#' mín, media, máx, sd, segundos), en el renglón i se tienen los datos de
#' la matriz de diferencias relativas de la i-ésima materia para cada prueba
#' de q1 y q2.
#'
#' @examples
#' mat_med_dispersion <- gen_mat_med_dispersion(param,param_sim)
#' 
gen_mat_med_dispersion <- function(param,param_sim){
  ##Se definen las variables que se van a utilizar:
  Materias <- param_sim$Materias
  mat_med_dispersion <- data.frame(Materia = 0,
                                   Intervalo = 0,
                                   Min = 0,Media = 0,Max = 0,
                                   sd = 0,Seg = 0)
  
  for(m in 1:length(Materias)){
    materia <- Materias[m]
    cat("\n Materia: ",materia)
    mat_med_disp_1_materia_n_pruebas <- gen_mat_med_disp_1_materia_n_pruebas(materia,
                                                                             param,
                                                                             param_sim)
    mat_med_dispersion <- rbind(mat_med_dispersion,
                                mat_med_disp_1_materia_n_pruebas)
  }
  ##Se quita el renglón de ceros:
  mat_med_dispersion <- mat_med_dispersion[mat_med_dispersion[,1]!=0,]
  
  return(mat_med_dispersion)
}








##### **AQUÍ** #####


##########################################################################
##### ESQUELETO #####
## Funciones que generan un esqueleto del siguiente semestre. Arroja un
##error en caso de no encontrar algún archivo que requiera.
##########################################################################


# gen_esqueleto -----------------------------------------------------------
#' Title: gen_esqueleto: Función que genera una matriz llamada
#' "mat_esqueleto", del semestre actual (sem_fin) que contiene el número
#' de grupos simulados para cada materia en cada hora. Arroja un error en
#' caso de no encontrar algún archivo que requiera.
#'
#' @param sem_fin: Variable tipo "integer" que indica el semestre
#' del cual se quiere obtener el esqueleto.
#' @param n_semestres_anteriores: Variable tipo "integer" que indica el
#' número de semestres anteriores al semestre actual para poder generar el
#' esqueleto.
#' @param directorio_info: Vector que contiene la ubicación y el nombre de
#' los archivos tipo ".Rdata" en donde se hayan guardado las matrices que
#' contienen la información de los semestres que se requieren para obtener
#' el esqueleto.
#' 
#' @example sem_fin <- 20201
#' @example n_semestres_anteriores <- 5
#' @example directorio_info <- c("m_grande por semestre/m_grande_20081.RData",
#' "m_grande por semestre/m_grande_20082.RData")
#'
#' @return mat_esqueleto: Matriz de 15 renglones con las horas (7-8,8-9,...,
#' 21-22) y tantas columnas como materias.
#'
gen_esqueleto <- function(directorio_info,param){
  # Start the clock!
  ptm <- proc.time()
  
  n_semestres_anteriores <- param$n_semestres_anteriores
  ##Se verifica que existen los archivos que se necesitan para formar el
  ##esqueleto.
  valida_info <- 0
  num_sem <- 0
  n_renglones <- 0
  
  for(i in 1:n_semestres_anteriores){
    ##CHECAR SI SE CAMBIA LA MANERA DE VERIFICAR LA EXISTENCIA DE LOS ARCHIVOS##
    if(file.exists(directorio_info[i])){
      num_sem <- num_sem +1
      load(directorio_info[i])
      n_renglones <- n_renglones + nrow(m_grande)}}
  
  if(num_sem==n_semestres_anteriores){
    valida_info <- 1}
  
  if(valida_info == 1){
    ##Se carga m_grande_total
    vec_excepciones <- "Inglés"
    nom_archivo_MGT <- paste0("Matrices m_grande_total/m_grande_total_",
                              param$sem_ini,"_",param$sem_fin,".RData")
    if(!file.exists(nom_archivo_MGT)){
      gen_m_grande_total(vec_excepciones,param)
    }else{
      load(nom_archivo_MGT)
    }
    
    #' Se define el vector de materias (vector con los nombres de todas las
    #' materias que se imparten en el Departamento de Matemáticas de la
    #' Facultad de Ciencias de la UNAM) y las variables que se utilizan
    nombre_hrs <- param$nombre_hrs
    vec_grupos_simulados <- rep(0,length(nombre_hrs))
    mat_simula_grupos <- guarda_mat_simula_grupos_1_sem(param)
    # load("vec_nom_materias_total.RData")
    materias_unicas <- param$vec_nom_materias_total#Vector con el nombre de las materias
    
    ## Inicializamos la matriz que va a contener la demanda simulada del
    ##número de grupos del siguiente semestre. Tiene tantos renglones como
    ##horas y tantas columnas como materias impartidas.
    mat_esqueleto <- matrix(0, nrow=length(param$nombre_hrs),
                            ncol=length(materias_unicas))
    
    vec_para_for <- 1:length(materias_unicas)
    pb <- txtProgressBar(min = 1, max = length(vec_para_for),
                         style = 3, width = 60)
    for(i in vec_para_for){
      setTxtProgressBar(pb, i)
      # version_matriz <- as.character(i)
      cat("\n***Iteración ",i," de ",length(materias_unicas))
      
      ##Se busca el vector con el número de grupos simulado de materia
      mat_info_grupos <- mat_simula_grupos[
        mat_simula_grupos[,1]==materias_unicas[i],c(2,3)]
      
      for(d in 1:length(nombre_hrs)){##Se recorren las horas
        if(dim(mat_info_grupos)[1] != 0){
          for(j in 1:dim(mat_info_grupos)[1]){ ##Se recorren los renglones
            hora <- mat_info_grupos[j,1]
            if(hora==nombre_hrs[d]){
              vec_grupos_simulados[d] <- mat_info_grupos[j,2]}}}}
      
      mat_esqueleto[,i] <- vec_grupos_simulados
    }
    close(pb)
    rownames(mat_esqueleto) <- param$nombre_hrs
    colnames(mat_esqueleto) <- materias_unicas
  }else{
    cat("\nNo se encontraron todos los archivos necesarios para generar 
        el esqueleto")
  }
  # Se guarda la variable tipo lista "lista_esqueleto", la cual contiene las
  #matrices "mat_simula_grupos" y "mat_esqueleto"
  lista_esqueleto <- list(mat_simula_grupos = mat_simula_grupos,
                          mat_esqueleto = mat_esqueleto)
  
  save(lista_esqueleto,file = paste0("lista_esqueleto_",param$sem_sig,".RData"))
  save(mat_simula_grupos,file = paste0("mat_simula_grupos por semestre/mat_simula_grupos_",
                                       param$sem_sig,".RData"))
  
  cat("La función gen_esqueleto tomó: ", (proc.time()-ptm)[3]/60," minutos\n\n\n" )
  return(mat_esqueleto)
}


############################################################################
##### SOLICITUDES #####
## Funciones que generan una matriz con solicitudes de todos los profesores.
############################################################################

# extrae_mat_x_prof -------------------------------------------------------
#' Esta función regresa una matriz de 16xnum_sem que contiene los nombres de
#' las materias que ha dado cada profesor por semestre.
#' Title: extrae_mat_x_prof
#' @param profesor: Nombre de algún profesor de la Facultad de Ciencias
#' @examples profesor <- "Arrigo Coen Coria"
#' 
#' @return materia_x_profesor: Matriz que contiene los nombres de las materias
#' que se le ha asignado a cada profesor por semestre. Tiene 16 renglones
#' y tanta columnas como semestres se tengan.
extrae_mat_x_prof <- function(profesor,param){
  m_grande_total = param$m_grande_total
  # lista_def_columnas_MG <- param$lista_def_columnas_MG
  num_col_Profesor <- arroja_ind_col_MG("Profesor")
  num_col_Materia <- arroja_ind_col_MG("Materia")
  num_col_NomMat_Act2000 <- arroja_ind_col_MG("NomMat_Act2000")
  num_col_NomMat_Act2006 <- arroja_ind_col_MG("NomMat_Act2006")
  num_col_NomMat_Act2015 <- arroja_ind_col_MG("NomMat_Act2015")
  num_col_NomMat_CdC1994 <- arroja_ind_col_MG("NomMat_CdC1994")
  num_col_NomMat_CdC2013 <- arroja_ind_col_MG("NomMat_CdC2013")
  num_col_NomMat_Mat1983 <- arroja_ind_col_MG("NomMat_Mat1983")
  num_col_NomMat_MAp2017 <- arroja_ind_col_MG("NomMat_MAp2017")
  num_col_Semestre <- arroja_ind_col_MG("Semestre")
  num_col_Grupo <- arroja_ind_col_MG("Grupo")
  
  mat_grupos_prof <- as.matrix(data.frame(m_grande_total[,num_col_Profesor],
                                          m_grande_total[,num_col_Materia],
                                          m_grande_total[,num_col_NomMat_Act2000],
                                          m_grande_total[,num_col_NomMat_Act2006],
                                          m_grande_total[,num_col_NomMat_Act2015],
                                          m_grande_total[,num_col_NomMat_CdC1994],
                                          m_grande_total[,num_col_NomMat_CdC2013],
                                          m_grande_total[,num_col_NomMat_Mat1983],
                                          m_grande_total[,num_col_NomMat_MAp2017],
                                          m_grande_total[,num_col_Semestre],
                                          m_grande_total[,num_col_Grupo]))
  
  mat_grupos <- mat_grupos_prof[mat_grupos_prof[,1]== profesor,2:11]
  mat_num_grupos <- unique(mat_grupos)#Se eliminan los renglones repetidos
  
  ## Se pone el if porque si sólo hay un vector marca error al poner nombres
  ##a las columnas en un objeto que tiene dimensión menor a 2
  if(length(mat_num_grupos)<11){
    mat_num_grupos <- matrix(mat_num_grupos,ncol = 10)
  }
  colnames(mat_num_grupos) <- c("Materias","NomMat_1","NomMat_2",
                                "NomMat_3","NomMat_4","NomMat_5",
                                "NomMat_6","NomMat_7","Sem","Grupos")
  
  ## Se tienen 16 renglones porque se tienen 8 columnas con nombres 
  ##para cada materia
  materia_x_profesor <- matrix(0,nrow = 16,ncol = length(param$Semestres))
  rownames(materia_x_profesor) <- c("Materia_1.0",
                                    "Materia_1.1",
                                    "Materia_1.2",
                                    "Materia_1.3",
                                    "Materia_1.4",
                                    "Materia_1.5",
                                    "Materia_1.6",
                                    "Materia_1.7",
                                    "Materia_2.0",
                                    "Materia_2.1",
                                    "Materia_2.2",
                                    "Materia_2.3",
                                    "Materia_2.4",
                                    "Materia_2.5",
                                    "Materia_2.6",
                                    "Materia_2.7")
  colnames(materia_x_profesor) <- param$nombre_sem
  
  for(j in 1:length(param$Semestres)){
    datos_x_sem <- mat_num_grupos[mat_num_grupos[,9]==param$Semestres[j],1:8]
    if(length(datos_x_sem)==0){
      materia_x_profesor[,j] <- 0
    }else if(length(datos_x_sem)==8){
      materia_x_profesor[1:8,j] <- datos_x_sem
      materia_x_profesor[9:16,j] <- 0
    }else{
      materia_x_profesor[1:8,j] <- datos_x_sem[1,]
      materia_x_profesor[9:16,j] <- datos_x_sem[2,]
    }
  }
  
  return(materia_x_profesor)
}


# simula_una_eleccion_materia ------------------------------------------------------
#' Title simula_una_eleccion_materia: Función que simula una elección de materia de
#' cada profesor.
#' @param profesor: Nombre de algún profesor de la Facultad de Ciencias
#' @examples profesor <- "Arrigo Coen Coria"
#' 
#' @return materia_simulada: Vector de tamaño 8 con los nombre de la materia simulada.
#' 
simula_una_eleccion_materia <- function(profesor,param){
  #Matriz que contiene los nombres de las materias que se le ha asignado a
  #cada profesor por semestre.
  materia_x_profesor <- extrae_mat_x_prof(profesor,param)
  # prob_eleccion_x_sem <- c(0.0125,0.0125,0.0125,0.0125,0.0125,0.0125,0.0125,
  #                          0.0125,0.1,0.5,0.3)
  prob_eleccion_x_sem <- c(rep(0.1/(length(param$Semestres)-3),
                               length(param$Semestres)-3),0.1,0.5,0.3)
  prob_acum <- c(0,cumsum(prob_eleccion_x_sem))
  num_aleatorio <- runif(1)
  
  for(k in 1:(length(prob_acum)-1)){
    if(num_aleatorio >= prob_acum[k] && num_aleatorio < prob_acum[k+1]){
      ##Para simular sólo una materia, se selecciona de manera aleatoria
      if(num_aleatorio >= 0.5){
        materia_simulada <- materia_x_profesor[1:8,k]
      }else{
        materia_simulada <- materia_x_profesor[9:16,k]
      }
    }
  }
  return(materia_simulada)
}


# simula_eleccion_materia ---------------------------------------------------------
#' Title simula_eleccion_materia: Función que simula varias elecciones de materias de
#' cada profesor.
#' @param profesor: Nombre de algún profesor de la Facultad de Ciencias
#' @examples profesor <- "Arrigo Coen Coria"
#' 
#' @return materias_simuladas: Vector con los nombres de las materias
#' simuladas.
simula_eleccion_materia <- function(profesor,param){
  num_simula_eleccion_materia <- param$num_simula_eleccion_materia
  mat_materias_simuladas <- matrix(0,nrow = 8,ncol = num_simula_eleccion_materia)
  materias_simuladas <- rep(0,num_simula_eleccion_materia)
  
  for(k in 1:num_simula_eleccion_materia){
    # cat("\nk = ",k)
    eleccion <- simula_una_eleccion_materia(profesor,param)
    # cat("\n eleccion = ",eleccion)
    mat_materias_simuladas[,k] <- eleccion
  }
  
  
  #Se toma un nombre de materia por cada columna
  for(k in 1:num_simula_eleccion_materia){
    materias_simuladas[k] <- sample(mat_materias_simuladas[,k],size = 1)
  }
  
  return(materias_simuladas)
}


# horario_de_materia_x_prof ---------------------------------------------
#' Title: horario_de_materia_x_prof: Función regresa una matriz de 2xnum_sem
#' que contiene los horarios materias que ha dado cada profesor por semestre.
#' @param profesor: Nombre de algún profesor de la Facultad de Ciencias
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' 
#' @examples profesor <- "Arrigo Coen Coria"
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#' 
#' @return horario_x_profesor: Matriz que contiene los horarios de las
#' materias que ha dado cada profesor por semestre.
#' 
horario_de_materia_x_prof <- function(profesor,param){
  m_grande_total = param$m_grande_total
  
  num_col_Profesor <- arroja_ind_col_MG("Profesor")
  num_col_Materia <- arroja_ind_col_MG("Materia")
  num_col_Semestre <- arroja_ind_col_MG("Semestre")
  num_col_Grupo <- arroja_ind_col_MG("Grupo")
  num_col_horario_num <- arroja_ind_col_MG("horario_num")
  
  
  mat_grupos <- m_grande_total[m_grande_total[,num_col_Profesor]== profesor,
                               c(num_col_Materia,num_col_Semestre,
                                 num_col_Grupo,num_col_horario_num)]
  mat_num_grupos <- unique(mat_grupos)#Se eliminan los renglones repetidos
  colnames(mat_num_grupos) <- c("Materias","Sem","Grupos","horario_num")
  
  horario_x_profesor <- matrix(0,nrow = 2,ncol = length(param$Semestres))
  rownames(horario_x_profesor) <- c("Materia1","Materia2")
  colnames(horario_x_profesor) <- param$nombre_sem
  
  for(j in 1:length(param$Semestres)){
    datos_x_sem <- mat_num_grupos[mat_num_grupos[,2]==param$Semestres[j],4]
    if(length(datos_x_sem)==1){
      horario_x_profesor[1,j] <- datos_x_sem
      horario_x_profesor[2,j] <- 0
    }else{
      for(k in 1:2){
        horario_x_profesor[k,j] <- datos_x_sem[k]}}}
  
  return(horario_x_profesor)
}


# simula_una_eleccion_horario ------------------------------------------------------
#' Title simula_una_eleccion_horario: Función que simula una elección de horarios de
#' cada profesor.
#' @param profesor: Nombre de algún profesor de la Facultad de Ciencias
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' 
#' @examples profesor <- "Arrigo Coen Coria"
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#' 
#' @return horarios_simulados: Horario simulado por profesor
#' 
simula_una_eleccion_horario <- function(profesor,param){
  #Matriz que contiene los nombres de las horarios que se le ha asignado a
  #cada profesor por semestre.
  horario_x_profesor <- horario_de_materia_x_prof(profesor,param)
  prob_eleccion_x_sem <- c(rep(0.1/(length(param$Semestres)-3),
                               length(param$Semestres)-3),0.1,0.5,0.3)
  prob_acum <- c(0,cumsum(prob_eleccion_x_sem))
  horarios_simulados <- 0
  num_aleatorio <- runif(1)
  # cat("\n num_aleatorio = ",num_aleatorio)
  
  valor_aux <- 0
  k <- 1
  while(valor_aux == 0 && k <= (length(prob_acum)-1)){
    # cat("\nk = ",k)
    if(num_aleatorio >= prob_acum[k] && num_aleatorio < prob_acum[k+1]){
      ##Preguntamos si hay datos en la primer materia del vector (con respecto
      #al semestre en el que vamos a elegir el horario)
      if(is.na(horario_x_profesor[1,k])){
        ##Preguntamos si hay datos en la segunda materia del vector (con respecto
        #al semestre en el que vamos a elegir el horario)
        # cat("\nif_1")
        if(is.na(horario_x_profesor[2,k])){
          # cat("\nif_2")
          num_aleatorio <- runif(1)
          k <- 0
          # cat("\n num_aleatorio = ",num_aleatorio)
        }else{
          # cat("\nelse_1")
          horarios_simulados <- horario_x_profesor[2,k]
          valor_aux <- 1
        }
      }else{
        # cat("\nelse_2")
        horarios_simulados <- horario_x_profesor[1,k]
        valor_aux <- 1
      }
    }
    k <- k +1
  }
  return(horarios_simulados)
}


# simula_eleccion_horario ---------------------------------------------------------
#' Title simula_eleccion_horario: Función que simula varias elecciones de horarios
#' de cada profesor.
#' @param profesor: Nombre de algún profesor de la Facultad de Ciencias
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' 
#' @examples profesor <- "Arrigo Coen Coria"
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#' 
#' @return horarios_simulados: Vector con los nombres de los horarios
#' simulados por profesor.
simula_eleccion_horario <- function(profesor,param){
  num_simula_eleccion_horario <- param$num_simula_eleccion_horario
  horarios_simulados <- rep(0,num_simula_eleccion_horario)
  
  for(k in 1:num_simula_eleccion_horario){
    (eleccion <- simula_una_eleccion_horario(profesor,param))
    horarios_simulados[k] <- eleccion
  }
  horarios_simulados <- as.integer(unique(horarios_simulados))
  
  return(horarios_simulados)
}


# gen_solicitudes ---------------------------------------------------------
#' Title gen_solicitudes: Función que guarda, en una matriz, la información
#' de las solicitudes de materia y de horario de todos los profesores las
#' cuales se generan por medio de las simulaciones de dichas elecciones
#' (materia y horario).
#'
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' 
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return mat_solicitudes: Matriz de 12 columnas que contiene la
#' información de las solicitudes de materia y de horario de todos los
#' profesores, en las primeras 6 columnas se tiene la información
#' de la simulación de elección de materias y en las últimas 6 columnas
#' se tiene la información de la simulación de elección de horarios,
#' la matriz puede no estar completamente llena),tiene como renglones
#' los nombres de los profesores.
#'
gen_solicitudes <- function(param){
  m_grande_total = param$m_grande_total
  num_col <- arroja_ind_col_MG("Profesor")
  Profesor <- m_grande_total[,num_col]
  
  # lista_def_columnas_MG <- param$lista_def_columnas_MG
  # Start the clock!
  ptm <- proc.time()
  
  ##Vector con nombres de profesores sin  repetición
  # Profesores <- sort(unique(lista_def_columnas_MG$Profesor))
  Profesores <- unique(Profesor)
  #Quitamos las entradas que sean iguales a cero en caso de existir
  Profesores <- Profesores[Profesores!=0]
  num_profesores <- length(Profesores)
  
  ##Inicializamos la matriz
  mat_solicitudes <- matrix(0,nrow = num_profesores,ncol = 12)
  rownames(mat_solicitudes) <- Profesores
  colnames(mat_solicitudes) <- c("Materia_1","Materia_2","Materia_3",
                                 "Materia_4","Materia_5","Materia_6",
                                 "Horario_1","Horario_2","Horario_3",
                                 "Horario_4","Horario_5","Horario_6")
  vec_para_for <- 1:num_profesores
  pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
  for(k in vec_para_for){
    setTxtProgressBar(pb, k)
    # cat("\n k_prof_solic = ",k)
    materias_simuladas <- simula_eleccion_materia(Profesores[k],param)
    horarios_simulados <- simula_eleccion_horario(Profesores[k],param)
    
    ##Los vectores anteriores no necesariamente tienen información en
    ##todas sus entradas, por lo que la matriz "mat_solicitudes" puede
    ##no estar completamente llena.
    mat_solicitudes[k,1:param$num_simula_eleccion_materia] <- materias_simuladas
    # mat_solicitudes[k,(param$num_simula_eleccion_materia+1):
    #                   (param$num_simula_eleccion_materia+
    #                      param$num_simula_eleccion_horario)] <- horarios_simulados
  }##Fin de for
  close(pb)
  
  cat("La función gen_solicitudes tomó: ",(proc.time()-ptm)[3]/60," minutos\n\n\n" )
  
  return(mat_solicitudes)
}



############################################################################
##### UNA ASIGNACIÓN #####
## Funciones que generan una matriz con las asignaciones de
##Materia-Profesor-Horario-Salón
############################################################################

# asigna_una_mat_prof_hora ---------------------------------------------------
#' Title: asigna_una_mat_prof_hora: Función que genera la asignación de una 
#' materia con profesor por hora, dependiendo del número de grupos simulados
#' para el siguiente semestre.
#'
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#' @return mat_asignacion: Matriz de 4 columnas (Materia,Profesor,Horario,
#' Salón), la cual tiene la información de las asignaciones generadas de
#' las simulaciones tanto del número de grupos como de las elecciones de
#' los profesores.
#'
asigna_una_mat_prof_hora <- function(mat_solicitudes,materia,param){
  m_grande_total = param$m_grande_total
  num_col <- arroja_ind_col_MG("Profesor")
  # Profesores <- sort(unique(m_grande_total[,2]))
  Profesores <- unique(m_grande_total[,num_col])
  #Quitamos las entradas que sean iguales a cero en caso de existir
  Profesores <- Profesores[Profesores!=0]
  num_profes <- length(Profesores)
  mat_prof_materia_horario <- matrix(c(Profesores,mat_solicitudes),
                                     ncol = 13)
  colnames(mat_prof_materia_horario) <- c("Profesores","Materia_1","Materia_2",
                                          "Materia_3","Materia_4","Materia_5",
                                          "Materia_6","Horario_1","Horario_2",
                                          "Horario_3","Horario_4","Horario_5",
                                          "Horario_6")
  gpos_simulados <- simula_grupos(materia,param)
  
  mat_asignacion <- matrix(0,nrow = sum(gpos_simulados),ncol = 4)
  colnames(mat_asignacion) <- c("Materia","Profesor","Horario","Salón")
  
  mat_asignacion[,1] <- materia ##Se llena la columna "Materia"
  
  ##Se llena la columna "Horario"
  if(sum(gpos_simulados)==0){
    vec_num_horas <- rep(0,sum(gpos_simulados))
    ## Marcamos con -1 la variable para que al generar la matriz con toda la
    ##información no haya error.
    mat_asignacion <- -1
  }else{
    i <- 1
    vec_num_horas <- rep(0,sum(gpos_simulados))
    for(k in 1:length(gpos_simulados)) {
      # cat("\n k = ",k)
      if(gpos_simulados[k]>0){
        # cat("\n  i = ",i)
        n_gpos <- gpos_simulados[k]
        mat_asignacion[i:(i+n_gpos-1),3] <- param$nombre_hrs[k]
        vec_num_horas[i:(i+n_gpos-1)] <- param$Horas[k]
        i <- i + n_gpos}}}
  
  ##Se llena la columna "Profesor"
  if(length(mat_asignacion)>1){##Verifica que haya información en la matriz
    datos_de_materia <- matrix(0,ncol = 7,nrow = num_profes)
    colnames(datos_de_materia) <- c("Profesor","Horario1","Horario2",
                                    "Horario3","Horario4","Horario5","Horario6")
    for(m in 1:num_profes){
      if(mat_solicitudes[m,1]==materia||
         mat_solicitudes[m,2]==materia||
         mat_solicitudes[m,3]==materia||
         mat_solicitudes[m,4]==materia||
         mat_solicitudes[m,5]==materia||
         mat_solicitudes[m,6]==materia){
        datos_de_materia[m,] <- mat_prof_materia_horario[m,c(1,8:13)]}}
    datos_de_materia <- unique(datos_de_materia)
    
    for(k in 1:length(vec_num_horas)){
      # cat("\nk = ",k)
      while(mat_asignacion[k,2] == 0){
        for(i in 1:dim(datos_de_materia)[1]){
          if(datos_de_materia[i,1] != 0){
            for(j in 2:7){
              if(datos_de_materia[i,j]==vec_num_horas[k] && mat_asignacion[k,2]==0){
                mat_asignacion[k,2] <- datos_de_materia[i,1]
                ##Parar evitar que un mismo profesor dé más de una clase a la misma
                #hora, se sustituye el valor de la matriz de datos por un cero.
                datos_de_materia[i,j] <- 0}}}}
        if(mat_asignacion[k,2] == 0){
          mat_asignacion[k,2] <- -1
        }
      }##Fin de while
    }##Fin de for(k in 1:length(vec_num_horas))
  }##Fin de if(length(mat_asignacion)>1)
  
  return(mat_asignacion)
}


# gen_asignacion ----------------------------------------------------------
#' Title gen_asignacion: Función que genera asignaciones de materia con
#' profesor por hora, dependiendo del número de grupos simulados para el
#' siguiente semestre, con la información de solicitudes que se obtiene de
#' la función "gen_solicitudes". 
#'
#' @param mat_solicitudes: Matriz de 12 columnas que contiene la
#' información de las solicitudes de materia y de horario de todos los
#' profesores, en las primeras 6 columnas se tiene la información
#' de la simulación de elección de materias y en las últimas 6 columnas
#' se tiene la información de la simulación de elección de horarios,
#' la matriz puede no estar completamente llena),tiene como renglones
#' los nombres de los profesores.
#' @param mat_esqueleto: Matriz de 15 renglones con las horas (7-8,8-9,...,
#' 21-22) y tantas columnas como materias impartidas en el semestre actual.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' 
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return mat_asignaciones: Matriz de cuatro columnas (Materia, Profesor,
#' Horario, Salón) la cual contiene en el i-ésimo renglón la asignación
#' por materia, profesor y horario //está pendiente la asignación de salón//
#'
gen_asignacion <- function(mat_esqueleto,mat_solicitudes,param){
  # Start the clock!
  ptm <- proc.time()
  
  ##Se inicializan las variables
  m_grande_total = param$m_grande_total
  num_col <- arroja_ind_col_MG("Materia")
  # Materias <- sort(unique(m_grande_total[,1]))
  # Materias <- unique(m_grande_total[,num_col])
  # load("vec_nom_materias_total.RData")
  Materias <- param$vec_nom_materias_total
  #Quitamos las entradas que sean iguales a cero en caso de existir
  Materias <- Materias[Materias!=0]
  num_materias <- length(Materias)
  
  # Para inicializar la matriz que se va a llenar con la información,
  #se suman los grupos totales obtenidos en la matriz "mat_esqueleto"
  #y se le suman 1000 renglones para tener una base y que la matriz no
  #incremente su tamaño en cadaiteración de llenado.
  gpos_simulados <- sum(mat_esqueleto)
  mat_asignaciones <- matrix(0,nrow = gpos_simulados+1000, ncol = 4)
  
  i <- 1
  vec_para_for <- 1:num_materias
  pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
  for(m in vec_para_for){
    setTxtProgressBar(pb, m)
    # cat("\n Materia = ",m," de ",num_materias)
    # cat("\n  i = ",i)
    mat_asignacion <- asigna_una_mat_prof_hora(mat_solicitudes,Materias[m],param)
    num_renglones <- dim(mat_asignacion)[1]
    
    if(length(mat_asignacion)==1){
      ## Si "mat_asignacion" tiene longitud de 1, implica que no hay grupos
      ##simulados para dicha materia
      i <- i + 1
    }else{
      for(d in 1:num_renglones){
        # cat("\n   d = ",d)
        mat_asignaciones[i,] <- mat_asignacion[d,]
        i <- i + 1
      }
    }
  }
  close(pb)
  
  ## Se eliminan los ceros de la matriz
  #No utilizar unique() porque no se conserva el número de grupos que
  #se requieren
  # mat_asignaciones <- unique(mat_asignaciones)
  mat_asignaciones <- matrix(mat_asignaciones[mat_asignaciones[,1]!=0],
                             ncol = 4)
  colnames(mat_asignaciones) <- c("Materia","Profesor","Horario","Salón")
  
  cat("La función gen_asignaciones tomó: ", (proc.time()-ptm)[3]/60," minutos\n\n\n" )
  
  ##ELIMINAR LOS PROFESORES QUE NO ESTÁN DISPONIBLES PARA DAR CLASES##
  return(mat_asignaciones)
}

############################################################################
##### ASIGNACIÓN COMPLETA #####
## Función que manda llamar todas las funciones para obtener la matriz
##de asignaciones Materia-Profesor-Horario(-Salón)
############################################################################

# gen_asignacion_completa -------------------------------------------------
#' Title gen_asignacion_completa: Función que genera la asignación completa,
#se le pide al usuario los parámetros que se requieren y al final se obtiene
#la matriz con las asignaciones de materia-profesor-horario(-salón)
#'
#' @param sem_ini: Semestre en el que se inicia la búsqueda de
#' información.
#' @param sem_fin: Semestre en el que se finaliza la búsqueda de
#' información para generar la asignación del siguiente semestre.
#'
#' @example sem_ini <- 20081
#' @example sem_fin <- 20181
#'
#' @return mat_asignaciones: Matriz de cuatro columnas (Materia, Profesor,
#' Horario, Salón) la cual contiene en el i-ésimo renglón la asignación
#' por materia, profesor y horario //está pendiente la asignación de salón//
#'
gen_asignacion_completa <- function(sem_ini,sem_fin){
  # Start the clock!
  ptm <- proc.time()##Se pone al inicio de la función
  
  # Lista -------------------------------------------------------------------
  ## Se definen las listas generales de parámetros que se van a utilizar:
  # list_url <- list()
  # list_url$sem_ini = 20081 # Datos SUPER GRANDE
  # # list_url$sem_ini = 20151 # Datos GRANDE
  # # list_url$sem_ini = 20172 # Datos Mediana 
  # # list_url$sem_ini = 20192 # Datos Chica 
  # # list_url$sem_fin = 20192
  # list_url$sem_fin = 20201
  # list_url$sem_actual = 20201
  # list_url$Actualiza_RAW_url = TRUE
  # list_url$Actualiza_limpia_base_url = TRUE
  # list_url$Actualiza_elimina_grupos_con_0 = TRUE
  # list_url$Salvar_URL_RData = TRUE
  # list_url$usar_vec_corto_num_materia = TRUE
  # list_url$elimina_pags_con_0_grupos = TRUE
  # # list_url$Carpeta_RData = "Archivos RData V01"
  # list_url$Carpeta_RData = "Archivos RData"
  # 
  # list_url$utilizar_RAW_anterior = T
  # list_url$usa_grupos_salvados = T
  # list_url$usa_vec_con_salon = T
  # list_url$usa_vec_con_info_salvados = T
  # 
  # 
  # list_url$planes_estudio = c(119,1176,2017,218,1556,217,2055)
  # list_url$file_name <- paste0(list_url$Carpeta_RData,"/Dat_URL_",list_url$sem_ini,"_",list_url$sem_fin,".RData")
  # list_url$file_name_RAW <- paste0(list_url$Carpeta_RData,"/Lista_RAW_Dat_URL_",list_url$sem_ini,"_",list_url$sem_fin,".RData")
  # list_url$nombres_carrera_plan <- c("Actuaría (plan 2000)",
  #                                    "Actuaría (plan 2006)",
  #                                    "Actuaría (plan 2015)",
  #                                    "Ciencias de la Computación (plan 1994)",
  #                                    "Ciencias de la Computación (plan 2013)",
  #                                    "Matemáticas (plan 1983)",
  #                                    "Matemáticas Aplicadas (plan 2017)")
  # list_url$mat_ubicaciones_url <- matrix(c("Materia"            ,'#info-contenido h2', T,F,
  #                                          "Profesor"           ,'tr:nth-child(1) td:nth-child(2) a',F,F,
  #                                          "Horario"            ,'tr:nth-child(1) td:nth-child(4)',F,T,
  #                                          "Lugares"            ,'#info-contenido div',F,F,
  #                                          "Alumnos"            ,'#info-contenido div',F,F,
  #                                          "Salon"              ,'tr:nth-child(1) td~ td+ td a , td:nth-child(4) a',F,F,
  #                                          "Grupo"              ,'#info-contenido div',F,F,
  #                                          "Carrera"            ,'h1',T,F,
  #                                          "Plan"               ,'h1',T,F,
  #                                          "Semestre"           ,-1,-1,-1,### FALTA POR HACER
  #                                          "Cambios"            ,-1,-1,-1,### FALTA POR HACER
  #                                          "Turno"              ,-1,-1,-1,### FALTA POR HACER
  #                                          "Semestre_de_materia",'#info-contenido h2',T,F,
  #                                          "Grupos_x_pag"       ,'strong',F,F,
  #                                          "Grupo_paralelo"     ,'em',F,F),ncol=4,byrow = T)
  # list_url$colnames_mat_posibles_url <- c("Semestre","Plan","Materia","URL","Grupos x pag","url_con_salon")
  # list_url$ncol_mat_posibles_url <- length(list_url$colnames_mat_posibles_url)
  # list_url$nrow_mat_posibles_url <- 200000
  # # list_url$nrow_mat_posibles_url = dim(list_url$mat_posibles_url)[1]
  # list_url$mat_RAW_url <- matrix(0,list_url$nrow_mat_posibles_url,list_url$ncol_mat_posibles_url)
  # list_url$mat_posibles_url <- list_url$mat_RAW_url
  # 
  # 
  # list_url$ncol_mat_Grande <- 13
  # list_url$mat_Grande <- matrix(0,0,list_url$ncol_mat_Grande)
  # list_url$mat_Grande_con_url <- matrix(0,0,list_url$ncol_mat_Grande+1)
  # 
  # list_url$semestres_reales <- list_url$sem_fin
  # list_url$plan_reales <- 2017
  # list_url$num_mat_reales <- 1
  # list_url$num_grupos <- rep(-1,list_url$nrow_mat_posibles_url)
  # list_url$url_con_salon <- NA
  # 
  # list_url$mat_paginas_error <- matrix(0,1,4)
  # 
  # 
  # list_url$indicadoras_actualiza_col_j_mat_Grande = rep(T,13)
  # 
  # 
  # colnames(list_url$mat_ubicaciones_url) <- c("Nombre columna","Ubicacion en pagina","Repetir","Elimina salto")
  # colnames(list_url$mat_Grande) <- c("Materia", "Profesor","Horario","Lugares","Alumnos","Salon",
  #                                    "Grupo","Carrera","Plan","Semestre","Cambios","Turno","Semestre_de_materia")
  # 
  # colnames(list_url$mat_paginas_error) <- c("Columna","length(vec)","num_gpo","Pagina")
  # 
  # if(F) {
  #   list_url$utilizar_RAW_anterior = F
  #   list_url$usa_grupos_salvados = F
  #   list_url$usa_vec_con_salon = F
  #   list_url$usa_vec_con_info_salvados = F
  # }
  # # Valida_list_url(list_url)
  # 
  # param = list(nombre_hrs = c("7-8","8-9","9-10","10-11","11-12",
  #                             "12-13","13-14","14-15","15-16",
  #                             "16-17","17-18","18-19","19-20",
  #                             "20-21","21-22"),
  #              nombre_sem = c("2015-1","2015-2","2016-1","2016-2",
  #                             "2017-1","2017-2","2018-1","2018-2",
  #                             "2019-1","2019-2","2020-1"),
  #              Semestres = c(20151,20152,20161,20162,20171,20172,20181,
  #                            20182,20191,20192,20201),
  #              Horas = c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21),
  #              q1 = 80, q2 = 90,
  #              num_simula_eleccion_materia = 6,
  #              num_simula_eleccion_horario = 6,
  #              nom_cols_m14 = c("Materia","Profesor","Horario","Lugares",
  #                               "Alumnos","Salon","Grupo","Carrera","Plan",
  #                               "Semestre","Cambios","Turno",
  #                               "Semestre_de_materia","url"),
  #              nom_cols_MG = c("Materia","Profesor","Horario","horario_num",
  #                              "Lugares","Alumnos","Salon","Grupo","Carrera",
  #                              "Plan","Semestre","Cambios","Turno",
  #                              "Semestre_de_materia","url","Act2000","Act2006",
  #                              "Act2015","CdC1994","CdC2013","Mat1983","MAp2017",
  #                              "NomMat_Act2000","NomMat_Act2006","NomMat_Act2015",
  #                              "NomMat_CdC1994","NomMat_CdC2013","NomMat_Mat1983",
  #                              "NomMat_MAp2017","URL_Act2000","URL_Act2006",
  #                              "URL_Act2015","URL_CdC1994","URL_CdC2013",
  #                              "URL_Mat1983","URL_MAp2017"),
  #              m_grande_total = matrix(0,ncol = 
  #                                        length(c("Materia","Profesor","Horario",
  #                                                 "horario_num","Lugares","Alumnos",
  #                                                 "Salon","Grupo","Carrera","Plan",
  #                                                 "Semestre","Cambios","Turno",
  #                                                 "Semestre_de_materia","url",
  #                                                 "Act2000","Act2006","Act2015",
  #                                                 "CdC1994","CdC2013","Mat1983",
  #                                                 "MAp2017","NomMat_Act2000",
  #                                                 "NomMat_Act2006","NomMat_Act2015",
  #                                                 "NomMat_CdC1994","NomMat_CdC2013",
  #                                                 "NomMat_Mat1983","NomMat_MAp2017",
  #                                                 "URL_Act2000","URL_Act2006",
  #                                                 "URL_Act2015","URL_CdC1994",
  #                                                 "URL_CdC2013","URL_Mat1983",
  #                                                 "URL_MAp2017"))))
# Termina definición de lista ---------------------------------------------

  list_url$sem_ini = sem_ini
  list_url$sem_actual = sem_fin
  list_url$sem_fin = sem_fin
  
  list_url$file_name = paste0(list_url$Carpeta_RData,"/Dat_URL_",list_url$sem_ini,
                              "_",list_url$sem_fin,".RData")
  list_url$file_name_RAW = paste0(list_url$Carpeta_RData,"/Lista_RAW_Dat_URL_",
                                  list_url$sem_ini,"_",list_url$sem_fin,".RData")
  
  
  if(nchar(list_url$sem_ini)!= 5 || nchar(list_url$sem_fin)!= 5){
    cat("\n***Los semestres ingresados no son válidos***")
  }else{
    ##### Se crea el vector para los semestres pares e impares #####
    semestres = (list_url$sem_ini:list_url$sem_fin)[
      (list_url$sem_ini:list_url$sem_fin) %% 10>0 &
        (list_url$sem_ini:list_url$sem_fin) %% 10<3]
    param$Semestres = semestres
    n_semestres_anteriores <- length(semestres)
    param$nombre_sem = as.character(semestres)
    
    
    ##### Se carga y se limpia la lista de urls (para no tener #####
    ## páginas sin información,...)
    list_url <- Actualiza_list_url(list_url)
    mat_posibles_url <- list_url$mat_posibles_url
    colnames(mat_posibles_url) <- c("Semestre","Plan","Materia","URL",
                                    "Grupos x pag","url_con_salon")
    # View(mat_posibles_url)
    save(mat_posibles_url, file = paste0("mat_posibles_url_",list_url$sem_ini,"_",
                                         list_url$sem_fin,".RData"))
    
    ##### Se obtiene "m_grande" y se genera un archivo para cada semestre #####
    
    
    ######### OBTENER M_GRANDE ############
    
    directorio_info <- rep(0,length(semestres))
    cat("\nSe obtiene m_grande y se genera un archivo para cada semestre")
    cat("\n")
    for(k in 1:length(semestres)){
      # cat("\n k = ",k)
      sem_info <- semestres[k]
      directorio_info[k] <- gen_m_grande(sem_info,list_url)
    }
    
    ##### Se genera el esqueleto del semestre que se quiere obtener #####
    cat("\nGenerando mat_esqueleto")
    cat("\n")
    mat_esqueleto <- gen_esqueleto(directorio_info,param)
    save(mat_esqueleto, file = paste0("mat_esqueleto_",list_url$sem_ini,"_",
                                      list_url$sem_fin,".RData"))
    
    ##Se carga y se define "m_grande_total" y la lista con sus columnas
    load(paste0("m_grande_total_",list_url$sem_ini,"_",list_url$sem_fin,".RData"))
    param$m_grande_total = m_grande_total
    
    ##### Se genera la matriz de solicitudes de todos los profesores #####
    cat("\nGenerando mat_solicitudes")
    cat("\n")
    mat_solicitudes <- gen_solicitudes(param)
    
    save(mat_solicitudes, file = paste0("mat_solicitudes_",list_url$sem_ini,"_",
                                        list_url$sem_fin,".RData"))
    
    
    ##### Se genera la matriz de asignaciones de todos los profesores #####
    cat("\nGenerando mat_asignaciones")
    cat("\n")
    mat_asignaciones <- gen_asignacion(mat_esqueleto,mat_solicitudes,param)
    save(mat_asignaciones, file = paste0("mat_asignaciones_",list_url$sem_ini,"_",
                                         list_url$sem_fin,".RData"))
    if((sem_fin%%2)==0){
      sem_sig <- sem_fin + 9
    }else{
      sem_sig <- sem_fin + 1
    }
    
    cat("\nLa matriz de asignaciones generada corresponde al semestre: ",sem_sig)
    cat("\nEl proceso total tomó: ", (proc.time()-ptm)[3]/60," minutos\n\n\n" )
    
    return(mat_asignaciones)
  }
}


############################################################################
################################ OTROS #####################################
############################################################################


# gen_nom_list_n_sim_1_materia --------------------------------------------
#' Title gen_nom_list_n_sim_1_materia: Función que genera el nombre de los
#' archivos que guardan las listas que contienen las matrices con las n
#' simulaciones por materia.
#'
#' @param num_sim: Número de matrices simuladas. 
#' @param num_materia: Índice de la materia de la cual se quiere obtener
#' la información.
#' @param sem_sig: Semestre del que se obtienen las simulaciones.
#'
#' @example num_sim <- 10
#' @example num_materia <- 5
#' @example sem_sig <- 20202
#' 
#' @return nom_lista_n_sim_1_materia: Variable tipo char con el nombre
#' de la lista que contiene las matrices con n simulaciones por materia.
#'
gen_nom_list_n_sim_1_materia <- function(num_sim,num_materia,sem_sig){
  ### Explicación del nombre:
  #' Se guarda en la carpeta "Listas mat_n_sim"
  #' "num_sim_sim" indica el número de simulaciones que tiene la lista
  #' "materia_num_materia" indica el número de materia de la simulación
  #' "sem_sig" indica el semestre del que se obtuvieron las simulaciones
  nom_lista_n_sim_1_materia <- paste0("Listas mat_n_sim/lista_mat_",
                                      num_sim,"_sim_materia_",num_materia,
                                      "_",sem_sig,".RData")
  return(nom_lista_n_sim_1_materia)
}


# gen_nom_list_n_sim_x_sem ------------------------------------------------
#' Title gen_nom_list_n_sim_x_sem Función que genera el nombre de los
#' archivos que guardan las listas de matrices con n simulaciones
#' para todas las materias.
#'
#' @param sem_sig: Semestre del que se obtienen las simulaciones.
#' @example sem_sig <- 20202
#'
#' @return nom_lista_n_sim_x_sem: Variable tipo char con el nombre
#' de la lista que contiene las listas de matrices con n simulaciones
#' para todas las materias.
#'
gen_nom_list_n_sim_x_sem <- function(sem_sig){
  ### Explicación del nombre:
  #' Se guarda en la carpeta "Listas mat_n_sim por semestre"
  #' "sem_sig" indica el semestre del que se obtuvieron las simulaciones
  nom_lista_n_sim_x_sem <- paste0("Listas mat_n_sim por semestre/lista_n_sim_por_sem_",
                                  sem_sig,".RData")
  
  return(nom_lista_n_sim_x_sem)
}


# gen_nom_heatmap ---------------------------------------------------------
#' Title gen_nom_heatmap: Función que genera los nombres de los heatmaps
#' para la varianza y diferencia relativa de las matrices obtenidas en
#' las pruebas.
#'
#' @param tipo: Indica si la gráfica es de varianza o de diferencia
#' relativa.
#' @param num_materia: Índice de la materia de la cual se quiere obtener
#' la información.
#' @param sem_sig: Semestre del que se obtienen las simulaciones.
#'
#' @example tipo <- "var_alum_materia" o "dif_relativa_alum_materia"
#' @example num_materia <- 5
#' @example sem_sig <- 20202
#'
#' @return nom_heatmap: Variable tipo char con el nombre de los heatmaps
#' para la varianza y diferencia relativa de las matrices obtenidas en
#' las pruebas.
#' 
gen_nom_heatmap <- function(tipo,num_materia,sem_sig){
  ### Explicación del nombre:
  #' Se guarda en la carpeta "Figuras/Matrices Simuladas"
  #' tipo puede ser "var_alum_materia" o "dif_relativa_alum_materia"
  #' "num_materia" indica el número de materia de la simulación
  #' "sem_sig" indica el semestre del que se obtuvieron las simulaciones
  nom_heatmap <- paste0("Figuras/Matrices Simuladas/heatmap_",tipo,"_",
                        num_materia,"_sem_",sem_sig,".jpeg")
  
  return(nom_heatmap)
}


# gen_mat_n_sim_n_sem -----------------------------------------------------------
#' Title gen_mat_n_sim_n_sem: Función que manda a llamar a la función
#' "gen_mat_n_sim_1_sem" para cada semestre de un vector dado. Se genera
#' una lista de listas para cada semestre en "vec_sem_sig". Esta función se
#' utiliza principalmente para hacer pruebas de que el modelo funciona.
#'
#' @param vec_sem_sig: Vector con los semestres de los que se desean obtener
#' las simulaciones. Deben estar ordenados del más antiguo al más reciente.
#' @param vec_k_sem_info; Vector con el número de semestres anteriores para
#' cada "sem_sig" del vector "vec_sem_sig"
#' @param num_sim: Número de matrices simuladas.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' 
#' @example vec_sem_sig <- c(20131,20152,20182,20201)
#' @example vec_k_sem_info <- 20201
#' @example num_sim <- c(10,15,21,24)##Se inicia en 2008-1
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
gen_mat_n_sim_n_sem <- function(vec_sem_sig,vec_k_sem_info,num_sim,param){
  #Se definen las variables que se van a utilizar
  ## Se carga el vector que contiene 333 materias para que todas las listas
  ##de cada semestre tengan las mismas materias.
  # load("vec_nom_materias_total.RData")
  Materias <- param$vec_nom_materias_total
  
  for(s in 1:length(vec_sem_sig)){#Recorre los semestres
    cat("\n Semestre simulado: ",vec_sem_sig[s])
    gen_mat_n_sim_1_sem(vec_k_sem_info[s],vec_sem_sig[s],num_sim,param)
  }
}


# gen_mat_max_num_gpos_real -----------------------------------------------
#' Title gen_mat_max_num_gpos_real: Función que guarda y genera la matriz
#' "mat_max_num_gpos_real" que tiene 4 columnas (Semestre, Materia, Horario, 
#' Número de grupos) que contiene la información del máximo número de
#' grupos reales por semestre y por hora.
#'
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return mat_max_num_gpos_real: Matriz de 4 columnas: Semestre, Materia,
#' Horario, Número de grupos que contiene la información del máximo número de
#' grupos reales por semestre y por hora.
#'
gen_mat_max_num_gpos_real <- function(param){
  #Se definen las variables que se van a utilizar
  num_col_Materia <- arroja_ind_col_RG("Materia") ##1
  num_col_horario <- arroja_ind_col_RG("Horario") ##2
  col_grupos_reales <- arroja_ind_col_RG("Grupos_Reales") ##3
  mat_max_num_gpos_real <- data.frame(Semestre = 0,Materia = 0,Horario = 0,
                                      Núm.Gpos.Reales = 0)
  
  for(d in 1:length(param$nombre_sem)){
    Semestre <- param$nombre_sem[d]
    cat("\n sem_info = ",Semestre)
    
    nom_mat_real_gpos <- paste0("mat_real_grupos por semestre/mat_real_grupos_",Semestre,".RData")
    load(nom_mat_real_gpos)
    
    num_max_aux <- max(mat_real_grupos[,col_grupos_reales])
    mat_real_grupos_aux <- mat_real_grupos[mat_real_grupos[,col_grupos_reales]==num_max_aux,
                                           c(num_col_Materia,num_col_horario,col_grupos_reales)]
    
    ## Se agrega la variable de "semestre" y se acomodan las columnas
    if(dim(mat_real_grupos_aux)[1] == 1){
      mat_max_num_gpos_real <- rbind(mat_max_num_gpos_real,cbind(Semestre,mat_real_grupos_aux))
    }else{##Si hay más de un grupo con el número máximo de grupos
      for(k in 1:dim(mat_real_grupos_aux)[1]){
        mat_max_num_gpos_real <- rbind(mat_max_num_gpos_real,cbind(Semestre,mat_real_grupos_aux[k,]))
      }
    }
  }##Fin de for(d)
  
  #Se quita el renglón de ceros
  mat_max_num_gpos_real <- mat_max_num_gpos_real[-1,]
  
  View(mat_max_num_gpos_real)
  
  save(mat_max_num_gpos_real,file = "num_max_grupos/mat_max_num_gpos_real.RData")
  write.csv(mat_max_num_gpos_real,file = "num_max_grupos/mat_max_num_gpos_real.csv",row.names = F)
  return(mat_max_num_gpos_real)
}


# gen_mat_max_num_gpos_sim ------------------------------------------------
#' Title gen_mat_max_num_gpos_sim: Función que guarda y genera la matriz
#' "mat_max_num_gpos_sim" que tiene 4 columnas (Semestre, Materia, Horario, 
#' Número de grupos) que contiene la información del máximo número de
#' grupos simulados por semestre y por hora.
#'
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return mat_max_num_gpos_sim: Matriz de 4 columnas: Semestre, Materia,
#' Horario, Número de grupos que contiene la información del máximo número de
#' grupos simulados por semestre y por hora.
#'
gen_mat_max_num_gpos_sim <- function(sem_ini,sem_fin,param){
  #Se definen las variables que se van a utilizar
  num_col_Materia <- arroja_ind_col_SG("Materia") ##1
  num_col_horario <- arroja_ind_col_SG("Horario") ##2
  col_grupos_simulados <- arroja_ind_col_SG("Grupos_Simulados") ##3
  mat_max_num_gpos_sim <- data.frame(Semestre = 0,Materia = 0,Horario = 0,
                                     Núm.Gpos.Simulados = 0)
  
  semestres_sim <- (sem_ini:sem_fin)[(sem_ini:sem_fin) %% 10>0 &(sem_ini:sem_fin) %% 10<3]
  
  for(d in 1:length(semestres_sim)){
    sem_info <- semestres_sim[d]
    cat("\n sem_info = ",sem_info)
    
    nom_archivo <- paste0("Matrices m_grande_total/m_grande_total_20081_",sem_info,".RData")
    load(nom_archivo)
    param$m_grande_total = m_grande_total
    mat_simula_grupos <- guarda_mat_simula_grupos_1_sem(param)
    
    num_max_aux <- max(mat_simula_grupos[,col_grupos_simulados])
    mat_simula_grupos_aux <- mat_simula_grupos[mat_simula_grupos[,col_grupos_simulados]==num_max_aux,
                                               c(num_col_Materia,num_col_horario,col_grupos_simulados)]
    
    ## Se agrega la variable de "semestre" y se acomodan las columnas
    if((sem_info%%2)==0){
      Semestre <- param$sem_fin + 9
    }else{
      Semestre <- param$sem_fin + 1
    }
    if(dim(mat_simula_grupos_aux)[1] == 1){
      mat_max_num_gpos_sim <- rbind(mat_max_num_gpos_sim,cbind(Semestre,mat_simula_grupos_aux))
    }else{##Si hay más de un grupo con el número máximo de grupos
      for(k in 1:dim(mat_simula_grupos_aux)[1]){
        mat_max_num_gpos_sim <- rbind(mat_max_num_gpos_sim,cbind(Semestre,mat_simula_grupos_aux[k,]))
      }
    }
  }##Fin de for(d)
  
  
  #Se quita el renglón de ceros
  mat_max_num_gpos_sim <- mat_max_num_gpos_sim[-1,]
  
  # View(mat_max_num_gpos_sim)
  
  save(mat_max_num_gpos_sim,file = "num_max_grupos/mat_max_num_gpos_sim.RData")
  write.csv(mat_max_num_gpos_sim,file = "num_max_grupos/mat_max_num_gpos_sim.csv",row.names = F)
  
  return(mat_max_num_gpos_sim)
}



# gen_mat_nom_cap_salon_x_sem ---------------------------------------------
#' Title gen_mat_nom_cap_salon_x_sem: Función que genera la matriz llamada
#' "mat_nom_cap_salon" que contiene en su primer columna los nombres de los
#' salones de la facultad y en las siguientes columas se tienen todas sus
#' capacidades por semestre, desde el 2013-1 hasta el 20201.
#'
#' @return mat_nom_cap_salon: Matriz que contiene en su primer columna los
#' nombres de los salones de la facultad y en las siguientes columnas sus
#' diferentes capacidades (lugares disponibles por salón).
#'
gen_mat_nom_cap_salon_x_sem <- function(){
  #Se definen las variables que se van a utilizar
  num_col_salon <- arroja_ind_col_MG("Salon")##7
  #Se carga la matriz "m_grande" de los semestres 2013-1 al 2020-1
  nom_archivo_MGT <- paste0("Matrices m_grande_total/m_grande_total_20131_20201.RData")
  
  if(!file.exists(nom_archivo_MGT)){
    vec_excepciones <- "Inglés"
    param$sem_ini = 20131##Inicio de información real
    param$sem_fin = 20201##Fin de información real
    param$sem_sig = 20202##Semestre de simulación
    param$Semestres = (param$sem_ini:param$sem_fin)[(param$sem_ini:param$sem_fin) 
                                                    %% 10>0 &(param$sem_ini:param$sem_fin) %% 10<3]
    param$nombre_sem = as.character(param$Semestres)
    param$n_semestres_anteriores = length(param$Semestres)
    
    m_grande_total <- gen_m_grande_total(vec_excepciones,param)
    param$m_grande_total = m_grande_total
  }else{
    load(nom_archivo_MGT)
  }
  
  mat_nom_cap_salon <- data.frame(Salon = 0,Cap_20131 = 0,Cap_20132 = 0,
                                  Cap_20141 = 0,Cap_20142 = 0,
                                  Cap_20151 = 0,Cap_20152 = 0,
                                  Cap_20161 = 0,Cap_20162 = 0,
                                  Cap_20171 = 0,Cap_20172 = 0,
                                  Cap_20181 = 0,Cap_20182 = 0,
                                  Cap_20191 = 0,Cap_20192 = 0,
                                  Cap_20201 = 0)
  
  ## Se eligen los nombres de los salones que se van a utilizar (se eliminan
  ##los nombres diferentes como de personas o NA) por casos
  nombres_salon <- unique(m_grande_total[,num_col_salon])
  
  ### Se eliminan: Vacíos / NA / "Presentación" ###
  nombres_salon <- nombres_salon[nombres_salon != ""]
  nombres_salon <- nombres_salon[!is.na(nombres_salon)]
  nombres_salon <- nombres_salon[nombres_salon != "Presentación"]
  
  ### EDIFICIO O / EDIFICIO P###
  vec_ind_aux <- 0
  for(d in 1:length(nombres_salon)){
    renglon <- c(nombres_salon[d],rep(0,15))
    texto <- substr(nombres_salon[d],1,2)
    if(texto=="O1" || texto=="O2" || texto=="P1" || texto=="P2"){
      mat_nom_cap_salon <- rbind(mat_nom_cap_salon,renglon)
      vec_ind_aux <- c(vec_ind_aux,d)
    }
  }
  vec_ind_aux <- vec_ind_aux[-1]#Se quita el cero inicial
  nombres_salon <- nombres_salon[-vec_ind_aux]
  
  ### Inicio con números (0,1,2,3) ###
  vec_ind_aux <- 0
  for(d in 1:length(nombres_salon)){
    renglon <- c(nombres_salon[d],rep(0,15))
    texto <- substr(nombres_salon[d],1,1)
    for(num in 0:3){##Los salones que inician con número, empiezan con 0,1,2 o 3
      if(texto == num){
        mat_nom_cap_salon <- rbind(mat_nom_cap_salon,renglon)
        vec_ind_aux <- c(vec_ind_aux,d)
      }
    }
  }
  vec_ind_aux <- vec_ind_aux[-1]#Se quita el cero inicial
  nombres_salon <- nombres_salon[-vec_ind_aux]
  
  ### "Taller" ###
  vec_ind_aux <- 0
  for(d in 1:length(nombres_salon)){
    renglon <- c(nombres_salon[d],rep(0,15))
    texto <- substr(nombres_salon[d],1,6)
    if(texto=="Taller"){
      mat_nom_cap_salon <- rbind(mat_nom_cap_salon,renglon)
      vec_ind_aux <- c(vec_ind_aux,d)
    }
  }
  vec_ind_aux <- vec_ind_aux[-1]#Se quita el cero inicial
  nombres_salon <- nombres_salon[-vec_ind_aux]
  
  ### "Laboratorio" ###
  vec_ind_aux <- 0
  for(d in 1:length(nombres_salon)){
    renglon <- c(nombres_salon[d],rep(0,15))
    texto <- substr(nombres_salon[d],1,11)
    if(texto=="Laboratorio"){
      mat_nom_cap_salon <- rbind(mat_nom_cap_salon,renglon)
      vec_ind_aux <- c(vec_ind_aux,d)
    }
  }
  vec_ind_aux <- vec_ind_aux[-1]#Se quita el cero inicial
  nombres_salon <- nombres_salon[-vec_ind_aux]
  
  ### "Salón" ###
  vec_ind_aux <- 0
  for(d in 1:length(nombres_salon)){
    renglon <- c(nombres_salon[d],rep(0,15))
    texto <- substr(nombres_salon[d],1,5)
    if(texto=="Salón"){
      mat_nom_cap_salon <- rbind(mat_nom_cap_salon,renglon)
      vec_ind_aux <- c(vec_ind_aux,d)
    }
  }
  vec_ind_aux <- vec_ind_aux[-1]#Se quita el cero inicial
  nombres_salon <- nombres_salon[-vec_ind_aux]
  
  ### "Aula" ###
  vec_ind_aux <- 0
  for(d in 1:length(nombres_salon)){
    renglon <- c(nombres_salon[d],rep(0,15))
    texto <- substr(nombres_salon[d],1,4)
    if(texto=="Aula"){
      mat_nom_cap_salon <- rbind(mat_nom_cap_salon,renglon)
      vec_ind_aux <- c(vec_ind_aux,d)
    }
  }
  vec_ind_aux <- vec_ind_aux[-1]#Se quita el cero inicial
  nombres_salon <- nombres_salon[-vec_ind_aux]
  
  ### El salón S4 se agrega manualmente para esta matriz, dado que
  ###es el único caso con esas características
  renglon <- 
    mat_nom_cap_salon <- rbind(mat_nom_cap_salon,c("S4",rep(0,15)))
  
  ## Se quita el renglón inicial de ceros
  mat_nom_cap_salon <- mat_nom_cap_salon[mat_nom_cap_salon[,1]!=0,]
  # View(mat_nom_cap_salon)
  
  ## Se llena el resto de la matriz (con las capacidades de los salones)
  sem_ini <- 20131
  sem_fin <- 20201
  semestres = (sem_ini:sem_fin)[(sem_ini:sem_fin) %% 10>0 &(sem_ini:sem_fin) %% 10<3]
  num_col_lugares <- arroja_ind_col_MG("Lugares")##5
  
  
  for(s in 2:(length(semestres)+1)){##Recorre los semestres
    sem_info <- semestres[s-1]
    nom_archivo <- paste0("m_grande por semestre/m_grande_",sem_info,".RData")
    load(nom_archivo)
    for(r in 1:dim(mat_nom_cap_salon)[1]){##Recorre los renglones de "mat_nom_cap_salon"
      nom_salon <- mat_nom_cap_salon[r,1]
      capacidades <- m_grande[m_grande[,num_col_salon] == nom_salon,num_col_lugares]
      ## En caso de no tener capacidad de salón se pone un -1 en la entrada (r,s)
      if(length(capacidades[!is.na(capacidades)]) == 0){
        mat_nom_cap_salon[r,s] <- -1
      }else{
        mat_nom_cap_salon[r,s] <- max(capacidades[!is.na(capacidades)])
      }
    }
  }
  save(mat_nom_cap_salon,file = "mat_nom_cap_salon.RData")
  # View(mat_nom_cap_salon)
  return(mat_nom_cap_salon)
}


# selecciona_cap_salon ----------------------------------------------------
#' Title selecciona_cap_salon: Función que arroja una matriz de dos
#' columnas, en la primera contiene los nombres de los salones de la
#' facultad y en la segunda la capacidad máxima de cada salón de
#' "mat_nom_cap_salon".
#'
#' @param mat_nom_cap_salon: Matriz que contiene en su primer columna los
#' nombres de los salones de la facultad y en las siguientes columnas sus
#' diferentes capacidades (lugares disponibles por salón).
#' 
#' @example mat_nom_cap_salon[25,] <- c("O132",0,-1,...,27,60)
#' 
#' @return mat_cap_salon: Matriz de dos columnas, en la primera contiene
#' los nombres de los salones de la facultad y en la segunda la capacidad
#' máxima de cada salón de "mat_nom_cap_salon".
#'
selecciona_cap_salon <- function(mat_nom_cap_salon){
  #Se definen las variables que se van a utilizar
  mat_cap_salon <- matrix(0,nrow = dim(mat_nom_cap_salon)[1],ncol = 2)
  mat_cap_salon[,1] <- mat_nom_cap_salon[,1]
  mat_aux <- mat_nom_cap_salon[,-1]
  
  for(d in 1:dim(mat_aux)[1]){##Recorre los renglones
    mat_cap_salon[d,2] <- max(as.numeric(mat_aux[d,]))
  }
  
  return(mat_cap_salon)
}



# gen_mat_materias_rep_1_sem ----------------------------------------------
#' Title gen_mat_materias_rep_1_sem: Función que recibe como parámetros
#' "sem_info" y "profesor", arroja una matriz de cuatro columnas (semestre,
#' profesor, materia, hora) en caso de que "profesor" tenga más de una
#' materia asignada a la misma hora.
#'
#' @param sem_info: Semestre del que se desea obtener información.
#' @param profesor: Nombre de algún profesor de la Facultad de Ciencias
#' 
#' @example sem_info <- 20182
#' @examples profesor <- "Arrigo Coen Coria"
#'
#' @return mat_materias_rep_1_sem: Matriz de cuatro columnas (semestre, profesor,
#' materia, hora) que tiene información en caso de que "profesor" tenga más
#' de una materia asignada a la misma hora, de un semestre.
#'
gen_mat_materias_rep_1_sem <- function(sem_info,profesor){
  #Se definen las variables que se van a utilizar:
  mat_materias_rep_1_sem <- data.frame(Semestre = 0,Profesor = 0,Materia = 0,Hora = 0)
  num_col_Materia <- arroja_ind_col_MG("Materia")##1
  num_col_Profesor <- arroja_ind_col_MG("Profesor")##2
  num_col_Horario <- arroja_ind_col_MG("Horario")##3
  num_col_horario_num <- arroja_ind_col_MG("horario_num")##4
  # num_col_Lugares <- arroja_ind_col_MG("Lugares")##5
  num_col_Alumnos <- arroja_ind_col_MG("Alumnos")##6
  num_col_Semestre <- arroja_ind_col_MG("Semestre")##11
  
  #Se carga la matriz "m_grande" de "sem_info"
  nom_archivo <- paste0("m_grande por semestre/m_grande_",sem_info,".RData")
  load(nom_archivo)
  
  #Se eliminan los renglones con información repetida
  m_grande = unique(m_grande)
  
  ##Se quitan los renglones vacíos de la matriz:
  m_grande <- m_grande[m_grande[1:(nrow(m_grande)),1]!=0,]
  
  #Se generan las matrices con la información de los grupos repetidos:
  # matriz_con_rep <- matrix(0,ncol = ncol(m_grande))
  # mat_resumen_rep <- matrix(0,ncol = 5)
  # Horarios <- unique(m_grande[,num_col_Horario])##String, no números
  Horarios <- unique(m_grande[,num_col_horario_num])##Números, no string
  
  prof_iguales <- m_grande[m_grande[,num_col_Profesor] == profesor, ]
  if(dim(prof_iguales)[1] > 1){
    for(h in 1:length(Horarios)){
      # horarios_iguales <- prof_iguales[prof_iguales[,num_col_Horario] == Horarios[h],]
      horarios_iguales <- prof_iguales[prof_iguales[,num_col_horario_num] == Horarios[h],]
      n_rep <- nrow(horarios_iguales)
      if(n_rep>1){
        num_alumnos <- horarios_iguales[,num_col_Alumnos]
        alum_diferentes <- 0 #Variable binaria, vale 0 si el número de alumnos es igual
        for(k in 2:n_rep){
          if(num_alumnos[1]!=num_alumnos[k]){
            alum_diferentes <- 1 #Variable binaria, vale 1 si el número de alumnos es diferente
          }
        }
        horarios_iguales <- horarios_iguales[,c(num_col_Semestre,num_col_Profesor,num_col_Materia,num_col_horario_num)]
        names(horarios_iguales) <- names(mat_materias_rep_1_sem)
        mat_materias_rep_1_sem <- rbind(mat_materias_rep_1_sem,horarios_iguales)
        # mat_resumen_rep <- rbind(mat_resumen_rep,c(sem_info,profesor,Horarios[h],n_rep,alum_diferentes))
      }
    }##Fin de for de Horarios
    ## Se quita el renglón inicial de ceros
    mat_materias_rep_1_sem <- mat_materias_rep_1_sem[mat_materias_rep_1_sem[,1]!=0,]
    # mat_resumen_rep <- mat_resumen_rep[mat_resumen_rep[,1]!=0,]
  }else{
    #No hay profesores con horarios iguales en diferentes materias
    mat_materias_rep_1_sem <- data.frame(Semestre = 0,Profesor = 0,Materia = 0,Hora = 0)
  }
  
  # View(mat_resumen_rep)
  return(mat_materias_rep_1_sem)
}


# gen_mat_materias_rep ----------------------------------------------------
#' Title gen_mat_materias_rep: Función que recibe como parámetros
#' "sem_ini" y "sem_fin", arroja una matriz de cuatro columnas (semestre,
#' profesor, materia, hora) en caso de que "profesor" tenga más de una
#' materia asignada a la misma hora, para todos los profesores en el
#' intervalo de semestres de "sem_ini" hasta "sem_fin".
#'
#' @param sem_ini: Semestre en el que se inicia la búsqueda de información.
#' @param sem_fin: Semestre en el que se finaliza la búsqueda de
#' información.
#'
#' @example sem_ini <- 20081
#' @example sem_fin <- 20181
#'
#' @return mat_materias_rep: Matriz de cuatro columnas (semestre, profesor,
#' materia, hora) que tiene información en caso de que "profesor" tenga más
#' de una materia asignada a la misma hora, de todos los profesores en los
#' semestres en el intervalo de semestres de "sem_ini" hasta "sem_fin".
#'
gen_mat_materias_rep <- function(param){
  #Se definen las variables que se van a utilizar:
  mat_materias_rep <- data.frame(Semestre = 0,Profesor = 0,
                                 Materia = 0,Hora = 0)
  m_grande_total <- param$m_grande_total
  # View(m_grande_total)
  num_col_prof <- arroja_ind_col_MG("Profesor")##2
  Profesores <- unique(m_grande_total[,num_col_prof])
  
  for(s in 1:length(semestres)){
    cat("\n s = ",s)
    for(p in 1:length(Profesores)){
      cat("\n p = ",p)
      # mat_materias_rep_1_sem <- gen_mat_materias_rep_1_sem(semestres[s],Profesores[p])
      mat_materias_rep_1_sem <- gen_mat_materias_rep_1_sem("20201",Profesores[p])
      mat_materias_rep <- rbind(mat_materias_rep,mat_materias_rep_1_sem)
    }
  }
  
  if(sum(as.numeric(mat_materias_rep[,1])) == 0){
    cat("\n No se encontraron profesores con horarios iguales en diferentes materias")
  }else{
    ## Se quitan los renglones de ceros
    mat_materias_rep <- mat_materias_rep[mat_materias_rep[,1]!=0,]
  }
  # View(mat_materias_rep)
  return(mat_materias_rep)
}


# checa_nom_1_materia_en_vec ----------------------------------------------
#' Title checa_nom_1_materia_en_vec: Función que recibe como parámetros el
#' nombre de una materia y el vector con los nombres de las materias e
#' imprime una lista con los diferentes nombres que pudiera tener "materia"
#' en "vec_nom_materias_total"
#' Por ejemplo "Estadística III" y "Modelos de Supervivencia y de Series
#' de Tiempo".
#'
#' @param materia: Nombre de algún curso impartido en la FC.
#' @param vec_nom_materias_total: Vector que contiene el nombre de las
#' materias sin repetición, conservando los nombres más recientes. 
#'
#' @example materia <- "Estadística III"
#' @example vec_nom_materias_total <- c("Robótica","Inglés VI","Sistemas
#' Dinámicos no Lineales",...,"Almacenes y Minería de Datos")
#'
checa_nom_1_materia_en_vec <- function(materia,vec_nom_materias_total){
  #' Se carga la matriz m_grande_total de 2008-1 a 2020-1 de la cual
  #' se va a obtener la lista de nombres que se desea
  load("Matrices m_grande_total/m_grande_total_20081_20201.RData")
  
  #Se definen las variables que se van a utilizar
  num_col_Materia <- arroja_ind_col_MG("Materia")##1
  num_col_NomMat_Act2000 <- arroja_ind_col_MG("NomMat_Act2000")##23
  num_col_NomMat_MAp2017 <- arroja_ind_col_MG("NomMat_MAp2017")##29
  vec_materias <- unique(m_grande_total[,num_col_Materia])##531
  
  var_aux <- vec_nom_materias_total[vec_nom_materias_total == materia]
  # var_aux <- vec_nom_materias_total[vec_nom_materias_total == "materia"]
  if(length(var_aux) != 0){#Si el nombre de "materia" se encuentra en el vector
    cat("\n\n La materia ",materia," se encuentra en el vector como ",materia)
  }else{
    ind_materia <- checa_ind_materia(materia,m_grande_total)
    
    if(length(ind_materia) != 0){#Si "materia" se encuentra en el vector con otro nombre
      mat_aux <- m_grande_total[ind_materia,c(num_col_Materia,
                                              num_col_NomMat_Act2000:num_col_NomMat_MAp2017)]
      mat_aux <- unique(mat_aux)
      vec_aux <- mat_aux[mat_aux != 0]
      nom_aux <- 0
      
      for(k in 1:length(vec_aux)){
        nom_aux <- c(nom_aux,vec_nom_materias_total[vec_nom_materias_total == vec_aux[k]])
      }
      if(length(nom_aux) > 1){
        #Se quita el cero inicial
        nom_aux <- nom_aux[-1]
        cat("\n\n La materia ",materia," se encuentra en el vector como \n",nom_aux) 
      }
    }else{#Si "materia" NO se encuentra en el vector
      cat("\n\n La materia ",materia," NO se encuentra en el vector")
      cat("\nAgregar la materia al vector: \n (1) SI \n (0) NO")
      agrega_materia = scan(file = "", what = numeric(), n = 1)
      
      if(agrega_materia == 1){
        vec_nom_materias_total <- c(vec_nom_materias_total,materia)
        save(vec_nom_materias_total, file = "vec_nom_materias_total.RData")
        cat("\n La materia ",materia," se agregó al vector")
      }else{
        cat("\n La materia ",materia," NO se agregó al vector")}}}
}


# checa_nom_materias_en_vec -----------------------------------------------
#' Title checa_nom_materias_en_vec: Función que recibe como parámetro el
#' vector con los nombres de las materias e imprime una lista con los
#' diferentes nombres que pudiera tener cada "materia" en "vec_nom_materias_total"
#' Por ejemplo "Estadística III" y "Modelos de Supervivencia y de Series
#' de Tiempo".
#'
#' @param vec_nom_materias_total: Vector que contiene el nombre de las
#' materias sin repetición, conservando los nombres más recientes. 
#'
#' @example vec_nom_materias_total <- c("Robótica","Inglés VI","Sistemas
#' Dinámicos no Lineales",...,"Almacenes y Minería de Datos")
#'
checa_nom_materias_en_vec <- function(vec_nom_materias_total){
  #' Se carga la matriz m_grande_total de 2008-1 a 2020-1 de la cual
  #' se va a obtener la lista de nombres que se desea
  load("Matrices m_grande_total/m_grande_total_20081_20201.RData")
  
  #Se definen las variables que se van a utilizar
  num_col_Materia <- arroja_ind_col_MG("Materia")##1
  vec_materias <- unique(m_grande_total[,num_col_Materia])##531
  
  for(d in 1:length(vec_materias)){
    materia <- vec_materias[d]
    checa_nom_1_materia_en_vec(materia,vec_nom_materias_total)
  }
}



##### PENDIENTES #####

# gen_mat_n_sim_1_sem -----------------------------------------------------
#' Title gen_mat_n_sim_1_sem: Función que arroja una lista con las
#' listas de matrices obtenidas en "gen_list_n_sim_1_materia" para "sem_sig".
#'
#' @param n_semestres_anteriores: Variable tipo "integer" que indica el
#' número de semestres anteriores a "sem_sig" para obtener la información
#' que se necesita para la simulación.
#' @param sem_sig: Semestre del que se obtienen las simulaciones
#' @param num_sim: Número de matrices simuladas.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' 
#' @example n_semestres_anteriores <- 10
#' @example sem_sig <- 20201
#' @example num_sim <- 20
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
#' @return lista_n_sim_por_sem: Lista con las listas de matrices
#' obtenidas en "gen_list_n_sim_1_materia" para "sem_sig".
#'
gen_mat_n_sim_1_sem <- function(num_sim,param){
  ##Se definen las variables que se van a utilizar:
  m_grande_total <- param$m_grande_total
  n_semestres_anteriores <- param$n_semestres_anteriores
  sem_sig <- param$sem_sig
  Materias <- param$vec_nom_materias_total
  lista_n_sim_por_sem <- list()
  nombres_lista <- 0
  
  for(d in 1:length(Materias)){#Recorre las materias
    materia <- Materias[d]
    num_materia <- d
    cat("\n Materia: ",num_materia,", ",materia)
    lista_n_sim_por_sem[[d]] <- gen_list_n_sim_1_materia(materia,num_sim,param)
    nombres_lista <- c(nombres_lista,paste0("lista_materia_sim_",d))}
  #Se quita el cero que está al inicio del vector de nombres
  nombres_lista <- nombres_lista[-1]
  names(lista_n_sim_por_sem) <- nombres_lista
  
  nom_lista_n_sim_x_sem <- gen_nom_list_n_sim_x_sem(sem_sig)
  save(lista_n_sim_por_sem,file = nom_lista_n_sim_x_sem)
  
  return(lista_n_sim_por_sem)
}


# guarda_mat_simula_grupos_1_sem ------------------------------------------------
#' Title guarda_mat_simula_grupos_1_sem: Función que guarda la matriz 
#' "mat_simula_grupos" la cual contiene 24 columnas: Materia, Horario,
#' Número de grupos simulados, Número de alumnos simulados, las últimas 20
#' columnas indican el número de simulaciones del tamaño de grupo, en sus
#' renglones se tiene el número de alumnos de cada grupo simulado.
#'
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return mat_simula_grupos: Matriz con 24 columnas: Materia, Horario,
#' Número de grupos simulados, Número de alumnos simulados, las últimas 20
#' columnas indican el número de simulaciones del tamaño de grupo, en sus
#' renglones se tiene el número de alumnos de cada grupo simulado.
#' 
guarda_mat_simula_grupos_1_sem <- function(param){
  #Se definen las variables que se van a utilizar
  # num_col <- arroja_ind_col_MG("Materia")
  # Materias <- unique(param$m_grande_total[,num_col])
  # load("vec_nom_materias_total.RData")
  Materias <- param$vec_nom_materias_total
  n_materias <- length(Materias)
  
  ##Se define la matriz como data frame para agregar renglones más fácilmente.
  mat_simula_grupos <- data.frame(Materia = 0,Horario = 0,Núm.Gpos.Simulados = 0,
                                  Núm.Al.Simulados = 0,Sim_1 = 0,
                                  Sim_2 = 0,Sim_3 = 0,Sim_4 = 0,Sim_5 = 0,
                                  Sim_6 = 0,Sim_7 = 0,Sim_8 = 0,Sim_9 = 0,
                                  Sim_10 = 0,Sim_11 = 0,Sim_12 = 0,Sim_13 = 0,
                                  Sim_14 = 0,Sim_15 = 0,Sim_16 = 0,Sim_17 = 0,
                                  Sim_18 = 0,Sim_19 = 0,Sim_20 = 0)
  
  for(d in 1:n_materias){
    cat("\nMateria ",d," de ",n_materias)
    materia <- Materias[d]
    
    mat_simula_grupos_una_materia <- gen_mat_simula_grupos_una_materia(materia,param)
    mat_simula_grupos <- rbind(mat_simula_grupos,mat_simula_grupos_una_materia)
  }
  
  ##Se quitan los renglones que tengan número de alumnos simulados = 0
  col_alum_sim_total <- arroja_ind_col_SG("Alumnos_Simulados_Totales") ##4
  mat_simula_grupos <- mat_simula_grupos[mat_simula_grupos[,col_alum_sim_total]>0,]
  
  sem_sig <- param$sem_sig
  # View(mat_simula_grupos)
  # nom_archivo <- paste0("mat_simula_grupos por semestre/mat_simula_grupos_",
  #                       sem_sig,"_V04.RData")
  nom_archivo <- paste0("mat_simula_grupos por semestre/mat_simula_grupos_",
                        sem_sig,".RData")
  save(mat_simula_grupos,file = nom_archivo)
  return(mat_simula_grupos)
}


# guarda_heatmap_1_sem ----------------------------------------------------
#' Title guarda_heatmap_1_sem: Función que guarda todas las imágenes de tipo
#' "jpeg" de las gráficas heatmap para las matrices de datos generadas.
#'
#' @param sem_sig: Semestre del que se obtienen las simulaciones
#' @param num_sim: Número de matrices simuladas.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#'
#' @example sem_sig <- 20202
#' @example num_sim <- 5
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
#' @export jpeg: Imagen de las gráficas de heatmap obtenidas de todas
#' las matrices para cada materia.
#'
guarda_heatmap_1_sem <- function(num_sim,param){
  #Se definen las variables que se van a utilizar
  sem_sig <- param$sem_sig
  num_col_Materia <- arroja_ind_col_MG("Materia")
  num_col_1er_grupo <- arroja_ind_col_RG("col_1er_grupo") ##5
  num_col_ult_grupo <- arroja_ind_col_RG("col_ult_grupo") ##24
  # Materias <- unique(param$m_grande_total[,num_col_Materia])
  # load("vec_nom_materias_total.RData")
  Materias <- param$vec_nom_materias_total
  
  # Se guarda la matriz con el nombre de las materias para
  #saber la materia correspondiente a la gráfica
  # save(Materias,file = "Figuras/Matrices Simuladas/Materias.RData")
  # save(Materias,file = "Materias.RData")
  
  #Se guardan las figuras correspondientes a la varianza
  for(d in 1:length(Materias)){
    cat("\n heatmap ",d," de ",length(Materias))
    materia <- Materias[d]
    num_materia <- d
    
    nom_lista <- paste0("Listas mat_n_sim/lista_mat_",num_sim,"_sim_materia_",
                        num_materia,"_",sem_sig,".RData")
    # nom_lista <- paste0("Listas mat_n_sim_PRUEBAS/lista_mat_",num_sim,"_sim_materia_",
    #                     num_materia,"_",sem_sig,".RData")
    if(file.exists(nom_lista)){
      load(nom_lista)
    }else{
      lista_mat_n_sim <- gen_mat_n_sim_1_materia(materia,num_materia,num_sim,param)
    }
    
    mat_var_alum_x_materia_1_sem <- gen_mat_var_alum_x_materia_1_sem(lista_mat_n_sim,param)
    nom_archivo <- paste0("Figuras/Matrices Simuladas/heatmap_var_alum_materia_",d,"_sem_",sem_sig,".jpeg")
    # nom_archivo <- paste0("Figuras/Matrices Simuladas/heatmap_PRUEBAS_var_materia_",d,"_sem_",sem_sig,".jpeg")
    guarda_una_fig_heatmap(mat_var_alum_x_materia_1_sem,num_materia,nom_archivo)
  }
  
  # Se guardan las figuras correspondientes a la diferencia entre valores reales
  #y la esperanza.
  for(d in 1:length(Materias)){
    cat("\n heatmap ",d," de ",length(Materias))
    materia <- Materias[d]
    num_materia <- d
    
    nom_lista <- paste0("Listas mat_n_sim/lista_mat_",num_sim,"_sim_materia_",
                        num_materia,"_",sem_sig,".RData")
    # nom_lista <- paste0("Listas mat_n_sim_PRUEBAS/lista_mat_",num_sim,"_sim_materia_",
    #                     num_materia,"_",sem_sig,".RData")
    if(file.exists(nom_lista)){
      load(nom_lista)
    }else{
      lista_mat_n_sim <- gen_mat_n_sim_1_materia(materia,num_materia,num_sim,param)
    }
    mat_esp_alum_x_materia_1_sem <- gen_mat_esp_alum_x_materia_1_sem(lista_mat_n_sim,param)
    mat_real_grupos_una_materia <- gen_mat_real_grupos_una_materia(materia,sem_sig,param)
    mat_dif_alum_x_materia_1_sem <- gen_mat_dif_alum_x_materia_1_sem(mat_esp_alum_x_materia_1_sem,mat_real_grupos_una_materia)
    nom_archivo <- paste0("Figuras/Matrices Simuladas/heatmap_dif_real_esp_materia_",d,"_sem_",sem_sig,".jpeg")
    # nom_archivo <- paste0("Figuras/Matrices Simuladas/PRUEBAS_V01/heatmap_PRUEBAS_V02_dif_real_esp_materia_",d,"_sem_",sem_sig,".jpeg")
    guarda_una_fig_heatmap(mat_dif_alum_x_materia_1_sem,num_materia,nom_archivo)
    
    mat_real_aux <- mat_real_grupos_una_materia[,num_col_1er_grupo:num_col_ult_grupo]
    mat_real_aux <- as.matrix(mat_real_aux,nrow=dim(mat_real_aux)[1],ncol=dim(mat_real_aux)[1])
    mat_dif_relativa_alum_x_materia_1_sem <- matrix(0,nrow = dim(mat_real_aux)[1],
                                                    ncol = dim(mat_real_aux)[2])
    if(sum(as.numeric(mat_real_aux)) != 0){###SUMAR RENGLONES Y COLS
      #Se hace la división en caso de que la matriz de datos reales sea
      #distinta de cero en todas sus entradas
      # mat_dif_relativa_alum_x_materia_1_sem <- mat_dif_alum_x_materia_1_sem/mat_real_aux
      for(c in 1:dim(mat_real_aux)[2]){#Recorre columnas
        for(r in 1:dim(mat_real_aux)[1]){#Recorre renglones
          if(mat_dif_alum_x_materia_1_sem[r,c]==0){
            mat_dif_relativa_alum_x_materia_1_sem[r,c] <- 0
          }else{
            mat_dif_relativa_alum_x_materia_1_sem[r,c] <- mat_dif_alum_x_materia_1_sem[r,c]/mat_real_aux[r,c]
          }}#for r
      }#for c
    }else{
      mat_dif_relativa_alum_x_materia_1_sem <- mat_real_aux
    }
    nom_archivo_2 <- paste0("Figuras/Matrices Simuladas/heatmap_mat_dif_relativa_alum_materia_",d,"_sem_",sem_sig,".jpeg")
    # nom_archivo_2 <- paste0("Figuras/Matrices Simuladas/PRUEBAS_V01/heatmap_PRUEBAS_V02_mat_dif_relativa_alum_materia_",d,"_sem_",sem_sig,".jpeg")
    guarda_una_fig_heatmap(mat_dif_relativa_alum_x_materia_1_sem,num_materia,nom_archivo_2)
  }#for d
}


# guarda_heatmap_n_sem ----------------------------------------------------
#' Title guarda_heatmap_n_sem: Función que manda a llamar a la función
#' "guarda_heatmap_1_sem" la cual se encarga de graficar y guardar las
#' figuras de los "heatmaps" de las diferencias de alumnos, grupos y su
#' varianza.
#'
#' @param vec_sem_sig: Vector con los semestres de los que se desean obtener
#' las simulaciones. Deben estar ordenados del más antiguo al más reciente.
#' @param vec_k_sem_info: Vector con el número de semestres anteriores para
#' cada "sem_sig" del vector "vec_sem_sig"
#' @param num_sim: Número de matrices simuladas.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#'
#' @example vec_sem_sig <- c(20131,20152,20182,20201)
#' @example vec_k_sem_info <- c(10,15,21,24)##Se inicia en 2008-1
#' @example num_sim <- 10
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' 
guarda_heatmap_n_sem <- function(vec_sem_sig,vec_k_sem_info,num_sim,param){
  ##Se definen las variables que se van a utilizar:
  num_col_Semestre <- arroja_ind_col_MG("Semestre")##11
  MGT <- param$m_grande_total
  nom_col_real_alum <- "Alumnos_Reales_Totales"
  nom_col_real_gpos <- "Grupos_Reales"
  mat_aux_real_alum <- matrix(0,nrow = 15,ncol = length(vec_sem_sig))
  mat_aux_real_gpos <- matrix(0,nrow = 15,ncol = length(vec_sem_sig))
  
  ##Se realiza un ciclo para cada semestre
  for(s in 1:length(vec_sem_sig)){
    # cat("\n s = ",s)
    for(d in 1:length(param$sem_totales)){
      # cat("\n d = ",d)
      if(param$sem_totales[d]==vec_sem_sig[s]){
        ind_sem_ini <- d-vec_k_sem_info[s]
        # cat("\n sem_ini = ",param$sem_totales[ind_sem_ini])
        
        ind_sem_fin <- d-1
        # cat("\n sem_fin = ",param$sem_totales[ind_sem_fin])
      }
    }#Fin for(d)
    ##Se definen las variables de param que cambian:
    param$sem_ini = param$sem_totales[ind_sem_ini]
    param$sem_fin = param$sem_totales[ind_sem_fin]
    param$sem_sig = vec_sem_sig[s]
    param$Semestres = param$sem_totales[ind_sem_ini:ind_sem_fin]
    param$nombre_sem = as.character(param$Semestres)
    param$n_semestres_anteriores = length(param$Semestres)
    
    MG_aux_1 <- MGT[MGT[,num_col_Semestre]<= param$sem_fin,]
    MG_aux_2 <- MG_aux_1[MG_aux_1[,num_col_Semestre]>= param$sem_ini,]
    param$m_grande_total = MG_aux_2
    
    guarda_heatmap_1_sem(num_sim,param)
    
    ##Se obtienen las matrices que contienen los datos de alumnos totales
    ##reales por semestre
    nom_archivo_real <- paste0("mat_real_grupos por semestre/mat_real_grupos_",
                               vec_sem_sig[s],".RData")
    load(nom_archivo_real)
    mat_aux_real_alum[,s] <- gen_vec_esp_datos_real_1_sem(mat_real_grupos,
                                                           nom_col_real_alum,
                                                           param)
    mat_aux_real_gpos[,s] <- gen_vec_esp_datos_real_1_sem(mat_real_grupos,
                                                           nom_col_real_gpos,
                                                           param)
  }#Fin for(s)
  rownames(mat_aux_real_alum) <- param$nombre_hrs
  colnames(mat_aux_real_alum) <- vec_sem_sig
  rownames(mat_aux_real_gpos) <- param$nombre_hrs
  colnames(mat_aux_real_gpos) <- vec_sem_sig
  
  #Se cargan las matrices con las diferencias de datos
  mat_dif_total_alumnos_x_sem <- gen_mat_dif_total_alumnos_x_sem(vec_sem_sig,vec_k_sem_info,
                                                                 num_sim,param)
  # View(mat_dif_total_alumnos_x_sem)
  mat_dif_total_gpos_x_sem <- gen_mat_dif_total_gpos_x_sem(vec_sem_sig,param)
  # View(mat_dif_total_gpos_x_sem)
  
  nom_archivo <- paste0("dif_relativa_total_de_alumnos_x_sem_",
                        vec_sem_sig[1],"-",vec_sem_sig[length(vec_sem_sig)])
  mat_dif_relativas_alum <- gen_mat_dif_relativas(mat_dif_total_alumnos_x_sem,
                                                  mat_aux_real_alum,nom_archivo)
  colnames(mat_dif_relativas_alum) <- vec_sem_sig
  rownames(mat_dif_relativas_alum) <- param$nombre_hrs
  View(mat_dif_relativas_alum)
  
  nom_archivo <- paste0("dif_relativa_total_de_gpos_x_sem_",
                        vec_sem_sig[1],"-",vec_sem_sig[length(vec_sem_sig)])
  mat_dif_relativas_gpos <- gen_mat_dif_relativas(mat_dif_total_gpos_x_sem,
                                                  mat_aux_real_gpos,nom_archivo)
  colnames(mat_dif_relativas_gpos) <- vec_sem_sig
  rownames(mat_dif_relativas_gpos) <- param$nombre_hrs
  View(mat_dif_relativas_gpos)
}



# simula_grupos -----------------------------------------------------------
#' Title simula_grupos: Función que arroja el vector con el número de grupos
#' simulados por hora, que depende del número de alumnos que se estimaron
#' con modelo hw() Holt-Winters y se simularon.
#'
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @param materia: Nombre de algún curso impartido en la FC.
#' 
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#' @example materia <- "Estadística I"
#'
#' @return vec_grupos_simulados: Vector con el número de grupos simulados
#' por hora
#'
simula_grupos <- function(materia,param){
  # cat("\n Entró a la función simula_grupos")
  nombre_hrs <- param$nombre_hrs
  vec_grupos_simulados <- rep(0,length(nombre_hrs))
  
  mat_simula_grupos <- guarda_mat_simula_grupos_1_sem(param)
  ##Se busca el vector con el número de grupos simulado de materia
  mat_info_grupos <- mat_simula_grupos[mat_simula_grupos[,1]==materia,c(2,3)]
  
  for(d in 1:length(nombre_hrs)){##Se recorren las horas
    for(j in 1:dim(mat_info_grupos)[1]){ ##Se recorren los renglones
      hora <- mat_info_grupos[j,1]
      if(hora==nombre_hrs[d]){
        vec_grupos_simulados[d] <- mat_info_grupos[j,2]
      }
    }
  }
  
  return(vec_grupos_simulados)
}


# guarda_mat_real_grupos_x_sem --------------------------------------------------
#' Title guarda_mat_real_grupos_x_sem: Función que guarda la matriz 
#' "mat_real_grupos" la cual contiene 24 columnas: Materia, Horario,
#' Número de grupos reales, Número de alumnos reales, las últimas 20
#' columnas indican el número de simulaciones del tamaño de grupo, en sus
#' renglones se tiene el número real de alumnos de cada grupo por hora.
#'
#' @param sem_info: Semestre del que se desea obtener información
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example sem_info <- 20182
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return mat_real_grupos: Matriz con 24 columnas: Materia, Horario,
#' Número de grupos reales, Número de alumnos reales, las últimas 20
#' columnas indican el número de simulaciones del tamaño de grupo, en sus
#' renglones se tiene el número real de alumnos de cada grupo por hora.
#' 
guarda_mat_real_grupos_x_sem <- function(sem_info,param){
  #Se definen las variables que se van a utilizar
  num_col <- arroja_ind_col_MG("Materia")
  # Materias <- unique(param$m_grande_total[,num_col])
  # load("vec_nom_materias_total.RData")
  Materias <- param$vec_nom_materias_total
  n_materias <- length(Materias)
  
  ##Se define la matriz como data frame para agregar renglones más fácilmente.
  mat_real_grupos <- data.frame(Materia = 0,Horario = 0,
                                Núm.Gpos.Reales = 0,Núm.Al.Reales = 0,Grupo_1 = 0,
                                Grupo_2 = 0,Grupo_3 = 0,Grupo_4 = 0,Grupo_5 = 0,
                                Grupo_6 = 0,Grupo_7 = 0,Grupo_8 = 0,Grupo_9 = 0,
                                Grupo_10 = 0,Grupo_11 = 0,Grupo_12 = 0,Grupo_13 = 0,
                                Grupo_14 = 0,Grupo_15 = 0,Grupo_16 = 0,Grupo_17 = 0,
                                Grupo_18 = 0,Grupo_19 = 0,Grupo_20 = 0)
  
  for(d in 1:n_materias){
    # cat("\nMateria ",d," de ",n_materias)
    materia <- Materias[d]
    
    mat_real_grupos_una_materia <- gen_mat_real_grupos_una_materia(materia,sem_info,param)
    mat_real_grupos <- rbind(mat_real_grupos,mat_real_grupos_una_materia)
  }
  
  col_grupos_reales <- arroja_ind_col_RG("Grupos_Reales") ##3
  ##Se quitan los renglones que no tienen información de grupos:
  mat_real_grupos <- mat_real_grupos[mat_real_grupos[,col_grupos_reales]>0,]
  
  # View(mat_real_grupos)
  nom_mat_real_gpos <- paste0("mat_real_grupos por semestre/mat_real_grupos_",sem_info,".RData")
  save(mat_real_grupos,file = nom_mat_real_gpos)
  return(mat_real_grupos)
}



# datos_num_max_de_gpos_sim --------------------------------------------------
#' datos_num_max_de_gpos_sim: Función que regresa un vector con los valores del
#' número máximo de grupos simulados por semestre considerando que están
#' dividos por materia y horario.
#'
#' @param sem_ini: Semestre en el que se inicia la búsqueda de
#' información.
#' @param sem_fin: Semestre en el que se finaliza la búsqueda de
#' información para generar la asignación del siguiente semestre.
#'
#' @example sem_ini <- 20132
#' @example sem_fin <- 20202
#'
#' @return vec_num_max_gpos_sim: Vector con los valores del número máximo
#' de grupos simulados.
#'
datos_num_max_de_gpos_sim <- function(sem_ini,sem_fin){
  semestres <- (sem_ini:sem_fin)[(sem_ini:sem_fin) %% 10>0 &(sem_ini:sem_fin) %% 10<3]
  vec_num_max_gpos_sim <- rep(0,length(semestres))
  for(d in 1:length(semestres)){
    nom_archivo <- paste0("mat_simula_grupos por semestre/mat_simula_grupos_",semestres[d],".RData")
    load(nom_archivo)
    vec_num_max_gpos_sim[d] <- max(mat_simula_grupos[,3])
  }
  return(vec_num_max_gpos_sim)
}


# datos_num_max_de_gpos_real --------------------------------------------------
#' datos_num_max_de_gpos_real: Función que regresa un vector con los valores del
#' número máximo de grupos reales por semestre considerando que están
#' dividos por materia y horario.
#'
#' @param sem_ini: Semestre en el que se inicia la búsqueda de
#' información.
#' @param sem_fin: Semestre en el que se finaliza la búsqueda de
#' información para generar la asignación del siguiente semestre.
#'
#' @example sem_ini <- 20081
#' @example sem_fin <- 20201
#'
#' @return vec_num_max_gpos_real: Vector con los valores del número máximo
#' de grupos reales
#'
datos_num_max_de_gpos_real <- function(sem_ini,sem_fin){
  semestres <- (sem_ini:sem_fin)[(sem_ini:sem_fin) %% 10>0 &(sem_ini:sem_fin) %% 10<3]
  vec_num_max_gpos_real <- rep(0,length(semestres))
  col_grupos_reales <- arroja_ind_col_RG("Grupos_Reales") ##3
  for(d in 1:length(semestres)){
    nom_mat_real_gpos <- paste0("mat_real_grupos por semestre/mat_real_grupos_",semestres[d],".RData")
    load(nom_mat_real_gpos)
    
    vec_num_max_gpos_real[d] <- max(mat_real_grupos[,col_grupos_reales])
  }
  return(vec_num_max_gpos_real)
}


# guarda_una_mat_diferencias ---------------------------------------------
#' Title guarda_una_mat_diferencias: Función que arroja la matriz de diferencias
#' por semestre entre datos reales y datos simulados, la cual se utiliza para
#' hacer pruebas de comparación.
#'
#' @param sem_info: Semestre del que se desea obtener información. 
#' @example sem_info <- 20182
#'
#' @return mat_diferencias: Matriz con 24 columnas Materia, Horario,
#' Gpos_Real-Gpos_Sim, Alum_Real-Alum_Sim, las últimas 20 columnas tienen
#' las diferencias por grupos.
#'
guarda_una_mat_diferencias <- function(sem_info){
  ##Se toman las matrices con la información real y la simulada:
  nom_mat_sim_gpos <- paste0("Matrices simula_grupos/mat_simula_grupos_",sem_info,".RData")
  load(nom_mat_sim_gpos)
  nom_mat_real_gpos <- paste0("mat_real_grupos por semestre/mat_real_grupos_",sem_info,".RData")
  load(nom_mat_real_gpos)
  
  ##Se llena la columna 1: "Materia"
  vec_materias <- c(mat_real_grupos[,arroja_ind_col_RG("Materia")],
                    mat_simula_grupos[,arroja_ind_col_SG("Materia")])
  vec_horarios <- c(mat_real_grupos[,arroja_ind_col_RG("Horario")],
                    mat_simula_grupos[,arroja_ind_col_SG("Horario")])
  
  mat_materias_horario <- unique(cbind(vec_materias,vec_horarios))
  
  ## Se define la matriz "mat_diferencias" y se llenan las columnas 1 y 2
  num_renglones <- dim(mat_materias_horario)[1]
  mat_diferencias <- matrix(0,nrow = num_renglones,ncol = 24)
  mat_diferencias[,1:2] <- mat_materias_horario
  
  ##Se llenan las columnas 3-24
  col_1er_grupo_real <- arroja_ind_col_RG("col_1er_grupo") ##5
  col_ult_grupo_real <- arroja_ind_col_RG("col_ult_grupo") ##24
  col_1er_grupo_sim <- arroja_ind_col_SG("col_1er_grupo") ##5
  col_ult_grupo_sim <- arroja_ind_col_SG("col_ult_grupo") ##24
  for(d in 1:num_renglones){
    # cat("\nIteración ",d," de ",num_renglones)
    info_real <- 0
    cont_real <- 1
    while(info_real == 0 && cont_real <= dim(mat_real_grupos)[1]){
      # cat("\n  cont_real = ",cont_real)
      if(mat_diferencias[d,1]==mat_real_grupos[cont_real,arroja_ind_col_RG("Materia")] &&
         mat_diferencias[d,2]==mat_real_grupos[cont_real,arroja_ind_col_RG("Horario")]){
        
        num_gpos_real <- mat_real_grupos[cont_real,arroja_ind_col_RG("Grupos_Reales")]
        num_alum_real <- mat_real_grupos[cont_real,arroja_ind_col_RG("Alumnos_Reales_Totales")]
        vec_gpos_real <- c(mat_real_grupos[cont_real,col_1er_grupo_real:col_ult_grupo_real])
        
        info_real <- 1
      }
      cont_real <- cont_real + 1
    }
    
    ## En caso de que no haya información de esa materia en ese horario en la
    ##matriz "mat_real_grupos"
    if(info_real == 0){
      num_gpos_real <- 0
      num_alum_real <- 0
      vec_gpos_real <- rep(0,20)
    }
    
    info_sim <- 0
    cont_sim <- 1
    while(info_sim == 0 && cont_sim <= dim(mat_simula_grupos)[1]){
      # cat("\n   cont_sim = ",cont_sim)
      if(mat_diferencias[d,1]==mat_simula_grupos[cont_sim,arroja_ind_col_SG("Materia")] &&
         mat_diferencias[d,2]==mat_simula_grupos[cont_sim,arroja_ind_col_SG("Horario")]){
        
        num_gpos_sim <- mat_simula_grupos[cont_sim,arroja_ind_col_SG("Grupos_Simulados")]
        num_alum_sim <- mat_simula_grupos[cont_sim,arroja_ind_col_SG("Alumnos_Simulados_Totales")]
        vec_gpos_sim <- c(mat_simula_grupos[cont_sim,col_1er_grupo_sim:col_ult_grupo_sim])
        
        info_sim <- 1
      }
      cont_sim <- cont_sim + 1
    }
    
    ## En caso de que no haya información de esa materia en ese horario en la
    ##matriz "mat_simula_grupos"
    if(info_sim == 0){
      num_gpos_sim <- 0
      num_alum_sim <- 0
      vec_gpos_sim <- rep(0,20)
    }
    
    mat_diferencias[d,3] <- num_gpos_real - num_gpos_sim
    mat_diferencias[d,4] <- num_alum_real - num_alum_sim
    for(k in 5:24){
      mat_diferencias[d,k] <- as.numeric(vec_gpos_real[k-4]) - as.numeric(vec_gpos_sim[k-4])
    }
  }##Fin for(d)
  
  colnames(mat_diferencias) <- c("Materia","Horario","Gpos_Real-Gpos_Sim","Alum_Real-Alum_Sim",
                                 "Dif_Gpo_1","Dif_Gpo_2","Dif_Gpo_3","Dif_Gpo_4","Dif_Gpo_5",
                                 "Dif_Gpo_6","Dif_Gpo_7","Dif_Gpo_8","Dif_Gpo_9","Dif_Gpo_10",
                                 "Dif_Gpo_11","Dif_Gpo_12","Dif_Gpo_13","Dif_Gpo_14","Dif_Gpo_15",
                                 "Dif_Gpo_16","Dif_Gpo_17","Dif_Gpo_18","Dif_Gpo_19","Dif_Gpo_20")
  nom_mat_dif <- paste0("mat_diferencias por semestre/mat_diferencias_",sem_info,".RData")
  save(mat_diferencias,file = nom_mat_dif)
  nom_mat_dif_csv <- paste0("mat_diferencias por semestre/mat_diferencias_",sem_info,".csv")
  write.csv(mat_diferencias, file = nom_mat_dif_csv,row.names = F)
  # View(mat_diferencias)
  # return(mat_diferencias)
}


# guarda_mat_diferencias --------------------------------------------------
#' Title guarda_mat_diferencias: Función en la que se obtienen todas las
#' matrices "mat_diferencias" para cada semestre desde "sem_ini" hasta
#' "sem_fin".
#'
#' @param sem_ini: Semestre en el que se inicia la búsqueda de
#' información.
#' @param sem_fin: Semestre en el que se finaliza la búsqueda de
#' información para generar la asignación del siguiente semestre.
#'
#' @example sem_ini <- 20132
#' @example sem_fin <- 20201
#' 
guarda_mat_diferencias <- function(sem_ini,sem_fin){
  semestres <- (sem_ini:sem_fin)[(sem_ini:sem_fin) %% 10>0 &(sem_ini:sem_fin) %% 10<3]
  
  vec_para_for <- 1:length(semestres)
  pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
  for(k in vec_para_for){
    setTxtProgressBar(pb, k)
    sem_info <- semestres[k]
    
    guarda_una_mat_diferencias(sem_info)
  }
  close(pb)
}


# guarda_mat_esp_dif ------------------------------------------------------
#' Title guarda_mat_esp_dif: Función que guarda la matriz "mat_esp_dif" la cual
#' tiene la esperanza de los datos de los valores de la matriz de diferencias.
#'
#' @param sem_ini: Semestre en el que se inicia la búsqueda de
#' información.
#' @param sem_fin: Semestre en el que se finaliza la búsqueda de
#' información para generar la asignación del siguiente semestre.
#'
#' @example sem_ini <- 20132
#' @example sem_fin <- 20201
#' 
guarda_mat_esp_dif <- function(sem_ini,sem_fin){
  semestres <- (sem_ini:sem_fin)[(sem_ini:sem_fin) %% 10>0 &(sem_ini:sem_fin) %% 10<3]
  mat_esp_dif <- matrix(0,nrow = length(semestres),ncol = 23)
  mat_esp_dif[,1] <- semestres
  
  vec_para_for <- 1:length(semestres)
  pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
  for(k in vec_para_for){##Recorre los semestres
    setTxtProgressBar(pb, k)
    
    nom_mat_dif <- paste0("mat_diferencias por semestre/mat_diferencias_",semestres[k],".RData")
    load(nom_mat_dif)
    
    for(d in 3:dim(mat_diferencias)[2]){##Recorre las columnas
      mat_esp_dif[k,d-1] <- mean(as.numeric(mat_diferencias[,d]))
    }
  }
  close(pb)
  colnames(mat_esp_dif) <- c("Semestre","E[Gpos_Real-Gpos_Sim]",
                             "E[Alum_Real-Alum_Sim]","E[Gpo_1]","E[Gpo_2]","E[Gpo_3]",
                             "E[Gpo_4]","E[Gpo_5]","E[Gpo_6]","E[Gpo_7]","E[Gpo_8]",
                             "E[Gpo_9]","E[Gpo_10]","E[Gpo_11]","E[Gpo_12]","E[Gpo_13]",
                             "E[Gpo_14]","E[Gpo_15]","E[Gpo_16]","E[Gpo_17]","E[Gpo_18]",
                             "E[Gpo_19]","E[Gpo_20]")
  
  nom_mat_esp <- paste0("mat_dif_real-sim_esp_var_sd/mat_esp_dif.RData")
  save(mat_esp_dif,file = nom_mat_esp)
  
  ## Se guardan los datos en un archivo de excel para graficar
  write.csv(mat_esp_dif, file = "mat_dif_real-sim_esp_var_sd/mat_esp_dif.csv",row.names = F)
}


# guarda_mat_var_dif ------------------------------------------------------
#' Title guarda_mat_var_dif: Función que guarda la matriz "mat_var_dif" la
#' cual tiene la varianza de los datos de los valores de la matriz de
#' diferencias.
#'
#' @param sem_ini: Semestre en el que se inicia la búsqueda de
#' información.
#' @param sem_fin: Semestre en el que se finaliza la búsqueda de
#' información para generar la asignación del siguiente semestre.
#'
#' @example sem_ini <- 20132
#' @example sem_fin <- 20201
#' 
guarda_mat_var_dif <- function(sem_ini,sem_fin){
  semestres <- (sem_ini:sem_fin)[(sem_ini:sem_fin) %% 10>0 &(sem_ini:sem_fin) %% 10<3]
  mat_var_dif <- matrix(0,nrow = length(semestres),ncol = 23)
  mat_var_dif[,1] <- semestres
  
  vec_para_for <- 1:length(semestres)
  pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
  for(k in vec_para_for){##Recorre los semestres
    setTxtProgressBar(pb, k)
    
    nom_mat_dif <- paste0("mat_diferencias por semestre/mat_diferencias_",semestres[k],".RData")
    load(nom_mat_dif)
    
    for(d in 3:dim(mat_diferencias)[2]){##Recorre las columnas
      mat_var_dif[k,d-1] <- var(as.numeric(mat_diferencias[,d]))
    }
  }
  close(pb)
  
  colnames(mat_var_dif) <- c("Semestre","Var(Gpos_Real-Gpos_Sim)",
                             "Var(Alum_Real-Alum_Sim)","Var(Gpo_1)","Var(Gpo_2)",
                             "Var(Gpo_3)","Var(Gpo_4)","Var(Gpo_5)","Var(Gpo_6)",
                             "Var(Gpo_7)","Var(Gpo_8)","Var(Gpo_9)","Var(Gpo_10)",
                             "Var(Gpo_11)","Var(Gpo_12)","Var(Gpo_13)","Var(Gpo_14)",
                             "Var(Gpo_15)","Var(Gpo_16)","Var(Gpo_17)","Var(Gpo_18)",
                             "Var(Gpo_19)","Var(Gpo_20)")
  
  nom_mat_var <- paste0("mat_dif_real-sim_esp_var_sd/mat_var_dif.RData")
  save(mat_var_dif,file = nom_mat_var)
  ## Se guardan los datos en un archivo de excel para graficar
  write.csv(mat_var_dif, file = "mat_dif_real-sim_esp_var_sd/mat_var_dif.csv",row.names = F)
}


# guarda_mat_sd_dif ------------------------------------------------------
#' Title guarda_mat_sd_dif: Función que guarda la matriz "mat_sd_dif" la
#' cual tiene la desviación estándar de los datos de los valores de la
#' matriz de diferencias.
#'
#' @param sem_ini: Semestre en el que se inicia la búsqueda de
#' información.
#' @param sem_fin: Semestre en el que se finaliza la búsqueda de
#' información para generar la asignación del siguiente semestre.
#'
#' @example sem_ini <- 20132
#' @example sem_fin <- 20201
#' 
guarda_mat_sd_dif <- function(sem_ini,sem_fin){
  semestres <- (sem_ini:sem_fin)[(sem_ini:sem_fin) %% 10>0 &(sem_ini:sem_fin) %% 10<3]
  mat_sd_dif <- matrix(0,nrow = length(semestres),ncol = 23)
  mat_sd_dif[,1] <- semestres
  
  
  vec_para_for <- 1:length(semestres)
  pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
  for(k in vec_para_for){##Recorre los semestres
    setTxtProgressBar(pb, k)
    
    nom_mat_dif <- paste0("mat_diferencias por semestre/mat_diferencias_",semestres[k],".RData")
    load(nom_mat_dif)
    
    for(d in 3:dim(mat_diferencias)[2]){##Recorre las columnas
      mat_sd_dif[k,d-1] <- sd(as.numeric(mat_diferencias[,d]))
    }
  }
  close(pb)
  
  colnames(mat_sd_dif) <- c("Semestre","sd(Gpos_Real-Gpos_Sim)",
                            "sd(Alum_Real-Alum_Sim)","sd(Gpo_1)","sd(Gpo_2)",
                            "sd(Gpo_3)","sd(Gpo_4)","sd(Gpo_5)","sd(Gpo_6)",
                            "sd(Gpo_7)","sd(Gpo_8)","sd(Gpo_9)","sd(Gpo_10)",
                            "sd(Gpo_11)","sd(Gpo_12)","sd(Gpo_13)","sd(Gpo_14)",
                            "sd(Gpo_15)","sd(Gpo_16)","sd(Gpo_17)","sd(Gpo_18)",
                            "sd(Gpo_19)","sd(Gpo_20)")
  
  nom_mat_sd <- paste0("mat_dif_real-sim_esp_var_sd/mat_sd_dif.RData")
  save(mat_sd_dif,file = nom_mat_sd)
  ## Se guardan los datos en un archivo de excel para graficar
  write.csv(mat_sd_dif, file = "mat_dif_real-sim_esp_var_sd/mat_sd_dif.csv",row.names = F)
}


# guarda_mat_esp_sim ------------------------------------------------------
#' Title guarda_mat_esp_sim: Función que guarda la matriz "mat_esp_sim" la
#' cual tiene la esperanza de los datos de los valores de la matriz de datos
#' simulados.
#'
#' @param sem_ini: Semestre en el que se inicia la búsqueda de
#' información.
#' @param sem_fin: Semestre en el que se finaliza la búsqueda de
#' información para generar la asignación del siguiente semestre.
#'
#' @example sem_ini <- 20132
#' @example sem_fin <- 20201
#' 
guarda_mat_esp_sim <- function(sem_ini,sem_fin){
  semestres <- (sem_ini:sem_fin)[(sem_ini:sem_fin) %% 10>0 &(sem_ini:sem_fin) %% 10<3]
  mat_esp_sim <- matrix(0,nrow = length(semestres),ncol = 23)
  mat_esp_sim[,1] <- semestres
  
  vec_para_for <- 1:length(semestres)
  pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
  for(k in vec_para_for){##Recorre los semestres
    setTxtProgressBar(pb, k)
    
    nom_mat_dif <- paste0("mat_diferencias por semestre/mat_diferencias_",semestres[k],".RData")
    load(nom_mat_dif)
    
    for(d in 3:dim(mat_diferencias)[2]){##Recorre las columnas
      mat_esp_sim[k,d-1] <- mean(as.numeric(mat_diferencias[,d]))
    }
  }
  close(pb)
  colnames(mat_esp_sim) <- c("Semestre","E[Gpos_Sim]","E[Alum_Sim]",
                             "E[Gpo_1]","E[Gpo_2]","E[Gpo_3]","E[Gpo_4]",
                             "E[Gpo_5]","E[Gpo_6]","E[Gpo_7]","E[Gpo_8]",
                             "E[Gpo_9]","E[Gpo_10]","E[Gpo_11]","E[Gpo_12]",
                             "E[Gpo_13]","E[Gpo_14]","E[Gpo_15]","E[Gpo_16]",
                             "E[Gpo_17]","E[Gpo_18]","E[Gpo_19]","E[Gpo_20]")
  
  nom_mat_esp <- paste0("mat_gpos_sim_esp_var_sd/mat_esp_sim.RData")
  save(mat_esp_sim,file = nom_mat_esp)
  
  ## Se guardan los datos en un archivo de excel para graficar
  write.csv(mat_esp_sim, file = "mat_gpos_sim_esp_var_sd/mat_esp_sim.csv",row.names = F)
}


# guarda_mat_var_sim ------------------------------------------------------
#' Title guarda_mat_var_sim: Función que guarda la matriz "mat_var_sim" la
#' cual tiene la varianza de los datos de los valores de la matriz de datos
#' simulados.
#'
#' @param sem_ini: Semestre en el que se inicia la búsqueda de
#' información.
#' @param sem_fin: Semestre en el que se finaliza la búsqueda de
#' información para generar la asignación del siguiente semestre.
#'
#' @example sem_ini <- 20132
#' @example sem_fin <- 20201
#' 
guarda_mat_var_sim <- function(sem_ini,sem_fin){
  semestres <- (sem_ini:sem_fin)[(sem_ini:sem_fin) %% 10>0 &(sem_ini:sem_fin) %% 10<3]
  mat_var_sim <- matrix(0,nrow = length(semestres),ncol = 23)
  mat_var_sim[,1] <- semestres
  
  vec_para_for <- 1:length(semestres)
  pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
  for(k in vec_para_for){##Recorre los semestres
    setTxtProgressBar(pb, k)
    
    nom_mat_dif <- paste0("mat_diferencias por semestre/mat_diferencias_",semestres[k],".RData")
    load(nom_mat_dif)
    
    for(d in 3:dim(mat_diferencias)[2]){##Recorre las columnas
      mat_var_sim[k,d-1] <- var(as.numeric(mat_diferencias[,d]))
    }
  }
  close(pb)
  
  colnames(mat_var_sim) <- c("Semestre","Var(Gpos_Sim)","Var(Alum_Sim)","Var(Gpo_1)",
                             "Var(Gpo_2)","Var(Gpo_3)","Var(Gpo_4)","Var(Gpo_5)",
                             "Var(Gpo_6)","Var(Gpo_7)","Var(Gpo_8)","Var(Gpo_9)",
                             "Var(Gpo_10)","Var(Gpo_11)","Var(Gpo_12)","Var(Gpo_13)",
                             "Var(Gpo_14)","Var(Gpo_15)","Var(Gpo_16)","Var(Gpo_17)",
                             "Var(Gpo_18)","Var(Gpo_19)","Var(Gpo_20)")
  
  nom_mat_var <- paste0("mat_gpos_sim_esp_var_sd/mat_var_sim.RData")
  save(mat_var_sim,file = nom_mat_var)
  ## Se guardan los datos en un archivo de excel para graficar
  write.csv(mat_var_sim, file = "mat_gpos_sim_esp_var_sd/mat_var_sim.csv",row.names = F)
}


# guarda_mat_sd_sim ------------------------------------------------------
#' Title guarda_mat_sd_sim: Función que guarda la matriz "mat_sd_sim" la
#' cual tiene la desviación estándar de los datos de los valores de la
#' matriz datos simulados.
#'
#' @param sem_ini: Semestre en el que se inicia la búsqueda de
#' información.
#' @param sem_fin: Semestre en el que se finaliza la búsqueda de
#' información para generar la asignación del siguiente semestre.
#'
#' @example sem_ini <- 20132
#' @example sem_fin <- 20201
#' 
guarda_mat_sd_sim <- function(sem_ini,sem_fin){
  semestres <- (sem_ini:sem_fin)[(sem_ini:sem_fin) %% 10>0 &(sem_ini:sem_fin) %% 10<3]
  mat_sd_sim <- matrix(0,nrow = length(semestres),ncol = 23)
  mat_sd_sim[,1] <- semestres
  
  
  vec_para_for <- 1:length(semestres)
  pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
  for(k in vec_para_for){##Recorre los semestres
    setTxtProgressBar(pb, k)
    
    nom_mat_dif <- paste0("mat_diferencias por semestre/mat_diferencias_",semestres[k],".RData")
    load(nom_mat_dif)
    
    for(d in 3:dim(mat_diferencias)[2]){##Recorre las columnas
      mat_sd_sim[k,d-1] <- sd(as.numeric(mat_diferencias[,d]))
    }
  }
  close(pb)
  
  colnames(mat_sd_sim) <-  c("Semestre","sd(Gpos_Sim)","sd(Alum_Sim)","sd(Gpo_1)","sd(Gpo_2)",
                             "sd(Gpo_3)","sd(Gpo_4)","sd(Gpo_5)","sd(Gpo_6)","sd(Gpo_7)",
                             "sd(Gpo_8)","sd(Gpo_9)","sd(Gpo_10)","sd(Gpo_11)","sd(Gpo_12)",
                             "sd(Gpo_13)","sd(Gpo_14)","sd(Gpo_15)","sd(Gpo_16)","sd(Gpo_17)",
                             "sd(Gpo_18)","sd(Gpo_19)","sd(Gpo_20)")
  
  nom_mat_sd <- paste0("mat_gpos_sim_esp_var_sd/mat_sd_sim.RData")
  save(mat_sd_sim,file = nom_mat_sd)
  ## Se guardan los datos en un archivo de excel para graficar
  write.csv(mat_sd_sim, file = "mat_gpos_sim_esp_var_sd/mat_sd_sim.csv",row.names = F)
}


# Valida_list_url ---------------------------------------------------------
#' Validador de list_url
#' @param list_url lista con las variables globales
#' @param sem_ini semestre inicial de información; eg. 20192
#' @param sem_fin semestre final de información; eg. 20201
#' @param sem_actual semestre actual de información; eg. 20201
#' @param Actualiza_RAW_url indicadora si se actualiza la matriz mat_RAW_url;
#'   utlizado por función Actualiza_list_url
#' @param Actualiza_limpia_base_url indicadora de limpiar la matriz
#'   mat_posibles_url; utlizado por función Actualiza_list_url par llamar a la
#'   función limpia_base_url
#' @param Actualiza_elimina_grupos_con_0
#' @param Salvar_URL_RData
#' @param usar_vec_corto_num_materia
#' @param planes_estudio
#' @param file_name
#' @param file_name_RAW
#' @param nombres_carrera_plan
#' @param mat_ubicaciones_url matriz con las ubicaciones de cada elemento que se
#'   extrae de una url
#' @param colnames_mat_posibles_url
#' @param ncol_mat_posibles_url número de columnas de matriz mat_posibles_url
#' @param nrow_mat_posibles_url número de renglones de matriz mat_posibles_url
#' @param mat_RAW_url
#' @param mat_posibles_url
#' @param utilizar_RAW_anterior indicadora si se utiliza un archivo ya
#'   existente; utilizada por función posibles_url
#' @param mat_Grande
#' @param semestres_reales
#' @param num_grupos
#' @param url_con_salon
#' @param plan_reales
#' @param num_mat_reales
#' @param indicadoras_actualiza_col_j_mat_Grande indicadora que utiliza la
#'   función XXX para saber que columnas de mat_Grande se deben de actualizar
#' @param mat_paginas_error matriz que guarda el historial de póginas con errores
#' @param elimina_pags_con_0_grupos se borran las páginas que no tienen grupos
#' @param usa_grupos_salvados indicadora de usar el archivo .RData con grupos salvados de cada página
#' @param usa_vec_con_salon indicadora de usar el archivo .RData con variable vec_con_salon
#' @param Carpeta_RData 
#' @param usa_vec_con_info_salvados indicadora de usar el archivo .RData con variable vec_con_info_salvados
#'
#' @return
#'
#' @examples
Valida_list_url <- function(list_url,
                            sem_ini = c(20151,20081,20172,20192)[4], # 1 = Super GRANDE,..., 4 = chica
                            sem_fin = 20201,
                            sem_actual = 20201,
                            Actualiza_RAW_url = TRUE,
                            Actualiza_limpia_base_url = TRUE,
                            Actualiza_elimina_grupos_con_0 = TRUE,
                            Salvar_URL_RData = TRUE,
                            usar_vec_corto_num_materia = TRUE,
                            elimina_pags_con_0_grupos = TRUE,
                            Carpeta_RData = "Archivos RData V01",
                            usa_grupos_salvados = TRUE,
                            usa_vec_con_salon = TRUE,
                            usa_vec_con_info_salvados = TRUE,
                            planes_estudio = c(119,1176,2017,218,1556,217,2055),
                            file_name = paste0("Dat_URL_",list_url$sem_ini,"_",list_url$sem_fin,".RData"),
                            file_name_RAW = paste0("Lista_RAW_",list_url$file_name),
                            nombres_carrera_plan = c("Actuaría (plan 2000)",
                                                     "Actuaría (plan 2006)",
                                                     "Actuaría (plan 2015)",
                                                     "Ciencias de la Computación (plan 1994)",
                                                     "Ciencias de la Computación (plan 2013)",
                                                     "Matemáticas (plan 1983)",
                                                     "Matemáticas Aplicadas (plan 2017)"),
                            mat_ubicaciones_url = matrix(c("Materia"            ,'#info-contenido h2', T,F,
                                                           "Profesor"           ,'tr:nth-child(1) td:nth-child(2) a',F,F,
                                                           "Horario"            ,'tr:nth-child(1) td:nth-child(4)',F,T,
                                                           "Lugares"            ,'#info-contenido div',F,F,
                                                           "Alumnos"            ,'#info-contenido div',F,F,
                                                           "Salon"              ,'tr:nth-child(1) td~ td+ td a , td:nth-child(4) a',F,F,
                                                           "Grupo"              ,'#info-contenido div',F,F,
                                                           "Carrera"            ,'h1',T,F,
                                                           "Plan"               ,'h1',T,F,
                                                           "Semestre"           ,-1,-1,-1,### FALTA POR HACER
                                                           "Cambios"            ,-1,-1,-1,### FALTA POR HACER
                                                           "Turno"              ,-1,-1,-1,### FALTA POR HACER
                                                           "Semestre_de_materia",'#info-contenido h2',T,F,
                                                           "Grupos_x_pag"       ,'strong',F,F,
                                                           "Grupo_paralelo"     ,'em',F,F),ncol=4,byrow = T),
                            # colnames(list_url$mat_ubicaciones_url) = c("Nombre columna","Ubicacion en pagina","Repetir","Elimina salto"),
                            colnames_mat_posibles_url = c("Semestre","Plan","Materia","URL","Grupos x pag","url_con_salon"),
                            ncol_mat_posibles_url = length(list_url$colnames_mat_posibles_url),
                            nrow_mat_posibles_url = 20000,
                            mat_RAW_url = matrix(0,list_url$nrow_mat_posibles_url,list_url$ncol_mat_posibles_url),
                            mat_posibles_url = list_url$mat_RAW_url,
                            utilizar_RAW_anterior = T,
                            mat_Grande = matrix(0,sum(as.numeric(list_url$mat_posibles_url[,5])),13),
                            indicadoras_actualiza_col_j_mat_Grande = rep(T,13),
                            semestres_reales = NA,
                            num_grupos = NA,
                            url_con_salon = NA,
                            # colnames(list_url$mat_Grande) = c("Materia", "Profesor","Horario","Lugares","Alumnos","Salon",
                            #                                    "Grupo","Carrera","Plan","Semestre","Cambios","Turno","Semestre_de_materia"),
                            plan_reales = NA,
                            num_mat_reales = NA,
                            mat_paginas_error = matrix(0,1,4)) {
  # EJEMPLO DE list_url
  list_url_EJEMPLO <- list()
  list_url_EJEMPLO$sem_ini = 20192 # Datos Chica 
  list_url_EJEMPLO$sem_fin = 20201
  list_url_EJEMPLO$sem_actual = 20201
  list_url_EJEMPLO$Actualiza_RAW_url = TRUE
  list_url_EJEMPLO$Actualiza_limpia_base_url = TRUE
  list_url_EJEMPLO$Actualiza_elimina_grupos_con_0 = TRUE
  list_url_EJEMPLO$Salvar_URL_RData = TRUE
  list_url_EJEMPLO$usar_vec_corto_num_materia = TRUE
  list_url_EJEMPLO$elimina_pags_con_0_grupos = TRUE
  list_url_EJEMPLO$Carpeta_RData = "Archivos RData V01"
  list_url_EJEMPLO$usa_grupos_salvados = TRUE
  list_url_EJEMPLO$usa_vec_con_salon = TRUE
  list_url_EJEMPLO$usa_vec_con_info_salvados = TRUE
  list_url_EJEMPLO$planes_estudio = c(119,1176,2017,218,1556,217,2055)
  list_url_EJEMPLO$file_name <- paste0("Dat_URL_",list_url$sem_ini,"_",list_url$sem_fin,".RData")
  list_url_EJEMPLO$file_name_RAW <- paste0(list_url$Carpeta_RData,"/Lista_RAW_Dat_URL_",list_url$sem_ini,"_",list_url$sem_fin,".RData")
  list_url_EJEMPLO$nombres_carrera_plan <- c("Actuaría (plan 2000)",
                                             "Actuaría (plan 2006)",
                                             "Actuaría (plan 2015)",
                                             "Ciencias de la Computación (plan 1994)",
                                             "Ciencias de la Computación (plan 2013)",
                                             "Matemáticas (plan 1983)",
                                             "Matemáticas Aplicadas (plan 2017)")
  list_url_EJEMPLO$mat_ubicaciones_url <- matrix(c("Materia"            ,'#info-contenido h2', T,F,
                                                   "Profesor"           ,'tr:nth-child(1) td:nth-child(2) a',F,F,
                                                   "Horario"            ,'tr:nth-child(1) td:nth-child(4)',F,T,
                                                   "Lugares"            ,'#info-contenido div',F,F,
                                                   "Alumnos"            ,'#info-contenido div',F,F,
                                                   "Salon"              ,'tr:nth-child(1) td~ td+ td a , td:nth-child(4) a',F,F,
                                                   "Grupo"              ,'#info-contenido div',F,F,
                                                   "Carrera"            ,'h1',T,F,
                                                   "Plan"               ,'h1',T,F,
                                                   "Semestre"           ,-1,-1,-1,### FALTA POR HACER
                                                   "Cambios"            ,-1,-1,-1,### FALTA POR HACER
                                                   "Turno"              ,-1,-1,-1,### FALTA POR HACER
                                                   "Semestre_de_materia",'#info-contenido h2',T,F,
                                                   "Grupos_x_pag"       ,'strong',F,F,
                                                   "Grupo_paralelo"     ,'em',F,F),ncol=4,byrow = T)
  list_url_EJEMPLO$colnames_mat_posibles_url <- c("Semestre","Plan","Materia","URL","Grupos x pag","url_con_salon")
  list_url_EJEMPLO$ncol_mat_posibles_url <- length(list_url_EJEMPLO$colnames_mat_posibles_url)
  list_url_EJEMPLO$nrow_mat_posibles_url <- 20000
  list_url_EJEMPLO$mat_RAW_url <- matrix(0,list_url_EJEMPLO$nrow_mat_posibles_url,list_url_EJEMPLO$ncol_mat_posibles_url)
  list_url_EJEMPLO$mat_posibles_url <- list_url_EJEMPLO$mat_RAW_url
  list_url_EJEMPLO$utilizar_RAW_anterior <- T
  
  list_url_EJEMPLO$ncol_mat_Grande <- 13
  list_url_EJEMPLO$mat_Grande <- matrix(0,1,list_url_EJEMPLO$ncol_mat_Grande)
  list_url_EJEMPLO$mat_Grande_con_url <- matrix(0,1,list_url_EJEMPLO$ncol_mat_Grande+1)
  
  list_url_EJEMPLO$semestres_reales <- NA
  list_url_EJEMPLO$plan_reales <- NA
  list_url_EJEMPLO$num_mat_reales <- NA
  list_url_EJEMPLO$num_grupos <- NA
  list_url_EJEMPLO$url_con_salon <- NA
  
  list_url_EJEMPLO$mat_paginas_error <- matrix(0,1,4)
  list_url_EJEMPLO$indicadoras_actualiza_col_j_mat_Grande = rep(T,13)
  
  #  Nombres de columnas
  colnames(list_url_EJEMPLO$mat_ubicaciones_url) = c("Nombre columna","Ubicacion en pagina","Repetir","Elimina salto")
  colnames(list_url_EJEMPLO$mat_Grande) = c("Materia", "Profesor","Horario","Lugares","Alumnos","Salon",
                                            "Grupo","Carrera","Plan","Semestre","Cambios","Turno","Semestre_de_materia")
  colnames(list_url_EJEMPLO$mat_paginas_error) <- c("Columna","length(vec)","num_gpo","Pagina")
  
  # INICIO DE VALIDACIONES
  error1_bien0 <- 0
  # if(length(list_url)!=length(list_url_EJEMPLO)){
  if(length(setdiff(names(list_url_EJEMPLO),names(list_url)))>0 || length(setdiff(names(list_url),names(list_url_EJEMPLO))>0)) {
    error1_bien0 <- error1_bien0 +1
    cat("Error",error1_bien0,":\n\t","¡ERROR EN VARIABLES DE list_url!\n")
    cat("Faltan las variables:\n\t",setdiff(names(list_url_EJEMPLO),names(list_url)),"\n",
        "Sobran las variables:\n\t",setdiff(names(list_url),names(list_url_EJEMPLO)),"\n\n")
  }
  
  if(list_url$ncol_mat_Grande != list_url_EJEMPLO$ncol_mat_Grande) {
    error1_bien0 <- error1_bien0 +1
    cat("Error",error1_bien0,":\n\t variable ncol_mat_Grande incorrecta \n",
        "Deberían ser ",list_url_EJEMPLO$ncol_mat_Grande," y vale ",
        list_url$ncol_mat_Grande,"\n")
  } 
  
  if(is.null(list_url$mat_Grande)) {
    error1_bien0 <- error1_bien0 +1
    cat("Error: la matriz mat_Grande es NULL \n")
  } else if(ncol(list_url$mat_Grande) != ncol(list_url_EJEMPLO$mat_Grande)) {
    error1_bien0 <- error1_bien0 +1
    cat("Error en el número de columnas de list_url$mat_Grande \n")
  }
  
  if(any(dim(list_url$mat_ubicaciones_url) != dim(list_url_EJEMPLO$mat_ubicaciones_url))) {
    error1_bien0 <- error1_bien0 +1
    cat("Error",error1_bien0,":\n\t números incorrectos en las dimensiones de mat_ubicaciones_url \n",
        "Deberían ser ",dim(list_url_EJEMPLO$mat_ubicaciones_url)," y son",
        dim(list_url$mat_ubicaciones_url),"\n")
  } else if(!all.equal(list_url$mat_ubicaciones_url,list_url_EJEMPLO$mat_ubicaciones_url)){
    cat("¡ERROR EN ENTRADAS DE mat_ubicaciones_url \n")
    error1_bien0 <- error1_bien0 +1
  }
  
  
  if(list_url$file_name != paste0(list_url$Carpeta_RData,"/Dat_URL_",list_url$sem_ini,"_",list_url$sem_fin,".RData")) {
    error1_bien0 <- error1_bien0 +1
    cat("Error",error1_bien0,":\n\t file_name incorrecto, debería ser:\n\t",
        paste0(list_url$Carpeta_RData,"/Dat_URL_",list_url$sem_ini,"_",list_url$sem_fin,".RData"),
        "\n pero es:\n\t",
        list_url$file_name,"\n")
  }
  
  
  if(list_url$file_name_RAW != paste0(list_url$Carpeta_RData,"/Lista_RAW_Dat_URL_",list_url$sem_ini,"_",list_url$sem_fin,".RData")) {
    error1_bien0 <- error1_bien0 +1
    cat("Error",error1_bien0,":\n\t file_name_RAW incorrecto, debería ser:\n\t",
        paste0(list_url$Carpeta_RData,"/Lista_RAW_Dat_URL_",list_url$sem_ini,"_",list_url$sem_fin,".RData"),
        "\n pero es:\n\t",
        list_url$file_name_RAW,"\n")
  }
  
  if(!all(colnames(list_url$mat_Grande)==colnames(list_url_EJEMPLO$mat_Grande))) {
    error1_bien0 <- error1_bien0 +1
    cat("Error",error1_bien0,":\n\t error en nombres de las columnas de list_url$mat_Grande \n")
  }
  
  if(nrow(list_url$mat_Grande)!=sum(as.numeric(list_url$mat_posibles_url[,5]))) {
    error1_bien0 <- error1_bien0 +1
    cat("Error",error1_bien0,":\n\t el número de columnas de mat_Grande no coincide con la suma de todos los grupos:\n",
        "nrow(list_url$mat_Grande) = ",nrow(list_url$mat_Grande)," y debería ser ",
        sum(as.numeric(list_url$mat_posibles_url[,5]))," (la suma de la columna 5 de mat_posibles_url)\n")
  }
  
  if(nrow(list_url$mat_Grande_con_url)!=sum(as.numeric(list_url$mat_posibles_url[,5]))) {
    error1_bien0 <- error1_bien0 +1
    cat("Error",error1_bien0,":\n\t el número de columnas de mat_Grande_con_url no coincide con la suma de todos los grupos:\n",
        "nrow(list_url$mat_Grande) = ",nrow(list_url$mat_Grande_con_url)," y debería ser ",
        sum(as.numeric(list_url$mat_posibles_url[,5]))," (la suma de la columna 5 de mat_posibles_url)\n")
  }
  
  if(nrow(list_url$mat_posibles_url)!=length(list_url$num_grupos)) {
    error1_bien0 <- error1_bien0 +1
    cat("Error",error1_bien0,":\n\t número de renglones de mat_posibles_url no coincide con longitud de num_grupos\n",
        "nrow(list_url$mat_posibles_url) = ",nrow(list_url$mat_posibles_url)," y \n",
        "length(list_url$num_grupos)=",length(list_url$num_grupos),"\n")
  }
  
  if(error1_bien0 >0) {
    cat(" *** EXISTEN",error1_bien0,"ERRORES EN list_url *** \n")
    return(F)
  } else {
    cat(" *** La variable list_url es adecuada ***\n")
    return(T)
  }
}



# My_plot_progress --------------------------------------------------------
#' Plot de barras de progreso
#'
#' @param percentages vector de valores numericos entre cero y uno para graficar
#' @param names vector con nombres de cada barra
#' @param title_plot titulo del plot
#'
#' Ver https://www.r-bloggers.com/multiple-progress-bars/
#' @return
#' @export
#'
#' @examples
#' percentages <- runif(3)
#' names=c("uno","dos","tres")
#' title_plot <- "avance"
#' My_plot_progress(percentages,names,title_plot)
My_plot_progress <- function(percentages,names_percentages,title_plot)	{
  percentages <- rev(percentages)
  names_percentages <- rev(names_percentages)
  vectOfBar <- c(percentages)*100
  numOfBar <- length(vectOfBar)
  plot(c(0,100), c(0,numOfBar), type='n', xlab='', ylab='', yaxt='n', mar=c(3,3,3,3))
  for(i in 1:numOfBar) {
    rect(0, 0.1+i-1, vectOfBar[i], 0.9+i-1, col=rainbow(numOfBar)[i])
    # text(0.5, 0.5+i-1, paste('Status ', i, ': ', round(vectOfBar[i],2), '%', sep=''), adj=0)
    text(0.5, 0.5+i-1, paste(names_percentages[i],': ', round(vectOfBar[i],2), '%', sep=''), adj=0)
  }
  title(title_plot)
}

##########################################################################
##### MATRIZ CON POSIBLES URL #####
## Funciones que generan la lista de posibles URL de donde se va a extraer
##la información de las páginas de la facultad
##########################################################################


# posibles_url ------------------------------------------------------------
#' Title: posibles_url: Función que arroja la lista "list_url"  dentro de
#' la cual se encuentra la matriz con las posibles URL de las páginas de
#' horarios de la FC. Dicha matriz tiene 6 columnas: Semestre, Plan,
#' Materia, URL, Grupos por página, url_con_salon. Las últimas 2 columnas
#' se llenan con la función "Actualiza_list_url"
#'
#' Obs: para 20081-20201 tarda 41 minutos, 20151-20201 tarda 18 minutos,
#' 20172-20201 tarda 9 minutos y 20192-20201 tarda 3 min
#' 
#' @param sem_ini Número que representa el semestre en el cual se desea iniciar
#'   la búsqueda de información.
#' @param sem_fin  Número que representa el semestre en el cual se desea
#'   finalizar la búsqueda de información.

#' @param intervalo_num_materia indicador si se utiliza el vector corto o si si
#'   hace una búsqueda en un vector más grande
#'
#' @example sem_ini <- 20081
#' @example sem_fin <- 20201
#'
#' @return mat_posibles_url: Matriz con 4 columnas: 1) Semestre 2) Plan 3)
#'   Número de materia 4) Posibles url
posibles_url = function(list_url){
  
  if(!Valida_list_url(list_url)){
    return(list_url)
  }
  
  # En caso de ya existir el archivo con la matriz RAW
  if(file.exists(list_url$file_name_RAW) && list_url$utilizar_RAW_anterior){
    list_url_nueva <- list_url
    # Cargando matriz RAW ya existente
    load(list_url$file_name_RAW)
    Sys.sleep(1)
    # Actualizando variables que actualizaría esta función
    nrow_anterior <- nrow(list_url$mat_RAW_url[,1:4])
    list_url_nueva$mat_RAW_url = cbind(list_url$mat_RAW_url[,1:4],
                                       rep(0,nrow_anterior),
                                       rep(0,nrow_anterior))
    list_url_nueva$mat_posibles_url = cbind(list_url$mat_posibles_url[,1:4],
                                            rep(0,nrow_anterior),
                                            rep(0,nrow_anterior))
    list_url_nueva$semestres_reales = list_url$semestres_reales
    list_url_nueva$plan_reales = list_url$plan_reales
    list_url_nueva$num_mat_reales = list_url$num_mat_reales
    
    list_url_nueva$nrow_mat_posibles_url <- nrow(list_url_nueva$mat_posibles_url)
    
    if(length(list_url_nueva$num_grupos)!=nrow(list_url_nueva$mat_posibles_url)) {
      cat("Se borró la información de num_grupos por no ser de la longitud correcta:\n",
          "length(num_grupos) = ",length(list_url_nueva$num_grupos),
          "nrow(mat_posibles_url) = ",nrow(list_url_nueva$mat_posibles_url),"\n\n")
      list_url_nueva$num_grupos = rep(-1, nrow(list_url_nueva$mat_posibles_url))
      
    }
    
    # Guardamos la información 
    list_url  <- list_url_nueva
    # Corregimos mat_Grande
    if(nrow(list_url$mat_Grande) == 0) {
      list_url$mat_Grande <- matrix(-1,sum(as.numeric(list_url$mat_posibles_url[,5])),
                                    list_url$ncol_mat_Grande)
      colnames(list_url$mat_Grande) <- c("Materia", "Profesor","Horario","Lugares",
                                         "Alumnos","Salon","Grupo","Carrera","Plan",
                                         "Semestre","Cambios","Turno",
                                         "Semestre_de_materia")
    }
    
    cat("Se utilizará la matriz RAW ya existente del archivo:","\n\n",
        list_url$file_name_RAW,"\n\n")
    return(list_url)
  }
  
  sem_ini <- list_url$sem_ini
  sem_fin <- list_url$sem_fin
  # usar_vec_corto_num_materia <- list_url$usar_vec_corto_num_materia
  
  # Start the clock!
  ptm <- proc.time()
  ## Sólo se van a tomar en cuenta los planes de estudio vigentes
  planes_estudio = list_url$planes_estudio
  
  #Inicializamos las variables:
  
  if(list_url$usar_vec_corto_num_materia) {
    load(paste0(list_url$Carpeta_RData,"/Datos_INTERVALO_NUM_MATERIA.RData"))
  } else intervalo_num_materia = 1:2000 # Obs: el más grande encontrado fue 1841
  
  
  # intervalo_num_materia = 800:815
  mat_posibles_url = matrix(0, nrow = list_url$nrow_mat_posibles_url,
                            ncol = list_url$ncol_mat_posibles_url)
  colnames(mat_posibles_url) <- list_url$colnames_mat_posibles_url
  
  ## Se crea el vector para los semestres pares e impares
  (semestres = (list_url$sem_ini:list_url$sem_fin)[(list_url$sem_ini:list_url$sem_fin) 
                                                   %% 10>0 &
                                                     (list_url$sem_ini:list_url$sem_fin) 
                                                   %% 10<3])
  
  semestres_reales <- c(NULL)
  plan_reales <- c(NULL)
  num_mat_reales <- c(NULL)
  i_sem <- 1
  i_plan <- 1
  i_num_materia <- 1
  title_plot <- "Progreso de extracción url"
  
  i = 0
  for(i_sem in 1:length(semestres)){
    for(i_plan in 1:length(planes_estudio)) {
      for(i_num_materia in 1:length(intervalo_num_materia)) {
        # Graficamos el avance de la extracción
        if(i_num_materia/100==floor(i_num_materia/100)){
          # Sys.sleep(0.06)
          names_percentages <- c(paste0("Semestre ",semestres[i_sem]),
                                 paste0("Plan ",planes_estudio[i_plan]),"Materias")
          My_plot_progress(c(i_sem/length(semestres),i_plan/length(planes_estudio),
                             i_num_materia/length(intervalo_num_materia)),
                           names_percentages,title_plot)
        } # fi graficando
        url = paste0("http://www.fciencias.unam.mx/docencia/horarios/",semestres[i_sem],
                     "/",planes_estudio[i_plan],
                     "/",intervalo_num_materia[i_num_materia])
        
        #Probamos si la página existe:
        tryCatch({
          read_html(url)
          i <- i+1
          mat_posibles_url[i,1:4] <- c(semestres[i_sem],
                                       planes_estudio[i_plan],
                                       intervalo_num_materia[i_num_materia],
                                       url)
          semestres_reales <- c(semestres_reales,semestres[i_sem])
          plan_reales <- c(plan_reales,planes_estudio[i_plan])
          num_mat_reales <- c(num_mat_reales,intervalo_num_materia[i_num_materia])
        }, 
        error=function(e){})
      } # fin for materias
    } # fin for planes
  } # fin de for semestres
  
  semestres_reales <- unique(na.omit(semestres_reales))
  plan_reales <- unique(na.omit(plan_reales))
  num_mat_reales <- unique(na.omit(num_mat_reales))
  
  names_percentages <- c(paste0("Semestre ",semestres[i_sem]),
                         paste0("Plan ",planes_estudio[i_plan]),"Materias")
  My_plot_progress(c(1,1,1),names_percentages,title_plot)
  
  ##Quitamos los renglones vacíos de la matriz:
  # mat_posibles_url = mat_posibles_url[1:i,]
  n_renglones<- dim(mat_posibles_url)[1]
  mat_posibles_url = matrix(mat_posibles_url[mat_posibles_url[1:n_renglones,1]!=0],ncol = 6)
  
  # Regresamos una lista con los valores adecuados
  
  list_url$mat_RAW_url=mat_posibles_url
  list_url$mat_posibles_url=mat_posibles_url
  list_url$semestres_reales=semestres_reales
  list_url$plan_reales=plan_reales
  list_url$num_mat_reales=num_mat_reales
  
  if(nrow(list_url$mat_Grande) == 0) {
    list_url$mat_Grande <- matrix(-1,sum(as.numeric(list_url$mat_posibles_url[,5])),
                                  list_url$ncol_mat_Grande)
    
    colnames(list_url$mat_Grande) <- c("Materia", "Profesor","Horario","Lugares",
                                       "Alumnos","Salon","Grupo","Carrera","Plan",
                                       "Semestre","Cambios","Turno",
                                       "Semestre_de_materia")
  }
  
  # Salvando intervalo de materia para futuras corridas
  intervalo_num_materia <- sort(list_url$num_mat_reales)
  archivo_intervalo_num_materia <- paste0(list_url$Carpeta_RData,
                                          "/Datos_INTERVALO_NUM_MATERIA.RData")
  save(intervalo_num_materia, file = archivo_intervalo_num_materia)
  
  # Stop the clock
  cat("La función posibles_url tardó: ",(proc.time()-ptm)[3]/60," minutos\n")
  
  return(list_url)
}



# url_con1_sin0_info ------------------------------------------------------
#' Title: url_con1_sin0_info
#' @param url: página de internet correspondiente a algún horario de la FC
#'
#' @return 1 si la página tiene información, 0 si no
#'
#' @example url = paste0("http://www.fciencias.unam.mx/docencia/horarios/",sem,"/",
#' plan,"/",num_materia)
url_con1_sin0_info = function(url){
  valor_url = 0
  
  tryCatch({
    webpage <- read_html(url)
    profesor_data_html <- html_nodes(webpage,'tr:nth-child(1) td:nth-child(1)')
    (profesor <- html_text(profesor_data_html))
    if(length(profesor) > 0){
      valor_url = 1
    }
  }, error=function(e){})
  
  return(valor_url)
}


# url_con_info ------------------------------------------------------------
#' Title: url_con_info
#' @param mat_posibles_url: Matriz con 4 columnas:
#' 1) Semestre
#' 2) Plan
#' 3) Número de materia
#' 4) Posibles url
#'
#' @return vec_con_info: Vector binario que indica si cada url tiene o no información
#'
#' @example mat_posibles_url[16407,] = c(20182,217,991,
#' http://www.fciencias.unam.mx/docencia/horarios/20182/217/991)
url_con_info = function(list_url){
  if(file.exists(paste0(list_url$Carpeta_RData,"/Datos_vec_con_info_",list_url$sem_ini,"_",list_url$sem_fin,".RData")) && 
     list_url$usa_vec_con_info_salvados) {
    cat("Se utilizara del archivo \n\t",
        paste0(list_url$Carpeta_RData,"/Datos_vec_con_info_",list_url$sem_ini,"_",list_url$sem_fin,".RData"),"\n\n")
    load(paste0(list_url$Carpeta_RData,"/Datos_vec_con_info_",list_url$sem_ini,"_",list_url$sem_fin,".RData"))
    Sys.sleep(1)
  } else {
    mat_posibles_url <- list_url$mat_posibles_url
    vec_posibles_url = mat_posibles_url[,4] # la col 4 contiene URL
    vec_con_info = rep(-1,length(vec_posibles_url))
    
    vec_para_for <- 1:length(vec_posibles_url)
    pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
    cat("Limpiando mat_posibles_url:\n")
    for(i in vec_para_for){
      setTxtProgressBar(pb, i)
      vec_con_info[i] = url_con1_sin0_info(as.character(mat_posibles_url[i,4]))
    }
    close(pb)
    print("\n")
    cat("Se encontraron ",mean(vec_con_info==1)*100,"% url con información\n")
  }
  save(vec_con_info,
       file = paste0(list_url$Carpeta_RData,"/Datos_vec_con_info_",list_url$sem_ini,"_",list_url$sem_fin,".RData"))
  return(vec_con_info)
}



# limpia_base_url ---------------------------------------------------------
#' Title: limpia_base_url
#' @param mat_posibles_url: Matriz con 4 columnas:
#' 1) Semestre
#' 2) Plan
#' 3) Número de materia
#' 4) Posibles url
#'
#' @return mat_limpia1_url: Matriz "mat_posibles_url" sólo con páginas que
#' tienen información
#'
#' @example mat_posibles_url[16407,] = c(20182,217,991,
#' http://www.fciencias.unam.mx/docencia/horarios/20182/217/991)
limpia_base_url = function(list_url){
  # mat_posibles_url <- list_url$mat_posibles_url
  longitud_original <- nrow(list_url$mat_posibles_url)
  
  # Primero borramos las que no tengan información
  vec_con_info = url_con_info(list_url)
  # mat_limpia1_url = mat_posibles_url[vec_con_info>0,] # ANTES
  list_url$mat_posibles_url = list_url$mat_posibles_url[vec_con_info>0,] # AHORA
  list_url$num_grupos = list_url$num_grupos[vec_con_info>0] # AHORA
  
  longitud_nueva <- nrow(list_url$mat_posibles_url)
  
  cat("La longitud original era ",longitud_original," la nueva longitud es ",
      longitud_nueva,":\n\t se borraron ",longitud_original-longitud_nueva,
      " un ",(longitud_original-longitud_nueva)*100/longitud_original,"% \n")
  
  return(list_url) 
}



# elimina_grupos_con_0 ----------------------------------------------------
#' Eliminando páginas sin grupos
#'
#' @param list_url 
#'
#' @return
#' @export
#'
#' @examples
elimina_grupos_con_0 <- function(list_url) {
  # Logitud original sin borrar páginas con 0 en su número de grupos
  (longitud_original <- nrow(list_url$mat_posibles_url))
  i_poginas_CON_grupos <- which(list_url$mat_posibles_url[,5]!="0")
  i_poginas_SIN_grupos <- which(list_url$mat_posibles_url[,5]=="0")
  (longitud_nueva <- length(i_poginas_CON_grupos))
  if(longitud_nueva!=longitud_original) {
    list_url$mat_posibles_url = list_url$mat_posibles_url[i_poginas_CON_grupos,]
    list_url$num_grupos = list_url$num_grupos[i_poginas_CON_grupos]
  }
  cat("La longitud original era ",longitud_original," la nueva longitud es ",
      longitud_nueva,":\n se borraron ",longitud_original-longitud_nueva,
      " un ",(longitud_original-longitud_nueva)*100/longitud_original,"% \n")
  return(list_url)
}



# genera_num_grupos -------------------------------------------------------
#' Genera el vector de números de grupos de cada página web
#'
#' @param list_url lista con información de url
#'
#' @return
#' @export
#'
#' @examples
genera_num_grupos <- function(list_url){
  if(file.exists(paste0(list_url$Carpeta_RData,"/Datos_num_grupos_x_pag_",list_url$sem_ini,"_",list_url$sem_fin,".RData")) && 
     list_url$usa_grupos_salvados) {
    cat("Se utilizara del archivo \n\t",
        paste0(list_url$Carpeta_RData,"/Datos_num_grupos_x_pag_",list_url$sem_ini,"_",list_url$sem_fin,".RData"),"\n\n")
    load(paste0(list_url$Carpeta_RData,"/Datos_num_grupos_x_pag_",list_url$sem_ini,"_",list_url$sem_fin,".RData"))
    Sys.sleep(1)
  } else {
    m4_posibles_url <- list_url$mat_posibles_url
    dim_matriz <- dim(m4_posibles_url)
    num_grupos <- rep(0,dim_matriz[1])
    cat("Extrayendo número de grupos de cada página web:\n")
    ubicacion_grupos <- list_url$mat_ubicaciones_url[list_url$mat_ubicaciones_url[,1]=="Grupos_x_pag",2]
    vec_para_for <- 1:dim_matriz[1]
    pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
    for(i in 1:dim_matriz[1]){
      setTxtProgressBar(pb, i)
      url <- m4_posibles_url[i,4]
      tryCatch({
        webpage <- read_html(url)
        grupo_data_html <- html_nodes(webpage,ubicacion_grupos)
        grupo <- html_text(grupo_data_html)
        num_grupos[i] <- length(grupo)
      }, error=function(e){})
      # Antes estaba fuera del tryCatch
      # num_grupos[i] <- length(grupo)
    }
    close(pb)
    print("\n")
  }
  list_url$num_grupos <- num_grupos
  
  list_url$mat_posibles_url[,5] <- list_url$num_grupos
  colnames(list_url$mat_posibles_url) <- list_url$colnames_mat_posibles_url
  
  save(num_grupos,
       file = paste0(list_url$Carpeta_RData,"/Datos_num_grupos_x_pag_",list_url$sem_ini,"_",list_url$sem_fin,".RData"))
  
  return(list_url)
}



# url_con1_sin0_salon -----------------------------------------------------
#' Title: url_con1_sin0_salon
#' Esta función recibe como parámetro una url, la cual se checa si tiene o no
#' el salón del grupo, en caso de que lo tenga, la función regresa un 1 y en caso 
#' contrario regresa un cero
#' 
#' @param url: página de internet correspondiente a algún horario de la FC
#'
#' @return 1 si la página tiene información, 0 si no
#'
#' @example url = paste0("http://www.fciencias.unam.mx/docencia/horarios/",sem,"/",
#' plan,"/",num_materia)
url_con1_sin0_salon <- function(url){
  valor_url <- 0
  tryCatch({
    webpage <- read_html(url)
    salon_data_html <- html_nodes(webpage,'tr:nth-child(1) td~ td+ td a')
    salon <- html_text(salon_data_html)
    if(length(salon) > 0){
      valor_url <- 1
    }
  }, error=function(e){})
  return(valor_url)
}



# actualiza_list_url_con_salon --------------------------------------------
#' Title: actualiza_list_url_con_salon
#' Esta función recibe como parámetro la matriz de 4 columnas generada por la
#' función "posibles_url", se crea un vector con las url que se encuentran en la 
#' cuarta columna de dicha matriz y se manda a llamar a la función "url_con1_sin0_salon"
#' dentro de un "for" que recorre las páginas que están en el vector para regresar
#' un vector binario, en el cual los 1 indican que la página correspondiente a ese
#' renglón si tiene información del salón, 0 si no.
#' @param m4_posibles_url: Matriz con 4 columnas:
#' 1) Semestre
#' 2) Plan
#' 3) Número de materia
#' 4) Posibles url
#'
#' @return vec_con_salon: Vector binario que indica si cada url tiene o no información
#' del salón
#'
#' @example m4_posibles_url[16407,] = c(20182,217,991,
#' http://www.fciencias.unam.mx/docencia/horarios/20182/217/991)
actualiza_list_url_con_salon <- function(list_url){
  if(file.exists(paste0(list_url$Carpeta_RData,"/Datos_vec_con_salon",
                        list_url$sem_ini,"_",list_url$sem_fin,".RData")) && 
     list_url$usa_vec_con_salon){
    cat("Se utilizara del archivo \n\t",
        paste0(list_url$Carpeta_RData,"/Datos_vec_con_salon",list_url$sem_ini,"_",list_url$sem_fin,".RData"),"\n\n")
    load(paste0(list_url$Carpeta_RData,"/Datos_vec_con_salon",list_url$sem_ini,"_",list_url$sem_fin,".RData"))
    Sys.sleep(1)
  } else {
    m4_posibles_url <- list_url$mat_posibles_url
    #En la 4° columna de "m4_posibles_url" están las url
    vec_posibles_url <- m4_posibles_url[,4]
    vec_con_salon <- rep(-1,length(vec_posibles_url))
    vec_para_for <- 1:length(vec_posibles_url)
    pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
    for(i in 1:length(vec_posibles_url)){
      setTxtProgressBar(pb, i)
      vec_con_salon[i] <- url_con1_sin0_salon(vec_posibles_url[i])
    }
    close(pb)
    print("\n")
  }
  
  list_url$url_con_salon <- vec_con_salon
  list_url$mat_posibles_url[,6] <- list_url$url_con_salon
  save(vec_con_salon,
       file = paste0(list_url$Carpeta_RData,"/Datos_vec_con_salon",
                     list_url$sem_ini,"_",list_url$sem_fin,".RData"))
  
  return(list_url)
}



# Actualiza_list_url ------------------------------------------------------
#' Generador de archivo .RData con información de semestres sacada de url.
#' Llena las columnas "Grupos por página" y "url_con_salon" de la matriz
#' "mat_posibles_url" para que tenga completas sus 6 columnas.
#'
#' @param sem_ini semestre inicial
#' @param sem_fin semestre final
#' @param file_name nombre del archivo que se quiere salvar
#'
#' @return list_url
#'
#' @examples
#' sem_ini = 20151
#' sem_fin = 20201
#' file_name <- "Posibles_URL_V01_10_no.RData"
#' salva_limpia_genera_url(sem_ini,sem_fin,file_name)
#'
Actualiza_list_url <- function(list_url) {
  
  if(!Valida_list_url(list_url)){
    return(list_url)
  }
  # sem_ini <- list_url$sem_ini
  # sem_fin <- list_url$sem_fin
  # file_name <- list_url$file_name
  # Start the clock!
  ptm <- proc.time()
  
  # Extrayendo posibles paginas url
  if(list_url$Actualiza_RAW_url) {
    cat("\n ---- Extrayendo posibles páginas url\n")
    list_url <- posibles_url(list_url)
    save(list_url, file = list_url$file_name_RAW)
    cat("\n ---- Extrayendo grupos url\n")
    list_url <- genera_num_grupos(list_url)
    cat("\n ---- Actualizando url con salon url\n")
    list_url <- actualiza_list_url_con_salon(list_url)
    cat("OK --- Fin de extracción de posibles páginas url\n")
    save(list_url, file = list_url$file_name_RAW)
  }
  
  if(list_url$Actualiza_limpia_base_url) {
    cat("---- Actualizando limpia_base_url\n")
    list_url <- limpia_base_url(list_url)
    cat("OK --- Fin de limpieza de base url\n")
  }
  
  if(list_url$Actualiza_elimina_grupos_con_0) {
    cat("------- Función elimina_grupos_con_0\n")
    list_url <- elimina_grupos_con_0(list_url)
    cat("OK --- Fin de elimina_grupos_con_0\n")
  }
  
  if(nrow(list_url$mat_Grande) == 0) {
    list_url$nrow_mat_posibles_url <- nrow(list_url$mat_posibles_url)
    list_url$mat_Grande <- matrix(-1,sum(as.numeric(list_url$mat_posibles_url[,5])),
                                  list_url$ncol_mat_Grande)
    colnames(list_url$mat_Grande) <- c("Materia", "Profesor","Horario","Lugares","Alumnos","Salon",
                                       "Grupo","Carrera","Plan","Semestre","Cambios","Turno","Semestre_de_materia")
    list_url$mat_Grande_con_url <- matrix(-1,sum(as.numeric(list_url$mat_posibles_url[,5])),
                                          list_url$ncol_mat_Grande+1)
    
    colnames(list_url$mat_Grande_con_url) <- c("Materia", "Profesor","Horario","Lugares","Alumnos","Salon",
                                               "Grupo","Carrera","Plan","Semestre","Cambios","Turno","Semestre_de_materia","url")
  }
  
  if(list_url$Salvar_URL_RData) {
    cat("---- Salvando variables\n")
    save(list_url, file = list_url$file_name)
    cat("Se guardó el archivo:\n\t", list_url$file_name,"\n")
    cat("OK --- Fin de salvado de variables\n")
  }
  
  cat("La función Actualiza_list_url tomó: ", (proc.time()-ptm)[3]/60," minutos\n\n\n" )
  return(list_url)
}


##########################################################################
##### M_GRANDE / M_GRANDE_TOTAL #####
## Funciones que generan la matriz "m_grande" con la información de las
##páginas de la facultad.
##########################################################################


# borra_i_posible_grupo ---------------------------------------------------
#' Borra renglones de vec que contengan string_a_buscar
#'
#' @param string_a_buscar 
#' @param vec 
#'
#' @return
#' @export
#'
#' @examples
borra_i_posible_grupo <- function(string_a_buscar,vec){
  i_a_borrar <- grep(string_a_buscar, vec)
  if(length(i_a_borrar)>0) {
    vec <- vec[-(c(i_a_borrar,i_a_borrar-1))]
  }
  return(vec)
}



# corrige_i_posible_grupo -------------------------------------------------
#' Junta la entrada anterior y la que contiene a string_a_buscar
#'
#' @param string_a_buscar 
#' @param vec 
#'
#' @return
#' @export
#'
#' @examples
corrige_i_posible_grupo <- function(string_a_buscar,vec){
  i_corrige <- grep(string_a_buscar, vec)
  if(length(i_corrige)>0) {
    aux <- vec
    for(ii in 1:length(i_corrige)){
      aux[i_corrige[ii]-1] <- paste(vec[i_corrige[ii]-1],vec[i_corrige[ii]],collapse = "")
    }
    aux <- aux[-i_corrige]
    vec <- aux
  }
  return(vec)
}


# imprime_errores_mat_info_k_pag ------------------------------------------
#' Title: imprime_errores_mat_info_k_pag
#'
#' @param mat_info_k_pag 
#' @param list_url 
#'
#' @return
#' @export
#'
#' @examples
imprime_errores_mat_info_k_pag <- function(mat_info_k_pag,list_url) {
  
  col_names_tabla <- c("Materia","Profesor","Horario","Lugares","Alumnos","Salon  ",
                       "Grupo  ","Carrera","Plan  ", "Semestre","Cambios","Turno  ",
                       "Sem_de_mater","url    ")
  cat(" --- La dimension de mat_info_k_pag es ",dim(mat_info_k_pag)," ---\n",
      "------------------------------------------------------------------------------\n",
      "\t","Nombre","\t# vacios","\t% vacios","\t# NA","\t% NA","\n",
      "------------------------------------------------------------------------------\n")
  for(i in 1:ncol(mat_info_k_pag)) {
    porciento_vacios <- as.character(round(sum(mat_info_k_pag[,i]=="",na.rm = T)*100/nrow(mat_info_k_pag),
                                           digits = 2))
    # porciento_vacios <- paste(rep(" ",5-nchar(porciento_vacios)),porciento_vacios,collapse = "")
    n_vacios <- sum(mat_info_k_pag[,i]=="",na.rm = T)
    if(n_vacios==0) porciento_vacios <- 0
    
    cat(i,"\t", col_names_tabla[i],"\t",
        n_vacios,"\t\t", 
        porciento_vacios,
        "%\t\t",
        sum(is.na(mat_info_k_pag[,i])),"\t",
        round(sum(is.na(mat_info_k_pag[,i]))*100/nrow(mat_info_k_pag), digits = 2),
        "%\n")
  }
  cat(" --- Se revisaron ",nrow(mat_info_k_pag)," páginas distintas ---\n")
}



# extrae_info_1_pag -------------------------------------------------------
#' Extrae la información de una página web
#'
#' @param url 
#'
#' @return
#' @export
#'
#' @examples
#' load("Archivos RData V01/Dat_URL_20172_20201.RData") # Mediana
#'ii <- sample(1:nrow(list_url$mat_posibles_url),1)
#'url <- list_url$mat_posibles_url[ii,4]
#'extrae_info_1_pag(url)
#'
extrae_info_1_pag <- function(url) {
  cat("\nLeyendo url: ",url)
  # Sacamos toda la información de una página
  webpage <- read_html(url)
  data_html <- html_nodes(read_html(url),'#info-contenido')
  vec <- html_text(data_html)
  
  vec <- strsplit(vec,"Grupo ")[[1]] # [1] "a" "b" "c" "c" "b" "a"
  vec <- vec[-1]
  vec
  
  vec_strings_a_borrar <- c("especial","Especial","paralelo a ","xtraordinario por etapas")
  vec_strings_a_corrige <- c("exclusivo para ","semipresencial","nuevo aprobado por el CT")
  
  for(string_a_buscar in vec_strings_a_borrar)
    vec <- borra_i_posible_grupo(string_a_buscar,vec)
  
  ##En este ciclo "vec" puede quedar vacío por lo que se pone una
  ##condición en el siguiente for para que el programa no se detenga
  
  if(identical(vec,character(0))){
    vec <- 0
  }else{
    for(string_a_buscar in vec_strings_a_corrige)
      vec <- corrige_i_posible_grupo(string_a_buscar,vec)
  }
  
  if(identical(vec,character(0))){
    vec <- 0
  }
  
  vec
  
  # if(length(vec)>1){
  if(vec!=0){##Puede tener longitud 1 y si tener información (con esta condición)
    ##se generan "warnings" cuando si hay información en "vec"
    mat_info_un_pag <- matrix(0,length(vec),14)
    # colnames(mat_info) <- c("grupo","lugares","alumnos","profesor","horario","salon")
    
    colnames(mat_info_un_pag) <- c("Materia","Profesor","Horario","Lugares",
                                   "Alumnos","Salon","Grupo","Carrera","Plan",
                                   "Semestre","Cambios","Turno","Semestre_de_materia","url")
    
    # Extraemos de la página materia y semestre_de_materia
    materia_y_semestre_de_materia <- html_text(html_nodes(webpage,'#info-contenido h2'))
    
    # Extraemos variables constantes entre grupos: 
    # 1  Materia
    # 8  Carrera
    # 9  Plan
    # 10 Semestre
    # 13 Semestre_de_materia
    # 14 url
    
    # 1  Materia
    mat_info_un_pag[,1] <- substr(materia_y_semestre_de_materia,1,
                                  regexpr(',', materia_y_semestre_de_materia)[1]-1)  # Materia
    
    # Extraemos de la página la carrera y el plan
    carrera_plan <- html_text(html_nodes(webpage,'h1'))
    
    # 8  Carrera
    mat_info_un_pag[,8] <- substr(carrera_plan,1,nchar(carrera_plan)-12) # Carrera
    # 9  Plan
    mat_info_un_pag[,9] <- substr(carrera_plan,nchar(carrera_plan)-4,nchar(carrera_plan)-1)#Plan
    # 10 Semestre
    mat_info_un_pag[,10] <- substr(url,48,52) # Semestre
    # 13 Semestre_de_materia
    mat_info_un_pag[,13] <- substr(materia_y_semestre_de_materia,
                                   regexpr(',',materia_y_semestre_de_materia)[1]+2,
                                   nchar(materia_y_semestre_de_materia)) # Semestre_de_materia
    # 14 url
    mat_info_un_pag[,14] <- url
    
    i <- 2
    i <- 1
    # Con este for extraemos variables que cambian por grupo:
    # 2 Profesor
    # 3 Horario
    # 4 Lugares
    # 5 Alumnos
    # 6 Salón
    # 7 Grupo 
    for(i in 1:nrow(mat_info_un_pag)){
      x <- vec[i]
      x <- strsplit(x,"\n")[[1]] 
      x
      
      texto <- vec[i]
      texto
      
      # Separamos con respecto a \n
      texto <- strsplit(texto, split='\n', fixed=TRUE)[[1]]
      texto
      # Encontramos los lugares de profesor y ayudante
      i_profesores <- which(unlist(gregexpr('Profesor', texto))>0)+1
      i_ayudantess <- which(unlist(gregexpr('Ayudante', texto))>0)+1
      
      # Posibles mejoras utilizando los horarios de ayudantes
      # i_horariosss <- c(i_profesores,i_ayudantess)+1
      
      as.vector(regexpr(' a ', texto))
      
      aux_encuentra_a <- which(as.vector(regexpr(' a ', texto))>-1)
      aux_encuentra_a <- aux_encuentra_a[aux_encuentra_a>2]
      aux_encuentra_a
      
      i_horariosss <- aux_encuentra_a
      i_horariosss
      
      # Hacemos los vectores de prof y ayudante
      vec_i_profesores <- sort(unique(texto[i_profesores]))
      vec_i_ayudantess <- sort(unique(texto[i_ayudantess]))
      vec_i_horariosss <- sort(unique(texto[i_horariosss]))
      
      vec_i_profesores
      vec_i_ayudantess
      vec_i_horariosss
      
      # Eliminamos na
      vec_i_profesores <- paste(vec_i_profesores[!is.na(vec_i_profesores)],collapse = " / ")
      vec_i_ayudantess <- paste(vec_i_ayudantess[!is.na(vec_i_ayudantess)],collapse = " / ")
      vec_i_horariosss <- paste(vec_i_horariosss[!is.na(vec_i_horariosss)],collapse = " / ")
      
      vec_i_profesores
      vec_i_ayudantess
      vec_i_horariosss
      
      # mat_info_un_pag[i,3] <- x[8] # horario
      mat_info_un_pag[i,2] <- vec_i_profesores
      mat_info_un_pag[i,3] <- vec_i_horariosss # horario
      
      # Obs: se podría guardar la informacion de vec_i_ayudantess
      
      if(regexpr('lugares', x[1])[1]>0) {
        mat_info_un_pag[i,4] <- substr(x[1],7,regexpr('lugares', x[1])[1]-2) # lugares
        mat_info_un_pag[i,5] <- substr(x[1],regexpr('lugares', x[1])[1]+9,
                                       regexpr('alumno', x[1])[1]-2) # alumnos
      } else {
        mat_info_un_pag[i,4] <- "" # lugares
        mat_info_un_pag[i,5] <- substr(x[1],6,regexpr('alumno', x[1])[1]-2) # alumnos
      }
      if(mat_info_un_pag[i,5]=="Un") mat_info_un_pag[i,5] <- "1" # alumnos
      mat_info_un_pag[i,6] <- x[9] # salon
      mat_info_un_pag[i,7] <- substr(x[1],1,4) # grupo
    }
  }else{
    mat_info_un_pag <- matrix(0,length(vec),14)
  }##Fin de if(vec!=0)
  
  return(mat_info_un_pag)
}


# limpia_m_grande ---------------------------------------------------------
#' Title limpia_m_grande: Genera la matriz "m_grande" para cada semestre,
#' sin grupos repetidos.
#'
#' @param sem_info: Semestre del que se desea obtener información.
#' @param mat_info_k_pag: Matriz de 14 columnas (Materia,Profesor,Horario,
#' Lugares,Alumnos,Salon,Grupo,Carrera,Plan,Semestre,Cambios,Turno,
#' Semestre_de_materia,url), con la información de cada semestre.
#'
#' @example sem_info <- 20182
#' @example mat_info_k_pag[378,] = c(Problemas Socio-Económicos de México,
#' Silvia Alonso Reyes,7 a 8,"",38,O122,6007,Actuaría,2000,20101,0,0,Primer
#' Semestre,http://www.fciencias.unam.mx/docencia/horarios/20101/119/1109)
#' 
#' @return m_grande: Matriz de 36 columnas (Materia,Profesor,Horario,
#' horario_num,Lugares,Alumnos,Salon,Grupo,Carrera,Plan,Semestre,Cambios,
#' Turno,Semestre_de_materia,url,Act2000,Act2006,Act2015,CdC1994,CdC2013,
#' Mat1983,MAp2017,NomMat_Act2000,NomMat_Act2006,NomMat_Act2015,
#' NomMat_CdC1994,NomMat_CdC2013,NomMat_Mat1983,NomMat_MAp2017,URL_Act2000,
#' URL_Act2006,URL_Act2015,URL_CdC1994,URL_CdC2013,URL_Mat1983,URL_MAp2017),
#' con la información de "sem_info". Las columnas 16-22 son columnas
#' binarias las cuales indican con un 1 si el grupo del i-ésimo renglón
#' pertenece a la carrera y plan correspondiente al nombre de cada columna,
#' hay un 0 e.o.c. Las columnas 23-29 indican el nombre de las materias
#' dependiendo del plan y carrera al que pertenecen. Las columnas 30-36 
#' indican las URLs dependiendo del plan y carrera al que pertenecen.
#' 
limpia_m_grande <- function(sem_info,mat_info_k_pag,param){
  #Se eliminan los renglones con información repetida
  mat_info_k_pag = unique(mat_info_k_pag)
  
  ##Se quitan los renglones vacíos de la matriz:
  mat_info_k_pag <- matrix(mat_info_k_pag[mat_info_k_pag[1:(nrow(mat_info_k_pag)),
                                                         1]!=0,],ncol = dim(mat_info_k_pag)[2])
  
  #Se generan las matrices con la información de los grupos repetidos:
  i <- 1
  matriz_con_rep <- matrix(0,ncol = ncol(mat_info_k_pag),nrow = nrow(mat_info_k_pag))
  mat_resumen_rep <- matrix(0,nrow = nrow(mat_info_k_pag),ncol = 5)
  Profesores <- unique(mat_info_k_pag[,2])
  Horarios <- unique(mat_info_k_pag[,3])##String, no números
  
  for(p in 1:length(Profesores)){
    prof_iguales <- matrix(mat_info_k_pag[mat_info_k_pag[,2] == Profesores[p] , ],
                           ncol = 14)
    for(h in 1:length(Horarios)){
      horarios_iguales <- matrix(prof_iguales[prof_iguales[,3] == Horarios[h] , ],
                                 ncol = 14)
      n_rep <- nrow(horarios_iguales)
      if(n_rep>1){
        num_alumnos <- horarios_iguales[,5]
        alum_diferentes <- 0 #Suponemos que el número de alumnos es igual
        for(k in 2:n_rep){
          if(num_alumnos[1]!=num_alumnos[k]){
            alum_diferentes <- 1
          }
        }
        
        matriz_con_rep[i:(i+n_rep-1),] <- matrix(horarios_iguales,ncol = 14)
        mat_resumen_rep[i,] <- c(sem_info,Profesores[p],Horarios[h],
                                 n_rep,alum_diferentes)
        i <- i + n_rep
      }
    }##Fin de for de Horarios
  }##Fin de for de Profesores
  
  ##Quitamos los renglones vacíos de la matriz:
  matriz_con_rep <- matrix(matriz_con_rep[matriz_con_rep[1:(nrow(matriz_con_rep)),
                                                         1]!=0,],ncol = 14)
  colnames(matriz_con_rep) <- param$nom_cols_m14
  save(matriz_con_rep, file = paste0("mat_aux de limpia_m_grande/matriz_con_rep_",
                                     sem_info,".RData"))
  mat_resumen_rep <- matrix(mat_resumen_rep[mat_resumen_rep[1:(nrow(mat_resumen_rep)),1]!=0,],
                            ncol = 5)
  colnames(mat_resumen_rep) <- c("Semestre","Profesor","Horario","Repeticiones","AlumDif")
  save(mat_resumen_rep, file = paste0("mat_aux de limpia_m_grande/mat_resumen_rep_",
                                      sem_info,".RData"))
  
  n_gpos_alum_dif <- sum(as.numeric(mat_resumen_rep[,5]))
  cat("\nHay ",n_gpos_alum_dif,
      " grupos iguales con diferente número de alumnos para el semestre ",
      sem_info," de ",dim(mat_resumen_rep)[1]," grupos repetidos")
  
  
  #Se define "m_grande"
  m_grande <- as.data.frame(matrix(0,nrow = nrow(mat_info_k_pag),
                                   ncol = length(param$nom_cols_MG)))
  
  num_col_Materia <- arroja_ind_col_MG("Materia")##1
  num_col_Profesor <- arroja_ind_col_MG("Profesor")##2
  num_col_Horario <- arroja_ind_col_MG("Horario")##3
  num_col_horario_num <- arroja_ind_col_MG("horario_num")##4
  num_col_Lugares <- arroja_ind_col_MG("Lugares")##5
  num_col_Alumnos <- arroja_ind_col_MG("Alumnos")##6
  # num_col_Salon <- arroja_ind_col_MG("Salon")##7
  num_col_Grupo <- arroja_ind_col_MG("Grupo")##8
  num_col_Carrera <- arroja_ind_col_MG("Carrera")##9
  num_col_Plan <- arroja_ind_col_MG("Plan")##10
  # num_col_Semestre <- arroja_ind_col_MG("Semestre")##11
  # num_col_Cambios <- arroja_ind_col_MG("Cambios")##12
  num_col_Turno <- arroja_ind_col_MG("Turno")##13
  num_col_Semestre_de_materia <- arroja_ind_col_MG("Semestre_de_materia")##14
  num_col_url <- arroja_ind_col_MG("url")##15
  num_col_Act2000 <- arroja_ind_col_MG("Act2000")##16
  num_col_Act2006 <- arroja_ind_col_MG("Act2006")##17
  num_col_Act2015 <- arroja_ind_col_MG("Act2015")##18
  num_col_CdC1994 <- arroja_ind_col_MG("CdC1994")##19
  num_col_CdC2013 <- arroja_ind_col_MG("CdC2013")##20
  num_col_Mat1983 <- arroja_ind_col_MG("Mat1983")##21
  num_col_MAp2017 <- arroja_ind_col_MG("MAp2017")##22
  num_col_NomMat_Act2000 <- arroja_ind_col_MG("NomMat_Act2000")##23
  num_col_NomMat_Act2006 <- arroja_ind_col_MG("NomMat_Act2006")##24
  num_col_NomMat_Act2015 <- arroja_ind_col_MG("NomMat_Act2015")##25
  num_col_NomMat_CdC1994 <- arroja_ind_col_MG("NomMat_CdC1994")##26
  num_col_NomMat_CdC2013 <- arroja_ind_col_MG("NomMat_CdC2013")##27
  num_col_NomMat_Mat1983 <- arroja_ind_col_MG("NomMat_Mat1983")##28
  num_col_NomMat_MAp2017 <- arroja_ind_col_MG("NomMat_MAp2017")##29
  num_col_URL_Act2000 <- arroja_ind_col_MG("URL_Act2000")##30
  num_col_URL_Act2006 <- arroja_ind_col_MG("URL_Act2006")##31
  num_col_URL_Act2015 <- arroja_ind_col_MG("URL_Act2015")##32
  num_col_URL_CdC1994 <- arroja_ind_col_MG("URL_CdC1994")##33
  num_col_URL_CdC2013 <- arroja_ind_col_MG("URL_CdC2013")##34
  num_col_URL_Mat1983 <- arroja_ind_col_MG("URL_Mat1983")##35
  num_col_URL_MAp2017 <- arroja_ind_col_MG("URL_MAp2017")##36
  
  
  names(m_grande) <- param$nom_cols_MG
  m_grande[,num_col_Materia:num_col_Horario] <- mat_info_k_pag[,1:3]
  m_grande[,num_col_Lugares:num_col_url] <- mat_info_k_pag[,4:14]
  
  vec_indices_rep <- 0
  ##Se eliminan los renglones con información repetida
  vec_para_for <- 1:dim(m_grande)[1]
  pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
  for(k in vec_para_for){#Se recorre sobre los renglones de los grupos repetidos
    setTxtProgressBar(pb, k)
    for(r in 1:dim(mat_resumen_rep)[1]){
      profesor <- mat_resumen_rep[r,2]
      horario <- mat_resumen_rep[r,3]
      if(m_grande[k,num_col_Profesor]==profesor && 
         m_grande[k,num_col_Horario]==horario){
        vec_indices_rep <- c(vec_indices_rep,k)
      }
    }
  }
  close(pb)
  
  vec_indices_rep <- vec_indices_rep[-1]
  
  if(length(vec_indices_rep)>0){
    m_grande <- m_grande[-vec_indices_rep,]
  }
  
  ##Se llenan las últimas 7 columnas
  for(j in 1:dim(m_grande)[1]){
    switch(m_grande[j,num_col_Plan],
           '2000' = {m_grande[j,num_col_Act2000] = 1},
           '2006' = {m_grande[j,num_col_Act2006] = 1},
           '2015' = {m_grande[j,num_col_Act2015] = 1},
           '1994' = {m_grande[j,num_col_CdC1994] = 1},
           '2013' = {m_grande[j,num_col_CdC2013] = 1},
           '1983' = {m_grande[j,num_col_Mat1983] = 1},
           '2017' = {m_grande[j,num_col_MAp2017] = 1}
    )
  }
  
  ## Se agregan los renglones únicos (de la información repetida) a "m_grande"
  ##para tener completa la información
  d <- 1##Contador para la matriz "matriz_con_rep"
  cambio_materia <- 0
  cambio_grupo <- 0
  cambio_carrera <- 0
  cambio_plan <- 0
  
  for(k in 1:dim(mat_resumen_rep)[1]){
    cat("\nk = ",k)
    renglon <- c(matriz_con_rep[d,1:3],0,matriz_con_rep[d,4:14],
                 rep(0,(length(param$nom_cols_MG)-15)))
    num_repeticiones <- as.numeric(mat_resumen_rep[k,4])
    rango <- d:(d+num_repeticiones-1)
    
    ##Materia:
    if(length(unique(matriz_con_rep[rango,1]))>1){
      renglon[num_col_Materia] <- ""
      cambio_materia <- 1}
    
    ##Lugares:
    if(is.na(as.numeric(unique(matriz_con_rep[rango,4])))){
      renglon[num_col_Lugares] <- 0
    }else{
      renglon[num_col_Lugares] <- max(as.numeric(matriz_con_rep[rango,4]))
    }
    
    ##Alumnos:
    renglon[num_col_Alumnos] <- sum(as.numeric(unique(matriz_con_rep[rango,5])))
    
    ##Grupo:
    if(length(unique(matriz_con_rep[rango,7]))>1){
      renglon[num_col_Grupo] <- ""
      cambio_grupo <- 1}
    
    ##Carrera
    if(length(unique(matriz_con_rep[rango,8]))>1){
      renglon[num_col_Carrera] <- ""
      cambio_carrera <- 1}
    
    ##Plan
    if(length(unique(matriz_con_rep[rango,9]))>1){
      renglon[num_col_Plan] <- ""
      cambio_plan <- 1}
    
    ##Semestre de materia
    renglon[num_col_Semestre_de_materia] <- ""
    
    ##URL
    renglon[num_col_url] <- ""
    
    for(r in 1:num_repeticiones){
      ##Materia:
      if(cambio_materia == 1){
        renglon[num_col_Materia] <- paste(renglon[num_col_Materia],matriz_con_rep[d,1],sep = "/")}
      
      ##Grupo:
      if(cambio_grupo == 1){
        renglon[num_col_Grupo] <- paste(renglon[num_col_Grupo],matriz_con_rep[d,7],sep = "/")}
      
      ##Carrera
      if(cambio_carrera == 1){
        renglon[num_col_Carrera] <- paste(renglon[num_col_Carrera],matriz_con_rep[d,8],sep = "/")}
      
      ##Plan
      if(cambio_plan == 1){
        renglon[num_col_Plan] <- paste(renglon[num_col_Plan],matriz_con_rep[d,9],sep = "/")}
      
      ##Semestre de materia
      renglon[num_col_Semestre_de_materia] <- paste(renglon[num_col_Semestre_de_materia],matriz_con_rep[d,13],sep = "/")
      
      ##URL
      renglon[num_col_url] <- paste(renglon[num_col_url],matriz_con_rep[d,14],sep = "/")
      
      switch(matriz_con_rep[d,9],
             '2000' = {renglon[num_col_Act2000] = 1;
             renglon[num_col_NomMat_Act2000] = matriz_con_rep[d,1];
             renglon[num_col_URL_Act2000] = matriz_con_rep[d,14]},
             '2006' = {renglon[num_col_Act2006] = 1;
             renglon[num_col_NomMat_Act2006] = matriz_con_rep[d,1];
             renglon[num_col_URL_Act2006] = matriz_con_rep[d,14]},
             '2015' = {renglon[num_col_Act2015] = 1;
             renglon[num_col_NomMat_Act2015] = matriz_con_rep[d,1];
             renglon[num_col_URL_Act2015] = matriz_con_rep[d,14]},
             '1994' = {renglon[num_col_CdC1994] = 1;
             renglon[num_col_NomMat_CdC1994] = matriz_con_rep[d,1];
             renglon[num_col_URL_CdC1994] = matriz_con_rep[d,14]},
             '2013' = {renglon[num_col_CdC2013] = 1;
             renglon[num_col_NomMat_CdC2013] = matriz_con_rep[d,1];
             renglon[num_col_URL_CdC2013] = matriz_con_rep[d,14]},
             '1983' = {renglon[num_col_Mat1983] = 1;
             renglon[num_col_NomMat_Mat1983] = matriz_con_rep[d,1];
             renglon[num_col_URL_Mat1983] = matriz_con_rep[d,14]},
             '2017' = {renglon[num_col_MAp2017] = 1;
             renglon[num_col_NomMat_MAp2017] = matriz_con_rep[d,1];
             renglon[num_col_URL_MAp2017] = matriz_con_rep[d,14]}
      )
      d <- d + 1
    }
    m_grande <- rbind(m_grande,renglon)
  }##Fin de for(k)
  
  ## Se llena la columna 4 "horario_num" con los datos de los horarios
  ##en variables de tipo "numeric"
  ##Se agrega la columna que tiene los horarios como números y no como string
  horario_string <- m_grande[,num_col_Horario]
  ## Se toman los primeros 2 caracteres de cada hora y se convierte en
  ##número, al encontrar ":" toma sólo el primer caracter y al encontrar
  ## "" en la entrada se pone un cero
  horario_numeros <- as.numeric(substr(horario_string,1,2))
  # View(horario_numeros)
  indices_NA <- rep(0,length(horario_numeros))
  k <- 1
  for(i in 1:length(horario_numeros)){
    cat("\n i =",i)
    if(is.na(horario_numeros[i])){
      indices_NA[k] <- i
      k <- k + 1}}
  indices_NA <- indices_NA[indices_NA>0]
  
  if(length(indices_NA) > 0){
    for(i in 1:length(indices_NA)){
      if(substr(horario_string[indices_NA[i]],2,2) == ":"){
        horario_numeros[indices_NA[i]] <- 
          as.numeric(substr(horario_string[indices_NA[i]],1,1))
      }else if(substr(horario_string[indices_NA[i]],2,2) == ""){
        horario_numeros[indices_NA[i]] <- 0
      }
    }
  }
  # View(horario_numeros)
  m_grande[,num_col_horario_num] <- horario_numeros
  
  ##Se ponen ceros en la columna de "Lugares" en caso de tenerlos
  lugares <- as.numeric(m_grande[,num_col_Lugares])
  for(i in 1:length(lugares)){
    if(is.na(lugares[i])){
      lugares[i] <- 0
    }
  }
  # View(lugares)
  m_grande[,num_col_Lugares] <- lugares
  
  ##Se ponen ceros en la columna de "Alumnos" en caso de tenerlos
  num_alumnos <- as.numeric(m_grande[,num_col_Alumnos])
  for(i in 1:length(num_alumnos)){
    if(is.na(num_alumnos[i])){
      num_alumnos[i] <- 0
    }
  }
  # View(num_alumnos)
  m_grande[,num_col_Alumnos] <- num_alumnos
  
  ##Se llena la columna 13 "Turno"
  ##Turno matutino: 7am - 14:00hrs (incluyendo la clase de 14-15hrs)
  ##Turno vespertino: 15-21 hrs (incluyendo la clase de 21-22hrs)
  Turno <- m_grande[,num_col_Turno]
  for(i in 1:length(horario_numeros)){
    if(is.na(horario_numeros[i])){
      # Turno[i] <- "NA"
      Turno[i] <- 0
    }else if(horario_numeros[i] < 15){
      Turno[i] <- "M" ##Matutino
    }else if(horario_numeros[i] >= 15){
      Turno[i] <- "V" ##Vespertino
    }
  }
  # View(Turno)
  m_grande[,num_col_Turno] <- Turno
  
  # View(m_grande)
  return(m_grande)
}


# gen_m_grande_SIN_MOD -------------------------------------------------------
#' Title gen_m_grande_SIN_MOD: Función que recibe como parámetro el semestre
#' del que se desea obtener la información y genera un archivo de tipo
#' ".Rdata" con la matriz "m_grande" de dicho semestre. Regresa la matriz
#' "m_grande_SIN_MOD" de "sem_info".
#'
#' @param sem_info: Semestre del que se desea obtener información.
#' @param list_url: Lista con parámetros utilizados
#' 
#' @example sem_info <- 20182
#' @example list_url <-list()
#' 
#' @return m_grande: Matriz de 36 columnas (Materia,Profesor,Horario,
#' horario_num,Lugares,Alumnos,Salon,Grupo,Carrera,Plan,Semestre,Cambios,
#' Turno,Semestre_de_materia,url,Act2000,Act2006,Act2015,CdC1994,CdC2013,
#' Mat1983,MAp2017,NomMat_Act2000,NomMat_Act2006,NomMat_Act2015,
#' NomMat_CdC1994,NomMat_CdC2013,NomMat_Mat1983,NomMat_MAp2017,URL_Act2000,
#' URL_Act2006,URL_Act2015,URL_CdC1994,URL_CdC2013,URL_Mat1983,URL_MAp2017),
#' con la información de "sem_info". Las columnas 16-22 son columnas
#' binarias las cuales indican con un 1 si el grupo del i-ésimo renglón
#' pertenece a la carrera y plan correspondiente al nombre de cada columna,
#' hay un 0 e.o.c. Las columnas 23-29 indican el nombre de las materias
#' dependiendo del plan y carrera al que pertenecen. Las columnas 30-36 
#' indican las URLs dependiendo del plan y carrera al que pertenecen.
#'
gen_m_grande_SIN_MOD <- function(sem_info,list_url,param){
  mat_posibles_url <- list_url$mat_posibles_url
  # direccion_info <- paste0("m_grande por semestre/m_grande_",sem_info,".RData")
  direccion_info <- paste0("m_grande por semestre SIN MODIFICAR/m_grande_SIN_MOD_",
                           sem_info,".RData")
  
  #Este vector contiene las url del "sem_info"
  vec_url_sem <- mat_posibles_url[mat_posibles_url[,1]==sem_info,4]
  
  #Factor por pagina: Es un estimado de cuantos grupos hay por página (a ojo)
  media_grupos_por_pagina <- 10
  mat_info_k_pag <- matrix(0,length(vec_url_sem)*media_grupos_por_pagina,14)
  
  colnames(mat_info_k_pag) <- c("Materia","Profesor","Horario","Lugares","Alumnos","Salon",
                                "Grupo","Carrera","Plan","Semestre","Cambios","Turno",
                                "Semestre_de_materia","url")
  
  if(!file.exists(direccion_info)){
    ##En caso de que el archivo de la matriz "m_grande" no exista:
    vec_para_for <- 1:length(vec_url_sem)
    pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
    
    indice_grupos <- 1
    for(i in vec_para_for){
      # cat("\ni = ",i)
      setTxtProgressBar(pb, i)
      url <- vec_url_sem[i]
      mat_info_un_pag <- extrae_info_1_pag(url)
      # mat_info_un_pag <- matrix(-1,sample(2:10,1),13)
      mat_info_k_pag[indice_grupos:(indice_grupos+nrow(mat_info_un_pag)-1),] <- mat_info_un_pag
      # cat(indice_grupos,(indice_grupos+nrow(mat_info_un_pag)-1),"\n")
      indice_grupos <- indice_grupos + nrow(mat_info_un_pag)
    }
    close(pb)
    
    mat_info_k_pag <- mat_info_k_pag[1:(indice_grupos-1),]
    nom_archivo <- paste0("mat_info_k_pag por semestre/mat_info_k_pag_",sem_info,".RData")
    save(mat_info_k_pag, file = nom_archivo)
    
    # nombre_m_grande <- paste0("m_grande_",sem_info)
    # assign(nombre_m_grande,mat_info_k_pag)
    # nom_matriz <- as.name(nombre_m_grande)
    ### REVISAR CÓMO GUARDAR LA MATRIZ CON EL NOMBRE ADECUADO ###
    ### EXCEPCIONES ###
    # vec_excepciones <- 1##Caso de inglés
    # m_grande <- limpia_m_grande(sem_info,mat_info_k_pag,param,vec_excepciones)
    m_grande <- limpia_m_grande(sem_info,mat_info_k_pag,param)
    
    # Salvando archivo
    save(m_grande, file = direccion_info)
  }else{
    load(direccion_info)
  }
  imprime_errores_mat_info_k_pag(m_grande,list_url)
  
  # return(direccion_info)
  return(m_grande)
}


# actualiza_m_grande_1_sem ------------------------------------------------
#' Title actualiza_m_grande_1_sem: Función que actualiza la matriz
#' "m_grande_SIN_ING" de "sem_info".
#' - Se unen las materias iguales en un solo renglón
#' - Se conserva el nombre más reciente de la materia repetida
#' - Se verifica que las matrices sólo tengan datos del semestre
#' correspondiente.
#'
#' @param sem_info: Semestre del que se desea obtener información.
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#' 
#' @example sem_info <- 20182
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
#' @return m_grande: Matriz de 36 columnas (Materia,Profesor,Horario,
#' horario_num,Lugares,Alumnos,Salon,Grupo,Carrera,Plan,Semestre,Cambios,
#' Turno,Semestre_de_materia,url,Act2000,Act2006,Act2015,CdC1994,CdC2013,
#' Mat1983,MAp2017,NomMat_Act2000,NomMat_Act2006,NomMat_Act2015,
#' NomMat_CdC1994,NomMat_CdC2013,NomMat_Mat1983,NomMat_MAp2017,URL_Act2000,
#' URL_Act2006,URL_Act2015,URL_CdC1994,URL_CdC2013,URL_Mat1983,URL_MAp2017),
#' con la información de "sem_info". Las columnas 16-22 son columnas
#' binarias las cuales indican con un 1 si el grupo del i-ésimo renglón
#' pertenece a la carrera y plan correspondiente al nombre de cada columna,
#' hay un 0 e.o.c. Las columnas 23-29 indican el nombre de las materias
#' dependiendo del plan y carrera al que pertenecen. Las columnas 30-36 
#' indican las URLs dependiendo del plan y carrera al que pertenecen.
#'
actualiza_m_grande_1_sem <- function(sem_info,param){
  #Se carga la matriz "m_grande" que se va a actualizar
  nom_archivo <- paste0("m_grande por semestre SIN MODIFICAR/m_grande_SIN_MOD_",
                        sem_info,".RData")
  load(nom_archivo)
  m_grande_SIN_MOD <- m_grande
  
  ##Se definen las variables que se van a utilizar:
  m_grande <- as.data.frame(matrix(0,ncol = length(param$nom_cols_MG)))
  names(m_grande) <- param$nom_cols_MG
  mat_aux_rep <- as.data.frame(matrix(0,ncol = length(param$nom_cols_MG)))
  names(mat_aux_rep) <- param$nom_cols_MG
  num_col_Materia <- arroja_ind_col_MG("Materia")##1
  num_col_Profesor <- arroja_ind_col_MG("Profesor")##2
  num_col_Horario <- arroja_ind_col_MG("Horario")##3
  num_col_horario_num <- arroja_ind_col_MG("horario_num")##4
  num_col_Lugares <- arroja_ind_col_MG("Lugares")##5
  num_col_Alumnos <- arroja_ind_col_MG("Alumnos")##6
  num_col_Salon <- arroja_ind_col_MG("Salon")##7
  num_col_Grupo <- arroja_ind_col_MG("Grupo")##8
  num_col_Carrera <- arroja_ind_col_MG("Carrera")##9
  num_col_Plan <- arroja_ind_col_MG("Plan")##10
  num_col_Semestre <- arroja_ind_col_MG("Semestre")##11
  num_col_Cambios <- arroja_ind_col_MG("Cambios")##12
  num_col_Turno <- arroja_ind_col_MG("Turno")##13
  num_col_Sem_de_mat <- arroja_ind_col_MG("Semestre_de_materia")##14
  num_col_url <- arroja_ind_col_MG("url")##15
  num_col_Act2000 <- arroja_ind_col_MG("Act2000")##16
  num_col_MAp2017 <- arroja_ind_col_MG("MAp2017")##22
  num_col_NomMat_Act2006 <- arroja_ind_col_MG("NomMat_Act2006")##24
  num_col_NomMat_CdC1994 <- arroja_ind_col_MG("NomMat_CdC1994")##26
  num_col_URL_Act2006 <- arroja_ind_col_MG("URL_Act2006")##31
  num_col_URL_CdC1994 <- arroja_ind_col_MG("URL_CdC1994")##33
  num_col_URL_MAp2017 <- arroja_ind_col_MG("URL_MAp2017")##36
  
  #Se carga el archivo con las materias repetidas desde el 2008-1 al 2020-1
  load("mat_materias_rep_20081_20201.RData")
  
  #Se define una matriz auxiliar con la información repetida del "sem_info"
  materias_rep_aux <- mat_materias_rep[mat_materias_rep[,1]==sem_info,]
  
  #Se verifica que la matriz sólo tenga datos del semestre "sem_info"
  m_grande_SIN_MOD <- m_grande_SIN_MOD[m_grande_SIN_MOD[,num_col_Semestre]==sem_info,]
  
  if(dim(materias_rep_aux)[1]>1){##Si hay repeticiones
    profesores_unique <- unique(materias_rep_aux[,2])
    horarios_unique <- unique(materias_rep_aux[,4])
    num_profesores <- length(profesores_unique)
    if(length(horarios_unique) < length(profesores_unique)){
      horarios_unique <- c(horarios_unique,
                           rep(0,(length(profesores_unique)-length(horarios_unique))))}
    
    #Se eliminan los renglones repetidos
    ind_aux <- 0
    for(d in 1:num_profesores){
      # cat("\n d = ",d)
      for(r in 1:dim(m_grande_SIN_MOD)[1]){
        # cat("\n r = ",r)
        # cat("\n Profesor: ",m_grande_SIN_MOD[r,num_col_Profesor])
        if(m_grande_SIN_MOD[r,num_col_Profesor]==profesores_unique[d] && 
           m_grande_SIN_MOD[r,num_col_horario_num] == horarios_unique[d]){
          ind_aux <- c(ind_aux,r)}}}
    ## Se quita el cero inicial
    ind_aux <- ind_aux[-1]
    ind_aux <- unique(ind_aux)
    
    mat_aux <- m_grande_SIN_MOD[ind_aux,]
    m_grande <- m_grande_SIN_MOD[-ind_aux,]
    
    #Se agrega la información combinada por casos:
    switch(sem_info,
           #' Las materias "Seminario de Aplicaciones de Cómputo II" y
           #' "Biología Matemática I", son la misma, así se se combinan
           #' los renglones se agrega a "m_grande"
           '20101' = {renglon <- c("Biología Matemática I",
                                   mat_aux[1,num_col_Profesor:num_col_Lugares],
                                   sum(as.numeric(mat_aux[,num_col_Alumnos])),
                                   "Taller de Análisis Numérico",
                                   "7040/4207",
                                   paste0(mat_aux[1,num_col_Carrera],
                                          mat_aux[2,num_col_Carrera]),
                                   paste0(mat_aux[1,num_col_Plan],mat_aux[2,num_col_Plan]),
                                   mat_aux[1,num_col_Semestre:num_col_Turno],
                                   paste0(mat_aux[1,num_col_Sem_de_mat],
                                          mat_aux[2,num_col_Sem_de_mat]),
                                   paste0(mat_aux[1,num_col_url],mat_aux[2,num_col_url]),
                                   0,0,0,1,0,1,1,0,0,0,
                                   "Seminario de Aplicaciones de Cómputo II",0,
                                   "Biología Matemática I","Biología Matemática I",0,0,0,
                                   "http://www.fciencias.unam.mx/docencia/horarios/20101/218/424",
                                   0,
                                   "http://www.fciencias.unam.mx/docencia/horarios/20101/217/275",
                                   "http://www.fciencias.unam.mx/docencia/horarios/20101/2055/275")
           renglon[num_col_Cambios] <- "1/"
           names(renglon) <- param$nom_cols_MG  
           m_grande <- rbind(m_grande,renglon)
           },##Fin 20101
           '20111' = {
             #' Las materias "Ecuaciones Diferenciales I" y "Cálculo Diferencial e
             #' Integral I", son diferentes, pero las clases si comienzan a la misma
             #' hora, Ecuaciones de 18-19hrs y Cálculo de 18-20hrs, dado que se tiene
             #' la misma ayudante pudiera ser que se intercambien las horas, pero
             #' no se puede asignar más de una clase a la misma hora al mismo profesor.
             renglon_1 <- c("Ecuaciones Diferenciales I",
                            mat_aux[1,num_col_Profesor:num_col_Alumnos],
                            "O123","4172",
                            mat_aux[1,num_col_Carrera:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/"
             names(renglon_1) <- param$nom_cols_MG  
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c("Cálculo Diferencial e Integral I",
                            mat_aux[2,num_col_Profesor:num_col_Alumnos],
                            "Taller Interdisciplinario de Física y Biomedicina I",
                            "4039",
                            mat_aux[2,num_col_Carrera:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/"
             names(renglon_2) <- param$nom_cols_MG  
             m_grande <- rbind(m_grande,renglon_2)
           },##Fin 20111
           '20121' = {
             #' Las materias "Inteligencia Artificial" y "Lingüística Computacional",
             #' son diferentes, y los horarios también, coinciden por las clases de los
             #' ayudantes.
             renglon_1 <- c("Inteligencia Artificial",
                            mat_aux[1,num_col_Profesor:num_col_Horario],
                            18,##Empieza a las 18:30hrs y termina a las 20hrs
                            mat_aux[1,num_col_Lugares:num_col_Alumnos],
                            "Taller de Lenguajes de Programación","7016",
                            mat_aux[1,num_col_Carrera:num_col_Cambios],
                            "V",
                            mat_aux[1,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/"
             names(renglon_1) <- param$nom_cols_MG  
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c("Lingüística Computacional",
                            mat_aux[2,num_col_Profesor:num_col_Horario],
                            17,##Empieza a las 17hrs y termina a las 18:30hrs
                            mat_aux[2,num_col_Lugares:num_col_Alumnos],
                            "Taller de Ingeniería de Software","7027",
                            mat_aux[2,num_col_Carrera:num_col_Cambios],
                            "V",
                            mat_aux[2,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/"
             names(renglon_2) <- param$nom_cols_MG  
             m_grande <- rbind(m_grande,renglon_2)
             
             #' Las materias "Introducción a Ciencias de la Computación" y "Teoría
             #' de la Computación", son diferentes, y los horarios también, coinciden
             #' por las clases de los ayudantes.
             renglon_3 <- c(mat_aux[3,num_col_Materia:num_col_Horario],
                            16,##Empieza a las 16hrs y termina a las 17:30hrs
                            mat_aux[3,num_col_Lugares:num_col_Alumnos],
                            "O218",
                            mat_aux[3,num_col_Grupo:num_col_Cambios],
                            "V",
                            mat_aux[3,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_3[num_col_Cambios] <- "1/"
             names(renglon_3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_3)
             
             renglon_4 <- c("Teoría de la Computación",
                            mat_aux[4,num_col_Profesor:num_col_Alumnos],
                            "P210","7014",
                            mat_aux[4,num_col_Carrera:num_col_URL_MAp2017])
             renglon_4[num_col_Cambios] <- "1/"
             names(renglon_4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_4)
           },##Fin 20121
           '20122' = {
             ind_aux <- 0
             for(r in 1:dim(m_grande)[1]){
               if(m_grande[r,num_col_Profesor]=="Antonio Francisco Menéndez Leonel de Cervantes" && 
                  m_grande[r,num_col_horario_num] == "14"){
                 ind_aux <- c(ind_aux,r)}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             #' Las materias "Programación I" e "Introducción a Ciencias
             #' de la Computación I" son diferentes, los horarios coinciden
             #' por las horas de los ayudantes.
             renglon_1 <- c("Programación I",
                            mat_aux[1,num_col_Profesor:num_col_Alumnos],
                            "Laboratorio de Enseñanza de Cómputo de Actuaría","6019",
                            mat_aux[1,num_col_Carrera:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/"
             names(renglon_1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c("Introducción a Ciencias de la Computación I",
                            mat_aux[2,num_col_Profesor:num_col_Horario],
                            16,##Empieza a las 16hrs y termina a las 17:30hrs
                            mat_aux[2,num_col_Lugares:num_col_Alumnos],
                            "Laboratorio de Ciencias de la Computación 3",
                            mat_aux[2,num_col_Grupo:num_col_Cambios],
                            "V",
                            mat_aux[2,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/"
             names(renglon_2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_2)
             
             #' Las materias "Seminario de Computación Teórica" y "Teoría de Códigos"
             #' son diferentes, los horarios coinciden por las horas de los ayudantes.
             renglon_3 <- c(mat_aux[3,num_col_Materia:num_col_Horario],
                            18,##Empieza a las 18:30hrs y termina a las 20hrs
                            mat_aux[3,num_col_Lugares:num_col_Alumnos],
                            "Laboratorio de Ciencias de la Computación 2",
                            mat_aux[3,num_col_Grupo:num_col_Cambios],
                            "V",
                            mat_aux[3,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_3[num_col_Cambios] <- "1/"
             names(renglon_3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_3)
             
             renglon_4 <- c(mat_aux[4,num_col_Materia:num_col_Horario],
                            17,##Empieza a las 17hrs y termina a las 18:30hrs
                            mat_aux[4,num_col_Lugares:num_col_Alumnos],
                            "P104",
                            mat_aux[4,num_col_Grupo:num_col_Cambios],
                            "V",
                            mat_aux[4,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_4[num_col_Cambios] <- "1/"
             names(renglon_4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_4)
             
             
             #' Las materias "Proceso Digital de Imagenes" y "Redes de Computadoras"
             #' son diferentes, los horarios coinciden por las horas de los ayudantes.
             renglon_5 <- c("Proceso Digital de Imagenes",
                            mat_aux[5,num_col_Profesor:num_col_Horario],
                            8,
                            mat_aux[5,num_col_Lugares:num_col_Alumnos],
                            "Laboratorio de Innovación Tecnológica","7037",
                            "Ciencias de la Computación/Ciencias de la Computación",
                            mat_aux[5,num_col_Plan:num_col_URL_MAp2017])
             renglon_5[num_col_Cambios] <- "1/"
             names(renglon_5) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_5)
             
             renglon_6 <- c("Redes de Computadora",
                            mat_aux[6,num_col_Profesor:num_col_Horario],
                            7,
                            mat_aux[6,num_col_Lugares:num_col_Alumnos],
                            "Laboratorio de Innovación Tecnológica","7069",
                            mat_aux[6,num_col_Carrera:num_col_URL_MAp2017])
             renglon_6[num_col_Cambios] <- "1/"
             names(renglon_6) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_6)
           },##Fin 20122
           '20131' = {
             #' Las materias "Redes de Computadoras" e "Introducción a Ciencias
             #' de la Computación I" son diferentes, los horarios coinciden
             #' por las horas de los ayudantes.
             renglon_1 <- c("Redes de Computadoras",
                            mat_aux[1,num_col_Profesor:num_col_Salon],
                            "7016",
                            mat_aux[1,num_col_Carrera:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/"
             names(renglon_1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c("Introducción a Ciencias de la Computación I",
                            mat_aux[2,num_col_Profesor:num_col_Horario],
                            16,##Empieza a las 16hrs y termina a las 17:30hrs
                            39,40,
                            "Laboratorio de Ciencias de la Computación 2",
                            mat_aux[2,num_col_Grupo:num_col_Cambios],
                            "V",
                            mat_aux[2,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/"
             names(renglon_2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_2)
             
             
             #' Las materias "Probabilidad y Estadística" y "Estadística I"
             #' son diferentes, los horarios coinciden por las horas de los ayudantes.
             renglon_3 <- c(mat_aux[3,num_col_Materia:num_col_Horario],
                            17,##Empieza a las 17hrs y termina a las 18:30hrs
                            mat_aux[3,num_col_Lugares:num_col_URL_MAp2017])
             renglon_3[num_col_Cambios] <- "1/"
             names(renglon_3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_3)
             
             renglon_4 <- c("Estadística I",
                            mat_aux[4,num_col_Profesor:num_col_Salon],
                            "6218",
                            mat_aux[4,num_col_Carrera:num_col_URL_MAp2017])
             renglon_4[num_col_Cambios] <- "1/"
             names(renglon_4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_4)
           },##Fin 20131
           '20132' = {
             ind_aux <- 0
             for(r in 1:dim(m_grande)[1]){
               if(m_grande[r,num_col_Profesor]=="José de Jesús Galaviz Casas" && 
                  m_grande[r,num_col_horario_num] == "10"){
                 ind_aux <- c(ind_aux,r)}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             ind_aux <- 0
             for(r in 1:dim(m_grande)[1]){
               if(m_grande[r,num_col_Profesor]=="Manuel Cristobal López Michelone" && 
                  m_grande[r,num_col_horario_num] == "14"){
                 ind_aux <- c(ind_aux,r)}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             #' Las materias "	Matemáticas para las Ciencias Aplicadas IV" y
             #' "Cálculo Diferencial e Integral II" son diferentes, los horarios
             #' coinciden por las horas de los ayudantes.
             renglon_1 <- c(mat_aux[1,num_col_Materia:num_col_Alumnos],
                            "P208",
                            mat_aux[1,num_col_Grupo:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/"
             names(renglon_1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c(mat_aux[2,num_col_Materia:num_col_Horario],
                            9,##Empieza a las 9hrs y termina a las 11hrs
                            mat_aux[2,num_col_Lugares:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/"
             names(renglon_2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_2)
             
             
             #' Las materias "Fundamentos de Bases de Datos" y "Modelado y Programación"
             #' son diferentes, los horarios coinciden por las horas de los ayudantes.
             renglon_3 <- c("Fundamentos de Bases de Datos",
                            mat_aux[3,num_col_Profesor:num_col_Horario],
                            8,53,51,"102 (Yelizcalli)",
                            mat_aux[3,num_col_Grupo:num_col_URL_MAp2017])
             renglon_3[num_col_Cambios] <- "1/"
             names(renglon_3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_3)
             
             renglon_4 <- c("Modelado y Programación",
                            mat_aux[4,num_col_Profesor:num_col_Horario],
                            16,##Empieza a las 16hrs y termina a las 17:30hrs
                            mat_aux[4,num_col_Lugares:num_col_Salon],
                            "7041",
                            mat_aux[4,num_col_Carrera:num_col_Cambios],
                            "V",
                            mat_aux[4,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_4[num_col_Cambios] <- "1/"
             names(renglon_4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_4)
             
             
             #' Las materias "Seminario de Computación Teórica" y "Organización y
             #' Arquitectura de Computadoras" son diferentes, los horarios coinciden
             #' por las horas de los ayudantes.
             renglon_5 <- c(mat_aux[5,num_col_Materia:num_col_URL_MAp2017])
             renglon_5[num_col_Cambios] <- "1/"
             names(renglon_5) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_5)
             
             renglon_6 <- c("Organización y Arquitectura de Computadoras",
                            mat_aux[6,num_col_Profesor:num_col_Horario],
                            16,##Empieza a las 16hrs y termina a las 17:30hrs
                            31,30,"P211",
                            mat_aux[6,num_col_Grupo:num_col_Cambios],
                            "V",
                            mat_aux[6,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_6[num_col_Cambios] <- "1/"
             names(renglon_6) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_6)
             
             
             #' Las materias "Proceso Digital de Imagenes" e "Inteligencia
             #' Artificial" son diferentes, los horarios coinciden por las
             #' horas de los ayudantes.
             renglon_7 <- c(mat_aux[7,num_col_Materia:num_col_URL_MAp2017])
             renglon_7[num_col_Cambios] <- "1/"
             names(renglon_7) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_7)
             
             renglon_8 <- c("Inteligencia Artificial",
                            mat_aux[8,num_col_Profesor:num_col_Horario],
                            9,
                            mat_aux[8,num_col_Lugares:num_col_Salon],
                            "7001",
                            mat_aux[8,num_col_Carrera:num_col_URL_MAp2017])
             renglon_8[num_col_Cambios] <- "1/"
             names(renglon_8) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_8)
           },##Fin 20132
           '20141' = {
             #' Las materias "Introducción a Ciencias de la Computación I" y
             #' "Modelado y Programación" son diferentes, coinciden en horario
             #' pero se imparten en días distintos
             renglon_1 <- c("Introducción a Ciencias de la Computación I",
                            mat_aux[1,num_col_Profesor],
                            "10 a 12 / 13 a 14 / 16 a 17:30	Lu Mi",
                            16,##Empieza a las 16hrs y termina a las 17:30hrs
                            mat_aux[1,num_col_Lugares:num_col_Cambios],
                            "V",
                            mat_aux[1,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/2/"
             names(renglon_1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c("Modelado y Programación",
                            mat_aux[2,num_col_Profesor],
                            "10 a 12 / 16 a 17:30	Ma Ju",
                            16,##Empieza a las 16hrs y termina a las 17:30hrs
                            mat_aux[2,num_col_Lugares:num_col_Salon],
                            "7045",
                            mat_aux[2,num_col_Carrera:num_col_Cambios],
                            "V",
                            mat_aux[1,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/2/"
             names(renglon_2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_2)
             
             
             #' Las materias "Diseño de interfaces" y "Programación I" son
             #' diferentes, los horarios coinciden por las horas de los ayudantes.
             renglon_3 <- c(mat_aux[3,num_col_Materia:num_col_Horario],
                            17,##Empieza a las 17hrs y termina a las 18:30hrs
                            mat_aux[3,num_col_Lugares:num_col_Cambios],
                            "V",
                            mat_aux[3,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_3[num_col_Cambios] <- "1/"
             names(renglon_3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_3)
             
             renglon_4 <- c("Programación I",
                            mat_aux[4,num_col_Profesor:num_col_Salon],
                            "9285",
                            mat_aux[4,num_col_Carrera:num_col_URL_MAp2017])
             renglon_4[num_col_Cambios] <- "1/"
             names(renglon_4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_4)
           },##Fin 20141
           '20142' = {
             ind_aux <- 0
             prof_aux_1 <- c("José de Jesús Galaviz Casas","Baruch Demian Gaxiola Valles")
             for(r in 1:dim(m_grande)[1]){
               for(p in 1:length(prof_aux_1)){
                 if(m_grande[r,num_col_Profesor]==prof_aux_1[p] && 
                    m_grande[r,num_col_horario_num] == "10"){
                   ind_aux <- c(ind_aux,r)}}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             ind_aux <- 0
             prof_aux_2 <- c("Selene Marisol Martínez Ramírez","Manuel Cristobal López Michelone")
             for(r in 1:dim(m_grande)[1]){
               for(p in 1:length(prof_aux_2)){
                 if(m_grande[r,num_col_Profesor]==prof_aux_2[p] && 
                    m_grande[r,num_col_horario_num] == "14"){
                   ind_aux <- c(ind_aux,r)}}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             #' Las materias "Algoritmos Paralelos" y "Estructuras de Datos" son
             #' diferentes, los horarios coinciden por las horas de los ayudantes.
             renglon_1 <- c("Algoritmos Paralelos",
                            mat_aux[1,num_col_Profesor:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/"
             names(renglon_1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c("Estructuras de Datos",
                            mat_aux[2,num_col_Profesor:num_col_Horario],
                            16,##Empieza a las 16hrs y termina a las 17:30hrs
                            mat_aux[2,num_col_Lugares:num_col_Cambios],
                            "V",
                            mat_aux[2,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/"
             names(renglon_2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_2)
             
             
             #' Las materias "Lógica Computacional" y "Semántica y Verificación" son
             #' diferentes, los horarios coinciden por las horas de los ayudantes.
             renglon_3 <- c("Lógica Computacional",
                            mat_aux[3,num_col_Profesor:num_col_Horario],
                            16,##Empieza a las 16hrs y termina a las 17:30hrs
                            mat_aux[3,num_col_Lugares:num_col_Cambios],
                            "V",
                            mat_aux[3,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_3[num_col_Cambios] <- "1/"
             names(renglon_3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_3)
             
             renglon_4 <- c("Semántica y Verificación",
                            ##Empieza a las 13hrs y termina a las 14:30hrs
                            mat_aux[4,num_col_Profesor:num_col_Alumnos],
                            "Salón 301 del IIMAS","7016",
                            mat_aux[4,num_col_Carrera:num_col_URL_MAp2017])
             renglon_4[num_col_Cambios] <- "1/"
             names(renglon_4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_4)
             
             
             #' Las materias "Criptografía y Seguridad" y "Organización y
             #' Arquitectura de Computadoras" son diferentes, los horarios coinciden
             #' por las horas de los ayudantes.
             renglon_5 <- c("Criptografía y Seguridad",
                            mat_aux[5,num_col_Profesor:num_col_horario_num],
                            30,
                            mat_aux[5,num_col_Alumnos:num_col_URL_MAp2017])
             renglon_5[num_col_Cambios] <- "1/"
             names(renglon_5) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_5)
             
             renglon_6 <- c("Organización y Arquitectura de Computadoras",
                            mat_aux[6,num_col_Profesor:num_col_Horario],
                            12,##12-13 //Un ay. imparte los jueves de 10-12
                            mat_aux[6,num_col_Lugares:num_col_URL_MAp2017])
             renglon_6[num_col_Cambios] <- "1/"
             names(renglon_6) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_6)
             
             
             #' Las materias "Seminario de Aplicaciones de Cómputo II" y "Visión
             #' Por Computadora" es la misma pero con diferente nombre de acuerdo
             #' al plan de estudios.
             renglon_7 <- c(mat_aux[8,num_col_Materia:num_col_Lugares],
                            9,#Se suman los alumnos de ambos nombres
                            mat_aux[8,num_col_Salon],
                            "7084/7087",
                            "Ciencias de la Computación/Ciencias de la Computación",
                            "/1994/2013",
                            mat_aux[8,num_col_Semestre:num_col_Turno],
                            "/Optativas/Optativas",
                            "/http://www.fciencias.unam.mx/docencia/horarios/20142/218/424/http://www.fciencias.unam.mx/docencia/horarios/20142/1556/794",
                            as.numeric(mat_aux[7,num_col_Act2000:num_col_MAp2017])+as.numeric(mat_aux[8,num_col_Act2000:num_col_MAp2017]),
                            0,0,0,"Seminario de Aplicaciones de Cómputo II",
                            "Visión Por Computadora",0,0,0,0,0,
                            "http://www.fciencias.unam.mx/docencia/horarios/20142/218/424",
                            "http://www.fciencias.unam.mx/docencia/horarios/20142/1556/794",
                            0,0)
             renglon_7[num_col_Cambios] <- "1/"
             names(renglon_7) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_7)
             
             
             #' Las materias "Recuperación y Búsqueda de Información en Textos" y
             #' "Proceso Digital de Imagenes" son diferentes, los horarios coinciden
             #' por las horas de los ayudantes.
             renglon_8 <- c("Recuperación y Búsqueda de Información en Textos",
                            mat_aux[9,num_col_Profesor:num_col_Horario],
                            9,##9-10hrs
                            mat_aux[9,num_col_Lugares:num_col_URL_MAp2017])
             renglon_8[num_col_Cambios] <- "1/"
             names(renglon_8) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_8)
             
             renglon_9 <- c("Proceso Digital de Imagenes",
                            mat_aux[10,num_col_Profesor:num_col_Horario],
                            8,##8-9hrs
                            mat_aux[10,num_col_Lugares:num_col_URL_MAp2017])
             renglon_9[num_col_Cambios] <- "1/"
             names(renglon_9) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_9)
             
             
             #' Las materias "Tecnologías para Desarrollos en Internet" y
             #' "Diseño de Interfaces de Usuario" son diferentes, los horarios coinciden
             #' por las horas de los ayudantes.
             renglon_10 <- c("Tecnologías para Desarrollos en Internet",
                             mat_aux[11,num_col_Profesor:num_col_Horario],
                             17,##Empieza a las 13hrs y termina a las 14:30hrs
                             30,
                             mat_aux[11,num_col_Alumnos:num_col_Cambios],
                             "V",
                             mat_aux[11,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_10[num_col_Cambios] <- "1/"
             names(renglon_10) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_10)
             
             renglon_11 <- c("Diseño de Interfaces de Usuario",
                             mat_aux[12,num_col_Profesor:num_col_Horario],
                             9,##9-10hrs
                             mat_aux[12,num_col_Lugares:num_col_URL_MAp2017])
             renglon_11[num_col_Cambios] <- "1/"
             names(renglon_11) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_11)
           },##Fin 20142
           '20151' = {
             #' Las materias "Computación Distribuida" e "Introducción a Ciencias
             #' de la Computación", son diferentes, los horarios coinciden por
             #' las clases de los ayudantes.
             renglon_1 <- c("Computación Distribuida",
                            mat_aux[1,num_col_Profesor:num_col_horario_num],
                            50,
                            mat_aux[1,num_col_Alumnos:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/"
             names(renglon_1) <- param$nom_cols_MG  
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c("Lingüística Computacional",
                            mat_aux[2,num_col_Profesor:num_col_Horario],
                            16,##Empieza a las 16hrs y termina a las 17:30hrs
                            mat_aux[2,num_col_Lugares:num_col_Cambios],
                            "V",
                            mat_aux[2,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/"
             names(renglon_2) <- param$nom_cols_MG  
             m_grande <- rbind(m_grande,renglon_2)
           },##Fin 20151
           '20152' = {
             #' Las materias "Programación" y "Diseño de Interfaces de Usuario" son
             #' diferentes, los horarios coinciden por las horas de los ayudantes.
             renglon_1 <- c("Programación",
                            mat_aux[1,num_col_Profesor:num_col_Salon],
                            "9796",
                            mat_aux[1,num_col_Carrera:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/"
             names(renglon_1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c("Diseño de Interfaces de Usuario",
                            mat_aux[2,num_col_Profesor:num_col_Horario],
                            17,##Empieza a las 17hrs y termina a las 18:30hrs
                            mat_aux[2,num_col_Lugares:num_col_Cambios],
                            "V",
                            mat_aux[2,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/"
             names(renglon_2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_2)
             
             
             #' Las materias "Probabilidad y Estadística" y "Probabilidad II" son
             #' diferentes, los horarios coinciden por las horas de los ayudantes.
             renglon_3 <- c(mat_aux[3,num_col_Materia:num_col_Horario],
                            17,##Empieza a las 17hrs y termina a las 18:30hrs
                            mat_aux[3,num_col_Lugares:num_col_URL_MAp2017])
             renglon_3[num_col_Cambios] <- "1/"
             names(renglon_3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_3)
             
             renglon_4 <- c("Probabilidad II",
                            mat_aux[4,num_col_Profesor:num_col_Salon],
                            "9607",
                            mat_aux[4,num_col_Carrera:num_col_URL_MAp2017])
             renglon_4[num_col_Cambios] <- "1/"
             names(renglon_4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_4)
           },##Fin 20152
           '20161' = {
             ind_aux <- 0
             for(r in 1:dim(m_grande)[1]){
               if(m_grande[r,num_col_Profesor]=="Pilar Selene Linares Arévalo" && 
                  m_grande[r,num_col_horario_num] == "13"){
                 ind_aux <- c(ind_aux,r)}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             ind_aux <- 0
             for(r in 1:dim(m_grande)[1]){
               if(m_grande[r,num_col_Profesor]=="Jonathan Banfi Vázquez" && 
                  m_grande[r,num_col_horario_num] == "14"){
                 ind_aux <- c(ind_aux,r)}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             
             #' Las materias "Ecuaciones Diferenciales I" y "Variable Compleja I" son
             #' diferentes, los horarios coinciden por las horas de los ayudantes.
             renglon_1 <- c("Ecuaciones Diferenciales I",
                            mat_aux[1,num_col_Profesor:num_col_Salon],
                            "4331",
                            mat_aux[1,num_col_Carrera:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/"
             names(renglon_1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c("Variable Compleja I",
                            mat_aux[2,num_col_Profesor:num_col_Horario],
                            14,##Empieza a las 14hrs y termina a las 15hrs
                            mat_aux[2,num_col_Lugares:num_col_Salon],
                            "4333",
                            mat_aux[2,num_col_Carrera:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/"
             names(renglon_2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_2)
             
             
             #' Las materias "Estructuras Discretas" y "Lógica Computacional II" son
             #' diferentes, los horarios coinciden por las horas de los ayudantes.
             renglon_3 <- c("Estructuras Discretas",
                            mat_aux[3,num_col_Profesor:num_col_Horario],
                            16,##Empieza a las 16hrs y termina a las 17:30hrs
                            mat_aux[3,num_col_Lugares:num_col_Cambios],
                            "V",
                            mat_aux[3,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_3[num_col_Cambios] <- "1/"
             names(renglon_3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_3)
             
             renglon_4 <- c("Lógica Computacional II",
                            mat_aux[4,num_col_Profesor:num_col_URL_MAp2017])
             renglon_4[num_col_Cambios] <- "1/"
             names(renglon_4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_4)
             
             
             #' Las materias "Seminario de Ciencias de la Computación A" y
             #' "Criptografía y Seguridad" son diferentes, los horarios coinciden
             #' por las horas de los ayudantes.
             renglon_5 <- c("Seminario de Ciencias de la Computación A",
                            mat_aux[5,num_col_Profesor:num_col_Horario],
                            19,##Empieza a las 19hrs y termina a las 20hrs
                            mat_aux[5,num_col_Lugares:num_col_Cambios],
                            "V",
                            mat_aux[5,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_5[num_col_Cambios] <- "1/"
             names(renglon_5) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_5)
             
             renglon_6 <- c("Criptografía y Seguridad",
                            mat_aux[6,num_col_Profesor:num_col_Horario],
                            8,##8-9 //Un ay. imparte los viernes de 14-16
                            mat_aux[6,num_col_Lugares:num_col_URL_MAp2017])
             renglon_6[num_col_Cambios] <- "1/"
             names(renglon_6) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_6)
           },##Fin 20161
           '20162' = {
             #' Las materias "Estructuras de Datos" y "Redes Neuronales"
             #' son diferentes, los horarios coinciden por las horas de
             #' los ayudantes.
             renglon_1 <- c("Estructuras de Datos",
                            mat_aux[1,num_col_Profesor:num_col_Horario],
                            16,##Empieza a las 16hrs y termina a las 17:30hrs
                            mat_aux[1,num_col_Lugares:num_col_Cambios],
                            "V",
                            mat_aux[1,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/"
             names(renglon_1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c("Redes Neuronales",
                            mat_aux[2,num_col_Profesor:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/"
             names(renglon_2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_2)
             
             
             #' Las materias "Inteligencia Artificial" y "Modelado y Programación" son
             #' diferentes, los horarios coinciden por las horas de los ayudantes.
             renglon_3 <- c("Inteligencia Artificial",
                            mat_aux[3,num_col_Profesor:num_col_Horario],
                            9,##Empieza a las 9hrs y termina a las 10hrs
                            mat_aux[3,num_col_Lugares:num_col_Cambios],
                            "M",
                            mat_aux[3,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_3[num_col_Cambios] <- "1/"
             names(renglon_3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_3)
             
             renglon_4 <- c("Modelado y Programación",
                            mat_aux[4,num_col_Profesor:num_col_Alumnos],
                            "Aula del Futuro, CECADET","7032",
                            mat_aux[4,num_col_Carrera:num_col_URL_MAp2017])
             renglon_4[num_col_Cambios] <- "1/"
             names(renglon_4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_4)
             
             
             #' Las materias "Inglés III" e "	Inglés II" son diferentes,
             #' las clases presenciales se imparten en días distintos.
             renglon_5 <- c(mat_aux[5,num_col_Materia:num_col_Profesor],
                            "13 a 15 Vi",
                            mat_aux[5,num_col_horario_num:num_col_URL_MAp2017])
             renglon_5[num_col_Cambios] <- "1/2/"
             names(renglon_5) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_5)
             
             renglon_6 <- c(mat_aux[6,num_col_Materia:num_col_Profesor],
                            "13 a 14 Lu Ma Mi Ju",
                            mat_aux[6,num_col_horario_num:num_col_URL_MAp2017])
             renglon_6[num_col_Cambios] <- "1/2/"
             names(renglon_6) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_6)
           },##Fin 20162
           '20171' = {
             ind_aux <- 0
             for(r in 1:dim(m_grande)[1]){
               if(m_grande[r,num_col_Profesor]=="Arturo García Miranda" && 
                  m_grande[r,num_col_horario_num] == "14"){
                 ind_aux <- c(ind_aux,r)}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             ind_aux <- 0
             for(r in 1:dim(m_grande)[1]){
               for(h in 13:14){
                 if(m_grande[r,num_col_Profesor]=="Lidia Fabiola Quevedo Rojas" && 
                    m_grande[r,num_col_horario_num] == h){
                   ind_aux <- c(ind_aux,r)}}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             
             #' Las materias "Seminario de Apoyo a la Titulación en
             #' Matemáticas A" y "Álgebra Lineal I" son diferentes, los
             #' horarios coinciden porque la profesora imparte el seminario
             #' a la misma hora los lunes y jueves, se le da prioridad a la
             #' materia de "Álgebra Lineal I" (en cuanto al horario) debido
             #' a que es una materia obligatoria.
             renglon_1 <- c(mat_aux[1,num_col_Materia:num_col_Profesor],
                            "16 a 18 Lu/ 17 a 18 Ju",
                            17,##Lunes: 16-18, Jueves: 17-18
                            mat_aux[1,num_col_Lugares:num_col_Alumnos],
                            "Cubículo 224, Departamento de Matemáticas",
                            mat_aux[1,num_col_Grupo:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/2/"
             names(renglon_1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- mat_aux[2,num_col_Materia:num_col_URL_MAp2017]
             renglon_2[num_col_Cambios] <- "1/"
             names(renglon_2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_2)
             
             
             #' Las materias "Inglés III" e "	Inglés II" son diferentes,
             #' las clases presenciales se imparten en días distintos.
             renglon_3 <- c(mat_aux[3,num_col_Materia:num_col_Profesor],
                            "14 a 16 Vi/ 15 a 16 Mi Ju Sesión AVE",
                            mat_aux[3,num_col_horario_num:num_col_Alumnos],
                            "002 (Yelizcalli)",
                            mat_aux[3,num_col_Grupo:num_col_URL_MAp2017])
             renglon_3[num_col_Cambios] <- "1/2/"
             names(renglon_3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_3)
             
             renglon_4 <- c(mat_aux[4,num_col_Materia:num_col_Profesor],
                            "14 a 15 Lu Ma Mi Ju",
                            mat_aux[4,num_col_horario_num:num_col_URL_MAp2017])
             renglon_4[num_col_Cambios] <- "1/2/"
             names(renglon_4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_4)
             
             
             #' Se encontró que el profesor "Arturo García Miranda" imparte varias
             #' clases de inglés en diferentes días que pueden o no ser del mismo
             #' nivel, pero son grupos diferentes.
             #' Se quitaron todos los renglones de "m_grande" de dicho profesor
             #' para hacer los cambios correspondientes.
             #' Nota: Se toma en cuenta el horario de las clases presenciales
             #' no de las clases en línea.
             # m_grande[m_grande[,num_col_Profesor]=="Arturo García Miranda",]
             ind_aux <- 0
             for(r in 1:dim(m_grande)[1]){
               if(m_grande[r,num_col_Profesor]=="Arturo García Miranda"){
                 ind_aux <- c(ind_aux,r)}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             mat_aux_2 <- mat_aux[mat_aux[,num_col_Profesor]=="Arturo García Miranda",]
             
             ## INGLÉS I ##
             renglon_A1 <- c(mat_aux_2[2,num_col_Materia:num_col_Profesor],
                             "13 a 14 Lu Ma Mi Ju",
                             mat_aux_2[2,num_col_horario_num:num_col_URL_MAp2017])
             renglon_A1[num_col_Cambios] <- "1/2/"
             names(renglon_A1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_A1)
             
             renglon_A2 <- c(mat_aux_2[3,num_col_Materia:num_col_Profesor],
                             "14 a 16 Ma",
                             mat_aux_2[3,num_col_horario_num:num_col_URL_MAp2017])
             renglon_A2[num_col_Cambios] <- "1/2/"
             names(renglon_A2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_A2)
             
             ## INGLÉS III ##
             renglon_A3 <- c(mat_aux_2[1,num_col_Materia:num_col_Profesor],
                             "13 a 15 Sa Sesión AVE/ 14 a 16 Vi",
                             14,##14-16hrs Viernes
                             mat_aux_2[1,num_col_Lugares:num_col_Alumnos],
                             "003 (Yelizcalli)",
                             mat_aux_2[1,num_col_Grupo:num_col_URL_MAp2017])
             renglon_A3[num_col_Cambios] <- "1/2/"
             names(renglon_A3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_A3)
             
             renglon_A4 <- c(mat_aux_2[4,num_col_Materia:num_col_Profesor],
                             "14 a 16 Lu / 18 a 20 Ma Sesión AVE",
                             mat_aux_2[4,num_col_horario_num:num_col_Alumnos],
                             "003 (Yelizcalli)",
                             mat_aux_2[4,num_col_Grupo:num_col_URL_MAp2017])
             renglon_A4[num_col_Cambios] <- "1/2/"
             names(renglon_A4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_A4)
             
             renglon_A5 <- c(mat_aux_2[5,num_col_Materia:num_col_Profesor],
                             "11 a 13 Sa Sesión AVE/ 14 a 16 Ju",
                             14,##14-16hrs Jueves
                             mat_aux_2[5,num_col_Lugares:num_col_Alumnos],
                             "003 (Yelizcalli)",
                             mat_aux_2[5,num_col_Grupo:num_col_URL_MAp2017])
             renglon_A5[num_col_Cambios] <- "1/2/"
             names(renglon_A5) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_A5)
             
             ## INGLÉS IV ##
             renglon_A6 <- c(mat_aux_2[6,num_col_Materia:num_col_Profesor],
                             "18 a 20 Mi Sesión AVE/ 9 a 11 Sa",
                             9,##9-11hrs Sábado
                             mat_aux_2[6,num_col_Lugares:num_col_Alumnos],
                             "204 (Yelizcalli)",
                             mat_aux_2[6,num_col_Grupo:num_col_Cambios],
                             "M",
                             mat_aux_2[6,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_A6[num_col_Cambios] <- "1/2/"
             names(renglon_A6) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_A6)
             
             
             #' Se encontró que la profesora "Lidia Fabiola Quevedo Rojas" imparte varias
             #' clases de inglés en diferentes días que pueden o no ser del mismo
             #' nivel, pero son grupos diferentes.
             #' Se quitaron todos los renglones de "m_grande" de dicho profesor
             #' para hacer los cambios correspondientes.
             #' Nota: Se toma en cuenta el horario de las clases presenciales
             #' no de las clases en línea.
             # m_grande[m_grande[,num_col_Profesor]=="Lidia Fabiola Quevedo Rojas",]
             ind_aux <- 0
             for(r in 1:dim(m_grande)[1]){
               if(m_grande[r,num_col_Profesor]=="Lidia Fabiola Quevedo Rojas"){
                 ind_aux <- c(ind_aux,r)}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             mat_aux_3 <- mat_aux[mat_aux[,num_col_Profesor]=="Lidia Fabiola Quevedo Rojas",]
             
             ## INGLÉS I ##
             renglon_L1 <- c(mat_aux_3[1,num_col_Materia:num_col_Profesor],
                             "14 a 16 Ju",
                             mat_aux_3[1,num_col_horario_num:num_col_URL_MAp2017])
             renglon_L1[num_col_Cambios] <- "1/2/"
             names(renglon_L1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_L1)
             
             
             ## INGLÉS IV ##
             renglon_L2 <- c(mat_aux_3[3,num_col_Materia:num_col_Profesor],
                             "13 a 14 Lu Ma Mi Ju",
                             mat_aux_3[3,num_col_horario_num:num_col_URL_MAp2017])
             renglon_L2[num_col_Cambios] <- "1/2/"
             names(renglon_L2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_L2)
             
             
             renglon_L3 <- c(mat_aux_3[5,num_col_Materia:num_col_Profesor],
                             "11 a 13 Sa Sesión AVE/ 14 a 16 Vi",
                             14,##14-16hrs Viernes
                             mat_aux_3[5,num_col_Lugares:num_col_Alumnos],
                             "O216",
                             mat_aux_3[5,num_col_Grupo:num_col_URL_MAp2017])
             renglon_L3[num_col_Cambios] <- "1/2/"
             names(renglon_L3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_L3)
             
             
             ## INGLÉS V ##
             renglon_L4 <- c(mat_aux_3[2,num_col_Materia:num_col_Profesor],
                             "13 a 15 Mi Sesión AVE/ 14 a 16 Lu",
                             14,##14-16hrs Lunes
                             mat_aux_3[2,num_col_Lugares:num_col_Alumnos],
                             "O216",
                             mat_aux_3[2,num_col_Grupo:num_col_URL_MAp2017])
             renglon_L4[num_col_Cambios] <- "1/2/"
             names(renglon_L4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_L4)
             
             
             renglon_L5 <- c(mat_aux_3[2,num_col_Materia:num_col_Profesor],
                             #Se toman los nombres de Materia y de Profesor del
                             #renglón 2
                             "14 a 16 Ma / 16 a 18 Mi Sesión AVE",
                             mat_aux_3[4,num_col_horario_num:num_col_Lugares],
                             32,"003 (Yelizcalli)","9386","Actuaría","2015",
                             mat_aux_3[4,num_col_Semestre:num_col_Turno],
                             "Sexto Semestre",
                             "http://www.fciencias.unam.mx/docencia/horarios/20171/2017/1640",
                             0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
             renglon_L5[num_col_Cambios] <- "1/2/"
             names(renglon_L5) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_L5)
             
             renglon_L6 <- c(mat_aux_3[2,num_col_Materia:num_col_Profesor],
                             #Se toman los nombres de Materia y de Profesor del
                             #renglón 2
                             "14 a 16 Mi / 16 a 18 Ju Sesión AVE",
                             mat_aux_3[4,num_col_horario_num:num_col_Lugares],
                             40,"003 (Yelizcalli)","9387","Actuaría","2015",
                             mat_aux_3[4,num_col_Semestre:num_col_Turno],
                             "Sexto Semestre",
                             "http://www.fciencias.unam.mx/docencia/horarios/20171/2017/1640",
                             0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
             renglon_L6[num_col_Cambios] <- "1/2/"
             names(renglon_L6) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_L6)
           },##Fin 20171
           '20172' = {
             ind_aux <- 0
             for(r in 1:dim(m_grande)[1]){
               if(m_grande[r,num_col_Profesor]=="Olga Nelly Sánchez Cárdenas" && 
                  m_grande[r,num_col_horario_num] == "14"){
                 ind_aux <- c(ind_aux,r)}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             
             #' Las materias "Inglés II", "Inglés II" e "Inglés III"
             #' son diferentes, las clases presenciales son únicamente un día
             #' a la semana y también hay sesiones virtuales, se tienen 4
             #' clases diferentes.
             renglon_1 <- c(mat_aux[1,num_col_Materia:num_col_Profesor],
                            "13 a 15 Vi / 16 a 18 Vi Sesión virtual",
                            mat_aux[1,num_col_horario_num:num_col_Alumnos],
                            "102 (Yelizcalli)",
                            mat_aux[1,num_col_Grupo:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/2/"
             names(renglon_1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c(mat_aux[2,num_col_Materia:num_col_Profesor],
                            "13 a 15 Sa Sesión virtual / 9 a 11 Sa",
                            9,##9-11hrs Sábado
                            mat_aux[2,num_col_Lugares:num_col_Alumnos],
                            "004 (Yelizcalli)",
                            mat_aux[2,num_col_Grupo:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/2/"
             names(renglon_2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_2)
             
             
             renglon_3 <- c(mat_aux[3,num_col_Materia:num_col_Profesor],
                            "13 a 14 Lu Ma Mi Ju",
                            mat_aux[3,num_col_horario_num:num_col_URL_MAp2017])
             renglon_3[num_col_Cambios] <- "1/2/"
             names(renglon_3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_3)
             
             
             #' Las materias "Inglés VI" e "/Inglés II/Inglés II/Inglés IV/Inglés V"
             #' son diferentes, las clases presenciales son únicamente un día
             #' a la semana y también hay sesiones virtuales, se tienen 5
             #' clases diferentes.
             renglon_4 <- c(mat_aux[4,num_col_Materia:num_col_Profesor],
                            "11 a 13 Vi Sesión virtual / 14 a 16 Vi",
                            14,##14-16 Viernes
                            mat_aux[4,num_col_Lugares:num_col_Alumnos],
                            "002 (Yelizcalli)",
                            mat_aux[4,num_col_Grupo:num_col_URL_MAp2017])
             renglon_4[num_col_Cambios] <- "1/2/"
             names(renglon_4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_4)
             
             renglon_5 <- c("Inglés II",
                            mat_aux[5,num_col_Profesor],
                            "11 a 13 Ju Sesión virtual / 13 a 15 Ju",
                            13,##13-15 Jueves
                            50,46,"102 (Yelizcalli)","9343",
                            "Actuaría","2015",
                            mat_aux[5,num_col_Semestre:num_col_Turno],
                            "Segundo Semestre",
                            "http://www.fciencias.unam.mx/docencia/horarios/20172/2017/1235",
                            mat_aux[5,num_col_Act2000:num_col_NomMat_Act2006],0,
                            mat_aux[5,num_col_NomMat_CdC1994:num_col_URL_Act2006],0,
                            mat_aux[5,num_col_URL_CdC1994:num_col_URL_MAp2017])
             renglon_5[num_col_Cambios] <- "1/2/"
             names(renglon_5) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_5)
             
             renglon_6 <- c("Inglés II",
                            mat_aux[5,num_col_Profesor],
                            "11 a 13 Lu Sesión virtual / 13 a 15 Lu",
                            13,##13-15 Lunes
                            50,50,"004 (Yelizcalli)","9344",
                            "Actuaría","2015",
                            mat_aux[5,num_col_Semestre:num_col_Turno],
                            "Segundo Semestre",
                            "http://www.fciencias.unam.mx/docencia/horarios/20172/2017/1235",
                            mat_aux[5,num_col_Act2000:num_col_NomMat_Act2006],0,
                            mat_aux[5,num_col_NomMat_CdC1994:num_col_URL_Act2006],0,
                            mat_aux[5,num_col_URL_CdC1994:num_col_URL_MAp2017])
             names(renglon_6) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_6)
             
             renglon_7 <- c("Inglés IV",
                            mat_aux[5,num_col_Profesor],
                            "11 a 13 Ma Sesión virtual / 13 a 15 Ma",
                            13,##13-15 Martes
                            38,37,"004 (Yelizcalli)","9350","Actuaría","2015",
                            mat_aux[5,num_col_Semestre:num_col_Turno],
                            "Cuarto Semestre",
                            "http://www.fciencias.unam.mx/docencia/horarios/20172/2017/1436",
                            mat_aux[5,num_col_Act2000:num_col_NomMat_Act2006],0,
                            mat_aux[5,num_col_NomMat_CdC1994:num_col_URL_Act2006],0,
                            mat_aux[5,num_col_URL_CdC1994:num_col_URL_MAp2017])
             renglon_7[num_col_Cambios] <- "1/2/"
             names(renglon_7) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_7)
             
             renglon_8 <- c("Inglés V",
                            mat_aux[5,num_col_Profesor],
                            "11 a 13 Mi Sesión virtual / 13 a 15 Mi",
                            13,##13-15 Miércoles
                            44,44,"003 (Yelizcalli)","9353","Actuaría","2015",
                            mat_aux[5,num_col_Semestre:num_col_Turno],
                            "Sexto Semestre",
                            "http://www.fciencias.unam.mx/docencia/horarios/20172/2017/1640",
                            mat_aux[5,num_col_Act2000:num_col_NomMat_Act2006],0,
                            mat_aux[5,num_col_NomMat_CdC1994:num_col_URL_Act2006],0,
                            mat_aux[5,num_col_URL_CdC1994:num_col_URL_MAp2017])
             renglon_8[num_col_Cambios] <- "1/2/"
             names(renglon_8) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_8)
             
             
             #' Las materias "Seminario de Ciencias de la Computación A" y "Robótica"
             #' son diferentes, los horarios coinciden por las horas de los ayudantes.
             renglon_9 <- c(mat_aux[6,num_col_Materia:num_col_Horario],
                            17,##Empieza a las 17hrs y termina a las 18:30hrs
                            mat_aux[6,num_col_Lugares:num_col_Cambios],
                            "V",
                            mat_aux[6,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_9[num_col_Cambios] <- "1/"
             names(renglon_9) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_9)
             
             renglon_10 <- c("Robótica",
                             mat_aux[7,num_col_Profesor:num_col_Horario],
                             18,##Empieza a las 18:30hrs y termina a las 20hrs
                             mat_aux[7,num_col_Lugares:num_col_Salon],
                             "7014",
                             "Ciencias de la Computación/Ciencias de la Computación",
                             # mat_aux[6,num_col_Plan:num_col_Cambios],
                             mat_aux[7,num_col_Plan:num_col_Cambios],
                             "V",
                             mat_aux[7,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_10[num_col_Cambios] <- "1/"
             names(renglon_10) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_10)
             
             
             #' Las materias "Inglés III" e "/Inglés III/Inglés V	" son diferentes,
             #' las clases presenciales son únicamente un día a la semana y
             #' también hay sesiones virtuales, se tienen 3 clases diferentes.
             renglon_11 <- c(mat_aux[8,num_col_Materia:num_col_Profesor],
                             "14 a 16 Ma Ju",
                             mat_aux[8,num_col_horario_num:num_col_URL_MAp2017])
             names(renglon_11) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_11)
             
             renglon_12 <- c("Inglés III",
                             mat_aux[9,num_col_Profesor],
                             "14 a 16 Mi / 16 a 18 Mi Sesión virtual",
                             mat_aux[9,num_col_horario_num],
                             32,31,mat_aux[9,num_col_Salon],
                             "9348","Actuaría","2015",
                             mat_aux[9,num_col_Semestre:num_col_Turno],
                             "Tercer Semestre",
                             "http://www.fciencias.unam.mx/docencia/horarios/20172/2017/1336",
                             mat_aux[9,num_col_Act2000:num_col_NomMat_Act2006],0,
                             mat_aux[9,num_col_NomMat_CdC1994:num_col_URL_Act2006],0,
                             mat_aux[9,num_col_URL_CdC1994:num_col_URL_MAp2017])
             renglon_12[num_col_Cambios] <- "1/2/"
             names(renglon_12) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_12)
             
             renglon_13 <- c("Inglés V",
                             mat_aux[9,num_col_Profesor],
                             "14 a 16 Lu / 16 a 18 Lu Sesión virtual",
                             mat_aux[9,num_col_horario_num],
                             40,38,"003 (Yelizcalli)","9354","Actuaría","2015",
                             mat_aux[9,num_col_Semestre:num_col_Turno],
                             "Sexto Semestre",
                             "http://www.fciencias.unam.mx/docencia/horarios/20172/2017/1640",
                             mat_aux[9,num_col_Act2000:num_col_NomMat_Act2006],0,
                             mat_aux[9,num_col_NomMat_CdC1994:num_col_URL_Act2006],0,
                             mat_aux[9,num_col_URL_CdC1994:num_col_URL_MAp2017])
             renglon_13[num_col_Cambios] <- "1/2/"
             names(renglon_13) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_13)
           },##Fin 20172
           '20191' = {
             ind_aux <- 0
             for(r in 1:dim(m_grande)[1]){
               if(m_grande[r,num_col_Profesor]=="Canek Peláez Valdés" && 
                  m_grande[r,num_col_horario_num] == "10"){
                 ind_aux <- c(ind_aux,r)}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             ind_aux <- 0
             for(r in 1:dim(m_grande)[1]){
               if(m_grande[r,num_col_Profesor]=="Maria Teresa Saavedra Sordo" && 
                  m_grande[r,num_col_horario_num] == "14"){
                 ind_aux <- c(ind_aux,r)}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             #' Las materias "Seminario de Apoyo a la Titulación en Actuaría B" y
             #' "Taller de Modelación I" son diferentes, los horarios
             #' coinciden pero los días que son impartidas difieren.
             renglon_1 <- c(mat_aux[1,num_col_Materia:num_col_Profesor],
                            "13 a 14:30	Lu Vi",
                            mat_aux[1,num_col_horario_num:num_col_Alumnos],
                            "Cubículo 028, Departamento de Matemáticas",
                            mat_aux[1,num_col_Grupo:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/2/"
             names(renglon_1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c(mat_aux[2,num_col_Materia:num_col_Profesor],
                            "13 a 15 Ma Ju",
                            mat_aux[2,num_col_horario_num:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/2/"
             names(renglon_2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_2)
             
             
             #' Las materias "Redes de Computadoras" e "Introducción a Ciencias de
             #' la Computación I" son diferentes, los horarios coinciden por las
             #' horas de los ayudantes.
             renglon_3 <- c("Redes de Computadoras",
                            mat_aux[3,num_col_Profesor:num_col_URL_MAp2017])
             renglon_3[num_col_Cambios] <- "1/"
             names(renglon_3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_3)
             
             renglon_4 <- c("Introducción a Ciencias de la Computación I",
                            mat_aux[4,num_col_Profesor:num_col_Horario],
                            13,##Empieza a las 13hrs y termina a las 14:30hrs
                            mat_aux[4,num_col_Lugares:num_col_URL_MAp2017])
             renglon_4[num_col_Cambios] <- "1/"
             names(renglon_4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_4)
             
             
             #' La materia "Fundamentos de Bases de Datos" tiene 2 horarios distintos,
             #' hay una coincidencia por las horas de los ayudantes.
             renglon_5 <- c("Fundamentos de Bases de Datos",
                            mat_aux[5,num_col_Profesor:num_col_Horario],
                            8,##8-9 en el salón 103 del Tlahuizcalpan
                            mat_aux[5,num_col_Lugares:num_col_URL_MAp2017])
             renglon_5[num_col_Cambios] <- "1/"
             names(renglon_5) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_5)
             
             renglon_6 <- c("Fundamentos de Bases de Datos",
                            mat_aux[6,num_col_Profesor:num_col_Horario],
                            9,##9-10
                            mat_aux[6,num_col_Lugares:num_col_URL_MAp2017])
             renglon_6[num_col_Cambios] <- "1/"
             names(renglon_6) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_6)
             
             
             #' Las materias "Mecánica Vectorial" y "Relatividad" son
             #' diferentes, los horarios coinciden pero los días en
             #' los que se imparten difieren.
             renglon_7 <- c(mat_aux[7,num_col_Materia:num_col_Profesor],
                            "8 a 10	Lu Mi Vi",
                            mat_aux[7,num_col_horario_num:num_col_URL_MAp2017])
             renglon_7[num_col_Cambios] <- "1/2/"
             names(renglon_7) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_7)
             
             renglon_8 <- c(mat_aux[8,num_col_Materia:num_col_Profesor],
                            "8 a 9:30	Ma Ju",
                            mat_aux[8,num_col_horario_num:num_col_URL_MAp2017])
             renglon_8[num_col_Cambios] <- "1/2/"
             names(renglon_8) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_8)
             
             
             #' Las materias "Seminario de Ciencias de la Computación B" e
             #' "Introducción a Ciencias de la Computación I" son diferentes,
             #' los horarios coinciden por las horas de los ayudantes.
             renglon_9 <- mat_aux[9,num_col_Materia:num_col_URL_MAp2017]
             renglon_9[num_col_Cambios] <- "1/"
             names(renglon_9) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_9)
             
             renglon_10 <- c("Introducción a Ciencias de la Computación I",
                             mat_aux[10,num_col_Profesor:num_col_Horario],
                             13,##Empieza a las 13hrs y termina a las 14:30hrs
                             mat_aux[10,num_col_Lugares:num_col_URL_MAp2017])
             renglon_10[num_col_Cambios] <- "1/"
             names(renglon_10) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_10)
             
             
             #' La materia "Inglés I" es impartida en días distintos de la
             #' semana por la profesora "Maria Teresa Saavedra Sordo" en el
             #' mismo horario.
             renglon_11 <- c(mat_aux[11,num_col_Materia:num_col_Profesor],
                             "14 a 16 Ma / 9 a 11 Sa Sesión virtual",
                             mat_aux[11,num_col_horario_num:num_col_Alumnos],
                             "O215",
                             mat_aux[11,num_col_Grupo:num_col_URL_MAp2017])
             renglon_11[num_col_Cambios] <- "1/2/"
             names(renglon_11) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_11)
             
             renglon_12 <- c(mat_aux[12,num_col_Materia:num_col_Profesor],
                             "14 a 16 Ju / 7 a 9 Vi Sesión virtual",
                             mat_aux[12,num_col_horario_num:num_col_URL_MAp2017])
             renglon_12[num_col_Cambios] <- "1/2/"
             names(renglon_12) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_12)
           },##Fin 20191
           '20192' = {
             ind_aux <- 0
             prof_aux_1 <- c("Lidia Fabiola Quevedo Rojas",
                             "María Azucena Rivera Vidal",
                             "Maria Teresa Saavedra Sordo")
             for(r in 1:dim(m_grande)[1]){
               for(p in 1:length(prof_aux_1)){
                 if(m_grande[r,num_col_Profesor]==prof_aux_1[p] && 
                    m_grande[r,num_col_horario_num] == "13"){
                   ind_aux <- c(ind_aux,r)}}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             
             #' Las materias de inglés sólo se imparten un día a la semana, los
             #' días en que se imparten las clases se van a colocar en la
             #' columna "Horario" para que sólo haya números en la columna
             #' "horario_num"
             
             #' Las materias "Inglés III" e "Inglés IV" son diferentes,
             #' las clases presenciales son únicamente un día a la semana
             #' y también hay sesiones virtuales.
             renglon_1 <- c(mat_aux[1,num_col_Materia:num_col_Profesor],
                            "13 a 14 Lu Mi/ 9 a 11	Sa Sesión virtual",
                            mat_aux[1,num_col_horario_num:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/2/"
             names(renglon_1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c(mat_aux[2,num_col_Materia:num_col_Profesor],
                            "13 a 15 Sa Sesión virtual/ 14 a 16 Vi",
                            14,##14-16 Viernes
                            mat_aux[2,num_col_Lugares:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/2/"
             names(renglon_2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_2)
             
             #' Las materias "Inglés II", "Inglés III" e "Inglés IV" son diferentes,
             #' las clases presenciales son únicamente un día a la semana
             #' y también hay sesiones virtuales. Se tienen 4 grupos distintos.
             mat_aux_2 <- mat_aux[mat_aux[,num_col_Profesor]=="Lidia Fabiola Quevedo Rojas",]
             
             ## INGLÉS II ##
             renglon_A1 <- c(mat_aux_2[1,num_col_Materia:num_col_Profesor],
                             "14 a 15 Ma Ju / 7 a 9	Sa Sesión virtual",
                             mat_aux_2[1,num_col_horario_num:num_col_URL_MAp2017])
             renglon_A1[num_col_Cambios] <- "1/2/"
             names(renglon_A1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_A1)
             
             ## INGLÉS III ##
             renglon_A2 <- c(mat_aux_2[3,num_col_Materia:num_col_Profesor],
                             "13 a 14 Lu Mi / 9 a 11 Sa Sesión virtual",
                             mat_aux_2[3,num_col_horario_num:num_col_URL_MAp2017])
             renglon_A2[num_col_Cambios] <- "1/2/"
             names(renglon_A2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_A2)
             
             ## INGLÉS IV ##
             renglon_A3 <- c(mat_aux_2[4,num_col_Materia:num_col_Profesor],
                             "13 a 15 Sa Sesión virtual / 8 a 10 Ma",
                             8,##8-10 Martes
                             mat_aux_2[4,num_col_Lugares:num_col_URL_MAp2017])
             renglon_A3[num_col_Cambios] <- "1/2/"
             names(renglon_A3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_A3)
             
             ## INGLÉS V ##
             renglon_A4 <- c("Inglés V",
                             mat_aux_2[2,num_col_Profesor],
                             "14 a 16 Lu / 18 a 20 Ju Sesión virtual",
                             mat_aux_2[2,num_col_horario_num:num_col_Lugares],
                             15,
                             mat_aux_2[2,num_col_Salon],
                             "7096","Ciencias de la Computación","2013",
                             mat_aux_2[2,num_col_Semestre:num_col_Turno],
                             "Quinto Semestre",
                             "http://www.fciencias.unam.mx/docencia/horarios/20192/1556/1535",
                             mat_aux_2[2,num_col_Act2000:num_col_NomMat_CdC1994],0,0,0,0,
                             mat_aux_2[2,num_col_URL_Act2006:num_col_URL_CdC1994],0,0,0)
             renglon_A4[num_col_Cambios] <- "1/2/"
             names(renglon_A4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_A4)
             
             renglon_A5 <- c("Inglés V",
                             mat_aux_2[2,num_col_Profesor],
                             "14 a 16 Mi / 18 a 20 Vi Sesión virtual",
                             mat_aux_2[2,num_col_horario_num:num_col_Lugares],
                             14,
                             mat_aux_2[2,num_col_Salon],
                             "7097","Ciencias de la Computación","2013",
                             mat_aux_2[2,num_col_Semestre:num_col_Turno],
                             "Quinto Semestre",
                             "http://www.fciencias.unam.mx/docencia/horarios/20192/1556/1535",
                             mat_aux_2[2,num_col_Act2000:num_col_NomMat_CdC1994],0,0,0,0,
                             mat_aux_2[2,num_col_URL_Act2006:num_col_URL_CdC1994],0,0,0)
             renglon_A5[num_col_Cambios] <- "1/2/"
             names(renglon_A5) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_A5)
             
             
             #' Las materias "Inglés IV" de la profesora "María Azucena Rivera Vidal"
             #' son diferentes, los horarios son distintos.
             renglon_3 <- c(mat_aux[6,num_col_Materia:num_col_Profesor],
                            "13 a 14 Ma Ju / 9 a 11	Sa Sesión virtual",
                            mat_aux[6,num_col_horario_num:num_col_URL_MAp2017])
             renglon_3[num_col_Cambios] <- "1/2/"
             names(renglon_3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_3)
             
             renglon_4 <- c(mat_aux[10,num_col_Materia:num_col_Profesor],
                            "13 a 15 Sa Sesión virtual / 16 a 18 Ju",
                            16,##16-18 Jueves
                            mat_aux[10,num_col_Lugares:num_col_Cambios],
                            "V",
                            mat_aux[10,num_col_Sem_de_mat:num_col_URL_MAp2017])
             renglon_4[num_col_Cambios] <- "1/2/"
             names(renglon_4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_4)
             
             
             #' Las materias "Inglés IV" e "Inglés II" son diferentes,
             #' las clases presenciales son diferentes días a la semana,
             #' también hay sesiones virtuales.
             renglon_5 <- c(mat_aux[7,num_col_Materia:num_col_Profesor],
                            "13 a 14 Lu Mi/ 9 a 11 Sa Sesión virtual",
                            mat_aux[7,num_col_horario_num:num_col_URL_MAp2017])
             renglon_5[num_col_Cambios] <- "1/2/"
             names(renglon_5) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_5)
             
             renglon_6 <- c(mat_aux[8,num_col_Materia:num_col_Profesor],
                            "13 a 15 Sa Sesión virtual / 14 a 16 Ma",
                            14,##14-16 Martes
                            mat_aux[8,num_col_Lugares:num_col_URL_MAp2017])
             renglon_6[num_col_Cambios] <- "1/2/"
             names(renglon_6) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_6)
           },##Fin 20192
           '20201' = {
             ind_aux <- 0
             for(r in 1:dim(m_grande)[1]){
               if(m_grande[r,num_col_Profesor]=="Silvia Loera Rivera" && 
                  m_grande[r,num_col_horario_num] == "14"){
                 ind_aux <- c(ind_aux,r)}}
             ## Se quita el cero inicial
             ind_aux <- ind_aux[-1]
             ind_aux <- unique(ind_aux)
             
             mat_aux <- rbind(mat_aux,m_grande[ind_aux,])
             m_grande <- m_grande[-ind_aux,]
             
             #' Las materias "Redes de Computadoras" e "Introducción a Ciencias
             #' de la Computación" son diferentes, los horarios coinciden
             #' por las horas de los ayudantes.
             renglon_1 <- c("Redes de Computadoras",
                            mat_aux[1,num_col_Profesor:num_col_Salon],
                            "7004",
                            mat_aux[1,num_col_Carrera:num_col_URL_MAp2017])
             renglon_1[num_col_Cambios] <- "1/"
             names(renglon_1) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_1)
             
             renglon_2 <- c("Introducción a Ciencias de la Computación",
                            mat_aux[2,num_col_Profesor:num_col_Horario],
                            13,
                            mat_aux[2,num_col_Lugares:num_col_URL_MAp2017])
             renglon_2[num_col_Cambios] <- "1/"
             names(renglon_2) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_2)
             
             #' Las materias de inglés sólo se imparten un día a la semana, los
             #' días en que se imparten las clases se van a colocar en la
             #' columna "Horario" para que sólo haya números en la columna
             #' "horario_num"
             
             #' Las materias "Inglés I", "Inglés III", "/Inglés V/Inglés VI"
             #' son diferentes, las clases presenciales son únicamente un día
             #' a la semana y también hay sesiones virtuales, se tienen 4
             #' clases diferentes.
             renglon_3 <- c(mat_aux[3,num_col_Materia:num_col_Profesor],
                            "13 a 15 Ma",
                            13,##Se imparte los martes
                            mat_aux[3,num_col_Lugares:num_col_URL_MAp2017])
             renglon_3[num_col_Cambios] <- "1/2/"
             names(renglon_3) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_3)
             
             renglon_4 <- c(mat_aux[4,num_col_Materia:num_col_Profesor],
                            "13 a 15 Vi",
                            13,##Se imparte los viernes
                            mat_aux[4,num_col_Lugares:num_col_Alumnos],
                            "P104",
                            mat_aux[4,num_col_Grupo:num_col_URL_MAp2017])
             renglon_4[num_col_Cambios] <- "1/2/"
             names(renglon_4) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_4)
             
             renglon_5 <- c("Inglés V",
                            mat_aux[5,num_col_Profesor],
                            "15 a 17	Lu Sesión virtual / 13 a 15 Lu",13,25,25,"P105","9325",
                            "Actuaría","2015",
                            mat_aux[5,num_col_Semestre:num_col_Turno],
                            "Sexto Semestre",
                            "http://www.fciencias.unam.mx/docencia/horarios/20201/2017/1640",
                            mat_aux[5,num_col_Act2000:num_col_NomMat_Act2006],0,
                            mat_aux[5,num_col_NomMat_CdC1994:num_col_URL_Act2006],0,
                            mat_aux[5,num_col_URL_CdC1994:num_col_URL_MAp2017])
             renglon_5[num_col_Cambios] <- "1/2/"
             names(renglon_5) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_5)
             
             renglon_6 <- c("Inglés VI",
                            mat_aux[5,num_col_Profesor],
                            "13 a 15 Mi / 15 a 17 Mi Sesión virtual",13,25,26,
                            "P105","9327","Actuaría","2015",
                            mat_aux[5,num_col_Semestre:num_col_Turno],
                            "Séptimo Semestre",
                            "http://www.fciencias.unam.mx/docencia/horarios/20201/2017/1740",
                            mat_aux[5,num_col_Act2000:num_col_NomMat_Act2006],0,
                            mat_aux[5,num_col_NomMat_CdC1994:num_col_URL_Act2006],0,
                            mat_aux[5,num_col_URL_CdC1994:num_col_URL_MAp2017])
             renglon_6[num_col_Cambios] <- "1/2/"
             names(renglon_6) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_6)
             
             
             #' Las materias "Taller de Herramientas Computacionales" y "Manejo de
             #' Datos" son diferentes, el horario es el mismo, pero se imparten
             #' en días distintos
             renglon_7 <- c(mat_aux[6,num_col_Materia:num_col_Profesor],
                            "7 a 9 Ma Ju",7,##Se imparte los martes y jueves de 7-9hrs
                            mat_aux[6,num_col_Lugares:num_col_URL_MAp2017])
             renglon_7[num_col_Cambios] <- "1/2/"
             names(renglon_7) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_7)
             
             renglon_8 <- c("Manejo de Datos",
                            mat_aux[7,num_col_Profesor],
                            "7 a 8 Lu Mi Vi",7,
                            mat_aux[7,num_col_Lugares:num_col_Salon],
                            "9172",
                            mat_aux[7,num_col_Carrera:num_col_URL_MAp2017])
             renglon_8[num_col_Cambios] <- "1/2/"
             names(renglon_8) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_8)
             
             
             #' Las materias "Inglés III" e "/Inglés V/Inglés VI"
             #' son diferentes, las clases presenciales son únicamente un día
             #' a la semana y también hay sesiones virtuales, se tienen 3
             #' clases diferentes.
             renglon_9 <- c(mat_aux[8,num_col_Materia:num_col_Profesor],
                            "14 a 16 Vi Sesión virtual / 14 a 16 Lu",
                            14,#Se imparten los lunes y viernes
                            mat_aux[8,num_col_Lugares:num_col_Alumnos],
                            "P104",
                            mat_aux[8,num_col_Grupo:num_col_URL_MAp2017])
             renglon_9[num_col_Cambios] <- "1/2/"
             names(renglon_9) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_9)
             
             renglon_10 <- c("Inglés V",
                             mat_aux[9,num_col_Profesor],
                             "14 a 16 Ma / 16 a 18 Ma Sesión virtual",
                             14,#Se imparte los martes
                             25,28,"P105","9326","Actuaría","2015",
                             mat_aux[9,num_col_Semestre:num_col_Turno],
                             "Sexto Semestre",
                             "http://www.fciencias.unam.mx/docencia/horarios/20201/2017/1640",
                             mat_aux[9,num_col_Act2000:num_col_NomMat_Act2006],0,
                             mat_aux[9,num_col_NomMat_CdC1994:num_col_URL_Act2006],0,
                             mat_aux[9,num_col_URL_CdC1994:num_col_URL_MAp2017])
             renglon_10[num_col_Cambios] <- "1/2/"
             names(renglon_10) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_10)
             
             renglon_11 <- c("Inglés VI",
                             mat_aux[9,num_col_Profesor],
                             "14 a 16 Ju / 16 a 18 Ju Sesión virtual",
                             14,#Se imparten los jueves
                             25,25,"P105","9328","Actuaría","2015",
                             mat_aux[9,num_col_Semestre:num_col_Turno],
                             "Séptimo Semestre",
                             "http://www.fciencias.unam.mx/docencia/horarios/20201/2017/1740",
                             mat_aux[9,num_col_Act2000:num_col_NomMat_Act2006],0,
                             mat_aux[9,num_col_NomMat_CdC1994:num_col_URL_Act2006],0,
                             mat_aux[9,num_col_URL_CdC1994:num_col_URL_MAp2017])
             renglon_11[num_col_Cambios] <- "1/2/"
             names(renglon_11) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_11)
             
             
             #' Las materias "Inglés I" e "Inglés VI" son diferentes, las
             #' clases presenciales son únicamente un día a la semana y
             #' también hay sesiones virtuales, se tienen 2 clases diferentes.
             renglon_12 <- c(mat_aux[10,num_col_Materia:num_col_Profesor],
                             "14 a 16	Mi",14,#Se imparte los miércoles
                             mat_aux[10,num_col_Lugares:num_col_URL_MAp2017])
             renglon_12[num_col_Cambios] <- "1/2/"
             names(renglon_12) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_12)
             
             renglon_13 <- c(mat_aux[11,num_col_Materia:num_col_Profesor],
                             "14 a 16 Sesión presencial / 16 a 18	Sesión virtual",14,
                             mat_aux[11,num_col_Lugares:num_col_URL_MAp2017])
             renglon_13[num_col_Cambios] <- "1/2/"
             names(renglon_13) <- param$nom_cols_MG
             m_grande <- rbind(m_grande,renglon_13)
           }##Fin 20201
    )##Fin switch(sem_info)
  }else{##Si NO hay repeticiones
    m_grande <- m_grande_SIN_MOD
  }
  
  # save(m_grande, file = paste0("m_grande por semestre/m_grande_",sem_info,".RData"))
  save(m_grande, file = paste0("m_grande por semestre SIN INGLES/m_grande_SIN_ING_",sem_info,".RData"))
  return(m_grande)
}



# imprime_info_idiomas_1_sem ----------------------------------------------
#' Title imprime_info_idiomas_1_sem: Función que imprime la información de
#' "idioma". Arroja una variable binaria que indica si la matriz "m_grande"
#' del semestre "sem_info" requiere modificación.
#'
#' @param nom_archivo : Nombre del archivo que se va a cargar con la
#' matriz "m_grande".
#' @param sem_info: Semestre del que se desea obtener información.
#' @param idioma: Variable de tipo "char" la cual indica el idioma del
#' que se debe tomar la información.
#'
#' @example nom_archivo <- "m_grande por semestre SIN INGLES/m_grande_SIN_ING_20182.RData"
#' @example sem_info <- 20182
#' @example idioma <- "Inglés"
#'
#' @return mod_1si_0no: Variable binaria que indica si la matriz "m_grande"
#' del semestre "sem_info" requiere modificación.
imprime_info_idiomas_1_sem <- function(nom_archivo,sem_info,idioma){
  ##Se definen las variables que se van a utilizar:
  num_col_Materia <- arroja_ind_col_MG("Materia")##1
  num_col_NomMat_Act2000 <- arroja_ind_col_MG("NomMat_Act2000")##23
  num_col_NomMat_MAp2017 <- arroja_ind_col_MG("NomMat_MAp2017")##29
  mod_1si_0no <- 0
  
  load(nom_archivo)#Se carga "m_grande" de "idioma" del semestre "sem_info"
  vec_ind_aux <- 0
  
  #Se obtienen los renglones que contienen grupos de "idioma"
  for(d in 1:dim(m_grande)[1]){##Recorre renglones
    texto_1 <- substr(m_grande[d,num_col_Materia],1,6)
    texto_2 <- substr(m_grande[d,num_col_Materia],1,7)
    # if(texto_1=="Inglés" || texto_2=="/Inglés"){
    if(texto_1==idioma || texto_2==paste0("/",idioma)){
      vec_ind_aux <- c(vec_ind_aux,d)}}
  
  if(length(vec_ind_aux) > 1){#Si hay clases de "idioma"
    vec_ind_aux <- vec_ind_aux[-1]#Se quita el cero inicial
    
    #' El vector "vec_ind_aux_2" contiene los índices de los renglones
    #' de "m_grande" que tienen más de una materia de inglés
    vec_ind_aux_2 <- 0
    for(d in 1:length(vec_ind_aux)){##Recorre los índices de los renglones
      if(!all(m_grande[vec_ind_aux[d],
                       num_col_NomMat_Act2000:num_col_NomMat_MAp2017]==rep(0,7))){
        vec_ind_aux_2 <- c(vec_ind_aux_2,vec_ind_aux[d])}}
    if(length(vec_ind_aux_2) > 1){#En caso de que haya clases repetidas
      vec_ind_aux_2 <- vec_ind_aux_2[-1]#Se quita el cero inicial
      
      cat("\n En el semestre ",sem_info," se tienen ",length(vec_ind_aux_2),
          " clases repetidas de ",idioma)
      mod_1si_0no <- 1
      
    }else{#En caso de que en "m_grande" no haya clases repetidas
      cat("\n La matriz m_grande del semestre ",sem_info,
          " no tiene clases repetidas de ",idioma)
    }
  }else{#En caso de que en "m_grande" no haya clases de "idioma"
    cat("\n La matriz m_grande del semestre ",sem_info,
        " no tiene clases de ",idioma)
  }
  
  return(mod_1si_0no)
}


# imprime_info_idiomas ----------------------------------------------------
#' Title imprime_info_idiomas: Función que imprime la información de los
#' idiomas de los semestres 2008-1 al 2020-1. Arroja un vector con los
#' semestres que requieren modificación de las materias de "idioma".
#'
#' @param idioma: Variable de tipo "char" la cual indica el idioma del
#' que se debe tomar la información.
#'
#' @example idioma <- "Inglés"
#'
#' @return vec_sem_idiomas: Vector con los semestres que requieren
#' modificación en "m_grande".
imprime_info_idiomas <- function(idioma){
  semestres <- (20081:20201)[(20081:20201)%% 10>0 &(20081:20201)%% 10<3]
  vec_sem_idiomas <- 0
  for(s in 1:length(semestres)){
    sem_info <- semestres[s]
    nom_archivo <- paste0("m_grande por semestre SIN INGLES/m_grande_SIN_ING_",
                          sem_info,".RData")
    if(file.exists(nom_archivo)){
      mod_1si_0no <- imprime_info_idiomas_1_sem(nom_archivo,sem_info,idioma)}
    
    if(mod_1si_0no == 1){
      vec_sem_idiomas <- c(vec_sem_idiomas,sem_info)}
  }#fin for(s)
  if(length(vec_sem_idiomas) > 1){
    vec_sem_idiomas <- vec_sem_idiomas[-1]
  }
  return(vec_sem_idiomas)
}


# revisa_gpos_idiomas_1_sem -----------------------------------------------
#' Title revisa_gpos_idiomas_1_sem: Función en la que se identifican y
#' separan los grupos de idiomas que se imparten en días distintos pero en
#' el mismo horario.
#'
#' @param sem_info: Semestre del que se desea obtener información.
#' @param idioma: Variable de tipo "char" la cual indica el idioma del
#' que se debe tomar la información.
#'
#' @example sem_info <- 20182
#' @example idioma <- "Inglés"
#' 
#' @return m_grande: Matriz de 36 columnas (Materia,Profesor,Horario,
#' horario_num,Lugares,Alumnos,Salon,Grupo,Carrera,Plan,Semestre,Cambios,
#' Turno,Semestre_de_materia,url,Act2000,Act2006,Act2015,CdC1994,CdC2013,
#' Mat1983,MAp2017,NomMat_Act2000,NomMat_Act2006,NomMat_Act2015,
#' NomMat_CdC1994,NomMat_CdC2013,NomMat_Mat1983,NomMat_MAp2017,URL_Act2000,
#' URL_Act2006,URL_Act2015,URL_CdC1994,URL_CdC2013,URL_Mat1983,URL_MAp2017),
#' con la información de "sem_info". Las columnas 16-22 son columnas
#' binarias las cuales indican con un 1 si el grupo del i-ésimo renglón
#' pertenece a la carrera y plan correspondiente al nombre de cada columna,
#' hay un 0 e.o.c. Las columnas 23-29 indican el nombre de las materias
#' dependiendo del plan y carrera al que pertenecen. Las columnas 30-36 
#' indican las URLs dependiendo del plan y carrera al que pertenecen.
#'
revisa_gpos_idiomas_1_sem <- function(sem_info,idioma){
  ##Se definen las variables que se van a utilizar:
  num_col_Materia <- arroja_ind_col_MG("Materia")##1
  num_col_Profesor <- arroja_ind_col_MG("Profesor")##2
  num_col_horario_num <- arroja_ind_col_MG("horario_num")##4
  num_col_Semestre <- arroja_ind_col_MG("Semestre")##11
  num_col_Cambios <- arroja_ind_col_MG("Cambios")##12
  num_col_Turno <- arroja_ind_col_MG("Turno")##13
  num_col_Act2000 <- arroja_ind_col_MG("Act2000")##16
  num_col_NomMat_Act2000 <- arroja_ind_col_MG("NomMat_Act2000")##23
  num_col_NomMat_Act2006 <- arroja_ind_col_MG("NomMat_Act2006")##24
  num_col_NomMat_CdC1994 <- arroja_ind_col_MG("NomMat_CdC1994")##26
  num_col_NomMat_MAp2017 <- arroja_ind_col_MG("NomMat_MAp2017")##29
  num_col_URL_Act2006 <- arroja_ind_col_MG("URL_Act2006")##31
  num_col_URL_CdC1994 <- arroja_ind_col_MG("URL_CdC1994")##33
  num_col_URL_MAp2017 <- arroja_ind_col_MG("URL_MAp2017")##36
  
  #Se define el vector que contiene los semestres que requieren modificación
  vec_sem_idiomas <- imprime_info_idiomas(idioma)
  
  switch(idioma,
         'Inglés' = {
           #Se carga la matriz "m_grande" de "sem_info"
           nom_archivo <- paste0("m_grande por semestre SIN INGLES/m_grande_SIN_ING_",
                                 sem_info,".RData")
           load(nom_archivo)
           # View(m_grande)
           m_grande_SIN_ING <- m_grande
           
           if(any(sem_info == vec_sem_idiomas)){#En caso de que se requieran modificaciones
             #Se obtienen los índices de las materias de inglés
             vec_ind_aux <- 0
             for(d in 1:dim(m_grande_SIN_ING)[1]){##Recorre renglones
               texto_1 <- substr(m_grande_SIN_ING[d,num_col_Materia],1,6)
               texto_2 <- substr(m_grande_SIN_ING[d,num_col_Materia],1,7)
               if(texto_1=="Inglés" || texto_2=="/Inglés"){
                 vec_ind_aux <- c(vec_ind_aux,d)}}
             vec_ind_aux <- vec_ind_aux[-1]#Se quita el cero inicial
             
             #' El vector "vec_ind_aux_2" contiene los índices de los renglones
             #' de "m_grande_SIN_ING" que tienen más de una materia de inglés
             vec_ind_aux_2 <- 0
             for(d in 1:length(vec_ind_aux)){##Recorre los índices de los renglones
               if(!all(m_grande_SIN_ING[vec_ind_aux[d],
                                        num_col_NomMat_Act2000:num_col_NomMat_MAp2017]==rep(0,7))){
                 vec_ind_aux_2 <- c(vec_ind_aux_2,vec_ind_aux[d])}}
             vec_ind_aux_2 <- vec_ind_aux_2[-1]#Se quita el cero inicial
             
             #Se modifica "m_grande"
             mat_aux <- m_grande_SIN_ING[vec_ind_aux_2,]
             m_grande <- m_grande_SIN_ING[-vec_ind_aux_2,]
             
             switch(sem_info,
                    '20152' = {
                      #' Las materias "/Inglés I/Inglés II" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      renglon_1 <- c("Inglés I",
                                     mat_aux[1,num_col_Profesor],
                                     "14 a 16	Ma Ju",
                                     14,##14-16hrs Martes Jueves
                                     40,40,"P102","7347",
                                     "Ciencias de la Computación","2013",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Primer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20152/1556/1124",
                                     0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_1[num_col_Cambios] <- "1/2/"
                      names(renglon_1) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_1)
                      
                      renglon_2 <- c("Inglés II",
                                     mat_aux[1,num_col_Profesor],
                                     "14 a 16 Lu Mi",
                                     14,##14-16hrs Lunes Miércoles
                                     40,38,"O122","7348",
                                     "Ciencias de la Computación","2013",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Segundo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20152/1556/1223",
                                     0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_2[num_col_Cambios] <- "1/2/"
                      names(renglon_2) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_2)
                      
                      
                      #' Las materias "/Inglés II/Inglés III" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      renglon_3 <- c("Inglés II",
                                     mat_aux[2,num_col_Profesor],
                                     "14 a 16 Lu Mi",
                                     14,##14-16hrs Lunes Miércoles
                                     42,41,"P201","7349",
                                     "Ciencias de la Computación","2013",
                                     mat_aux[2,num_col_Semestre:num_col_Turno],
                                     "Segundo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20152/1556/1223",
                                     0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_3[num_col_Cambios] <- "1/2/"
                      names(renglon_3) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_3)
                      
                      renglon_4 <- c("Inglés III",
                                     mat_aux[2,num_col_Profesor],
                                     "14 a 16 Ma Ju",
                                     14,##14-16hrs Martes Jueves
                                     40,25,"P201","7350",
                                     "Ciencias de la Computación","2013",
                                     mat_aux[2,num_col_Semestre:num_col_Turno],
                                     "Tercer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20152/1556/1322",
                                     0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_4[num_col_Cambios] <- "1/2/"
                      names(renglon_4) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_4)
                    },##Fin 20152
                    '20161' = {
                      #' Las materias "/Inglés II/Inglés I/Inglés III" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      renglon_1 <- c("Inglés II",
                                     mat_aux[1,num_col_Profesor],
                                     "14 a 16	Vi",
                                     14,##14-16hrs Viernes
                                     77,74,"201 (Yelizcalli)","9253","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Segundo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20161/2017/1235",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_1[num_col_Cambios] <- "1/2/"
                      names(renglon_1) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_1)
                      
                      renglon_2 <- c("Inglés I",
                                     mat_aux[1,num_col_Profesor],
                                     "14 a 16 Ma Ju",
                                     14,##14-16hrs Martes Jueves
                                     60,54,"204 (Yelizcalli)","7096",
                                     "Ciencias de la Computación","2013",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Primer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20161/1556/1124",
                                     0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_2[num_col_Cambios] <- "1/2/"
                      names(renglon_2) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_2)
                      
                      renglon_3 <- c("Inglés III",
                                     mat_aux[1,num_col_Profesor],
                                     "14 a 16 Lu Mi",
                                     14,##14-16hrs Lunes Miércoles
                                     30,26,"201 (Yelizcalli)","7099",
                                     "Ciencias de la Computación","2013",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Tercer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20161/1556/1322",
                                     0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_3[num_col_Cambios] <- "1/2/"
                      names(renglon_3) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_3)
                      
                      
                      #' Las materias "/Inglés III/Inglés I/Inglés II" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      renglon_4 <- c("Inglés III",
                                     mat_aux[2,num_col_Profesor],
                                     "9 a 11 Sa",
                                     9,##9-11hrs Sábado
                                     40,39,"011",##Tlahuizcalpan
                                     "9261","Actuaría","2015",
                                     mat_aux[2,num_col_Semestre:num_col_Turno],
                                     "Tercer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20161/2017/1336",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_4[num_col_Cambios] <- "1/2/"
                      names(renglon_4) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_4)
                      
                      renglon_5 <- c("Inglés III",
                                     mat_aux[2,num_col_Profesor],
                                     "11 a 13 Sa",
                                     11,##11-13hrs Sábado
                                     40,33,"002 (Yelizcalli)","9263","Actuaría","2015",
                                     mat_aux[2,num_col_Semestre:num_col_Turno],
                                     "Tercer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20161/2017/1336",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_5[num_col_Cambios] <- "1/2/"
                      names(renglon_5) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_5)
                      
                      renglon_6 <- c("Inglés III",
                                     mat_aux[2,num_col_Profesor],
                                     "14 a 16 Vi",
                                     14,##14-16hrs Viernes
                                     40,40,"106 (Yelizcalli)","9265","Actuaría","2015",
                                     mat_aux[2,num_col_Semestre:num_col_Turno],
                                     "Tercer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20161/2017/1336",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_6[num_col_Cambios] <- "1/2/"
                      names(renglon_6) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_6)
                      
                      renglon_7 <- c("Inglés I",
                                     mat_aux[2,num_col_Profesor],
                                     "14 a 16 Ma Ju",
                                     14,##14-16hrs Martes Jueves
                                     60,53,"203 (Yelizcalli)","7095",
                                     "Ciencias de la Computación","2013",
                                     mat_aux[2,num_col_Semestre:num_col_Turno],
                                     "Primer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20161/1556/1124",
                                     0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_7[num_col_Cambios] <- "1/2/"
                      names(renglon_7) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_7)
                      
                      renglon_8 <- c("Inglés II",
                                     mat_aux[2,num_col_Profesor],
                                     "14 a 16 Lu Mi",
                                     14,##14-16hrs Lunes Miércoles
                                     45,40,"202 (Yelizcalli)","7097",
                                     "Ciencias de la Computación","2013",
                                     mat_aux[2,num_col_Semestre:num_col_Turno],
                                     "Segundo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20161/1556/1223",
                                     0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_8[num_col_Cambios] <- "1/2/"
                      names(renglon_8) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_8)
                      
                      
                      #' Las materias "/Inglés III/Inglés IV" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      renglon_9 <- c("Inglés III",
                                     mat_aux[3,num_col_Profesor],
                                     "14 a 16 Ma Ju",
                                     14,##14-16hrs Martes Jueves
                                     40,16,"O129","7098",
                                     "Ciencias de la Computación","2013",
                                     mat_aux[3,num_col_Semestre:num_col_Turno],
                                     "Tercer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20161/1556/1322",
                                     0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_9[num_col_Cambios] <- "1/2/"
                      names(renglon_9) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_9)
                      
                      renglon_10 <- c("Inglés IV",
                                      mat_aux[3,num_col_Profesor],
                                      "14 a 16 Lu Mi",
                                      14,##14-16hrs Lunes Miércoles
                                      40,20,"204 (Yelizcalli)","7100",
                                      "Ciencias de la Computación","2013",
                                      mat_aux[3,num_col_Semestre:num_col_Turno],
                                      "Cuarto Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20161/1556/1426",
                                      0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_10[num_col_Cambios] <- "1/2/"
                      names(renglon_10) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_10)
                    },##Fin 20161
                    '20162' = {
                      #' Las materias "/Inglés I/Inglés I/Inglés II/Inglés II" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      #' Manuel Enrique Camargo Coronel	
                      renglon_1 <- c("Inglés I",
                                     mat_aux[1,num_col_Profesor],
                                     "13 a 15 Lu",
                                     13,##13-15hrs Lunes
                                     55,24,"P201","9343","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Primer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20162/2017/1135",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_1[num_col_Cambios] <- "1/2/"
                      names(renglon_1) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_1)
                      
                      
                      renglon_2 <- c("Inglés I",
                                     mat_aux[1,num_col_Profesor],
                                     "13 a 15 Mi",
                                     13,##13-15hrs Miércoles
                                     55,20,"P201","9344","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Primer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20162/2017/1135",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_2[num_col_Cambios] <- "1/2/"
                      names(renglon_2) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_2)
                      
                      renglon_3 <- c("Inglés II",
                                     mat_aux[1,num_col_Profesor],
                                     "13 a 15 Ma",
                                     13,##13-15hrs Martes
                                     50,48,"P201","9345","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Segundo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20162/2017/1235",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_3[num_col_Cambios] <- "1/2/"
                      names(renglon_3) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_3)
                      
                      renglon_4 <- c("Inglés II",
                                     mat_aux[1,num_col_Profesor],
                                     "13 a 15 Ju",
                                     13,##13-15hrs Jueves
                                     50,36,"P201","9347","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Segundo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20162/2017/1235",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_4[num_col_Cambios] <- "1/2/"
                      names(renglon_4) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_4)
                      
                      
                      #' Las materias "/Inglés II/Inglés IV" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      #' Lidia Fabiola Quevedo Rojas
                      renglon_L1 <- c("Inglés II",
                                      mat_aux[2,num_col_Profesor],
                                      "14 a 16 Ma",
                                      14,##14-16hrs Martes
                                      50,47,"O219","9346","Actuaría","2015",
                                      mat_aux[2,num_col_Semestre:num_col_Turno],
                                      "Segundo Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20162/2017/1235",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_L1[num_col_Cambios] <- "1/2/"
                      names(renglon_L1) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_L1)
                      
                      renglon_L2 <- c("Inglés II",
                                      mat_aux[2,num_col_Profesor],
                                      "13 a 15 Vi",
                                      13,##13-15hrs Viernes
                                      50,31,"P201","9348","Actuaría","2015",
                                      mat_aux[2,num_col_Semestre:num_col_Turno],
                                      "Segundo Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20162/2017/1235",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_L2[num_col_Cambios] <- "1/2/"
                      names(renglon_L2) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_L2)
                      
                      
                      renglon_L3 <- c("Inglés IV",
                                      mat_aux[2,num_col_Profesor],
                                      "9 a 11 Sa",
                                      9,##9-11hrs Sábado
                                      45,45,"O125","9354","Actuaría","2015",
                                      mat_aux[2,num_col_Semestre:num_col_Turno],
                                      "Cuarto Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20162/2017/1436",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_L3[num_col_Cambios] <- "1/2/"
                      names(renglon_L3) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_L3)
                      
                      renglon_L4 <- c("Inglés IV",
                                      mat_aux[2,num_col_Profesor],
                                      "11 a 13 Sa",
                                      11,##11-13hrs Sábado
                                      45,40,"O125","9355","Actuaría","2015",
                                      mat_aux[2,num_col_Semestre:num_col_Turno],
                                      "Cuarto Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20162/2017/1436",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_L4[num_col_Cambios] <- "1/2/"
                      names(renglon_L4) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_L4)
                      
                      renglon_L5 <- c("Inglés IV",
                                      mat_aux[2,num_col_Profesor],
                                      "13 a 15 Ju",
                                      13,##13-15hrs Jueves
                                      45,45,"203 (Yelizcalli)","9356","Actuaría","2015",
                                      mat_aux[2,num_col_Semestre:num_col_Turno],
                                      "Cuarto Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20162/2017/1436",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_L5[num_col_Cambios] <- "1/2/"
                      names(renglon_L5) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_L5)
                      
                      renglon_L6 <- c("Inglés V",
                                      mat_aux[3,num_col_Profesor],
                                      "14 a 16 Lu Mi",
                                      14,##14-16hrs Lunes Miércoles
                                      35,27,"O219","7096",
                                      "Ciencias de la Computación","2013",
                                      mat_aux[3,num_col_Semestre:num_col_Turno],
                                      "Quinto Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20162/1556/1535",
                                      0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_L6[num_col_Cambios] <- "1/2/"
                      names(renglon_L6) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_L6)
                      
                      
                      #' Las materias "/Inglés III/Inglés IV" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      #' Martha del Carmen Riveroll Alvarez
                      renglon_M1 <- c("Inglés III",
                                      mat_aux[4,num_col_Profesor],
                                      "14 a 16 Ma Ju",
                                      14,##14-16hrs Martes Jueves
                                      40,18,"P210","7094",
                                      "Ciencias de la Computación","2013",
                                      mat_aux[4,num_col_Semestre:num_col_Turno],
                                      "Tercer Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20162/1556/1322",
                                      0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_M1[num_col_Cambios] <- "1/2/"
                      names(renglon_M1) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_M1)
                      
                      renglon_M2 <- c("Inglés IV",
                                      mat_aux[4,num_col_Profesor],
                                      "14 a 16 Lu Mi",
                                      14,##14-16hrs Lunes Martes
                                      45,41,"P210","7095",
                                      "Ciencias de la Computación","2013",
                                      mat_aux[4,num_col_Semestre:num_col_Turno],
                                      "Cuarto Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20162/1556/1426",
                                      0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_M2[num_col_Cambios] <- "1/2/"
                      names(renglon_M2) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_M2)
                    },##Fin 20162
                    '20171' = {
                      #' Las materias "Inglés V/Inglés V/Inglés V" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      renglon_1 <- c("Inglés V",
                                     mat_aux[1,num_col_Profesor],
                                     "14 a 16 Lu / 13 a 15 Sa Sesión AVE",
                                     14,##14-16hrs Lunes
                                     0,10,"O216","9385","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Sexto Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20171/2017/1640",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_1[num_col_Cambios] <- "1/2/"
                      names(renglon_1) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_1)
                      
                      
                      renglon_2 <- c("Inglés V",
                                     mat_aux[1,num_col_Profesor],
                                     "14 a 16 Ma / 16 a 18 Mi Sesión AVE",
                                     14,##14-16hrs Martes
                                     0,32,"003 (Yelizcalli)","9386","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Sexto Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20171/2017/1640",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_2[num_col_Cambios] <- "1/2/"
                      names(renglon_2) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_2)
                      
                      renglon_3 <- c("Inglés V",
                                     mat_aux[1,num_col_Profesor],
                                     "14 a 16 Mi / 16 a 18 Ju Sesión AVE",
                                     14,##14-16hrs Miércoles
                                     0,40,"003 (Yelizcalli)","9387","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Sexto Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20171/2017/1640",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_3[num_col_Cambios] <- "1/2/"
                      names(renglon_3) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_3)
                    },##Fin 20171
                    '20172' = {
                      #' Las materias "/Inglés II/Inglés IV/Inglés VI" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      renglon_1 <- c("Inglés II",
                                     mat_aux[1,num_col_Profesor],
                                     "13 a 15 Ma / 15 a 17 Ma Sesión virtual",
                                     13,##13-15hrs Martes
                                     50,50,"102 (Yelizcalli)","9341","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Segundo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20172/2017/1235",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_1[num_col_Cambios] <- "1/2/"
                      names(renglon_1) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_1)
                      
                      renglon_2 <- c("Inglés IV",
                                     mat_aux[1,num_col_Profesor],
                                     "13 a 15 Lu / 15 a 17 Lu Sesión virtual",
                                     13,##13-15hrs Lunes
                                     45,45,"102 (Yelizcalli)","9349","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Cuarto Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20172/2017/1436",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_2[num_col_Cambios] <- "1/2/"
                      names(renglon_2) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_2)
                      
                      renglon_3 <- c("Inglés VI",
                                     mat_aux[1,num_col_Profesor],
                                     "13 a 15 Mi / 15 a 17 Mi Sesión virtual",
                                     13,##13-15hrs Miércoles
                                     48,48,"002 (Yelizcalli)","9355","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Séptimo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20172/2017/1740",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_3[num_col_Cambios] <- "1/2/"
                      names(renglon_3) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_3)
                      
                      
                      #' Las materias "/Inglés III/Inglés V" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      renglon_4 <- c("Inglés III",
                                     mat_aux[2,num_col_Profesor],
                                     "14 a 16 Mi / 16 a 18 Mi Sesión virtual",
                                     14,##14-16hrs Miércoles
                                     32,31,"102 (Yelizcalli)","9348","Actuaría","2015",
                                     mat_aux[2,num_col_Semestre:num_col_Turno],
                                     "Tercer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20172/2017/1336",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_4[num_col_Cambios] <- "1/2/"
                      names(renglon_4) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_4)
                      
                      renglon_5 <- c("Inglés V",
                                     mat_aux[2,num_col_Profesor],
                                     "14 a 16 Lu / 16 a 18 Lu Sesión virtual",
                                     14,##14-16hrs Lunes
                                     40,38,"003 (Yelizcalli)","9354","Actuaría","2015",
                                     mat_aux[2,num_col_Semestre:num_col_Turno],
                                     "Sexto Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20172/2017/1640",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_5[num_col_Cambios] <- "1/2/"
                      names(renglon_5) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_5)
                    },##Fin 20172
                    '20181' = {
                      #' Las materias "/Inglés I/Inglés I/Inglés III/Inglés III/Inglés VI"
                      #' son diferentes, las clases presenciales se imparten en días
                      #' distintos.
                      #' Arturo García Miranda	
                      renglon_A1 <- c("Inglés I",
                                      mat_aux[1,num_col_Profesor],
                                      "13 a 15 Lu / 11 a 13 Lu Sesión virtual",
                                      13,##13-15hrs Lunes
                                      50,57,"001 (Yelizcalli)","9337","Actuaría","2015",
                                      mat_aux[1,num_col_Semestre:num_col_Turno],
                                      "Primer Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20181/2017/1135",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_A1[num_col_Cambios] <- "1/2/"
                      names(renglon_A1) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_A1)
                      
                      renglon_A2 <- c("Inglés I",
                                      mat_aux[1,num_col_Profesor],
                                      "13 a 15 Ju / 11 a 13 Ju Sesión virtual",
                                      13,##13-15hrs Jueves
                                      50,57,"008",##Tlahuizcalpan
                                      "9340","Actuaría","2015",
                                      mat_aux[1,num_col_Semestre:num_col_Turno],
                                      "Primer Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20181/2017/1135",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_A2[num_col_Cambios] <- "1/2/"
                      names(renglon_A2) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_A2)
                      
                      renglon_A3 <- c("Inglés III",
                                      mat_aux[1,num_col_Profesor],
                                      "13 a 15 Mi / 15 a 17 Mi Sesión virtual",
                                      13,##13-15hrs Miércoles
                                      45,43,"P201","9346","Actuaría","2015",
                                      mat_aux[1,num_col_Semestre:num_col_Turno],
                                      "Tercer Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20181/2017/1336",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_A3[num_col_Cambios] <- "1/2/"
                      names(renglon_A3) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_A3)
                      
                      renglon_A4 <- c("Inglés III",
                                      mat_aux[1,num_col_Profesor],
                                      "13 a 15 Vi / 11 a 13 Vi Sesión virtual",
                                      13,##13-15hrs Viernes
                                      45,31,"001 (Yelizcalli)","9348","Actuaría","2015",
                                      mat_aux[1,num_col_Semestre:num_col_Turno],
                                      "Tercer Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20181/2017/1336",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_A4[num_col_Cambios] <- "1/2/"
                      names(renglon_A4) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_A4)
                      
                      renglon_A5 <- c("Inglés VI",
                                      mat_aux[1,num_col_Profesor],
                                      "13 a 15 Ma / 15 a 17 Ma Sesión virtual",
                                      13,##13-15hrs Martes
                                      50,55,"001 (Yelizcalli)","9356","Actuaría","2015",
                                      mat_aux[1,num_col_Semestre:num_col_Turno],
                                      "Séptimo Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20181/2017/1740",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_A5[num_col_Cambios] <- "1/2/"
                      names(renglon_A5) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_A5)
                      
                      
                      #' Las materias "Inglés I/Inglés I/Inglés I" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      #' Lidia Fabiola Quevedo Rojas
                      renglon_L1 <- c("Inglés I",
                                      mat_aux[2,num_col_Profesor],
                                      "14 a 16 Ma / 16 a 18 Ma Sesión virtual",
                                      14,##14-16hrs Martes
                                      50,57,"O216","9338","Actuaría","2015",
                                      mat_aux[2,num_col_Semestre:num_col_Turno],
                                      "Primer Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20181/2017/1135",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_L1[num_col_Cambios] <- "1/2/"
                      names(renglon_L1) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_L1)
                      
                      renglon_L2 <- c("Inglés I",
                                      mat_aux[2,num_col_Profesor],
                                      "14 a 16 Mi / 15 a 17 Lu Sesión virtual",
                                      14,##14-16hrs Miércoles
                                      50,58,"O216","9343","Actuaría","2015",
                                      mat_aux[2,num_col_Semestre:num_col_Turno],
                                      "Primer Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20181/2017/1135",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_L2[num_col_Cambios] <- "1/2/"
                      names(renglon_L2) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_L2)
                      
                      
                      #' Las materias "/Inglés I/Inglés II/Inglés V" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      #' Martha del Carmen Riveroll Alvarez	
                      renglon_M1 <- c("Inglés I",
                                      mat_aux[3,num_col_Profesor],
                                      "13 a 15 Vi / 11 a 13 Vi Sesión virtual",
                                      13,##13-15hrs Viernes
                                      53,41,"008",##Tlahuizcalpan
                                      "9341","Actuaría","2015",
                                      mat_aux[3,num_col_Semestre:num_col_Turno],
                                      "Primer Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20181/2017/1135",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_M1[num_col_Cambios] <- "1/2/"
                      names(renglon_M1) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_M1)
                      
                      renglon_M2 <- c("Inglés I",
                                      mat_aux[3,num_col_Profesor],
                                      "13 a 15 Lu / 15 a 17 Lu Sesión virtual",
                                      13,##13-15hrs Lunes
                                      50,53,"008",##Tlahuizcalpan
                                      "9342","Actuaría","2015",
                                      mat_aux[3,num_col_Semestre:num_col_Turno],
                                      "Primer Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20181/2017/1135",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_M2[num_col_Cambios] <- "1/2/"
                      names(renglon_M2) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_M2)
                      
                      renglon_M3 <- c("Inglés II",
                                      mat_aux[3,num_col_Profesor],
                                      "14 a 16 Ma / 16 a 18 Ma Sesión virtual",
                                      14,##14-16hrs Martes
                                      40,25,"004 (Yelizcalli)","9345","Actuaría","2015",
                                      mat_aux[3,num_col_Semestre:num_col_Turno],
                                      "Segundo Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20181/2017/1235",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_M3[num_col_Cambios] <- "1/2/"
                      names(renglon_M3) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_M3)
                      
                      renglon_M4 <- c("Inglés V",
                                      mat_aux[4,num_col_Profesor],
                                      "14 a 16 Ju / 16 a 18 Ju Sesión virtual",
                                      14,##14-16hrs Jueves
                                      40,36,"004 (Yelizcalli)","9352","Actuaría","2015",
                                      mat_aux[4,num_col_Semestre:num_col_Turno],
                                      "Sexto Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20181/2017/1640",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_M4[num_col_Cambios] <- "1/2/"
                      names(renglon_M4) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_M4)
                    },##Fin 20181
                    '20182' = {
                      #' Las materias "/Inglés II/Inglés V/Inglés VI" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      renglon_1 <- c("Inglés II",
                                     mat_aux[1,num_col_Profesor],
                                     "13 a 15 Lu / 15 a 17	Lu Sesión virtual",
                                     13,##13-15hrs Lunes
                                     45,35,"O219","9353","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Segundo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20182/2017/1235",
                                     mat_aux[1,num_col_Act2000:num_col_NomMat_Act2006],0,
                                     mat_aux[1,num_col_NomMat_CdC1994:num_col_URL_Act2006],0,
                                     mat_aux[1,num_col_URL_CdC1994:num_col_URL_MAp2017])
                      renglon_1[num_col_Cambios] <- "1/2/"
                      names(renglon_1) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_1)
                      
                      renglon_2 <- c("Inglés V",
                                     mat_aux[1,num_col_Profesor],
                                     "13 a 15 Vi / 15 a 17	Vi Sesión virtual",
                                     13,##13-15hrs Viernes
                                     55,48,"O219","9363","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Sexto Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20182/2017/1640",
                                     mat_aux[1,num_col_Act2000:num_col_NomMat_Act2006],0,
                                     mat_aux[1,num_col_NomMat_CdC1994:num_col_URL_Act2006],0,
                                     mat_aux[1,num_col_URL_CdC1994:num_col_URL_MAp2017])
                      renglon_2[num_col_Cambios] <- "1/2/"
                      names(renglon_2) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_2)
                      
                      renglon_3 <- c("Inglés VI",
                                     mat_aux[1,num_col_Profesor],
                                     "13 a 15 Mi / 15 a 17	Mi Sesión virtual",
                                     13,##13-15hrs Lunes
                                     40,34,"004 (Yelizcalli)","9366","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Séptimo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20182/2017/1740",
                                     mat_aux[1,num_col_Act2000:num_col_NomMat_Act2006],0,
                                     mat_aux[1,num_col_NomMat_CdC1994:num_col_URL_Act2006],0,
                                     mat_aux[1,num_col_URL_CdC1994:num_col_URL_MAp2017])
                      renglon_3[num_col_Cambios] <- "1/2/"
                      names(renglon_3) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_3)
                      
                      
                      #' Las materias "/Inglés II/Inglés VI/Inglés II" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      renglon_4 <- c("Inglés II",
                                     mat_aux[2,num_col_Profesor],
                                     "14 a 16 Ma / 16 a 18	Ma Sesión virtual",
                                     14,##14-16hrs Martes
                                     50,48,"P201","9354","Actuaría","2015",
                                     mat_aux[2,num_col_Semestre:num_col_Turno],
                                     "Segundo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20182/2017/1235",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_4[num_col_Cambios] <- "1/2/"
                      names(renglon_4) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_4)
                      
                      renglon_5 <- c("Inglés VI",
                                     mat_aux[2,num_col_Profesor],
                                     "14 a 16 Ju / 16 a 18 Ju Sesión virtual",
                                     14,##14-16hrs Jueves
                                     45,40,"102 (Yelizcalli)","9367","Actuaría","2015",
                                     mat_aux[2,num_col_Semestre:num_col_Turno],
                                     "Séptimo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20182/2017/1740",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_5[num_col_Cambios] <- "1/2/"
                      names(renglon_5) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_5)
                      
                      renglon_6 <- c("Inglés VI",
                                     mat_aux[2,num_col_Profesor],
                                     "13 a 15 Lu / 15 a 17 Lu Sesión virtual",
                                     13,##13-15hrs Jueves
                                     45,27,"P107","9390","Actuaría","2015",
                                     mat_aux[2,num_col_Semestre:num_col_Turno],
                                     "Séptimo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20182/2017/1740",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_6[num_col_Cambios] <- "1/2/"
                      names(renglon_6) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_6)
                      
                      renglon_7 <- c("Inglés II",
                                     mat_aux[2,num_col_Profesor],
                                     "14 a 16 Mi / 16 a 18	Mi Sesión virtual",
                                     14,##14-16hrs Miércoles
                                     45,44,"P201","7095",
                                     "Ciencias de la Computación","2013",
                                     mat_aux[2,num_col_Semestre:num_col_Turno],
                                     "Segundo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20182/1556/1223",
                                     0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_7[num_col_Cambios] <- "1/2/"
                      names(renglon_7) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_7)
                      
                      
                      #' Las materias "/Inglés II/Inglés II/Inglés IV" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      renglon_8 <- c("Inglés II",
                                     mat_aux[3,num_col_Profesor],
                                     "11 a 13 Mi Sesión virtual / 13 a 15 Mi",
                                     13,##13-15hrs Miércoles
                                     45,43,"O219","9355","Actuaría","2015",
                                     mat_aux[3,num_col_Semestre:num_col_Turno],
                                     "Segundo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20182/2017/1235",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_8[num_col_Cambios] <- "1/2/"
                      names(renglon_8) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_8)
                      
                      renglon_9 <- c("Inglés II",
                                     mat_aux[3,num_col_Profesor],
                                     "11 a 13 Vi Sesión virtual / 13 a 15 Vi",
                                     13,##13-15hrs Viernes
                                     45,26,"O213","9357","Actuaría","2015",
                                     mat_aux[3,num_col_Semestre:num_col_Turno],
                                     "Segundo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20182/2017/1235",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_9[num_col_Cambios] <- "1/2/"
                      names(renglon_9) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_9)
                      
                      renglon_10 <- c("Inglés IV",
                                      mat_aux[3,num_col_Profesor],
                                      "11 a 13 Lu Sesión virtual / 13 a 15 Lu",
                                      13,##13-15hrs Lunes
                                      45,43,"O134","9360","Actuaría","2015",
                                      mat_aux[3,num_col_Semestre:num_col_Turno],
                                      "Cuarto Semestre",
                                      "http://www.fciencias.unam.mx/docencia/horarios/20182/2017/1436",
                                      0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_10[num_col_Cambios] <- "1/2/"
                      names(renglon_10) <- param$nom_cols_MG
                      m_grande <- rbind(m_grande,renglon_10)
                    },##Fin 20182
                    '20192' = {
                      #' Las materias "Inglés I/Inglés VI" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      renglon_1 <- c("Inglés I",
                                     mat_aux[1,num_col_Profesor],
                                     "12 a 14 Mi / 18 a 20 Ma Sesión virtual",
                                     12,##12-14hrs Miércoles
                                     15,11,"P204","7088",
                                     "Ciencias de la Computación","2013",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Primer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20192/1556/1124",
                                     0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_1[num_col_Cambios] <- "1/2/"
                      names(renglon_1) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_1)
                      
                      renglon_2 <- c("Inglés VI",
                                     mat_aux[1,num_col_Profesor],
                                     "12 a 14 Ju / 18 a 20 Vi Sesión virtual",
                                     12,##12-14hrs Jueves
                                     20,15,"P109","7099",
                                     "Ciencias de la Computación","2013",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Octavo Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20192/1556/1829",
                                     0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_2[num_col_Cambios] <- "1/2/"
                      names(renglon_2) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_2)
                    },##Fin 20192
                    '20201' = {
                      #' Las materias "Inglés I/Inglés I" son diferentes,
                      #' las clases presenciales se imparten en días distintos.
                      renglon_1 <- c("Inglés I",
                                     mat_aux[1,num_col_Profesor],
                                     "14 a 16	Vi",
                                     14,##14-16hrs Viernes
                                     45,32,"O215","9313","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Primer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20201/2017/1135",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_1[num_col_Cambios] <- "1/2/"
                      names(renglon_1) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_1)
                      
                      renglon_2 <- c("Inglés I",
                                     mat_aux[1,num_col_Profesor],
                                     "14 a 16	Lu",
                                     14,##14-16hrs Lunes
                                     45,29,"O215","9314","Actuaría","2015",
                                     mat_aux[1,num_col_Semestre:num_col_Turno],
                                     "Primer Semestre",
                                     "http://www.fciencias.unam.mx/docencia/horarios/20201/2017/1135",
                                     0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
                      renglon_2[num_col_Cambios] <- "1/2/"
                      names(renglon_2) <- param$nom_cols_MG  
                      m_grande <- rbind(m_grande,renglon_2)
                    },##Fin 20201
             )##Fin de switch(sem_info)
           }else{#En caso de que la matriz no requiera modificaciones
             m_grande <- m_grande_SIN_ING
           }
         },##Fin "Inglés"
         'Francés' = {},##En caso de que algún día haya clases de francés
         'Alemán' = {},##En caso de que algún día haya clases de alemán
         'Mandarín' = {},##En caso de que algún día haya clases de mandarín
  )##Fin de switch(idioma)
  
  save(m_grande, file = paste0("m_grande por semestre/m_grande_",sem_info,".RData"))
  return(m_grande)
}


# actualiza_col_cambios ---------------------------------------------------
#' Title actualiza_col_cambios: Función que se encarga de actualizar la
#' columna "Cambios" de "m_grande".
#'
#' @param m_grande: Matriz de 36 columnas (Materia,Profesor,Horario,
#' horario_num,Lugares,Alumnos,Salon,Grupo,Carrera,Plan,Semestre,Cambios,
#' Turno,Semestre_de_materia,url,Act2000,Act2006,Act2015,CdC1994,CdC2013,
#' Mat1983,MAp2017,NomMat_Act2000,NomMat_Act2006,NomMat_Act2015,
#' NomMat_CdC1994,NomMat_CdC2013,NomMat_Mat1983,NomMat_MAp2017,URL_Act2000,
#' URL_Act2006,URL_Act2015,URL_CdC1994,URL_CdC2013,URL_Mat1983,URL_MAp2017),
#' con la información de "sem_info". Las columnas 16-22 son columnas
#' binarias las cuales indican con un 1 si el grupo del i-ésimo renglón
#' pertenece a la carrera y plan correspondiente al nombre de cada columna,
#' hay un 0 e.o.c. Las columnas 23-29 indican el nombre de las materias
#' dependiendo del plan y carrera al que pertenecen. Las columnas 30-36 
#' indican las URLs dependiendo del plan y carrera al que pertenecen. 
#' @example m_grande[155,] <- c("Álgebra Moderna II",...,0)
#'
#' @return m_grande
#'
actualiza_col_cambios <- function(m_grande){
  num_col_Salon <- arroja_ind_col_MG("Salon")##7
  num_col_Cambios <- arroja_ind_col_MG("Cambios")##12
  num_col_NomMat_Act2000 <- arroja_ind_col_MG("NomMat_Act2000")##23
  num_col_NomMat_MAp2017 <- arroja_ind_col_MG("NomMat_MAp2017")##29
  # col_cambios_aux <- m_grande[,num_col_Cambios]
  
  for(d in 1:dim(m_grande)[1]){
    ##Cambio (3): Se eliminaron los grupos repetidos
    renlon_aux <- m_grande[d,num_col_NomMat_Act2000:num_col_NomMat_MAp2017]
    if(!all(renlon_aux == rep(0,7))){
      m_grande[d,num_col_Cambios] <- paste(m_grande[d,num_col_Cambios],3,
                                           sep = "/")}
    
    ##Cambio (4): Páginas que no tienen información del salón
    if(m_grande[d,num_col_Salon]=="" || is.na(m_grande[d,num_col_Salon])){
      m_grande[d,num_col_Cambios] <- paste(m_grande[d,num_col_Cambios],4,
                                           sep = "/")}
  }#Fin for(d)
  return(m_grande)
}


# gen_m_grande_SIN_Num_Materia --------------------------------------------
#' Title gen_m_grande_SIN_Num_Materia: Función que recibe como parámetro el
#' semestre del que se desea obtener la información y genera un archivo de
#' tipo ".Rdata" con la matriz "m_grande" de dicho semestre. Regresa el
#' nombre de la ubicación en la que se encuentra guardado el archivo.
#' La matriz "m_grande" que se guarda está limpia y actualizada.
#'
#' @param sem_info: Semestre del que se desea obtener información.
#' @param vec_excepciones: Vector que contiene las posibles excepciones que
#' se deben de tomar en cuenta al crear "m_grande".
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#' 
#' @example sem_info <- 20182
#' @example vec_excepciones <- c("Inglés")
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' 
#' @return m_grande_SNM: Matriz de 36 columnas (Materia,Profesor,Horario,
#' horario_num,Lugares,Alumnos,Salon,Grupo,Carrera,Plan,Semestre,Cambios,
#' Turno,Semestre_de_materia,url,Act2000,Act2006,Act2015,CdC1994,CdC2013,
#' Mat1983,MAp2017), con la información de sem_info. Las columnas 16-22
#' son columnas binarias las cuales indican con un 1 si el grupo del
#' i-ésimo renglón pertenece a la carrera y plan correspondiente al
#' nombre de cada columna, hay un 0 e.o.c. Las columnas 23-29 indican
#' el nombre de las materias dependiendo del plan y carrera al que
#' pertenecen. Las columnas 30-36 indican las URLs dependiendo del plan
#' y carrera al que pertenecen.
#'
gen_m_grande_SIN_Num_Materia <- function(sem_info,vec_excepciones,param){
  ##Se definen las variables que se van a utilizar:
  nom_arch_SIN_MOD <- paste0("m_grande por semestre SIN MODIFICAR/m_grande_SIN_MOD_",
                             sem_info,".RData")
  nom_arch_SIN_ING <- paste0("m_grande por semestre SIN INGLES/m_grande_SIN_ING_",
                             sem_info,".RData")
  direccion_info <- paste0("m_grande por semestre SIN NUM MATERIA/m_grande_SNM_",
                           sem_info,".RData")
  
  #' Se hace la revisión por etapas para que se actualice la matriz
  #' hasta el punto necesario.
  if(file.exists(direccion_info)){
    load(direccion_info)
  }else if(!file.exists(direccion_info) && file.exists(nom_arch_SIN_ING)){
    ##En caso de que el archivo de la matriz "m_grande" no exista:
    m_grande <- revisa_gpos_idiomas_1_sem(sem_info,vec_excepciones)
  }else if(!file.exists(nom_arch_SIN_ING) && file.exists(nom_arch_SIN_MOD)){
    ##En caso de que el archivo de la matriz "m_grande_SIN_ING" no exista:
    m_grande <- actualiza_m_grande_1_sem(sem_info,param)
    m_grande <- revisa_gpos_idiomas_1_sem(sem_info,vec_excepciones)
  }else if(!file.exists(nom_arch_SIN_MOD)){
    ##En caso de que el archivo de la matriz "m_grande_SIN_MOD" no exista:
    m_grande <- gen_m_grande_SIN_MOD(sem_info,list_url,param)
    m_grande <- actualiza_m_grande_1_sem(sem_info,param)
    m_grande <- revisa_gpos_idiomas_1_sem(sem_info,vec_excepciones)
  }
  
  #Se actualiza la columna "Cambios"
  m_grande_SNM <- actualiza_col_cambios(m_grande)
  
  save(m_grande_SNM, file = direccion_info)
  return(m_grande_SNM)
}


# gen_vec_nom_materias_total ----------------------------------------------
#' Title gen_vec_nom_materias_total: Función que carga la matriz
#' "m_grande_total" de los semestres 2008-1 a 2020-1 de la cual se va
#' obtiene la lista de nombres de las materias sin repetición, conservando
#' los nombres más recientes de las materias.
#'
#' @return vec_nom_materias_total: Vector que contiene el nombre de las
#' materias sin repetición, conservando los nombres más recientes.
#'
gen_vec_nom_materias_total <- function(){
  #Se definen las variables que se van a utilizar:
  vec_nom_materias_total <- 0
  num_col_Materia <- arroja_ind_col_MG("Materia")##1
  num_col_Act2000 <- arroja_ind_col_MG("Act2000")##16
  num_col_Act2006 <- arroja_ind_col_MG("Act2006")##17
  num_col_Act2015 <- arroja_ind_col_MG("Act2015")##18
  num_col_CdC1994 <- arroja_ind_col_MG("CdC1994")##19
  num_col_CdC2013 <- arroja_ind_col_MG("CdC2013")##20
  num_col_Mat1983 <- arroja_ind_col_MG("Mat1983")##21
  num_col_MAp2017 <- arroja_ind_col_MG("MAp2017")##22
  num_col_NomMat_Act2000 <- arroja_ind_col_MG("NomMat_Act2000")##23
  num_col_NomMat_Act2006 <- arroja_ind_col_MG("NomMat_Act2006")##24
  num_col_NomMat_Act2015 <- arroja_ind_col_MG("NomMat_Act2015")##25
  num_col_NomMat_CdC1994 <- arroja_ind_col_MG("NomMat_CdC1994")##26
  num_col_NomMat_CdC2013 <- arroja_ind_col_MG("NomMat_CdC2013")##27
  num_col_NomMat_Mat1983 <- arroja_ind_col_MG("NomMat_Mat1983")##28
  num_col_NomMat_MAp2017 <- arroja_ind_col_MG("NomMat_MAp2017")##29
  
  #' Se carga la matriz m_grande_total de 2008-1 a 2020-1 de la cual
  #' se va a obtener la lista de nombres que se desea
  load("Matrices m_grande_total/m_grande_total_20081_20201_VECTOR.RData")
  # View(m_grande_total)
  
  for(r in 1:dim(m_grande_total)[1]){#Se recorren los renglones
    renglon <- m_grande_total[r,num_col_Act2000:num_col_MAp2017]
    # cat("\n renglon = ",renglon)
    # print(renglon)
    if(sum(as.numeric(renglon)) == 1){##Se toman las materias que tengan un sólo nombre
      vec_nom_materias_total <- c(vec_nom_materias_total,m_grande_total[r,num_col_Materia])
    }else if(sum(as.numeric(renglon)) > 1){
      ##Se toman las materias que tengan más de un nombre, conservando el más reciente
      # Se reordenan las columnas para que queden los datos ordenados de acuerdo
      #al plan de estudios, del más antiguo al más reciente.
      renglon_aux <- c(m_grande_total[r,num_col_Mat1983],
                       m_grande_total[r,num_col_CdC1994],
                       m_grande_total[r,num_col_Act2000],
                       m_grande_total[r,num_col_Act2006],
                       m_grande_total[r,num_col_CdC2013],
                       m_grande_total[r,num_col_Act2015],
                       m_grande_total[r,num_col_MAp2017])
      reng_aux_nom <- c(m_grande_total[r,num_col_NomMat_Mat1983],
                        m_grande_total[r,num_col_NomMat_CdC1994],
                        m_grande_total[r,num_col_NomMat_Act2000],
                        m_grande_total[r,num_col_NomMat_Act2006],
                        m_grande_total[r,num_col_NomMat_CdC2013],
                        m_grande_total[r,num_col_NomMat_Act2015],
                        m_grande_total[r,num_col_NomMat_MAp2017])
      var_aux <- 0
      for(c in length(renglon_aux):1){
        # cat("\n c = ",c)
        if(var_aux==0 && renglon_aux[c]==1){
          vec_nom_materias_total <- c(vec_nom_materias_total,reng_aux_nom[c])
          var_aux <- 1
        }#if
      }#for c
    }#if else
  }#for r
  
  #Se toman los nombres sin repetición
  vec_nom_materias_total <- unique(vec_nom_materias_total)
  
  #Se quitan los ceros
  vec_nom_materias_total <- vec_nom_materias_total[vec_nom_materias_total!=0]
  save(vec_nom_materias_total, file = "vec_nom_materias_total.RData")
  
  return(vec_nom_materias_total)
}


# agrega_col_num_materia --------------------------------------------------
#' Title agrega_col_num_materia: Función que agrega otra columna a la
#' matriz m_grande con el número del nombre de materia de acuerdo al
#' vector "vec_nom_materias".
#'
#' @param m_grande_SNM: Matriz de 36 columnas (Materia,Profesor,Horario,
#' horario_num,Lugares,Alumnos,Salon,Grupo,Carrera,Plan,Semestre,Cambios,
#' Turno,Semestre_de_materia,url,Act2000,Act2006,Act2015,CdC1994,CdC2013,
#' Mat1983,MAp2017,NomMat_Act2000,NomMat_Act2006,NomMat_Act2015,
#' NomMat_CdC1994,NomMat_CdC2013,NomMat_Mat1983,NomMat_MAp2017,URL_Act2000,
#' URL_Act2006,URL_Act2015,URL_CdC1994,URL_CdC2013,URL_Mat1983,URL_MAp2017),
#' con la información de "sem_info". Las columnas 16-22 son columnas
#' binarias las cuales indican con un 1 si el grupo del i-ésimo renglón
#' pertenece a la carrera y plan correspondiente al nombre de cada columna,
#' hay un 0 e.o.c. Las columnas 23-29 indican el nombre de las materias
#' dependiendo del plan y carrera al que pertenecen. Las columnas 30-36 
#' indican las URLs dependiendo del plan y carrera al que pertenecen.
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#' 
#' @example m_grande_SNM <- c("Cálculo Diferencial e Integral I",...,0)
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
#' @return m_grande: Matriz de 37 columnas (las de "m_grande_SNM" más
#' la columna ), con la información de "sem_info".
#'
agrega_col_num_materia <- function(m_grande_SNM,param){
  ##Se definen las variables que se van a utilizar:
  num_col_Materia <- arroja_ind_col_MG("Materia")##1
  vec_nom_materias_total <- param$vec_nom_materias_total
  m_grande <- matrix(0,ncol = length(param$nom_cols_MG))
  colnames(m_grande) <- param$nom_cols_MG
  
  for(d in 1:length(vec_nom_materias_total)){#Recorre las materias
    if(dim(m_grande_SNM)[1] > 0){
      materia <- vec_nom_materias_total[d]
      ind_materia <- checa_ind_materia(materia,m_grande_SNM)
      if(length(ind_materia) > 0){
        Num_materia <- rep(d,length(ind_materia))
        mat_aux <- cbind(m_grande_SNM[ind_materia,],Num_materia)
        colnames(mat_aux) <- param$nom_cols_MG
        m_grande <- rbind(m_grande,mat_aux)
        
        m_grande_SNM <- m_grande_SNM[-ind_materia,]}}}
  
  ## Se quita el renglón de ceros inicial
  m_grande <- m_grande[m_grande[,num_col_Materia]!=0,]
  # View(m_grande)
  return(m_grande)
}


# gen_m_grande ------------------------------------------------------------
#' Title gen_m_grande: Función que recibe como parámetro el semestre del
#' que se desea obtener la información y genera un archivo de tipo ".Rdata"
#' con la matriz "m_grande" de dicho semestre. Regresa el nombre de la
#' ubicación en la que se encuentra guardado el archivo.
#' La matriz "m_grande" que se guarda está limpia y actualizada.
#' Se agrega la columna con el número del nombre de materia de acuerdo al
#' vector "vec_nom_materias".
#'
#' @param sem_info: Semestre del que se desea obtener información.
#' @param vec_excepciones: Vector que contiene las posibles excepciones que
#' se deben de tomar en cuenta al crear "m_grande".
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#' 
#' @example sem_info <- 20182
#' @example vec_excepciones <- c("Inglés")
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' 
#' @return direccion_info: Nombre de la ubicación en la que se
#' encuentra guardada la matriz "m_grande".
#'
gen_m_grande <- function(sem_info,vec_excepciones,param){
  ##Se definen las variables que se van a utilizar:
  nom_arch_SIN_Num_Mat <- paste0("m_grande por semestre SIN NUM MATERIA/m_grande_SNM_",
                                 sem_info,".RData")
  direccion_info <- paste0("m_grande por semestre/m_grande_",sem_info,".RData")
  
  #' Se hace la revisión por etapas para que se actualice la matriz
  #' hasta el punto necesario.
  if(file.exists(direccion_info)){
    load(direccion_info)
  }else if(!file.exists(direccion_info) && !file.exists(nom_arch_SIN_Num_Mat)){
    # En caso de que el archivo de la matriz "m_grande" no exista ni tampoco
    #el archivo de "m_grande_SNM":
    m_grande_SNM <- gen_m_grande_SIN_Num_Materia(sem_info,vec_excepciones,param)
    m_grande <- agrega_col_num_materia(m_grande_SNM,param)
  }else if(!file.exists(direccion_info) && file.exists(nom_arch_SIN_Num_Mat)){
    ##En caso de que el archivo de la matriz "m_grande" no exista:
    load(nom_arch_SIN_Num_Mat)
    m_grande_SNM <- m_grande
    
    m_grande <- agrega_col_num_materia(m_grande_SNM,param)
  }
  
  save(m_grande, file = direccion_info)
  return(direccion_info)
}


# gen_m_grande_total ------------------------------------------------------
#' Title gen_m_grande_total: Función que genera la matriz "m_grande_total"
#' para un intervalo semestres.
#'
#' @param vec_excepciones: Vector que contiene las posibles excepciones que
#' se deben de tomar en cuenta al crear "m_grande".
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#' 
#' @example vec_excepciones <- c("Inglés")
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return m_grande_total: Matriz de 37 columnas (Materia,Profesor,Horario,
#' horario_num,Lugares,Alumnos,Salon,Grupo,Carrera,Plan,Semestre,Cambios,
#' Turno,Semestre_de_materia,url,Act2000,Act2006,Act2015,CdC1994,CdC2013,
#' Mat1983,MAp2017), con la información de todos los semestres entre "sem_ini"
#' y "sem_fin." Las columnas 16-22 son columnas binarias las cuales indican
#' con un 1 si el grupo del i-ésimo renglón pertenece a la carrera y plan
#' correspondiente al nombre de cada columna, hay un 0 e.o.c. Las columnas
#' 23-29 indican el nombre de las materias dependiendo del plan y carrera
#' al que pertenecen. Las columnas 30-36 indican las URLs dependiendo del
#' plan y carrera al que pertenecen. La columna 37 tiene el número de materia
#' correspondiente al vector de materias.
#'
gen_m_grande_total <- function(vec_excepciones,param){
  ##Se definen las variables que se van a utilizar:
  semestres <- param$Semestres
  
  m_grande_total <- matrix(0,ncol = length(param$nom_cols_MG))
  colnames(m_grande_total) <- param$nom_cols_MG
  num_col_Materia <- arroja_ind_col_MG("Materia")##1
  num_col_sem <- arroja_ind_col_MG("Semestre")##11
  
  for(d in 1:length(semestres)){
    sem_info <- semestres[d]
    nom_archivo <- paste0("m_grande por semestre/m_grande_",sem_info,".RData")
    
    if(!file.exists(nom_archivo)){
      nom_archivo <- gen_m_grande(sem_info,vec_excepciones,param)
    }
    load(nom_archivo)
    m_grande_total <- rbind(m_grande_total,m_grande)
  }
  ## Se quita el renglón de ceros inicial
  m_grande_total <- m_grande_total[m_grande_total[,num_col_Materia]!=0,]
  save(m_grande_total, file = paste0("Matrices m_grande_total/m_grande_total_",
                                     param$sem_ini,"_",param$sem_fin,".RData"))
  # save(m_grande_total, file = paste0("Matrices m_grande_total/m_grande_total_PRUEBA_",
  #                                    semestres[1],"_",sem_fin,".RData"))
  param$m_grande_total = m_grande_total
  
  return(m_grande_total)
}


##########################################################################
##### ÍNDICES #####
#' Funciones que encuentran los índices de las columnas para las diferentes
#' matrices que contienen información.
##########################################################################

# arroja_ind_col_SG -------------------------------------------------------
#' Title arroja_ind_col_SG: Función que recibe el nombre de la columna que
#' se busca y devuelve el número de columna en "mat_simula_grupos" con ese
#' nombre.
#'
#' @param nombre_col: Nombre de la columna de "mat_simula_grupos" de la que
#' se busca conocer la columna en la que se encuentra en dicha matriz.
#' @example nombre_col <- "Grupos_Simulados"
#'
#' @return num_col: Número de columna en "mat_simula_grupos" con nombre
#' "nombre_col".
#' @example num_col <- 3
#'
arroja_ind_col_SG <- function(nombre_col){
  load("mat_def_grupos_simulados.RData")
  dim_mat <- dim(mat_def_grupos_simulados)
  
  for(d in 1:dim_mat[1]){
    if(nombre_col == mat_def_grupos_simulados[d,1]){
      num_col <- mat_def_grupos_simulados[d,2]
    }
  }
  num_col <- as.numeric(num_col)
  
  return(num_col)
}


# arroja_ind_col_RG -------------------------------------------------------
#' Title arroja_ind_col_RG: Función que recibe el nombre de la columna que
#' se busca y devuelve el número de columna en "mat_real_grupos" con ese
#' nombre.
#'
#' @param nombre_col: Nombre de la columna de "mat_real_grupos" de la que
#' se busca conocer la columna en la que se encuentra en dicha matriz.
#' @example nombre_col <- "Alumnos_Reales_Totales"
#'
#' @return num_col: Número de columna en "mat_real_grupos" con nombre
#' "nombre_col".
#' @example num_col <- 4
#'
arroja_ind_col_RG <- function(nombre_col){
  load("mat_def_grupos_reales.RData")
  dim_mat <- dim(mat_def_grupos_reales)
  
  for(d in 1:dim_mat[1]){
    if(nombre_col == mat_def_grupos_reales[d,1]){
      num_col <- mat_def_grupos_reales[d,2]
    }
  }
  num_col <- as.numeric(num_col)
  
  return(num_col)
}


# arroja_ind_col_MG -------------------------------------------------------
#' Title arroja_ind_col_MG: Función que recibe el nombre de la columna que
#' se busca y devuelve el número de columna en m_grande con ese nombre.
#'
#' @param nombre_col: Nombre de la columna de m_grande de la que se busca
#' conocer la columna en la que se encuentra en dicha matriz.
#' @example nombre_col <- "Plan"
#'
#' @return num_col: Número de columna en m_grande con nombre "nombre_col".
#' @example num_col <- 10
#'
arroja_ind_col_MG <- function(nombre_col){
  load("mat_def_columnas_MG.RData")
  dim_mat <- dim(mat_def_columnas_MG)
  
  for(d in 1:dim_mat[1]){
    if(nombre_col == mat_def_columnas_MG[d,1]){
      num_col <- mat_def_columnas_MG[d,2]
    }
  }
  num_col <- as.numeric(num_col)
  
  return(num_col)
}


# checa_ind_materia -------------------------------------------------------
#' Title checa_ind_materia: Función que revisa en qué índices de la matriz
#' coincide "materia" con los nombres que se encuentran en las columnas:
#'  Materia
#'  NomMat_Act2000
#'  NomMat_Act2006
#'  NomMat_Act2015
#'  NomMat_CdC1994
#'	NomMat_CdC2013
#'  NomMat_Mat1983
#'  NomMat_MAp2017
#' Arroja un vector con los índices en los que hay coincidencia para que
#' se cree la matriz con la información necesaria.
#'
#' @param materia: Nombre de algún curso impartido en la FC.
#' @param matriz: Puede ser "m_grande_total" o "m_grande" (de cada semestre)
#' 
#' @example materia <- "Probabilidad I"
#'
#' @return ind_materia: Vector con los índices en los que hay coincidencia
#' entre "materia" y alguna de las columnas correspondientes.
#'
checa_ind_materia <- function(materia,matriz){
  #Se definen las variables que se van a utlizar
  # lista_def_columnas_MG <- param$lista_def_columnas_MG
  # m_grande_total <- param$m_grande_total
  
  num_col_Materia <- arroja_ind_col_MG("Materia")
  num_col_NomMat_Act2000 <- arroja_ind_col_MG("NomMat_Act2000")
  num_col_NomMat_Act2006 <- arroja_ind_col_MG("NomMat_Act2006")
  num_col_NomMat_Act2015 <- arroja_ind_col_MG("NomMat_Act2015")
  num_col_NomMat_CdC1994 <- arroja_ind_col_MG("NomMat_CdC1994")
  num_col_NomMat_CdC2013 <- arroja_ind_col_MG("NomMat_CdC2013")
  num_col_NomMat_Mat1983 <- arroja_ind_col_MG("NomMat_Mat1983")
  num_col_NomMat_MAp2017 <- arroja_ind_col_MG("NomMat_MAp2017")
  
  Materia <- matriz[,num_col_Materia]
  NomMat_Act2000 <- matriz[,num_col_NomMat_Act2000]
  NomMat_Act2006 <- matriz[,num_col_NomMat_Act2006]
  NomMat_Act2015 <- matriz[,num_col_NomMat_Act2015]
  NomMat_CdC1994 <- matriz[,num_col_NomMat_CdC1994]
  NomMat_CdC2013 <- matriz[,num_col_NomMat_CdC2013]
  NomMat_Mat1983 <- matriz[,num_col_NomMat_Mat1983]
  NomMat_MAp2017 <- matriz[,num_col_NomMat_MAp2017]
  
  ind_materia <- 0
  
  for(k in 1:length(Materia)){
    if(materia == Materia[k] ||
       materia == NomMat_Act2000[k] ||
       materia == NomMat_Act2006[k] ||
       materia == NomMat_Act2015[k] ||
       materia == NomMat_CdC1994[k] ||
       materia == NomMat_CdC2013[k] ||
       materia == NomMat_Mat1983[k] ||
       materia == NomMat_MAp2017[k]){
      ind_materia <- c(ind_materia,k)
    }
  }
  ind_materia <- ind_materia[-1]
  
  ## No se pone "if" para evitar que el vector se quede con longitud
  ##cero porque cada materia se encuentra al menos una vez en la
  ##matriz
  
  return(ind_materia)
}


# arroja_num_materia ------------------------------------------------
#' Title arroja_num_materia: Función que recibe el nombre de la
#' materia que se busca y devuelve el número de materia en "vec_nom_materias"
#' con ese nombre.
#'
#' @param nom_materia: Nombre de la materia de "vec_nom_materias" de la
#' que se busca conocer el número de materia.
#' @example materia <- "Estadística I"
#'
#' @return num_col: Número de materia
#' @example num_col <- 42
#'
arroja_num_materia <- function(materia,param){
  vec_nom_materias_total <- param$vec_nom_materias_total
  
  for(d in 1:length(vec_nom_materias_total)){
    if(materia == vec_nom_materias_total[d]){
      num_col <- d
    }
  }
  num_col <- as.numeric(num_col)
  
  return(num_col)
}

##########################################################################
##### SIMULACIÓN #####
#'Funciones encargadas de extraer, estimar y simular el número de alumnos
#'totales y el tamaño de cada grupo.
##########################################################################


# extrae_alumnos_1_materia ----------------------------------------------------------
#' Title: extrae_alumnos_1_materia: Se extrae el número de alumnos por hora y por
#' semestre de "materia".
#' 
#' @param materia: Nombre de la materia de la cual se obtendrá el número de
#' grupos por semestre.
#' @examples materia <- "Probabilidad I"
#' @return mat_alumnos_x_hora_sem: Matriz con 15 renglones (horas) y tantas columnas
#' como semestres de los que se desee obtener la información. Tiene como información
#' el número de alumnos por hora y semestre de "materia".
#' 
extrae_alumnos_1_materia <- function(materia,param){
  ##Se definen las variables que se van a utilizar
  m_grande_total <- param$m_grande_total
  ind_materia <- checa_ind_materia(materia,m_grande_total)
  num_col_Alumnos <- arroja_ind_col_MG("Alumnos")
  Alumnos <- m_grande_total[,num_col_Alumnos]
  num_col_horario_num <- arroja_ind_col_MG("horario_num")
  horario_num <- m_grande_total[,num_col_horario_num]
  num_col_Semestre <- arroja_ind_col_MG("Semestre")
  Semestre <- m_grande_total[,num_col_Semestre]
  
  ##Vector con el número de alumnos de "materia", en tipo "numeric"
  datos_alumnos <- Alumnos[ind_materia]
  
  ##Matriz con 2 columnas: Horas-Semestre de "materia"
  datos_horas_sem <- cbind(horario_num[ind_materia],Semestre[ind_materia])
  ##Matriz con 3 columnas: Horas-Semestre-Número de alumnos de "materia"
  mat_alumnos <- cbind(datos_horas_sem,datos_alumnos)
  ##Se eliminan los renglones repetidos
  mat_num_alumnos <- unique(mat_alumnos)
  colnames(mat_num_alumnos) <- c("Horas","Sem","Alumnos")
  
  mat_alumnos_x_hora_sem <- matrix(0,nrow = length(param$Horas),
                                   ncol = length(param$Semestres))
  rownames(mat_alumnos_x_hora_sem) <- param$nombre_hrs
  colnames(mat_alumnos_x_hora_sem) <- param$nombre_sem
  
  for(j in 1:length(param$Semestres)){
    # cat("\nj = ",j)
    ## Matriz con las columnas Horas-Alumnos con la información
    ##de cada semestre
    datos_x_sem <- mat_num_alumnos[mat_num_alumnos[,2]==param$Semestres[j],
                                   c(1,3)]
    for(k in 1:length(param$Horas)){
      # cat("\nk = ",k)
      if(anyNA(datos_x_sem) || length(datos_x_sem)<2){
        #En caso de que no haya información
        mat_alumnos_x_hora_sem[k,j] <- 0
      }else if(dim(datos_x_sem)[1]>1 && length(datos_x_sem)>2){
        ## Entra al if si se tiene más de un renglón en la matriz
        ##y se tiene más de 2 datos. La segunda condición se pone
        ##para evitar errores cuando la matriz sólo tiene un renglón.
        mat_alumnos_x_hora_sem[k,j] <- sum(as.numeric(datos_x_sem
                                                      [datos_x_sem[,1]==
                                                          param$Horas[k],2]))
        ##Se suma el número de alumnos del semestre por cada hora
      }else if(datos_x_sem[1]==param$Horas[k]){
        #En caso de que hay sólo un renglón con info
        mat_alumnos_x_hora_sem[k,j] <- sum(as.numeric(datos_x_sem[2]))
      }
    }##fin for(k) #Renglones
  }##fin for(j) #Columnas
  # View(mat_alumnos_x_hora_sem)
  return(mat_alumnos_x_hora_sem)
}


# estima_alumnos_1_materia -----------------------------------------------------------
#' Title: estima_alumnos_1_materia: Función que arroja una matriz de 15 renglones (horas)
#' y 3 columnas: cota1, media, cota2; los cuales corresponden a la estimación
#' del número de alumnos que se tendrán en el siguiente semestre por hora.
#' @param mat_alumnos: Matriz con 15 renglones (horas) y tantas columnas
#' como semestres de los que se desee obtener la información. Tiene como
#' información el número de alumnos por hora y semestre de "materia".
#' @example mat_alumnos[5,] <- c(74,39,...,0)
#' 
#' @return mat_alumnos_estimados: matriz de 15 renglones (horas) y 3 columnas:
#' cota1, media, cota2; los cuales corresponden a la estimación del número
#' de alumnos que se tendrán en el siguiente semestre por hora.
estima_alumnos_1_materia <- function(mat_alumnos,param){
  ##Se definen las variables que se van a utilizar
  q1 <- param$q1
  q2 <- param$q2
  mat_alumnos_estimados <- matrix(0,ncol = 3,nrow = length(param$Horas))
  
  ##A cada renglón se le aplica la función HoltWinters, la cual recibe
  ##un objeto de tipo "ts" (time series)
  for(i in 1:length(param$Horas)){
    tsData <- ts(mat_alumnos[i,],frequency = 2)
    # Ajuste hw
    alumnos.fit.q1 <- hw(tsData,h=1,level = q1,seasonal = "additive")
    alumnos.fit.q2 <- hw(tsData,h=1,level = q2,seasonal = "additive")
    
    #En caso de que haya valores negativos en las estimaciones se acota
    #por abajo con cero.
    cota1 <- max(0,alumnos.fit.q1$lower[1])
    media <- max(0,alumnos.fit.q1$mean[1])
    cota2 <- max(0,alumnos.fit.q2$upper[1])
    mat_alumnos_estimados[i,] <- c(cota1,media,cota2)
  }##Fin for(i)
  rownames(mat_alumnos_estimados) <- param$nombre_hrs
  colnames(mat_alumnos_estimados) <- c("Cota1","Media","Cota2")
  # View(mat_alumnos_estimados)???
  
  return(mat_alumnos_estimados)
}


# simula_alumnos_1_materia -----------------------------------------------------------
#' Title: simula_alumnos_1_materia: Se manda llamar la función "estima_alumnos_1_materia" para
#' simular una variable aleatoria discreta en el intervalo de las cotas
#' arrojadas por dicha función. La función simula_alumnos_1_materia regresa un vector
#' con el número total de alumnos simulados por cada hora para el siguiente
#' semestre.
#' @param materia: Nombre de algún curso impartido en la FC
#' @example materia <- "Probabilidad I"
#' 
#' @return vec_alumnos_simulados: Vector con el número total de alumnos
#' simulados por cada hora para el siguiente semestre.
simula_alumnos_1_materia <- function(materia,param){
  ##Extracción de información
  mat_alumnos_x_hora_sem <- extrae_alumnos_1_materia(materia,param)
  
  ##Estimación de alumnos
  mat_alumnos_estimados <- estima_alumnos_1_materia(mat_alumnos_x_hora_sem,param)
  
  ##Simulación
  n_rep <- length(param$Horas)
  vec_alumnos_simulados <- rep(0,n_rep)
  
  for(i in 1:n_rep){
    cota1 <- floor(mat_alumnos_estimados[i,1])
    cota2 <- ceiling(mat_alumnos_estimados[i,3])
    alum_sim <- runif(1,min = cota1,max = cota2)
    vec_alumnos_simulados[i] <- ceiling(alum_sim)
  }
  vec_alumnos_simulados <- matrix(vec_alumnos_simulados,nrow = n_rep)
  rownames(vec_alumnos_simulados) <- param$nombre_hrs
  
  return(vec_alumnos_simulados)
}


# simula_tam_gpo_1_materia -----------------------------------------------------
#' Title simula_tam_gpo_1_materia: Función que simula el tamaño de los grupos por
#' materia en cada hora dependiendo del número de alumnos que se han tenido.
#'
#' @param materia: Nombre de algún curso impartido en la FC
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#' 
#' @example materia <- "Probabilidad I"
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
#' @return sim_tam_gpo_x_hora: Vector que indica el tamaño simulado de cada
#' grupo por hora.
#'
simula_tam_gpo_1_materia <- function(materia,param){
  ##Inicializamos las variables que se van a utilizar
  # lista_def_columnas_MG <- param$lista_def_columnas_MG
  m_grande_total <- param$m_grande_total
  ind_materia <- checa_ind_materia(materia,m_grande_total)
  num_col_horario_num <- arroja_ind_col_MG("horario_num")
  num_col_Alumnos <- arroja_ind_col_MG("Alumnos")
  horario_num <- m_grande_total[,num_col_horario_num]
  Alumnos <- m_grande_total[,num_col_Alumnos]
  
  ##Matriz con 2 columnas: Horas-Número de alumnos de "materia", en tipo "numeric"
  datos_horas_alumnos <- cbind(horario_num[ind_materia],Alumnos[ind_materia])
  colnames(datos_horas_alumnos) <- c("Horas","Alumnos")
  vec_horas <- 7:21
  sim_tam_gpo_x_hora <- rep(0,length(param$nombre_hrs))
  
  for(d in 1:length(param$nombre_hrs)){
    ##Se toman las cantidades > 0 de alumnos por hora
    vec_tam <- datos_horas_alumnos[datos_horas_alumnos[,1]==vec_horas[d],2]
    #' No aplicamos la función unique al vector para respetar la probabilidad
    #' de elegir tamaños de grupo entre más repetidos haya.
    # vec_tam <- unique(vec_tam[vec_tam>0])
    vec_tam <- vec_tam[vec_tam>0]
    if(length(vec_tam) > 1){
      sim_tam_gpo_x_hora[d] <- sample(vec_tam,size = 1)
    }else if(length(vec_tam) == 1){
      ## Cuando el vector sólo tiene una entrada la función sample
      ##toma una muestra aleatoria entre 1 y el número en esa entrada,
      ##es por ello que se separan en casos de acuerdo a la longitud
      ##del vector.
      sim_tam_gpo_x_hora[d] <- vec_tam
    }
  }#Fin for(d)
  
  return(sim_tam_gpo_x_hora)
}


# gen_mat_simula_gpos_1_materia --------------------------------------
#' Title gen_mat_simula_gpos_1_materia: Función que guarda la matriz 
#' "mat_simula_grupos_una_materia" por materia, la cual contiene 24 columnas:
#' Materia, Horario, Número de grupos simulados, Número de alumnos simulados,
#' las últimas 20 columnas indican el número de simulaciones del tamaño de
#' grupo, en sus renglones se tiene el número de alumnos de cada grupo
#' simulado por hora.
#'
#' @param materia: Nombre de algún curso impartido en la FC
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#' 
#' @example materia <- "Probabilidad I"
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
#' @return mat_simula_grupos_una_materia: Matriz con 24 columnas: Materia, Horario,
#' Número de grupos simulados, Número de alumnos simulados, las últimas 20
#' columnas indican el número de simulaciones del tamaño de grupo, en sus
#' renglones se tiene el número de alumnos de cada grupo por hora.
#' 
gen_mat_simula_gpos_1_materia <- function(materia,param){
  # Se obtiene el índice de los renglones de la matriz "m_grande_total"
  #que tienen la información de "materia"
  m_grande_total <- param$m_grande_total
  ind_materia <- checa_ind_materia(materia,m_grande_total)
  
  ##Se define la matriz como data frame
  mat_simula_grupos_una_materia <- data.frame(Materia = 0,Horario = param$nombre_hrs,
                                              Núm.Gpos.Simulados = 0,
                                              Núm.Al.Simulados = 0,Sim_1 = 0,Sim_2 = 0,
                                              Sim_3 = 0,Sim_4 = 0,Sim_5 = 0,Sim_6 = 0,
                                              Sim_7 = 0,Sim_8 = 0,Sim_9 = 0,Sim_10 = 0,
                                              Sim_11 = 0,Sim_12 = 0,Sim_13 = 0,Sim_14 = 0,
                                              Sim_15 = 0,Sim_16 = 0,Sim_17 = 0,Sim_18 = 0,
                                              Sim_19 = 0,Sim_20 = 0)
  
  ## Números de columna de la matriz "mat_simula_grupos_una_materia"
  col_def_Materia <- arroja_ind_col_SG("Materia") ##1
  col_def_Horario <- arroja_ind_col_SG("Horario") ##2
  col_grupos_simulados <- arroja_ind_col_SG("Grupos_Simulados") ##3
  col_alum_sim_total <- arroja_ind_col_SG("Alumnos_Simulados_Totales") ##4
  col_1er_grupo <- arroja_ind_col_SG("col_1er_grupo") ##5
  col_ult_grupo <- arroja_ind_col_SG("col_ult_grupo") ##24
  rango_grupos <- col_1er_grupo:col_ult_grupo
  
  ##Se llenan las columnas: "Materia", "Horario" y "Núm.Al.Simulados" la cual 
  #tiene el número de alumnos simulados para "materia" en cada horario.
  # El número 15 es porque hay 15 horarios en total (7-8,...,21-22)
  vec_renglones <- 1:length(param$Horas)
  mat_simula_grupos_una_materia[vec_renglones,col_def_Materia] <- materia
  # mat_simula_grupos_una_materia[vec_renglones,col_def_Horario] <- param$nombre_hrs
  vec_alumnos_simulados <- simula_alumnos_1_materia(materia,param)
  mat_simula_grupos_una_materia[vec_renglones,col_alum_sim_total] <- vec_alumnos_simulados
  
  ##Se llenan las demás columnas de la matriz "mat_simula_grupos_una_materia",
  #en caso de que si haya habido grupos simulados para "materia"
  if(sum(vec_alumnos_simulados) > 0){
    for(d in rango_grupos){##Se recorren las columnas
      # cat("\n d =",d)
      alto <- 0
      while(alto == 0){
        #Se simula el tamaño de grupo por hora en cada columna
        sim_tam_gpo_x_hora <- simula_tam_gpo_1_materia(materia,param)
        for(j in 1:length(param$Horas)){ ##Se recorren los renglones
          # cat("\nj =",j)
          num_alumnos <- mat_simula_grupos_una_materia[j,col_alum_sim_total]
          
          ##Preguntamos si la suma de los alumnos sigue siendo menor
          #al número de alumnos simulados
          if(sum(mat_simula_grupos_una_materia[j,rango_grupos])<num_alumnos){
            mat_simula_grupos_una_materia[j,d] <- sim_tam_gpo_x_hora[j]
            # cat("\n Entra al primer if")
          }else if(sum(mat_simula_grupos_una_materia[j,rango_grupos])>=num_alumnos
                   || d==col_ult_grupo){
            # cat("\n Entra al segundo if")
            alto <- 1
          }
          # cat("\n alto =",alto)
        }##Fin de for(j) #Renglones
      }##Fin de while
    }##Fin de for(d) #Columnas
    
    ##Se llena la columna con el número de grupos simulados por materia y
    ##se restan tantos alumnos como sean necesarios en el último grupo
    ##para que la suma del número de alumnos por grupo sea igual al número
    ##de alumnos simulados. Después se acomodan de mayor a menor los grupos.
    for(k in 1:length(param$Horas)){
      renglon <- mat_simula_grupos_una_materia[k,rango_grupos]
      num_gpos_sim <- length(renglon[renglon>0])
      mat_simula_grupos_una_materia[k,col_grupos_simulados] <- num_gpos_sim
      num_alum_sim <- mat_simula_grupos_una_materia[k,col_alum_sim_total]
      suma_alum_x_gpo <- sum(mat_simula_grupos_una_materia[k,col_1er_grupo:col_ult_grupo])
      col_ult_gpo_sim <- col_alum_sim_total+num_gpos_sim
      
      mat_simula_grupos_una_materia[k,col_ult_gpo_sim] <- mat_simula_grupos_una_materia[k,col_ult_gpo_sim]-
        (suma_alum_x_gpo-num_alum_sim)
      
      rango <- col_1er_grupo:col_ult_gpo_sim
      mat_simula_grupos_una_materia[k,rango] <- sort(mat_simula_grupos_una_materia[k,rango],
                                                     decreasing = T)
    }
  }else{ #Si no hay grupos simulados de "materia"
    mat_simula_grupos_una_materia <- data.frame(Materia = 0,Horario = param$nombre_hrs,
                                                Núm.Gpos.Simulados = 0,
                                                Núm.Al.Simulados = 0,Sim_1 = 0,Sim_2 = 0,
                                                Sim_3 = 0,Sim_4 = 0,Sim_5 = 0,Sim_6 = 0,
                                                Sim_7 = 0,Sim_8 = 0,Sim_9 = 0,Sim_10 = 0,
                                                Sim_11 = 0,Sim_12 = 0,Sim_13 = 0,Sim_14 = 0,
                                                Sim_15 = 0,Sim_16 = 0,Sim_17 = 0,Sim_18 = 0,
                                                Sim_19 = 0,Sim_20 = 0)
    ##Se llena la columna: "Materia"
    mat_simula_grupos_una_materia[vec_renglones,col_def_Materia] <- materia
  }
  
  # View(mat_simula_grupos_una_materia)
  # nom_archivo <- paste0("mat_simula_grupos por materia 20202/mat_simula_grupos_",materia,".RData")
  # save(mat_simula_grupos_una_materia,file = nom_archivo)
  return(mat_simula_grupos_una_materia)
}


# gen_list_n_sim_1_materia --------------------------------------------------
#' Title gen_list_n_sim_1_materia: Función que hace "n" simulaciones fijando un
#' semestre y una materia; arroja una matriz con el número de alumnos por
#' grupo ordenados de mayor a menor por cada hora. Las "n" matrices generadas
#' se guardan en una lista llamada "lista_mat_n_sim". Cada matriz generada
#' tiene 24 columnas: Materia, Horario, Número total de grupos simulados,
#' Número total de alumos simulados, las siguientes 20 columnas contienen
#' el número de alumnos simulados, ordenados de mayor a menor. Las matrices
#' generadas se guardan en una lista llamada "lista_mat_n_sim".
#'
#' @param materia: Nombre de algún curso impartido en la FC.
#' @param num_materia: Número del índice de "materia" en "vec_nom_materias_total"
#' @param num_sim: Número de matrices simuladas.
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#' 
#' @example materia <- "Probabilidad I"
#' @example num_materia <- 60
#' @example num_sim <- 10
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
#' @return lista_mat_n_sim: Lista que contiene las "n" matrices generadas con
#' el número de alumnos simulados, ordenados de mayor a menor.
#'
gen_list_n_sim_1_materia <- function(materia,num_sim,param){
  #Se definen las variables que se van a utilizar
  sem_sig <- param$sem_sig
  num_materia <- arroja_num_materia(materia,param)
  m_grande_total <- param$m_grande_total
  
  #Se inicia con las n simulaciones
  lista_mat_n_sim <- list()
  nombres_lista <- 0
  for(k in 1:num_sim){
    # cat("\nSimulación número ",k)
    mat_1_sim <- gen_mat_simula_gpos_1_materia(materia,param)
    
    lista_mat_n_sim[[k]] <- mat_1_sim
    nombres_lista <- c(nombres_lista,paste0("mat_",k,"_sim"))
  }
  #Se quita el cero que está al inicio del vector de nombres
  nombres_lista <- nombres_lista[-1]
  names(lista_mat_n_sim) <- nombres_lista
  
  nom_lista_n_sim <- gen_nom_list_n_sim_1_materia(num_sim,num_materia,sem_sig)
  
  save(lista_mat_n_sim,file = nom_lista_n_sim)
  return(lista_mat_n_sim)
}


##########################################################################
##### PRUEBAS RÁPIDAS #####
#'Funciones para las pruebas del modelo para la asignación de horarios.
#'Las pruebas son prácticamente instantáneas.
##########################################################################

# gen_mat_m_filtrada ------------------------------------------------------
#' Title gen_mat_m_filtrada: Función que genera la matriz "m_filtrada" la
#' cual es una submatriz de "m_grande_total". La submatriz contiene la
#' información de "materia" de los semestres correspondientes a los semestres
#' de "vec_sem_sig".
#' Por ejemplo, si
#' vec_sem_sig <- c(20131,20152,20182) y vec_sem_ant <- c(5,5,5) entonces
#' se toma la información desde 2020-2 hasta 2018-1 (16 semestres).
#'SUPONEMOS QUE LA LISTA "param" YA TIENE CARGADA LA MATRIZ "m_grande_total"
#'DE 2008-1 HASTA 2020-1.
#'
#' @param vec_sem_sig: Vector con los semestres de los que se desean obtener
#' las simulaciones. Deben estar ordenados del más antiguo al más reciente.
#' @param vec_sem_ant; Vector con el número de semestres anteriores para
#' cada "sem_sig" del vector "vec_sem_sig"
#' @param materia: Nombre de algún curso impartido en la FC.
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#'
#' @example vec_sem_sig <- c(20131,20152,20182,20201)
#' @example vec_sem_ant <- c(10,15,21,24)##Se inicia en 2008-1
#' @example materia <- "Estadística III"
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' 
#' @return m_filtrada: Submatriz de "m_grande_total" que  contiene la
#' información de "materia" de los semestres correspondientes a la
#' información que se requiere.
#' Por ejemplo:
#' vec_sem_sig <- c(20131,20152,20182) y vec_sem_ant <- c(5,5,5) entonces
#' se toma la información desde 2020-2 hasta 2018-1 (16 semestres).
#' 
gen_mat_m_filtrada <- function(vec_sem_sig,vec_sem_ant,materia,param){
  ##Se definen las variables que se van a utilizar:
  num_col_Materia <- arroja_ind_col_MG("Materia")##1
  num_col_sem <- arroja_ind_col_MG("Semestre")##11
  m_filtrada <- matrix(0,ncol = length(param$nom_cols_MG))
  colnames(m_filtrada) <- param$nom_cols_MG
  
  #' Se carga el archivo para que siempre se cargue la matriz completa
  #' y de ahí se obtengan los datos que se desean
  load(file = paste0("Matrices m_grande_total/m_grande_total_",
                     param$sem_ini,"_",param$sem_fin,".RData"))
  param$m_grande_total = m_grande_total
  MGT <- param$m_grande_total
  
  #Se obtienen los índices de los renglones que corresponden a "materia"
  ind_materia <- checa_ind_materia(materia,MGT)
  
  ### Se define m_filtrada ##
  #Materia:
  mat_aux <- MGT[ind_materia,]
  
  #Semestres:
  for(d in 1:length(param$sem_totales)){
    if(param$sem_totales[d]==vec_sem_sig[1]){
      ind_sem_ini <- d-vec_sem_ant[1]
    }
    
    if(param$sem_totales[d]==vec_sem_sig[length(vec_sem_sig)]){
      ind_sem_fin <- d-1
    }
  }
  vec_sem_aux = param$sem_totales[ind_sem_ini:ind_sem_fin]
  
  for(s in 1:length(vec_sem_aux)){
    mat_aux2 <- mat_aux[mat_aux[,num_col_sem]==vec_sem_aux[s],]
    colnames(mat_aux2) <- param$nom_cols_MG
    m_filtrada <- rbind(m_filtrada,mat_aux2)
  }
  ## Se quita el renglón de ceros inicial
  m_filtrada <- m_filtrada[m_filtrada[,num_col_Materia]!=0,]
  return(m_filtrada)
}


# gen_list_n_sim_1_mat_n_sem ----------------------------------------------
#' Title gen_list_n_sim_1_mat_n_sem: Función utilizada para hacer pruebas
#' rápidas con una sola materia. Genera n simulaciones de varios semestres.
#'
#' @param vec_sem_sig: Vector con los semestres de los que se desean obtener
#' las simulaciones. Deben estar ordenados de más antiguo a más reciente.
#' @param vec_sem_ant; Vector con el número de semestres anteriores para
#' cada "sem_sig" del vector "vec_sem_sig"
#' @param materia: Nombre de algún curso impartido en la FC.
#' @param num_sim: Número de matrices simuladas.
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#'
#' @example vec_sem_sig <- c(20131,20152,20182,20201)
#' @example vec_sem_ant <- c(10,15,21,24)##Se inicia en 2008-1
#' @example materia <- "Estadística III"
#' @example num_sim <- 20
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' 
gen_list_n_sim_1_mat_n_sem <- function(vec_sem_sig,vec_sem_ant,materia,
                                       num_sim,param){
  ##Se define la matriz m_filtrada para que el proceso sea más eficiente
  m_filtrada <- gen_mat_m_filtrada(vec_sem_sig,vec_sem_ant,materia,param)
  param$m_grande_total = m_filtrada
  
  ##Se realiza un ciclo para cada semestre
  for(s in 1:length(vec_sem_sig)){
    # cat("\n s = ",s)
    for(d in 1:length(param$sem_totales)){
      # cat("\n d = ",d)
      if(param$sem_totales[d]==vec_sem_sig[s]){
        ind_sem_ini <- d-vec_sem_ant[s]
        # cat("\n sem_ini = ",param$sem_totales[ind_sem_ini])
        
        ind_sem_fin <- d-1
        # cat("\n sem_fin = ",param$sem_totales[ind_sem_fin])
      }
    }#Fin for(d)
    ##Se definen las variables de param que cambian:
    param$sem_ini = param$sem_totales[ind_sem_ini]
    param$sem_fin = param$sem_totales[ind_sem_fin]
    param$sem_sig = vec_sem_sig[s]
    param$Semestres = param$sem_totales[ind_sem_ini:ind_sem_fin]
    param$nombre_sem = as.character(param$Semestres)
    param$n_semestres_anteriores = length(param$Semestres)
    
    gen_list_n_sim_1_materia(materia,num_sim,param)
  }#Fin for(s)
}


# arroja_error_espacio_en_mat ---------------------------------------------
#' Title arroja_error_espacio_en_mat: Función que indica si se debe arrojar
#' un error o no al momento de revisar si hay espacio suficiente para guardar
#' la información de los grupos reales. Arroja un 1 si el número de columnas
#' para guardar la información es menor al número máximo de grupos reales.
#'
#' @param sem_info: Semestre del que se desea obtener información.
#' @example sem_info <- 20182
#'
#' @return error_1si_0no: Variable binaria la cual vale 1 si el número de
#' columnas para guardar la información es menor al número máximo de grupos
#' reales (i.e. si hay un error).
#'
arroja_error_espacio_en_mat <- function(sem_info){
  ##Se carga la matriz m_grande de "sem_info"
  dir_info <- paste0("mat_real_grupos por semestre/mat_real_grupos_",sem_info,".RData")
  load(dir_info)
  
  ## Números de columna de la matriz "mat_real_grupos"
  col_grupos_reales <- arroja_ind_col_RG("Grupos_Reales") ##3
  col_1er_grupo <- arroja_ind_col_RG("col_1er_grupo") ##5
  col_ult_grupo <- arroja_ind_col_RG("col_ult_grupo") ##24
  
  ##Se definen las variables que se van a utilizar:
  error_1si_0no <- 0
  num_max_gpos <- max(mat_real_grupos[,col_grupos_reales])
  
  if((col_ult_grupo - col_1er_grupo + 1) < num_max_gpos){
    error_1si_0no <- 1
  }
  
  return(error_1si_0no)
}


# gen_mat_real_grupos_una_materia ----------------------------------------
#' Title gen_mat_real_grupos_una_materia: Función que guarda la matriz 
#' "mat_real_grupos_una_materia" la cual contiene 24 columnas: Materia,
#' Horario, Número de grupos reales, Número de alumnos reales, las últimas
#' 20 columnas indican el número de simulaciones del tamaño de grupo, en sus
#' renglones se tiene el número real de alumnos de cada grupo por hora.
#' Fijando materia y semestre
#'
#' @param materia: Nombre de algún curso impartido en la FC.
#' @param sem_info: Semestre del que se desea obtener información
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#' @example materia <- "Probabilidad I"
#' @example sem_info <- 20182
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return mat_real_grupos_una_materia: Matriz con 24 columnas: Materia, Horario,
#' Número de grupos reales, Número de alumnos reales, las últimas 20
#' columnas indican el número de simulaciones del tamaño de grupo, en sus
#' renglones se tiene el número real de alumnos de cada grupo por hora.
#' 
gen_mat_real_grupos_una_materia <- function(materia,sem_info,param){
  error_1si_0no <- arroja_error_espacio_en_mat(sem_info)
  
  if(error_1si_0no == 0){
    ##Se define el número de columna para "Materia", "Horario" y "Núm. de alumnos"
    num_col_Materia <- arroja_ind_col_MG("Materia")
    num_col_horario_num <- arroja_ind_col_MG("horario_num")
    num_col_alum <- arroja_ind_col_MG("Alumnos")
    
    ##Se carga la matriz m_grande de "sem_info"
    direccion_info <- paste0("m_grande por semestre/m_grande_",sem_info,".RData")
    load(direccion_info)
    
    ##Se define la matriz como data frame para agregar renglones más fácilmente.
    mat_real_grupos_una_materia <- data.frame(Materia = 0,Horario = 1:15,
                                              Núm.Gpos.Reales = 0,Núm.Al.Reales = 0,Grupo_1 = 0,
                                              Grupo_2 = 0,Grupo_3 = 0,Grupo_4 = 0,Grupo_5 = 0,
                                              Grupo_6 = 0,Grupo_7 = 0,Grupo_8 = 0,Grupo_9 = 0,
                                              Grupo_10 = 0,Grupo_11 = 0,Grupo_12 = 0,Grupo_13 = 0,
                                              Grupo_14 = 0,Grupo_15 = 0,Grupo_16 = 0,Grupo_17 = 0,
                                              Grupo_18 = 0,Grupo_19 = 0,Grupo_20 = 0)
    
    ## Números de columna de la matriz "mat_real_grupos_una_materia"
    col_def_Materia <- arroja_ind_col_RG("Materia") ##1
    col_def_Horario <- arroja_ind_col_RG("Horario") ##2
    col_grupos_reales <- arroja_ind_col_RG("Grupos_Reales") ##3
    col_alum_real_total <- arroja_ind_col_RG("Alumnos_Reales_Totales") ##4
    col_1er_grupo <- arroja_ind_col_RG("col_1er_grupo") ##5
    col_ult_grupo <- arroja_ind_col_RG("col_ult_grupo") ##24
    rango_grupos <- col_1er_grupo:col_ult_grupo
    
    ##Se llena la columna auxiliar de horario:
    col_aux_hora <- 7:21
    
    ##Se llenan las columnas: "Materia" y "Horario"
    # El número 15 es porque hay 15 horarios en total (7-8,...,21-22)
    mat_real_grupos_una_materia[1:15,col_def_Materia] <- materia
    mat_real_grupos_una_materia[1:15,col_def_Horario] <- param$nombre_hrs
    
    ##Se llena la información de los grupos
    ind_materia <- checa_ind_materia(materia,m_grande)
    
    if(length(ind_materia) != 0){#Si hay grupos de "materia" en "sem_info"
      mat_materia <- m_grande[ind_materia,]
      for(d in 1:dim(mat_real_grupos_una_materia)[1]){##Recorre los renglones de "mat_real_grupos"
        # cat("\nd = ",d)
        for(h in 7:21){##Recorre las horas
          # cat("\n   h = ",h)
          mat_horario <- mat_materia[mat_materia[,num_col_horario_num]==h,]
          if(length(mat_horario)>=dim(m_grande)[2] && dim(mat_horario)[1] !=0){
            for(j in 1:dim(mat_horario)[1]){
              # cat("\n      j = ",j)
              if(mat_horario[j,num_col_horario_num] == col_aux_hora[d]){
                mat_real_grupos_una_materia[d,col_grupos_reales] <- dim(mat_horario)[1]
                mat_real_grupos_una_materia[d,(col_1er_grupo+j-1)] <- mat_horario[j,num_col_alum]
              }
            }##Fin de for(j)
          }
        }##Fin de for(h)
      }##Fin de for(d)
      
      ##Se llena la columna con el número de alumnos totales y se ordenan de
      ##mayor a menor los grupos con respecto al número de alumnos que se tienen
      for(k in 1:dim(mat_real_grupos_una_materia)[1]){
        renglon <- mat_real_grupos_una_materia[k,rango_grupos]
        mat_real_grupos_una_materia[k,col_alum_real_total] <- sum(renglon)
        
        # col_ult_gpo_sim <- col_alum_real_total+mat_real_grupos_una_materia[k,col_grupos_reales]
        # rango <- col_1er_grupo:col_ult_gpo_sim
        mat_real_grupos_una_materia[k,rango_grupos] <- sort(mat_real_grupos_una_materia[k,rango_grupos],decreasing = T)
      }
      # View(mat_real_grupos_una_materia)
    }else{#Si no hay grupos de "materia" en "sem_info"
      mat_real_grupos_una_materia <- data.frame(Materia = 0,Horario = 1:15,
                                                Núm.Gpos.Reales = 0,Núm.Al.Reales = 0,Grupo_1 = 0,
                                                Grupo_2 = 0,Grupo_3 = 0,Grupo_4 = 0,Grupo_5 = 0,
                                                Grupo_6 = 0,Grupo_7 = 0,Grupo_8 = 0,Grupo_9 = 0,
                                                Grupo_10 = 0,Grupo_11 = 0,Grupo_12 = 0,Grupo_13 = 0,
                                                Grupo_14 = 0,Grupo_15 = 0,Grupo_16 = 0,Grupo_17 = 0,
                                                Grupo_18 = 0,Grupo_19 = 0,Grupo_20 = 0)
      ##Se llenan las columnas: "Materia" y "Horario"
      # El número 15 es porque hay 15 horarios en total (7-8,...,21-22)
      mat_real_grupos_una_materia[1:15,col_def_Materia] <- materia
      mat_real_grupos_una_materia[1:15,col_def_Horario] <- param$nombre_hrs
    }
    return(mat_real_grupos_una_materia)
  }else{
    cat("\n ***ERROR*** \nEl espacio para guardar la información es insuficiente")
  }
}


# gen_mat_esp_alum_x_materia_1_sem ------------------------------------------
#' Title gen_mat_esp_alum_x_materia_1_sem: Función que genera una matriz con
#' 20 columnas y 15 renglones que contiene la esperanza por cada entrada de
#' las "n" matrices generadas en la función "gen_mat_n_sim_1_sem".
#'
#' @param lista_mat_n_sim: Lista que contiene las matrices generadas con
#' el número de alumnos simulados, ordenados de mayor a menor.
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#' 
#' @example lista_mat_n_sim[[5]][1,] <- c(12-13,54,32,31,6,1,0,...,0)
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
#' @return mat_esp_alum_x_materia_1_sem: Matriz de 15 renglones (horas) y
#' 20 columnas que contiene la esperanza por cada entrada de las "n" matrices
#' generadas en la función "gen_mat_n_sim_1_sem".
#'
gen_mat_esp_alum_x_materia_1_sem <- function(lista_mat_n_sim,param){
  n_sim <- length(names(lista_mat_n_sim))
  mat_esp_alum_x_materia_1_sem <- matrix(0,nrow = 15,ncol = 20)
  lista_aux <- list()
  num_col_Horario <- arroja_ind_col_SG("Horario")
  num_col_1er_grupo <- arroja_ind_col_SG("col_1er_grupo")
  num_col_ult_grupo <- arroja_ind_col_SG("col_ult_grupo")
  
  for(k in 1:n_sim){
    lista_aux[[k]] <- lista_mat_n_sim[[k]][,c(num_col_Horario,
                                              num_col_1er_grupo:num_col_ult_grupo)]
  }
  
  vec_info <- list()
  for(c in 2:21){
    # cat("\n c = ",c)
    for(r in 1:15){
      # cat("\n r = ",r)
      for(i in 1:n_sim){
        ##Se toma la entrada (r,c) del i-ésimo elemento de "lista_aux"
        vec_info[i] <- lista_aux[[i]][r,c]
      }
      mat_esp_alum_x_materia_1_sem[r,(c-1)] <- mean(sapply(vec_info, mean))
    }
  }
  rownames(mat_esp_alum_x_materia_1_sem) <- param$nombre_hrs
  colnames(mat_esp_alum_x_materia_1_sem) <- c("E[Gpo_1]","E[Gpo_2]","E[Gpo_3]",
                                              "E[Gpo_4]","E[Gpo_5]","E[Gpo_6]",
                                              "E[Gpo_7]","E[Gpo_8]","E[Gpo_9]",
                                              "E[Gpo_10]","E[Gpo_11]","E[Gpo_12]",
                                              "E[Gpo_13]","E[Gpo_14]","E[Gpo_15]",
                                              "E[Gpo_16]","E[Gpo_17]","E[Gpo_18]",
                                              "E[Gpo_19]","E[Gpo_20]")
  
  # save(mat_esp_alum_x_materia_1_sem,file = "mat_esp_alum_x_materia_1_sem_Modelos de Supervivencia y de Series de Tiempo.RData")
  return(mat_esp_alum_x_materia_1_sem)
}


# gen_mat_dif_alum_x_materia_1_sem ----------------------------------------------------
#' Title gen_mat_dif_alum_x_materia_1_sem: Función que genera una matriz con 20 columnas
#' y 15 renglones que contiene la diferencia entre los valores reales menos
#' la esperanza, para cada entrada, fijando materia y semestre.
#'
#' @param mat_esp_alum_x_materia_1_sem: Matriz de 15 renglones (horas) y
#' 20 columnas que contiene la esperanza por cada entrada de las "n" matrices
#' generadas en la función "gen_mat_n_sim_1_sem".
#' @example mat_esp_alum_x_materia_1_sem[6,] <- c(121.0,0.2,3.2,6.8,1.8,0,...,0)
#'
#' @return mat_dif_alum_x_materia_1_sem: Matriz de 15 renglones (horas) y 20 columnas
#' que contiene la diferencia entre los valores reales menos la esperanza,
#' para cada entrada.
#'
gen_mat_dif_alum_x_materia_1_sem <- function(mat_esp_alum_x_materia_1_sem,
                                             mat_real_grupos_una_materia){
  mat_dif_alum_x_materia_1_sem <- matrix(0,nrow = 15,ncol = 20)
  col_1er_grupo_real <- arroja_ind_col_RG("col_1er_grupo") ##5
  col_ult_grupo_real <- arroja_ind_col_RG("col_ult_grupo") ##24
  mat_real_aux <- mat_real_grupos_una_materia[,col_1er_grupo_real:col_ult_grupo_real]
  
  for(c in 1:dim(mat_dif_alum_x_materia_1_sem)[2]){
    # cat("\n c = ",c)
    for(r in 1:dim(mat_dif_alum_x_materia_1_sem)[1]){
      # cat("\n r = ",r)
      mat_dif_alum_x_materia_1_sem[r,c] <- mat_real_aux[r,c] - mat_esp_alum_x_materia_1_sem[r,c]
    }
  }
  rownames(mat_dif_alum_x_materia_1_sem) <- param$nombre_hrs
  colnames(mat_dif_alum_x_materia_1_sem) <- c("dif_Gpo_1","dif_Gpo_2","dif_Gpo_3",
                                              "dif_Gpo_4","dif_Gpo_5","dif_Gpo_6",
                                              "dif_Gpo_7","dif_Gpo_8","dif_Gpo_9",
                                              "dif_Gpo_10","dif_Gpo_11","dif_Gpo_12",
                                              "dif_Gpo_13","dif_Gpo_14","dif_Gpo_15",
                                              "dif_Gpo_16","dif_Gpo_17","dif_Gpo_18",
                                              "dif_Gpo_19","dif_Gpo_20")
  
  # View(mat_dif_alum_x_materia_1_sem)
  # save(mat_dif_alum_x_materia_1_sem,file = "mat_dif_alum_x_materia_1_sem_Modelos de Supervivencia y de Series de Tiempo.RData")
  return(mat_dif_alum_x_materia_1_sem)
}


# gen_mat_var_alum_x_materia_1_sem ------------------------------------------
#' Title gen_mat_var_alum_x_materia_1_sem: Función que genera una matriz con
#' 20 columnas que contiene la esperanza por cada entrada de las "n" matrices
#' generadas en la función "gen_mat_n_sim_1_sem".
#'
#' @param lista_mat_n_sim: Lista que contiene las matrices generadas con
#' el número de alumnos simulados, ordenados de mayor a menor.
#' @example lista_mat_n_sim[[5]][1,] <- c(12-13,54,32,31,6,1,0,...,0)
#'
#' @return mat_var_alum_x_materia_1_sem: Matriz de 15 renglones (horas) y
#' 20 columnas que contiene la esperanza por cada entrada de las "n" matrices
#' generadas en la función "gen_mat_n_sim_1_sem".
#'
gen_mat_var_alum_x_materia_1_sem <- function(lista_mat_n_sim,param){
  #Se definen las variables que se van a utilizar
  n_sim <- length(names(lista_mat_n_sim))
  mat_var_alum_x_materia_1_sem <- matrix(0,nrow = 15,ncol = 20)
  lista_aux <- list()
  num_col_Horario <- arroja_ind_col_SG("Horario")
  num_col_1er_grupo <- arroja_ind_col_SG("col_1er_grupo")
  num_col_ult_grupo <- arroja_ind_col_SG("col_ult_grupo")
  
  for(k in 1:n_sim){
    lista_aux[[k]] <- lista_mat_n_sim[[k]][,c(num_col_Horario,
                                              num_col_1er_grupo:num_col_ult_grupo)]
  }
  
  vec_info <- list()
  for(c in 2:21){
    # cat("\n c = ",c)
    for(r in 1:15){
      # cat("\n r = ",r)
      for(d in 1:n_sim){
        vec_info[d] <- lista_aux[[d]][r,c]
      }
      mat_var_alum_x_materia_1_sem[r,(c-1)] <- var(sapply(vec_info, mean))
    }
  }
  rownames(mat_var_alum_x_materia_1_sem) <- param$nombre_hrs
  colnames(mat_var_alum_x_materia_1_sem) <- c("Var(Gpo_1)","Var(Gpo_2)",
                                              "Var(Gpo_3)","Var(Gpo_4)",
                                              "Var(Gpo_5)","Var(Gpo_6)",
                                              "Var(Gpo_7)","Var(Gpo_8)",
                                              "Var(Gpo_9)","Var(Gpo_10)",
                                              "Var(Gpo_11)","Var(Gpo_12)",
                                              "Var(Gpo_13)","Var(Gpo_14)",
                                              "Var(Gpo_15)","Var(Gpo_16)",
                                              "Var(Gpo_17)","Var(Gpo_18)",
                                              "Var(Gpo_19)","Var(Gpo_20)")
  
  # save(mat_var_alum_x_materia_1_sem,file = "mat_var_alum_x_materia_1_sem_Estadística III.RData")
  # View(mat_var_alum_x_materia_1_sem)
  return(mat_var_alum_x_materia_1_sem)
}


# gen_vec_suma_datos_real_1_sem ------------------------------------------------
#' Title gen_vec_suma_datos_real_1_sem: Función en la que se genera el vector
#' "vec_suma_x_sem_real" el cual contiene la suma de los datos reales
#' por hora y por materia de 1 semestre.
#'
#' @param mat_real_grupos: Matriz con 24 columnas: Materia, Horario,
#' Número de grupos reales, Número de alumnos reales, las últimas 20
#' columnas indican el número de simulaciones del tamaño de grupo, en sus
#' renglones se tiene el número real de alumnos de cada grupo por hora.
#' @param nom_col: Nombre de la columna de datos que se requieren
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#' 
#' @example mat_real_grupos[1,] <- c("Probabilidad I","9-10",15,150,50,...,0)
#' @example nom_col <- "Alumnos_Reales_Totales"
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
#' @return vec_suma_x_sem_real: Vector que contiene la suma de los datos
#' reales por hora y por materia de 1 semestre.
#'
gen_vec_suma_datos_real_1_sem <- function(mat_real_grupos,nom_col,param){
  #Se definen las variables que se van a utilizar
  horas <- param$nombre_hrs
  num_col_horario <- arroja_ind_col_RG("Horario") ##2
  num_col <- arroja_ind_col_RG(nom_col) ##4
  vec_suma_x_sem_real <- matrix(0,nrow = 15,ncol = 1)
  
  for(h in 1:length(horas)){#Recorre los horarios
    vec_aux <- mat_real_grupos[mat_real_grupos[,num_col_horario]==horas[h],num_col]
    # vec_suma_x_sem_real[h] <- sum(vec_aux)
    vec_suma_x_sem_real[h] <- mean(vec_aux)
  }
  
  return(vec_suma_x_sem_real)
}


# gen_vec_esp_datos_sim_1_sem ------------------------------------------------
#' Title gen_vec_esp_datos_sim_1_sem: Función en la que se genera el vector
#' "vec_suma_x_sem" el cual contiene la suma de los datos de las simulaciones
#' por hora y por materia de 1 semestre ya sea del número total de grupos
#' o del número total de alumnos.
#'
#' @param lista_n_sim_por_sem: Lista que contiene las listas de "n" simulaciones
#' por cada materia obtenidas de la función "gen_mat_n_sim_1_sem" 
#' @param nom_col: Nombre de la columna de la que se desea obtener la información.
#' 
#' @example lista_n_sim_por_sem: 
#' @example nom_col <- "Alumnos_Simulados_Totales"
#'
#' @return vec_suma_x_sem: Vector que contiene la suma de los datos de las
#' simulaciones por hora y por materia de 1 semestre.
#'
gen_vec_esp_datos_sim_1_sem <- function(lista_n_sim_por_sem,nom_col){
  #Se definen las variables que se van a utilizar
  num_col <- arroja_ind_col_SG(nom_col)
  vec_esp_x_sem <- matrix(0,nrow = 15,ncol = 1)
  
  for(m in 1:length(lista_n_sim_por_sem)){#Recorre las materias
    lista_aux <- list()
    lista_aux <- lista_n_sim_por_sem[[m]]##Lista con "n" simulaciones
    mat_aux <- matrix(0,nrow = 15,ncol = length(lista_aux))
    vec_info <- matrix(0,nrow = 15,ncol = 1)
    for(i in 1:length(lista_aux)){
      mat_aux[,i] <- lista_aux[[i]][,num_col]
    }##Fin for i
    
    ##Se obtiene la esperanza de los datos de las "n" simulaciones por hora
    vec_info <- rowMeans(mat_aux)
    vec_esp_x_sem <- vec_esp_x_sem + vec_info
    # cat("\n vec_esp_x_sem: ",vec_esp_x_sem)
  }##Fin for m
  
  # vec_esp_x_sem <- round(vec_esp_x_sem)### Verificar se usa "ceiling" o "round"
  vec_esp_x_sem <- ceiling(vec_esp_x_sem)### Se debe usar "ceiling"
  return(vec_esp_x_sem)
}



# gen_mat_dif_relativas ---------------------------------------------------
#' Title gen_mat_dif_relativas: Función que recibe dos matrices, una
#' matriz de diferencias absolutas y una matriz de valores reales, arroja
#' una matriz con las diferencias relativas correspondientes y guarda la
#' gráfica "heatmap" de ella.
#'
#' @param mat_dif_abs: Matriz de diferencias absolutas entre datos reales
#' y la esperanza de los valores simulados.
#' @param mat_real: Matriz con valores reales por semestre.
#' @param nom_archivo : Nombre del archivo con el que se va a guardar la
#' gráfica "heatmap" de la matriz "mat_dif_relativas".
#'
#' @example mat_dif_abs[5,] <- c(2103,2372,...,2122,4204)
#' @example mat_real[5,] <- c(3568,3791,...,3395,5576)
#' @example nom_archivo <- "dif_relativa_total_de_alumnos_x_sem"
#'
#' @return mat_dif_relativas: Matriz con las diferencias relativas
#' correspondientes a las matrices que se pasan como parámetro
#'
gen_mat_dif_relativas <- function(mat_dif_abs,mat_real,nom_archivo){
  # mat_dif_relativas <- mat_dif_abs/mat_real
  mat_dif_relativas <- matrix(0,nrow = nrow(mat_dif_abs),
                              ncol = ncol(mat_dif_abs))
  
  for(c in 1:dim(mat_dif_abs)[2]){#Recorre columnas
    for(r in 1:dim(mat_dif_abs)[1]){#Recorre renglones
      if(mat_real[r,c]==0){
        #' En caso de que haya cero en la entrada (r,c) de la matriz de
        #' datos reales
        mat_dif_relativas[r,c] <- 0
      }else{
        mat_dif_relativas[r,c] <- mat_dif_abs[r,c]/mat_real[r,c]}}}
  
  ## Para guardar gráficas de R como imagen .jpeg se manda llamar la función
  ## jpeg() antes de generar una gráfica. Al hacer esto, le indicamos a R
  ##que en lugar de mandar nuestro gráfico a una ventana del escritorio, lo
  ##mande a un dispositivo gráfico distinto.
  ## La función dev.off(), se utiliza para cerrar el dispositivo gráfico
  ##elegido y así poder crear más gráficos después.
  nombre_archivo <- paste0("Figuras/Matrices Simuladas/heatmap_",nom_archivo,".jpeg")
  jpeg(filename = nombre_archivo, width = 800, height = 700)
  colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
  heatmap(mat_dif_relativas, Colv = NA, Rowv = NA, scale="none",
          col=colMain,main = nom_archivo)
  dev.off()
  
  nom_arch_matriz <- paste0("mat_dif_relativas/",nom_archivo,".RData")
  save(mat_dif_relativas,file = nom_arch_matriz)
  
  return(mat_dif_relativas)
}


# gen_vec_esp_dat_sim_1_sem_1_materia ------------------------------------------------
#' Title gen_vec_esp_dat_sim_1_sem_1_materia: Función en la que se genera el vector
#' "vec_suma_x_sem" el cual contiene la suma de los datos de las simulaciones
#' por hora y por materia de 1 semestre ya sea del número total de grupos
#' o del número total de alumnos.
#'
#' @param lista_mat_n_sim: Lista que contiene las "n" simulaciones de una materia.
#' @param nom_col: Nombre de la columna de la que se desea obtener la información.
#' 
#' @example lista_mat_n_sim: 
#' @example nom_col <- "Alumnos_Simulados_Totales" o "Grupos_Simulados"
#'
#' @return vec_suma_x_sem: Vector que contiene la suma de los datos de las
#' simulaciones por hora de 1 materia y 1 semestre.
#'
gen_vec_esp_dat_sim_1_sem_1_materia <- function(lista_mat_n_sim,nom_col){
  #Se definen las variables que se van a utilizar
  num_col <- arroja_ind_col_SG(nom_col)
  vec_esp_x_sem <- matrix(0,nrow = 15,ncol = 1)
  
  mat_aux <- matrix(0,nrow = 15,ncol = length(lista_mat_n_sim))
  vec_info <- matrix(0,nrow = 15,ncol = 1)
  for(i in 1:length(lista_mat_n_sim)){
    mat_aux[,i] <- lista_mat_n_sim[[i]][,num_col]
  }##Fin for i
  
  ##Se obtiene la esperanza de los datos de las "n" simulaciones por hora
  vec_info <- rowMeans(mat_aux)
  vec_esp_x_sem <- vec_esp_x_sem + vec_info
  # cat("\n vec_esp_x_sem: ",vec_esp_x_sem)
  vec_esp_x_sem <- ceiling(vec_esp_x_sem)### Se debe usar "ceiling"
  return(vec_esp_x_sem)
}



# pruebas_aleatorias_1_materia --------------------------------------------
#' Title pruebas_aleatorias_1_materia: Función que manda a llamar a la
#' función encargada de realizar las pruebas rápidas para una materia.
#'
#' @param materia: Nombre de algún curso impartido en la FC.
#' @param num_sem_ant: Número de semestres de información se quieren para
#' la simulación.
#' @param num_sim: Número de matrices simuladas.
#' @param num_seg_sig: Número de semestres de los que se van a obtener las
#' simulaciones.
#' @param cont_1si_0no: Variable binaria que vale 1 si los semestres a 
#' simular son continuos y 0 sino.
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#' 
#' @example materia <- "Estadística III"
#' @example num_sem_ant <- 5
#' @example num_sim <- 10
#' @example num_seg_sig <- 3
#' @example cont_1si_0no <- 1 ó 0
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' 
pruebas_aleatorias_1_materia <- function(materia,num_sem_ant,num_sim,
                                         num_seg_sig,cont_1si_0no,param){
  #Se definen las variables que se van a utilizar
  vec_sem_aux <- param$sem_totales[15:(length(param$sem_totales)-1)]#20151 - 20201
  if(cont_1si_0no == 1){
    vec_ind_aux <- 1:(length(vec_sem_aux)-num_seg_sig)
    ind_1 <- sample(vec_ind_aux,1)
    vec_sem_sig <- vec_sem_aux[ind_1:(ind_1+num_seg_sig-1)]
  }else{
    vec_sem_sig <- sort(sample(vec_sem_aux,num_seg_sig))
  }
  print(vec_sem_sig)
  vec_sem_ant <- rep(num_sem_ant,length(vec_sem_sig))
  guarda_heatmap_1_materia_n_sem(vec_sem_sig,vec_sem_ant,materia,num_sim,param)
}



# pruebas_aleatorias ------------------------------------------------------
#' Title pruebas_aleatorias: Función que manda a llamar "n_materias" veces
#' a la función "pruebas_aleatorias_1_materia".
#'
#' @param n_materias: Número de materias de las cuales se van a obtener las
#' pruebas rápidas.
#' @param materias_con_error: Vector con los índices de las materias que
#' tienen algún error, por el momento no se toman en cuenta ya que se quieren
#' obtener resultados rápidos y no detenerse en los casos especiales. Se notó
#' que las materias con error son optativas y casi sin alumnos.
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#' 
#' @example n_materias <- 5
#' @example materias_con_error <- c(127,168,195,198,211,215,216,223,237,253)
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total) 
#'
pruebas_aleatorias <- function(n_materias,materias_con_error,param){
  #Se definen las variables que se van a utilizar
  vec_aux <- 1:length(param$vec_nom_materias_total)
  vec_ind_aux <- vec_aux[-materias_con_error]
  ind_materias <- sample(vec_ind_aux,n_materias)
  
  i <- 1
  for(d in ind_materias){
    cat("\nPRUEBA: ",i)
    # Start the clock!
    ptm <- proc.time()
    #Se definen las variables aleatorias que se van a utilizar
    num_sem_ant <- sample(5:10,1)
    num_sim <- sample(10:15,1)
    num_seg_sig <- sample(3:5,1)
    cont_1si_0no <- sample(0:1,1)
    
    materia <- param$vec_nom_materias_total[d]
    cat("\n Materia: ",d," = ",materia)
    pruebas_aleatorias_1_materia(materia,num_sem_ant,num_sim,
                                 num_seg_sig,cont_1si_0no,param)
    
    #Se imprime la información
    cat("\nSe probaron ",1," materias")
    cat("\n",num_seg_sig," semestres")
    cat("\n",num_sem_ant," semestres de información")
    cat("\n CON m_filtrada")
    cat("\n",num_sim," simulaciones por materia")
    
    # Stop the clock
    minutos <- (proc.time()-ptm)[3]/60
    cat("\nEl proceso tardó: ",minutos," minutos\n")
    
    i <- i + 1
  }
  
  # #Se imprime la información
  # cat("\nSe probaron ",n_materias," materias")
  # cat("\n",num_seg_sig," semestres")
  # cat("\n",num_sem_ant," semestres de información")
  # cat("\n CON m_filtrada")
  # cat("\n",num_sim," simulaciones por materia")
  # 
  # # Stop the clock
  # minutos <- (proc.time()-ptm)[3]/60
  # cat("\nEl proceso tardó: ",minutos," minutos\n")
}




##########################################################################
##### HEATMAP #####
#'Funciones encargadas de graficar las matrices con heatmaps y guardar
#'las imágenes.
##########################################################################

# guarda_una_fig_heatmap --------------------------------------------------
#' Title guarda_una_fig_heatmap: Función que genera una imagen de tipo "jpeg"
#' de las gráficas de heatmap para las matrices de datos generadas.
#'
#' @param matriz: Matriz de la cual se obtiene la gráfica de tipo heatmap
#' y se guarda la imagen "jpeg". La matriz de datos puede contener la 
#' diferencia entre los datos reales menos la esperanza de los simulados,
#' la varianza de los datos simulados, ...
#' @param num_materia: Índice de la materia de la cual se quiere obtener
#' la gráfica de tipo heatmap, del vector "Materias".
#' @param nom_archivo: Nombre con el que se guardará la imagen "jpeg".
#' 
#' @example matriz[6,] <- c(121.0,0.2,3.2,6.8,1.8,0,...,0)
#' @example num_materia <- 5
#' @example nom_archivo <- "Figuras/Matrices Simuladas/heatmap_var_materia_5_sem_20201.jpeg"
#'
#' @export jpeg: Imagen de las gráficas de heatmap obtenidas de alguna matriz.
#'
guarda_una_fig_heatmap <- function(matriz,num_materia,nom_archivo){
  load(file = "vec_nom_materias_total.RData")
  
  ## Para guardar gráficas de R como imagen .jpeg se manda llamar la función
  ## jpeg() antes de generar una gráfica. Al hacer esto, le indicamos a R
  ##que en lugar de mandar nuestro gráfico a una ventana del escritorio, lo
  ##mande a un dispositivo gráfico distinto.
  ## La función dev.off(), se utiliza para cerrar el dispositivo gráfico
  ##elegido y así poder crear más gráficos después.
  jpeg(filename = nom_archivo, width = 800, height = 700)
  colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
  heatmap(matriz, Colv = NA, Rowv = NA, scale="none",col=colMain,
          main = vec_nom_materias_total[num_materia])
  dev.off()
}


# guarda_heatmap_1_materia_1_sem ------------------------------------------
#' Title guarda_heatmap_1_materia_1_sem: Función que guarda las imágenes de
#' tipo "jpeg" de las gráficas heatmap para las pruebas hechas para un
#' semestre.
#'
#' @param lista_mat_n_sim: Lista que contiene las matrices generadas con
#' el número de alumnos simulados, ordenados de mayor a menor.
#' @param materia: Nombre de algún curso impartido en la FC.
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#' 
#' @example lista_mat_n_sim[[5]][1,] <- c(12-13,54,32,31,6,1,0,...,0)
#' @example materia <- "Estadística III"
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' 
guarda_heatmap_1_materia_1_sem <- function(lista_mat_n_sim,materia,sem_sig,param){
  #Se definen las variables que se van a utilizar
  num_materia <- arroja_num_materia(materia,param)
  num_col_1er_grupo <- arroja_ind_col_RG("col_1er_grupo") ##5
  num_col_ult_grupo <- arroja_ind_col_RG("col_ult_grupo") ##24
  
  #Se guardan las figuras correspondientes a la varianza
  mat_var_alum_x_materia_1_sem <- gen_mat_var_alum_x_materia_1_sem(lista_mat_n_sim,param)
  nom_heatmap_var <- gen_nom_heatmap("var_alum_materia",num_materia,sem_sig)
  guarda_una_fig_heatmap(mat_var_alum_x_materia_1_sem,num_materia,nom_heatmap_var)
  guarda_una_fig_heatmap(mat_var_alum_x_materia_1_sem,num_materia,nom_heatmap_var)
  
  # Se guardan las figuras correspondientes a la diferencia entre valores reales
  #y la esperanza.
  mat_esp_alum_x_materia_1_sem <- gen_mat_esp_alum_x_materia_1_sem(lista_mat_n_sim,param)
  mat_real_grupos_una_materia <- gen_mat_real_grupos_una_materia(materia,sem_sig,param)
  mat_dif_alum_x_materia_1_sem <- gen_mat_dif_alum_x_materia_1_sem(mat_esp_alum_x_materia_1_sem,
                                                                   mat_real_grupos_una_materia)
  
  mat_real_aux <- mat_real_grupos_una_materia[,num_col_1er_grupo:num_col_ult_grupo]
  mat_real_aux <- as.matrix(mat_real_aux,nrow=dim(mat_real_aux)[1],ncol=dim(mat_real_aux)[1])
  mat_dif_relativa_alum_x_materia_1_sem <- matrix(0,nrow = dim(mat_real_aux)[1],
                                                  ncol = dim(mat_real_aux)[2])
  if(sum(as.numeric(mat_real_aux)) != 0){###SUMAR RENGLONES Y COLS
    #Se hace la división en caso de que la matriz de datos reales sea
    #distinta de cero en todas sus entradas
    for(c in 1:dim(mat_real_aux)[2]){#Recorre columnas
      for(r in 1:dim(mat_real_aux)[1]){#Recorre renglones
        if(mat_real_aux[r,c]==0){
          #Esn caso de que el valor real sea cero, no se hace la división
          mat_dif_relativa_alum_x_materia_1_sem[r,c] <- 0
        }else{
          mat_dif_relativa_alum_x_materia_1_sem[r,c] <- mat_dif_alum_x_materia_1_sem[r,c]/mat_real_aux[r,c]
        }}#for r
    }#for c
  }else{
    #En caso de que toda la matriz de valores reales sea cero
    mat_dif_relativa_alum_x_materia_1_sem <- mat_real_aux
  }
  nom_heatmap_dif_rel <- gen_nom_heatmap("dif_relativa_alum_materia",num_materia,sem_sig)
  guarda_una_fig_heatmap(mat_dif_relativa_alum_x_materia_1_sem,num_materia,nom_heatmap_dif_rel)
}


# guarda_heatmap_1_materia_n_sem ------------------------------------------
#' Title guarda_heatmap_1_materia_n_sem: Función que guarda todas las
#' imágenes de tipo "jpeg" de las gráficas heatmap para las pruebas hechas.
#'
#' @param vec_sem_sig: Vector con los semestres de los que se desean obtener
#' las simulaciones. Deben estar ordenados de más antiguo a más reciente.
#' @param vec_sem_ant: Vector con el número de semestres anteriores para
#' cada "sem_sig" del vector "vec_sem_sig"
#' @param materia: Nombre de algún curso impartido en la FC.
#' @param num_sim: Número de matrices simuladas.
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#'
#' @example vec_sem_sig <- c(20131,20152,20182,20201)
#' @example vec_sem_ant <- c(10,15,21,24)##Se inicia en 2008-1
#' @example materia <- "Estadística III"
#' @example num_sim <- 10
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' 
guarda_heatmap_1_materia_n_sem <- function(vec_sem_sig,vec_sem_ant,materia,
                                           num_sim,param){
  #Se definen las variables que se van a utilizar
  num_materia <- arroja_num_materia(materia,param)
  nom_col_sim_alum <- "Alumnos_Simulados_Totales"
  nom_col_sim_gpos <- "Grupos_Simulados"
  nom_col_real_alum <- "Alumnos_Reales_Totales"
  nom_col_real_gpos <- "Grupos_Reales"
  
  mat_aux_real_alum <- matrix(0,nrow = 15,ncol = length(vec_sem_sig))
  mat_aux_real_gpos <- matrix(0,nrow = 15,ncol = length(vec_sem_sig))
  mat_aux_sim_alum <- matrix(0,nrow = 15,ncol = length(vec_sem_sig))
  mat_aux_sim_gpos <- matrix(0,nrow = 15,ncol = length(vec_sem_sig))
  
  mat_dif_total_alumnos_x_sem <- matrix(0,nrow = 15,ncol = length(vec_sem_sig))
  mat_dif_total_gpos_x_sem <- matrix(0,nrow = 15,ncol = length(vec_sem_sig))
  
  #Se generan los archivos de la simulación
  gen_list_n_sim_1_mat_n_sem(vec_sem_sig,vec_sem_ant,materia,num_sim,param)
  
  for(s in 1:length(vec_sem_sig)){#Recorre los semestres
    sem_sig <- vec_sem_sig[s]
    nom_lista_n_sim <- gen_nom_list_n_sim_1_materia(num_sim,num_materia,sem_sig)
    load(nom_lista_n_sim)
    
    #Se guardan las gráficas de una materia
    guarda_heatmap_1_materia_1_sem(lista_mat_n_sim,materia,sem_sig,param)
    
    ##Se obtienen las matrices que contienen los datos de alumnos totales
    ##reales por semestre
    nom_archivo_real <- paste0("mat_real_grupos por semestre/mat_real_grupos_",
                               vec_sem_sig[s],".RData")
    load(nom_archivo_real)
    mat_aux_real_alum[,s] <- gen_vec_suma_datos_real_1_sem(mat_real_grupos,
                                                           nom_col_real_alum,
                                                           param)
    mat_aux_real_gpos[,s] <- gen_vec_suma_datos_real_1_sem(mat_real_grupos,
                                                           nom_col_real_gpos,
                                                           param)
    mat_aux_sim_alum[,s] <- gen_vec_esp_dat_sim_1_sem_1_materia(lista_mat_n_sim,
                                                                nom_col_sim_alum)
    mat_aux_sim_gpos[,s] <- gen_vec_esp_dat_sim_1_sem_1_materia(lista_mat_n_sim,
                                                                nom_col_sim_gpos)
  }#Fin for(s)
  rownames(mat_aux_real_alum) <- param$nombre_hrs
  colnames(mat_aux_real_alum) <- vec_sem_sig
  rownames(mat_aux_real_gpos) <- param$nombre_hrs
  colnames(mat_aux_real_gpos) <- vec_sem_sig
  rownames(mat_aux_sim_alum) <- param$nombre_hrs
  colnames(mat_aux_sim_alum) <- vec_sem_sig
  rownames(mat_aux_sim_gpos) <- param$nombre_hrs
  colnames(mat_aux_sim_gpos) <- vec_sem_sig
  
  #Se cargan las matrices con las diferencias de datos
  mat_dif_total_alumnos_x_sem <- mat_aux_real_alum - mat_aux_sim_alum
  rownames(mat_dif_total_alumnos_x_sem) <- param$nombre_hrs
  colnames(mat_dif_total_alumnos_x_sem) <- vec_sem_sig
  
  mat_dif_total_gpos_x_sem <- mat_aux_real_gpos - mat_aux_sim_gpos
  rownames(mat_dif_total_gpos_x_sem) <- param$nombre_hrs
  colnames(mat_dif_total_gpos_x_sem) <- vec_sem_sig
  
  
  nom_archivo <- paste0("dif_relativa_total_de_alumnos_x_sem_",
                        vec_sem_sig[1],"-",vec_sem_sig[length(vec_sem_sig)],
                        "_materia_",num_materia)
  mat_dif_relativas_alum <- gen_mat_dif_relativas(mat_dif_total_alumnos_x_sem,
                                                  mat_aux_real_alum,nom_archivo)
  colnames(mat_dif_relativas_alum) <- vec_sem_sig
  rownames(mat_dif_relativas_alum) <- param$nombre_hrs
  # View(mat_dif_relativas_alum)
  
  nom_archivo <- paste0("dif_relativa_total_de_gpos_x_sem_",
                        vec_sem_sig[1],"-",vec_sem_sig[length(vec_sem_sig)],
                        "_materia_",num_materia)
  mat_dif_relativas_gpos <- gen_mat_dif_relativas(mat_dif_total_gpos_x_sem,
                                                  mat_aux_real_gpos,nom_archivo)
  colnames(mat_dif_relativas_gpos) <- vec_sem_sig
  rownames(mat_dif_relativas_gpos) <- param$nombre_hrs
  # View(mat_dif_relativas_gpos)
}





##### **AQUÍ** #####


##########################################################################
##### ESQUELETO #####
## Funciones que generan un esqueleto del siguiente semestre. Arroja un
##error en caso de no encontrar algún archivo que requiera.
##########################################################################


# gen_esqueleto -----------------------------------------------------------
#' Title: gen_esqueleto: Función que genera una matriz llamada
#' "mat_esqueleto", del semestre actual (sem_fin) que contiene el número
#' de grupos simulados para cada materia en cada hora. Arroja un error en
#' caso de no encontrar algún archivo que requiera.
#'
#' @param sem_fin: Variable tipo "integer" que indica el semestre
#' del cual se quiere obtener el esqueleto.
#' @param n_semestres_anteriores: Variable tipo "integer" que indica el
#' número de semestres anteriores al semestre actual para poder generar el
#' esqueleto.
#' @param directorio_info: Vector que contiene la ubicación y el nombre de
#' los archivos tipo ".Rdata" en donde se hayan guardado las matrices que
#' contienen la información de los semestres que se requieren para obtener
#' el esqueleto.
#' 
#' @example sem_fin <- 20201
#' @example n_semestres_anteriores <- 5
#' @example directorio_info <- c("m_grande por semestre/m_grande_20081.RData",
#' "m_grande por semestre/m_grande_20082.RData")
#'
#' @return mat_esqueleto: Matriz de 15 renglones con las horas (7-8,8-9,...,
#' 21-22) y tantas columnas como materias.
#'
gen_esqueleto <- function(directorio_info,param){
  # Start the clock!
  ptm <- proc.time()
  
  n_semestres_anteriores <- param$n_semestres_anteriores
  ##Se verifica que existen los archivos que se necesitan para formar el
  ##esqueleto.
  valida_info <- 0
  num_sem <- 0
  n_renglones <- 0
  
  for(i in 1:n_semestres_anteriores){
    ##CHECAR SI SE CAMBIA LA MANERA DE VERIFICAR LA EXISTENCIA DE LOS ARCHIVOS##
    if(file.exists(directorio_info[i])){
      num_sem <- num_sem +1
      load(directorio_info[i])
      n_renglones <- n_renglones + nrow(m_grande)}}
  
  if(num_sem==n_semestres_anteriores){
    valida_info <- 1}
  
  if(valida_info == 1){
    ##Se carga m_grande_total
    vec_excepciones <- "Inglés"
    nom_archivo_MGT <- paste0("Matrices m_grande_total/m_grande_total_",
                              param$sem_ini,"_",param$sem_fin,".RData")
    if(!file.exists(nom_archivo_MGT)){
      gen_m_grande_total(vec_excepciones,param)
    }else{
      load(nom_archivo_MGT)
    }
    
    #' Se define el vector de materias (vector con los nombres de todas las
    #' materias que se imparten en el Departamento de Matemáticas de la
    #' Facultad de Ciencias de la UNAM) y las variables que se utilizan
    nombre_hrs <- param$nombre_hrs
    vec_grupos_simulados <- rep(0,length(nombre_hrs))
    mat_simula_grupos <- guarda_mat_simula_grupos_1_sem(param)
    # load("vec_nom_materias_total.RData")
    materias_unicas <- param$vec_nom_materias_total#Vector con el nombre de las materias
    
    ## Inicializamos la matriz que va a contener la demanda simulada del
    ##número de grupos del siguiente semestre. Tiene tantos renglones como
    ##horas y tantas columnas como materias impartidas.
    mat_esqueleto <- matrix(0, nrow=length(param$nombre_hrs),
                            ncol=length(materias_unicas))
    
    vec_para_for <- 1:length(materias_unicas)
    pb <- txtProgressBar(min = 1, max = length(vec_para_for),
                         style = 3, width = 60)
    for(i in vec_para_for){
      setTxtProgressBar(pb, i)
      # version_matriz <- as.character(i)
      cat("\n***Iteración ",i," de ",length(materias_unicas))
      
      ##Se busca el vector con el número de grupos simulado de materia
      mat_info_grupos <- mat_simula_grupos[
        mat_simula_grupos[,1]==materias_unicas[i],c(2,3)]
      
      for(d in 1:length(nombre_hrs)){##Se recorren las horas
        if(dim(mat_info_grupos)[1] != 0){
          for(j in 1:dim(mat_info_grupos)[1]){ ##Se recorren los renglones
            hora <- mat_info_grupos[j,1]
            if(hora==nombre_hrs[d]){
              vec_grupos_simulados[d] <- mat_info_grupos[j,2]}}}}
      
      mat_esqueleto[,i] <- vec_grupos_simulados
    }
    close(pb)
    rownames(mat_esqueleto) <- param$nombre_hrs
    colnames(mat_esqueleto) <- materias_unicas
  }else{
    cat("\nNo se encontraron todos los archivos necesarios para generar 
        el esqueleto")
  }
  # Se guarda la variable tipo lista "lista_esqueleto", la cual contiene las
  #matrices "mat_simula_grupos" y "mat_esqueleto"
  lista_esqueleto <- list(mat_simula_grupos = mat_simula_grupos,
                          mat_esqueleto = mat_esqueleto)
  
  save(lista_esqueleto,file = paste0("lista_esqueleto_",param$sem_sig,".RData"))
  save(mat_simula_grupos,file = paste0("mat_simula_grupos por semestre/mat_simula_grupos_",
                                       param$sem_sig,".RData"))
  
  cat("La función gen_esqueleto tomó: ", (proc.time()-ptm)[3]/60," minutos\n\n\n" )
  return(mat_esqueleto)
}


############################################################################
##### SOLICITUDES #####
## Funciones que generan una matriz con solicitudes de todos los profesores.
############################################################################

# extrae_mat_x_prof -------------------------------------------------------
#' Esta función regresa una matriz de 16xnum_sem que contiene los nombres de
#' las materias que ha dado cada profesor por semestre.
#' Title: extrae_mat_x_prof
#' @param profesor: Nombre de algún profesor de la Facultad de Ciencias
#' @examples profesor <- "Arrigo Coen Coria"
#' 
#' @return materia_x_profesor: Matriz que contiene los nombres de las materias
#' que se le ha asignado a cada profesor por semestre. Tiene 16 renglones
#' y tanta columnas como semestres se tengan.
extrae_mat_x_prof <- function(profesor,param){
  m_grande_total = param$m_grande_total
  # lista_def_columnas_MG <- param$lista_def_columnas_MG
  num_col_Profesor <- arroja_ind_col_MG("Profesor")
  num_col_Materia <- arroja_ind_col_MG("Materia")
  num_col_NomMat_Act2000 <- arroja_ind_col_MG("NomMat_Act2000")
  num_col_NomMat_Act2006 <- arroja_ind_col_MG("NomMat_Act2006")
  num_col_NomMat_Act2015 <- arroja_ind_col_MG("NomMat_Act2015")
  num_col_NomMat_CdC1994 <- arroja_ind_col_MG("NomMat_CdC1994")
  num_col_NomMat_CdC2013 <- arroja_ind_col_MG("NomMat_CdC2013")
  num_col_NomMat_Mat1983 <- arroja_ind_col_MG("NomMat_Mat1983")
  num_col_NomMat_MAp2017 <- arroja_ind_col_MG("NomMat_MAp2017")
  num_col_Semestre <- arroja_ind_col_MG("Semestre")
  num_col_Grupo <- arroja_ind_col_MG("Grupo")
  
  mat_grupos_prof <- as.matrix(data.frame(m_grande_total[,num_col_Profesor],
                                          m_grande_total[,num_col_Materia],
                                          m_grande_total[,num_col_NomMat_Act2000],
                                          m_grande_total[,num_col_NomMat_Act2006],
                                          m_grande_total[,num_col_NomMat_Act2015],
                                          m_grande_total[,num_col_NomMat_CdC1994],
                                          m_grande_total[,num_col_NomMat_CdC2013],
                                          m_grande_total[,num_col_NomMat_Mat1983],
                                          m_grande_total[,num_col_NomMat_MAp2017],
                                          m_grande_total[,num_col_Semestre],
                                          m_grande_total[,num_col_Grupo]))
  
  mat_grupos <- mat_grupos_prof[mat_grupos_prof[,1]== profesor,2:11]
  mat_num_grupos <- unique(mat_grupos)#Se eliminan los renglones repetidos
  
  ## Se pone el if porque si sólo hay un vector marca error al poner nombres
  ##a las columnas en un objeto que tiene dimensión menor a 2
  if(length(mat_num_grupos)<11){
    mat_num_grupos <- matrix(mat_num_grupos,ncol = 10)
  }
  colnames(mat_num_grupos) <- c("Materias","NomMat_1","NomMat_2",
                                "NomMat_3","NomMat_4","NomMat_5",
                                "NomMat_6","NomMat_7","Sem","Grupos")
  
  ## Se tienen 16 renglones porque se tienen 8 columnas con nombres 
  ##para cada materia
  materia_x_profesor <- matrix(0,nrow = 16,ncol = length(param$Semestres))
  rownames(materia_x_profesor) <- c("Materia_1.0",
                                    "Materia_1.1",
                                    "Materia_1.2",
                                    "Materia_1.3",
                                    "Materia_1.4",
                                    "Materia_1.5",
                                    "Materia_1.6",
                                    "Materia_1.7",
                                    "Materia_2.0",
                                    "Materia_2.1",
                                    "Materia_2.2",
                                    "Materia_2.3",
                                    "Materia_2.4",
                                    "Materia_2.5",
                                    "Materia_2.6",
                                    "Materia_2.7")
  colnames(materia_x_profesor) <- param$nombre_sem
  
  for(j in 1:length(param$Semestres)){
    datos_x_sem <- mat_num_grupos[mat_num_grupos[,9]==param$Semestres[j],1:8]
    if(length(datos_x_sem)==0){
      materia_x_profesor[,j] <- 0
    }else if(length(datos_x_sem)==8){
      materia_x_profesor[1:8,j] <- datos_x_sem
      materia_x_profesor[9:16,j] <- 0
    }else{
      materia_x_profesor[1:8,j] <- datos_x_sem[1,]
      materia_x_profesor[9:16,j] <- datos_x_sem[2,]
    }
  }
  
  return(materia_x_profesor)
}


# simula_una_eleccion_materia ------------------------------------------------------
#' Title simula_una_eleccion_materia: Función que simula una elección de materia de
#' cada profesor.
#' @param profesor: Nombre de algún profesor de la Facultad de Ciencias
#' @examples profesor <- "Arrigo Coen Coria"
#' 
#' @return materia_simulada: Vector de tamaño 8 con los nombre de la materia simulada.
#' 
simula_una_eleccion_materia <- function(profesor,param){
  #Matriz que contiene los nombres de las materias que se le ha asignado a
  #cada profesor por semestre.
  materia_x_profesor <- extrae_mat_x_prof(profesor,param)
  # prob_eleccion_x_sem <- c(0.0125,0.0125,0.0125,0.0125,0.0125,0.0125,0.0125,
  #                          0.0125,0.1,0.5,0.3)
  prob_eleccion_x_sem <- c(rep(0.1/(length(param$Semestres)-3),
                               length(param$Semestres)-3),0.1,0.5,0.3)
  prob_acum <- c(0,cumsum(prob_eleccion_x_sem))
  num_aleatorio <- runif(1)
  
  for(k in 1:(length(prob_acum)-1)){
    if(num_aleatorio >= prob_acum[k] && num_aleatorio < prob_acum[k+1]){
      ##Para simular sólo una materia, se selecciona de manera aleatoria
      if(num_aleatorio >= 0.5){
        materia_simulada <- materia_x_profesor[1:8,k]
      }else{
        materia_simulada <- materia_x_profesor[9:16,k]
      }
    }
  }
  return(materia_simulada)
}


# simula_eleccion_materia ---------------------------------------------------------
#' Title simula_eleccion_materia: Función que simula varias elecciones de materias de
#' cada profesor.
#' @param profesor: Nombre de algún profesor de la Facultad de Ciencias
#' @examples profesor <- "Arrigo Coen Coria"
#' 
#' @return materias_simuladas: Vector con los nombres de las materias
#' simuladas.
simula_eleccion_materia <- function(profesor,param){
  num_simula_eleccion_materia <- param$num_simula_eleccion_materia
  mat_materias_simuladas <- matrix(0,nrow = 8,ncol = num_simula_eleccion_materia)
  materias_simuladas <- rep(0,num_simula_eleccion_materia)
  
  for(k in 1:num_simula_eleccion_materia){
    # cat("\nk = ",k)
    eleccion <- simula_una_eleccion_materia(profesor,param)
    # cat("\n eleccion = ",eleccion)
    mat_materias_simuladas[,k] <- eleccion
  }
  
  
  #Se toma un nombre de materia por cada columna
  for(k in 1:num_simula_eleccion_materia){
    materias_simuladas[k] <- sample(mat_materias_simuladas[,k],size = 1)
  }
  
  return(materias_simuladas)
}


# horario_de_materia_x_prof ---------------------------------------------
#' Title: horario_de_materia_x_prof: Función regresa una matriz de 2xnum_sem
#' que contiene los horarios materias que ha dado cada profesor por semestre.
#' @param profesor: Nombre de algún profesor de la Facultad de Ciencias
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#' 
#' @examples profesor <- "Arrigo Coen Coria"
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#' 
#' @return horario_x_profesor: Matriz que contiene los horarios de las
#' materias que ha dado cada profesor por semestre.
#' 
horario_de_materia_x_prof <- function(profesor,param){
  m_grande_total = param$m_grande_total
  
  num_col_Profesor <- arroja_ind_col_MG("Profesor")
  num_col_Materia <- arroja_ind_col_MG("Materia")
  num_col_Semestre <- arroja_ind_col_MG("Semestre")
  num_col_Grupo <- arroja_ind_col_MG("Grupo")
  num_col_horario_num <- arroja_ind_col_MG("horario_num")
  
  
  mat_grupos <- m_grande_total[m_grande_total[,num_col_Profesor]== profesor,
                               c(num_col_Materia,num_col_Semestre,
                                 num_col_Grupo,num_col_horario_num)]
  mat_num_grupos <- unique(mat_grupos)#Se eliminan los renglones repetidos
  colnames(mat_num_grupos) <- c("Materias","Sem","Grupos","horario_num")
  
  horario_x_profesor <- matrix(0,nrow = 2,ncol = length(param$Semestres))
  rownames(horario_x_profesor) <- c("Materia1","Materia2")
  colnames(horario_x_profesor) <- param$nombre_sem
  
  for(j in 1:length(param$Semestres)){
    datos_x_sem <- mat_num_grupos[mat_num_grupos[,2]==param$Semestres[j],4]
    if(length(datos_x_sem)==1){
      horario_x_profesor[1,j] <- datos_x_sem
      horario_x_profesor[2,j] <- 0
    }else{
      for(k in 1:2){
        horario_x_profesor[k,j] <- datos_x_sem[k]}}}
  
  return(horario_x_profesor)
}


# simula_una_eleccion_horario ------------------------------------------------------
#' Title simula_una_eleccion_horario: Función que simula una elección de horarios de
#' cada profesor.
#' @param profesor: Nombre de algún profesor de la Facultad de Ciencias
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#' 
#' @examples profesor <- "Arrigo Coen Coria"
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#' 
#' @return horarios_simulados: Horario simulado por profesor
#' 
simula_una_eleccion_horario <- function(profesor,param){
  #Matriz que contiene los nombres de las horarios que se le ha asignado a
  #cada profesor por semestre.
  horario_x_profesor <- horario_de_materia_x_prof(profesor,param)
  prob_eleccion_x_sem <- c(rep(0.1/(length(param$Semestres)-3),
                               length(param$Semestres)-3),0.1,0.5,0.3)
  prob_acum <- c(0,cumsum(prob_eleccion_x_sem))
  horarios_simulados <- 0
  num_aleatorio <- runif(1)
  # cat("\n num_aleatorio = ",num_aleatorio)
  
  valor_aux <- 0
  k <- 1
  while(valor_aux == 0 && k <= (length(prob_acum)-1)){
    # cat("\nk = ",k)
    if(num_aleatorio >= prob_acum[k] && num_aleatorio < prob_acum[k+1]){
      ##Preguntamos si hay datos en la primer materia del vector (con respecto
      #al semestre en el que vamos a elegir el horario)
      if(is.na(horario_x_profesor[1,k])){
        ##Preguntamos si hay datos en la segunda materia del vector (con respecto
        #al semestre en el que vamos a elegir el horario)
        # cat("\nif_1")
        if(is.na(horario_x_profesor[2,k])){
          # cat("\nif_2")
          num_aleatorio <- runif(1)
          k <- 0
          # cat("\n num_aleatorio = ",num_aleatorio)
        }else{
          # cat("\nelse_1")
          horarios_simulados <- horario_x_profesor[2,k]
          valor_aux <- 1
        }
      }else{
        # cat("\nelse_2")
        horarios_simulados <- horario_x_profesor[1,k]
        valor_aux <- 1
      }
    }
    k <- k +1
  }
  return(horarios_simulados)
}


# simula_eleccion_horario ---------------------------------------------------------
#' Title simula_eleccion_horario: Función que simula varias elecciones de horarios
#' de cada profesor.
#' @param profesor: Nombre de algún profesor de la Facultad de Ciencias
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#' 
#' @examples profesor <- "Arrigo Coen Coria"
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#' 
#' @return horarios_simulados: Vector con los nombres de los horarios
#' simulados por profesor.
simula_eleccion_horario <- function(profesor,param){
  num_simula_eleccion_horario <- param$num_simula_eleccion_horario
  horarios_simulados <- rep(0,num_simula_eleccion_horario)
  
  for(k in 1:num_simula_eleccion_horario){
    (eleccion <- simula_una_eleccion_horario(profesor,param))
    horarios_simulados[k] <- eleccion
  }
  horarios_simulados <- as.integer(unique(horarios_simulados))
  
  return(horarios_simulados)
}


# gen_solicitudes ---------------------------------------------------------
#' Title gen_solicitudes: Función que guarda, en una matriz, la información
#' de las solicitudes de materia y de horario de todos los profesores las
#' cuales se generan por medio de las simulaciones de dichas elecciones
#' (materia y horario).
#'
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#' 
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return mat_solicitudes: Matriz de 12 columnas que contiene la
#' información de las solicitudes de materia y de horario de todos los
#' profesores, en las primeras 6 columnas se tiene la información
#' de la simulación de elección de materias y en las últimas 6 columnas
#' se tiene la información de la simulación de elección de horarios,
#' la matriz puede no estar completamente llena),tiene como renglones
#' los nombres de los profesores.
#'
gen_solicitudes <- function(param){
  m_grande_total = param$m_grande_total
  num_col <- arroja_ind_col_MG("Profesor")
  Profesor <- m_grande_total[,num_col]
  
  # lista_def_columnas_MG <- param$lista_def_columnas_MG
  # Start the clock!
  ptm <- proc.time()
  
  ##Vector con nombres de profesores sin  repetición
  # Profesores <- sort(unique(lista_def_columnas_MG$Profesor))
  Profesores <- unique(Profesor)
  #Quitamos las entradas que sean iguales a cero en caso de existir
  Profesores <- Profesores[Profesores!=0]
  num_profesores <- length(Profesores)
  
  ##Inicializamos la matriz
  mat_solicitudes <- matrix(0,nrow = num_profesores,ncol = 12)
  rownames(mat_solicitudes) <- Profesores
  colnames(mat_solicitudes) <- c("Materia_1","Materia_2","Materia_3",
                                 "Materia_4","Materia_5","Materia_6",
                                 "Horario_1","Horario_2","Horario_3",
                                 "Horario_4","Horario_5","Horario_6")
  vec_para_for <- 1:num_profesores
  pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
  for(k in vec_para_for){
    setTxtProgressBar(pb, k)
    # cat("\n k_prof_solic = ",k)
    materias_simuladas <- simula_eleccion_materia(Profesores[k],param)
    horarios_simulados <- simula_eleccion_horario(Profesores[k],param)
    
    ##Los vectores anteriores no necesariamente tienen información en
    ##todas sus entradas, por lo que la matriz "mat_solicitudes" puede
    ##no estar completamente llena.
    mat_solicitudes[k,1:param$num_simula_eleccion_materia] <- materias_simuladas
    # mat_solicitudes[k,(param$num_simula_eleccion_materia+1):
    #                   (param$num_simula_eleccion_materia+
    #                      param$num_simula_eleccion_horario)] <- horarios_simulados
  }##Fin de for
  close(pb)
  
  cat("La función gen_solicitudes tomó: ",(proc.time()-ptm)[3]/60," minutos\n\n\n" )
  
  return(mat_solicitudes)
}



############################################################################
##### UNA ASIGNACIÓN #####
## Funciones que generan una matriz con las asignaciones de
##Materia-Profesor-Horario-Salón
############################################################################

# asigna_una_mat_prof_hora ---------------------------------------------------
#' Title: asigna_una_mat_prof_hora: Función que genera la asignación de una 
#' materia con profesor por hora, dependiendo del número de grupos simulados
#' para el siguiente semestre.
#'
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#' @return mat_asignacion: Matriz de 4 columnas (Materia,Profesor,Horario,
#' Salón), la cual tiene la información de las asignaciones generadas de
#' las simulaciones tanto del número de grupos como de las elecciones de
#' los profesores.
#'
asigna_una_mat_prof_hora <- function(mat_solicitudes,materia,param){
  m_grande_total = param$m_grande_total
  num_col <- arroja_ind_col_MG("Profesor")
  # Profesores <- sort(unique(m_grande_total[,2]))
  Profesores <- unique(m_grande_total[,num_col])
  #Quitamos las entradas que sean iguales a cero en caso de existir
  Profesores <- Profesores[Profesores!=0]
  num_profes <- length(Profesores)
  mat_prof_materia_horario <- matrix(c(Profesores,mat_solicitudes),
                                     ncol = 13)
  colnames(mat_prof_materia_horario) <- c("Profesores","Materia_1","Materia_2",
                                          "Materia_3","Materia_4","Materia_5",
                                          "Materia_6","Horario_1","Horario_2",
                                          "Horario_3","Horario_4","Horario_5",
                                          "Horario_6")
  gpos_simulados <- simula_grupos(materia,param)
  
  mat_asignacion <- matrix(0,nrow = sum(gpos_simulados),ncol = 4)
  colnames(mat_asignacion) <- c("Materia","Profesor","Horario","Salón")
  
  mat_asignacion[,1] <- materia ##Se llena la columna "Materia"
  
  ##Se llena la columna "Horario"
  if(sum(gpos_simulados)==0){
    vec_num_horas <- rep(0,sum(gpos_simulados))
    ## Marcamos con -1 la variable para que al generar la matriz con toda la
    ##información no haya error.
    mat_asignacion <- -1
  }else{
    i <- 1
    vec_num_horas <- rep(0,sum(gpos_simulados))
    for(k in 1:length(gpos_simulados)) {
      # cat("\n k = ",k)
      if(gpos_simulados[k]>0){
        # cat("\n  i = ",i)
        n_gpos <- gpos_simulados[k]
        mat_asignacion[i:(i+n_gpos-1),3] <- param$nombre_hrs[k]
        vec_num_horas[i:(i+n_gpos-1)] <- param$Horas[k]
        i <- i + n_gpos}}}
  
  ##Se llena la columna "Profesor"
  if(length(mat_asignacion)>1){##Verifica que haya información en la matriz
    datos_de_materia <- matrix(0,ncol = 7,nrow = num_profes)
    colnames(datos_de_materia) <- c("Profesor","Horario1","Horario2",
                                    "Horario3","Horario4","Horario5","Horario6")
    for(m in 1:num_profes){
      if(mat_solicitudes[m,1]==materia||
         mat_solicitudes[m,2]==materia||
         mat_solicitudes[m,3]==materia||
         mat_solicitudes[m,4]==materia||
         mat_solicitudes[m,5]==materia||
         mat_solicitudes[m,6]==materia){
        datos_de_materia[m,] <- mat_prof_materia_horario[m,c(1,8:13)]}}
    datos_de_materia <- unique(datos_de_materia)
    
    for(k in 1:length(vec_num_horas)){
      # cat("\nk = ",k)
      while(mat_asignacion[k,2] == 0){
        for(i in 1:dim(datos_de_materia)[1]){
          if(datos_de_materia[i,1] != 0){
            for(j in 2:7){
              if(datos_de_materia[i,j]==vec_num_horas[k] && mat_asignacion[k,2]==0){
                mat_asignacion[k,2] <- datos_de_materia[i,1]
                ##Parar evitar que un mismo profesor dé más de una clase a la misma
                #hora, se sustituye el valor de la matriz de datos por un cero.
                datos_de_materia[i,j] <- 0}}}}
        if(mat_asignacion[k,2] == 0){
          mat_asignacion[k,2] <- -1
        }
      }##Fin de while
    }##Fin de for(k in 1:length(vec_num_horas))
  }##Fin de if(length(mat_asignacion)>1)
  
  return(mat_asignacion)
}


# gen_asignacion ----------------------------------------------------------
#' Title gen_asignacion: Función que genera asignaciones de materia con
#' profesor por hora, dependiendo del número de grupos simulados para el
#' siguiente semestre, con la información de solicitudes que se obtiene de
#' la función "gen_solicitudes". 
#'
#' @param mat_solicitudes: Matriz de 12 columnas que contiene la
#' información de las solicitudes de materia y de horario de todos los
#' profesores, en las primeras 6 columnas se tiene la información
#' de la simulación de elección de materias y en las últimas 6 columnas
#' se tiene la información de la simulación de elección de horarios,
#' la matriz puede no estar completamente llena),tiene como renglones
#' los nombres de los profesores.
#' @param mat_esqueleto: Matriz de 15 renglones con las horas (7-8,8-9,...,
#' 21-22) y tantas columnas como materias impartidas en el semestre actual.
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#' 
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return mat_asignaciones: Matriz de cuatro columnas (Materia, Profesor,
#' Horario, Salón) la cual contiene en el i-ésimo renglón la asignación
#' por materia, profesor y horario //está pendiente la asignación de salón//
#'
gen_asignacion <- function(mat_esqueleto,mat_solicitudes,param){
  # Start the clock!
  ptm <- proc.time()
  
  ##Se inicializan las variables
  m_grande_total = param$m_grande_total
  num_col <- arroja_ind_col_MG("Materia")
  # Materias <- sort(unique(m_grande_total[,1]))
  # Materias <- unique(m_grande_total[,num_col])
  # load("vec_nom_materias_total.RData")
  Materias <- param$vec_nom_materias_total
  #Quitamos las entradas que sean iguales a cero en caso de existir
  Materias <- Materias[Materias!=0]
  num_materias <- length(Materias)
  
  # Para inicializar la matriz que se va a llenar con la información,
  #se suman los grupos totales obtenidos en la matriz "mat_esqueleto"
  #y se le suman 1000 renglones para tener una base y que la matriz no
  #incremente su tamaño en cadaiteración de llenado.
  gpos_simulados <- sum(mat_esqueleto)
  mat_asignaciones <- matrix(0,nrow = gpos_simulados+1000, ncol = 4)
  
  i <- 1
  vec_para_for <- 1:num_materias
  pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
  for(m in vec_para_for){
    setTxtProgressBar(pb, m)
    # cat("\n Materia = ",m," de ",num_materias)
    # cat("\n  i = ",i)
    mat_asignacion <- asigna_una_mat_prof_hora(mat_solicitudes,Materias[m],param)
    num_renglones <- dim(mat_asignacion)[1]
    
    if(length(mat_asignacion)==1){
      ## Si "mat_asignacion" tiene longitud de 1, implica que no hay grupos
      ##simulados para dicha materia
      i <- i + 1
    }else{
      for(d in 1:num_renglones){
        # cat("\n   d = ",d)
        mat_asignaciones[i,] <- mat_asignacion[d,]
        i <- i + 1
      }
    }
  }
  close(pb)
  
  ## Se eliminan los ceros de la matriz
  #No utilizar unique() porque no se conserva el número de grupos que
  #se requieren
  # mat_asignaciones <- unique(mat_asignaciones)
  mat_asignaciones <- matrix(mat_asignaciones[mat_asignaciones[,1]!=0],
                             ncol = 4)
  colnames(mat_asignaciones) <- c("Materia","Profesor","Horario","Salón")
  
  cat("La función gen_asignaciones tomó: ", (proc.time()-ptm)[3]/60," minutos\n\n\n" )
  
  ##ELIMINAR LOS PROFESORES QUE NO ESTÁN DISPONIBLES PARA DAR CLASES##
  return(mat_asignaciones)
}

############################################################################
##### ASIGNACIÓN COMPLETA #####
## Función que manda llamar todas las funciones para obtener la matriz
##de asignaciones Materia-Profesor-Horario(-Salón)
############################################################################

# gen_asignacion_completa -------------------------------------------------
#' Title gen_asignacion_completa: Función que genera la asignación completa,
#se le pide al usuario los parámetros que se requieren y al final se obtiene
#la matriz con las asignaciones de materia-profesor-horario(-salón)
#'
#' @param sem_ini: Semestre en el que se inicia la búsqueda de
#' información.
#' @param sem_fin: Semestre en el que se finaliza la búsqueda de
#' información para generar la asignación del siguiente semestre.
#'
#' @example sem_ini <- 20081
#' @example sem_fin <- 20181
#'
#' @return mat_asignaciones: Matriz de cuatro columnas (Materia, Profesor,
#' Horario, Salón) la cual contiene en el i-ésimo renglón la asignación
#' por materia, profesor y horario //está pendiente la asignación de salón//
#'
gen_asignacion_completa <- function(sem_ini,sem_fin){
  # Start the clock!
  ptm <- proc.time()##Se pone al inicio de la función
  
  # Lista -------------------------------------------------------------------
  ## Se definen las listas generales de parámetros que se van a utilizar:
  # list_url <- list()
  # list_url$sem_ini = 20081 # Datos SUPER GRANDE
  # # list_url$sem_ini = 20151 # Datos GRANDE
  # # list_url$sem_ini = 20172 # Datos Mediana 
  # # list_url$sem_ini = 20192 # Datos Chica 
  # # list_url$sem_fin = 20192
  # list_url$sem_fin = 20201
  # list_url$sem_actual = 20201
  # list_url$Actualiza_RAW_url = TRUE
  # list_url$Actualiza_limpia_base_url = TRUE
  # list_url$Actualiza_elimina_grupos_con_0 = TRUE
  # list_url$Salvar_URL_RData = TRUE
  # list_url$usar_vec_corto_num_materia = TRUE
  # list_url$elimina_pags_con_0_grupos = TRUE
  # # list_url$Carpeta_RData = "Archivos RData V01"
  # list_url$Carpeta_RData = "Archivos RData"
  # 
  # list_url$utilizar_RAW_anterior = T
  # list_url$usa_grupos_salvados = T
  # list_url$usa_vec_con_salon = T
  # list_url$usa_vec_con_info_salvados = T
  # 
  # 
  # list_url$planes_estudio = c(119,1176,2017,218,1556,217,2055)
  # list_url$file_name <- paste0(list_url$Carpeta_RData,"/Dat_URL_",list_url$sem_ini,"_",list_url$sem_fin,".RData")
  # list_url$file_name_RAW <- paste0(list_url$Carpeta_RData,"/Lista_RAW_Dat_URL_",list_url$sem_ini,"_",list_url$sem_fin,".RData")
  # list_url$nombres_carrera_plan <- c("Actuaría (plan 2000)",
  #                                    "Actuaría (plan 2006)",
  #                                    "Actuaría (plan 2015)",
  #                                    "Ciencias de la Computación (plan 1994)",
  #                                    "Ciencias de la Computación (plan 2013)",
  #                                    "Matemáticas (plan 1983)",
  #                                    "Matemáticas Aplicadas (plan 2017)")
  # list_url$mat_ubicaciones_url <- matrix(c("Materia"            ,'#info-contenido h2', T,F,
  #                                          "Profesor"           ,'tr:nth-child(1) td:nth-child(2) a',F,F,
  #                                          "Horario"            ,'tr:nth-child(1) td:nth-child(4)',F,T,
  #                                          "Lugares"            ,'#info-contenido div',F,F,
  #                                          "Alumnos"            ,'#info-contenido div',F,F,
  #                                          "Salon"              ,'tr:nth-child(1) td~ td+ td a , td:nth-child(4) a',F,F,
  #                                          "Grupo"              ,'#info-contenido div',F,F,
  #                                          "Carrera"            ,'h1',T,F,
  #                                          "Plan"               ,'h1',T,F,
  #                                          "Semestre"           ,-1,-1,-1,### FALTA POR HACER
  #                                          "Cambios"            ,-1,-1,-1,### FALTA POR HACER
  #                                          "Turno"              ,-1,-1,-1,### FALTA POR HACER
  #                                          "Semestre_de_materia",'#info-contenido h2',T,F,
  #                                          "Grupos_x_pag"       ,'strong',F,F,
  #                                          "Grupo_paralelo"     ,'em',F,F),ncol=4,byrow = T)
  # list_url$colnames_mat_posibles_url <- c("Semestre","Plan","Materia","URL","Grupos x pag","url_con_salon")
  # list_url$ncol_mat_posibles_url <- length(list_url$colnames_mat_posibles_url)
  # list_url$nrow_mat_posibles_url <- 200000
  # # list_url$nrow_mat_posibles_url = dim(list_url$mat_posibles_url)[1]
  # list_url$mat_RAW_url <- matrix(0,list_url$nrow_mat_posibles_url,list_url$ncol_mat_posibles_url)
  # list_url$mat_posibles_url <- list_url$mat_RAW_url
  # 
  # 
  # list_url$ncol_mat_Grande <- 13
  # list_url$mat_Grande <- matrix(0,0,list_url$ncol_mat_Grande)
  # list_url$mat_Grande_con_url <- matrix(0,0,list_url$ncol_mat_Grande+1)
  # 
  # list_url$semestres_reales <- list_url$sem_fin
  # list_url$plan_reales <- 2017
  # list_url$num_mat_reales <- 1
  # list_url$num_grupos <- rep(-1,list_url$nrow_mat_posibles_url)
  # list_url$url_con_salon <- NA
  # 
  # list_url$mat_paginas_error <- matrix(0,1,4)
  # 
  # 
  # list_url$indicadoras_actualiza_col_j_mat_Grande = rep(T,13)
  # 
  # 
  # colnames(list_url$mat_ubicaciones_url) <- c("Nombre columna","Ubicacion en pagina","Repetir","Elimina salto")
  # colnames(list_url$mat_Grande) <- c("Materia", "Profesor","Horario","Lugares","Alumnos","Salon",
  #                                    "Grupo","Carrera","Plan","Semestre","Cambios","Turno","Semestre_de_materia")
  # 
  # colnames(list_url$mat_paginas_error) <- c("Columna","length(vec)","num_gpo","Pagina")
  # 
  # if(F) {
  #   list_url$utilizar_RAW_anterior = F
  #   list_url$usa_grupos_salvados = F
  #   list_url$usa_vec_con_salon = F
  #   list_url$usa_vec_con_info_salvados = F
  # }
  # # Valida_list_url(list_url)
  # 
  # param = list(nombre_hrs = c("7-8","8-9","9-10","10-11","11-12",
  #                             "12-13","13-14","14-15","15-16",
  #                             "16-17","17-18","18-19","19-20",
  #                             "20-21","21-22"),
  #              nombre_sem = c("2015-1","2015-2","2016-1","2016-2",
  #                             "2017-1","2017-2","2018-1","2018-2",
  #                             "2019-1","2019-2","2020-1"),
  #              Semestres = c(20151,20152,20161,20162,20171,20172,20181,
  #                            20182,20191,20192,20201),
  #              Horas = c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21),
  #              q1 = 80, q2 = 90,
  #              num_simula_eleccion_materia = 6,
  #              num_simula_eleccion_horario = 6,
  #              nom_cols_m14 = c("Materia","Profesor","Horario","Lugares",
  #                               "Alumnos","Salon","Grupo","Carrera","Plan",
  #                               "Semestre","Cambios","Turno",
  #                               "Semestre_de_materia","url"),
  #              nom_cols_MG = c("Materia","Profesor","Horario","horario_num",
  #                              "Lugares","Alumnos","Salon","Grupo","Carrera",
  #                              "Plan","Semestre","Cambios","Turno",
  #                              "Semestre_de_materia","url","Act2000","Act2006",
  #                              "Act2015","CdC1994","CdC2013","Mat1983","MAp2017",
  #                              "NomMat_Act2000","NomMat_Act2006","NomMat_Act2015",
  #                              "NomMat_CdC1994","NomMat_CdC2013","NomMat_Mat1983",
  #                              "NomMat_MAp2017","URL_Act2000","URL_Act2006",
  #                              "URL_Act2015","URL_CdC1994","URL_CdC2013",
  #                              "URL_Mat1983","URL_MAp2017"),
  #              m_grande_total = matrix(0,ncol = 
  #                                        length(c("Materia","Profesor","Horario",
  #                                                 "horario_num","Lugares","Alumnos",
  #                                                 "Salon","Grupo","Carrera","Plan",
  #                                                 "Semestre","Cambios","Turno",
  #                                                 "Semestre_de_materia","url",
  #                                                 "Act2000","Act2006","Act2015",
  #                                                 "CdC1994","CdC2013","Mat1983",
  #                                                 "MAp2017","NomMat_Act2000",
  #                                                 "NomMat_Act2006","NomMat_Act2015",
  #                                                 "NomMat_CdC1994","NomMat_CdC2013",
  #                                                 "NomMat_Mat1983","NomMat_MAp2017",
  #                                                 "URL_Act2000","URL_Act2006",
  #                                                 "URL_Act2015","URL_CdC1994",
  #                                                 "URL_CdC2013","URL_Mat1983",
  #                                                 "URL_MAp2017"))))
  # Termina definición de lista ---------------------------------------------
  
  list_url$sem_ini = sem_ini
  list_url$sem_actual = sem_fin
  list_url$sem_fin = sem_fin
  
  list_url$file_name = paste0(list_url$Carpeta_RData,"/Dat_URL_",list_url$sem_ini,
                              "_",list_url$sem_fin,".RData")
  list_url$file_name_RAW = paste0(list_url$Carpeta_RData,"/Lista_RAW_Dat_URL_",
                                  list_url$sem_ini,"_",list_url$sem_fin,".RData")
  
  
  if(nchar(list_url$sem_ini)!= 5 || nchar(list_url$sem_fin)!= 5){
    cat("\n***Los semestres ingresados no son válidos***")
  }else{
    ##### Se crea el vector para los semestres pares e impares #####
    semestres = (list_url$sem_ini:list_url$sem_fin)[
      (list_url$sem_ini:list_url$sem_fin) %% 10>0 &
        (list_url$sem_ini:list_url$sem_fin) %% 10<3]
    param$Semestres = semestres
    n_semestres_anteriores <- length(semestres)
    param$nombre_sem = as.character(semestres)
    
    
    ##### Se carga y se limpia la lista de urls (para no tener #####
    ## páginas sin información,...)
    list_url <- Actualiza_list_url(list_url)
    mat_posibles_url <- list_url$mat_posibles_url
    colnames(mat_posibles_url) <- c("Semestre","Plan","Materia","URL",
                                    "Grupos x pag","url_con_salon")
    # View(mat_posibles_url)
    save(mat_posibles_url, file = paste0("mat_posibles_url_",list_url$sem_ini,"_",
                                         list_url$sem_fin,".RData"))
    
    ##### Se obtiene "m_grande" y se genera un archivo para cada semestre #####
    
    
    ######### OBTENER M_GRANDE ############
    
    directorio_info <- rep(0,length(semestres))
    cat("\nSe obtiene m_grande y se genera un archivo para cada semestre")
    cat("\n")
    for(k in 1:length(semestres)){
      # cat("\n k = ",k)
      sem_info <- semestres[k]
      directorio_info[k] <- gen_m_grande(sem_info,list_url)
    }
    
    ##### Se genera el esqueleto del semestre que se quiere obtener #####
    cat("\nGenerando mat_esqueleto")
    cat("\n")
    mat_esqueleto <- gen_esqueleto(directorio_info,param)
    save(mat_esqueleto, file = paste0("mat_esqueleto_",list_url$sem_ini,"_",
                                      list_url$sem_fin,".RData"))
    
    ##Se carga y se define "m_grande_total" y la lista con sus columnas
    load(paste0("m_grande_total_",list_url$sem_ini,"_",list_url$sem_fin,".RData"))
    param$m_grande_total = m_grande_total
    
    ##### Se genera la matriz de solicitudes de todos los profesores #####
    cat("\nGenerando mat_solicitudes")
    cat("\n")
    mat_solicitudes <- gen_solicitudes(param)
    
    save(mat_solicitudes, file = paste0("mat_solicitudes_",list_url$sem_ini,"_",
                                        list_url$sem_fin,".RData"))
    
    
    ##### Se genera la matriz de asignaciones de todos los profesores #####
    cat("\nGenerando mat_asignaciones")
    cat("\n")
    mat_asignaciones <- gen_asignacion(mat_esqueleto,mat_solicitudes,param)
    save(mat_asignaciones, file = paste0("mat_asignaciones_",list_url$sem_ini,"_",
                                         list_url$sem_fin,".RData"))
    if((sem_fin%%2)==0){
      sem_sig <- sem_fin + 9
    }else{
      sem_sig <- sem_fin + 1
    }
    
    cat("\nLa matriz de asignaciones generada corresponde al semestre: ",sem_sig)
    cat("\nEl proceso total tomó: ", (proc.time()-ptm)[3]/60," minutos\n\n\n" )
    
    return(mat_asignaciones)
  }
}


############################################################################
################################ OTROS #####################################
############################################################################


# gen_nom_list_n_sim_1_materia --------------------------------------------
#' Title gen_nom_list_n_sim_1_materia: Función que genera el nombre de los
#' archivos que guardan las listas que contienen las matrices con las n
#' simulaciones por materia.
#'
#' @param num_sim: Número de matrices simuladas. 
#' @param num_materia: Índice de la materia de la cual se quiere obtener
#' la información.
#' @param sem_sig: Semestre del que se obtienen las simulaciones.
#'
#' @example num_sim <- 10
#' @example num_materia <- 5
#' @example sem_sig <- 20202
#' 
#' @return nom_lista_n_sim_1_materia: Variable tipo char con el nombre
#' de la lista que contiene las matrices con n simulaciones por materia.
#'
gen_nom_list_n_sim_1_materia <- function(num_sim,num_materia,sem_sig){
  ### Explicación del nombre:
  #' Se guarda en la carpeta "Listas mat_n_sim"
  #' "num_sim_sim" indica el número de simulaciones que tiene la lista
  #' "materia_num_materia" indica el número de materia de la simulación
  #' "sem_sig" indica el semestre del que se obtuvieron las simulaciones
  nom_lista_n_sim_1_materia <- paste0("Listas mat_n_sim/lista_mat_",
                                      num_sim,"_sim_materia_",num_materia,
                                      "_",sem_sig,".RData")
  return(nom_lista_n_sim_1_materia)
}


# gen_nom_list_n_sim_x_sem ------------------------------------------------
#' Title gen_nom_list_n_sim_x_sem Función que genera el nombre de los
#' archivos que guardan las listas de matrices con n simulaciones
#' para todas las materias.
#'
#' @param sem_sig: Semestre del que se obtienen las simulaciones.
#' @example sem_sig <- 20202
#'
#' @return nom_lista_n_sim_x_sem: Variable tipo char con el nombre
#' de la lista que contiene las listas de matrices con n simulaciones
#' para todas las materias.
#'
gen_nom_list_n_sim_x_sem <- function(sem_sig){
  ### Explicación del nombre:
  #' Se guarda en la carpeta "Listas mat_n_sim por semestre"
  #' "sem_sig" indica el semestre del que se obtuvieron las simulaciones
  nom_lista_n_sim_x_sem <- paste0("Listas mat_n_sim por semestre/lista_n_sim_por_sem_",
                                  sem_sig,".RData")
  
  return(nom_lista_n_sim_x_sem)
}


# gen_nom_heatmap ---------------------------------------------------------
#' Title gen_nom_heatmap: Función que genera los nombres de los heatmaps
#' para la varianza y diferencia relativa de las matrices obtenidas en
#' las pruebas.
#'
#' @param tipo: Indica si la gráfica es de varianza o de diferencia
#' relativa.
#' @param num_materia: Índice de la materia de la cual se quiere obtener
#' la información.
#' @param sem_sig: Semestre del que se obtienen las simulaciones.
#'
#' @example tipo <- "var_alum_materia" o "dif_relativa_alum_materia"
#' @example num_materia <- 5
#' @example sem_sig <- 20202
#'
#' @return nom_heatmap: Variable tipo char con el nombre de los heatmaps
#' para la varianza y diferencia relativa de las matrices obtenidas en
#' las pruebas.
#' 
gen_nom_heatmap <- function(tipo,num_materia,sem_sig){
  ### Explicación del nombre:
  #' Se guarda en la carpeta "Figuras/Matrices Simuladas"
  #' tipo puede ser "var_alum_materia" o "dif_relativa_alum_materia"
  #' "num_materia" indica el número de materia de la simulación
  #' "sem_sig" indica el semestre del que se obtuvieron las simulaciones
  nom_heatmap <- paste0("Figuras/Matrices Simuladas/heatmap_",tipo,"_",
                        num_materia,"_sem_",sem_sig,".jpeg")
  
  return(nom_heatmap)
}


# gen_mat_n_sim_n_sem -----------------------------------------------------------
#' Title gen_mat_n_sim_n_sem: Función que manda a llamar a la función
#' "gen_mat_n_sim_1_sem" para cada semestre de un vector dado. Se genera
#' una lista de listas para cada semestre en "vec_sem_sig". Esta función se
#' utiliza principalmente para hacer pruebas de que el modelo funciona.
#'
#' @param vec_sem_sig: Vector con los semestres de los que se desean obtener
#' las simulaciones.
#' @param vec_sem_ant; Vector con el número de semestres anteriores para
#' cada "sem_sig" del vector "vec_sem_sig"
#' @param num_sim: Número de matrices simuladas.
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#' 
#' @example vec_sem_sig <- c(20131,20152,20182,20201)
#' @example vec_sem_ant <- 20201
#' @example num_sim <- c(10,15,21,24)##Se inicia en 2008-1
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
gen_mat_n_sim_n_sem <- function(vec_sem_sig,vec_sem_ant,num_sim,param){
  #Se definen las variables que se van a utilizar
  ## Se carga el vector que contiene 333 materias para que todas las listas
  ##de cada semestre tengan las mismas materias.
  # load("vec_nom_materias_total.RData")
  Materias <- param$vec_nom_materias_total
  
  for(s in 1:length(vec_sem_sig)){#Recorre los semestres
    cat("\n Semestre simulado: ",vec_sem_sig[s])
    gen_mat_n_sim_1_sem(vec_sem_ant[s],vec_sem_sig[s],num_sim,param)
  }
}


# gen_mat_max_num_gpos_real -----------------------------------------------
#' Title gen_mat_max_num_gpos_real: Función que guarda y genera la matriz
#' "mat_max_num_gpos_real" que tiene 4 columnas (Semestre, Materia, Horario, 
#' Número de grupos) que contiene la información del máximo número de
#' grupos reales por semestre y por hora.
#'
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return mat_max_num_gpos_real: Matriz de 4 columnas: Semestre, Materia,
#' Horario, Número de grupos que contiene la información del máximo número de
#' grupos reales por semestre y por hora.
#'
gen_mat_max_num_gpos_real <- function(param){
  #Se definen las variables que se van a utilizar
  num_col_Materia <- arroja_ind_col_RG("Materia") ##1
  num_col_horario <- arroja_ind_col_RG("Horario") ##2
  col_grupos_reales <- arroja_ind_col_RG("Grupos_Reales") ##3
  mat_max_num_gpos_real <- data.frame(Semestre = 0,Materia = 0,Horario = 0,
                                      Núm.Gpos.Reales = 0)
  
  for(d in 1:length(param$nombre_sem)){
    Semestre <- param$nombre_sem[d]
    cat("\n sem_info = ",Semestre)
    
    nom_mat_real_gpos <- paste0("mat_real_grupos por semestre/mat_real_grupos_",Semestre,".RData")
    load(nom_mat_real_gpos)
    
    num_max_aux <- max(mat_real_grupos[,col_grupos_reales])
    mat_real_grupos_aux <- mat_real_grupos[mat_real_grupos[,col_grupos_reales]==num_max_aux,
                                           c(num_col_Materia,num_col_horario,col_grupos_reales)]
    
    ## Se agrega la variable de "semestre" y se acomodan las columnas
    if(dim(mat_real_grupos_aux)[1] == 1){
      mat_max_num_gpos_real <- rbind(mat_max_num_gpos_real,cbind(Semestre,mat_real_grupos_aux))
    }else{##Si hay más de un grupo con el número máximo de grupos
      for(k in 1:dim(mat_real_grupos_aux)[1]){
        mat_max_num_gpos_real <- rbind(mat_max_num_gpos_real,cbind(Semestre,mat_real_grupos_aux[k,]))
      }
    }
  }##Fin de for(d)
  
  #Se quita el renglón de ceros
  mat_max_num_gpos_real <- mat_max_num_gpos_real[-1,]
  
  View(mat_max_num_gpos_real)
  
  save(mat_max_num_gpos_real,file = "num_max_grupos/mat_max_num_gpos_real.RData")
  write.csv(mat_max_num_gpos_real,file = "num_max_grupos/mat_max_num_gpos_real.csv",row.names = F)
  return(mat_max_num_gpos_real)
}


# gen_mat_max_num_gpos_sim ------------------------------------------------
#' Title gen_mat_max_num_gpos_sim: Función que guarda y genera la matriz
#' "mat_max_num_gpos_sim" que tiene 4 columnas (Semestre, Materia, Horario, 
#' Número de grupos) que contiene la información del máximo número de
#' grupos simulados por semestre y por hora.
#'
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return mat_max_num_gpos_sim: Matriz de 4 columnas: Semestre, Materia,
#' Horario, Número de grupos que contiene la información del máximo número de
#' grupos simulados por semestre y por hora.
#'
gen_mat_max_num_gpos_sim <- function(sem_ini,sem_fin,param){
  #Se definen las variables que se van a utilizar
  num_col_Materia <- arroja_ind_col_SG("Materia") ##1
  num_col_horario <- arroja_ind_col_SG("Horario") ##2
  col_grupos_simulados <- arroja_ind_col_SG("Grupos_Simulados") ##3
  mat_max_num_gpos_sim <- data.frame(Semestre = 0,Materia = 0,Horario = 0,
                                     Núm.Gpos.Simulados = 0)
  
  semestres_sim <- (sem_ini:sem_fin)[(sem_ini:sem_fin) %% 10>0 &(sem_ini:sem_fin) %% 10<3]
  
  for(d in 1:length(semestres_sim)){
    sem_info <- semestres_sim[d]
    cat("\n sem_info = ",sem_info)
    
    nom_archivo <- paste0("Matrices m_grande_total/m_grande_total_20081_",sem_info,".RData")
    load(nom_archivo)
    param$m_grande_total = m_grande_total
    mat_simula_grupos <- guarda_mat_simula_grupos_1_sem(param)
    
    num_max_aux <- max(mat_simula_grupos[,col_grupos_simulados])
    mat_simula_grupos_aux <- mat_simula_grupos[mat_simula_grupos[,col_grupos_simulados]==num_max_aux,
                                               c(num_col_Materia,num_col_horario,col_grupos_simulados)]
    
    ## Se agrega la variable de "semestre" y se acomodan las columnas
    if((sem_info%%2)==0){
      Semestre <- param$sem_fin + 9
    }else{
      Semestre <- param$sem_fin + 1
    }
    if(dim(mat_simula_grupos_aux)[1] == 1){
      mat_max_num_gpos_sim <- rbind(mat_max_num_gpos_sim,cbind(Semestre,mat_simula_grupos_aux))
    }else{##Si hay más de un grupo con el número máximo de grupos
      for(k in 1:dim(mat_simula_grupos_aux)[1]){
        mat_max_num_gpos_sim <- rbind(mat_max_num_gpos_sim,cbind(Semestre,mat_simula_grupos_aux[k,]))
      }
    }
  }##Fin de for(d)
  
  
  #Se quita el renglón de ceros
  mat_max_num_gpos_sim <- mat_max_num_gpos_sim[-1,]
  
  # View(mat_max_num_gpos_sim)
  
  save(mat_max_num_gpos_sim,file = "num_max_grupos/mat_max_num_gpos_sim.RData")
  write.csv(mat_max_num_gpos_sim,file = "num_max_grupos/mat_max_num_gpos_sim.csv",row.names = F)
  
  return(mat_max_num_gpos_sim)
}



# gen_mat_nom_cap_salon_x_sem ---------------------------------------------
#' Title gen_mat_nom_cap_salon_x_sem: Función que genera la matriz llamada
#' "mat_nom_cap_salon" que contiene en su primer columna los nombres de los
#' salones de la facultad y en las siguientes columas se tienen todas sus
#' capacidades por semestre, desde el 2013-1 hasta el 20201.
#'
#' @return mat_nom_cap_salon: Matriz que contiene en su primer columna los
#' nombres de los salones de la facultad y en las siguientes columnas sus
#' diferentes capacidades (lugares disponibles por salón).
#'
gen_mat_nom_cap_salon_x_sem <- function(){
  #Se definen las variables que se van a utilizar
  num_col_salon <- arroja_ind_col_MG("Salon")##7
  #Se carga la matriz "m_grande" de los semestres 2013-1 al 2020-1
  nom_archivo_MGT <- paste0("Matrices m_grande_total/m_grande_total_20131_20201.RData")
  
  if(!file.exists(nom_archivo_MGT)){
    vec_excepciones <- "Inglés"
    param$sem_ini = 20131##Inicio de información real
    param$sem_fin = 20201##Fin de información real
    param$sem_sig = 20202##Semestre de simulación
    param$Semestres = (param$sem_ini:param$sem_fin)[(param$sem_ini:param$sem_fin) 
                                                    %% 10>0 &(param$sem_ini:param$sem_fin) %% 10<3]
    param$nombre_sem = as.character(param$Semestres)
    param$n_semestres_anteriores = length(param$Semestres)
    
    m_grande_total <- gen_m_grande_total(vec_excepciones,param)
    param$m_grande_total = m_grande_total
  }else{
    load(nom_archivo_MGT)
  }
  
  mat_nom_cap_salon <- data.frame(Salon = 0,Cap_20131 = 0,Cap_20132 = 0,
                                  Cap_20141 = 0,Cap_20142 = 0,
                                  Cap_20151 = 0,Cap_20152 = 0,
                                  Cap_20161 = 0,Cap_20162 = 0,
                                  Cap_20171 = 0,Cap_20172 = 0,
                                  Cap_20181 = 0,Cap_20182 = 0,
                                  Cap_20191 = 0,Cap_20192 = 0,
                                  Cap_20201 = 0)
  
  ## Se eligen los nombres de los salones que se van a utilizar (se eliminan
  ##los nombres diferentes como de personas o NA) por casos
  nombres_salon <- unique(m_grande_total[,num_col_salon])
  
  ### Se eliminan: Vacíos / NA / "Presentación" ###
  nombres_salon <- nombres_salon[nombres_salon != ""]
  nombres_salon <- nombres_salon[!is.na(nombres_salon)]
  nombres_salon <- nombres_salon[nombres_salon != "Presentación"]
  
  ### EDIFICIO O / EDIFICIO P###
  vec_ind_aux <- 0
  for(d in 1:length(nombres_salon)){
    renglon <- c(nombres_salon[d],rep(0,15))
    texto <- substr(nombres_salon[d],1,2)
    if(texto=="O1" || texto=="O2" || texto=="P1" || texto=="P2"){
      mat_nom_cap_salon <- rbind(mat_nom_cap_salon,renglon)
      vec_ind_aux <- c(vec_ind_aux,d)
    }
  }
  vec_ind_aux <- vec_ind_aux[-1]#Se quita el cero inicial
  nombres_salon <- nombres_salon[-vec_ind_aux]
  
  ### Inicio con números (0,1,2,3) ###
  vec_ind_aux <- 0
  for(d in 1:length(nombres_salon)){
    renglon <- c(nombres_salon[d],rep(0,15))
    texto <- substr(nombres_salon[d],1,1)
    for(num in 0:3){##Los salones que inician con número, empiezan con 0,1,2 o 3
      if(texto == num){
        mat_nom_cap_salon <- rbind(mat_nom_cap_salon,renglon)
        vec_ind_aux <- c(vec_ind_aux,d)
      }
    }
  }
  vec_ind_aux <- vec_ind_aux[-1]#Se quita el cero inicial
  nombres_salon <- nombres_salon[-vec_ind_aux]
  
  ### "Taller" ###
  vec_ind_aux <- 0
  for(d in 1:length(nombres_salon)){
    renglon <- c(nombres_salon[d],rep(0,15))
    texto <- substr(nombres_salon[d],1,6)
    if(texto=="Taller"){
      mat_nom_cap_salon <- rbind(mat_nom_cap_salon,renglon)
      vec_ind_aux <- c(vec_ind_aux,d)
    }
  }
  vec_ind_aux <- vec_ind_aux[-1]#Se quita el cero inicial
  nombres_salon <- nombres_salon[-vec_ind_aux]
  
  ### "Laboratorio" ###
  vec_ind_aux <- 0
  for(d in 1:length(nombres_salon)){
    renglon <- c(nombres_salon[d],rep(0,15))
    texto <- substr(nombres_salon[d],1,11)
    if(texto=="Laboratorio"){
      mat_nom_cap_salon <- rbind(mat_nom_cap_salon,renglon)
      vec_ind_aux <- c(vec_ind_aux,d)
    }
  }
  vec_ind_aux <- vec_ind_aux[-1]#Se quita el cero inicial
  nombres_salon <- nombres_salon[-vec_ind_aux]
  
  ### "Salón" ###
  vec_ind_aux <- 0
  for(d in 1:length(nombres_salon)){
    renglon <- c(nombres_salon[d],rep(0,15))
    texto <- substr(nombres_salon[d],1,5)
    if(texto=="Salón"){
      mat_nom_cap_salon <- rbind(mat_nom_cap_salon,renglon)
      vec_ind_aux <- c(vec_ind_aux,d)
    }
  }
  vec_ind_aux <- vec_ind_aux[-1]#Se quita el cero inicial
  nombres_salon <- nombres_salon[-vec_ind_aux]
  
  ### "Aula" ###
  vec_ind_aux <- 0
  for(d in 1:length(nombres_salon)){
    renglon <- c(nombres_salon[d],rep(0,15))
    texto <- substr(nombres_salon[d],1,4)
    if(texto=="Aula"){
      mat_nom_cap_salon <- rbind(mat_nom_cap_salon,renglon)
      vec_ind_aux <- c(vec_ind_aux,d)
    }
  }
  vec_ind_aux <- vec_ind_aux[-1]#Se quita el cero inicial
  nombres_salon <- nombres_salon[-vec_ind_aux]
  
  ### El salón S4 se agrega manualmente para esta matriz, dado que
  ###es el único caso con esas características
  # renglon <- 
  mat_nom_cap_salon <- rbind(mat_nom_cap_salon,c("S4",rep(0,15)))
  
  ## Se quita el renglón inicial de ceros
  mat_nom_cap_salon <- mat_nom_cap_salon[mat_nom_cap_salon[,1]!=0,]
  # View(mat_nom_cap_salon)
  
  ## Se llena el resto de la matriz (con las capacidades de los salones)
  sem_ini <- 20131
  sem_fin <- 20201
  semestres = (sem_ini:sem_fin)[(sem_ini:sem_fin) %% 10>0 &(sem_ini:sem_fin) %% 10<3]
  num_col_lugares <- arroja_ind_col_MG("Lugares")##5
  
  
  for(s in 2:(length(semestres)+1)){##Recorre los semestres
    sem_info <- semestres[s-1]
    nom_archivo <- paste0("m_grande por semestre/m_grande_",sem_info,".RData")
    load(nom_archivo)
    for(r in 1:dim(mat_nom_cap_salon)[1]){##Recorre los renglones de "mat_nom_cap_salon"
      nom_salon <- mat_nom_cap_salon[r,1]
      capacidades <- m_grande[m_grande[,num_col_salon] == nom_salon,num_col_lugares]
      ## En caso de no tener capacidad de salón se pone un -1 en la entrada (r,s)
      if(length(capacidades[!is.na(capacidades)]) == 0){
        mat_nom_cap_salon[r,s] <- -1
      }else{
        mat_nom_cap_salon[r,s] <- max(capacidades[!is.na(capacidades)])
      }
    }
  }
  # View(mat_nom_cap_salon)
  return(mat_nom_cap_salon)
}


# selecciona_cap_salon ----------------------------------------------------
#' Title selecciona_cap_salon: Función que arroja una matriz de dos
#' columnas, en la primera contiene los nombres de los salones de la
#' facultad y en la segunda la capacidad máxima de cada salón de
#' "mat_nom_cap_salon".
#'
#' @param mat_nom_cap_salon: Matriz que contiene en su primer columna los
#' nombres de los salones de la facultad y en las siguientes columnas sus
#' diferentes capacidades (lugares disponibles por salón).
#' 
#' @example mat_nom_cap_salon[25,] <- c("O132",0,-1,...,27,60)
#' 
#' @return mat_cap_salon: Matriz de dos columnas, en la primera contiene
#' los nombres de los salones de la facultad y en la segunda la capacidad
#' máxima de cada salón de "mat_nom_cap_salon".
#'
selecciona_cap_salon <- function(mat_nom_cap_salon){
  #Se definen las variables que se van a utilizar
  mat_cap_salon <- matrix(0,nrow = dim(mat_nom_cap_salon)[1],ncol = 2)
  mat_cap_salon[,1] <- mat_nom_cap_salon[,1]
  mat_aux <- mat_nom_cap_salon[,-1]
  
  for(d in 1:dim(mat_aux)[1]){##Recorre los renglones
    mat_cap_salon[d,2] <- max(as.numeric(mat_aux[d,]))
  }
  
  return(mat_cap_salon)
}



# gen_mat_materias_rep_1_sem ----------------------------------------------
#' Title gen_mat_materias_rep_1_sem: Función que recibe como parámetros
#' "sem_info" y "profesor", arroja una matriz de cuatro columnas (semestre,
#' profesor, materia, hora) en caso de que "profesor" tenga más de una
#' materia asignada a la misma hora.
#'
#' @param sem_info: Semestre del que se desea obtener información.
#' @param profesor: Nombre de algún profesor de la Facultad de Ciencias
#' 
#' @example sem_info <- 20182
#' @examples profesor <- "Arrigo Coen Coria"
#'
#' @return mat_materias_rep_1_sem: Matriz de cuatro columnas (semestre, profesor,
#' materia, hora) que tiene información en caso de que "profesor" tenga más
#' de una materia asignada a la misma hora, de un semestre.
#'
gen_mat_materias_rep_1_sem <- function(sem_info,profesor){
  #Se definen las variables que se van a utilizar:
  mat_materias_rep_1_sem <- data.frame(Semestre = 0,Profesor = 0,Materia = 0,Hora = 0)
  num_col_Materia <- arroja_ind_col_MG("Materia")##1
  num_col_Profesor <- arroja_ind_col_MG("Profesor")##2
  num_col_Horario <- arroja_ind_col_MG("Horario")##3
  num_col_horario_num <- arroja_ind_col_MG("horario_num")##4
  # num_col_Lugares <- arroja_ind_col_MG("Lugares")##5
  num_col_Alumnos <- arroja_ind_col_MG("Alumnos")##6
  num_col_Semestre <- arroja_ind_col_MG("Semestre")##11
  
  #Se carga la matriz "m_grande" de "sem_info"
  nom_archivo <- paste0("m_grande por semestre/m_grande_",sem_info,".RData")
  load(nom_archivo)
  
  #Se eliminan los renglones con información repetida
  m_grande = unique(m_grande)
  
  ##Se quitan los renglones vacíos de la matriz:
  m_grande <- m_grande[m_grande[1:(nrow(m_grande)),1]!=0,]
  
  #Se generan las matrices con la información de los grupos repetidos:
  # matriz_con_rep <- matrix(0,ncol = ncol(m_grande))
  # mat_resumen_rep <- matrix(0,ncol = 5)
  # Horarios <- unique(m_grande[,num_col_Horario])##String, no números
  Horarios <- unique(m_grande[,num_col_horario_num])##Números, no string
  
  prof_iguales <- m_grande[m_grande[,num_col_Profesor] == profesor, ]
  if(dim(prof_iguales)[1] > 1){
    for(h in 1:length(Horarios)){
      # horarios_iguales <- prof_iguales[prof_iguales[,num_col_Horario] == Horarios[h],]
      horarios_iguales <- prof_iguales[prof_iguales[,num_col_horario_num] == Horarios[h],]
      n_rep <- nrow(horarios_iguales)
      if(n_rep>1){
        num_alumnos <- horarios_iguales[,num_col_Alumnos]
        alum_diferentes <- 0 #Variable binaria, vale 0 si el número de alumnos es igual
        for(k in 2:n_rep){
          if(num_alumnos[1]!=num_alumnos[k]){
            alum_diferentes <- 1 #Variable binaria, vale 1 si el número de alumnos es diferente
          }
        }
        horarios_iguales <- horarios_iguales[,c(num_col_Semestre,num_col_Profesor,num_col_Materia,num_col_horario_num)]
        names(horarios_iguales) <- names(mat_materias_rep_1_sem)
        mat_materias_rep_1_sem <- rbind(mat_materias_rep_1_sem,horarios_iguales)
        # mat_resumen_rep <- rbind(mat_resumen_rep,c(sem_info,profesor,Horarios[h],n_rep,alum_diferentes))
      }
    }##Fin de for de Horarios
    ## Se quita el renglón inicial de ceros
    mat_materias_rep_1_sem <- mat_materias_rep_1_sem[mat_materias_rep_1_sem[,1]!=0,]
    # mat_resumen_rep <- mat_resumen_rep[mat_resumen_rep[,1]!=0,]
  }else{
    #No hay profesores con horarios iguales en diferentes materias
    mat_materias_rep_1_sem <- data.frame(Semestre = 0,Profesor = 0,Materia = 0,Hora = 0)
  }
  
  # View(mat_resumen_rep)
  return(mat_materias_rep_1_sem)
}


# gen_mat_materias_rep ----------------------------------------------------
#' Title gen_mat_materias_rep: Función que recibe como parámetros
#' "sem_ini" y "sem_fin", arroja una matriz de cuatro columnas (semestre,
#' profesor, materia, hora) en caso de que "profesor" tenga más de una
#' materia asignada a la misma hora, para todos los profesores en el
#' intervalo de semestres de "sem_ini" hasta "sem_fin".
#'
#' @param sem_ini: Semestre en el que se inicia la búsqueda de información.
#' @param sem_fin: Semestre en el que se finaliza la búsqueda de
#' información.
#'
#' @example sem_ini <- 20081
#' @example sem_fin <- 20181
#'
#' @return mat_materias_rep: Matriz de cuatro columnas (semestre, profesor,
#' materia, hora) que tiene información en caso de que "profesor" tenga más
#' de una materia asignada a la misma hora, de todos los profesores en los
#' semestres en el intervalo de semestres de "sem_ini" hasta "sem_fin".
#'
gen_mat_materias_rep <- function(param){
  #Se definen las variables que se van a utilizar:
  mat_materias_rep <- data.frame(Semestre = 0,Profesor = 0,
                                 Materia = 0,Hora = 0)
  m_grande_total <- param$m_grande_total
  # View(m_grande_total)
  num_col_prof <- arroja_ind_col_MG("Profesor")##2
  Profesores <- unique(m_grande_total[,num_col_prof])
  
  for(s in 1:length(semestres)){
    cat("\n s = ",s)
    for(p in 1:length(Profesores)){
      cat("\n p = ",p)
      # mat_materias_rep_1_sem <- gen_mat_materias_rep_1_sem(semestres[s],Profesores[p])
      mat_materias_rep_1_sem <- gen_mat_materias_rep_1_sem("20201",Profesores[p])
      mat_materias_rep <- rbind(mat_materias_rep,mat_materias_rep_1_sem)
    }
  }
  
  if(sum(as.numeric(mat_materias_rep[,1])) == 0){
    cat("\n No se encontraron profesores con horarios iguales en diferentes materias")
  }else{
    ## Se quitan los renglones de ceros
    mat_materias_rep <- mat_materias_rep[mat_materias_rep[,1]!=0,]
  }
  # View(mat_materias_rep)
  return(mat_materias_rep)
}


# checa_nom_1_materia_en_vec ----------------------------------------------
#' Title checa_nom_1_materia_en_vec: Función que recibe como parámetros el
#' nombre de una materia y el vector con los nombres de las materias e
#' imprime una lista con los diferentes nombres que pudiera tener "materia"
#' en "vec_nom_materias_total"
#' Por ejemplo "Estadística III" y "Modelos de Supervivencia y de Series
#' de Tiempo".
#'
#' @param materia: Nombre de algún curso impartido en la FC.
#' @param vec_nom_materias_total: Vector que contiene el nombre de las
#' materias sin repetición, conservando los nombres más recientes. 
#'
#' @example materia <- "Estadística III"
#' @example vec_nom_materias_total <- c("Robótica","Inglés VI","Sistemas
#' Dinámicos no Lineales",...,"Almacenes y Minería de Datos")
#'
checa_nom_1_materia_en_vec <- function(materia,vec_nom_materias_total){
  #' Se carga la matriz m_grande_total de 2008-1 a 2020-1 de la cual
  #' se va a obtener la lista de nombres que se desea
  load("Matrices m_grande_total/m_grande_total_20081_20201.RData")
  
  #Se definen las variables que se van a utilizar
  num_col_Materia <- arroja_ind_col_MG("Materia")##1
  num_col_NomMat_Act2000 <- arroja_ind_col_MG("NomMat_Act2000")##23
  num_col_NomMat_MAp2017 <- arroja_ind_col_MG("NomMat_MAp2017")##29
  vec_materias <- unique(m_grande_total[,num_col_Materia])##531
  
  var_aux <- vec_nom_materias_total[vec_nom_materias_total == materia]
  # var_aux <- vec_nom_materias_total[vec_nom_materias_total == "materia"]
  if(length(var_aux) != 0){#Si el nombre de "materia" se encuentra en el vector
    cat("\n\n La materia ",materia," se encuentra en el vector como ",materia)
  }else{
    ind_materia <- checa_ind_materia(materia,m_grande_total)
    
    if(length(ind_materia) != 0){#Si "materia" se encuentra en el vector con otro nombre
      mat_aux <- m_grande_total[ind_materia,c(num_col_Materia,
                                              num_col_NomMat_Act2000:num_col_NomMat_MAp2017)]
      mat_aux <- unique(mat_aux)
      vec_aux <- mat_aux[mat_aux != 0]
      nom_aux <- 0
      
      for(k in 1:length(vec_aux)){
        nom_aux <- c(nom_aux,vec_nom_materias_total[vec_nom_materias_total == vec_aux[k]])
      }
      if(length(nom_aux) > 1){
        #Se quita el cero inicial
        nom_aux <- nom_aux[-1]
        cat("\n\n La materia ",materia," se encuentra en el vector como \n",nom_aux) 
      }
    }else{#Si "materia" NO se encuentra en el vector
      cat("\n\n La materia ",materia," NO se encuentra en el vector")
      cat("\nAgregar la materia al vector: \n (1) SI \n (0) NO")
      agrega_materia = scan(file = "", what = numeric(), n = 1)
      
      if(agrega_materia == 1){
        vec_nom_materias_total <- c(vec_nom_materias_total,materia)
        save(vec_nom_materias_total, file = "vec_nom_materias_total.RData")
        cat("\n La materia ",materia," se agregó al vector")
      }else{
        cat("\n La materia ",materia," NO se agregó al vector")}}}
}


# checa_nom_materias_en_vec -----------------------------------------------
#' Title checa_nom_materias_en_vec: Función que recibe como parámetro el
#' vector con los nombres de las materias e imprime una lista con los
#' diferentes nombres que pudiera tener cada "materia" en "vec_nom_materias_total"
#' Por ejemplo "Estadística III" y "Modelos de Supervivencia y de Series
#' de Tiempo".
#'
#' @param vec_nom_materias_total: Vector que contiene el nombre de las
#' materias sin repetición, conservando los nombres más recientes. 
#'
#' @example vec_nom_materias_total <- c("Robótica","Inglés VI","Sistemas
#' Dinámicos no Lineales",...,"Almacenes y Minería de Datos")
#'
checa_nom_materias_en_vec <- function(vec_nom_materias_total){
  #' Se carga la matriz m_grande_total de 2008-1 a 2020-1 de la cual
  #' se va a obtener la lista de nombres que se desea
  load("Matrices m_grande_total/m_grande_total_20081_20201.RData")
  
  #Se definen las variables que se van a utilizar
  num_col_Materia <- arroja_ind_col_MG("Materia")##1
  vec_materias <- unique(m_grande_total[,num_col_Materia])##531
  
  for(d in 1:length(vec_materias)){
    materia <- vec_materias[d]
    checa_nom_1_materia_en_vec(materia,vec_nom_materias_total)
  }
}



##### PENDIENTES #####

# gen_mat_n_sim_1_sem -----------------------------------------------------
#' Title gen_mat_n_sim_1_sem: Función que arroja una lista con las
#' listas de matrices obtenidas en "gen_list_n_sim_1_materia" para "sem_sig".
#'
#' @param n_semestres_anteriores: Variable tipo "integer" que indica el
#' número de semestres anteriores a "sem_sig" para obtener la información
#' que se necesita para la simulación.
#' @param sem_sig: Semestre del que se obtienen las simulaciones
#' @param num_sim: Número de matrices simuladas.
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#' 
#' @example n_semestres_anteriores <- 10
#' @example sem_sig <- 20201
#' @example num_sim <- 20
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
#' @return lista_n_sim_por_sem: Lista con las listas de matrices
#' obtenidas en "gen_list_n_sim_1_materia" para "sem_sig".
#'
gen_mat_n_sim_1_sem <- function(num_sim,param){
  ##Se definen las variables que se van a utilizar:
  m_grande_total <- param$m_grande_total
  n_semestres_anteriores <- param$n_semestres_anteriores
  sem_sig <- param$sem_sig
  Materias <- param$vec_nom_materias_total
  lista_n_sim_por_sem <- list()
  nombres_lista <- 0
  
  for(d in 1:length(Materias)){#Recorre las materias
    materia <- Materias[d]
    num_materia <- d
    cat("\n Materia: ",num_materia,", ",materia)
    lista_n_sim_por_sem[[d]] <- gen_list_n_sim_1_materia(materia,num_sim,param)
    nombres_lista <- c(nombres_lista,paste0("lista_materia_sim_",d))}
  #Se quita el cero que está al inicio del vector de nombres
  nombres_lista <- nombres_lista[-1]
  names(lista_n_sim_por_sem) <- nombres_lista
  
  nom_lista_n_sim_x_sem <- gen_nom_list_n_sim_x_sem(sem_sig)
  save(lista_n_sim_por_sem,file = nom_lista_n_sim_x_sem)
  
  return(lista_n_sim_por_sem)
}


# guarda_mat_simula_grupos_1_sem ------------------------------------------------
#' Title guarda_mat_simula_grupos_1_sem: Función que guarda la matriz 
#' "mat_simula_grupos" la cual contiene 24 columnas: Materia, Horario,
#' Número de grupos simulados, Número de alumnos simulados, las últimas 20
#' columnas indican el número de simulaciones del tamaño de grupo, en sus
#' renglones se tiene el número de alumnos de cada grupo simulado.
#'
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return mat_simula_grupos: Matriz con 24 columnas: Materia, Horario,
#' Número de grupos simulados, Número de alumnos simulados, las últimas 20
#' columnas indican el número de simulaciones del tamaño de grupo, en sus
#' renglones se tiene el número de alumnos de cada grupo simulado.
#' 
guarda_mat_simula_grupos_1_sem <- function(param){
  #Se definen las variables que se van a utilizar
  # num_col <- arroja_ind_col_MG("Materia")
  # Materias <- unique(param$m_grande_total[,num_col])
  # load("vec_nom_materias_total.RData")
  Materias <- param$vec_nom_materias_total
  n_materias <- length(Materias)
  
  ##Se define la matriz como data frame para agregar renglones más fácilmente.
  mat_simula_grupos <- data.frame(Materia = 0,Horario = 0,Núm.Gpos.Simulados = 0,
                                  Núm.Al.Simulados = 0,Sim_1 = 0,
                                  Sim_2 = 0,Sim_3 = 0,Sim_4 = 0,Sim_5 = 0,
                                  Sim_6 = 0,Sim_7 = 0,Sim_8 = 0,Sim_9 = 0,
                                  Sim_10 = 0,Sim_11 = 0,Sim_12 = 0,Sim_13 = 0,
                                  Sim_14 = 0,Sim_15 = 0,Sim_16 = 0,Sim_17 = 0,
                                  Sim_18 = 0,Sim_19 = 0,Sim_20 = 0)
  
  for(d in 1:n_materias){
    cat("\nMateria ",d," de ",n_materias)
    materia <- Materias[d]
    
    mat_simula_grupos_una_materia <- gen_mat_simula_grupos_una_materia(materia,param)
    mat_simula_grupos <- rbind(mat_simula_grupos,mat_simula_grupos_una_materia)
  }
  
  ##Se quitan los renglones que tengan número de alumnos simulados = 0
  col_alum_sim_total <- arroja_ind_col_SG("Alumnos_Simulados_Totales") ##4
  mat_simula_grupos <- mat_simula_grupos[mat_simula_grupos[,col_alum_sim_total]>0,]
  
  sem_sig <- param$sem_sig
  # View(mat_simula_grupos)
  # nom_archivo <- paste0("mat_simula_grupos por semestre/mat_simula_grupos_",
  #                       sem_sig,"_V04.RData")
  nom_archivo <- paste0("mat_simula_grupos por semestre/mat_simula_grupos_",
                        sem_sig,".RData")
  save(mat_simula_grupos,file = nom_archivo)
  return(mat_simula_grupos)
}


# guarda_heatmap_1_sem ----------------------------------------------------
#' Title guarda_heatmap_1_sem: Función que guarda todas las imágenes de tipo
#' "jpeg" de las gráficas heatmap para las matrices de datos generadas.
#'
#' @param sem_sig: Semestre del que se obtienen las simulaciones
#' @param num_sim: Número de matrices simuladas.
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#'
#' @example sem_sig <- 20202
#' @example num_sim <- 5
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
#' @export jpeg: Imagen de las gráficas de heatmap obtenidas de todas
#' las matrices para cada materia.
#'
guarda_heatmap_1_sem <- function(num_sim,param){
  #Se definen las variables que se van a utilizar
  sem_sig <- param$sem_sig
  num_col_Materia <- arroja_ind_col_MG("Materia")
  num_col_1er_grupo <- arroja_ind_col_RG("col_1er_grupo") ##5
  num_col_ult_grupo <- arroja_ind_col_RG("col_ult_grupo") ##24
  # Materias <- unique(param$m_grande_total[,num_col_Materia])
  # load("vec_nom_materias_total.RData")
  Materias <- param$vec_nom_materias_total
  
  # Se guarda la matriz con el nombre de las materias para
  #saber la materia correspondiente a la gráfica
  # save(Materias,file = "Figuras/Matrices Simuladas/Materias.RData")
  # save(Materias,file = "Materias.RData")
  
  #Se guardan las figuras correspondientes a la varianza
  for(d in 1:length(Materias)){
    cat("\n heatmap ",d," de ",length(Materias))
    materia <- Materias[d]
    num_materia <- d
    
    nom_lista <- paste0("Listas mat_n_sim/lista_mat_",num_sim,"_sim_materia_",
                        num_materia,"_",sem_sig,".RData")
    # nom_lista <- paste0("Listas mat_n_sim_PRUEBAS/lista_mat_",num_sim,"_sim_materia_",
    #                     num_materia,"_",sem_sig,".RData")
    if(file.exists(nom_lista)){
      load(nom_lista)
    }else{
      lista_mat_n_sim <- gen_mat_n_sim_1_materia(materia,num_materia,num_sim,param)
    }
    
    mat_var_alum_x_materia_1_sem <- gen_mat_var_alum_x_materia_1_sem(lista_mat_n_sim,param)
    nom_archivo <- paste0("Figuras/Matrices Simuladas/heatmap_var_alum_materia_",d,"_sem_",sem_sig,".jpeg")
    # nom_archivo <- paste0("Figuras/Matrices Simuladas/heatmap_PRUEBAS_var_materia_",d,"_sem_",sem_sig,".jpeg")
    guarda_una_fig_heatmap(mat_var_alum_x_materia_1_sem,num_materia,nom_archivo)
  }
  
  # Se guardan las figuras correspondientes a la diferencia entre valores reales
  #y la esperanza.
  for(d in 1:length(Materias)){
    cat("\n heatmap ",d," de ",length(Materias))
    materia <- Materias[d]
    num_materia <- d
    
    nom_lista <- paste0("Listas mat_n_sim/lista_mat_",num_sim,"_sim_materia_",
                        num_materia,"_",sem_sig,".RData")
    # nom_lista <- paste0("Listas mat_n_sim_PRUEBAS/lista_mat_",num_sim,"_sim_materia_",
    #                     num_materia,"_",sem_sig,".RData")
    if(file.exists(nom_lista)){
      load(nom_lista)
    }else{
      lista_mat_n_sim <- gen_mat_n_sim_1_materia(materia,num_materia,num_sim,param)
    }
    mat_esp_alum_x_materia_1_sem <- gen_mat_esp_alum_x_materia_1_sem(lista_mat_n_sim,param)
    mat_real_grupos_una_materia <- gen_mat_real_grupos_una_materia(materia,sem_sig,param)
    mat_dif_alum_x_materia_1_sem <- gen_mat_dif_alum_x_materia_1_sem(mat_esp_alum_x_materia_1_sem,mat_real_grupos_una_materia)
    nom_archivo <- paste0("Figuras/Matrices Simuladas/heatmap_dif_real_esp_materia_",d,"_sem_",sem_sig,".jpeg")
    # nom_archivo <- paste0("Figuras/Matrices Simuladas/PRUEBAS_V01/heatmap_PRUEBAS_V02_dif_real_esp_materia_",d,"_sem_",sem_sig,".jpeg")
    guarda_una_fig_heatmap(mat_dif_alum_x_materia_1_sem,num_materia,nom_archivo)
    
    mat_real_aux <- mat_real_grupos_una_materia[,num_col_1er_grupo:num_col_ult_grupo]
    mat_real_aux <- as.matrix(mat_real_aux,nrow=dim(mat_real_aux)[1],ncol=dim(mat_real_aux)[1])
    mat_dif_relativa_alum_x_materia_1_sem <- matrix(0,nrow = dim(mat_real_aux)[1],
                                                    ncol = dim(mat_real_aux)[2])
    if(sum(as.numeric(mat_real_aux)) != 0){###SUMAR RENGLONES Y COLS
      #Se hace la división en caso de que la matriz de datos reales sea
      #distinta de cero en todas sus entradas
      # mat_dif_relativa_alum_x_materia_1_sem <- mat_dif_alum_x_materia_1_sem/mat_real_aux
      for(c in 1:dim(mat_real_aux)[2]){#Recorre columnas
        for(r in 1:dim(mat_real_aux)[1]){#Recorre renglones
          if(mat_dif_alum_x_materia_1_sem[r,c]==0){
            mat_dif_relativa_alum_x_materia_1_sem[r,c] <- 0
          }else{
            mat_dif_relativa_alum_x_materia_1_sem[r,c] <- mat_dif_alum_x_materia_1_sem[r,c]/mat_real_aux[r,c]
          }}#for r
      }#for c
    }else{
      mat_dif_relativa_alum_x_materia_1_sem <- mat_real_aux
    }
    nom_archivo_2 <- paste0("Figuras/Matrices Simuladas/heatmap_mat_dif_relativa_alum_materia_",d,"_sem_",sem_sig,".jpeg")
    # nom_archivo_2 <- paste0("Figuras/Matrices Simuladas/PRUEBAS_V01/heatmap_PRUEBAS_V02_mat_dif_relativa_alum_materia_",d,"_sem_",sem_sig,".jpeg")
    guarda_una_fig_heatmap(mat_dif_relativa_alum_x_materia_1_sem,num_materia,nom_archivo_2)
  }#for d
}


# guarda_heatmap_n_sem ----------------------------------------------------
#' Title guarda_heatmap_n_sem: Función que manda a llamar a la función
#' "guarda_heatmap_1_sem" la cual se encarga de graficar y guardar las
#' figuras de los "heatmaps" de las diferencias de alumnos, grupos y su
#' varianza.
#'
#' @param vec_sem_sig: Vector con los semestres de los que se desean obtener
#' las simulaciones. Deben estar ordenados de más antiguo a más reciente.
#' @param vec_sem_ant: Vector con el número de semestres anteriores para
#' cada "sem_sig" del vector "vec_sem_sig"
#' @param num_sim: Número de matrices simuladas.
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#'
#' @example vec_sem_sig <- c(20131,20152,20182,20201)
#' @example vec_sem_ant <- c(10,15,21,24)##Se inicia en 2008-1
#' @example num_sim <- 10
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#' 
guarda_heatmap_n_sem <- function(vec_sem_sig,vec_sem_ant,num_sim,param){
  ##Se definen las variables que se van a utilizar:
  num_col_Semestre <- arroja_ind_col_MG("Semestre")##11
  MGT <- param$m_grande_total
  nom_col_real_alum <- "Alumnos_Reales_Totales"
  nom_col_real_gpos <- "Grupos_Reales"
  mat_aux_real_alum <- matrix(0,nrow = 15,ncol = length(vec_sem_sig))
  mat_aux_real_gpos <- matrix(0,nrow = 15,ncol = length(vec_sem_sig))
  
  ##Se realiza un ciclo para cada semestre
  for(s in 1:length(vec_sem_sig)){
    # cat("\n s = ",s)
    for(d in 1:length(param$sem_totales)){
      # cat("\n d = ",d)
      if(param$sem_totales[d]==vec_sem_sig[s]){
        ind_sem_ini <- d-vec_sem_ant[s]
        # cat("\n sem_ini = ",param$sem_totales[ind_sem_ini])
        
        ind_sem_fin <- d-1
        # cat("\n sem_fin = ",param$sem_totales[ind_sem_fin])
      }
    }#Fin for(d)
    ##Se definen las variables de param que cambian:
    param$sem_ini = param$sem_totales[ind_sem_ini]
    param$sem_fin = param$sem_totales[ind_sem_fin]
    param$sem_sig = vec_sem_sig[s]
    param$Semestres = param$sem_totales[ind_sem_ini:ind_sem_fin]
    param$nombre_sem = as.character(param$Semestres)
    param$n_semestres_anteriores = length(param$Semestres)
    
    MG_aux_1 <- MGT[MGT[,num_col_Semestre]<= param$sem_fin,]
    MG_aux_2 <- MG_aux_1[MG_aux_1[,num_col_Semestre]>= param$sem_ini,]
    param$m_grande_total = MG_aux_2
    
    guarda_heatmap_1_sem(num_sim,param)
    
    ##Se obtienen las matrices que contienen los datos de alumnos totales
    ##reales por semestre
    nom_archivo_real <- paste0("mat_real_grupos por semestre/mat_real_grupos_",
                               vec_sem_sig[s],".RData")
    load(nom_archivo_real)
    mat_aux_real_alum[,s] <- gen_vec_suma_datos_real_1_sem(mat_real_grupos,
                                                           nom_col_real_alum,
                                                           param)
    mat_aux_real_gpos[,s] <- gen_vec_suma_datos_real_1_sem(mat_real_grupos,
                                                           nom_col_real_gpos,
                                                           param)
  }#Fin for(s)
  rownames(mat_aux_real_alum) <- param$nombre_hrs
  colnames(mat_aux_real_alum) <- vec_sem_sig
  rownames(mat_aux_real_gpos) <- param$nombre_hrs
  colnames(mat_aux_real_gpos) <- vec_sem_sig
  
  #Se cargan las matrices con las diferencias de datos
  mat_dif_total_alumnos_x_sem <- gen_mat_dif_total_alumnos_x_sem(vec_sem_sig,vec_sem_ant,
                                                                 num_sim,param)
  # View(mat_dif_total_alumnos_x_sem)
  mat_dif_total_gpos_x_sem <- gen_mat_dif_total_gpos_x_sem(vec_sem_sig,param)
  # View(mat_dif_total_gpos_x_sem)
  
  nom_archivo <- paste0("dif_relativa_total_de_alumnos_x_sem_",
                        vec_sem_sig[1],"-",vec_sem_sig[length(vec_sem_sig)])
  mat_dif_relativas_alum <- gen_mat_dif_relativas(mat_dif_total_alumnos_x_sem,
                                                  mat_aux_real_alum,nom_archivo)
  colnames(mat_dif_relativas_alum) <- vec_sem_sig
  rownames(mat_dif_relativas_alum) <- param$nombre_hrs
  View(mat_dif_relativas_alum)
  
  nom_archivo <- paste0("dif_relativa_total_de_gpos_x_sem_",
                        vec_sem_sig[1],"-",vec_sem_sig[length(vec_sem_sig)])
  mat_dif_relativas_gpos <- gen_mat_dif_relativas(mat_dif_total_gpos_x_sem,
                                                  mat_aux_real_gpos,nom_archivo)
  colnames(mat_dif_relativas_gpos) <- vec_sem_sig
  rownames(mat_dif_relativas_gpos) <- param$nombre_hrs
  View(mat_dif_relativas_gpos)
}



# simula_grupos -----------------------------------------------------------
#' Title simula_grupos: Función que arroja el vector con el número de grupos
#' simulados por hora, que depende del número de alumnos que se estimaron
#' con modelo hw() Holt-Winters y se simularon.
#'
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#' @param materia: Nombre de algún curso impartido en la FC.
#' 
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#' @example materia <- "Estadística I"
#'
#' @return vec_grupos_simulados: Vector con el número de grupos simulados
#' por hora
#'
simula_grupos <- function(materia,param){
  # cat("\n Entró a la función simula_grupos")
  nombre_hrs <- param$nombre_hrs
  vec_grupos_simulados <- rep(0,length(nombre_hrs))
  
  mat_simula_grupos <- guarda_mat_simula_grupos_1_sem(param)
  ##Se busca el vector con el número de grupos simulado de materia
  mat_info_grupos <- mat_simula_grupos[mat_simula_grupos[,1]==materia,c(2,3)]
  
  for(d in 1:length(nombre_hrs)){##Se recorren las horas
    for(j in 1:dim(mat_info_grupos)[1]){ ##Se recorren los renglones
      hora <- mat_info_grupos[j,1]
      if(hora==nombre_hrs[d]){
        vec_grupos_simulados[d] <- mat_info_grupos[j,2]
      }
    }
  }
  
  return(vec_grupos_simulados)
}


# guarda_mat_real_grupos_x_sem --------------------------------------------------
#' Title guarda_mat_real_grupos_x_sem: Función que guarda la matriz 
#' "mat_real_grupos" la cual contiene 24 columnas: Materia, Horario,
#' Número de grupos reales, Número de alumnos reales, las últimas 20
#' columnas indican el número de simulaciones del tamaño de grupo, en sus
#' renglones se tiene el número real de alumnos de cada grupo por hora.
#'
#' @param sem_info: Semestre del que se desea obtener información
#' @param param: Lista con los diferentes parámetros que se utlizan en las
#' funciones que se mandan llamar.
#' @example sem_info <- 20182
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return mat_real_grupos: Matriz con 24 columnas: Materia, Horario,
#' Número de grupos reales, Número de alumnos reales, las últimas 20
#' columnas indican el número de simulaciones del tamaño de grupo, en sus
#' renglones se tiene el número real de alumnos de cada grupo por hora.
#' 
guarda_mat_real_grupos_x_sem <- function(sem_info,param){
  #Se definen las variables que se van a utilizar
  num_col <- arroja_ind_col_MG("Materia")
  # Materias <- unique(param$m_grande_total[,num_col])
  # load("vec_nom_materias_total.RData")
  Materias <- param$vec_nom_materias_total
  n_materias <- length(Materias)
  
  ##Se define la matriz como data frame para agregar renglones más fácilmente.
  mat_real_grupos <- data.frame(Materia = 0,Horario = 0,
                                Núm.Gpos.Reales = 0,Núm.Al.Reales = 0,Grupo_1 = 0,
                                Grupo_2 = 0,Grupo_3 = 0,Grupo_4 = 0,Grupo_5 = 0,
                                Grupo_6 = 0,Grupo_7 = 0,Grupo_8 = 0,Grupo_9 = 0,
                                Grupo_10 = 0,Grupo_11 = 0,Grupo_12 = 0,Grupo_13 = 0,
                                Grupo_14 = 0,Grupo_15 = 0,Grupo_16 = 0,Grupo_17 = 0,
                                Grupo_18 = 0,Grupo_19 = 0,Grupo_20 = 0)
  
  for(d in 1:n_materias){
    # cat("\nMateria ",d," de ",n_materias)
    materia <- Materias[d]
    
    mat_real_grupos_una_materia <- gen_mat_real_grupos_una_materia(materia,sem_info,param)
    mat_real_grupos <- rbind(mat_real_grupos,mat_real_grupos_una_materia)
  }
  
  col_grupos_reales <- arroja_ind_col_RG("Grupos_Reales") ##3
  ##Se quitan los renglones que no tienen información de grupos:
  mat_real_grupos <- mat_real_grupos[mat_real_grupos[,col_grupos_reales]>0,]
  
  # View(mat_real_grupos)
  nom_mat_real_gpos <- paste0("mat_real_grupos por semestre/mat_real_grupos_",sem_info,".RData")
  save(mat_real_grupos,file = nom_mat_real_gpos)
  return(mat_real_grupos)
}



# datos_num_max_de_gpos_sim --------------------------------------------------
#' datos_num_max_de_gpos_sim: Función que regresa un vector con los valores del
#' número máximo de grupos simulados por semestre considerando que están
#' dividos por materia y horario.
#'
#' @param sem_ini: Semestre en el que se inicia la búsqueda de
#' información.
#' @param sem_fin: Semestre en el que se finaliza la búsqueda de
#' información para generar la asignación del siguiente semestre.
#'
#' @example sem_ini <- 20132
#' @example sem_fin <- 20202
#'
#' @return vec_num_max_gpos_sim: Vector con los valores del número máximo
#' de grupos simulados.
#'
datos_num_max_de_gpos_sim <- function(sem_ini,sem_fin){
  semestres <- (sem_ini:sem_fin)[(sem_ini:sem_fin) %% 10>0 &(sem_ini:sem_fin) %% 10<3]
  vec_num_max_gpos_sim <- rep(0,length(semestres))
  for(d in 1:length(semestres)){
    nom_archivo <- paste0("mat_simula_grupos por semestre/mat_simula_grupos_",semestres[d],".RData")
    load(nom_archivo)
    vec_num_max_gpos_sim[d] <- max(mat_simula_grupos[,3])
  }
  return(vec_num_max_gpos_sim)
}


# datos_num_max_de_gpos_real --------------------------------------------------
#' datos_num_max_de_gpos_real: Función que regresa un vector con los valores del
#' número máximo de grupos reales por semestre considerando que están
#' dividos por materia y horario.
#'
#' @param sem_ini: Semestre en el que se inicia la búsqueda de
#' información.
#' @param sem_fin: Semestre en el que se finaliza la búsqueda de
#' información para generar la asignación del siguiente semestre.
#'
#' @example sem_ini <- 20081
#' @example sem_fin <- 20201
#'
#' @return vec_num_max_gpos_real: Vector con los valores del número máximo
#' de grupos reales
#'
datos_num_max_de_gpos_real <- function(sem_ini,sem_fin){
  semestres <- (sem_ini:sem_fin)[(sem_ini:sem_fin) %% 10>0 &(sem_ini:sem_fin) %% 10<3]
  vec_num_max_gpos_real <- rep(0,length(semestres))
  col_grupos_reales <- arroja_ind_col_RG("Grupos_Reales") ##3
  for(d in 1:length(semestres)){
    nom_mat_real_gpos <- paste0("mat_real_grupos por semestre/mat_real_grupos_",semestres[d],".RData")
    load(nom_mat_real_gpos)
    
    vec_num_max_gpos_real[d] <- max(mat_real_grupos[,col_grupos_reales])
  }
  return(vec_num_max_gpos_real)
}


# guarda_una_mat_diferencias ---------------------------------------------
#' Title guarda_una_mat_diferencias: Función que arroja la matriz de diferencias
#' por semestre entre datos reales y datos simulados, la cual se utiliza para
#' hacer pruebas de comparación.
#'
#' @param sem_info: Semestre del que se desea obtener información. 
#' @example sem_info <- 20182
#'
#' @return mat_diferencias: Matriz con 24 columnas Materia, Horario,
#' Gpos_Real-Gpos_Sim, Alum_Real-Alum_Sim, las últimas 20 columnas tienen
#' las diferencias por grupos.
#'
guarda_una_mat_diferencias <- function(sem_info){
  ##Se toman las matrices con la inforación real y la simulada:
  nom_mat_sim_gpos <- paste0("Matrices simula_grupos/mat_simula_grupos_",sem_info,".RData")
  load(nom_mat_sim_gpos)
  nom_mat_real_gpos <- paste0("mat_real_grupos por semestre/mat_real_grupos_",sem_info,".RData")
  load(nom_mat_real_gpos)
  
  ##Se llena la columna 1: "Materia"
  vec_materias <- c(mat_real_grupos[,arroja_ind_col_RG("Materia")],
                    mat_simula_grupos[,arroja_ind_col_SG("Materia")])
  vec_horarios <- c(mat_real_grupos[,arroja_ind_col_RG("Horario")],
                    mat_simula_grupos[,arroja_ind_col_SG("Horario")])
  
  mat_materias_horario <- unique(cbind(vec_materias,vec_horarios))
  
  ## Se define la matriz "mat_diferencias" y se llenan las columnas 1 y 2
  num_renglones <- dim(mat_materias_horario)[1]
  mat_diferencias <- matrix(0,nrow = num_renglones,ncol = 24)
  mat_diferencias[,1:2] <- mat_materias_horario
  
  ##Se llenan las columnas 3-24
  col_1er_grupo_real <- arroja_ind_col_RG("col_1er_grupo") ##5
  col_ult_grupo_real <- arroja_ind_col_RG("col_ult_grupo") ##24
  col_1er_grupo_sim <- arroja_ind_col_SG("col_1er_grupo") ##5
  col_ult_grupo_sim <- arroja_ind_col_SG("col_ult_grupo") ##24
  for(d in 1:num_renglones){
    # cat("\nIteración ",d," de ",num_renglones)
    info_real <- 0
    cont_real <- 1
    while(info_real == 0 && cont_real <= dim(mat_real_grupos)[1]){
      # cat("\n  cont_real = ",cont_real)
      if(mat_diferencias[d,1]==mat_real_grupos[cont_real,arroja_ind_col_RG("Materia")] &&
         mat_diferencias[d,2]==mat_real_grupos[cont_real,arroja_ind_col_RG("Horario")]){
        
        num_gpos_real <- mat_real_grupos[cont_real,arroja_ind_col_RG("Grupos_Reales")]
        num_alum_real <- mat_real_grupos[cont_real,arroja_ind_col_RG("Alumnos_Reales_Totales")]
        vec_gpos_real <- c(mat_real_grupos[cont_real,col_1er_grupo_real:col_ult_grupo_real])
        
        info_real <- 1
      }
      cont_real <- cont_real + 1
    }
    
    ## En caso de que no haya información de esa materia en ese horario en la
    ##matriz "mat_real_grupos"
    if(info_real == 0){
      num_gpos_real <- 0
      num_alum_real <- 0
      vec_gpos_real <- rep(0,20)
    }
    
    info_sim <- 0
    cont_sim <- 1
    while(info_sim == 0 && cont_sim <= dim(mat_simula_grupos)[1]){
      # cat("\n   cont_sim = ",cont_sim)
      if(mat_diferencias[d,1]==mat_simula_grupos[cont_sim,arroja_ind_col_SG("Materia")] &&
         mat_diferencias[d,2]==mat_simula_grupos[cont_sim,arroja_ind_col_SG("Horario")]){
        
        num_gpos_sim <- mat_simula_grupos[cont_sim,arroja_ind_col_SG("Grupos_Simulados")]
        num_alum_sim <- mat_simula_grupos[cont_sim,arroja_ind_col_SG("Alumnos_Simulados_Totales")]
        vec_gpos_sim <- c(mat_simula_grupos[cont_sim,col_1er_grupo_sim:col_ult_grupo_sim])
        
        info_sim <- 1
      }
      cont_sim <- cont_sim + 1
    }
    
    ## En caso de que no haya información de esa materia en ese horario en la
    ##matriz "mat_simula_grupos"
    if(info_sim == 0){
      num_gpos_sim <- 0
      num_alum_sim <- 0
      vec_gpos_sim <- rep(0,20)
    }
    
    mat_diferencias[d,3] <- num_gpos_real - num_gpos_sim
    mat_diferencias[d,4] <- num_alum_real - num_alum_sim
    for(k in 5:24){
      mat_diferencias[d,k] <- as.numeric(vec_gpos_real[k-4]) - as.numeric(vec_gpos_sim[k-4])
    }
  }##Fin for(d)
  
  colnames(mat_diferencias) <- c("Materia","Horario","Gpos_Real-Gpos_Sim","Alum_Real-Alum_Sim",
                                 "Dif_Gpo_1","Dif_Gpo_2","Dif_Gpo_3","Dif_Gpo_4","Dif_Gpo_5",
                                 "Dif_Gpo_6","Dif_Gpo_7","Dif_Gpo_8","Dif_Gpo_9","Dif_Gpo_10",
                                 "Dif_Gpo_11","Dif_Gpo_12","Dif_Gpo_13","Dif_Gpo_14","Dif_Gpo_15",
                                 "Dif_Gpo_16","Dif_Gpo_17","Dif_Gpo_18","Dif_Gpo_19","Dif_Gpo_20")
  nom_mat_dif <- paste0("mat_diferencias por semestre/mat_diferencias_",sem_info,".RData")
  save(mat_diferencias,file = nom_mat_dif)
  nom_mat_dif_csv <- paste0("mat_diferencias por semestre/mat_diferencias_",sem_info,".csv")
  write.csv(mat_diferencias, file = nom_mat_dif_csv,row.names = F)
  # View(mat_diferencias)
  # return(mat_diferencias)
}


# guarda_mat_diferencias --------------------------------------------------
#' Title guarda_mat_diferencias: Función en la que se obtienen todas las
#' matrices "mat_diferencias" para cada semestre desde "sem_ini" hasta
#' "sem_fin".
#'
#' @param sem_ini: Semestre en el que se inicia la búsqueda de
#' información.
#' @param sem_fin: Semestre en el que se finaliza la búsqueda de
#' información para generar la asignación del siguiente semestre.
#'
#' @example sem_ini <- 20132
#' @example sem_fin <- 20201
#' 
guarda_mat_diferencias <- function(sem_ini,sem_fin){
  semestres <- (sem_ini:sem_fin)[(sem_ini:sem_fin) %% 10>0 &(sem_ini:sem_fin) %% 10<3]
  
  vec_para_for <- 1:length(semestres)
  pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
  for(k in vec_para_for){
    setTxtProgressBar(pb, k)
    sem_info <- semestres[k]
    
    guarda_una_mat_diferencias(sem_info)
  }
  close(pb)
}


# guarda_mat_esp_dif ------------------------------------------------------
#' Title guarda_mat_esp_dif: Función que guarda la matriz "mat_esp_dif" la cual
#' tiene la esperanza de los datos de los valores de la matriz de diferencias.
#'
#' @param sem_ini: Semestre en el que se inicia la búsqueda de
#' información.
#' @param sem_fin: Semestre en el que se finaliza la búsqueda de
#' información para generar la asignación del siguiente semestre.
#'
#' @example sem_ini <- 20132
#' @example sem_fin <- 20201
#' 
guarda_mat_esp_dif <- function(sem_ini,sem_fin){
  semestres <- (sem_ini:sem_fin)[(sem_ini:sem_fin) %% 10>0 &(sem_ini:sem_fin) %% 10<3]
  mat_esp_dif <- matrix(0,nrow = length(semestres),ncol = 23)
  mat_esp_dif[,1] <- semestres
  
  vec_para_for <- 1:length(semestres)
  pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
  for(k in vec_para_for){##Recorre los semestres
    setTxtProgressBar(pb, k)
    
    nom_mat_dif <- paste0("mat_diferencias por semestre/mat_diferencias_",semestres[k],".RData")
    load(nom_mat_dif)
    
    for(d in 3:dim(mat_diferencias)[2]){##Recorre las columnas
      mat_esp_dif[k,d-1] <- mean(as.numeric(mat_diferencias[,d]))
    }
  }
  close(pb)
  colnames(mat_esp_dif) <- c("Semestre","E[Gpos_Real-Gpos_Sim]",
                             "E[Alum_Real-Alum_Sim]","E[Gpo_1]","E[Gpo_2]","E[Gpo_3]",
                             "E[Gpo_4]","E[Gpo_5]","E[Gpo_6]","E[Gpo_7]","E[Gpo_8]",
                             "E[Gpo_9]","E[Gpo_10]","E[Gpo_11]","E[Gpo_12]","E[Gpo_13]",
                             "E[Gpo_14]","E[Gpo_15]","E[Gpo_16]","E[Gpo_17]","E[Gpo_18]",
                             "E[Gpo_19]","E[Gpo_20]")
  
  nom_mat_esp <- paste0("mat_dif_real-sim_esp_var_sd/mat_esp_dif.RData")
  save(mat_esp_dif,file = nom_mat_esp)
  
  ## Se guardan los datos en un archivo de excel para graficar
  write.csv(mat_esp_dif, file = "mat_dif_real-sim_esp_var_sd/mat_esp_dif.csv",row.names = F)
}


# guarda_mat_var_dif ------------------------------------------------------
#' Title guarda_mat_var_dif: Función que guarda la matriz "mat_var_dif" la
#' cual tiene la varianza de los datos de los valores de la matriz de
#' diferencias.
#'
#' @param sem_ini: Semestre en el que se inicia la búsqueda de
#' información.
#' @param sem_fin: Semestre en el que se finaliza la búsqueda de
#' información para generar la asignación del siguiente semestre.
#'
#' @example sem_ini <- 20132
#' @example sem_fin <- 20201
#' 
guarda_mat_var_dif <- function(sem_ini,sem_fin){
  semestres <- (sem_ini:sem_fin)[(sem_ini:sem_fin) %% 10>0 &(sem_ini:sem_fin) %% 10<3]
  mat_var_dif <- matrix(0,nrow = length(semestres),ncol = 23)
  mat_var_dif[,1] <- semestres
  
  vec_para_for <- 1:length(semestres)
  pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
  for(k in vec_para_for){##Recorre los semestres
    setTxtProgressBar(pb, k)
    
    nom_mat_dif <- paste0("mat_diferencias por semestre/mat_diferencias_",semestres[k],".RData")
    load(nom_mat_dif)
    
    for(d in 3:dim(mat_diferencias)[2]){##Recorre las columnas
      mat_var_dif[k,d-1] <- var(as.numeric(mat_diferencias[,d]))
    }
  }
  close(pb)
  
  colnames(mat_var_dif) <- c("Semestre","Var(Gpos_Real-Gpos_Sim)",
                             "Var(Alum_Real-Alum_Sim)","Var(Gpo_1)","Var(Gpo_2)",
                             "Var(Gpo_3)","Var(Gpo_4)","Var(Gpo_5)","Var(Gpo_6)",
                             "Var(Gpo_7)","Var(Gpo_8)","Var(Gpo_9)","Var(Gpo_10)",
                             "Var(Gpo_11)","Var(Gpo_12)","Var(Gpo_13)","Var(Gpo_14)",
                             "Var(Gpo_15)","Var(Gpo_16)","Var(Gpo_17)","Var(Gpo_18)",
                             "Var(Gpo_19)","Var(Gpo_20)")
  
  nom_mat_var <- paste0("mat_dif_real-sim_esp_var_sd/mat_var_dif.RData")
  save(mat_var_dif,file = nom_mat_var)
  ## Se guardan los datos en un archivo de excel para graficar
  write.csv(mat_var_dif, file = "mat_dif_real-sim_esp_var_sd/mat_var_dif.csv",row.names = F)
}


# guarda_mat_sd_dif ------------------------------------------------------
#' Title guarda_mat_sd_dif: Función que guarda la matriz "mat_sd_dif" la
#' cual tiene la desviación estándar de los datos de los valores de la
#' matriz de diferencias.
#'
#' @param sem_ini: Semestre en el que se inicia la búsqueda de
#' información.
#' @param sem_fin: Semestre en el que se finaliza la búsqueda de
#' información para generar la asignación del siguiente semestre.
#'
#' @example sem_ini <- 20132
#' @example sem_fin <- 20201
#' 
guarda_mat_sd_dif <- function(sem_ini,sem_fin){
  semestres <- (sem_ini:sem_fin)[(sem_ini:sem_fin) %% 10>0 &(sem_ini:sem_fin) %% 10<3]
  mat_sd_dif <- matrix(0,nrow = length(semestres),ncol = 23)
  mat_sd_dif[,1] <- semestres
  
  
  vec_para_for <- 1:length(semestres)
  pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
  for(k in vec_para_for){##Recorre los semestres
    setTxtProgressBar(pb, k)
    
    nom_mat_dif <- paste0("mat_diferencias por semestre/mat_diferencias_",semestres[k],".RData")
    load(nom_mat_dif)
    
    for(d in 3:dim(mat_diferencias)[2]){##Recorre las columnas
      mat_sd_dif[k,d-1] <- sd(as.numeric(mat_diferencias[,d]))
    }
  }
  close(pb)
  
  colnames(mat_sd_dif) <- c("Semestre","sd(Gpos_Real-Gpos_Sim)",
                            "sd(Alum_Real-Alum_Sim)","sd(Gpo_1)","sd(Gpo_2)",
                            "sd(Gpo_3)","sd(Gpo_4)","sd(Gpo_5)","sd(Gpo_6)",
                            "sd(Gpo_7)","sd(Gpo_8)","sd(Gpo_9)","sd(Gpo_10)",
                            "sd(Gpo_11)","sd(Gpo_12)","sd(Gpo_13)","sd(Gpo_14)",
                            "sd(Gpo_15)","sd(Gpo_16)","sd(Gpo_17)","sd(Gpo_18)",
                            "sd(Gpo_19)","sd(Gpo_20)")
  
  nom_mat_sd <- paste0("mat_dif_real-sim_esp_var_sd/mat_sd_dif.RData")
  save(mat_sd_dif,file = nom_mat_sd)
  ## Se guardan los datos en un archivo de excel para graficar
  write.csv(mat_sd_dif, file = "mat_dif_real-sim_esp_var_sd/mat_sd_dif.csv",row.names = F)
}


# guarda_mat_esp_sim ------------------------------------------------------
#' Title guarda_mat_esp_sim: Función que guarda la matriz "mat_esp_sim" la
#' cual tiene la esperanza de los datos de los valores de la matriz de datos
#' simulados.
#'
#' @param sem_ini: Semestre en el que se inicia la búsqueda de
#' información.
#' @param sem_fin: Semestre en el que se finaliza la búsqueda de
#' información para generar la asignación del siguiente semestre.
#'
#' @example sem_ini <- 20132
#' @example sem_fin <- 20201
#' 
guarda_mat_esp_sim <- function(sem_ini,sem_fin){
  semestres <- (sem_ini:sem_fin)[(sem_ini:sem_fin) %% 10>0 &(sem_ini:sem_fin) %% 10<3]
  mat_esp_sim <- matrix(0,nrow = length(semestres),ncol = 23)
  mat_esp_sim[,1] <- semestres
  
  vec_para_for <- 1:length(semestres)
  pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
  for(k in vec_para_for){##Recorre los semestres
    setTxtProgressBar(pb, k)
    
    nom_mat_dif <- paste0("mat_diferencias por semestre/mat_diferencias_",semestres[k],".RData")
    load(nom_mat_dif)
    
    for(d in 3:dim(mat_diferencias)[2]){##Recorre las columnas
      mat_esp_sim[k,d-1] <- mean(as.numeric(mat_diferencias[,d]))
    }
  }
  close(pb)
  colnames(mat_esp_sim) <- c("Semestre","E[Gpos_Sim]","E[Alum_Sim]",
                             "E[Gpo_1]","E[Gpo_2]","E[Gpo_3]","E[Gpo_4]",
                             "E[Gpo_5]","E[Gpo_6]","E[Gpo_7]","E[Gpo_8]",
                             "E[Gpo_9]","E[Gpo_10]","E[Gpo_11]","E[Gpo_12]",
                             "E[Gpo_13]","E[Gpo_14]","E[Gpo_15]","E[Gpo_16]",
                             "E[Gpo_17]","E[Gpo_18]","E[Gpo_19]","E[Gpo_20]")
  
  nom_mat_esp <- paste0("mat_gpos_sim_esp_var_sd/mat_esp_sim.RData")
  save(mat_esp_sim,file = nom_mat_esp)
  
  ## Se guardan los datos en un archivo de excel para graficar
  write.csv(mat_esp_sim, file = "mat_gpos_sim_esp_var_sd/mat_esp_sim.csv",row.names = F)
}


# guarda_mat_var_sim ------------------------------------------------------
#' Title guarda_mat_var_sim: Función que guarda la matriz "mat_var_sim" la
#' cual tiene la varianza de los datos de los valores de la matriz de datos
#' simulados.
#'
#' @param sem_ini: Semestre en el que se inicia la búsqueda de
#' información.
#' @param sem_fin: Semestre en el que se finaliza la búsqueda de
#' información para generar la asignación del siguiente semestre.
#'
#' @example sem_ini <- 20132
#' @example sem_fin <- 20201
#' 
guarda_mat_var_sim <- function(sem_ini,sem_fin){
  semestres <- (sem_ini:sem_fin)[(sem_ini:sem_fin) %% 10>0 &(sem_ini:sem_fin) %% 10<3]
  mat_var_sim <- matrix(0,nrow = length(semestres),ncol = 23)
  mat_var_sim[,1] <- semestres
  
  vec_para_for <- 1:length(semestres)
  pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
  for(k in vec_para_for){##Recorre los semestres
    setTxtProgressBar(pb, k)
    
    nom_mat_dif <- paste0("mat_diferencias por semestre/mat_diferencias_",semestres[k],".RData")
    load(nom_mat_dif)
    
    for(d in 3:dim(mat_diferencias)[2]){##Recorre las columnas
      mat_var_sim[k,d-1] <- var(as.numeric(mat_diferencias[,d]))
    }
  }
  close(pb)
  
  colnames(mat_var_sim) <- c("Semestre","Var(Gpos_Sim)","Var(Alum_Sim)","Var(Gpo_1)",
                             "Var(Gpo_2)","Var(Gpo_3)","Var(Gpo_4)","Var(Gpo_5)",
                             "Var(Gpo_6)","Var(Gpo_7)","Var(Gpo_8)","Var(Gpo_9)",
                             "Var(Gpo_10)","Var(Gpo_11)","Var(Gpo_12)","Var(Gpo_13)",
                             "Var(Gpo_14)","Var(Gpo_15)","Var(Gpo_16)","Var(Gpo_17)",
                             "Var(Gpo_18)","Var(Gpo_19)","Var(Gpo_20)")
  
  nom_mat_var <- paste0("mat_gpos_sim_esp_var_sd/mat_var_sim.RData")
  save(mat_var_sim,file = nom_mat_var)
  ## Se guardan los datos en un archivo de excel para graficar
  write.csv(mat_var_sim, file = "mat_gpos_sim_esp_var_sd/mat_var_sim.csv",row.names = F)
}


# guarda_mat_sd_sim ------------------------------------------------------
#' Title guarda_mat_sd_sim: Función que guarda la matriz "mat_sd_sim" la
#' cual tiene la desviación estándar de los datos de los valores de la
#' matriz datos simulados.
#'
#' @param sem_ini: Semestre en el que se inicia la búsqueda de
#' información.
#' @param sem_fin: Semestre en el que se finaliza la búsqueda de
#' información para generar la asignación del siguiente semestre.
#'
#' @example sem_ini <- 20132
#' @example sem_fin <- 20201
#' 
guarda_mat_sd_sim <- function(sem_ini,sem_fin){
  semestres <- (sem_ini:sem_fin)[(sem_ini:sem_fin) %% 10>0 &(sem_ini:sem_fin) %% 10<3]
  mat_sd_sim <- matrix(0,nrow = length(semestres),ncol = 23)
  mat_sd_sim[,1] <- semestres
  
  
  vec_para_for <- 1:length(semestres)
  pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
  for(k in vec_para_for){##Recorre los semestres
    setTxtProgressBar(pb, k)
    
    nom_mat_dif <- paste0("mat_diferencias por semestre/mat_diferencias_",semestres[k],".RData")
    load(nom_mat_dif)
    
    for(d in 3:dim(mat_diferencias)[2]){##Recorre las columnas
      mat_sd_sim[k,d-1] <- sd(as.numeric(mat_diferencias[,d]))
    }
  }
  close(pb)
  
  colnames(mat_sd_sim) <-  c("Semestre","sd(Gpos_Sim)","sd(Alum_Sim)","sd(Gpo_1)","sd(Gpo_2)",
                             "sd(Gpo_3)","sd(Gpo_4)","sd(Gpo_5)","sd(Gpo_6)","sd(Gpo_7)",
                             "sd(Gpo_8)","sd(Gpo_9)","sd(Gpo_10)","sd(Gpo_11)","sd(Gpo_12)",
                             "sd(Gpo_13)","sd(Gpo_14)","sd(Gpo_15)","sd(Gpo_16)","sd(Gpo_17)",
                             "sd(Gpo_18)","sd(Gpo_19)","sd(Gpo_20)")
  
  nom_mat_sd <- paste0("mat_gpos_sim_esp_var_sd/mat_sd_sim.RData")
  save(mat_sd_sim,file = nom_mat_sd)
  ## Se guardan los datos en un archivo de excel para graficar
  write.csv(mat_sd_sim, file = "mat_gpos_sim_esp_var_sd/mat_sd_sim.csv",row.names = F)
}




