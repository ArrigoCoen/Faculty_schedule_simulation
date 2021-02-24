##########################################################################
#' En este programa se encuentran las funciones y parámetros que se
#' requieren para la asignación de horarios y profesores para cada materia.
##########################################################################


##### PARÁMETROS INICIALES #####
#' Se instalan los paquetes necesarios para algunas funciones que se van
#' a utilizar.

# install.packages('zoo')
# install.packages('rvest')
# install.packages('dplyr')
# install.packages('purrr')
# install.packages('forecast')
# install.packages('xml2')
# install.packages('miceadds')
# install.packages('rJava')
# install.packages('xlsx')
# install.packages("writexl")
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
# install.packages('fitdistrplus')
# install.packages('stringdist')
# install.packages('plotGMM')
# install.packages('mixtools')
# install.packages('ggplot2')
# install.packages('randtests')
# install.packages('stats')
# install.packages('seastests')
# install.packages('tseries')
# install.packages('lmtest')
# install.packages('het.test')
# install.packages('magrittr')
# install.packages('dplyr')
# install.packages('resample')
# install.packages('ggpubr')
# install.packages('xtable')
# install.packages('readxl')
# install.packages('RColorBrewer')


#' Se cargan los paquetes
library(zoo)
library(rvest)
library(purrr)
library(forecast)
library(xml2)
library(stringr)
# library(xlsx)
library(writexl)
library(RColorBrewer)
library(astsa, quietly=TRUE, warn.conflicts=FALSE)
library(ggplot2)
library(knitr)
library(printr)
library(plyr)
library(lubridate)
library(gridExtra)
library(reshape2)
library(TTR)
library(randomcoloR)
library(manipulate)
library(fitdistrplus)
library(stringdist)
library(plotGMM)
library(mixtools)
library(ggplot2)
library(randtests)
library(stats)
library(seastests)
library(tseries)
library(lmtest)
# library(het.test)
library(magrittr)
library(dplyr)
library(resample)
library(ggpubr)
library(xtable)
library(readxl)


# param -------------------------------------------------------------------
#' Definimos una lista con distintos parámetros utilizados en las funciones.
param <- list()
param$sem_ini = 20081##Inicio de información real
param$sem_fin = 20201##Fin de información real
param$sem_sig = 20202##Semestre de simulación

#Vector con los semestres del 2008-1 al 2020-2:
param$sem_totales = (20081:20202)[(20081:20202)%% 10>0 &(20081:20202)%% 10<3]

#Vector con los semestres de "sem_ini" a "sem_fin":
param$Semestres = (param$sem_ini:param$sem_fin)[(param$sem_ini:param$sem_fin) 
                                                %% 10>0 
                                                &(param$sem_ini:param$sem_fin)
                                                %% 10<3]
param$nombre_sem = as.character(param$Semestres)#Semestres en tipo caracter

#Número de semestres anteriores al semestre de simulación:
param$n_semestres_anteriores = length(param$Semestres)

#Horas en las que se imparten las clases:
param$Horas = c(7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)
param$nombre_hrs = c("7-8","8-9","9-10","10-11","11-12",
                     "12-13","13-14","14-15","15-16",
                     "16-17","17-18","18-19","19-20",
                     "20-21","21-22")

#' Valores de los cuantiles de los intervalos de confianza para simular el
#' número de alumnos:
param$q1 = 85
param$q2 = 80

#' Nombres de las columnas para las matrices "m_grande":
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
param$m_grande_2015 = matrix(0,ncol = length(param$nom_cols_MG))
param$vec_nom_materias_total = 0
param$mat_nom_prof_total = 0
param$num_max_asig = 2 #Número máximo de materias asignadas por profesor
param$cota_TC = 1000 #Cota para ciclo
param$cota_asig = 6000 #Cota para ciclo
param$tam_poblacion = 25 #Tamaño de la población en el Algoritmo Genético (AG)
param$num_generaciones = 25 #Número de generaciones en el AG
param$prob_mutacion = 1/(6+18) #Probabilidad de mutación en el AG
param$n_cols_mat_calif = 2000 #Cota para el número de columnas
param$elige_TC = 0.7 #Probabilidad de elegir un profesor de tiempo completo en el AG
param$mat_info_AG = data.frame(Num_generaciones = 0,
                               Tam_pob = 0,
                               Tiempo = 0,
                               Mejor_calif = 0,
                               Num_genes_asig_fin = 0,
                               Calif_asig_fin = 0,
                               Prom_genes_gen1 = 0,
                               Prom_genes_generaciones = 0)

#' Se cargan los documentos para definir algunos parámetros

#Matriz con info real de "sem_ini" a "sem_fin":
load(file = paste0("m_grande_total_",param$sem_ini,"_",param$sem_fin,".RData"))
param$m_grande_total = m_grande_total 

#Vector con el nombre de las materias utilizadas:
load(file = "vec_nom_materias_total.RData")
param$vec_nom_materias_total = vec_nom_materias_total

#Matriz con el nombre de los profesores:
load(file = "mat_nom_prof_total.RData")
param$mat_nom_prof_total = mat_nom_prof_total

#Matriz con info real de 2015-1 a "sem_fin":
load("m_grande_total_20151_20201.RData")
param$m_grande_2015 = m_grande_total

#Matriz que guarda la información de las pruebas del AG:
load("mat_info_AG.RData")
param$mat_info_AG = mat_info_AG

# param_sim ---------------------------------------------------------------
#' Definimos una lista con distintos parámetros utilizados en las funciones
#' encargadas de las simulaciones.
param_sim <- list()
param_sim$vec_sem_sig = 20202
param_sim$k_sem_ant = 5#Número de semestres en la ventana de información
param_sim$Materias = c("Estadística III", #Puede ser una o más materias
                       "Teoría del Seguro",
                       "Cálculo Diferencial e Integral I",
                       "Investigación de Operaciones",
                       "Geometría Moderna I",
                       "Geometría Analítica II",
                       "Lógica Matemática I",
                       "Cálculo Diferencial e Integral III",
                       "Estadística I",
                       "Bases de Datos",
                       "Matemáticas Financieras",
                       "Cálculo Diferencial e Integral II",
                       "Probabilidad I",
                       "Probabilidad II",
                       "Procesos Estocásticos I")
param_sim$num_sim = 10#Número de simulaciones 

#Submatrices de "m_grande_total":
param_sim$m_filtrada = matrix(0,ncol = length(param$nom_cols_MG))
param_sim$sub_m_filtrada = matrix(0,ncol = length(param$nom_cols_MG))

#Matriz con las posibles combinaciones de q1 y q2:
param_sim$posibles_comb_q = matrix(c(80,85,95,99),ncol = 2,byrow = T)

# mat_def_columnas_MG -----------------------------------------------------
#' Se guarda la matriz "mat_def_columnas_MG" que tiene 3 columnas (Nombre,
#' Número, Descripción) y tantos renglones como columnas tenga la matriz
#' m_grande.
#' Contiene la descripción de las columnas de m_grande.

mat_def_columnas_MG <- matrix(0,ncol = 3,nrow = length(param$nom_cols_MG))
colnames(mat_def_columnas_MG) <- c("Nombre","Número","Descripción")
mat_def_columnas_MG[,1] <- param$nom_cols_MG
mat_def_columnas_MG[,2] <- 1:length(param$nom_cols_MG)
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



##### ÍNDICES #####
#' Funciones que encuentran los índices de las columnas para las diferentes
#' matrices que contienen información.

# arroja_ind_col_MG -------------------------------------------------------
#' Title arroja_ind_col_MG: Función que recibe el nombre de la columna que
#' se busca y devuelve el número de columna en "m_grande" con ese nombre.
#'
#' @param nombre_col: Nombre de la columna de "m_grande" de la que se busca
#' conocer el número de columna.
#'
#' @return num_col: Número de columna en "m_grande" con nombre "nombre_col".
#'
#' @examples
#' num_col <- arroja_ind_col_MG("Plan")
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
#' @param materia: Nombre de algún curso impartido en la Facultad de
#' Ciencias.
#' @param matriz: Puede ser "m_grande_total" o "m_grande" (para un
#'  semestre).
#'
#' @return ind_materia: Vector con los índices en los que hay coincidencia
#' entre "materia" y alguna de las columnas correspondientes.
#'
#' @examples
#' ind_materia <- checa_ind_materia("Probabilidad I",m_grande_total)
#' ind_materia <- checa_ind_materia("Estadística I",m_grande)
#' 
checa_ind_materia <- function(materia,matriz){
  #Se definen las variables que se van a utilizar
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


# My_plot_progress --------------------------------------------------------
#' Title My_plot_progress: Función que muestra una barra de progreso. Se
#' utiliza para visualizar el avance de una función.
#'
#' @param percentages: Vector de valores numericos entre cero y uno para
#' graficar la barra.
#' @param names_percentages: Vector con nombres de cada barra.
#' @param title_plot: Título del plot.
#'
#' @examples
#' Ver https://www.r-bloggers.com/multiple-progress-bars/
#' percentages <- runif(3)
#' names=c("uno","dos","tres")
#' title_plot <- "avance"
#' My_plot_progress(percentages,names,title_plot)
#' 
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




##### POSIBLES URL #####
#' Funciones que generan la lista de posibles URL de donde se va a extraer
#' la información de las páginas de la facultad

# Valida_list_url ---------------------------------------------------------
#' Validador de list_url: Función que valida la lista de URL de las páginas
#' de la Facultad de Ciencias.
#' @param list_url lista con las variables globales
#' @param sem_ini semestre inicial de información; eg. 20192
#' @param sem_fin semestre final de información; eg. 20201
#' @param sem_actual semestre actual de información; eg. 20201
#' @param Actualiza_RAW_url indicadora si se actualiza la matriz mat_RAW_url;
#'   utilizado por función Actualiza_list_url
#' @param Actualiza_limpia_base_url indicadora de limpiar la matriz
#'   mat_posibles_url; utilizado por función Actualiza_list_url par llamar a la
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
#' @return "T" si la variable "list_url" es adecuada. "F" si existen errores en la
#' lista "list_url".
#'
#' @examples
#' Valida_list_url()
#' 
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



# posibles_url ------------------------------------------------------------
#' Title: posibles_url: Función que arroja la lista "list_url"  dentro de
#' la cual se encuentra la matriz con las posibles URL de las páginas de
#' horarios de la FC. Dicha matriz tiene 6 columnas: Semestre, Plan,
#' Materia, URL, Grupos por página, url_con_salon. Las últimas 2 columnas
#' se llenan con la función "Actualiza_list_url"
#'
#' @param list_url: Lista con la información de posibles URL de la Facultad
#' de Ciencias.
#'
#' @return list_url: Lista con la información de posibles URL de la Facultad
#' de Ciencias.
#'
#' @examples
#' list_url <- posibles_url(list_url)
#' 
posibles_url <- function(list_url){
  
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
#' Title url_con1_sin0_info: Función que indica si una URL tiene o no
#' información.
#'
#' @param url: Página de internet de los horarios de la Facultad de Ciencias.
#'
#' @return 1 si la página tiene información, 0 si no.
#'
#' @examples
#' url_con1_sin0_info("http://www.fciencias.unam.mx/docencia/horarios/20182/2017/1541")
#' url_con1_sin0_info("http://www.fciencias.unam.mx/docencia/horarios/20081/1556/803")
#' 
url_con1_sin0_info <- function(url){
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
#' Title url_con_info: Función que verifica si cada URL tiene o no información.
#'
#' @param list_url: Lista con la información de posibles URL de la Facultad
#' de Ciencias.
#'
#' @return vec_con_info: Vector binario que indica si cada url tiene o no
#' información.
#'
#' @examples
#' vec_con_info <- url_con_info(list_url)
#' 
url_con_info <- function(list_url){
  if(file.exists(paste0(list_url$Carpeta_RData,"/Datos_vec_con_info_",
                        list_url$sem_ini,"_",list_url$sem_fin,".RData")) && 
     list_url$usa_vec_con_info_salvados) {
    cat("Se utilizara del archivo \n\t",
        paste0(list_url$Carpeta_RData,"/Datos_vec_con_info_",list_url$sem_ini,"_",
               list_url$sem_fin,".RData"),"\n\n")
    load(paste0(list_url$Carpeta_RData,"/Datos_vec_con_info_",list_url$sem_ini,"_",
                list_url$sem_fin,".RData"))
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
#' Title limpia_base_url: Función que quita las URL de "mat_posibles_url"
#' que no tienen información.
#'
#' @param list_url: Lista con la información de posibles URL de la Facultad
#' de Ciencias.
#'
#' @return list_url: Lista con la información de posibles URL de la Facultad
#' de Ciencias.
#'
#' @examples
#' list_url <- limpia_base_url(list_url)
#' 
limpia_base_url <- function(list_url){
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
#' Title elimina_grupos_con_0: Función que elimina páginas sin grupos.
#'
#' @param list_url: Lista con la información de posibles URL de la Facultad
#' de Ciencias.
#'
#' @return list_url: Lista con la información de posibles URL de la Facultad
#' de Ciencias.
#'
#' @examples
#' list_url <- elimina_grupos_con_0(list_url)
#' 
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
#' Title genera_num_grupos: Función que genera el vector con el número de
#' grupos de cada página web.
#'
#' @param list_url: Lista con la información de posibles URL de la Facultad
#' de Ciencias.
#'
#' @return list_url: Lista con la información de posibles URL de la Facultad
#' de Ciencias.
#'
#' @examples
#' list_url <- genera_num_grupos(list_url)
#' 
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
#' Title url_con1_sin0_salon: Función revisa si en la URL hay grupos con o
#' sin salón por grupo.
#' 
#' @param url: Página de internet de los horarios de la Facultad de Ciencias.
#'
#' @return 1 si la página tiene información del salón, 0 si no.
#'
#' @example
#' url_con1_sin0_salon("http://www.fciencias.unam.mx/docencia/horarios/20182/2017/1541")
#' url_con1_sin0_salon("http://www.fciencias.unam.mx/docencia/horarios/20081/119/4")
#' 
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
#' Title actualiza_list_url_con_salon: Función que actualiza la lista
#' "list_url" con datos de los salones.
#' 
#' @param list_url: Lista con la información de posibles URL de la Facultad
#' de Ciencias.
#'
#' @return list_url: Lista con la información de posibles URL de la Facultad
#' de Ciencias.
#'
#' @example
#' actualiza_list_url_con_salon(list_url)
#' 
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
#' Title Actualiza_list_url: Función que actualiza la lista "list_url".
#' 
#' @param list_url: Lista con la información de posibles URL de la Facultad
#' de Ciencias.
#'
#' @return list_url: Lista con la información de posibles URL de la Facultad
#' de Ciencias.
#'
#' @example
#' Actualiza_list_url(list_url)
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
    colnames(list_url$mat_Grande) <- c("Materia", "Profesor","Horario",
                                       "Lugares","Alumnos","Salon",
                                       "Grupo","Carrera","Plan","Semestre",
                                       "Cambios","Turno","Semestre_de_materia")
    list_url$mat_Grande_con_url <- matrix(-1,sum(as.numeric(list_url$mat_posibles_url[,5])),
                                          list_url$ncol_mat_Grande+1)
    
    colnames(list_url$mat_Grande_con_url) <- c("Materia", "Profesor","Horario",
                                               "Lugares","Alumnos","Salon",
                                               "Grupo","Carrera","Plan","Semestre",
                                               "Cambios","Turno",
                                               "Semestre_de_materia","url")
  }
  
  if(list_url$Salvar_URL_RData) {
    cat("---- Salvando variables\n")
    save(list_url, file = list_url$file_name)
    cat("Se guardó el archivo:\n\t", list_url$file_name,"\n")
    cat("OK --- Fin de salvado de variables\n")
  }
  
  cat("La función Actualiza_list_url tomó: ", (proc.time()-ptm)[3]/60,
      " minutos\n\n\n" )
  return(list_url)
}



##### EXTRAER INFORMACIÓN #####
#' Funciones que generan las matrices "m_grande" y "m_grande_total" con
#' la información de las páginas de la Facultad de Ciencias.


# borra_i_posible_grupo ---------------------------------------------------
#' Title borra_i_posible_grupo: Función que borra renglones de "vec" que
#' contengan "string_a_buscar".
#'
#' @param string_a_buscar: String con las palabras que se quieren borrar
#' de un grupo.
#' @param vec: String con el que se compara "string_a_buscar".
#'
#' @return vec: String corregido.
#'
#' @examples
#' borra_i_posible_grupo("especial",vec)
#' borra_i_posible_grupo("Especial",vec)
#' borra_i_posible_grupo("paralelo a ",vec)
#' borra_i_posible_grupo("xtraordinario por etapas",vec)
#' 
borra_i_posible_grupo <- function(string_a_buscar,vec){
  i_a_borrar <- grep(string_a_buscar, vec)
  if(length(i_a_borrar)>0) {
    vec <- vec[-(c(i_a_borrar,i_a_borrar-1))]
  }
  return(vec)
}



# corrige_i_posible_grupo -------------------------------------------------
#' Title corrige_i_posible_grupo: Función que une la entrada anterior y la
#' que contiene a "string_a_buscar".
#'
#' @param string_a_buscar: String con las palabras que se quieren borrar
#' de un grupo.
#' @param vec: String con el que se compara "string_a_buscar".
#'
#' @return vec: String corregido.
#'
#' @examples
#' corrige_i_posible_grupo("exclusivo para ",vec)
#' corrige_i_posible_grupo("semipresencial",vec)
#' corrige_i_posible_grupo("nuevo aprobado por el CT",vec)
#' 
corrige_i_posible_grupo <- function(string_a_buscar,vec){
  i_corrige <- grep(string_a_buscar, vec)
  if(length(i_corrige)>0) {
    aux <- vec
    for(ii in 1:length(i_corrige)){
      aux[i_corrige[ii]-1] <- paste(vec[i_corrige[ii]-1],
                                    vec[i_corrige[ii]],collapse = "")
    }
    aux <- aux[-i_corrige]
    vec <- aux
  }
  return(vec)
}


# imprime_errores_mat_info_k_pag ------------------------------------------
#' Title imprime_errores_mat_info_k_pag: Función que imprime los errores que
#' tiene la matriz "mat_info_k_pag".
#'
#' @param mat_info_k_pag: Matriz con la información de "k" diferentes URL.
#' @param list_url: Lista con la información de posibles URL de la Facultad
#' de Ciencias.
#'
#' @examples
#' imprime_errores_mat_info_k_pag(mat_info_k_pag,list_url)
#' 
imprime_errores_mat_info_k_pag <- function(mat_info_k_pag,list_url){
  
  col_names_tabla <- c("Materia","Profesor","Horario","Lugares","Alumnos","Salon  ",
                       "Grupo  ","Carrera","Plan  ", "Semestre","Cambios","Turno  ",
                       "Sem_de_mater","url    ")
  cat(" --- La dimension de mat_info_k_pag es ",dim(mat_info_k_pag)," ---\n",
      "------------------------------------------------------------------------------\n",
      "\t","Nombre","\t# vacios","\t% vacios","\t# NA","\t% NA","\n",
      "------------------------------------------------------------------------------\n")
  for(i in 1:ncol(mat_info_k_pag)) {
    porciento_vacios <- as.character(round(sum(mat_info_k_pag[,i]=="",
                                               na.rm = T)*100/nrow(mat_info_k_pag),
                                           digits = 2))
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
#' Title extrae_info_1_pag: Función que extrae la información de una página
#' con los horarios de la Facultad de Ciencias.
#'
#' @param url: Página de internet de los horarios de la Facultad de Ciencias.
#'
#' @return mat_info_un_pag: Matriz con 14 columnas (Materia, Profesor,
#' Horario, Lugares, Alumnos, Salon, Grupo, Carrera, Plan, Semestre,
#' Cambios, Turno, Semestre_de_materia, url)
#'
#' @examples
#' extrae_info_1_pag("http://www.fciencias.unam.mx/docencia/horarios/20182/2017/143")
#' extrae_info_1_pag("http://www.fciencias.unam.mx/docencia/horarios/20182/2017/1541")
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


##### LIMPIEZA DE DATOS #####
#' Funciones que limpian las matrices m_grande de cada semestre. Se combinan
#' los grupos repetidos.


# limpia_m_grande ---------------------------------------------------------
#' Title limpia_m_grande: Genera la matriz "m_grande" para cada semestre,
#' sin grupos repetidos.
#'
#' @param sem_info: Semestre del que se desea obtener información.
#' @param mat_info_k_pag: Matriz de 14 columnas (Materia,Profesor,Horario,
#' Lugares,Alumnos,Salon,Grupo,Carrera,Plan,Semestre,Cambios,Turno,
#' Semestre_de_materia,url), con la información de cada semestre.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
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
#' @examples
#' m_grande <- limpia_m_grande(sem_info,mat_info_k_pag,param)
#' 
limpia_m_grande <- function(sem_info,mat_info_k_pag,param){
  #Se eliminan los renglones con información repetida
  mat_info_k_pag = unique(mat_info_k_pag)
  
  ##Se quitan los renglones vacíos de la matriz:
  mat_info_k_pag <- matrix(mat_info_k_pag[mat_info_k_pag[1:(nrow(mat_info_k_pag)),
                                                         1]!=0,],
                           ncol = dim(mat_info_k_pag)[2])
  
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
  colnames(mat_resumen_rep) <- c("Semestre","Profesor","Horario","Repeticiones",
                                 "AlumDif")
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
        renglon[num_col_Materia] <- paste(renglon[num_col_Materia],
                                          matriz_con_rep[d,1],sep = "/")}
      
      ##Grupo:
      if(cambio_grupo == 1){
        renglon[num_col_Grupo] <- paste(renglon[num_col_Grupo],matriz_con_rep[d,7],
                                        sep = "/")}
      
      ##Carrera
      if(cambio_carrera == 1){
        renglon[num_col_Carrera] <- paste(renglon[num_col_Carrera],
                                          matriz_con_rep[d,8],sep = "/")}
      
      ##Plan
      if(cambio_plan == 1){
        renglon[num_col_Plan] <- paste(renglon[num_col_Plan],matriz_con_rep[d,9],
                                       sep = "/")}
      
      ##Semestre de materia
      renglon[num_col_Semestre_de_materia] <- paste(renglon[num_col_Semestre_de_materia],
                                                    matriz_con_rep[d,13],sep = "/")
      
      ##URL
      renglon[num_col_url] <- paste(renglon[num_col_url],matriz_con_rep[d,14],
                                    sep = "/")
      
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
#' @examples
#' m_grande <- actualiza_m_grande_1_sem(sem_info,param)
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
#' @param nom_archivo: Nombre del archivo que se va a cargar con la
#' matriz "m_grande".
#' @param sem_info: Semestre del que se desea obtener información.
#' @param idioma: Variable de tipo "char" la cual indica el idioma del
#' que se debe tomar la información.
#'
#' @return mod_1si_0no: Variable binaria que indica si la matriz "m_grande"
#' del semestre "sem_info" requiere modificación.
#'
#' @examples
#' nom_archivo <- "m_grande por semestre SIN INGLES/m_grande_SIN_ING_20182.RData"
#' sem_info <- 20182
#' idioma <- "Inglés"
#' imprime_info_idiomas_1_sem(nom_archivo,sem_info,idioma)
#' 
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
#' @return vec_sem_idiomas: Vector con los semestres que requieren
#' modificación en "m_grande".
#' 
#' @examples 
#' imprime_info_idiomas <- function("Inglés")
#' 
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
#' @examples
#' m_grande <- revisa_gpos_idiomas_1_sem(20182,"Inglés")
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
#'
#' @return m_grande
#' 
#' @examples m_grande <- actualiza_col_cambios(m_grande)
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
#' @examples
#' m_grande_SNM <- gen_m_grande_SIN_Num_Materia(20182,c("Inglés"),param)
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
#' @examples
#' vec_nom_materias_total <- gen_vec_nom_materias_total()
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
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
#' @return m_grande: Matriz de 37 columnas (las de "m_grande_SNM" más
#' la columna ), con la información de "sem_info".
#'
#' @examples
#' m_grande <- agrega_col_num_materia(m_grande_SNM,param)
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
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
#' @return direccion_info: Nombre de la ubicación en la que se
#' encuentra guardada la matriz "m_grande".
#'
#' @examples
#' direccion_info <- gen_m_grande(20182,c("Inglés"),param)
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
  #' Se agregaron 2 renglones a m_grande_20201 //2020/10/29
  # load("m_grande por semestre/m_grande_20201.RData")
  # r_1 <- c("Taller de Redacción","María Azucena Rivera Vidal","8:30 a 12:30",8,0,9,
  #          "303 (Nuevo Edificio)",6020,"Matemáticas Aplicadas",2017,20201,"1","M",
  #          "Quinto Semestre","http://www.fciencias.unam.mx/docencia/horarios/20201/2055/899",
  #          0,0,0,0,0,0,1,0,0,0,0,0,0,"Taller de Redacción",0,0,0,0,0,0,
  #          "http://www.fciencias.unam.mx/docencia/horarios/20201/2055/899",334)
  # r_2 <- c("Solución Numérica de Ecuaciones Diferenciales Ordinarias","	Gerardo Mejía Rodríguez",
  #          "8 a 9	",8,0,6,"Taller de Àlgebra",6019,"Matemáticas Aplicadas",2017,20201,"1","M",
  #          "Optativas Requeridas","http://www.fciencias.unam.mx/docencia/horarios/20201/2055/930",
  #          0,0,0,0,0,0,1,0,0,0,0,0,0,"Solución Numérica de Ecuaciones Diferenciales Ordinarias",
  #          0,0,0,0,0,0,"http://www.fciencias.unam.mx/docencia/horarios/20201/2055/930",335)
  # m_grande <- rbind(m_grande,r_1)
  # m_grande <- rbind(m_grande,r_2)
  # 
  # nom_file <- "C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal/m_grande por semestre/m_grande_20201.RData"
  # save(m_grande, file = nom_file)
  
  save(m_grande, file = direccion_info)
  return(direccion_info)
}



##### CORREGIR MATERIAS #####
#' Funciones encargadas de corregir los nombres de las materias para que
#' no haya repeticiones.


# gen_mat_nom_materias_total ----------------------------------------------
#' Title gen_mat_nom_materias_total: Función que guarda la matriz
#' "mat_nom_materias_total" que contiene los nombres de las materias que se
#' van a utilizar en la simulación y todos los posibles nombres con los que
#' aparece.
#' NOTA: En algunos casos se agruparon materias por falta de nombre en los
#' horarios. Se buscaron materias similares para juntarlas.
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
#' @return mat_nom_materias_total: Matriz que contiene los nombres de las
#' materias que se van a utilizar en la simulación y todos los posibles
#' nombres con los que aparece.
#'
#' @examples
#' mat_nom_materias_total <- gen_mat_nom_materias_total(param,param_sim)
#' 
gen_mat_nom_materias_total <- function(param,param_sim){
  #se carga la lista con el nombre de las materias por carrera
  load("lista_mat_materias_x_carrera.RData")
  #Se definen las variables que se van a utilizar
  materias_act <- lista_mat_materias_x_carrera[[1]]
  materias_CdC <- lista_mat_materias_x_carrera[[2]]
  materias_mate <- lista_mat_materias_x_carrera[[3]]
  materias_mateAp <- lista_mat_materias_x_carrera[[4]]
  mat_nombres_carreras <- unique(rbind(materias_act,materias_CdC,materias_mate,materias_mateAp))
  
  vec_nom_materias_total <- param$vec_nom_materias_total
  m_grande_2015 <- param$m_grande_2015
  param$m_grande_total = m_grande_2015
  param_sim$k_sem_ant = 11
  param_sim$vec_sem_sig = 20202
  num_col_Materia <- arroja_ind_col_MG("Materia")
  num_col_NomMat_Act2000 <- arroja_ind_col_MG("NomMat_Act2000")
  num_col_NomMat_MAp2017 <- arroja_ind_col_MG("NomMat_MAp2017")
  ind_quitar <- 0
  cont <- 1
  mat_nom_materias <- data.frame(Materia = vec_nom_materias_total,
                                 Num_Materia = 1:length(vec_nom_materias_total),
                                 Nom1 = 0,Nom2 = 0,Nom3 = 0,Nom4 = 0,Nom5 = 0,
                                 Nom6 = 0,Nom7 = 0,Nom8 = 0,Nom9 = 0,Nom10 = 0,
                                 Nom11 = 0,Nom12 = 0,Nom13 = 0,Nom14 = 0,Nom15 = 0,
                                 Nom16 = 0,Nom17 = 0,Nom18 = 0,Nom19 = 0,Nom20 = 0)
  mat_nom_materias_total <- data.frame(Materia = 0,
                                       Num_Materia = 1:length(vec_nom_materias_total),
                                       Nom1 = 0,Nom2 = 0,Nom3 = 0,Nom4 = 0,Nom5 = 0,
                                       Nom6 = 0,Nom7 = 0,Nom8 = 0,Nom9 = 0,Nom10 = 0,
                                       Nom11 = 0,Nom12 = 0,Nom13 = 0,Nom14 = 0,Nom15 = 0,
                                       Nom16 = 0,Nom17 = 0,Nom18 = 0,Nom19 = 0,Nom20 = 0)
  
  for(d in 1:length(vec_nom_materias_total)){
    # cat("\n = ",d)
    materia <- vec_nom_materias_total[d]
    param_sim$Materias = materia
    matriz <- gen_mat_m_filtrada(param,param_sim)
    
    if(dim(matriz)[1] == 0){
      cat("\nEliminar ",materia," de la lista, tiene índice ",d)
      ind_quitar <- c(ind_quitar,d)
    }else{
      nom_materias <- unique(matriz[,c(num_col_Materia,
                                       num_col_NomMat_Act2000:num_col_NomMat_MAp2017)])
      nom_materias <- unique(nom_materias[nom_materias!=0])
      if(d == mat_nombres_carreras[order(mat_nombres_carreras[,2]),][cont,2]){
        nom_materias <- c(nom_materias,
                          mat_nombres_carreras[order(mat_nombres_carreras[,2]),][d,1])
        cont <- cont + 1
      }
      mat_nom_materias[d,3:(3+length(nom_materias)-1)] <- nom_materias
    }
  }#Fin for(d)
  #Se quita el cero del inicio
  ind_quitar <- ind_quitar[-1]
  
  mat_nombres_carreras <- mat_nombres_carreras[order(mat_nombres_carreras[,2]),]
  mat_nom_materias[223,3] <- "Sistemas Dinámicos Discretos II"
  # View(mat_nom_materias)
  # materias_extras <- setdiff(1:length(vec_nom_materias_total),mat_nombres_carreras[,2])
  # length(materias_extras)#104
  # View(vec_nom_materias_total[materias_extras]) 
  
  mat_nom_materias_total[mat_nombres_carreras[,2],1] <- mat_nombres_carreras[,1]
  mat_nom_materias_total[mat_nombres_carreras[,2],c(3:22)] <- mat_nom_materias[
    mat_nombres_carreras[,2],c(3:22)]
  
  nom_adm_act_riesgo <- mat_nom_materias[c(1,148,288),]
  nom_adm_act_riesgo <- nom_adm_act_riesgo[,c(1,3:20)]
  nom_adm_act_riesgo <- unique(nom_adm_act_riesgo[nom_adm_act_riesgo!=0])
  mat_nom_materias_total[1,1:(2+length(nom_adm_act_riesgo))] <- 
    c("Administración Actuarial del Riesgo",
      1,nom_adm_act_riesgo)
  mat_nom_materias_total[c(148,288),] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[2,5] <- 0
  
  nom_rec_bus_info_text <- mat_nom_materias[c(3,257),]
  nom_rec_bus_info_text <- nom_rec_bus_info_text[,c(1,3:20)]
  nom_rec_bus_info_text <- unique(nom_rec_bus_info_text[nom_rec_bus_info_text!=0])
  mat_nom_materias_total[3,1:(2+length(nom_rec_bus_info_text))] <- 
    c("Recuperación y Búsqueda de Información en Textos",3,nom_rec_bus_info_text)
  mat_nom_materias_total[c(257),] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[4,] <- mat_nom_materias[4,]
  mat_nom_materias_total[4,1] <- "Administración de Empresas de Software"
  mat_nom_materias_total[4,8] <- 0
  mat_nom_materias_total[c(258,278),] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[60,5] <- "Probabilidad y Estadística"
  mat_nom_materias_total[5,] <- mat_nom_materias_total[60,]#Dejamos el 5, se quita el 60
  mat_nom_materias_total[5,2] <- 5
  mat_nom_materias_total[60,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[6,4] <- mat_nom_materias[248,1]
  mat_nom_materias_total[248,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[7,4] <- 0
  mat_nom_materias_total[260,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[8:10,4] <- 0
  
  mat_nom_materias_total[11,4] <- "Electromagnetismo II/Electromagnetismo II/Electromagnetismo II"
  mat_nom_materias_total[215,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  nom_fun_esp <- mat_nom_materias[c(12,53,208,231,224),]
  nom_fun_esp <- nom_fun_esp[,c(1,3:20)]
  (nom_fun_esp <- unique(nom_fun_esp[nom_fun_esp!=0]))
  nom_fun_esp[9] <- 0
  mat_nom_materias_total[12,1:(2+length(nom_fun_esp))] <- 
    c("Funciones Especiales y Transformadas Integrales",12,nom_fun_esp)
  mat_nom_materias_total[c(53,208,231,224),] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[13,4:5] <- mat_nom_materias[119,3:4]
  mat_nom_materias_total[119,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[14,4] <- 0
  
  mat_nom_materias_total[15,4] <- mat_nom_materias[195,1]
  mat_nom_materias_total[195,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[16,4] <- "Mecánica Cuántica/Mecánica Cuántica/Mecánica Cuántica"
  mat_nom_materias_total[216,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[17:20,4] <- 0
  
  mat_nom_materias_total[21:23,5] <- 0
  
  mat_nom_materias_total[24,c(5,6,7)] <- 0
  
  mat_nom_materias_total[25,5] <- 0
  
  mat_nom_materias_total[26,c(5,6,7)] <- 0
  
  mat_nom_materias_total[27,c(5,6,7)] <- 0
  mat_nom_materias_total[310,c(4,5)] <- 0
  
  mat_nom_materias_total[28:32,5] <- 0
  
  mat_nom_materias_total[33,5:6] <- mat_nom_materias[147,3:4]
  mat_nom_materias_total[147,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[34,] <- mat_nom_materias[34,]
  mat_nom_materias_total[34,5] <- 0
  
  mat_nom_materias_total[35,5] <- 0
  
  mat_nom_materias_total[36,1] <- "Introducción a las Matemáticas Discretas"
  mat_nom_materias_total[36,7] <- 0
  mat_nom_materias_total[311,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[37,5] <- 0
  
  mat_nom_materias_total[38:40,5] <- 0
  
  mat_nom_materias_total[41,] <- mat_nom_materias[41,]
  
  mat_nom_materias_total[42,1] <- "Inferencia Estadística"
  mat_nom_materias_total[42,7] <- 0
  mat_nom_materias_total[300,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[43,6] <- 0
  
  mat_nom_materias_total[44,] <- mat_nom_materias[44,]
  mat_nom_materias_total[44,1] <- "Teoría de Redes"
  mat_nom_materias_total[44,6:7] <- c("Análisis de Redes/Análisis de Redes/Análisis de Redes",
                                      "/Análisis de Redes/Teoría de Redes/Teoría de Redes/Teoría de Redes/Teoría de Redes/Análisis de Redes/Teoría de Redes/Teoría de Redes/Teoría de Redes/Teoría de Redes/Teoría de Redes/Teoría de Redes")
  mat_nom_materias_total[152,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[45,5] <- 0
  
  mat_nom_materias_total[46,6] <- 0
  
  mat_nom_materias_total[47:49,5] <- 0
  
  mat_nom_materias_total[50,1] <- "Manejo de Datos"
  mat_nom_materias_total[50,7:11] <- c("Sistemas Manejadores de Bases de Datos",
                                       "Sistemas de Bases de Datos",
                                       "Grandes Bases de Datos",
                                       "Fundamentos de Bases de Datos",
                                       "Almacenes y Minería de Datos")
  mat_nom_materias_total[50,12:16] <- c("Manejo de Datos",
                                        "Sistemas Manejadores de Bases de Datos/Sistemas Manejadores de Bases de Datos/Sistemas Manejadores de Bases de Datos",
                                        "/Sistemas de Bases de Datos/Sistemas de Bases de Datos/Fundamentos de Bases de Datos",
                                        "/Grandes Bases de Datos/Almacenes y Minería de Datos",
                                        "Manejo de Datos/Manejo de Datos/Manejo de Datos")
  mat_nom_materias_total[50,17:18] <- c("Programación II/Programación II/Programación II",
                                        "Programación II")
  mat_nom_materias_total[c(330,106,123,169,241,269,301,51),] <- rep("X",
                                                                    dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[52,5] <- 0
  
  nom_numerico <- mat_nom_materias[c(54,161,321),]
  nom_numerico <- nom_numerico[,c(1,3:20)]
  (nom_numerico <- unique(nom_numerico[nom_numerico!=0]))
  nom_numerico[7] <- 0
  mat_nom_materias_total[54,1:(2+length(nom_numerico))] <- c("Análisis Numérico",54,nom_numerico)
  mat_nom_materias_total[c(161,321),] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[55,5] <- 0
  
  mat_nom_materias_total[56,5:6] <- c("Seminario de Filosofía de la Ciencia I",
                                      "Seminario de Filosofía de la Ciencia I/Seminario de Filosofía de la Ciencia I/Seminario de Filosofía de la Ciencia I")
  mat_nom_materias_total[56,7:8] <- mat_nom_materias[319,3:4]
  mat_nom_materias_total[c(118,319),] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[57:58,5] <- 0
  
  nom_est_II <- mat_nom_materias[c(59,284),]
  nom_est_II <- nom_est_II[,c(1,3:20)]
  nom_est_II <- unique(nom_est_II[nom_est_II!=0])
  mat_nom_materias_total[59,1:(2+length(nom_est_II))] <- 
    c("Modelos no Paramétricos y de Regresión",59,nom_est_II)
  mat_nom_materias_total[59,7:8] <- mat_nom_materias[113,3:4]
  mat_nom_materias_total[c(113,284),] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[61:66,5] <- 0
  
  mat_nom_materias_total[67,] <- mat_nom_materias_total[240,]
  mat_nom_materias_total[67,2] <- 67
  mat_nom_materias_total[240,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[68,] <- mat_nom_materias_total[220,]
  mat_nom_materias_total[68,2] <- 68
  mat_nom_materias_total[68,6] <- "Matemáticas Discretas/Matemáticas Discretas/Matemáticas Discretas"
  mat_nom_materias_total[220,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  nom_progra_I <- mat_nom_materias[c(69,287),]
  nom_progra_I <- nom_progra_I[,c(1,3:20)]
  nom_progra_I <- unique(nom_progra_I[nom_progra_I!=0])
  nom_progra_I <- nom_progra_I[-c(6,7)]
  mat_nom_materias_total[69,1:(7+length(nom_progra_I))] <- c("Programación",69,
                                                             nom_progra_I,0,0,0,0,0)
  mat_nom_materias_total[287,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[70,6] <- 0
  mat_nom_materias_total[159,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[71:73,5] <- 0
  
  mat_nom_materias_total[73,5] <- mat_nom_materias[207,1]
  mat_nom_materias_total[73,6:7] <- mat_nom_materias[209,3:4]
  mat_nom_materias_total[c(207,209),] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[74,5:6] <- 0
  
  mat_nom_materias_total[75:77,5] <- 0
  
  nom_MASD <- mat_nom_materias[c(78,79,98,127,297),]
  nom_MASD <- nom_MASD[,c(1,3:20)]
  nom_MASD <- unique(nom_MASD[nom_MASD!=0])
  mat_nom_materias_total[78,1:(2+length(nom_MASD))] <- 
    c("Matemáticas Actuariales para Seguro de Daños, Fianzas y Reaseguro",78,nom_MASD)
  mat_nom_materias_total[c(79,98,127,297),] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[80,6] <- 0
  mat_nom_materias_total[143,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[81,5] <- 0
  
  nom_finanzas_I <- mat_nom_materias[c(82,298),]
  nom_finanzas_I <- nom_finanzas_I[,c(1,3:20)]
  (nom_finanzas_I <- unique(nom_finanzas_I[nom_finanzas_I!=0]))
  mat_nom_materias_total[82,1:(2+length(nom_finanzas_I))] <- 
    c("Métodos Cuantitativos en Finanzas",82,nom_finanzas_I)
  mat_nom_materias_total[298,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[83,4] <- "Matemáticas Financieras"
  mat_nom_materias_total[83,5:7] <- 0
  
  mat_nom_materias_total[84,5] <- 0
  
  nom_sem_app_act <- mat_nom_materias[c(85,142,303,323),]
  nom_sem_app_act <- nom_sem_app_act[,c(1,3:20)]
  (nom_sem_app_act <- unique(nom_sem_app_act[nom_sem_app_act!=0]))
  nom_sem_app_act <- nom_sem_app_act[-(9:15)]
  mat_nom_materias_total[85,1:(2+length(nom_sem_app_act))] <- 
    c("Seminario de Aplicaciones Actuariales",85,nom_sem_app_act)
  mat_nom_materias_total[85,11] <- mat_nom_materias[211,1]
  mat_nom_materias_total[85,12:13] <- mat_nom_materias[313,3:4]
  mat_nom_materias_total[c(142,303,323,333,211,313),] <- rep("X",dim(mat_nom_materias_total)[2])
  
  nom_finanzas_II <- mat_nom_materias[c(86,128,306),]
  nom_finanzas_II <- nom_finanzas_II[,c(1,3:20)]
  (nom_finanzas_II <- unique(nom_finanzas_II[nom_finanzas_II!=0]))
  nom_finanzas_II <- nom_finanzas_II[-(7:8)]
  mat_nom_materias_total[86,1:(2+length(nom_finanzas_II))] <- 
    c("Mercados Financieros y Valuación de Instrumentos",86,nom_finanzas_II)
  mat_nom_materias_total[c(128,306),] <- rep("X",dim(mat_nom_materias_total)[2])
  
  nom_mex_cont <- mat_nom_materias[c(87,176,275),]
  nom_mex_cont <- nom_mex_cont[,c(1,3:20)]
  (nom_mex_cont <- unique(nom_mex_cont[nom_mex_cont!=0]))
  mat_nom_materias_total[87,1:(2+length(nom_mex_cont))] <- 
    c("Análisis del México Contemporáneo",87,nom_mex_cont)
  mat_nom_materias_total[c(176,275),] <- rep("X",dim(mat_nom_materias_total)[2])
  
  nom_economia <- mat_nom_materias[c(88,304),]
  nom_economia <- nom_economia[,c(1,3:20)]
  (nom_economia <- unique(nom_economia[nom_economia!=0]))
  nom_economia[6:8] <- c("Formación Científica II",0,0)
  mat_nom_materias_total[88,1:(2+length(nom_economia))] <- c("Economía",88,nom_economia)
  mat_nom_materias_total[304,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  nom_derivados <- mat_nom_materias[c(91,184,326),]
  nom_derivados <- nom_derivados[,c(1,3:20)]
  (nom_derivados <- unique(nom_derivados[nom_derivados!=0]))
  mat_nom_materias_total[91,1:(2+length(nom_derivados))] <- 
    c("Productos Financieros Derivados",91,nom_derivados)
  mat_nom_materias_total[c(184,326),] <- rep("X",dim(mat_nom_materias_total)[2])
  
  nom_economia_II <- mat_nom_materias[c(93,307),]
  nom_economia_II <- nom_economia_II[,c(1,3:20)]
  (nom_economia_II <- unique(nom_economia_II[nom_economia_II!=0]))
  mat_nom_materias_total[93,1:(2+length(nom_economia_II))] <- 
    c("Temas Selectos de Economía",93,nom_economia_II)
  mat_nom_materias_total[93,7:8] <- mat_nom_materias[233,3:4]
  mat_nom_materias_total[c(307,233),] <- rep("X",dim(mat_nom_materias_total)[2])
  
  nom_demo <- mat_nom_materias[c(94,97,289,308),]
  nom_demo <- nom_demo[,c(1,3:20)]
  (nom_demo <- unique(nom_demo[nom_demo!=0]))
  mat_nom_materias_total[94,1:(2+length(nom_demo))] <- c("Demografía",94,nom_demo)
  mat_nom_materias_total[c(97,289,308),] <- rep("X",dim(mat_nom_materias_total)[2])
  
  nom_introCdC <- mat_nom_materias[c(95,101,222,234),]
  nom_introCdC <- nom_introCdC[,c(1,3:20)]
  (nom_introCdC <- unique(nom_introCdC[nom_introCdC!=0]))
  nom_introCdC[7:11] <- 0
  mat_nom_materias_total[95,1:(2+length(nom_introCdC))] <- 
    c("Introducción a Ciencias de la Computación",95,nom_introCdC)
  mat_nom_materias_total[95,9:10] <- mat_nom_materias[268,c(1,3)]
  mat_nom_materias_total[c(101,222,234,268),] <- rep("X",dim(mat_nom_materias_total)[2])
  
  nom_arq_compu <- mat_nom_materias[c(102,245),]
  nom_arq_compu <- nom_arq_compu[,c(1,3:20)]
  (nom_arq_compu <- unique(nom_arq_compu[nom_arq_compu!=0]))
  mat_nom_materias_total[102,1:(2+length(nom_arq_compu))] <- 
    c("Organización y Arquitectura de Computadoras",102,nom_arq_compu)
  mat_nom_materias_total[245,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  nom_an_algoritmos <- mat_nom_materias[c(103,205,243),]
  nom_an_algoritmos <- nom_an_algoritmos[,c(1,3:20)]
  (nom_an_algoritmos <- unique(nom_an_algoritmos[nom_an_algoritmos!=0]))
  mat_nom_materias_total[103,1:(2+length(nom_an_algoritmos))] <- 
    c("Análisis de Algoritmos",103,nom_an_algoritmos)
  mat_nom_materias_total[c(205,243),] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[104,6:7] <- mat_nom_materias[214,3:4]
  mat_nom_materias_total[c(214,242,247),] <- rep("X",dim(mat_nom_materias_total)[2])
  
  nom_sem_CdC <- mat_nom_materias[c(105,254,263,133,162,190,259,191,217,228,252),]
  nom_sem_CdC <- nom_sem_CdC[,c(1,3:20)]
  (nom_sem_CdC <- unique(nom_sem_CdC[nom_sem_CdC!=0]))
  (nom_sem_CdC <- nom_sem_CdC[-c(12:13,15:16,21:22,24:35)])
  mat_nom_materias_total[105,1:(2+length(nom_sem_CdC))] <- 
    c("Seminario de Ciencias de la Computación",105,nom_sem_CdC)
  mat_nom_materias_total[105,20:22] <- mat_nom_materias[164,3:5]
  mat_nom_materias_total[105,14] <- "Métodos Formales"
  mat_nom_materias_total[105,15] <- mat_nom_materias[255,1]
  mat_nom_materias_total[105,16] <- mat_nom_materias[274,1]
  mat_nom_materias_total[c(254,263,133,162,190,259,191,217,228,
                           252,164,282,291,255,274),] <- rep("X",dim(mat_nom_materias_total)[2])
  
  nom_mod_prog <- mat_nom_materias[c(107,246,168,196,277,296),]
  nom_mod_prog <- nom_mod_prog[,c(1,3:20)]
  (nom_mod_prog <- unique(nom_mod_prog[nom_mod_prog!=0]))
  nom_mod_prog[10:11] <- 0
  mat_nom_materias_total[107,1:(2+length(nom_mod_prog))] <- c("Modelado y Programación",
                                                              107,nom_mod_prog)
  mat_nom_materias_total[c(246,168,196,277,296),] <- rep("X",dim(mat_nom_materias_total)[2])
  
  nom_logica_comp <- mat_nom_materias[c(108,166,244,251),]
  nom_logica_comp <- nom_logica_comp[,c(1,3:20)]
  (nom_logica_comp <- unique(nom_logica_comp[nom_logica_comp!=0]))
  mat_nom_materias_total[108,1:(2+length(nom_logica_comp))] <- c("Lógica Computacional",
                                                                 108,nom_logica_comp)
  mat_nom_materias_total[c(166,244,251),] <- rep("X",dim(mat_nom_materias_total)[2])
  
  nom_interf_usuario <- mat_nom_materias[c(130,167,272),]
  nom_interf_usuario <- nom_interf_usuario[,c(1,3:20)]
  (nom_interf_usuario <- unique(nom_interf_usuario[nom_interf_usuario!=0]))
  mat_nom_materias_total[130,1:(2+length(nom_interf_usuario))] <-
    c("Diseño de Interfaces de Usuario",130,nom_interf_usuario)
  mat_nom_materias_total[c(167,272),] <- rep("X",dim(mat_nom_materias_total)[2])
  
  nom_sem_int_art <- mat_nom_materias[c(132,163,264,267,279,281),]
  nom_sem_int_art <- nom_sem_int_art[,c(1,3:20)]
  (nom_sem_int_art <- unique(nom_sem_int_art[nom_sem_int_art!=0]))
  nom_sem_int_art[10:11] <- c("Sistemas Expertos",0)
  mat_nom_materias_total[132,] <- c("Reconocimiento de Patrones y Aprendizaje Automatizado",
                                    132,nom_sem_int_art,rep(0,9))
  mat_nom_materias_total[132,13] <- mat_nom_materias[292,1]
  mat_nom_materias_total[c(163,264,267,279,281,198,292),] <- rep("X",
                                                                 dim(mat_nom_materias_total)[2])
  
  nom_sem_filos_mate <- mat_nom_materias[c(135,138,155,146),]
  nom_sem_filos_mate <- nom_sem_filos_mate[,c(1,3:20)]
  (nom_sem_filos_mate <- unique(nom_sem_filos_mate[nom_sem_filos_mate!=0]))
  mat_nom_materias_total[135,1:(2+length(nom_sem_filos_mate))] <-
    c("Seminario Filosofía de las Matemáticas",135,nom_sem_filos_mate)
  mat_nom_materias_total[c(138,155,146),] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[137,] <- mat_nom_materias[137,]
  
  mat_nom_materias_total[139,1] <- "Modelos de Supervivencia y de Series de Tiempo"
  mat_nom_materias_total[285,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[144,5] <- mat_nom_materias[273,1]
  mat_nom_materias_total[273,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[150,5] <- 0
  
  mat_nom_materias_total[151,4] <- "Análisis Matemático IV/Análisis Matemático IV/Análisis Matemático IV/Análisis Matemático IV/Análisis Matemático IV"
  
  nom_sem_IdO <- mat_nom_materias[c(160,305),]
  nom_sem_IdO <- nom_sem_IdO[,c(1,3:20)]
  (nom_sem_IdO <- unique(nom_sem_IdO[nom_sem_IdO!=0]))
  nom_sem_IdO[6:9] <- 0
  mat_nom_materias_total[160,1:(2+length(nom_sem_IdO))] <-
    c("Temas Selectos de Investigación de Operaciones",160,nom_sem_IdO)
  mat_nom_materias_total[305,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  nom_ing_software_II <- mat_nom_materias[c(165,192,265,283,294),]
  nom_ing_software_II <- nom_ing_software_II[,c(1,3:20)]
  (nom_ing_software_II <- unique(nom_ing_software_II[nom_ing_software_II!=0]))
  nom_ing_software_II <- nom_ing_software_II[-c(6,10:14)]
  mat_nom_materias_total[165,1:(2+length(nom_ing_software_II))] <-
    c("Ingeniería de Software II",165,nom_ing_software_II)
  mat_nom_materias_total[c(192,265,283,294),] <- rep("X",dim(mat_nom_materias_total)[2])
  
  nom_sem_estadistica <- mat_nom_materias[c(177,324),]
  nom_sem_estadistica <- nom_sem_estadistica[,c(1,3:20)]
  (nom_sem_estadistica <- unique(nom_sem_estadistica[nom_sem_estadistica!=0]))
  nom_sem_estadistica <- nom_sem_estadistica[-c(4:6)]
  mat_nom_materias_total[177,1:(2+length(nom_sem_estadistica))] <-
    c("Seminario de Estadística I",177,nom_sem_estadistica)
  mat_nom_materias_total[324,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[179,5:6] <- mat_nom_materias[232,3:4]
  mat_nom_materias_total[232,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[183,5:6] <- mat_nom_materias[290,3:4]
  mat_nom_materias_total[290,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[186,5:6] <- mat_nom_materias[188,3:4]
  mat_nom_materias_total[186,7:8] <- mat_nom_materias[213,3:4]
  mat_nom_materias_total[186,9] <- mat_nom_materias[293,1]
  mat_nom_materias_total[c(188,213,293),] <- rep("X",dim(mat_nom_materias_total)[2])
  
  nom_taller_h_comp <- mat_nom_materias[c(189,312,276,227),]
  nom_taller_h_comp <- nom_taller_h_comp[,c(1,3:20)]
  (nom_taller_h_comp <- unique(nom_taller_h_comp[nom_taller_h_comp!=0]))
  nom_taller_h_comp[6:7] <- 0
  mat_nom_materias_total[189,1:(2+length(nom_taller_h_comp))] <-
    c("Taller de Herramientas Computacionales",189,nom_taller_h_comp)
  mat_nom_materias_total[189,8:10] <- mat_nom_materias[229,3:5]
  mat_nom_materias_total[c(312,276,227,229,286),] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[193,] <- mat_nom_materias[193,]
  mat_nom_materias_total[193,1] <- "Redes Neuronales"
  mat_nom_materias_total[302,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[194,] <- mat_nom_materias[194,]
  mat_nom_materias_total[194,1] <- "Algoritmos Paralelos"
  mat_nom_materias_total[270,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[197,] <- mat_nom_materias[197,]
  mat_nom_materias_total[197,1] <- "Cómputo Evolutivo"
  mat_nom_materias_total[280,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  nom_sim_control <- mat_nom_materias[c(201,210),]
  nom_sim_control <- nom_sim_control[,c(1,3:20)]
  (nom_sim_control <- unique(nom_sim_control[nom_sim_control!=0]))
  mat_nom_materias_total[201,1:(2+length(nom_sim_control))] <- 
    c("Control Estadístico de la Calidad",201,nom_sim_control)
  mat_nom_materias_total[210,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[256,3:5] <- c("Diseño y Programación de Videojuegos",0,0)
  
  mat_nom_materias_total[262,1:4] <- c("Criptografía y Seguridad",262,
                                       "Introducción a la Criptografía",
                                       "Criptografía y Seguridad")
  mat_nom_materias_total[271,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[266,3:5] <- c("Métricas de Software",0,0)
  
  mat_nom_materias_total[295,3:5] <- c("Genómica Computacional",0,0)
  
  nom_sem_tit_mate <- mat_nom_materias[c(310,317),]
  nom_sem_tit_mate <- nom_sem_tit_mate[,c(1,3:20)]
  (nom_sem_tit_mate <- unique(nom_sem_tit_mate[nom_sem_tit_mate!=0]))
  nom_sem_tit_mate[3:6] <- 0
  mat_nom_materias_total[310,1:(2+length(nom_sem_tit_mate))] <- 
    c("Seminario de Apoyo a la Titulación en Matemáticas",310,nom_sem_tit_mate)
  mat_nom_materias_total[317,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  nom_sem_tit_CdC <- mat_nom_materias[c(315,316),]
  nom_sem_tit_CdC <- nom_sem_tit_CdC[,c(1,3:20)]
  (nom_sem_tit_CdC <- unique(nom_sem_tit_CdC[nom_sem_tit_CdC!=0]))
  mat_nom_materias_total[315,1:(2+length(nom_sem_tit_CdC))] <- 
    c("Seminario de Apoyo a la Titulación en Ciencias de la Computación",315,nom_sem_tit_CdC)
  mat_nom_materias_total[316,] <- rep("X",dim(mat_nom_materias_total)[2])
  
  mat_nom_materias_total[329,3:5] <- c("Sistemas Dinámicos no Lineales",0,0)
  
  # length(which(mat_nom_materias_total[,1]!="X")) ##201
  # length(which(mat_nom_materias_total[,1]=="X")) ##134
  
  ### LIMPIEZA DE "mat_nom_materias_total" ###
  mat_nom_materias_CON_X <- mat_nom_materias_total
  mat_nom_materias_SIN_X <- mat_nom_materias_total[mat_nom_materias_total[,1]!="X",]
  
  mat_nom_materias_total <- mat_nom_materias_SIN_X
  mat_nom_materias_total[,2] <- 1:dim(mat_nom_materias_SIN_X)[1]
  
  
  ##Cambios extra
  mat_nom_materias_total[43,6] <- "Investigación de Operaciones/Investigación de Operaciones/Investigación de Operaciones/Investigación de Operaciones/Investigación de Operaciones/Investigación de Operaciones/Investigación de Operaciones"
  mat_nom_materias_total[54,9] <- "Análisis Numérico/Análisis Numérico/Análisis Numérico/Análisis Numérico/Análisis Numérico/Análisis Numérico/Análisis Numérico"
  mat_nom_materias_total[144,6] <- "/Seminario de Aplicaciones Actuariales/Seminario de Aplicaciones Actuariales/Seminario de Estadística I"
  
  num_materia_202 <- dim(mat_nom_materias_total)[1]+1
  nom_comp_distrib <- c("Computación Distribuida",num_materia_202,
                        "Computación Distribuida",
                        "Principios de Computación Distribuida",
                        "/Principios de Computación Distribuida/Computación Concurrente",
                        "Computación Concurrente",
                        rep(0,16))
  mat_nom_materias_total <- rbind(mat_nom_materias_total,
                                  nom_comp_distrib)
  mat_nom_materias_total[98,c(8,9,13)] <- c("/Seminario de Computación Teórica II/Seminario de Ciencias de la Computación A",
                                            "/Seminario de Aplicaciones de Cómputo II/Seminario de Ciencias de la Computación A/Seminario de Ciencias de la Computación B",
                                            "/Seminario de Aplicaciones de Cómputo/Sistemas de Información Geográfica")
  mat_nom_materias_total[98,15] <- mat_nom_materias_total[98,22]
  mat_nom_materias_total[98,22] <- 0
  
  num_materia_203 <- dim(mat_nom_materias_total)[1]+1
  nom_anim_x_comp <- c("Animación por Computadora",num_materia_203,
                       "Animación por Computadora",
                       "/Seminario de Aplicaciones de Cómputo/Animación por Computadora",
                       rep(0,18))
  mat_nom_materias_total <- rbind(mat_nom_materias_total,
                                  nom_anim_x_comp)
  
  
  ##41 "Series de Tiempo"
  ##123 "Estadística III"
  # vec_series <- mat_nom_materias_total[41,]
  # mat_nom_materias_total[41,] <- mat_nom_materias_total[123,]
  # mat_nom_materias_total[41,7:8] <- vec_series[3:4]
  # mat_nom_materias_total <- mat_nom_materias_total[-123,]
  # mat_nom_materias_total[,2] <- 1:202
  
  save(mat_nom_materias_total, file = "mat_nom_materias_total.RData")
  vec_nom_materias_total <- mat_nom_materias_total[,1]
  save(vec_nom_materias_total,file = "vec_nom_materias_total.RData")
  
  return(mat_nom_materias_total)
}


# arroja_nom_correcto -----------------------------------------------------
#' Title arroja_nom_correcto: Función que arroja un vector con el nombre
#' correcto de la materia ingresada y su número de materia.
#'
#' @param materia: Nombre de alguna materia impartida en la FC.
#'
#' @return vec_info_nombre: Vector con el nombre correcto de la materia
#' ingresada y su número de materia.
#'
#' @examples
#' vec_info_nombre <- arroja_nom_correcto("Estadística I")
#' vec_info_nombre <- arroja_nom_correcto(materia)
#' 
arroja_nom_correcto <- function(materia){
  #Se carga la matriz con los nombres de las materias
  load("mat_nom_materias_total.RData")
  
  #Se definen las variables que se van a utilizar
  vec_info_nombre <- c(0,0)
  
  for(d in 1:dim(mat_nom_materias_total)[1]){
    ind <- which(materia == mat_nom_materias_total[d,c(1,3:dim(mat_nom_materias_total)[2])])
    if(length(ind) > 0){
      vec_info_nombre <- c(mat_nom_materias_total[d,1],d)
    }
  }#Fin for(d)
  
  if(vec_info_nombre[1] == 0){
    cat("\n La materia ",materia," no se encontró.")
  }else{
    cat("\n La materia ",materia," se llama: ",vec_info_nombre[1],
        ". Su número de materia es: ",vec_info_nombre[2])
  }
  
  return(vec_info_nombre)
}


# actualiza_col_num_materia -----------------------------------------------
#' Title actualiza_col_num_materia: Función que actualiza las matrices
#' "m_grande" con el nombre correcto para las materias y también actualiza
#' el número de materia en caso de ser necesario.
#'
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @examples
#' actualiza_col_num_materia(param)
#' 
actualiza_col_num_materia <- function(param){
  #Se definen las variables que se van a utilizar
  semestres <- param$sem_totales
  # semestres <- param$sem_totales[20:length(param$sem_totales)]
  num_col_Materia <- arroja_ind_col_MG("Materia")##1
  num_col_Profesor <- arroja_ind_col_MG("Profesor")##2
  num_col_Cambios <- arroja_ind_col_MG("Cambios")##12
  num_col_NumMateria <- arroja_ind_col_MG("Num_materia")##37
  
  for(s in 1:(length(semestres)-1)){
    sem_info <- semestres[s]
    nom_m_grande <- paste0("m_grande por semestre/m_grande_",sem_info,".RData")
    load(nom_m_grande)
    m_grande <- m_grande[m_grande[,num_col_Profesor]!=0,]
    m_grande <- m_grande[!is.na(m_grande[,num_col_Materia]),]
    
    for(r in 1:dim(m_grande)[1]){#Recorre renglones
      nom_materia <- m_grande[r,num_col_Materia]
      vec_info_nombre <- arroja_nom_correcto(nom_materia)
      if(vec_info_nombre[1] != 0){
        #Cuando si se encuentra el nombre de la materia
        m_grande[r,num_col_Materia] <- vec_info_nombre[1]
        
        if(m_grande[r,num_col_NumMateria] != vec_info_nombre[2]){
          #' En caso de que se haya cambiado el número de materia se registra
          #' en la columna de cambios.
          m_grande[r,num_col_Cambios] <- paste0(m_grande[r,num_col_Cambios],"/5")
        }
      }
      m_grande[r,num_col_NumMateria] <- vec_info_nombre[2]
    }#Fin for(r)
    save(m_grande,file = nom_m_grande)
  }#Fin for(s)
}


# corrige_sem_CdC --------------------------------------------------------
#' Title corrige_sem_CdC: Función que corrige los renglones de cada 
#' "m_grande" que tienen las materias: "Animación por Computadora" y
#' "Computación Distribuida".
#'
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @examples
#' corrige_sem_CdC(param)
#' 
corrige_sem_CdC <- function(param){
  #Se definen las variables que se van a utilizar
  semestres <- param$sem_totales
  num_col_Materia <- arroja_ind_col_MG("Materia")##1
  num_col_Cambios <- arroja_ind_col_MG("Cambios")##12
  num_col_NomMat_Act2000 <- arroja_ind_col_MG("NomMat_Act2000")##23
  num_col_NomMat_MAp2017 <- arroja_ind_col_MG("NomMat_MAp2017")##29
  num_col_NumMateria <- arroja_ind_col_MG("Num_materia")##37
  materia <- "Seminario de Ciencias de la Computación"
  vec_info_anim_comp <- arroja_nom_correcto("Animación por Computadora")
  vec_info_comp_distrib <- arroja_nom_correcto("Computación Distribuida")
  vec_comp_distrib <- c("Principios de Computación Distribuida",
                        "Computación Concurrente",
                        vec_info_comp_distrib[1])
  
  for(s in 1:(length(semestres)-1)){
    sem_info <- semestres[s]
    nom_m_grande <- paste0("m_grande por semestre/m_grande_",sem_info,".RData")
    load(nom_m_grande)
    
    indices <- checa_ind_materia(materia,m_grande)
    
    for(r in indices){#Recorre sólo los índices de los renglones que queremos cambiar
      vec_m_grande <- m_grande[r,num_col_NomMat_Act2000:num_col_NomMat_MAp2017]
      
      #Verificamos para "Animación por Computadora"
      if(any(vec_info_anim_comp[1] == vec_m_grande)){
        m_grande[r,c(num_col_Materia,num_col_Cambios,
                     num_col_NumMateria)] <- c(vec_info_anim_comp[1],
                                               paste0(m_grande[r,num_col_Cambios],"/1"),
                                               vec_info_anim_comp[2])
      }
      #Verificamos para "Computación Distribuida"
      if(any(vec_comp_distrib == vec_m_grande)){
        m_grande[r,c(num_col_Materia,num_col_Cambios,
                     num_col_NumMateria)] <- c(vec_info_comp_distrib[1],
                                               paste0(m_grande[r,num_col_Cambios],"/1"),
                                               vec_info_comp_distrib[2])
      }
    }#Fin for(r)
    save(m_grande,file = nom_m_grande)
  }#Fin for(s)
}



##### M_GRANDE_TOTAL / M_FILTRADA #####
#' Funciones que generan la matriz "m_grande_total" con la información de
#' las páginas de la facultad. La matriz "m_filtrada" es una submatriz
#' de "m_grande_total".


# gen_m_grande_total ------------------------------------------------------
#' Title gen_m_grande_total: Función que genera la matriz "m_grande_total"
#' para un intervalo semestres.
#'
#' @param vec_excepciones: Vector que contiene las posibles excepciones que
#' se deben de tomar en cuenta al crear "m_grande".
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
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
#' @examples
#' m_grande_total <- gen_m_grande_total(c("Inglés"),param)
#' 
gen_m_grande_total <- function(vec_excepciones,param){
  ##Se definen las variables que se van a utilizar:
  semestres <- param$Semestres
  m_grande_total <- matrix(0,ncol = length(param$nom_cols_MG))
  colnames(m_grande_total) <- param$nom_cols_MG
  m_grande_total <- data.frame(m_grande_total)
  
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
  m_grande_total <- m_grande_total %>% filter(m_grande_total$Materia != 0)
  save(m_grande_total, file = paste0("Matrices m_grande_total/m_grande_total_",
                                     param$sem_ini,"_",param$sem_fin,
                                     ".RData"))
  return(m_grande_total)
}


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
#' m_filtrada <- gen_mat_m_filtrada(param,param_sim)
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
  param_sim$m_filtrada <- m_filtrada
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



##### INFORMACIÓN EXTRA #####
#'Funciones encargadas de extraer, información adicional de las páginas
#'de la Facultad de Ciencias. Nombres de materias por carrera, nombres
#'de profesores de tiempo completo y de asignatura.

# agrega_nom_1_materia_en_vec ---------------------------------------------
#' Title agrega_nom_1_materia_en_vec: Función que recibe como parámetros el
#' nombre de una materia y el vector con los nombres de las materias y
#' guarda la materia en caso de dar la opción de "SI". También imprime una
#' lista con los diferentes nombres que pudiera tener "materia" en
#' "vec_nom_materias_total"
#' Por ejemplo "Estadística III" y "Modelos de Supervivencia y de Series
#' de Tiempo".
#'
#' @param materia: Nombre de algún curso impartido en la FC.
#' @param vec_nom_materias_total: Vector que contiene el nombre de las
#' materias sin repetición, conservando los nombres más recientes. 
#'
#' @examples
#' agrega_nom_1_materia_en_vec(materia,vec_nom_materias_total)
#'
agrega_nom_1_materia_en_vec <- function(materia,vec_nom_materias_total){
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


# carga_info_prof_tiempo_completo -----------------------------------------
#' Title carga_info_prof_tiempo_completo: Función que extrae los nombres de
#' los profesores de tiempo completo del Departamento de Matemáticas.
#'
#' @return vec_prof_TC: Vector con el nombre de los profesores de tiempo
#' completo del Departamento de Matemáticas.
#'
#' @examples
#' vec_prof_TC <- carga_info_prof_tiempo_completo()
#' 
carga_info_prof_tiempo_completo <- function(){
  #Página del Departamento de Matemáticas
  url <- "http://www.matematicas.unam.mx/index.php/nosotros/profesores-de-tiempo-completo"
  
  # Sacamos la información de la página
  webpage <- read_html(url)
  profesor_data_html <- html_nodes(webpage,'td a')#Selecciona 94 profesores
  prof_TC <- html_text(profesor_data_html)  
  prof_TC
  
  #Se define el vector en el que vamos a guardar los nombres
  vec_aux_prof_TC <- rep(0,length(prof_TC))
  vec_prof_TC <- 0
  
  ### Quitamos "\n\t\t\t\t\t\t\t"
  for(d in 1:length(vec_aux_prof_TC)){
    vec_aux_prof_TC[d] <- substr(prof_TC[d],9,nchar(prof_TC[d]))
  }
  
  #' Quitamos los títulos de los profesores: Dr., Dra., M., Act., ...
  #' Hay varios casos por lo que vamos a dividir los datos
  #' en grupos para poder quitar todos los títulos.
  #' Para ver de una mejor manera los casos de los títulos
  #' ordenamos los nombres:
  # sort(vec_aux_prof_TC)[1:10]
  # View(sort(vec_aux_prof_TC))
  ind_prof <- 0
  
  ### " Dra. " Un espacio al inicio, un espacio antes del nombre ###
  ### "	Dr.  " Un espacio al inicio, 2 espacios antes del nombre ###
  for(d in 1:length(vec_aux_prof_TC)){
    texto <- substr(vec_aux_prof_TC[d],1,6)
    if(texto==" Dr.  " || texto==" Dra. " || texto=="Act.  "){
      num_char <- nchar(vec_aux_prof_TC[d])
      nom_prof <- substr(vec_aux_prof_TC[d],7,num_char)
      vec_prof_TC <- c(vec_prof_TC,nom_prof)
      ind_prof <- c(ind_prof,d)
    }
  }#Fin for(d)
  #Se quita el cero del inicio
  ind_prof <- ind_prof[-1]
  #' Se actualiza el vector auxiliar para que la búsqueda sea cada vez
  #' de menos elementos
  vec_aux_prof_TC <- vec_aux_prof_TC[-ind_prof]
  # sort(vec_aux_prof_TC)[1:10]
  #Se reinicializa el vector de índices
  ind_prof <- 0
  
  
  ### " Dr. " Un espacio al inicio, un espacio antes del nombre ###
  ### "Act. " Un espacio antes del nombre ###
  ### "Dra. " Un espacio antes del nombre ###
  ### "Mat. " Un espacio antes del nombre ###
  for(d in 1:length(vec_aux_prof_TC)){
    texto <- substr(vec_aux_prof_TC[d],1,5)
    if(texto==" Dr. " || texto=="Act. " || texto=="Dra. " || texto=="Mat. "){
      num_char <- nchar(vec_aux_prof_TC[d])
      nom_prof <- substr(vec_aux_prof_TC[d],6,num_char)
      vec_prof_TC <- c(vec_prof_TC,nom_prof)
      ind_prof <- c(ind_prof,d)
    }
  }#Fin for(d)
  #Se quita el cero del inicio
  ind_prof <- ind_prof[-1]
  #' Se actualiza el vector auxiliar para que la búsqueda sea cada vez
  #' de menos elementos
  vec_aux_prof_TC <- vec_aux_prof_TC[-ind_prof]
  # sort(vec_aux_prof_TC)[1:10]
  #Se reinicializa el vector de índices
  ind_prof <- 0
  
  
  ### " M. en C. " Un espacio al inicio, un espacio antes del nombre ###
  for(d in 1:length(vec_aux_prof_TC)){
    texto <- substr(vec_aux_prof_TC[d],1,10)
    if(texto == " M. en C. "){
      num_char <- nchar(vec_aux_prof_TC[d])
      nom_prof <- substr(vec_aux_prof_TC[d],11,num_char)
      vec_prof_TC <- c(vec_prof_TC,nom_prof)
      ind_prof <- c(ind_prof,d)
    }
  }#Fin for(d)
  #Se quita el cero del inicio
  ind_prof <- ind_prof[-1]
  #' Se actualiza el vector auxiliar para que la búsqueda sea cada vez
  #' de menos elementos
  vec_aux_prof_TC <- vec_aux_prof_TC[-ind_prof]
  # sort(vec_aux_prof_TC)[1:10]
  #Se reinicializa el vector de índices
  ind_prof <- 0
  
  
  ### "Dr. " Un espacio antes del nombre ###
  for(d in 1:length(vec_aux_prof_TC)){
    texto <- substr(vec_aux_prof_TC[d],1,4)
    if(texto == "Dr. "){
      num_char <- nchar(vec_aux_prof_TC[d])
      nom_prof <- substr(vec_aux_prof_TC[d],5,num_char)
      vec_prof_TC <- c(vec_prof_TC,nom_prof)
      ind_prof <- c(ind_prof,d)
    }
  }#Fin for(d)
  #Se quita el cero del inicio
  ind_prof <- ind_prof[-1]
  #' Se actualiza el vector auxiliar para que la búsqueda sea cada vez
  #' de menos elementos
  vec_aux_prof_TC <- vec_aux_prof_TC[-ind_prof]
  # sort(vec_aux_prof_TC)[1:10]
  #Se reinicializa el vector de índices
  ind_prof <- 0
  
  
  ### "M. en C. " Un espacio antes del nombre ###
  for(d in 1:length(vec_aux_prof_TC)){
    texto <- substr(vec_aux_prof_TC[d],1,9)
    if(texto == "M. en C. "){
      num_char <- nchar(vec_aux_prof_TC[d])
      nom_prof <- substr(vec_aux_prof_TC[d],10,num_char)
      vec_prof_TC <- c(vec_prof_TC,nom_prof)
      ind_prof <- c(ind_prof,d)
    }
  }#Fin for(d)
  #Se quita el cero del inicio
  ind_prof <- ind_prof[-1]
  #' Se actualiza el vector auxiliar para que la búsqueda sea cada vez
  #' de menos elementos
  vec_aux_prof_TC <- vec_aux_prof_TC[-ind_prof]
  # sort(vec_aux_prof_TC)[1:10]
  #Se reinicializa el vector de índices
  ind_prof <- 0
  
  
  ### ""M. en I. de O. " Un espacio antes del nombre ###
  ### "M. en E. I. O. " Un espacio antes del nombre ###
  for(d in 1:length(vec_aux_prof_TC)){
    texto <- substr(vec_aux_prof_TC[d],1,15)
    if(texto=="M. en I. de O. " || texto=="M. en E. I. O. "){
      num_char <- nchar(vec_aux_prof_TC[d])
      nom_prof <- substr(vec_aux_prof_TC[d],16,num_char)
      vec_prof_TC <- c(vec_prof_TC,nom_prof)
      ind_prof <- c(ind_prof,d)
    }
  }#Fin for(d)
  #Se quita el cero del inicio
  vec_prof_TC <- vec_prof_TC[-1]
  
  
  #' Quitamos los espacios del final
  for(d in 1:length(vec_prof_TC)){
    num_char <- nchar(vec_prof_TC[d])
    texto <- substr(vec_prof_TC[d],num_char,num_char)
    if(texto==" "){
      nom_prof <- substr(vec_prof_TC[d],1,(num_char-1))
      vec_prof_TC[d] <- nom_prof
    }
  }#Fin for(d)
  
  #' Quitamos los espacios del final (en caso de tener 2)
  for(d in 1:length(vec_prof_TC)){
    num_char <- nchar(vec_prof_TC[d])
    texto <- substr(vec_prof_TC[d],num_char,num_char)
    if(texto==" "){
      nom_prof <- substr(vec_prof_TC[d],1,(num_char-1))
      vec_prof_TC[d] <- nom_prof
    }
  }#Fin for(d)
  
  return(vec_prof_TC)
}


# gen_mat_nom_prof_total --------------------------------------------------
#' Title gen_mat_nom_prof_total: Función que carga la matriz
#' "m_grande_total" de los semestres 2015-1 a 2020-1 de la cual se va
#' obtiene la lista de nombres de los profesores sin repetición. La matriz
#' "mat_nom_prof_total" tiene 2 columnas, en la primera se tiene el nombre
#' de los profesores y en la segunda se tiene un 1 si el profesor es de
#' tiempo completo y 0 si no.
#'
#' @return mat_nom_prof_total: Matriz de 2 columnas, en la primera se
#' tiene el nombre de los profesores y en la segunda se tiene un 1 si
#' el profesor es de tiempo completo y 0 si no.
#' 
#' @examples
#' mat_nom_prof_total <- gen_mat_nom_prof_total()
#'
gen_mat_nom_prof_total <- function(){
  #' Se carga la matriz m_grande_total de 2015-1 a 2020-1 de la cual
  #' se va a obtener la lista de nombres que se desea
  load("Matrices m_grande_total/m_grande_total_20151_20201.RData")
  # View(m_grande_total)
  
  #' Se carga el vector con los nombres de los profesores de tiempo
  #' completo del Departamento de Matemáticas.
  vec_prof_TC <- carga_info_prof_tiempo_completo()#94
  
  #Se definen las variables que se van a utilizar:
  num_col_Profesor <- arroja_ind_col_MG("Profesor")##2
  vec_aux <- unique(m_grande_total[,num_col_Profesor])#1389
  # mat_nom_prof_total <- data.frame(Profesor = vec_aux,Tiempo_Completo = 0)
  mat_nom_prof_total <- matrix(0,nrow = length(vec_aux),ncol = 2)
  mat_nom_prof_total[,1] <- vec_aux
  
  #Se quitan los renglones sin información o NA's
  mat_nom_prof_total <- mat_nom_prof_total[mat_nom_prof_total[,1]!="",]
  mat_nom_prof_total <- mat_nom_prof_total[!is.na(mat_nom_prof_total[,1]),]
  
  #Recorre renglones de "mat_nom_prof_total"
  for(d in 1:dim(mat_nom_prof_total)[1]){
    nom_prof <- mat_nom_prof_total[d,1]
    if(any(vec_prof_TC==nom_prof)){
      mat_nom_prof_total[d,2] <- 1
    }
  }#Fin for(d)
  
  #' Hasta aquí hay 83 profesores de tiempo completo, se verán los
  #' casos faltantes (11):
  mat_aux <- mat_nom_prof_total[mat_nom_prof_total[,2]==1,]
  ind_aux <- 0
  for(d in 1:length(vec_prof_TC)){#Recorre los profesores de Tc
    if(any(vec_prof_TC[d]==mat_aux[,1])){
      ind_aux <- c(ind_aux,d)
    }
  }
  #Se quita el cero inicial
  ind_aux <- ind_aux[-1]
  
  #Vemos los casos faltantes
  vec_prof_TC_aux <- vec_prof_TC[-ind_aux]
  # ind_faltantes <- 1:length(vec_prof_TC)
  # ind_faltantes <- ind_faltantes[-ind_aux]
  #' 1) "Alejandro Ricardo Garciadiego Dantán": Diferencia de acentos (778)
  #' 2) "Ana Luisa Solís González Cosío": Diferencia en nombre (516,1193)
  #' 3) "Edith Corina Sáenz Valadéz": Diferencia de acentos (107)
  #' 4) "Emilio Lluis Puebla": Diferencia en nombre (735)
  #' 5) "Guillermo Sienra Loera": Diferencia en nombre (141)
  #' 6) "Isabel Puga Espinosa": Diferencia en nombre (150)
  #' 7) "Ma. Asunción Begoña Fernández Fernández": Diferencia en Ma.-María (667)
  #' 8) "María de Lourdes Velasco Arregui": Diferencia en nombre. (660,1240)
  #' 9) "Mucuy-kak del Carmen Guevara Aguirre": Diferencia en Kak y kak (127)
  #' 10) "Óscar Alfredo Palmas Velasco": Diferencia de acentos (172)
  #' 11) "Úrsula Iturrarán Viveros": Diferencia en nombre (351,871)
  #' 
  #' De estos 11 casos, vamos a modificar a mano: 1,3,7,9,10,11
  mat_nom_prof_total[c(778,516,107,735,141,150,667,660,127,172,351),2] <- 1
  colnames(mat_nom_prof_total) <- c("Profesor","Tiempo_Completo")
  save(mat_nom_prof_total, file = "mat_nom_prof_total.RData")
  
  return(mat_nom_prof_total)
}


# limpia_mat_nom_prof_total -----------------------------------------------
#' Title limpia_mat_nom_prof_total: Función que se encarga de limpiar los
#' nombres de los profesores de asginatura. Guarda la matriz
#' "mat_nom_prof_total".
#'
#' @examples
#' limpia_mat_nom_prof_total()
#' 
limpia_mat_nom_prof_total <- function(){
  load("mat_nom_prof_total.RData")
  
  prof_TC <- mat_nom_prof_total[mat_nom_prof_total[,2]==1,]
  prof_asig <- mat_nom_prof_total[mat_nom_prof_total[,2]==0,]
  
  #Vamos a trabajar con "vec_prof_asig"
  vec_prof_asig <- sort(prof_asig[,1])
  
  #Quitamos el "/" del inicio
  for(d in 1:9){
    num_char <- nchar(vec_prof_asig[d])
    vec_prof_asig[d] <- substr(vec_prof_asig[d],4,num_char)
  }
  vec_prof_asig <- sort(vec_prof_asig)
  
  #Vemos los nombres que tienen "/" y dejamos sólo el primer nombre
  for(d in 1:length(vec_prof_asig)){
    nom_prof <- vec_prof_asig[d]
    diagonal <- gregexpr(pattern ='/',nom_prof)
    
    if(diagonal[[1]] > 0){
      fin <- diagonal[[1]][1]
      # num_char <- nchar(vec_prof_asig[d])
      vec_prof_asig[d] <- substr(vec_prof_asig[d],1,(fin-1))
    }
  }
  vec_prof_asig <- sort(unique(vec_prof_asig))
  
  #' El siguiente for va a recorrer todos los nombres en "vec_prof_asig" y va
  #' a guardar los índices de aquellos nombres que tengan más de 50% de 
  #' coincidencia.
  mat_repeticiones <- data.frame(Profesor = vec_prof_asig,
                                 Nom1 = 0,Nom2 = 0,Nom3 = 0,Nom4 = 0,Nom5 = 0,
                                 Nom6 = 0,Nom7 = 0,Nom8 = 0,Nom9 = 0,Nom10 = 0)
  for(p in 1:(length(vec_prof_asig)-1)){
    cat("\n p = ",p)
    nom_prof_1 <- vec_prof_asig[p]
    cont <- 2
    for(r in (p+1):length(vec_prof_asig)){
      nom_prof_2 <- vec_prof_asig[r]
      compara <- stringsim(nom_prof_1,nom_prof_2)
      
      if(compara > 0.6){
        mat_repeticiones[p,cont] <- nom_prof_2
        cont <- cont + 1
      }
    }#Fin for(r)
  }#Fin for(p)
  
  #Vimos la matriz y escribimos los índices con los nombres repetidos
  ind_rep <- c(12,17,19,25,29,34,36,48,61,65,70,
               78,83,88,93,96,98,106,109,133,136,137,141,146,
               155,157,163,167,169,174,177,187,190,201,213,216,
               219,232,238,249,257,272,290,305,310,328:333,348,
               352,358,367,401,412,420,460,462:463,467,476,482,
               486,489,491,496,510,572,583,595,611,644,647,674,
               686,700,708,733,742,760,768,784,805,816,819,823,
               828:829,836,839,847,852,857,863,874,879,882,898,
               907,932,943,1002,1015,1042,1066,1069,1088,1106,
               1133,1142,1147,1160,1244)
  vec_prof_asig <- vec_prof_asig[-ind_rep]
  vec_prof_asig <- sort(vec_prof_asig)
  #'#' Nombres que se eligió uno de ellos
  # 156: Antonmaria Gerolamo Enrico Minzoni Alessio*
  # 157: Antonmaria Minzoni Alessio
  # 
  # 158: Araceli Arteaga Jiménez *
  # 163: Aracely Arteaga Jiménez 
  # 
  # 695: José de Jesús Carlos Quintanar Sierra *
  # 708: José Jesús Carlos Quintanar Sierra 
  # 
  # 767: Juan Manuel Eugenio Ramírez de Arellano Niño-Rincón*
  # 768: Juan Manuel Eugenio Ramírez de Arellano Niño Rincón
  # 
  # 829: Loiret Alejandria Dosal Trujillo
  # 830: Loiret Alejandría Dosal Trujillo*
  # 
  # 874: Ma. Susana Barrera Ocampo
  # 947: María Susana Barrera Ocampo*
  # 
  # 881: Manuel de Llano de la Garza*
  # 882: Manuel De Llano De la Garza
  # 
  # 1014: Mónica Alicia Clapp Jiménez-Labora*
  # 1015: Mónica Alicia Clapp Jiménez Labora
  # 
  # 1042: Omar Antolin Camarena
  # 1043: Omar Antolín Camarena*
  # 
  # 1133: Roberto Carrillo Larraga
  # 1134: Roberto Carrillo Lárraga*
  # 
  # 1142: Rocío Jauregui Renaud
  # 1143: Rocío Jáuregui Renaud*
  # 
  # 1146: Rodrigo Domínguez López*
  # 1147: Rodrígo Domínguez López
  # 
  # 1160: Rosalio Fernando Rodríguez Zepeda
  # 1161: Rosalío Fernando Rodríguez Zepeda*
  
  
  #' Ambos nombres se quedaron
  # 639: Jonás Raffael Martínez Sánchez
  # 1089: Rafael Martínez Sánchez
  
  num_prof <- length(vec_prof_asig) + dim(prof_TC)[1]
  mat_nom_prof_total <- data.frame(Profesor = 1:num_prof,Tiempo_Completo = 0)
  mat_nom_prof_total[1:dim(prof_TC)[1],] <- prof_TC
  mat_nom_prof_total[(dim(prof_TC)[1]+1):num_prof,1] <- vec_prof_asig
  
  save(mat_nom_prof_total, file = "mat_nom_prof_total.RData")
}



# quita_num_gpos_x_materia ------------------------------------------------
#' Title quita_num_gpos_x_materia: Función que se encarga de limpiar los
#' nombres de las materias de "vec_materias".
#'
#' @param vec_materias: Vector con los nombres de las materias, con el
#' número de grupos por cada materia.
#'
#' @return vec_materias: Vector con los nombres de las materias, sin el
#' número de grupos por cada materia.
#'
#' @examples
#' quita_num_gpos_x_materia(materias_act)
#' quita_num_gpos_x_materia(materias_CdC)
#' quita_num_gpos_x_materia(materias_mate)
#' quita_num_gpos_x_materia(materias_mateAp)
#' 
quita_num_gpos_x_materia <- function(vec_materias){
  # Quitamos el número de grupos por materia
  for(d in 1:length(vec_materias)){
    num_char <- nchar(vec_materias[d])
    texto <- substr(vec_materias[d],(num_char-5),num_char)
    if(texto==" grupo" || texto=="grupos"){#Menores a 10
      vec_materias[d] <- substr(vec_materias[d],1,(num_char-10))
    }
  }#Fin for(d)
  
  #Quitamos las comas finales
  for(d in 1:length(vec_materias)){
    num_char <- nchar(vec_materias[d])
    texto <- substr(vec_materias[d],num_char,num_char)
    if(texto==","){
      vec_materias[d] <- substr(vec_materias[d],1,(num_char-1))
    }
  }#Fin for(d)
  
  return(vec_materias)
}


# carga_info_materias_x_carrera -------------------------------------------
#' Title carga_info_materias_x_carrera: Función que extrae los nombres de
#' las materias de cada carrera del Depto. de Mate (Actuaría, CdC, Mate y
#' MateAp) en un vector. Dicho vector lo guarda.
#'
#' @return lista_materias_x_carrera: Lista con los vectores de nombres para
#' cada carrera.
#'
#' @examples
#' lista_materias_x_carrera <- carga_info_materias_x_carrera()
#' 
carga_info_materias_x_carrera <- function(){
  raiz <- "http://www.fciencias.unam.mx/docencia/horarios/indiceplan/20201/"
  
  ### Actuaría ###
  # http://www.fciencias.unam.mx/docencia/horarios/indiceplan/20201/2017
  # div > a #Código SelectorGadget Selecciona 96 materias
  url <- paste0(raiz,2017)
  # Sacamos la información de la página
  webpage <- read_html(url)
  materias_act_data_html <- html_nodes(webpage,'div > a')
  materias_act <- html_text(materias_act_data_html)  
  # materias_act
  
  #Quitamos las entradas vacías
  materias_act <- materias_act[materias_act!=""]
  
  # Quitamos entradas con FB TWITTER ...
  materias_act <- materias_act[1:90]#9 materias
  
  materias_act <- quita_num_gpos_x_materia(materias_act)
  # save(materias_act, file = paste0("materias_act.RData"))
  
  
  ### Ciencias de la Computación ###
  # http://www.fciencias.unam.mx/docencia/horarios/indiceplan/20201/1556
  # div > a#Selecciona 83 materias
  url <- paste0(raiz,1556)
  # Sacamos la información de la página
  webpage <- read_html(url)
  materias_CdC_data_html <- html_nodes(webpage,'div > a')
  materias_CdC <- html_text(materias_CdC_data_html)  
  # materias_CdC
  
  #Quitamos las entradas vacías
  materias_CdC <- materias_CdC[materias_CdC!=""]
  
  # Quitamos entradas con FB TWITTER ...
  materias_CdC <- materias_CdC[1:77]#77 materias
  
  materias_CdC <- quita_num_gpos_x_materia(materias_CdC)
  # save(materias_CdC, file = paste0("materias_CdC.RData"))
  
  
  ### Matemáticas ###
  # http://www.fciencias.unam.mx/docencia/horarios/indiceplan/20201/217
  # div > a#Selecciona 134 materias
  url <- paste0(raiz,217)
  # Sacamos la información de la página
  webpage <- read_html(url)
  materias_mate_data_html <- html_nodes(webpage,'div > a')
  materias_mate <- html_text(materias_mate_data_html)  
  # materias_mate
  
  #Quitamos las entradas vacías
  materias_mate <- materias_mate[materias_mate!=""]
  
  # Quitamos entradas con FB TWITTER ...
  materias_mate <- materias_mate[1:128]#128 materias
  
  materias_mate <- quita_num_gpos_x_materia(materias_mate)
  # save(materias_mate, file = paste0("materias_mate.RData"))
  
  
  ### Matemáticas Aplicadas ###
  # http://www.fciencias.unam.mx/docencia/horarios/indiceplan/20201/2055
  # div > a#Selecciona 74 materias
  url <- paste0(raiz,2055)
  # Sacamos la información de la página
  webpage <- read_html(url)
  materias_mateAp_data_html <- html_nodes(webpage,'div > a')
  materias_mateAp <- html_text(materias_mateAp_data_html)  
  # materias_mateAp
  
  #Quitamos las entradas vacías
  materias_mateAp <- materias_mateAp[materias_mateAp!=""]
  
  # Quitamos entradas con FB TWITTER ...
  materias_mateAp <- materias_mateAp[1:68]#68 materias
  
  materias_mateAp <- quita_num_gpos_x_materia(materias_mateAp)
  # save(materias_mateAp, file = paste0("materias_mateAp.RData"))
  
  lista_materias_x_carrera <- list()
  lista_materias_x_carrera[[1]] <- materias_act
  lista_materias_x_carrera[[2]] <- materias_CdC
  lista_materias_x_carrera[[3]] <- materias_mate
  lista_materias_x_carrera[[4]] <- materias_mateAp
  
  return(lista_materias_x_carrera)
}


# gen_mat_materias_x_carrera ----------------------------------------------
#' Title gen_mat_materias_x_carrera: Función que genera la lista con las
#' matrices que tienen los nombres de las materias de cada carrera y el
#' número de materia para cada materia.
#'
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
#' @return lista_mat_materias_x_carrera: Lista con las matrices que tienen
#' los nombres de las materias de cada carrera y el número de cada materia.
#'
#' @examples
#' lista_mat_materias_x_carrera <- gen_mat_materias_x_carrera(param)
#' 
gen_mat_materias_x_carrera <- function(param){
  #Se definen las variables que se van a utilizar
  lista_materias_x_carrera <- carga_info_materias_x_carrera()
  materias_act <- lista_materias_x_carrera[[1]]
  materias_CdC <- lista_materias_x_carrera[[2]]
  materias_mate <- lista_materias_x_carrera[[3]]
  materias_mateAp <- lista_materias_x_carrera[[4]]
  mat_materias_act <- data.frame(Materia = materias_act,Num_materia = 0)
  mat_materias_CdC <- data.frame(Materia = materias_CdC,Num_materia = 0)
  mat_materias_mate <- data.frame(Materia = materias_mate,Num_materia = 0)
  mat_materias_mateAp <- data.frame(Materia = materias_mateAp,Num_materia = 0)
  
  vec_carrera <- c("act","CdC","mate","mateAp")
  for(d in 1:length(vec_carrera)){
    # cat("\n d = ",d)
    carrera <- vec_carrera[d]
    switch(carrera,
           'act' = {cota = length(materias_act)
           mat_aux <- mat_materias_act},
           'CdC' = {cota = length(materias_CdC)
           mat_aux <- mat_materias_CdC},
           'mate' = {cota = length(materias_mate)
           mat_aux <- mat_materias_mate},
           'mateAp' = {cota = length(materias_mateAp)
           mat_aux <- mat_materias_mateAp}
    )
    for(r in 1:cota){
      # cat("\n r = ",r)
      materia <- mat_aux[r,1]
      num_materia <- which(materia == param$vec_nom_materias_total)
      if(length(num_materia) > 0){
        mat_aux[r,2] <- num_materia
      }else{
        mat_aux[r,2] <- 0
      }
    }
    switch(carrera,
           'act' = {mat_materias_act <- mat_aux},
           'CdC' = {mat_materias_CdC <- mat_aux},
           'mate' = {mat_materias_mate <- mat_aux},
           'mateAp' = {mat_materias_mateAp <- mat_aux}
    )
  }#Fin for(d)
  
  mat_materias_act[33,2] <- 297
  mat_materias_act[79,2] <- 330
  mat_materias_mateAp[65,2] <- 333
  mat_materias_mateAp[35,2] <- 88#Economía-Actuaría de Monserrat Esquivel López
  #http://www.fciencias.unam.mx/docencia/horarios/20201/2017/1540
  #http://www.fciencias.unam.mx/docencia/horarios/20201/2055/923
  
  lista_mat_materias_x_carrera <- list()
  lista_mat_materias_x_carrera[[1]] <- mat_materias_act
  lista_mat_materias_x_carrera[[2]] <- mat_materias_CdC
  lista_mat_materias_x_carrera[[3]] <- mat_materias_mate
  lista_mat_materias_x_carrera[[4]] <- mat_materias_mateAp
  names(lista_mat_materias_x_carrera) <- c("Actuaria","CdC","Mate",
                                           "MateAp")
  
  save(lista_mat_materias_x_carrera,
       file = paste0("lista_mat_materias_x_carrera.RData"))
}


# guarda_num_alum_x_carrera -----------------------------------------------
#' Title guarda_num_alum_x_carrera: Función que guarda el vector con el
#' número de alumnos por grupo de cada carrera.
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
#' @examples
#' guarda_num_alum_x_carrera(param,param_sim)
#' 
guarda_num_alum_x_carrera <- function(param,param_sim){
  #Se carga la lista con la información de las materias de cada carrera
  load("lista_mat_materias_x_carrera.RData")
  
  #Se definen las variables que se van a utilizar
  lista_num_alum_x_carrera <- list()
  param_sim$k_sem_ant = 25
  num_col_Alumnos <- arroja_ind_col_MG("Alumnos")##6
  # m_grande_total <- param$m_grande_total
  # mat_materias_act <- lista_mat_materias_x_carrera[[1]]
  # mat_materias_CdC <- lista_mat_materias_x_carrera[[2]]
  # mat_materias_mate <- lista_mat_materias_x_carrera[[3]]
  # mat_materias_mateAp <- lista_mat_materias_x_carrera[[4]]
  # View(mat_materias_act)
  # View(mat_materias_CdC)
  # View(mat_materias_mate)
  # View(mat_materias_mateAp)
  
  #Guardamos el vector con el número de alumnos para cada carrera
  for(d in 1:length(lista_mat_materias_x_carrera)){
    param_sim$Materias = lista_mat_materias_x_carrera[[d]][,1]
    m_filtrada <- gen_mat_m_filtrada(param,param_sim)
    lista_num_alum_x_carrera[[d]] <- m_filtrada[,num_col_Alumnos]
  }
  names(lista_num_alum_x_carrera) <- c("num_alum_actuaria","num_alum_CdC",
                                       "num_alum_mate","num_alum_mateAp")
  save(lista_num_alum_x_carrera,file = "lista_num_alum_x_carrera.RData")
}



##### DEMANDA DE ALUMNOS #####
#'Funciones encargadas de extraer, estimar y simular la demanda del número
#'de alumnos totales para "sem_sig".

# gen_mat_alumnos_corregidos ----------------------------------------------
#' Title gen_mat_alumnos_corregidos: Función encargada de extraer la
#' información de los semestres que se buscan. Regresa la matriz 
#' "mat_alumnos_corregidos" de 15 renglones (horas) y k+s-1 columnas, la
#' cual tiene el número total de alumnos por hora y por semestre. Se suma
#' el número de alumnos en los semestres repetidos por cada hora. Hay
#' ceros en los semestres y horas en donde no hay información.
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
#' repetidos por cada hora. Hay ceros en los semestres y horas en
#' donde no hay información.
#'
#' @examples
#' gen_mat_alumnos_corregidos(c(20182,20191,20192,20201),param,param_sim)
#' 
gen_mat_alumnos_corregidos <- function(vec_s_sem_k_info,param,param_sim){
  ##Se definen las variables que se van a utilizar:
  num_col_horario_num <- arroja_ind_col_MG("horario_num")##4
  num_col_Alumnos <- arroja_ind_col_MG("Alumnos")##6
  num_col_Semestre <- arroja_ind_col_MG("Semestre")##11
  m_filtrada <- param_sim$m_filtrada
  # horas_unicas <- sort(unique(m_filtrada[,num_col_horario_num]))##horas
  sem_con_info <- sort(unique(m_filtrada[,num_col_Semestre]))##semestres
  mat_alumnos_corregidos <- matrix(0,nrow = length(param$Horas),
                                   ncol = length(vec_s_sem_k_info))
  rownames(mat_alumnos_corregidos) <- param$nombre_hrs
  colnames(mat_alumnos_corregidos) <- vec_s_sem_k_info
  
  if(dim(m_filtrada)[1] == 0){
    mat_alumnos_corregidos <- matrix(0,nrow = length(param$Horas),
                                     ncol = length(vec_s_sem_k_info))
  }else{
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
          vec_aux <- as.numeric(mat_hora_alum[mat_hora_alum[,1]==param$Horas[r],2])
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
  }
  
  # View(mat_alumnos_corregidos)
  return(mat_alumnos_corregidos)
}



# simula_alumnos ----------------------------------------------------------
#' Title simula_alumnos: Función que genera el vector "vec_sim_1_sem", el
#' cual tiene la simulación de un semestre.
#'
#' @param mat_alumnos_corregidos: Submatriz de "mat_alumnos_corregidos" con la
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
#' vec_sim_1_sem <- simula_alumnos(mat_alumnos_corregidos,param)
#' 
simula_alumnos <- function(mat_alumnos_corregidos,param){
  ##Se definen las variables que se van a utilizar:
  q1 <- param$q1
  q2 <- param$q2
  vec_sim_1_sem <- rep(0,length(param$Horas))
  
  for(r in 1:length(param$Horas)){
    vec_alumnos <- mat_alumnos_corregidos[r,]
    if(sum(vec_alumnos) == 0){#En caso de que no haya información en el renglón
      vec_sim_1_sem[r] <- 0
    }else{
      tsData <- ts(vec_alumnos,frequency = 2)
      # Ajuste hw
      alumnos.fit.q <- hw(tsData,h=1,level = c(q1,q2),seasonal = "additive")
      cota1 <- max(0,alumnos.fit.q$lower[1])
      media <- max(0,alumnos.fit.q$mean[1])
      cota2 <- max(0,alumnos.fit.q$upper[2])
      vec_sim_1_sem[r] <- sample(ceiling(cota1):ceiling(cota2),1)
    }
  }#Fin for(r)
  return(vec_sim_1_sem)
}


# gen_mat_demanda_alumnos -------------------------------------------------
#' Title gen_mat_demanda_alumnos: Función que genera la matriz
#' "mat_demanda_alumnos" con 15 renglones (horas) y 203 columnas (materias).
#' En la entrada (i,j) se tiene el número de alumnos simulados para la hora
#' i, y la materia j.
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
#' @return mat_demanda_alumnos: Matriz de 15 renglones (horas) y 203
#' columnas (materias). En la entrada (i,j) se tiene el número de alumnos
#' simulados para la hora i, y la materia j.
#'
#' @examples
#' mat_demanda_alumnos <- gen_mat_demanda_alumnos(param,param_sim)
#' 
gen_mat_demanda_alumnos <- function(param,param_sim){
  ptm <- proc.time()# Start the clock!
  
  #Se definen las variables que vamos a utilizar
  mat_demanda_alumnos <- matrix(0,nrow = length(param$nombre_hrs),
                                ncol = length(param$vec_nom_materias_total))
  vec_s_sem_k_info <- gen_vec_s_sem_k_info(param_sim$vec_sem_sig,
                                           param_sim$k_sem_ant,param)
  
  for(d in 1:length(param$vec_nom_materias_total)){
    materia <- param$vec_nom_materias_total[d]
    cat("\n materia ",d,":",materia)
    param_sim$Materias = materia
    param_sim$m_filtrada <- gen_mat_m_filtrada(param,param_sim)
    mat_alumnos_corregidos <- gen_mat_alumnos_corregidos(vec_s_sem_k_info,
                                                         param,param_sim)
    vec_alum_sim <- simula_alumnos(mat_alumnos_corregidos,param)
    mat_demanda_alumnos[,d] <- vec_alum_sim
  }
  cat("\nLa función gen_mat_demanda_alumnos tardó: ",
      (proc.time()-ptm)[3]," segundos\n")##45.91
  rownames(mat_demanda_alumnos) <- param$nombre_hrs
  colnames(mat_demanda_alumnos) <- param$vec_nom_materias_total
  
  return(mat_demanda_alumnos)
}



##### SOLICITUDES #####
## Funciones que generan una matriz con solicitudes de todos los profesores.


# gen_solicitudes_1_profesor ----------------------------------------------
#' Title gen_solicitudes_1_profesor: Función que genera la solicitud de 
#' un solo profesor. Arroja la matriz "mat_1_solicitud" de 5 columnas
#' (Profesor,TC,Materia,Num_Materia,Horario) y 6 renglones que tiene la
#' información de la solicitud de "nom_prof". Se eligen 2 materias y hasta
#' 3 diferentes horarios.
#'
#' @param nom_prof: Nombre del profesor del que se va a obtener la solicitud.
#' @param tipo_prof: Variable binaria que vale 1 si el profesor es de
#' tiempo completo y cero si no.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' 
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return mat_1_solicitud: Matriz de 5 columnas (Profesor,TC,Materia,
#' Num_Materia,Horario) y 6 renglones que tiene la información de la
#' solicitud de "nom_prof". Se eligen 2 materias y hasta 3 diferentes
#' horarios.
#'
#' @examples
#' mat_1_solicitud <- gen_solicitudes_1_profesor("Arrigo Coen",0,param)
#' mat_1_solicitud <- gen_solicitudes_1_profesor("Margarita Chávez",1,param)
#' 
gen_solicitudes_1_profesor <- function(nom_prof,tipo_prof,param){
  #Se definen las variables que se van a utilizar
  num_col_Profesor <- arroja_ind_col_MG("Profesor")
  num_col_horario_num <- arroja_ind_col_MG("horario_num")##4
  num_col_NumMateria <- arroja_ind_col_MG("Num_materia")##37
  vec_nom_materias_total <- param$vec_nom_materias_total#202
  m_grande_2015 <- param$m_grande_2015#8409 37
  mat_1_solicitud <- data.frame(Profesor = 0,TC = 0, Materia = rep(0,6),
                                Num_Materia = 0,Horario = 0)
  
  #Se definen las variables con la información de "nom_prof"
  mat_1_prof <- m_grande_2015 %>% filter(Profesor == nom_prof)
  materias_num_prof <- as.numeric(unique(mat_1_prof[,num_col_NumMateria]))
  materias_num_prof <- materias_num_prof[materias_num_prof!=0]
  horas_prof <- unique(mat_1_prof[,num_col_horario_num])
  
  #Se llena las primeras 2 columnas
  mat_1_solicitud[,1] <- nom_prof
  mat_1_solicitud[,2] <- tipo_prof
  
  #Se llena las columnas "Materia" y "Num_Materia"
  if(length(materias_num_prof)==2){
    mat_1_solicitud[1:3,3] <- vec_nom_materias_total[materias_num_prof[1]]
    mat_1_solicitud[4:6,3] <- vec_nom_materias_total[materias_num_prof[2]]
    mat_1_solicitud[1:3,4] <- materias_num_prof[1]
    mat_1_solicitud[4:6,4] <- materias_num_prof[2]
  }else if(length(materias_num_prof)==1){
    mat_1_solicitud[,3] <- vec_nom_materias_total[materias_num_prof]
    mat_1_solicitud[,4] <- materias_num_prof
  }else if(length(materias_num_prof)>2){
    muestra_materias <- sample(materias_num_prof,size = 2)
    mat_1_solicitud[1:3,3] <- vec_nom_materias_total[muestra_materias[1]]
    mat_1_solicitud[4:6,3] <- vec_nom_materias_total[muestra_materias[2]]
    mat_1_solicitud[1:3,4] <- muestra_materias[1]
    mat_1_solicitud[4:6,4] <- muestra_materias[2]
  }
  
  #Se llena la columna "Horario"
  #' A lo más van a tener 3 horas diferentes
  if(length(horas_prof)==3){
    mat_1_solicitud[c(1,4),5] <- horas_prof[1]
    mat_1_solicitud[c(2,5),5] <- horas_prof[2]
    mat_1_solicitud[c(3,6),5] <- horas_prof[3]
  }else if(length(horas_prof)==2){
    mat_1_solicitud[c(1:2,4:5),5] <- horas_prof[1]
    mat_1_solicitud[c(3,6),5] <- horas_prof[2]
  }else if(length(horas_prof)==1){
    mat_1_solicitud[,5] <- horas_prof
  }else if(length(horas_prof)>3){
    muestra_horas <- sample(horas_prof,size = 3)
    mat_1_solicitud[c(1,4),5] <- muestra_horas[1]
    mat_1_solicitud[c(2,5),5] <- muestra_horas[2]
    mat_1_solicitud[c(3,6),5] <- muestra_horas[3]
  }
  
  return(mat_1_solicitud)
}


# gen_solicitudes ---------------------------------------------------------
#' Title gen_solicitudes: Función que genera la solicitud de todos los
#' profesores en la matriz "mat_nom_prof_total". Arroja la matriz
#' "mat_solicitudes" de 5 columnas (Profesor,TC,Materia,Num_Materia,Horario)
#' que tiene la información de las solicitudes de todos los profesores. Se
#' eligen 2 materias y hasta 3 diferentes horarios. Se quitan los renglones
#' repetidos.
#'
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' 
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return mat_solicitudes: Matriz de 5 columnas (Profesor,TC,Materia,
#' Num_Materia,Horario) y 6 renglones que tiene la información de la
#' solicitud de "nom_prof". Se eligen 2 materias y hasta 3 diferentes
#' horarios. Se quitan los renglones repetidos.
#'
#' @examples
#' mat_solicitudes <- gen_solicitudes(param)
#' 
gen_solicitudes <- function(param){
  # Start the clock!
  ptm <- proc.time()
  
  #Se definen las variables que se van a utilizar
  # num_col_Profesor <- arroja_ind_col_MG("Profesor")
  mat_nom_prof_total <- param$mat_nom_prof_total#1224 2
  m_grande_2015 <- param$m_grande_2015#8409 37
  mat_solicitudes <- data.frame(Profesor = 0,TC = 0, Materia = 0,
                                Num_Materia = 0,Horario = 0)
  
  #' Se quitan los renglones de ceros, con NA o vaciós en la
  #' columna de "Profesor"
  m_grande_2015 <- m_grande_2015 %>% filter(Profesor != "" & Profesor != 0 &
                                              !is.na(Profesor))
  # dim(m_grande_2015)#8395 37
  param$m_grande_2015 = m_grande_2015#8395 37
  
  #Recorre el nombre de los profesores de la matriz "mat_nom_prof_total"
  for(p in 1:dim(mat_nom_prof_total)[1]){
    nom_prof <- mat_nom_prof_total[p,1]
    tipo_prof <- mat_nom_prof_total[p,2]
    mat_1_solicitud <- gen_solicitudes_1_profesor(nom_prof,tipo_prof,param)
    mat_solicitudes <- rbind(mat_solicitudes,mat_1_solicitud)
  }
  #Se quita el renglón inicial de ceros
  mat_solicitudes <- mat_solicitudes[mat_solicitudes[,1]!=0,]#8322
  #Se deja la matriz sin repeticiones
  mat_solicitudes <- unique(mat_solicitudes)#4792
  
  cat("La función gen_solicitudes se tardó: ",(proc.time()-ptm)[3]," segundos\n\n\n" )
  
  return(mat_solicitudes)
}


# gen_solicitudes_real ----------------------------------------------------
#' Title gen_solicitudes_real: Función que genera la matriz de
#' solicitudes de los profesores que depende del esqueleto. Se genera
#' primero la matriz "mat_solicitudes" con la función gen_solicitudes()
#' y después se hace una "intersección" con los grupos simulados en la
#' matriz "mat_esqueleto" y así se obtienen las solicitudes pseudo-reales
#' de los profesores.
#'
#' @param mat_esqueleto: Matriz de 15 renglones (horas) y 203 columnas
#' (materias). En la entrada (i,j) se tiene el número de grupos simulados
#' para la hora i, y la materia j.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
#' @return mat_solicitudes_real: Matriz de 5 columnas (Profesor,TC,Materia,
#' Num_Materia,Horario) y 6 renglones que tiene la información de la
#' solicitud de "nom_prof". Se eligen 2 materias y hasta 3 diferentes
#' horarios. Se quitan los renglones repetidos. Se hace una "intersección"
#' con los grupos simulados en la matriz "mat_esqueleto" y así se obtienen
#' las solicitudes pseudo-reales de los profesores.
#'
#' @examples
#' mat_solicitudes_real <- gen_solicitudes_real(mat_esqueleto,param)
#' 
gen_solicitudes_real <- function(mat_esqueleto,param){
  ptm <- proc.time()# Start the clock!
  #Se definen las variables que se van a utilizar
  mat_solicitudes_real <- data.frame(Profesor = 0,TC = 0, Materia = 0,
                                     Num_Materia = 0,Horario = 0)
  mat_solicitudes <- gen_solicitudes(param)#7.98 seg
  #' Quitamos las materias y el horario = 0
  mat_solicitudes <- mat_solicitudes %>% filter(Materia != 0)
  mat_solicitudes <- mat_solicitudes %>% filter(Horario != 0)
  
  for(r in 1:dim(mat_solicitudes)[1]){#Recorre renglones de "mat_solicitudes"
    renglon <- mat_solicitudes[r,]
    ind_hora <- which(7:21 == as.numeric(renglon[5]))
    ind_materia <- as.numeric(mat_solicitudes[r,4])
    
    if(mat_esqueleto[ind_hora,ind_materia] > 0){
      mat_solicitudes_real <- rbind(mat_solicitudes_real,renglon)
    }
  }#Fin for(r)
  #Quitamos el renglón de ceros inicial
  mat_solicitudes_real <- mat_solicitudes_real %>% filter(Profesor != 0)
  
  cat("\nLa función gen_solicitudes_real tardó: ",(proc.time()-ptm)[3],
      " segundos\n")
  return(mat_solicitudes_real)
}



##### GMM #####
#' Funciones que aplican el modelo de mezcla de Normales para generar una
#' matriz con la demanda de alumnos para el siguiente semestre.


# gen_normalmixEM_inicial ----------------------------------------------
#' Title gen_normalmixEM_inicial: Función que genera un primer modelo de
#' mezcla de Normales para cada materia en "vec_nom_materias_total".
#'
#' @param vec_s_sem_k_info: Vector con los "k_sem_ant + s - 1" semestres
#' de los que se quiere obtener la información para realizar la simulación
#' del vector "vec_sem_sig".
#' @param D_prima_inicial: Matriz de 15 renglones (horas) y 203 columnas
#' (materias). En la entrada (i,j) se tiene el número de alumnos simulados
#' para la horai, y la materia j.
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
#' @return lista_mod_y_wait: Lista de 2 elementos. Cada elemento es una
#' lista. La primera contiene todos los modelos de mezcla de Normales,
#' uno para cada materia en "vec_nom_materias_total". La segunda lista
#' contiene los datos que se le pasan como parámetro a la función "normalmixEM"
#' como "wait".
#'
#' @examples
#' gen_normalmixEM_inicial(vec_s_sem_k_info,D_prima_inicial,
#' param,param_sim)
#' 
gen_normalmixEM_inicial <- function(vec_s_sem_k_info,D_prima_inicial,
                                    param,param_sim){
  #Se definen las variables que se van a utilizar
  mixmdl <- list()
  wait <- list()
  vec_nom_materias_total <- param$vec_nom_materias_total
  Horas <- param$Horas
  prom_alum_x_materia <- rep(0,length(vec_nom_materias_total))
  
  for(c in 1:length(vec_nom_materias_total)){
    num_materia <- c
    materia <- vec_nom_materias_total[num_materia]
    cat("\n Materia ",num_materia,": ",materia)
    D_prima_1_materia <- D_prima_inicial[,c]
    
    param_sim$Materias = materia
    param_sim$m_filtrada = gen_mat_m_filtrada(param,param_sim)
    mat_al_corregidos <- gen_mat_alumnos_corregidos(vec_s_sem_k_info,
                                                    param,param_sim)
    ##Convertimos los datos para obtener la distribución por horas
    wait_1_materia <- 0
    mat_aux <- cbind(mat_al_corregidos,D_prima_1_materia)
    prom_alum_x_materia[c] <- ceiling(mean(colMeans(mat_aux)))
    
    if(sum(mat_aux) > 0){
      for(h in 1:length(Horas)){
        suma_x_hra <- sum(mat_aux[h,])
        if(suma_x_hra > 0){
          wait_1_materia <- c(wait_1_materia,rep(Horas[h],suma_x_hra))
        }
      }
      #Quitamos el cero inicial
      wait_1_materia <- wait_1_materia[-1]
    }
    
    if(mean(wait_1_materia)>0 && length(unique(wait_1_materia))>1){
      #' La 2° condición evita que haya error si sólo se tienen datos
      #' a una sola hora.
      mixmdl_1_materia <- normalmixEM(wait_1_materia,
                                      mean=mean(wait_1_materia))
      # mixmdl_1_materia <- normalmixEM(wait_1_materia,
      #                                 mean=mean(wait_1_materia),
      #                                 k=3)
      # mixmdl_1_materia <- normalmixEM(wait_1_materia,k = 3)
      # mixmdl_1_materia <- normalmixEM(wait_1_materia,k = 2)
      #' Con el siguiente comando se tiene un mejor ajsute inicial, pero
      #' en algunas materias hay error por no encontrar un buen modelo
      # mixmdl_1_materia = normalmixEM(wait_1_materia)
    }else{
      mixmdl_1_materia <- 0
    }
    mixmdl[[c]] <- mixmdl_1_materia
    wait[[c]] <- wait_1_materia
  }#Fin for(c)
  
  lista_mod_y_wait <- list()
  lista_mod_y_wait[[1]] <- mixmdl
  lista_mod_y_wait[[2]] <- wait
  lista_mod_y_wait[[3]] <- prom_alum_x_materia
  return(lista_mod_y_wait)
}


# actualiza_calif_D -----------------------------------------------
#' Title actualiza_calif_D: Función que actualiza las calificaciones
#' del esqueleto por grupo y por materia. Las calificaciones dependen de la
#' diferencia relativa entre D y D_prima.
#'
#' @param D: Matriz mat_demanda_alumnos, de 15 renglones (horas) y 203
#' columnas (materias). En la entrada (i,j) se tiene el número de alumnos
#' simulados para la hora i, y la materia j.
#' @param D_prima: Matriz de 15 renglones (horas) y 203 columnas (materias).
#' En la entrada (i,j) se tiene el número de alumnos simulados para la hora
#' i, y la materia j.
#' @param mat_calif_x_gpo: Matriz de 15 renglones (horas) y 203
#' columnas (materias). Contiene las calificaciones por grupo.
#' @param ind_materias: Vector con los índices de las materias que deben
#' de modificarse.
#'
#' @return calif_D: Lista de 2 elementos: "mat_calif_x_gpo" y
#' "vec_calif_x_materia". La matriz "mat_calif_x_gpo" (15*203) contiene las
#' calificaciones por grupo. El vector "vec_calif_x_materia"
#'
#' @examples
#' calif_D <- actualiza_calif_D(D,D_prima,mat_calif_x_gpo,
#' ind_materias)
#' 
actualiza_calif_D <- function(D,D_prima,mat_calif_x_gpo,ind_materias){
  #Se definen las variables que se van a utilizar
  calif_A <- mat_calif_x_gpo
  calif_B <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])
  
  for(c in ind_materias){#Recorre columnas
    for(r in 1:dim(D)[1]){#Recorre renglones
      if(D[r,c] > 0){
        if(D_prima[r,c] != D[r,c]){
          #La calificación es negativa si sobran alumnos
          #La calificación es positiva si faltan alumnos
          calif_B[r,c] <- (D[r,c] - D_prima[r,c])/D[r,c]
        }
      }else{#Para no tener -Inf
        if(D_prima[r,c] < D[r,c]){#Si faltan alumnos
          calif_B[r,c] <- 1
        }else if(D_prima[r,c] > D[r,c]){#Si sobran alumnos
          calif_B[r,c] <- -1}}
      
      if((calif_A[r,c]<0 && calif_B[r,c]>0) ||#Si antes faltaban y ahora sobran
         (calif_A[r,c]>0 && calif_B[r,c]<0)){#Si antes sobraban y ahora faltan
        calif_A[r,c] <- 0
        calif_B[r,c] <- 0
      }
      
    }#Fin for(r)
  }#Fin for(c)
  mat_calif_x_gpo <- calif_A + calif_B
  # vec_calif_x_materia <- colSums(mat_calif_x_gpo)
  vec_calif_x_materia <- colMeans(mat_calif_x_gpo)
  
  calif_D <- list()
  calif_D[[1]] <- mat_calif_x_gpo
  calif_D[[2]] <- vec_calif_x_materia
  return(calif_D)
}


# actualiza_D_prima -------------------------------------------------------
#' Title actualiza_D_prima: Función encargada de actualizar D_prima.
#'
#' @param cota: Cota para que el ciclo no sea infinito.
#' @param D: Matriz mat_demanda_alumnos, de 15 renglones (horas) y 203
#' columnas (materias). En la entrada (i,j) se tiene el número de alumnos
#' simulados para la hora i, y la materia j.
#' @param D_prima: Matriz de 15 renglones (horas) y 203 columnas (materias).
#' En la entrada (i,j) se tiene el número de alumnos simulados para la hora
#' i, y la materia j. 
#' @param mixmdl: Lista con "m" elementos. Cada elemento es el modelo de
#' mezcla de Normales para una materia.
#' @param calif_esq: Lista con 2 elementos: "mat_calif_x_gpo" y
#' "vec_calif_x_materia". La matriz "mat_calif_x_gpo" (15*203) contiene las
#' calificaciones por grupo. El vector "vec_calif_x_materia"
#' @param ind_materias: Vector con los índices de las materias que deben
#' de modificarse.
#'
#' @return D_prima: Matriz de 15x203 actualizada. En la entrada (i,j) se
#' tiene el nuevo número de alumnos simulados para la hora i, y la materia j.
#'
#' @examples
#' actualiza_D_prima(500,D,D_prima,mixmdl,calif_esq,c(5,182))
#' actualiza_D_prima(cota,D,D_prima,mixmdl,calif_esq,ind_materias)
#' 
actualiza_D_prima <- function(cota,D,D_prima,mixmdl,calif_esq,ind_materias){
  #' Para este punto ya comparamos D y D_prima. Se redefine D_prima.
  #' Recibe a D_prima como parámetro para que en caso de que no haya
  #' modificaciones, se regrese la misma matriz y no una llena de ceros.
  
  mat_calif_x_gpo <- calif_esq[[1]]
  vec_calif_x_materia <- calif_esq[[2]]
  for(c in ind_materias){#Recorre columnas
    cont_1 <- 1
    cont_2 <- 1
    if(sum(vec_calif_x_materia[c])>10 || 
       sum(vec_calif_x_materia[c]) < -20){#Sólo modificamos si
      #' la califición total de la materia está fuera de [-20,10]
      for(h in 1:length(param$Horas)){#Recorre las horas (renglones)
        # cat("\n h = ",h)
        if(length(mixmdl) > 0){
          (rand_num <- ceiling(rnorm(1,mixmdl[[c]]$mu,mixmdl[[c]]$sigma)))
          if(mat_calif_x_gpo[h,c] > 10){#Si faltan alumnos
            while(rand_num <= D[h,c]){
              (rand_num <- ceiling(rnorm(1,mixmdl[[c]]$mu,mixmdl[[c]]$sigma)))
              cont_1 <- cont_1 + 1#Para no tener ciclo infinito
              if(cont_1 >= cota){
                break;
              }
            }
            cont_1 <- 1#Reiniciamos el contador
            D_prima[h,c] <- max(0,rand_num)
          }
          if(mat_calif_x_gpo[h,c] < -10 && D[h,c]>0){#Si sobran alumnos
            #'La 2° cond. es para que no haya simulación si no hay alumnos en D
            #'Aquí la calificación debe ser menor a -10 porque es por
            #'grupo no por materia (ver gráficas de diferencias relativas
            #'entre D y E)
            while(rand_num > D[h,c]){
              (rand_num <- ceiling(rnorm(1,mixmdl[[c]]$mu,mixmdl[[c]]$sigma)))
              cont_2 <- cont_2 + 1#Para no tener ciclo infinito
              if(cont_2 >= cota){
                break;
              }
            }
            cont_2 <- 1#Reiniciamos el contador
            D_prima[h,c] <- max(0,rand_num)
          }
        }
      }#Fin for(h)
    }#Fin if()
  }#Fin for(c)
  return(D_prima)
}



# gen_normalmixEM_1_materia -----------------------------------------------
#' Title gen_normalmixEM_1_materia: Función que genera un modelo de
#' mezcla de Normales para una materia.
#'
#' @param wait_1_materia: Parámetro "wait" de una materia.
#' @param mixmdl_1_materia: Modelo de mezcla de Normales de una materia.
#'
#' @return mixmdl_1_materia: Modelo de mezcla de Normales actualizado de
#' una materia.
#'
#' @examples
#' gen_normalmixEM_1_materia(wait_1_materia,mixmdl_1_materia)
#' 
gen_normalmixEM_1_materia <- function(wait_1_materia,mixmdl_1_materia){
  #Se definen las variables que se van a utilizar
  
  if(mean(wait_1_materia) > 0){
    mixmdl_1_materia = normalmixEM(wait_1_materia,mean = mixmdl_1_materia$mu)
  }else{
    mixmdl_1_materia <- 0
  }
  return(mixmdl_1_materia)
}


# actualiza_mixmdl --------------------------------------------------------
#' Title actualiza_mixmdl: Función que actualiza todos los modelos de
#' mezcla de Normales para cada materia con el índice en "ind_materias".
#'
#' @param lista_mod_y_wait: Lista de 2 elementos. Cada elemento es una
#' matriz. La primera contiene todos los modelos de mezcla de Normales,
#' uno para cada materia en "vec_nom_materias_total". La segunda matriz
#' contiene los datos que se le pasan como parámetro a la función "normalmixEM"
#' como "wait".
#' @param D_prima: Matriz de 15 renglones (horas) y 203 columnas (materias).
#' En la entrada (i,j) se tiene el número de alumnos simulados para la hora
#' i, y la materia j.
#' @param ind_materias: Vector con los índices de las materias que deben
#' de modificarse.
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
#' @return lista_mod_y_wait: Lista actualizada de 2 elementos. Cada elemento
#' es una matriz. La primera contiene todos los modelos de mezcla de Normales,
#' uno para cada materia en "vec_nom_materias_total". La segunda matriz
#' contiene los datos que se le pasan como parámetro a la función "normalmixEM"
#' como "wait".
#'
#' @examples
#' actualiza_mixmdl(lista_mod_y_wait,D_prima,ind_materias,param,param_sim)
#' 
actualiza_mixmdl <- function(lista_mod_y_wait,D_prima,ind_materias,
                             param,param_sim){
  mixmdl <- lista_mod_y_wait[[1]]
  wait <- lista_mod_y_wait[[2]]
  
  for(c in ind_materias){
    num_materia <- c
    materia <- vec_nom_materias_total[num_materia]
    D_prima_1_materia <- D_prima[,c]
    wait_1_materia <- c(wait[[c]],D_prima_1_materia)
    mixmdl_1_materia <- mixmdl[[c]]
    mixmdl_1_materia <- gen_normalmixEM_1_materia(wait_1_materia,
                                                  mixmdl_1_materia)
    lista_mod_y_wait[[1]][[c]] <- mixmdl_1_materia
    lista_mod_y_wait[[2]][[c]] <- wait_1_materia
  }#Fin for(c)
  
  return(lista_mod_y_wait)
}



# gen_D_prima -------------------------------------------------------------
#' Title gen_D_prima: Función que actualiza "D_prima". Matriz de 15x203. En
#' la entrada (i,j) se tiene el número de alumnos simulados para la hora i,
#' y la materia j.
#'
#' @param D: Matriz mat_demanda_alumnos, de 15 renglones (horas) y 203
#' columnas (materias). En la entrada (i,j) se tiene el número de alumnos
#' simulados para la hora i, y la materia j.
#' @param D_prima_inicial: Primera matriz D_prima, de 15x203. En la entrada
#' (i,j) se tiene el número de alumnos simulados para la hora i, y la
#' materia j.
#' @param lista_mod_y_wait: Lista de 2 elementos. Cada elemento es una
#' matriz. La primera contiene todos los modelos de mezcla de Normales,
#' uno para cada materia en "vec_nom_materias_total". La segunda matriz
#' contiene los datos que se le pasan como parámetro a la función "normalmixEM"
#' como "wait".
#' @param cota: Cota para que el ciclo no sea infinito.
#'
#' @return D_prima: Matriz "D_prima" actualizada, de 15x203. En la entrada
#' (i,j) se tiene el número de alumnos simulados para la hora i, y la
#' materia j.
#'
#' @examples
#' gen_D_prima(D,D_prima_inicial,lista_mod_y_wait,cota)
#' 
gen_D_prima <- function(D,D_prima_inicial,lista_mod_y_wait,cota){
  #Se definen las variables que se van a utilizar
  mat_calif_x_gpo <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])
  cont <- 1
  D_prima <- D_prima_inicial
  mixmdl <- lista_mod_y_wait[[1]]
  # wait <- lista_mod_y_wait[[2]]
  
  #Calificación inicial
  calif_D <- actualiza_calif_D(D,D_prima,mat_calif_x_gpo,
                               1:dim(D)[2])
  mat_calif_x_gpo <- calif_D[[1]]
  vec_calif_x_materia <- calif_D[[2]]
  
  #' Actualizo mientras se cumplan las siguientes condiciones:
  while(any(vec_calif_x_materia< -20) || any(vec_calif_x_materia> 10)){
    ind_1 <- which(vec_calif_x_materia< -20)
    ind_2 <- which(vec_calif_x_materia> 10)
    ind_materias <- union(ind_1,ind_2)
    if(length(ind_materias) > 0){
      D_prima <- actualiza_D_prima(cota,D,D_prima,mixmdl,calif_D,
                                   ind_materias)
      
      calif_D <- actualiza_calif_D(D,D_prima,mat_calif_x_gpo,
                                   ind_materias)
      mat_calif_x_gpo <- calif_D[[1]]
      vec_calif_x_materia <- calif_D[[2]]
      
      lista_mod_y_wait <- actualiza_mixmdl(lista_mod_y_wait,D_prima,
                                           ind_materias,param,param_sim)
      mixmdl <- lista_mod_y_wait[[1]]
      
    }else{
      cat("\n Todas las calificaciones están dentro del intervalo [-20,10]")
      break;
    }
    if(cont >= cota){
      break;
    }
    cont <- cont + 1
  }#Fin while()
  return(D_prima)
}


##### ESQUELETO #####
## Funciones que generan un esqueleto del siguiente semestre.


# simula_alum_x_profesor --------------------------------------------------
#' Title simula_alum_x_profesor: Función que simula el número de alumnos
#' para un profesor y una materia. Se obtiene la información del número de
#' alumnos que ha tenido el profesor (del 2015-1 al 2020-1), se toma el
#' mín y el máx, se simula una uniforme en ese intervalo, se redondea el
#' valor con la función ceiling y así se obtiene el valor simulado.
#'
#' @param renglon: Vector con la información del profesor elegido para
#' asignarle un grupo (profesor,tipo_profesor,materia_al,num_materia_al,
#' hora_al).
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' 
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return num_alum_x_profesor: Número de alumnos simulados de un profesor
#' y una materia.
#'
#' @examples
#' num_alum_x_profesor <- simula_alum_x_profesor(renglon,param)
#' 
simula_alum_x_profesor <- function(renglon,param){
  #Se definen las variables que se van a utilizar
  nom_prof <- as.character(renglon[1])
  num_materia <- as.numeric(renglon[4])
  m_grande_2015 <- param$m_grande_2015
  sub_mat <- m_grande_2015 %>% filter(!is.na(Profesor))%>% filter(
    Profesor == nom_prof)%>% filter(Num_materia == num_materia)
  
  num_Alumnos <- as.numeric(sub_mat$Alumnos)
  num_alum_x_profesor <- sample(min(num_Alumnos):max(num_Alumnos), size=1)
  # num_alum_x_profesor <- ceiling(runif(1,min = min(num_Alumnos),
  #                                      max = max(num_Alumnos)))
  return(num_alum_x_profesor)
}


# verifica_demanda_cubierta -----------------------------------------------
#' Title verifica_demanda_cubierta: Función que arroja un 1 si aún existen
#' alumnos en la i-ésima hora y en la materia j, cero si no.
#'
#' @param mat_demanda_aux: Matriz con la demanda de alumnos que se le están
#' constantemente restando los alumnos ya simulados.
#' @param renglon: Vector con la información del profesor elegido para
#' asignarle un grupo (profesor,tipo_profesor,materia,num_materia,hora).
#'
#' @return sobran_alum_1si_0no: Variable binaria que vale 1 si aún existen
#' alumnos en la i-ésima hora y en la materia j, cero si no.
#'
#' @examples
#' sobran_alum_1si_0no <- verifica_demanda_cubierta(mat_demanda_aux,
#' renglon,mat_demanda_alumnos)
#' 
verifica_demanda_cubierta <- function(mat_demanda_aux,renglon,
                                      mat_demanda_alumnos){
  # Se definen las variables que se van a utilizar
  ind_hora <- which(7:21 == as.numeric(renglon[5]))
  ind_materia <- as.numeric(renglon[4])
  sobran_alum_1si_0no <- 0
  
  #' Se evitan errores cuando no hay hora en la columna "Horario" o
  #' el número de materia es cero
  if(length(ind_hora>0) && ind_materia!=0){
    (num_alum_aux <- mat_demanda_aux[ind_hora,ind_materia])
    if(num_alum_aux > 0){#Si sobran alumnos
      sobran_alum_1si_0no <- 1
      (num_alum <- mat_demanda_alumnos[ind_hora,ind_materia])
      if(num_alum_aux<=5 && num_alum>=10){
        #' Cota para evitar seguir simulando alumnos. Si originalmente
        #' se simularon más de 10 alumnos en el grupo y ahora se tienen
        #' 5 alumnos o menos entonces ya no se simulan más grupos
        sobran_alum_1si_0no <- 0
      }
    }
  }
  
  return(sobran_alum_1si_0no)
}


# actualiza_mat_solicitudes -----------------------------------------------
#' Title actualiza_mat_solicitudes: Función que actualiza la matriz
#' "mat_solicitudes". Le quitamos las horas que ya dieron los profesores.
#'
#' @param mat_a_actualizar: Matriz de solictudes. Puede ser de los
#' profesores de tiempo completo o de asignatura.
#' @param renglon: Vector con la información del profesor elegido para
#' asignarle un grupo (profesor,tipo_profesor,materia_al,num_materia_al,
#' hora_al).
#' @param prof_max_asig: Variable binaria que vale 1 si el profesor ya tiene
#' 2 o más materias asignadas y 0 si no.
#'
#' @return mat_solicitudes_act: Matriz de solicitudes actualizada.
#'
#' @examples
#' mat_solicitudes_act <- actualiza_mat_solicitudes(mat_a_actualizar,renglon,
#' prof_max_asig)
#' 
actualiza_mat_solicitudes <- function(mat_a_actualizar,renglon,prof_max_asig){
  # Se definen las variables que se van a utilizar
  profesor <- which(mat_a_actualizar[,1]==renglon[1])
  
  if(prof_max_asig == 1){
    mat_solicitudes_act <- mat_a_actualizar[-profesor,]
  }else{
    hora <- which(mat_a_actualizar[,5]==renglon[5])
    #' Se intersectan los conjuntos para tener los índices que se
    #' deben retirar de la matriz
    indices <- intersect(profesor,hora)
    if(length(indices) > 0){
      mat_solicitudes_act <- mat_a_actualizar[-indices,]
    }else{
      mat_solicitudes_act <- mat_a_actualizar
    }
  }
  
  return(mat_solicitudes_act)
}


# ciclo_esqueleto ---------------------------------------------------------
#' Title ciclo_esqueleto: Función auxiliar de "gen_esqueleto" encargada de
#' realizar el ciclo. Recibe las matrices dependiendo si se está en el caso
#' de profesores de tiempo completo o de asignatura.
#'
#' @param cota: Cota para que el ciclo no sea infinito.
#' @param mat_solicitudes: Matriz de 5 columnas (Profesor,TC,Materia,
#' Num_Materia,Horario) que tiene la información de las solicitudes de los
#' profesores.
#' @param mat_prof: Matriz de 2 columnas con el nombre de los profesores y
#' el número de materias que se le han asignado.
#' @param mat_demanda: Matriz de 15 renglones (horas) y 203 columnas
#' (materias). En la entrada (i,j) se tiene el número de alumnos restantes
#' para la hora i, y la materia j.
#' @param num_max_asig: Número máximo de materias que se le pueden asignar
#' a un profesor.
#' @param mat_demanda_alumnos: Matriz de 15 renglones (horas) y 203 columnas
#' (materias). En la entrada (i,j) se tiene el número de alumnos simulados
#' para la hora i, y la materia j.
#'
#' @return lista_ciclo: Lista con la matriz "mat_esqueleto" y la matriz
#' "mat_demanda" actualizados.
#'
#' @examples
#' lista_ciclo <- ciclo_esqueleto(1000,mat_solicitudes_TC,mat_prof_TC,
#' mat_esqueleto,mat_demanda_alumnos)
#' lista_ciclo <- ciclo_esqueleto(6500,mat_solicitudes_asig,mat_prof_asig,
#' mat_esqueleto,mat_demanda_alumnos)
#' 
ciclo_esqueleto <- function(cota,mat_solicitudes,mat_prof,mat_demanda,
                            num_max_asig,mat_demanda_alumnos){
  #Se definen las variables que se van a utilizar
  mat_esqueleto <- matrix(0,nrow = length(param$Horas),
                          ncol = length(param$vec_nom_materias_total))
  rownames(mat_esqueleto) <- param$Horas
  colnames(mat_esqueleto) <- param$vec_nom_materias_total
  
  for(n in 1:cota){#Cota para que el ciclo no sea infinito
    # cat("\n Iteración: ",n)
    #Se suman los valores positivos de la demanda de alumnos
    if(sum(mat_demanda[mat_demanda>0])>0 && dim(mat_solicitudes)[1]>0){
      #Número aleatorio para elegir profesor
      (num_al <- sample(x = 1:dim(mat_prof)[1], size = 1))
      (profesor <- mat_prof[num_al,1])
      mat_aux <- mat_solicitudes %>% filter(Profesor == profesor) %>% filter(
        !is.na(Materia))
      
      if(dim(mat_aux)[1]>0){#Si hay información de "profesor"
        #Número aleatorio para elegir materia
        (num_al_2 <- sample(x = 1:dim(mat_aux)[1], size = 1))
        (materia_al <- mat_aux[num_al_2,3])
        (num_materia_al <- mat_aux[num_al_2,4])
        
        #Número aleatorio para elegir horario
        (num_al_3 <- sample(x = 1:dim(mat_aux)[1], size = 1))
        (hora_al <- mat_aux[num_al_3,5])
        (TC <- mat_aux[num_al_3,2])
        
        (renglon <- c(profesor,TC,materia_al,num_materia_al,hora_al))
        
        #Índices de renglón y columna para "mat_esqueleto"
        (M_i <- which(param$Horas == as.numeric(renglon[5])))
        (M_j <- as.numeric(renglon[4]))
        
        #Se verifica si la demanda ha sido cubierta o no
        (sobran_alum_1si_0no <- verifica_demanda_cubierta(mat_demanda,
                                                          renglon,
                                                          mat_demanda_alumnos))
        
        if(sobran_alum_1si_0no == 1){
          #' Aún hay alumnos sin clase para esa materia. Simulamos el
          #' número de alumnos para este grupo
          (num_alum_x_profesor <- simula_alum_x_profesor(renglon,param))
          # num_alum_simulados <- num_alum_simulados + num_alum_x_profesor
          # mat_E[M_i,M_j] <- mat_E[M_i,M_j]+num_alum_x_profesor
          
          #Se actualizan las entradas de las matrices auxiliares
          # mat_demanda[M_i,M_j] <- max(0,mat_demanda[M_i,M_j]-num_alum_x_profesor)
          #' Permitimos los negativos que nos muestran los alumnos sobrantes
          mat_demanda[M_i,M_j] <- mat_demanda[M_i,M_j]-num_alum_x_profesor
          mat_esqueleto[M_i,M_j] <- mat_esqueleto[M_i,M_j] + 1
          
          mat_prof[num_al,2] <- as.numeric(mat_prof[num_al,2]) + 1
          prof_max_asig <- 0
          if(mat_prof[num_al,2] >= num_max_asig){
            prof_max_asig <- 1
          }
          mat_solicitudes <- actualiza_mat_solicitudes(mat_solicitudes,
                                                       renglon,prof_max_asig)
        }#Fin if(dim(mat_aux)[1]>0)
      }#Fin if Demanda existente
    }#Fin if Condiciones de paro
  }#Fin for(n)
  
  lista_ciclo <- list()
  lista_ciclo[[1]] <- mat_esqueleto
  lista_ciclo[[2]] <- mat_demanda
  names(lista_ciclo) <- c("mat_esqueleto","mat_demanda")
  return(lista_ciclo)
}



# gen_esqueleto -----------------------------------------------------------
#' Title gen_esqueleto: Función que arroja la matriz "mat_esqueleto". Matriz
#' de 15 renglones (horas) y 203 columnas (materias). En la entrada (i,j)
#' se tiene el número de grupos simulados para la hora i, y la materia j.
#'
#' @param mat_demanda_alumnos: Matriz de 15 renglones (horas) y 201
#' columnas (materias). En la entrada (i,j) se tiene el número de alumnos
#' simulados para la hora i, y la materia j.
#' @param mat_solicitudes: Matriz de 4 columnas (Profesor,TC,Materia,
#' Horario). Tiene la información de las solicitudes de los profesores. Se
#' eligen hasta dos materias y hasta 3 diferentes horarios. Se quitan los
#' renglones repetidos.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return mat_esqueleto: Matriz de 15 renglones (horas) y 203 columnas
#' (materias). En la entrada (i,j) se tiene el número de grupos simulados
#' para la hora i, y la materia j.
#'
#' @examples
#' mat_esqueleto <- gen_esqueleto(mat_demanda_alumnos,mat_solicitudes,
#' param)
#' 
gen_esqueleto <- function(mat_demanda_alumnos,mat_solicitudes,param){
  ptm <- proc.time()# Start the clock!
  #Se definen las variables que se van a utilizar
  num_max_asig <- param$num_max_asig
  mat_solicitudes <- data.frame(mat_solicitudes)
  mat_solicitudes_aux <- mat_solicitudes %>% filter(mat_solicitudes$Horario > 0)
  # mat_solicitudes_aux <- mat_solicitudes[as.numeric(mat_solicitudes[,5])>0,]
  # colnames(mat_solicitudes_aux) <- c("Profesor","TC","Materia",
  #                                    "Num_Materia","Horario")
  
  ##### Profesores de tiempo completo
  mat_solicitudes_TC <- mat_solicitudes_aux %>% filter(TC == 1)
  nom_prof_TC <- unique(mat_solicitudes_TC[,1])
  mat_prof_TC <- data.frame(Profesor = nom_prof_TC,Materias_Asig = 0)
  lista_ciclo_TC <- ciclo_esqueleto(param$cota_TC,mat_solicitudes_TC,
                                    mat_prof_TC,mat_demanda_alumnos,
                                    num_max_asig,mat_demanda_alumnos)
  
  ##### Profesores de asignatura
  mat_solicitudes_asig <- mat_solicitudes_aux %>% filter(TC == 0)
  nom_prof_asig <- unique(mat_solicitudes_asig[,1])
  mat_prof_asig <- data.frame(Profesor = nom_prof_asig,Materias_Asig = 0)
  
  lista_ciclo_asig <- ciclo_esqueleto(param$cota_asig,
                                      mat_solicitudes_asig,
                                      mat_prof_asig,lista_ciclo_TC[[2]],
                                      num_max_asig,mat_demanda_alumnos)
  
  #' Se suman los grupos de los profesores de tiempo completo y los
  #' de asignatura.
  mat_esqueleto <- lista_ciclo_TC[[1]] + lista_ciclo_asig[[1]]
  rownames(mat_esqueleto) <- param$nombre_hrs
  colnames(mat_esqueleto) <- param$vec_nom_materias_total
  
  cat("\nLa función gen_esqueleto tardó: ",(proc.time()-ptm)[3],
      " segundos\n")#19.48seg
  return(mat_esqueleto)
}



##### METODOLOGÍAS #####
#' Funciones que simulan un esqueleto. Se probaron 4 metodologías distintas.


# Metodología A -----------------------------------------------------------
#' Title metodo_A: Función que genera un esqueleto con la metodología A, la
#' cual implementa la mezcla de normales por materia y modifica el número de
#' alumnos en D si la calificación por materia está fuera de [-20,10] y 
#' si la calificación por grupo está fuera de [-10,10].
#' En esta metodología en la función "gen_normalmixEM_inicial" el modelo
#' se genera con el promedio de los datos, no se utiliza el parámatro k
#' que es el número de normales que tiene el modelo. El esqueleto se simula
#' con la función "gen_esqueleto" utilizando la matriz D' actualizada.
#'
#' @param cota: Cota para que el ciclo no sea infinito.
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
#' @return lista_esq_D_prima: Lista que contiene 2 matrices, la matriz
#' "mat_esqueleto" con el esqueleto generado y la matriz "D_prima" la
#' cual contiene la matriz de demanda de alumnos con la que se generó el
#' esqueleto.
#'
#' @examples
#' lista_esq_D_prima <- metodo_A(cota,param,param_sim)
#' 
metodo_A <- function(cota,param,param_sim){
  ptm <- proc.time()# Start the clock!
  #' Simulamos D0 (matriz de demanda con la que vamos a calificar a la
  #' matriz D').
  D0 <- gen_mat_demanda_alumnos(param,param_sim)#45.94 seg
  
  ### Obtener D'0 = E
  mat_solicitudes <- gen_solicitudes(param)#7.21 seg
  lista_info_esqueleto <- gen_esqueleto(D0,mat_solicitudes,param)#13.58 seg
  E <- lista_info_esqueleto[[8]]#Matriz con el número de alumnos simulados
  
  ### Aplicar mezcla de normales inicial
  vec_s_sem_k_info <- gen_vec_s_sem_k_info(param_sim$vec_sem_sig,
                                           param_sim$k_sem_ant,param)
  lista_mod_y_wait <- gen_normalmixEM_inicial(vec_s_sem_k_info,E,
                                              param,param_sim)
  
  ### Obtener D' para generar esqueleto
  D_prima <-  gen_D_prima(D0,E,lista_mod_y_wait,cota)
  
  ##Calificamos D_prima
  # mat_calif_x_gpo <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])
  # ind_materias <- 1:dim(D)[2]
  # calif_D <- actualiza_calif_D(D0,D_prima,mat_calif_x_gpo,ind_materias)
  # vec_calif_x_materia <- calif_D[[2]]
  # View(vec_calif_x_materia)
  
  ##Generar esqueleto
  mat_solicitudes <- gen_solicitudes(param)#8.94 seg
  mat_esqueleto <- gen_esqueleto(D_prima,mat_solicitudes,param)#14.05 seg
  rownames(mat_esqueleto) <- param$Horas
  colnames(mat_esqueleto) <- param$vec_nom_materias_total
  
  
  lista_esq_D_prima <- list()
  lista_esq_D_prima[[1]] <- mat_esqueleto
  lista_esq_D_prima[[2]] <- D_prima
  
  cat("\nLa metodología A tardó: ",(proc.time()-ptm)[3]/60,
      " minutos\n")
  return(lista_esq_D_prima)
}




# Metodología B -----------------------------------------------------------
#' Title metodo_B: Función que genera un esqueleto con la metodología B, la
#' cual implementa la mezcla de normales por esqueleto. Se genera un modelo
#' inicial con k = 3. Valor elegido al ver el histograma de los datos
#' en un esqueleto. En el modelo final se utiliza la media del modelo inical
#' como parámetro.
#'
#' @param n_rep: Número de veces que se generarán los esqueletos para
#' obtener información.
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
#' @return lista_esq_D_prima: Lista que contiene 2 matrices, la matriz
#' "mat_esqueleto" con el esqueleto generado y la matriz "D_prima" la
#' cual contiene el primedio de los esqueletos generados "n_rep" veces.
#'
#' @examples
#' lista_esq_D_prima <- metodo_B(n_rep,param,param_sim)
#' 
metodo_B <- function(n_rep,param,param_sim){
  ptm <- proc.time()# Start the clock!
  ### Obtener D_prima
  D_prima_inicial <- gen_mat_demanda_alumnos(param,param_sim)#46.41 seg
  prom_D <- D_prima_inicial
  
  ##Generar esqueleto inicial
  #' Solicitudes "ocultas"
  mat_solicitudes <- gen_solicitudes(param)#8.07 seg
  mat_esqueleto <- gen_esqueleto(D_prima_inicial,mat_solicitudes,
                                 param)#13.35 seg
  
  ##Convertimos los datos para obtener la distribución por horas
  wait_mat_esqueleto <- 0
  Horas <- param$Horas
  for(h in 1:length(Horas)){
    # cat("\n h = ",h)
    suma_x_hra <- sum(mat_esqueleto[h,])
    # cat("\n suma_x_hra = ",suma_x_hra)
    if(suma_x_hra > 0){
      wait_mat_esqueleto <- c(wait_mat_esqueleto,rep(Horas[h],suma_x_hra))
    }
    # cat("\n wait_mat_esqueleto = ",wait_mat_esqueleto)
  }
  #Quitamos el cero inicial
  wait_mat_esqueleto <- wait_mat_esqueleto[-1]
  
  #' Definimos las listas en las que vamos a guardar el número de grupos por materia
  #' y los modelos de mezcla de normales para cada esqueleto
  mat_gpos_x_materia <- matrix(0,nrow = n_rep,ncol = dim(mat_esqueleto)[2])
  mat_gpos_x_materia[1,] <- colSums(mat_esqueleto)
  mixmdl_1_esqueleto <- normalmixEM(wait_mat_esqueleto,k = 4)
  
  # wait_mat_esqueleto_inicial <- wait_mat_esqueleto
  # hist(wait_mat_esqueleto_inicial,freq = F,
  #      breaks = seq(5,22,by = 1),
  #      main = "Histograma de grupos en un esqueleto",
  #      xlab = "Horas",ylab = "Frecuencia relativa",
  #      ylim = c(0,0.15))
  # lines(density(rnorm(1000,mean = mixmdl_1_esqueleto$mu,
  #                     sd = mixmdl_1_esqueleto$sigma)),
  #       lty=1,lwd=2,col = "blue")
  # lines(density(wait_mat_esqueleto_inicial), lty=1,lwd=2,col = "green")
  # legend(15,0.15,c("GMM","density()"),bty = "n",
  #        col=c("blue","green"),lty=c(1,1),
  #        cex=1.1,lwd=2)
  
  #Hacemos "n_rep" veces el proceso
  for(d in 2:n_rep){
    cat("d = ",d)
    ### Obtener D
    D <- gen_mat_demanda_alumnos(param,param_sim)
    prom_D <- prom_D + D
    
    ##Generar esqueleto
    mat_solicitudes <- gen_solicitudes(param)
    mat_esqueleto <- gen_esqueleto(D,mat_solicitudes,param)
    mat_gpos_x_materia[d,] <- colSums(mat_esqueleto)
    # lista_de_lista_info_esqueleto[[d]] <- lista_info_esqueleto
    
    ##Convertimos los datos para obtener la distribución por horas
    for(h in 1:length(Horas)){
      suma_x_hra <- sum(mat_esqueleto[h,])
      if(suma_x_hra > 0){
        wait_mat_esqueleto <- c(wait_mat_esqueleto,rep(Horas[h],suma_x_hra))
      }
    }
  }#Fin for(d)
  mixmdl_esqueleto <- normalmixEM(wait_mat_esqueleto,
                                  k=length(mixmdl_1_esqueleto$mu),
                                  mean=mixmdl_1_esqueleto$mu)
  prom_gpos_x_materia <- ceiling(colMeans(mat_gpos_x_materia))
  
  # wait_mat_esqueleto_final <- wait_mat_esqueleto
  # hist(wait_mat_esqueleto_final,freq = F,
  #      breaks = seq(5,22,by = 1),
  #      main = "Histograma de grupos en un esqueleto",
  #      xlab = "Horas",ylab = "Frecuencia relativa",
  #      ylim = c(0,0.15))
  # lines(density(rnorm(1000,mean = mixmdl_esqueleto$mu,
  #                     sd = mixmdl_esqueleto$sigma)),
  #       lty=1,lwd=2,col = "blue")
  # legend(5,0.17,
  #        c("Densidad ajustada por modelo de mezcla de normales"),
  #        bty = "n",
  #        col=c("blue"),lty=1,
  #        cex=1.1,lwd=2)
  # lines(density(wait_mat_esqueleto_final), lty=1,lwd=2,col = "green")
  # legend(15,0.15,c("GMM","density()"),bty = "n",
  #        col=c("blue","green"),lty=c(1,1),
  #        cex=1.1,lwd=2)
  
  #Generamos el esqueleto final
  mat_esqueleto_final <- matrix(0,nrow = length(param$Horas),
                                ncol = length(param$vec_nom_materias_total))
  for(c in 1:length(param$vec_nom_materias_total)){
    num_gpos_1_materia <- prom_gpos_x_materia[c]
    (rand_num <- sort(round(rnorm(num_gpos_1_materia,mixmdl_esqueleto$mu,
                                  mixmdl_esqueleto$sigma))))
    ind_7 <- which(rand_num < 8)
    ind_22 <- which(rand_num >= 21)
    
    if(length(ind_7) > 0){
      rand_num[ind_7] <- 7
    }
    if(length(ind_22) > 0){
      rand_num[ind_22] <- 21
    }
    
    for(r in 1:length(param$Horas)){
      ind_hrs <- which(rand_num == param$Horas[r])
      if(length(ind_hrs) > 0){
        mat_esqueleto_final[r,c] <- length(ind_hrs)
      }
    }
  }#Fin for(c)
  # View(mat_esqueleto_final)
  rownames(mat_esqueleto_final) <- param$Horas
  colnames(mat_esqueleto_final) <- param$vec_nom_materias_total
  
  lista_esq_D_prima <- list()
  lista_esq_D_prima[[1]] <- mat_esqueleto_final
  lista_esq_D_prima[[2]] <- ceiling(prom_D/n_rep)
  cat("\nLa función metodo_B tardó: ",(proc.time()-ptm)[3]/60," minutos\n")
  return(lista_esq_D_prima)
}



# Metodología C -----------------------------------------------------------

#' Title metodo_B2: Función que genera un esqueleto con la metodología B2, la
#' cual implementa la mezcla de normales por esqueleto. Se genera un modelo
#' inicial con k = 3. Valor elegido al ver el histograma de los datos
#' en un esqueleto. En el modelo final se utiliza la media del modelo inical
#' como parámetro.
#'
#' @param n_rep: Número de veces que se generarán los esqueletos para
#' obtener información.
#' @param D_prima_inicial: Matriz de 15 renglones (horas) y 203 columnas
#' (materias). En la entrada (i,j) se tiene el número de alumnos simulados
#' para la hora i, y la materia j.
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
#' @return lista_esq_D_prima: Lista que contiene 2 matrices, la matriz
#' "mat_esqueleto" con el esqueleto generado y la matriz "D_prima" la
#' cual contiene el primedio de los esqueletos generados "n_rep" veces.
#'
#' @examples
#' lista_esq_D_prima <- metodo_B2(n_rep,D_prima_inicial,param,param_sim)
#' 
metodo_B2 <- function(n_rep,D_prima_inicial,param,param_sim){
  ptm <- proc.time()# Start the clock!
  ##Generar esqueleto inicial
  prom_D <- D_prima_inicial
  mat_solicitudes <- gen_solicitudes(param)#8.07 seg
  lista_info_esqueleto <- gen_esqueleto(D_prima_inicial,mat_solicitudes,param)#13.35 seg
  mat_esqueleto <- lista_info_esqueleto[[1]]
  
  ##Convertimos los datos para obtener la distribución por horas
  wait_mat_esqueleto <- 0
  Horas <- param$Horas
  for(h in 1:length(Horas)){
    suma_x_hra <- sum(mat_esqueleto[h,])
    if(suma_x_hra > 0){
      wait_mat_esqueleto <- c(wait_mat_esqueleto,rep(Horas[h],suma_x_hra))
    }
  }
  #Quitamos el cero inicial
  wait_mat_esqueleto <- wait_mat_esqueleto[-1]
  
  #' Definimos las listas en las que vamos a guardar el número de grupos por materia
  #' y los modelos de mezcla de normales para cada esqueleto
  mat_gpos_x_materia <- matrix(0,nrow = n_rep,ncol = dim(mat_esqueleto)[2])
  mat_gpos_x_materia[1,] <- colSums(mat_esqueleto)
  mixmdl_1_esqueleto <- normalmixEM(wait_mat_esqueleto,k = 4)
  
  #Hacemos "n_rep" veces el proceso
  for(d in 2:n_rep){
    cat("d = ",d)
    ### Obtener D
    # D <- gen_mat_demanda_alumnos(param,param_sim)
    # prom_D <- prom_D + D
    ##Generar esqueleto
    mat_solicitudes <- gen_solicitudes(param)
    lista_info_esqueleto <- gen_esqueleto(D_prima_inicial,
                                          mat_solicitudes,param)
    mat_esqueleto <- lista_info_esqueleto[[1]]
    mat_gpos_x_materia[d,] <- colSums(mat_esqueleto)
    
    ##Convertimos los datos para obtener la distribución por horas
    for(h in 1:length(Horas)){
      suma_x_hra <- sum(mat_esqueleto[h,])
      if(suma_x_hra > 0){
        wait_mat_esqueleto <- c(wait_mat_esqueleto,rep(Horas[h],suma_x_hra))
      }
    }
  }#8.963333 min
  mixmdl_esqueleto <- normalmixEM(wait_mat_esqueleto,
                                  k = length(mixmdl_1_esqueleto$mu),
                                  mean=mixmdl_1_esqueleto$mu)
  
  prom_gpos_x_materia <- ceiling(colMeans(mat_gpos_x_materia))
  
  #Generamos el esqueleto final
  mat_esqueleto_final <- matrix(0,nrow = length(param$Horas),
                                ncol = length(param$vec_nom_materias_total))
  for(c in 1:length(param$vec_nom_materias_total)){
    num_gpos_1_materia <- prom_gpos_x_materia[c]
    (rand_num <- sort(round(rnorm(num_gpos_1_materia,mixmdl_esqueleto$mu,
                                  mixmdl_esqueleto$sigma))))
    ind_7 <- which(rand_num < 8)
    ind_22 <- which(rand_num >= 21)
    
    if(length(ind_7) > 0){
      rand_num[ind_7] <- 7
    }
    if(length(ind_22) > 0){
      rand_num[ind_22] <- 21
    }
    
    for(r in 1:length(param$Horas)){
      ind_hrs <- which(rand_num == param$Horas[r])
      if(length(ind_hrs) > 0){
        mat_esqueleto_final[r,c] <- length(ind_hrs)
      }
    }
  }#Fin for(c)
  # View(mat_esqueleto_final)
  rownames(mat_esqueleto_final) <- param$Horas
  colnames(mat_esqueleto_final) <- param$vec_nom_materias_total
  
  lista_esq_D_prima <- list()
  lista_esq_D_prima[[1]] <- mat_esqueleto_final
  lista_esq_D_prima[[2]] <- D_prima_inicial
  # lista_esq_D_prima[[2]] <- ceiling(prom_D/n_rep)
  cat("\nLa función metodo_B2 tardó: ",(proc.time()-ptm)[3]/60," minutos\n")
  return(lista_esq_D_prima)
}


#' Title metodo_C: Función que genera un esqueleto con la metodología C, la
#' cual implementa la mezcla de normales por número de alumnos y por
#' esqueleto. Para el número de alumnos se genera un modelo inicial con k = 4.
#' Valor elegido al ver el histograma de los datos del número de alumnos. Para
#' el esqueleto se aplica la metodología B.
#'
#' @param n_rep: Número de veces que se generarán los esqueletos para
#' obtener información.
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
#' @return lista_esq_D_prima: Lista que contiene 2 matrices, la matriz
#' "mat_esqueleto" con el esqueleto generado y la matriz "D_prima" la
#' cual contiene el primedio de los esqueletos generados "n_rep" veces.
#'
#' @examples
#' lista_esq_D_prima <- metodo_C(n_rep,param,param_sim)
#' 
metodo_C <- function(n_rep,param,param_sim){
  ptm <- proc.time()# Start the clock!
  ### Obtener D
  D_inicial <- gen_mat_demanda_alumnos(param,param_sim)#42.96 seg
  
  #' Definimos las listas en las que vamos a guardar el número de alumnos
  #' por materia
  num_alum_x_materia <- list()
  num_alum_x_materia[[1]] <- colSums(D_inicial)
  
  ##Convertimos los datos para obtener la distribución por horas
  wait_alumnos <- 0
  Horas <- param$Horas
  for(h in 1:length(Horas)){
    suma_x_hra <- sum(D[h,])
    if(suma_x_hra > 0){
      wait_alumnos <- c(wait_alumnos,rep(Horas[h],suma_x_hra))
    }
  }
  #Quitamos el cero inicial
  wait_alumnos <- wait_alumnos[-1]
  
  #' Definimos la lista en las que vamos a guardar el número de alumnos
  #' por materia
  num_alum_x_materia <- list()
  num_alum_x_materia[[1]] <- colSums(D)
  mixmdl_1_D <- normalmixEM(wait_alumnos,k = 4)#Modelo inicial
  
  # hist(wait_alumnos,freq = F,breaks = seq(6,22,by = 1),)
  # lines(density(rnorm(1000,mean = mixmdl_1_D$mu,sd = mixmdl_1_D$sigma)),
  #       lty=1,lwd=2,col = "blue")
  # lines(density(wait_alumnos), lty=1,lwd=2,col = "green")
  # legend(15,0.14,c("GMM","density()"),bty = "n",
  #        col=c("blue","green"),lty=c(1,1),
  #        cex=1.1,lwd=2)
  
  #Hacemos "n_rep" veces el proceso
  for(d in 2:n_rep){
    cat("d = ",d)
    ### Obtener D
    D <- gen_mat_demanda_alumnos(param,param_sim)
    num_alum_x_materia[[d]] <- colSums(D)
    ##Convertimos los datos para obtener la distribución por horas
    for(h in 1:length(Horas)){
      suma_x_hra <- sum(D[h,])
      if(suma_x_hra > 0){
        wait_alumnos <- c(wait_alumnos,rep(Horas[h],suma_x_hra))
      }
    }
  }#2.7805 min
  
  ### Obtenemos el número promedio de grupos por materia
  mat_alum_x_materia <- matrix(0,nrow = n_rep,
                               ncol = length(param$vec_nom_materias_total))
  
  for(r in 1:n_rep){#Recorre las listas
    mat_alum_x_materia[r,] <- num_alum_x_materia[[r]]
  }
  prom_alum_x_materia <- ceiling(colMeans(mat_alum_x_materia))
  
  #Generamos la matriz D final
  D_final <- matrix(0,nrow = length(param$Horas),
                    ncol = length(param$vec_nom_materias_total))
  mixmdl_D <- normalmixEM(wait_alumnos,mixmdl_1_D$mu)#Modelo final
  
  # wait_alumnos_final <- wait_alumnos
  # hist(wait_alumnos_final,freq = F,breaks = seq(6,22,by = 1),)
  # lines(density(rnorm(1000,mean = mixmdl_D$mu,sd = mixmdl_D$sigma)),
  #       lty=1,lwd=2,col = "blue")
  # lines(density(wait_alumnos_final), lty=1,lwd=2,col = "green")
  # legend(15,0.12,c("GMM","density()"),bty = "n",
  #        col=c("blue","green"),lty=c(1,1),
  #        cex=1.1,lwd=2)
  
  for(c in 1:length(param$vec_nom_materias_total)){
    num_alum_1_materia <- prom_alum_x_materia[c]
    (rand_num <- sort(round(rnorm(num_alum_1_materia,mixmdl_D$mu,mixmdl_D$sigma))))
    ind_7 <- which(rand_num < 8)
    ind_22 <- which(rand_num >= 21)
    
    if(length(ind_7) > 0){
      rand_num[ind_7] <- 7
    }
    if(length(ind_22) > 0){
      rand_num[ind_22] <- 21
    }
    
    for(r in 1:length(param$Horas)){
      ind_hrs <- which(rand_num == param$Horas[r])
      if(length(ind_hrs) > 0){
        D_final[r,c] <- length(ind_hrs)
      }
    }
  }#Fin for(c)
  rownames(D_final) <- param$Horas
  colnames(D_final) <- param$vec_nom_materias_total
  
  ### Metodología B
  lista_esq_D_prima <- metodo_B2(n_rep,D_final,param,param_sim)
  
  cat("\nLa función metodo_C tardó: ",(proc.time()-ptm)[3]/60," minutos\n")
  return(lista_esq_D_prima)
}


# Metodología D -----------------------------------------------------------

#' Title actualiza_D_prima_metodo_D: Función encargada de actualizar
#' D_prima para la metodología D.
#'
#' @param cota: Cota para que el ciclo no sea infinito.
#' @param D: Matriz mat_demanda_alumnos, de 15 renglones (horas) y 203
#' columnas (materias). En la entrada (i,j) se tiene el número de alumnos
#' simulados para la hora i, y la materia j.
#' @param D_prima: Matriz de 15 renglones (horas) y 203 columnas (materias).
#' En la entrada (i,j) se tiene el número de alumnos simulados para la hora
#' i, y la materia j. 
#' @param mixmdl: Lista con "m" elementos. Cada elemento es el modelo de
#' mezcla de Normales para una materia.
#' @param calif_D: Lista con 2 elementos: "mat_calif_x_gpo" y
#' "vec_calif_x_materia". La matriz "mat_calif_x_gpo" (15*203) contiene las
#' calificaciones por grupo. El vector "vec_calif_x_materia"
#' @param ind_materias: Vector con los índices de las materias que deben
#' de modificarse.
#'
#' @return D_prima: Matriz de 15x203 actualizada. En la entrada (i,j) se
#' tiene el nuevo número de alumnos simulados para la hora i, y la materia j.
#'
#' @examples
#' actualiza_D_prima_metodo_D(500,D,D_prima,mixmdl,calif_D,c(5,182))
#' actualiza_D_prima_metodo_D(cota,D,D_prima,mixmdl,calif_D,ind_materias)
#' 
actualiza_D_prima_metodo_D <- function(cota,D,D_prima,mixmdl,calif_D,ind_materias){
  #' Para este punto ya comparamos D y D_prima. Se redefine D_prima.
  #' Recibe a D_prima como parámetro para que en caso de que no haya
  #' modificaciones, se regrese la misma matriz y no una llena de ceros.
  
  mat_calif_x_gpo <- calif_D[[1]]
  vec_calif_x_materia <- calif_D[[2]]
  for(c in ind_materias){#Recorre columnas
    cont_1 <- 1
    cont_2 <- 1
    if(sum(vec_calif_x_materia[c])>1 || 
       sum(vec_calif_x_materia[c]) < -1){#Sólo modificamos si
      #' la califición total de la materia está fuera de [-20,10]
      for(h in 1:length(param$Horas)){#Recorre las horas (renglones)
        # cat("\n h = ",h)
        (rand_num <- ceiling(rnorm(1,mixmdl$mu,mixmdl$sigma)))
        if(mat_calif_x_gpo[h,c] > 0.5){#Si faltan alumnos
          while(rand_num <= D[h,c]){
            (rand_num <- ceiling(rnorm(1,mixmdl$mu,mixmdl$sigma)))
            cont_1 <- cont_1 + 1#Para no tener ciclo infinito
            if(cont_1 >= cota){
              break;
            }
          }
          cont_1 <- 1#Reiniciamos el contador
          D_prima[h,c] <- max(0,rand_num)
        }
        if(mat_calif_x_gpo[h,c] < -0.5 && D[h,c]>0){#Si sobran alumnos
          #'La 2° cond. es para que no haya simulación si no hay alumnos en D
          #'Aquí la calificación debe ser menor a -10 porque es por
          #'grupo no por materia (ver gráficas de diferencias relativas
          #'entre D y E)
          while(rand_num > D[h,c]){
            (rand_num <- ceiling(rnorm(1,mixmdl$mu,mixmdl$sigma)))
            cont_2 <- cont_2 + 1#Para no tener ciclo infinito
            if(cont_2 >= cota){
              break;
            }
          }
          cont_2 <- 1#Reiniciamos el contador
          D_prima[h,c] <- max(0,rand_num)
        }
      }#Fin for(h)
    }#Fin if(calificación)
  }#Fin for(c)
  return(D_prima)
}

#' Title metodo_C2: Función que arroja la matriz "D_final" a la cual se le
#' aplicó la mezcla de normales por número de alumnos. Para el número de 
#' alumnos se genera un modelo inicial con k = 4.
#' Valor elegido al ver el histograma de los datos del número de alumnos. Para
#' el esqueleto se aplica la metodología B.
#'
#' @param n_rep: Número de veces que se generarán los esqueletos para
#' obtener información.
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
#' @return D_final: Matriz de 15 renglones (horas) y 203 columnas
#' (materias). En la entrada (i,j) se tiene el número de alumnos simulados
#' para la hora i, y la materia j.
#'
#' @examples
#' D_final <- metodo_C2(n_rep,param,param_sim)
#' 
metodo_C2 <- function(n_rep,param,param_sim){
  ptm <- proc.time()# Start the clock!
  ### Obtener D0 y D_inicial
  D0 <- gen_mat_demanda_alumnos(param,param_sim)#42.96 seg
  D_inicial <- gen_mat_demanda_alumnos(param,param_sim)#42.96 seg
  ind_materias <- 1:dim(D0)[2]
  cota <- 100*n_rep
  
  #' Definimos las listas en las que vamos a guardar el número de alumnos
  #' por materia
  num_alum_x_materia <- list()
  num_alum_x_materia[[1]] <- colSums(D_inicial)
  
  ##Convertimos los datos para obtener la distribución por horas
  wait_alumnos <- 0
  Horas <- param$Horas
  for(h in 1:length(Horas)){
    suma_x_hra <- sum(D[h,])
    if(suma_x_hra > 0){
      wait_alumnos <- c(wait_alumnos,rep(Horas[h],suma_x_hra))
    }
  }
  #Quitamos el cero inicial
  wait_alumnos <- wait_alumnos[-1]
  
  #' Definimos la lista en las que vamos a guardar el número de alumnos
  #' por materia
  num_alum_x_materia <- list()
  num_alum_x_materia[[1]] <- colSums(D)
  mixmdl_1_D <- normalmixEM(wait_alumnos,k = 4)#Modelo inicial
  
  #Hacemos "n_rep" veces el proceso
  for(d in 2:n_rep){
    cat("d = ",d)
    cont <- 1
    ### Obtener D
    D <- gen_mat_demanda_alumnos(param,param_sim)
    ### Obtener D' para generar esqueleto
    mat_calif_x_gpo <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])
    calif_D <- actualiza_calif_D(D0,D,mat_calif_x_gpo,ind_materias)
    mat_calif_x_gpo <- calif_D[[1]]
    vec_calif_x_materia <- calif_D[[2]]
    #' Actualizo mientras se cumplan las siguientes condiciones:
    while(any(vec_calif_x_materia< -20) || any(vec_calif_x_materia> 10)){
      ind_1 <- which(vec_calif_x_materia< -20)
      ind_2 <- which(vec_calif_x_materia> 10)
      ind_materias <- union(ind_1,ind_2)
      if(length(ind_materias) > 0){
        D <- actualiza_D_prima_metodo_D(cota,D0,D,mixmdl_1_D,
                                        calif_D,ind_materias)
        calif_D <- actualiza_calif_D(D0,D,mat_calif_x_gpo,
                                     ind_materias)
        mat_calif_x_gpo <- calif_D[[1]]
        vec_calif_x_materia <- calif_D[[2]]
      }else{
        cat("\n Todas las calificaciones están dentro del intervalo [-20,10]")
        break;
      }
      if(cont >= cota){
        break;
      }
      cont <- cont + 1
    }#Fin while()
    
    num_alum_x_materia[[d]] <- colSums(D)
    ##Convertimos los datos para obtener la distribución por horas
    for(h in 1:length(Horas)){
      suma_x_hra <- sum(D[h,])
      if(suma_x_hra > 0){
        wait_alumnos <- c(wait_alumnos,rep(Horas[h],suma_x_hra))
      }
    }
  }#2.7805 min
  
  ### Obtenemos el número promedio de grupos por materia
  mat_alum_x_materia <- matrix(0,nrow = n_rep,
                               ncol = length(param$vec_nom_materias_total))
  
  for(r in 1:n_rep){#Recorre las listas
    mat_alum_x_materia[r,] <- num_alum_x_materia[[r]]
  }
  prom_alum_x_materia <- ceiling(colMeans(mat_alum_x_materia))
  
  #Generamos la matriz D final
  D_final <- matrix(0,nrow = length(param$Horas),
                    ncol = length(param$vec_nom_materias_total))
  mixmdl_D <- normalmixEM(wait_alumnos,mixmdl_1_D$mu)#Modelo final
  
  for(c in 1:length(param$vec_nom_materias_total)){
    num_alum_1_materia <- prom_alum_x_materia[c]
    (rand_num <- sort(round(rnorm(num_alum_1_materia,mixmdl_D$mu,mixmdl_D$sigma))))
    ind_7 <- which(rand_num < 8)
    ind_22 <- which(rand_num >= 21)
    
    if(length(ind_7) > 0){
      rand_num[ind_7] <- 7
    }
    if(length(ind_22) > 0){
      rand_num[ind_22] <- 21
    }
    
    for(r in 1:length(param$Horas)){
      ind_hrs <- which(rand_num == param$Horas[r])
      if(length(ind_hrs) > 0){
        D_final[r,c] <- length(ind_hrs)
      }
    }
  }#Fin for(c)
  rownames(D_final) <- param$Horas
  colnames(D_final) <- param$vec_nom_materias_total
  
  cat("\nLa función metodo_C2 tardó: ",(proc.time()-ptm)[3]/60," minutos\n")
  return(D_final)
}



#' Title metodo_D: Función que genera un esqueleto con la metodología D, la
#' cual implementa la mezcla de normales por número de alumnos. Se combina
#' el inicio de la metodología C con la función "gen_esqueleto".
#'
#' @param n_rep: Número de veces que se generarán los esqueletos para
#' obtener información.
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
#' @return lista_esq_D_prima: Lista que contiene 2 matrices, la matriz
#' "mat_esqueleto" con el esqueleto generado y la matriz "D_prima" la
#' cual contiene el primedio de los esqueletos generados "n_rep" veces.
#'
#' @examples
#' lista_esq_D_prima <- metodo_D(n_rep,param,param_sim)
#' 
metodo_D <- function(n_rep,param,param_sim){
  ptm <- proc.time()# Start the clock!
  #Definimos D_inicial
  D_inicial <- metodo_C2(n_rep,param,param_sim)
  
  ##Generar esqueleto
  mat_solicitudes <- gen_solicitudes(param)#7.97 seg
  lista_info_esqueleto <- gen_esqueleto(D_inicial,mat_solicitudes,param)#10.76 seg
  mat_esqueleto <- lista_info_esqueleto[[1]]
  
  lista_esq_D_prima <- list()
  lista_esq_D_prima[[1]] <- mat_esqueleto
  lista_esq_D_prima[[2]] <- D_inicial
  
  cat("\nLa función metodo_D tardó: ",(proc.time()-ptm)[3]/60," minutos\n")
  return(lista_esq_D_prima)
}


##### ASIGNACIÓN #####
#' Función que que genera una matriz con las asignaciones de Materia-
#' Profesor-Horario.
#' 

# cuenta_asignaciones -----------------------------------------------------
#' Title cuenta_asignaciones: Función encargada de contar el número de
#' asignaciones que tiene cada profesor.
#'
#' @param mat_aux_solicitud: Matriz de 6 columnas (Profesor,TC,Materia,
#' Num_Materia,Horario,Num_Asig) que tiene la información de la solicitud
#' de los profesores y el número de asignaciones que tiene cada profesor.
#' @param mat_asig: Matriz de 4 columnas (Materia, Profesor,TC,Horario) la
#' cual contiene en el i-ésimo renglón la asignación por materia, profesor
#' y horario. La columna TC indica si el profesor es o no de tiempo completo.
#'
#' @return mat_aux_solicitud: Matriz de 6 columnas (Profesor,TC,Materia,
#' Num_Materia,Horario,Num_Asig) que tiene la información de la solicitud
#' de los profesores y el número de asignaciones que tiene cada profesor.
#'
#' @examples
#' mat_aux_solicitud <- cuenta_asignaciones(mat_aux_solicitud,mat_asig)
#' 
cuenta_asignaciones <- function(mat_aux_solicitud,mat_asig){
  for(d in 1:dim(mat_asig)[1]){
    #Sumamos a las solicitudes del profesor
    (ind_prof <- which(mat_aux_solicitud[,1] == mat_asig[d,2]))
    mat_aux_solicitud[ind_prof,6] <- as.numeric(mat_aux_solicitud[ind_prof,
                                                                  6]) + 1
    #Sumamos para no repetir materia ni hora
    (ind_hora <- which(mat_aux_solicitud[ind_prof,5] == mat_asig[d,4]))
    (ind_materia <- which(mat_aux_solicitud[ind_prof,3] == mat_asig[d,1]))
    ind <- union(ind_hora,ind_materia)
    mat_aux_solicitud[ind_prof[ind],6] <- mat_aux_solicitud[ind_prof[ind],
                                                            6] + 11
  }
  
  return(mat_aux_solicitud)
}


# gen_asignacion ----------------------------------------------------------
#' Title gen_asignacion: Función que genera asignaciones de materia con
#' profesor por hora, dependiendo del número de grupos simulados para el
#' siguiente semestre. Tenemos de información "mat_esqueleto" y
#' "mat_solicitudes_real".
#'
#' @param mat_esqueleto: Matriz de 15 renglones con las horas (7-8,8-9,...,
#' 21-22) y tantas columnas como materias impartidas en el semestre actual.
#' @param mat_solicitudes_real: Matriz de 5 columnas (Profesor,TC,Materia,
#' Num_Materia,Horario) que tiene la información de la solicitud de los
#' profesores. Se hace una "intersección" con los grupos simulados en la
#' matriz "mat_esqueleto" y así se obtienen las solicitudes pseudo-reales
#' de los profesores.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return lista_asignacion: Lista de 3 elementos:
#' 1) mat_asignacion: Matriz de cuatro columnas (Materia, Profesor,
#' TC,Horario) la cual contiene en el i-ésimo renglón la asignación
#' por materia, profesor y horario. La columna TC indica si el profesor
#' es o no de tiempo completo.
#' 2) m_esq_aux: Matriz con el número de grupos que quedaron sin asignación.
#' 3) m_sol_aux: Matriz de solicitudes reales con el número de materias
#' asignadas por profesor. (NO SE UTILIZA, SE PUEDE BORRAR DE LA LISTA)
#' 
#' @examples
#' lista_asignacion <- gen_asignacion(mat_esqueleto,mat_solicitudes_real,
#' param)
#'
gen_asignacion <- function(mat_esqueleto,mat_solicitudes_real,param){
  ptm <- proc.time()# Start the clock!
  mat_asignacion <- data.frame(Materia = 0, Profesor = 0,TC = 0,
                               Horario = 0)
  Materias <- param$vec_nom_materias_total
  Num_Asig <- rep(0,dim(mat_solicitudes_real)[1])
  m_sol_aux <- cbind(mat_solicitudes_real,Num_Asig)#Con el # de asig. de cada prof.
  m_esq_aux <- mat_esqueleto
  
  for(m in 1:length(Materias)){
    materia <- Materias[m]
    cat("\nMateria ",m,": ",materia)
    (vec_aux_esq <- mat_esqueleto[,m])
    for(h in 1:length(param$Horas)){
      mat_aux_solicitud <- m_sol_aux %>% filter(Materia == materia)
      (num_gpos <- vec_aux_esq[h])
      if(num_gpos > 0){#Si el # de gpos. simulados > 0
        #' La matriz "m_aux" tiene las solicitudes de los profesores
        #' que aún no pasan el número máximo de asignaciones.
        m_aux <- mat_aux_solicitud%>% filter(
          as.numeric(Num_Asig)<param$num_max_asig) %>% filter(
            Horario==param$Horas[h])
        
        if(dim(m_aux)[1] > num_gpos){
          #'En caso de que se tengan más grupos de los simulados en
          #'el esqueleto
          mat_asig <- m_aux[sample(1:dim(m_aux)[1],size = num_gpos),
                            c(3,1,2,5)]
          mat_asignacion <- rbind(mat_asignacion,mat_asig)
          m_esq_aux[h,m] <- m_esq_aux[h,m] - num_gpos
        }else{
          mat_asig <- m_aux[,c(3,1,2,5)]
          mat_asignacion <- rbind(mat_asignacion,mat_asig)
          m_esq_aux[h,m] <- m_esq_aux[h,m] - dim(m_aux)[1]
        }
        m_sol_aux <- cuenta_asignaciones(m_sol_aux,mat_asig)
      }#Fin if(num_gpos>0)
    }#Fin for(h)
  }
  mat_asignacion <- mat_asignacion %>% filter(Materia != 0)
  # View(mat_asignacion)
  
  cat("\nLa matriz mat_esqueleto tiene: ", sum(mat_esqueleto)," grupos" )
  cat("\nSe generaron: ", sum(mat_esqueleto) - sum(m_esq_aux),
      " grupos en la asignación" )
  cat("\nLa función gen_asignacion tomó: ", (proc.time()-ptm)[3],
      " segundos\n\n\n" )
  lista_asignacion <- list()
  lista_asignacion[[1]] <- mat_asignacion
  lista_asignacion[[2]] <- m_esq_aux
  lista_asignacion[[3]] <- m_sol_aux
  return(lista_asignacion)
}


# califica_asignacion ------------------------------------------------------
#' Title califica_asignacion: Función que califica un asignacion, por grupo
#' y de manera global. Las penalizaciones globales son:
#' - Penalización por grupos faltantes: Se resta de acuerdo a la diferencia
#' relativa por grupo.
#' - Si algún profesor de tiempo completo pidió alguna materia y
#' no se la dieron. Se penaliza con 10 por cada solicitud.
#' Nota:
#' Se penaliza por cada materia con tope a "num_max_asig",
#' Ej. si num_max_asig = 2 y un profesor pidió 3 o más  materias
#' pero sólo le dieron 1, entonces se penaliza 1; si le dieron 2
#' no hay penalización.
#' 
#' Las penalizaciones por grupo son:
#' - Se pone un +5 si el profesor asignado es de TC.
#' - Se pone un -1 por cada asignación que pudo haber tenido un
#' profesor de TC y tiene un profesor de asignatura.
#' - Para tener una calificación diferente para cada grupo, sumamos
#' a cada renglón una épsilon entre 0 y 0.1.
#'
#' @param mat_esqueleto: Matriz de 15 renglones (horas) y 203 columnas
#' (materias). En la entrada (i,j) se tiene el número de grupos simulados
#' para la hora i, y la materia j.
#' @param mat_solicitudes_real: Matriz de 5 columnas (Profesor,TC,Materia,
#' Num_Materia,Horario) que tiene la información de la solicitud de los
#' profesores. Se hace una "intersección" con los grupos simulados en la
#' matriz "mat_esqueleto" y así se obtienen las solicitudes pseudo-reales
#' de los profesores.
#' @param lista_asignacion: Lista de 3 elementos:
#' 1) mat_asignacion: Matriz de cuatro columnas (Materia, Profesor,
#' TC,Horario) la cual contiene en el i-ésimo renglón la asignación
#' por materia, profesor y horario. La columna TC indica si el profesor
#' es o no de tiempo completo.
#' 2) m_esq_aux: Matriz con el número de grupos en cada hijo.
#' 3) m_sol_aux: Matriz de solicitudes reales con el número de materias
#' asignadas por profesor. (NO SE UTILIZA, SE PUEDE BORRAR DE LA LISTA)
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return lista_calif_asignacion: Lista con 2 elementos:
#' 1) mat_calif_asig_x_gpo: Matriz de 6 columnas (Materia,Profesor,TC,
#' Horario, calif, Prob_Ac), que contiene la información por grupo asignado.
#' 2) calif_asignacion: Variable tipo numeric que indica la calificación
#' global de la asignación.
#'
#' @examples
#' lista_calif_asignacion <- califica_asignacion(mat_esqueleto,
#' mat_solicitudes_real,lista_asignacion,param)
#' 
califica_asignacion <- function(mat_esqueleto,mat_solicitudes_real,
                                lista_asignacion,param){
  ptm <- proc.time()# Start the clock!
  #Se definen las variables que se van a utilizar
  mat_asignacion <- lista_asignacion[[1]]
  mat_esqueleto_aux <- lista_asignacion[[2]]
  mat_calif_asig_x_gpo <- data.frame(mat_asignacion,calif = 0, Prob_Ac = 0)
  
  #' Penalización por grupo en esqueleto sin profesor:
  #' Se resta de acuerdo a la diferencia relativa por grupo sin profesor.
  # mat_diferencia <- mat_esqueleto - mat_esqueleto_aux
  # (gpos_sin_prof <- sum(!is.nan(mat_diferencia/mat_esqueleto)))
  
  #' Penalización por grupos sobrantes o faltantes:
  #' Se resta de acuerdo a la diferencia relativa por grupo.
  mat_diferencia <- mat_esqueleto - mat_esqueleto_aux
  dif_relativas <- mat_diferencia/mat_esqueleto
  vec_dif_rel <- dif_relativas[!is.nan(dif_relativas)]
  (gpos_sobrantes <- sum(vec_dif_rel[vec_dif_rel<0]))
  (gpos_faltantes <- sum(vec_dif_rel[vec_dif_rel>0]))
  
  
  #' Si algún profesor de tiempo completo pidió alguna materia y
  #' no se la dieron. Se penaliza con -10 por cada materia.
  mat_info_prof <- data.frame(Profesor = param$mat_nom_prof_total[,1],
                              TC = param$mat_nom_prof_total[,2],
                              Materias_solicitadas = 0,
                              Materias_asignadas = 0)
  
  for(p in 1:length(param$mat_nom_prof_total[,1])){
    (prof <- mat_info_prof[p,1])
    solicitudes <- mat_solicitudes_real %>% filter(Profesor == prof)
    mat_info_prof[p,3] <- dim(solicitudes)[1]
    
    asignaciones <- mat_asignacion %>% filter(Profesor == prof)
    mat_info_prof[p,4] <- dim(asignaciones)[1]
  }
  
  #' Nota:
  #' Se penaliza por cada materia con tope a "num_max_asig",
  #' Ej. si num_max_asig = 2 y un profesor pidió 3 o más  materias
  #' pero sólo le dieron 1, entonces se penaliza 1; si le dieron 2
  #' no hay penalización.
  pena_x_solicitud_negada <- 0
  mat_prof_TC <- mat_info_prof %>% filter(TC == 1)
  
  for(r in 1:dim(mat_prof_TC)[1]){#Recorre los renglones
    if(mat_prof_TC[r,3]>0 && mat_prof_TC[r,4]<2){
      num_sols <- min(mat_prof_TC[r,3],param$num_max_asig)
      num_neg <- num_sols - mat_prof_TC[r,4]
      pena_x_solicitud_negada <- pena_x_solicitud_negada + (10*num_neg)
    }
  }
  pena_x_solicitud_negada
  
  
  ### CALIFICACIÓN POR GRUPO ###
  
  #' Se pone un +5 si el profesor asignado es de TC
  ind_TC <- which(mat_calif_asig_x_gpo[,3] == 1)
  mat_calif_asig_x_gpo[ind_TC,5] <- 5
  
  #' Se penaliza con -1 por cada asignación que pudo haber tenido
  #' un profesor de TC y tiene un profesor de asignatura.
  mat_prof_TC <- data.frame(mat_prof_TC,Materias_negadas = 0)
  
  for(r in 1:dim(mat_prof_TC)[1]){#Recorre los renglones
    (num_sols <- min(mat_prof_TC[r,3],param$num_max_asig))
    mat_prof_TC[r,5] <- num_sols - mat_prof_TC[r,4]
  }
  TC_falta_asig <- mat_prof_TC %>% filter(Materias_negadas > 0)
  materias_no_asignadas <- data.frame(Profesor = 0,TC = 0, Materia = 0,
                                      Num_Materia = 0,Horario = 0)
  
  for(r in 1:dim(TC_falta_asig)[1]){#Recorre renglones de "TC_falta_asig"
    indices <- which(mat_solicitudes_real[,1] == TC_falta_asig[r,1])
    materias_no_asignadas <- rbind(materias_no_asignadas,
                                   mat_solicitudes_real[indices,])
  }
  materias_no_asignadas <- materias_no_asignadas %>% filter(Profesor != 0)
  
  for(r in 1:dim(mat_calif_asig_x_gpo)[1]){#Recorre renglones de "mat_calif_asig_x_gpo"
    materia <- mat_calif_asig_x_gpo[r,1]
    hora <- mat_calif_asig_x_gpo[r,4]
    mat_aux <- materias_no_asignadas %>% filter(Materia == materia) %>% filter(
      Horario == hora)
    
    mat_calif_asig_x_gpo[r,5] <- mat_calif_asig_x_gpo[r,5] - dim(mat_aux)[1]
  }
  
  #' Para tener una calificación diferente para cada grupo, sumamos
  #' a cada renglón una épsilon:
  for(r in 1:dim(mat_calif_asig_x_gpo)[1]){#Recorre renglones de "mat_calif_asig_x_gpo"
    (num_al <- round(runif(1,0,0.1),4))#Se redondea a 4 decimales
    if(mat_calif_asig_x_gpo[r,5] >= 0){
      mat_calif_asig_x_gpo[r,5] <- mat_calif_asig_x_gpo[r,5] + num_al
    }else{
      mat_calif_asig_x_gpo[r,5] <- mat_calif_asig_x_gpo[r,5] - num_al
    }
  }
  mat_calif_asig_x_gpo <- mat_calif_asig_x_gpo[order(mat_calif_asig_x_gpo$calif),]
  
  #' Agregamos una columna con la probabilidad acumulada de elegir cada
  #' grupo.
  (n_gpos <- dim(mat_calif_asig_x_gpo)[1])
  mat_calif_asig_x_gpo[1,6] <- (2*1)/(n_gpos*(n_gpos+1))
  for(r in 2:dim(mat_calif_asig_x_gpo)[1]){#Recorre renglones de "mat_calif_asig_x_gpo"
    prob <- (2*r)/(n_gpos*(n_gpos+1))
    mat_calif_asig_x_gpo[r,6] <- mat_calif_asig_x_gpo[(r-1),6] + prob
  }
  
  (calif_asignacion <- gpos_sobrantes + mean(mat_calif_asig_x_gpo[,5])
    -sum(gpos_faltantes,pena_x_solicitud_negada))
  
  lista_calif_asignacion <- list()
  lista_calif_asignacion[[1]] <- mat_calif_asig_x_gpo
  lista_calif_asignacion[[2]] <- calif_asignacion
  
  cat("\nLa función califica_asignacion tardó: ",(proc.time()-ptm)[3],
      " segundos\n")
  return(lista_calif_asignacion)
}


# poblacion_calif_iniciales -----------------------------------------------
#' Title poblacion_calif_iniciales: Función que se encarga de definir las
#' matrices y listas iniciales para el Algoritmo Genético.
#'
#' @param mat_esqueleto: Matriz de 15 renglones con las horas (7-8,8-9,...,
#' 21-22) y tantas columnas como materias impartidas en el semestre actual.
#' @param mat_solicitudes_real: Matriz de 5 columnas (Profesor,TC,Materia,
#' Num_Materia,Horario) que tiene la información de la solicitud de los
#' profesores. Se hace una "intersección" con los grupos simulados en la
#' matriz "mat_esqueleto" y así se obtienen las solicitudes pseudo-reales
#' de los profesores.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return lista_info_inicial: Lista de 4 elementos:
#' 1) mat_calif_asig_ini: Matriz con las calificaciones de las asignaciones
#' 2) poblacion_inicial: Lista de matrices con asignación y calificaciones
#' 3) mat_calif_x_generacion: Matriz con calificaciones 1 generación
#'
#' @examples
#' lista_info_inicial <- poblacion_calif_iniciales(mat_esqueleto,
#' mat_solicitudes_real,param)
#' 
poblacion_calif_iniciales <- function(mat_esqueleto,mat_solicitudes_real,
                                      param){
  ptm <- proc.time()# Start the clock!
  #Se definen las variables que se van a utilizar
  tam_poblacion <- param$tam_poblacion
  n_cols_mat_calif <- param$n_cols_mat_calif
  mat_calif_x_generacion <- matrix(NaN,nrow = tam_poblacion,
                                   ncol = n_cols_mat_calif)
  poblacion_inicial <- list()
  calif_asignacion_inicial <- rbind(1:tam_poblacion,rep(0,tam_poblacion))
  nombres_mat_calif <- rep(0,tam_poblacion)
  
  for(n in 1:tam_poblacion){
    cat("\n n = ", n)
    #Generamos la población inicial
    lista_asignacion <- gen_asignacion(mat_esqueleto,mat_solicitudes_real,
                                       param)#22.66 seg
    mat_asignacion <- lista_asignacion[[1]]
    #Calificamos
    #' Penalización por grupo en esqueleto sin profesor:
    #' Se resta de acuerdo a la diferencia relativa por grupo sin profesor.
    #' lista_asignacion[[2]] tiene los grupos que no tienen profesor,
    #' se divide esa matriz entre mat_esqueleto y se suman los valores
    #' ditintos de "NaN".
    (gpos_sin_prof <- sum(!is.nan(lista_asignacion[[2]]/mat_esqueleto)))
    
    #' Si algún profesor de tiempo completo pidió alguna materia y
    #' no se la dieron. Se penaliza con -10 por cada materia.
    mat_info_prof <- data.frame(Profesor = param$mat_nom_prof_total[,1],
                                TC = param$mat_nom_prof_total[,2],
                                Materias_solicitadas = 0,
                                Materias_asignadas = 0)
    
    for(p in 1:length(param$mat_nom_prof_total[,1])){
      (prof <- mat_info_prof[p,1])
      solicitudes <- mat_solicitudes_real %>% filter(Profesor == prof)
      mat_info_prof[p,3] <- dim(solicitudes)[1]
      
      asignaciones <- mat_asignacion %>% filter(Profesor == prof)
      mat_info_prof[p,4] <- dim(asignaciones)[1]
    }
    
    #' Nota:
    #' Se penaliza por cada materia con tope a "num_max_asig",
    #' Ej. si num_max_asig = 2 y un profesor pidió 3 o más  materias
    #' pero sólo le dieron 1, entonces se penaliza 1; si le dieron 2
    #' no hay penalización.
    pena_x_solicitud_negada <- 0
    mat_prof_TC <- mat_info_prof %>% filter(TC == 1)
    
    for(r in 1:dim(mat_prof_TC)[1]){#Recorre los renglones
      if(mat_prof_TC[r,3]>0 && mat_prof_TC[r,4]<2){
        num_sols <- min(mat_prof_TC[r,3],param$num_max_asig)
        num_neg <- num_sols - mat_prof_TC[r,4]
        pena_x_solicitud_negada <- pena_x_solicitud_negada + (10*num_neg)
      }
    }
    pena_x_solicitud_negada##680
    
    
    ### CALIFICACIÓN POR GRUPO ###
    mat_calif_asig_x_gpo <- data.frame(mat_asignacion,calif = 0, Prob_Ac = 0)
    #' Se pone un +5 si el profesor asignado es de TC
    ind_TC <- which(mat_calif_asig_x_gpo[,3] == 1)
    mat_calif_asig_x_gpo[ind_TC,5] <- 5
    
    #' Se penaliza con -1 por cada asignación que pudo haber tenido
    #' un profesor de TC y tiene un profesor de asignatura.
    mat_prof_TC <- data.frame(mat_prof_TC,Materias_negadas = 0)
    
    for(r in 1:dim(mat_prof_TC)[1]){#Recorre los renglones
      (num_sols <- min(mat_prof_TC[r,3],param$num_max_asig))
      mat_prof_TC[r,5] <- num_sols - mat_prof_TC[r,4]
    }
    TC_falta_asig <- mat_prof_TC %>% filter(Materias_negadas > 0)
    materias_no_asignadas <- data.frame(Profesor = 0,TC = 0, Materia = 0,
                                        Num_Materia = 0,Horario = 0)
    
    for(r in 1:dim(TC_falta_asig)[1]){#Recorre renglones de "TC_falta_asig"
      indices <- which(mat_solicitudes_real[,1] == TC_falta_asig[r,1])
      materias_no_asignadas <- rbind(materias_no_asignadas,
                                     mat_solicitudes_real[indices,])
    }
    materias_no_asignadas <- materias_no_asignadas %>% filter(Profesor != 0)
    
    for(r in 1:dim(mat_calif_asig_x_gpo)[1]){#Recorre renglones de "mat_calif_asig_x_gpo"
      materia <- mat_calif_asig_x_gpo[r,1]
      hora <- mat_calif_asig_x_gpo[r,4]
      mat_aux <- materias_no_asignadas %>% filter(Materia == materia) %>% filter(
        Horario == hora)
      
      mat_calif_asig_x_gpo[r,5] <- mat_calif_asig_x_gpo[r,5] - dim(mat_aux)[1]
    }
    
    #' Para tener una calificación diferente para cada grupo, sumamos
    #' a cada renglón una épsilon:
    for(r in 1:dim(mat_calif_asig_x_gpo)[1]){#Recorre renglones de "mat_calif_asig_x_gpo"
      (num_al <- round(runif(1,0,0.1),4))#Se redondea a 4 decimales
      if(mat_calif_asig_x_gpo[r,5] >= 0){
        mat_calif_asig_x_gpo[r,5] <- mat_calif_asig_x_gpo[r,5] + num_al
      }else{
        mat_calif_asig_x_gpo[r,5] <- mat_calif_asig_x_gpo[r,5] - num_al
      }
    }
    mat_calif_asig_x_gpo <- mat_calif_asig_x_gpo[order(mat_calif_asig_x_gpo$calif),]
    
    #' Agregamos una columna con la probabilidad acumulada de elegir cada
    #' grupo.
    (n_gpos <- dim(mat_calif_asig_x_gpo)[1])
    mat_calif_asig_x_gpo[1,6] <- (2*1)/(n_gpos*(n_gpos+1))
    for(r in 2:dim(mat_calif_asig_x_gpo)[1]){#Recorre renglones de "mat_calif_asig_x_gpo"
      prob <- (2*r)/(n_gpos*(n_gpos+1))
      mat_calif_asig_x_gpo[r,6] <- mat_calif_asig_x_gpo[(r-1),6] + prob
    }
    
    (calif_asignacion <- mean(mat_calif_asig_x_gpo[,5])-
        sum(gpos_sin_prof,pena_x_solicitud_negada))
    
    poblacion_inicial[[n]] <- mat_calif_asig_x_gpo
    calif_asignacion_inicial[2,n] <- calif_asignacion
    mat_calif_x_generacion[n,1:length(
      mat_calif_asig_x_gpo[,5])] <- mat_calif_asig_x_gpo[,5]
  }
  
  ## 3) Ordenar de menor a mayor y definir la probabilidad acumulada
  calif_asignacion_inicial <- calif_asignacion_inicial[,order(
    calif_asignacion_inicial[2,])]
  mat_calif_asig_ini <- data.frame(ind_Asig = calif_asignacion_inicial[1,],
                                   Calif = calif_asignacion_inicial[2,])
  
  lista_info_inicial <- list()
  lista_info_inicial[[1]] <- mat_calif_asig_ini#Matriz con las calificaciones de las asignaciones
  lista_info_inicial[[2]] <- poblacion_inicial#Lista de matrices con asignación y calificaciones
  lista_info_inicial[[3]] <- mat_calif_x_generacion#Matriz con calificaciones 1 generación
  
  cat("\nLa función poblacion_calif_iniciales tardó: ",
      (proc.time()-ptm)[3]/60," minutos\n")
  return(lista_info_inicial)
}





# elige_gen_de_solicitud --------------------------------------------------
#' Title elige_gen_de_solicitud: Función que elige un gen de la matriz de
#' solicitudes cuando el gen tiene una mutación.
#'
#' @param mat_solicitudes_real: Matriz de 5 columnas (Profesor,TC,Materia,
#' Num_Materia,Horario) que tiene la información de la solicitud de los
#' profesores. Se hace una "intersección" con los grupos simulados en la
#' matriz "mat_esqueleto" y así se obtienen las solicitudes pseudo-reales
#' de los profesores.
#' @param hijo: Asignación que se crea a partir de 2 padres.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return gen_elegido: Vector de 4 entradas (Materia,Profesor,TC,Horario)
#' con la información del gen del padre elegido.
#'
#' @examples
#' gen_elegido <- elige_gen_de_solicitud(mat_solicitudes_real,hijo,param)
#' 
elige_gen_de_solicitud <- function(mat_solicitudes_real,hijo,param){
  #Se definen las variables que se van a utilizar
  problema <- 0
  mat_prof_TC <- mat_solicitudes_real %>% filter(TC == 1)
  mat_prof_asig <- mat_solicitudes_real %>% filter(TC == 0)
  
  #Se elige con mayor probabilidad a los profesores de TC
  (r_num_TC <- runif(1))
  if(r_num_TC <= param$elige_TC && dim(mat_prof_TC)[1] > 0){
    #' La 2° condición sirve porque se van eliminando los grupos
    #' de la matriz de solicitudes.
    mat_solicitudes <- mat_prof_TC
  }else{
    mat_solicitudes <- mat_prof_asig
  }
  
  (r_num_gen <- sample(1:dim(mat_solicitudes)[1],size = 1))
  (gen_elegido <- mat_solicitudes[r_num_gen,c(3,1,2,5)])
  (prof <- as.character(gen_elegido[2]))
  mat_aux <- hijo %>% filter(Profesor == prof)
  
  if(dim(mat_aux)[1] >= param$num_max_asig){
    #' En caso de que se tengan "num_max_asig" materias asignadas, en el
    #' hijo, al profesor del gen elegido. Cabe aclarar que a lo más se
    #' pueden asignar "num_max_asig" materias.
    problema <- 1
  }
  
  while(problema == 1){
    (r_num_gen <- sample(1:dim(mat_solicitudes)[1],size = 1))
    (gen_elegido <- mat_solicitudes[r_num_gen,c(3,1,2,5)])
    prof <- as.character(gen_elegido[2])
    mat_aux <- hijo %>% filter(Profesor == prof)
    
    if(dim(mat_aux)[1] < param$num_max_asig){
      problema <- 0
    }
  }#Fin while()
  
  return(gen_elegido)
}



# ajusta_genes_padres -----------------------------------------------------
#' Title ajusta_genes_padres: Función que se encarga de quitar la
#' información en los padres, del profesor en "gen_elegido" a esa hora y
#' con esa materia. Se tiene una cota para que el número de grupos del hijo
#' no supere el número de grupos de mat_esqueleto.
#'
#' @param esq_hijo: Matriz de 15 renglones (horas) y 203 columnas
#' (materias). En la entrada (i,j) se tiene el número de grupos del hijo
#' para la hora i, y la materia j. Esqueleto del hijo.
#' @param padre_1: Asignación elegida para crear un hijo.
#' @param padre_2: Asignación elegida para crear un hijo.
#' @param gen_elegido: Vector de 4 entradas (Materia,Profesor,TC,Horario)
#' con la información del gen del padre elegido.
#' @param mat_esqueleto: Matriz de 15 renglones (horas) y 203 columnas
#' (materias). En la entrada (i,j) se tiene el número de grupos simulados
#' para la hora i, y la materia j. Es diferente a "mat_esqueleto" porque
#' no se puede tener información privilegiada para la creación antes de la
#' calificación.
#'
#' @return lista_padres: Lista con los 2 padres actualizados.
#'
#' @examples
#' lista_padres <- ajusta_genes_padres(esq_hijo,padre_1,padre_2,
#' gen_elegido,mat_esqueleto)
#' 
ajusta_genes_padres <- function(esq_hijo,padre_1,padre_2,gen_elegido,
                                mat_esqueleto){
  cat("\n Se eligió el gen: \n",as.character(gen_elegido))
  # cat(paste("Se eligió el gen:",as.character(gen_elegido[1]),as.character(gen_elegido[2]),
  #           as.character(gen_elegido[3]),as.character(gen_elegido[4])),
  #     file="outfile.txt",sep="\n",append=TRUE)
  
  cat("\n El padre 1 tiene ",dim(padre_1)[1]," genes. \n El padre 2 tiene ",
      dim(padre_2)[1]," genes")
  # cat(paste("El padre 1 tiene ",dim(padre_1)[1]," genes."),
  #     file="outfile.txt",sep="\n",append=TRUE)
  # cat(paste("El padre 2 tiene ",dim(padre_2)[1]," genes."),
  #     file="outfile.txt",sep="\n",append=TRUE)
  
  (num_materia_gen <- arroja_num_materia(as.character(gen_elegido[1]),
                                         param))
  (ind_hora_gen <- which(7:21 == as.numeric(gen_elegido[4])))
  ind_elim_1 <- numeric(0)
  ind_elim_2 <- numeric(0)
  if(esq_hijo[ind_hora_gen,num_materia_gen] >= mat_esqueleto[ind_hora_gen,
                                                             num_materia_gen]){
    cat("\nEl hijo tiene ",esq_hijo[ind_hora_gen,num_materia_gen],
        " grupos. \nEl esqueleto tiene ",
        mat_esqueleto[ind_hora_gen,num_materia_gen],"grupos.")
    # cat(paste("El hijo tiene ",esq_hijo[ind_hora_gen,num_materia_gen],
    #           " grupos."),file="outfile.txt",sep="\n",append=TRUE)
    # cat(paste("El esqueleto tiene ",mat_esqueleto[ind_hora_gen,num_materia_gen],
    #           " grupos."),file="outfile.txt",sep="\n",append=TRUE)
    #' Índices de cada padre con la materia del gen elegido
    (ind_elim_1 <- which(padre_1[,1] == as.character(gen_elegido[1])))
    (ind_elim_2 <- which(padre_2[,1] == as.character(gen_elegido[1])))
  }
  
  #' Padre 1
  (ind_prof_1 <- which(padre_1[,2] == as.character(gen_elegido[2])))
  (ind_hora_1 <- which(padre_1[,4] == as.character(gen_elegido[4])))
  (ind_materia_1 <- which(padre_1[,1] == as.character(gen_elegido[1])))
  (ind_1 <- intersect(ind_prof_1,union(ind_hora_1,ind_materia_1)))
  #' Se intersecta los índices de ind_elim_1 con los de la hora
  #' porque sólo se eliminan esos grupos.
  (ind_elim_1 <-intersect(ind_elim_1,ind_hora_1))
  (ind_1 <- union(ind_1,ind_elim_1))
  if(length(ind_1) > 0){
    padre_1 <- padre_1[-ind_1,]
    cat("\n Se eliminaron del padre 1: ",length(ind_1)," entradas")
    # cat(paste("Se eliminaron del padre 1: ",length(ind_1)," entradas"),
    #     file="outfile.txt",sep="\n",append=TRUE)
  }
  
  #' Padre 2
  (ind_prof_2 <- which(padre_2[,2] == as.character(gen_elegido[2])))
  (ind_hora_2 <- which(padre_2[,4] == as.character(gen_elegido[4])))
  (ind_materia_2 <- which(padre_2[,1] == as.character(gen_elegido[1])))
  (ind_2 <- intersect(ind_prof_2,union(ind_hora_2,ind_materia_2)))
  #' Se intersecta los índices de ind_elim_2 con los de la hora
  #' porque sólo se eliminan esos grupos.
  (ind_elim_2 <- intersect(ind_elim_2,ind_hora_2))
  (ind_2 <- union(ind_2,ind_elim_2))
  if(length(ind_2) > 0){
    padre_2 <- padre_2[-ind_2,]
    cat("\n Se eliminaron del padre 2: ",length(ind_2)," entradas")
    # cat(paste("Se eliminaron del padre 2: ",length(ind_2)," entradas"),
    #     file="outfile.txt",sep="\n",append=TRUE)
  }
  
  cat("\n El padre 1 tiene ",dim(padre_1)[1]," genes. \n El padre 2 tiene ",
      dim(padre_2)[1]," genes\n\n")
  # cat(paste("El padre 1 tiene ",dim(padre_1)[1]," genes."),
  #     file="outfile.txt",sep="\n",append=TRUE)
  # cat(paste("El padre 2 tiene ",dim(padre_2)[1]," genes."),
  #     file="outfile.txt",sep="\n",append=TRUE)
  
  lista_padres <- list()
  lista_padres[[1]] <- padre_1
  lista_padres[[2]] <- padre_2
  return(lista_padres)
}




# califica_ordena_asig ----------------------------------------------------
#' Title califica_ordena_asig: Función que se encarga de ordenar las
#' asignaciones por calificación
#'
#' @param poblacion_nueva: Lista con los hijos de la nueva población,
#' ya calificados.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return lista_info: Lista de 3 elementos:
#' 1) mat_calif_asig: Matriz con las calificaciones de las
#' asignaciones
#' 2) poblacion_inicial: Lista de matrices con asignación y
#' calificaciones
#' 3) mat_calif_x_generacion: Matriz con calificaciones 1 generación
#'
#' @examples
#' lista_info <- califica_ordena_asig(poblacion_nueva,param)
#' 
califica_ordena_asig <- function(poblacion_nueva,param){
  ptm <- proc.time()# Start the clock!
  #Se definen las variables que se van a utilizar
  tam_poblacion <- param$tam_poblacion
  n_cols_mat_calif <- param$n_cols_mat_calif
  mat_calif_x_generacion <- matrix(NaN,nrow = tam_poblacion,
                                   ncol = n_cols_mat_calif)
  poblacion <- list()
  calif_asignacion <- rbind(1:tam_poblacion,rep(0,tam_poblacion))
  
  for(n in 1:tam_poblacion){
    cat("\n n = ", n)
    #Calificamos
    lista_calif_asignacion <- poblacion_nueva[[n]]
    mat_calif_asig_x_gpo <- lista_calif_asignacion[[1]]
    poblacion[[n]] <- mat_calif_asig_x_gpo
    calif_asignacion[2,n] <- lista_calif_asignacion[[2]]
    mat_calif_x_generacion[n,1:length(
      mat_calif_asig_x_gpo[,5])] <- mat_calif_asig_x_gpo[,5]
  }
  
  ## 3) Ordenar de menor a mayor y definir la probabilidad acumulada
  calif_asignacion <- calif_asignacion[,order(
    calif_asignacion[2,])]
  mat_calif_asig <- data.frame(ind_Asig = calif_asignacion[1,],
                               Calif = calif_asignacion[2,])
  
  lista_info <- list()
  lista_info[[1]] <- mat_calif_asig#Matriz con las calificaciones de las asignaciones
  lista_info[[2]] <- poblacion#Lista de matrices con asignación y calificaciones
  lista_info[[3]] <- mat_calif_x_generacion#Matriz con calificaciones 1 generación
  
  cat("\nLa función califica_ordena_asig tardó: ",
      (proc.time()-ptm)[3]/60," minutos\n")
  
  return(lista_info)
}


# ajusta_mat_solicitudes --------------------------------------------------
#' Title ajusta_mat_solicitudes: Función que elimina la información del
#' profesor del gen elegido en la matriz de solicitudes. Se quitan los grupos
#' con la misma hora y materia del profesor del gen elegido.
#'
#' @param mat_solicitudes_restantes: Matriz de 5 columnas (Profesor,TC,
#' Materia,Num_Materia,Horario) que tiene la información de las solicitudes
#' pseudo-reales de los profesores. Es una submatriz de "mat_solicitudes_real".
#' @param gen_elegido: Vector de 4 entradas (Materia,Profesor,TC,Horario)
#' con la información del gen del padre elegido.
#'
#' @return mat_solicitudes_restantes: Submatriz de "mat_solicitudes_real".
#' Matriz de 5 columnas (Profesor,TC,Materia,Num_Materia,Horario) que tiene
#' la información de las solicitudes pseudo-reales de los profesores.
#'
#' @examples
#' mat_solicitudes_restantes <- ajusta_mat_solicitudes(
#' mat_solicitudes_restantes,gen_elegido)
#' 
ajusta_mat_solicitudes <- function(mat_solicitudes_restantes,gen_elegido){
  #Se definen las variables que se van a utilizar
  (prof <- as.character(gen_elegido[2]))
  (hora <- as.character(gen_elegido[4]))
  (materia <- as.character(gen_elegido[1]))
  (ind_prof <- which(mat_solicitudes_restantes[,1] == prof))
  (ind_hora <- which(mat_solicitudes_restantes[,5] == hora))
  (ind_materia <- which(mat_solicitudes_restantes[,3] == materia))
  (elim_hora_prof <- intersect(ind_prof,ind_hora))
  (elim_materia_prof <- intersect(ind_prof,ind_materia))
  (ind_elim <- union(elim_hora_prof,elim_materia_prof))
  
  mat_solicitudes_restantes <- mat_solicitudes_restantes[-ind_elim,]
  
  return(mat_solicitudes_restantes)
}



##### ALGORITMO GENÉTICO #####
#' Funciones que aplican el Algoritmo Genético para obtener una matriz
#' de asignaciones Materia-Profesor-Horario.

# AG_asignaciones ---------------------------------------------------------
#' Title AG_asignaciones: Función que aplica el algoritmo genético a las
#' asignaciones para encontrar una buena asignación.
#'
#' @param mat_esqueleto: Matriz de 15 renglones (horas) y 203 columnas
#' (materias). En la entrada (i,j) se tiene el número de grupos simulados
#' para la hora i, y la materia j.
#' @param mat_solicitudes_real: Matriz de 5 columnas (Profesor,TC,Materia,
#' Num_Materia,Horario) y 6 renglones que tiene la información de la
#' solicitud de "nom_prof". Se eligen 2 materias y hasta 3 diferentes
#' horarios. Se quitan los renglones repetidos. Se hace una "intersección"
#' con los grupos simulados en la matriz "mat_esqueleto" y así se obtienen
#' las solicitudes pseudo-reales de los profesores.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
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
#' list_asignacion_final <- AG_asignaciones(mat_esqueleto,
#' mat_solicitudes_real,mat_esqueleto_cotas,param)
#' 
AG_asignaciones <- function(mat_esqueleto,mat_solicitudes_real,param){
  ptm <- proc.time()# Start the clock!
  #Se definen las variables que se van a utilizar
  (tam_poblacion <- param$tam_poblacion)
  (num_generaciones <- param$num_generaciones)
  prob_mutacion <- param$prob_mutacion
  n_cols_mat_calif <- param$n_cols_mat_calif
  matrices_calif_x_generacion <- list()
  mejores_asig <- list()
  vec_prob_asig <- (2*(1:tam_poblacion))/(tam_poblacion*(tam_poblacion+1))
  calif_mejor_elem <- rep(0,(num_generaciones+1))
  colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
  mat_calif_generaciones <- matrix(0,nrow = tam_poblacion,
                                   ncol = (num_generaciones+1))
  mat_num_genes <- matrix(0,nrow = tam_poblacion,
                          ncol = (num_generaciones+1))
  mat_solicitudes_restantes <- mat_solicitudes_real#1886 5
  
  ptm_generaciones <- proc.time()# Start the clock!
  # g <- 1
  # g <- 2
  for(g in 1:num_generaciones){
    cat("\n *** GENERACIÓN ",g," ***")
    
    # ptm <- proc.time()# Start the clock!
    if(g == 1){#Población inicial
      ### 1) Generar población inicial y 2) Calificar
      # set.seed(1802)
      lista_info_inicial <- poblacion_calif_iniciales(mat_esqueleto,
                                                      mat_solicitudes_real,
                                                      param)#5.22/4.82 min
      mat_calif_asig <- lista_info_inicial[[1]]
      # pob_ini <- lista_info_inicial[[2]]
      poblacion <- lista_info_inicial[[2]]
      for (p in 1:tam_poblacion) {
        mat_num_genes[p,g] <- dim(poblacion[[p]])[1]
      }
      
      
      ### 12) Guardar una matriz con la calificación x gpo. de las
      #' asignaciones (como xiii de T45)
      matrices_calif_x_generacion[[g]] <- lista_info_inicial[[3]]
      
      ### 13) Hacer heatmap de la matriz en 12)
      heatmap(matrices_calif_x_generacion[[g]][,1:650],
              Colv = NA, Rowv = NA,
              main = paste0("Calificaciones ordenadas de generación ",g),
              scale="none",col=colMain)
      
      ### 11) Guardar la mejor asignación de la generación
      ind_mejor_asig <- mat_calif_asig[tam_poblacion,1]
      mejores_asig[[g]] <- list(mat_calif_asig,
                                poblacion[[ind_mejor_asig]])
      
      #Graficar datos
      calif_mejor_elem[g] <- mat_calif_asig[tam_poblacion,2]
      plot(calif_mejor_elem[1:g],main = "Calificaciones del mejor elemento",
           xlab = "Generación",ylab = "Calificación")
      mat_calif_generaciones[,g] <- mat_calif_asig[,2]
      matplot(mat_calif_generaciones[,1:g],type = "l",
              main = "Calificaciones de las asignaciones por generación",
              xlab = "Asignaciones",ylab = "Calificaciones")
      
      
      matplot(mat_num_genes[,1:g],type = "l",
              main = "Número de genes por generación",
              xlab = "Asignaciones",ylab = "Número de genes")
    }#Termina población inicial
    
    # ### 13) Hacer heatmap de la matriz en 12)
    # heatmap(matrices_calif_x_generacion[[g]][,1:650],
    #         Colv = NA, Rowv = NA,
    #         main = paste0("Calificaciones ordenadas de generación ",g),
    #         scale="none",col=colMain)
    # 
    # ### 11) Guardar la mejor asignación de la generación
    # ind_mejor_asig <- mat_calif_asig[tam_poblacion,1]
    # mejores_asig[[g]] <- list(mat_calif_asig,
    #                           poblacion[[ind_mejor_asig]])
    # 
    # #Graficar datos
    # calif_mejor_elem[g] <- mat_calif_asig[tam_poblacion,2]
    # plot(calif_mejor_elem[1:g],main = "Calificaciones del mejor elemento",
    #      xlab = "Generación",ylab = "Calificación")
    # mat_calif_generaciones[,g] <- mat_calif_asig[,2]
    # matplot(mat_calif_generaciones[,1:g],type = "l",
    #         main = "Calificaciones de las asignaciones por generación",
    #         xlab = "Asignaciones",ylab = "Calificaciones")
    
    # save(calif_mejor_elem,file = "calif_mejor_elem.RData")
    # save(mat_calif_generaciones,file = "mat_calif_generaciones.RData")
    # ptm_pob <- proc.time()# Start the clock!
    for(n in 1:tam_poblacion){
      cat("\n *** HIJO ",n," ***")
      if(n == 1){poblacion_nueva <- list()}
      hijo <- data.frame(Materia = 0, Profesor = 0,TC = 0,
                         Horario = 0)
      esq_hijo <- matrix(0,nrow = length(param$Horas),
                         ncol = length(param$vec_nom_materias_total))
      ### 4) Elegir 2 padres con prob = 2i/(n*(n+1))
      #' La selección es por "Rank Selection":
      #' a) Elegir 2 individuos aleatoriamente
      #' b) La asignación con la calificación más alta es el padre 1
      #' c) Repetir a) y b) para el padre 2
      #' Nota: Se toman los índices y no los números de las asignaciones
      #' porque se tiene el vector de probabilidades que depende de
      #' la posición en la que se encuentren las asignaciones.
      ind_padres <- c(0,0)
      (ind_mat_1 <- sample(x = 1:tam_poblacion,size = 2,
                           prob = vec_prob_asig))
      (ind_padres[1] <- max(ind_mat_1))
      (ind_mat_2 <- sample(x = 1:tam_poblacion,size = 2,
                           prob = vec_prob_asig))
      
      #' Para no tener al mismo padre dos veces
      while(max(ind_mat_2) == max(ind_mat_1)){
        (ind_mat_2 <- sample(x = 1:tam_poblacion,size = 2,
                             prob = vec_prob_asig))
      }
      (ind_padres[2] <- max(ind_mat_2))
      
      padre_1 <- poblacion[[ind_padres[1]]]
      padre_2 <- poblacion[[ind_padres[2]]]
      
      while(dim(padre_1)[1]!=0 && dim(padre_2)[1]!=0){
        # Repetir hasta que uno de los padres se quede sin genes.
        
        ### 5) Con prob = 0.5 se elige un padre
        (ind_padre_elegido <- sample(x=1:2,size = 1))
        cat("\n Se eligió al padre ",ind_padre_elegido)
        
        if(ind_padre_elegido == 1){
          padre_elegido <- padre_1
        }else{
          padre_elegido <- padre_2
        }
        
        ### 6) Elegir un gen (grupo) del padre seleccionado con prob = 2i/(n*(n+1))
        (num_genes <- dim(padre_elegido)[1])
        vec_prob_genes <- (2*(1:num_genes))/(num_genes*(num_genes+1))
        (ind_gen <- sample(x = 1:num_genes,
                           size = 1,
                           prob = vec_prob_genes))
        (gen_elegido <- padre_elegido[ind_gen,1:4])
        
        ### 7) Mutación
        (r_num_muta <- runif(1))
        if(r_num_muta<prob_mutacion && dim(mat_solicitudes_restantes)[1]>0){
          #La 2° condición verifica si aún hay solicitudes para elegir
          cat("\n Entra a mutación")
          cat("\n dim(mat_solicitudes_restantes)[1] = ",
              dim(mat_solicitudes_restantes)[1])
          # cat("Entra a mutación",file="outfile.txt",sep="\n",append=TRUE)
          (gen_elegido <- elige_gen_de_solicitud(mat_solicitudes_restantes,
                                                 hijo,param))
        }
        if(r_num_muta<prob_mutacion && dim(mat_solicitudes_restantes)[1]==0){
          #La 2° condición verifica que ya no hay solicitudes para elegir
          cat("\n*** La matriz mat_solicitudes_restantes, ya no tiene información ***")
          cat("\n*** g = ",g," ***")
          cat("\n*** hijo = ",n," ***")
        }
        if(dim(gen_elegido)[1]==0 || is.na(gen_elegido[1])){
          #' Cuando la matriz de solicitudes restantes tiene pocos
          #' grupos se arroja un gen lleno de NA's
          cat("\n Entra al if de *emergencia*")
          (gen_elegido <- padre_elegido[ind_gen,1:4])
        }
        
        hijo <- rbind(hijo,gen_elegido)
        (num_materia_gen <- arroja_num_materia(as.character(gen_elegido[1]),
                                               param))
        (ind_hora_gen <- which(7:21 == as.numeric(gen_elegido[4])))
        esq_hijo[ind_hora_gen,num_materia_gen] <- esq_hijo[ind_hora_gen,
                                                           num_materia_gen] + 1
        
        #' Se ajusta la información de las solicitudes
        mat_solicitudes_restantes <- ajusta_mat_solicitudes(
          mat_solicitudes_restantes,gen_elegido)
        
        
        ### 8) Ajustar información de los padres con respecto al nuevo
        ###gen del hijo
        lista_padres <- ajusta_genes_padres(esq_hijo,padre_1,padre_2,
                                            gen_elegido,mat_esqueleto)
        padre_1 <- lista_padres[[1]]
        padre_2 <- lista_padres[[2]]
      }#Fin while()
      #' Quitamos el renglón de ceros inicial
      hijo <- unique(hijo)#Para evitar repeticiones en los grupos
      hijo <- hijo %>% filter(Profesor != 0)
      
      ### 9) Añadir los genes restantes del otro padre al hijo
      if(dim(padre_1)[1] > 0){
        for(i in 1:dim(padre_1)[1]){
          gen_elegido <- padre_1[i,]
          (num_materia_gen <- arroja_num_materia(as.character(gen_elegido[1]),
                                                 param))
          (ind_hora_gen <- which(7:21 == as.numeric(gen_elegido[4])))
          esq_hijo[ind_hora_gen,num_materia_gen] <- esq_hijo[ind_hora_gen,
                                                             num_materia_gen]+1
        }
        hijo <- rbind(hijo,padre_1[,1:4])
      }
      if(dim(padre_2)[1] > 0){
        for(i in 1:dim(padre_2)[1]){
          gen_elegido <- padre_2[i,]
          (num_materia_gen <- arroja_num_materia(as.character(gen_elegido[1]),
                                                 param))
          (ind_hora_gen <- which(7:21 == as.numeric(gen_elegido[4])))
          esq_hijo[ind_hora_gen,num_materia_gen] <- esq_hijo[ind_hora_gen,
                                                             num_materia_gen]+1
        }
        hijo <- rbind(hijo,padre_2[,1:4])
      }
      
      #' Se asignan profesores a los grupos faltantes, como la
      #' población inicial.
      esq_gpos_faltantes <- mat_esqueleto - esq_hijo
      lista_asig <- gen_asignacion(esq_gpos_faltantes,
                                   mat_solicitudes_restantes,
                                   param)#12.78 seg
      mat_asig <- lista_asig[[1]]
      hijo <- rbind(hijo,mat_asig)
      
      ### 2) Calificar y 3) Ordenar las calificaciones del hijo
      # esq_hijo <- gen_esq_hijo(hijo,param)#Grupos con profesor en el hijo
      lista_hijo <- list(hijo,esq_hijo)
      lista_calif_hijo <- califica_asignacion(mat_esqueleto,
                                              mat_solicitudes_real,
                                              lista_hijo,param)
      poblacion_nueva[[n]] <- lista_calif_hijo
      
      #Cada hijo vuelve a iniciar con la matriz de solicitudes completa
      mat_solicitudes_restantes <- mat_solicitudes_real#1886 5
    }#Fin for(n)
    # cat("\nEl ciclo tardó: ",(proc.time()-ptm_pob)[3]/60,
    #     " minutos. Para 1 generación \n")#15/37.83min
    
    lista_info <- califica_ordena_asig(poblacion_nueva,param)
    mat_calif_asig <- lista_info[[1]]
    poblacion <- lista_info[[2]]
    for (p in 1:tam_poblacion) {
      mat_num_genes[p,(g+1)] <- dim(poblacion[[p]])[1]
    }
    # save(mat_num_genes,file = "mat_num_genes.RData")
    ### 12) Guardar una matriz con la calificación x gpo. de las
    #' asignaciones (como xiii de T45)
    matrices_calif_x_generacion[[(g+1)]] <- lista_info[[3]]
    
    ### 13) Hacer heatmap de la matriz en 12)
    heatmap(matrices_calif_x_generacion[[(g+1)]][,1:650],
            Colv = NA, Rowv = NA,
            main = paste0("Calificaciones ordenadas de generación ",g+1),
            scale="none",col=colMain)
    
    ### 11) Guardar la mejor asignación de la generación
    ind_mejor_asig <- mat_calif_asig[tam_poblacion,1]
    mejores_asig[[(g+1)]] <- list(mat_calif_asig,
                                  poblacion[[ind_mejor_asig]])
    
    #Graficar datos
    calif_mejor_elem[(g+1)] <- mat_calif_asig[tam_poblacion,2]
    plot(calif_mejor_elem[1:(g+1)],
         main = "Calificaciones del mejor elemento",
         xlab = "Generación",ylab = "Calificación")
    mat_calif_generaciones[,(g+1)] <- mat_calif_asig[,2]
    matplot(mat_calif_generaciones[,1:(g+1)],type = "l",
            main = "Calificaciones de las asignaciones por generación",
            xlab = "Asignaciones",ylab = "Calificaciones")
    
    matplot(mat_num_genes[,1:(g+1)],type = "l",
            main = "Número de genes por generación",
            xlab = "Asignaciones",ylab = "Número de genes")
  }#Fin for(g)
  tiempo_minutos <- (proc.time()-ptm_generaciones)[3]/60
  cat("\nEl ciclo tardó: ",tiempo_minutos," minutos. Para ",
      num_generaciones," generaciones \n")
  
  ### 14) Se define la asignación final
  mejor_asig <- mejores_asig[[(num_generaciones+1)]][[2]]
  mat_asignacion_final <- cbind(mejor_asig$Materia,
                                mejor_asig$Profesor,
                                mejor_asig$Horario)
  colnames(mat_asignacion_final) <- c("Materia","Profesor","Horario")
  # View(mat_asignacion_final)
  # save(mat_asignacion_final,file = "mat_asignacion_final.RData")
  
  
  # Esqueleto asignación final
  esq_asig_final <- matrix(0,nrow = length(param$Horas),
                           ncol = length(param$vec_nom_materias_total))
  rownames(esq_asig_final) <- param$nombre_hrs
  colnames(esq_asig_final) <- param$vec_nom_materias_total
  asig_final  <- data.frame(mat_asignacion_final ,Num_Materia = 0)
  
  for(r in 1:dim(asig_final )[1]){
    materia <- asig_final$Materia[r]
    asig_final$Num_Materia[r] <- arroja_num_materia(materia,param)
  }
  
  for(m in 1:length(param$vec_nom_materias_total)){
    materia <- param$vec_nom_materias_total[m]
    cat("\n Materia ",m,": ",materia)
    mat_materia <- asig_final  %>% filter(Materia == materia)
    for(h in 1:length(param$Horas)){
      hora <- param$Horas[h]
      mat_hora <- mat_materia %>% filter(Horario == hora)
      esq_asig_final[h,m] <- dim(mat_hora)[1]
    }
  }
  
  
  # Info de grupos sin asignación
  dif_x_materia <- colSums(mat_esqueleto) - colSums(esq_asig_final)
  dif_rel <- (colSums(mat_esqueleto) - colSums(esq_asig_final))/colSums(mat_esqueleto)
  info_gpos_sin_asig <- data.frame(mat_esq = colSums(mat_esqueleto),
                                   esq_asig_fin = colSums(esq_asig_final),
                                   gpos_sin_asig = dif_x_materia,
                                   dif_rel = dif_rel)
  # View(info_gpos_sin_asig)
  
  # Vector con info de AG
  vec_info_AG <- data.frame(Num_generaciones = num_generaciones+1,
                            Tam_pob = tam_poblacion,
                            Tiempo = tiempo_minutos,
                            Mejor_calif = max(calif_mejor_elem),
                            Num_genes_asig_fin = dim(mat_asignacion_final)[1],
                            Calif_asig_fin = calif_mejor_elem[num_generaciones+1],
                            Prom_genes_gen1 = mean(mat_num_genes[,1]),
                            Prom_genes_generaciones = mean(
                              mat_num_genes[,2:(num_generaciones+1)]))
  
  list_asignacion_final <- list()
  list_asignacion_final[[1]] <- mat_asignacion_final
  list_asignacion_final[[2]] <- calif_mejor_elem #Vector con calificaciones de los mejores elementos por generación
  list_asignacion_final[[3]] <- mat_calif_generaciones #Matriz con calificaciones de todos los elementos de todas las generaciones
  list_asignacion_final[[4]] <- matrices_calif_x_generacion #Lista de tamaño num_generaciones+1 con las matrices de calificaciones ordenadas por generación .
  list_asignacion_final[[5]] <- mejores_asig #Lista de tamaño num_generaciones+1 con la información de los mejores hijos de cada generación.
  list_asignacion_final[[6]] <- mat_num_genes #Matriz con el número de genes de todos los elementos por generación
  list_asignacion_final[[7]] <- mat_esqueleto
  list_asignacion_final[[8]] <- mat_solicitudes_real
  list_asignacion_final[[9]] <- param
  list_asignacion_final[[10]] <- vec_info_AG #Vector con información del AG y sus resultados
  list_asignacion_final[[11]] <- esq_asig_final #mat_esqueleto de la asignación final
  list_asignacion_final[[12]] <- info_gpos_sin_asig #Matriz con las columnas: mat_esq (gpos. x materia en mat_esqueleto), esq_asig_fin (gpos. x materia en esq_asig_final), gpos_sin_asig (gpos. sin asignación x materia), dif_rel (diferencia relativa x materia)
  
  names(list_asignacion_final) <- c("mat_asignacion_final",
                                    "calif_mejor_elem",
                                    "mat_calif_generaciones",
                                    "matrices_calif_x_generacion",
                                    "mejores_asig",
                                    "mat_num_genes",
                                    "mat_esqueleto",
                                    "mat_solicitudes_real",
                                    "param",
                                    "vec_info_AG",
                                    "esq_asig_final",
                                    "info_gpos_sin_asig")
  cat("\nLa función AG_asignaciones tardó: ",(proc.time()-ptm)[3]/60,
      " minutos\n")
  return(list_asignacion_final)
}


# AG_asignaciones_con_xlsx ------------------------------------------------
#' Title AG_asignaciones_con_xlsx: Función que aplica el algoritmo genético
#' a las asignaciones para encontrar una buena asignación. La función tiene
#' la opción de leer un documento de excel con asignaciones previas. En caso
#' de que si lea el documento, quita las asignaciones previas de las
#' solicitudes más la respectiva información y genera una asignación.
#'
#' @param mat_esqueleto: Matriz de 15 renglones (horas) y 203 columnas
#' (materias). En la entrada (i,j) se tiene el número de grupos simulados
#' para la hora i, y la materia j.
#' @param mat_solicitudes_real: Matriz de 5 columnas (Profesor,TC,Materia,
#' Num_Materia,Horario) y 6 renglones que tiene la información de la
#' solicitud de "nom_prof". Se eligen 2 materias y hasta 3 diferentes
#' horarios. Se quitan los renglones repetidos. Se hace una "intersección"
#' con los grupos simulados en la matriz "mat_esqueleto" y así se obtienen
#' las solicitudes pseudo-reales de los profesores.
#' @param con_xlsx_1_sin_xlsx_0: Variable binaria que vale 1 si se debe
#' de leer el archivo xlsx y 0 si no.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
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
#' list_asignacion_final <- AG_asignaciones_con_xlsx(mat_esqueleto,
#' mat_solicitudes_real,con_xlsx_1_sin_xlsx_0,param)
#' 
AG_asignaciones_con_xlsx <- function(mat_esqueleto,
                                     mat_solicitudes_real,
                                     con_xlsx_1_sin_xlsx_0,param){
  if(con_xlsx_1_sin_xlsx_0 == 0){
    #' Si no se carga el archivo de excel se realiza la asignación
    #' con el AG de manera normal. Sin modificar las matrices con el
    #' esqueleto y las solicitudes.
    list_asignacion_final <- AG_asignaciones(mat_esqueleto,
                                             mat_solicitudes_real,param)
  }else{
    #Si se carga el archivo de excel
    asig_fijas <- read_excel("horario.xlsx", sheet = "Horario")
    mat_solicitudes_restantes <- mat_solicitudes_real
    mat_esqueleto_restante <- mat_esqueleto
    
    for(r in 1:dim(asig_fijas)[1]){
      (materia <- as.character(asig_fijas[r,1]))
      (prof <- as.character(asig_fijas[r,2]))
      (hora <- as.character(asig_fijas[r,3]))
      
      #Se eliminan las solicitudes
      (ind_prof <- which(mat_solicitudes_restantes[,1] == prof))
      (ind_hora <- which(mat_solicitudes_restantes[,5] == hora))
      (ind_materia <- which(mat_solicitudes_restantes[,3] == materia))
      (elim_hora_prof <- intersect(ind_prof,ind_hora))
      (elim_materia_prof <- intersect(ind_prof,ind_materia))
      (ind_elim <- union(elim_hora_prof,elim_materia_prof))
      if(length(ind_elim) > 0){
        mat_solicitudes_restantes <- mat_solicitudes_restantes[-ind_elim,]
      }
      
      #Se eliminan los grupos en el esqueleto
      (h_hora <- which(7:21 == as.numeric(hora)))
      (j_materia <- arroja_num_materia(materia,param))
      mat_esqueleto_restante[h_hora,j_materia] <- mat_esqueleto_restante[h_hora,
                                                                         j_materia] - 1
      
    }#Fin for(r)
    list_asignacion_final <- AG_asignaciones(mat_esqueleto_restante,
                                             mat_solicitudes_restantes,param)
    list_asignacion_final[[1]] <- rbind(list_asignacion_final[[1]],asig_fijas)
  }#Fin else
  
  return(list_asignacion_final)
}



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
#' 7) mat_esqueleto: Matriz con el esqueleto utilizado para generar la
#' asignación.
#' 8) mat_solicitudes_real: Matriz con las solicitudes simuladas de los
#' profesores.
#' 9) param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
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
  #' 5) Simulación de esqueletos
  n_rep <- 10
  set.seed(42)
  lista_esq_D_prima <- metodo_B(n_rep,param,param_sim)
  mat_esqueleto <- lista_esq_D_prima[[1]]
  # View(mat_esqueleto)
  
  #' 4b) Simulación de solicitudes de profesores (pseudo-real)
  set.seed(42)
  mat_solicitudes_real <- gen_solicitudes_real(mat_esqueleto,param)#8.3 seg
  # View(mat_solicitudes_real)
  
  #' 10) AG aplicado a asignaciones: Aquí ya va a salir una buena asignación
  set.seed(42)
  list_asignacion_final <- AG_asignaciones_con_xlsx(mat_esqueleto,
                                                    mat_solicitudes_real,
                                                    con_xlsx_1_sin_xlsx_0,
                                                    param)
  cat("\nLa función gen_asignacion_completa tardó: ",
      (proc.time()-ptm)[3]/60," minutos. \n")
  return(list_asignacion_final)
}

