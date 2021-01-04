##########################################################################
##### PARÁMETROS INICIALES #####
#' En este programa se encuentran las funciones y parámetros que se
#' requieren para la asignación de horarios y profesores para cada materia.
#' En esta versión ya no se tienen las funciones que limpian las matrices
#' "m_grande".
##########################################################################

# Inicio ------------------------------------------------------------------
# rm(list=ls())  # Borra variables
# cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
# setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")

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
library("writexl")#Para guardar data frames
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
library(het.test)
library(magrittr)
library(dplyr)
library(resample)
library(ggpubr)
library('xtable')


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
param$q1 = 85
param$q2 = 80
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
param$m_grande_2015 = matrix(0,ncol = length(param$nom_cols_MG))
param$vec_nom_materias_total = 0
param$mat_nom_prof_total = 0
param$num_max_asig = 2
param$cota_TC = 1000
param$cota_asig = 6000
param$tam_poblacion = 5
param$num_generaciones = 2
param$prob_mutacion = 1/(6+18)
param$n_cols_mat_calif = 2000
param$elige_TC = 0.7
# param$ = 
# param$ = 
# param$ = 


load(file = paste0("Matrices m_grande_total/m_grande_total_",
                   param$sem_ini,"_",param$sem_fin,".RData"))
param$m_grande_total = m_grande_total

load(file = "vec_nom_materias_total.RData")
param$vec_nom_materias_total = vec_nom_materias_total

load(file = "mat_nom_prof_total.RData")
param$mat_nom_prof_total = mat_nom_prof_total

load("Matrices m_grande_total/m_grande_total_20151_20201.RData")
#Se quitan los renglones con NA
# m_grande_total <- m_grande_total[!is.na(m_grande_total[,1]),]
# param$m_grande_2015 = m_grande_total[!is.na(m_grande_total[,1]),]
param$m_grande_2015 = m_grande_total

# param_sim ---------------------------------------------------------------
param_sim <- list()
# param_sim$vec_sem_sig = c(20191,20192,20201)
param_sim$vec_sem_sig = 20202
param_sim$k_sem_ant = 5##Se inicia en 2016-2, 2017-1, 2017-2
# param_sim$Materias = "Estadística III" ##Puede ser una o más materias
param_sim$Materias = c("Estadística III",
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
                       "Procesos Estocásticos I")#15
param_sim$num_sim = 10
param_sim$m_filtrada = matrix(0,ncol = length(param$nom_cols_MG))
param_sim$sub_m_filtrada = matrix(0,ncol = length(param$nom_cols_MG))
param_sim$posibles_comb_q = matrix(c(80,85,95,99),ncol = 2,byrow = T)


# param_graficas ----------------------------------------------------------
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

# demo(graphics) # Ejecútela usted
# dev.off()#Para salir de la función par()

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
  #Se definen las variables que se van a utilizar
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
#' @param materia: Nombre de la materia de la que se busca conocer el
#' número de materia.
#' @example materia <- "Estadística I"
#'
#' @return num_materia: Número de materia
#' @example num_materia <- 42
#'
arroja_num_materia <- function(materia){
  #Se carga la matriz con los nombres de las materias
  load("mat_nom_materias_total.RData")
  dim_mat_nom <- dim(mat_nom_materias_total)
  
  #Se definen las variables que se van a utilizar
  vec_info_nombre <- c(0,0)
  
  for(d in 1:dim_mat_nom[1]){
    ind <- which(mat_nom_materias_total[d,c(1,3:dim_mat_nom[2])] == materia)
    if(length(ind) > 0){
      vec_info_nombre <- c(mat_nom_materias_total[d,1],d)
    }
  }#Fin for(d)
  
  if(vec_info_nombre[1] == 0){
    cat("\n La materia ",materia," no se encontró.")
    num_materia <- 0
  }else{
    num_materia <- as.numeric(vec_info_nombre[2])
  }
  
  return(num_materia)
}

##########################################################################
##### MATERIAS #####
#' Funciones encargadas de corregir los nombres de las materias para que
#' no haya repeticiones.
##########################################################################


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


##########################################################################
##### M_GRANDE_TOTAL / M_FILTRADA #####
## Función que generan la matriz "m_grande_total" con la información de las
##páginas de la facultad.
##########################################################################

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

##########################################################################
##### INFORMACIÓN EXTRA #####
#'Funciones encargadas de extraer, información adicional de las páginas
#'de la facultad de Ciencias.
##########################################################################

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



##########################################################################
##### SIM. DEMANDA DE ALUMNOS #####
#'Funciones encargadas de extraer, estimar y simular la demanda del número
#'de alumnos totales para "sem_sig".
##########################################################################

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

############################################################################
##### SOLICITUDES #####
## Funciones que generan una matriz con solicitudes de todos los profesores.
############################################################################



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


##########################################################################
##### GMM #####
#' Funcion que aplican el modelo de mezcla de Normales para generar una
#' matriz con la demanda de alumnos para el siguiente semestre.
##########################################################################



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


##########################################################################
##### ESQUELETO #####
## Funciones que generan un esqueleto del siguiente semestre.
##########################################################################

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
  num_alum_x_profesor <- ceiling(runif(1,min = min(num_Alumnos),
                                       max = max(num_Alumnos)))
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



############################################################################
##### METODOLOGÍAS #####
#' Funciones que simulan un esqueleto. Se probaron 4 metodologías distintas.
############################################################################


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



############################################################################
##### ASIGNACIÓN #####
#' Función que que genera una matriz con las asignaciones de Materia-
#' Profesor-Horario.
############################################################################

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
    (num_al <- round(runif(1,0,0.1),4))
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
      (num_al <- round(runif(1,0,0.1),4))
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
  if(r_num_TC <= param$elige_TC){
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
  
  (num_materia_gen <- arroja_num_materia(as.character(gen_elegido[1])))
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



##### **AQUÍ** #####

############################################################################
##### ASIGNACIÓN COMPLETA #####
## Función que manda llamar todas las funciones para obtener la matriz
##de asignaciones Materia-Profesor-Horario(-Salón)
############################################################################



