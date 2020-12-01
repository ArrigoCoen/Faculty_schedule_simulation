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
#' @param materia: Nombre de la materia de la que se busca conocer el
#' número de materia.
#' @example materia <- "Estadística I"
#'
#' @return num_materia: Número de materia
#' @example num_materia <- 42
#'
arroja_num_materia <- function(materia){
  vec_info_nombre <- arroja_nom_correcto(materia)
  num_materia <- as.numeric(vec_info_nombre[2])
  
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
#' se deben de tomar en cuenta al crear "m_grande".
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
  names(lista_mat_materias_x_carrera) <- c("Actuaria","CdD","Mate",
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
#' "mat_demanda_alumnos" con 15 renglones (horas) y 201 columnas (materias).
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
#' @return mat_demanda_alumnos: Matriz de 15 renglones (horas) y 201
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
  vec_nom_materias_total <- param$vec_nom_materias_total#201
  m_grande_2015 <- param$m_grande_2015#8393 37
  mat_1_solicitud <- data.frame(Profesor = 0,TC = 0, Materia = rep(0,6),
                                Num_Materia = 0,Horario = 0)
  
  #Se definen las variables con la información de "nom_prof"
  mat_1_prof <- m_grande_2015[m_grande_2015[,num_col_Profesor]==nom_prof,]
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
  num_col_Profesor <- arroja_ind_col_MG("Profesor")
  mat_nom_prof_total <- param$mat_nom_prof_total#1387 2
  m_grande_2015 <- param$m_grande_2015#8409 37
  mat_solicitudes <- data.frame(Profesor = 0,TC = 0, Materia = 0,
                                Num_Materia = 0,Horario = 0)
  
  #' Se quitan los renglones de ceros, con NA o vaciós en la
  #' columna de "Profesor"
  m_grande_2015 <- m_grande_2015[m_grande_2015[,num_col_Profesor]!="",]
  m_grande_2015 <- m_grande_2015[m_grande_2015[,num_col_Profesor]!=0,]
  m_grande_2015 <- m_grande_2015[!is.na(m_grande_2015[,num_col_Profesor]),]
  # dim(m_grande_2015)#8393 37
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
#' matriz. La primera contiene todos los modelos de mezcla de Normales,
#' uno para cada materia en "vec_nom_materias_total". La segunda matriz
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
  
  for(c in 1:length(vec_nom_materias_total)){
    num_materia <- c
    materia <- vec_nom_materias_total[num_materia]
    cat("\n Materia ",num_materia,": ",materia)
    D_prima_1_materia <- D_prima_inicial[,c]
    
    param_sim$Materias = materia
    param_sim$m_filtrada <- gen_mat_m_filtrada(param,param_sim)
    mat_al_corregidos <- gen_mat_alumnos_corregidos(vec_s_sem_k_info,
                                                    param,param_sim)
    wait_1_materia = c(as.vector(mat_al_corregidos),D_prima_1_materia)
    
    if(mean(wait_1_materia) > 0){
      mixmdl_1_materia = normalmixEM(wait_1_materia,mean=mean(wait_1_materia))
    }else{
      mixmdl_1_materia <- 0
    }
    mixmdl[[c]] <- mixmdl_1_materia
    wait[[c]] <- wait_1_materia
  }#Fin for(c)
  
  lista_mod_y_wait <- list()
  lista_mod_y_wait[[1]] <- mixmdl
  lista_mod_y_wait[[2]] <- wait
  return(lista_mod_y_wait)
}


# actualiza_calif_esqueleto -----------------------------------------------
#' Title actualiza_calif_esqueleto: Función que actualiza las calificaciones
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
#' @return calif_esq: Lista de 2 elementos: "mat_calif_x_gpo" y
#' "vec_calif_x_materia". La matriz "mat_calif_x_gpo" (15*203) contiene las
#' calificaciones por grupo. El vector "vec_calif_x_materia"
#'
#' @examples
#' calif_esq <- actualiza_calif_esqueleto(D,D_prima,mat_calif_x_gpo,
#' ind_materias)
#' 
actualiza_calif_esqueleto <- function(D,D_prima,mat_calif_x_gpo,ind_materias){
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
  vec_calif_x_materia <- colSums(mat_calif_x_gpo)
  
  calif_esq <- list()
  calif_esq[[1]] <- mat_calif_x_gpo
  calif_esq[[2]] <- vec_calif_x_materia
  return(calif_esq)
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
    mixmdl_1_materia = normalmixEM(wait_1_materia,mean = mixmdl[[c]]$mu)
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
  calif_esq <- actualiza_calif_esqueleto(D,D_prima,mat_calif_x_gpo,
                                         1:dim(D)[2])
  mat_calif_x_gpo <- calif_esq[[1]]
  vec_calif_x_materia <- calif_esq[[2]]
  
  #' Actualizo mientras se cumplan las siguientes condiciones:
  while(any(vec_calif_x_materia< -20) || any(vec_calif_x_materia> 10)){
    ind_1 <- which(vec_calif_x_materia< -20)
    ind_2 <- which(vec_calif_x_materia> 10)
    ind_materias <- union(ind_1,ind_2)
    if(length(ind_materias) > 0){
      D_prima <- actualiza_D_prima(cota,D,D_prima,mixmdl,calif_esq,
                                   ind_materias)
      
      calif_esq <- actualiza_calif_esqueleto(D,D_prima,mat_calif_x_gpo,
                                             ind_materias)
      mat_calif_x_gpo <- calif_esq[[1]]
      vec_calif_x_materia <- calif_esq[[2]]
      
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
  num_col_Profesor <- arroja_ind_col_MG("Profesor")##2
  num_col_Alumnos <- arroja_ind_col_MG("Alumnos")##6
  num_col_NumMateria <- arroja_ind_col_MG("Num_materia")##37
  m_grande_2015 <- param$m_grande_2015
  m_grande_2015 <- m_grande_2015[!is.na(m_grande_2015[,num_col_Profesor]),]
  sub_mat_prof <- m_grande_2015[m_grande_2015[,num_col_Profesor]==nom_prof,]
  sub_mat <- sub_mat_prof[sub_mat_prof[,num_col_NumMateria]==num_materia,]
  
  num_Alumnos <- as.numeric(sub_mat[,num_col_Alumnos])
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
  sobran_alum_1si_0no <- 0
  
  #' Se evitan errores cuando no hay hora en la columna "Horario" o
  #' el número de materia es cero
    if(length(ind_hora>0) && renglon[4]!=0){
      (num_alum_aux <- mat_demanda_aux[ind_hora,as.numeric(renglon[4])])
      if(num_alum_aux > 0){#Si sobran alumnos
        sobran_alum_1si_0no <- 1
        (num_alum <- mat_demanda_alumnos[ind_hora,as.numeric(renglon[4])])
        if(num_alum_aux<=5 && num_alum>=10){#Cota para evitar seguir simulando alumnos
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
#' @param mat_demanda: Matriz de 15 renglones (horas) y 201 columnas
#' (materias). En la entrada (i,j) se tiene el número de alumnos simulados
#' para la hora i, y la materia j.
#'
#' @return lista_ciclo: Lista con la matriz "mat_esqueleto", la matriz
#' "mat_prof" y la matriz "mat_demanda".
#'
#' @examples
#' lista_ciclo <- ciclo_esqueleto(1000,mat_solicitudes_TC,mat_prof_TC,mat_esqueleto,
#' mat_demanda_alumnos)
#' lista_ciclo <- ciclo_esqueleto(6500,mat_solicitudes_asig,mat_prof_asig,
#' mat_esqueleto,mat_demanda_alumnos)
#' 
ciclo_esqueleto <- function(cota,mat_solicitudes,mat_prof,mat_demanda,
                            num_max_asig,mat_demanda_alumnos){
  #Se definen las variables que se van a utilizar
  num_alum_simulados <- 0
  mat_esqueleto <- matrix(0,nrow = length(param$Horas),
                          ncol = length(param$vec_nom_materias_total))
  rownames(mat_esqueleto) <- 1:length(param$Horas)
  colnames(mat_esqueleto) <- 1:length(param$vec_nom_materias_total)
  mat_E <- matrix(0,nrow = length(param$Horas),
                          ncol = length(param$vec_nom_materias_total))
  rownames(mat_E) <- 1:length(param$Horas)
  colnames(mat_E) <- 1:length(param$vec_nom_materias_total)
  
  for(n in 1:cota){#Cota para que el ciclo no sea infinito
    # cat("\n Iteración: ",n)
    #Se suman los valores positivos de la demanda de alumnos
    if(sum(mat_demanda[mat_demanda>0])>0 && dim(mat_solicitudes)[1]>0){
      #Número aleatorio para elegir profesor
      (num_al <- round(runif(1,min = 1,max = dim(mat_prof)[1])))
      (profesor <- mat_prof[num_al,1])
      mat_aux <- mat_solicitudes[mat_solicitudes[,1]==profesor,]
      
      if(dim(mat_aux)[1]>0){#Si hay información de "profesor"
        #Número aleatorio para elegir materia
        (num_al_2 <- round(runif(1,min = 1,max = dim(mat_aux)[1])))
        (materia_al <- mat_aux[num_al_2,3])
        (num_materia_al <- mat_aux[num_al_2,4])
        
        #Número aleatorio para elegir horario
        (num_al_3 <- round(runif(1,min = 1,max = dim(mat_aux)[1])))
        (hora_al <- mat_aux[num_al_3,5])
        
        (renglon <- c(profesor,1,materia_al,num_materia_al,hora_al))
        
        #Índices de renglón y columna para "mat_esqueleto"
        (M_i <- which(param$Horas == as.numeric(renglon[5])))
        (M_j <- as.numeric(renglon[4]))
        
        #Se verifica si la demanda ha sido cubierta o no
        (sobran_alum_1si_0no <- verifica_demanda_cubierta(mat_demanda,renglon,mat_demanda_alumnos))
        
        if(sobran_alum_1si_0no == 1){#Aún hay alumnos sin clase para esa materia
          #Simulamos el número de alumnos para este grupo
          (num_alum_x_profesor <- simula_alum_x_profesor(renglon,param))
          num_alum_simulados <- num_alum_simulados + num_alum_x_profesor
          mat_E[M_i,M_j] <- mat_E[M_i,M_j]+num_alum_x_profesor
          
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
  lista_ciclo[[2]] <- mat_prof
  lista_ciclo[[3]] <- mat_demanda
  lista_ciclo[[4]] <- mat_solicitudes
  lista_ciclo[[5]] <- num_alum_simulados
  lista_ciclo[[6]] <- mat_E
  names(lista_ciclo) <- c("mat_esqueleto","mat_prof","mat_demanda",
                          "mat_solicitudes","num_alum_simulados","mat_E")
  return(lista_ciclo)
}



# gen_esqueleto -----------------------------------------------------------
#' Title gen_esqueleto: Función que arroja una lista con las matrices:
#' 1) mat_esqueleto: Matriz de 15 renglones (horas) y 201 columnas
#' (materias). En la entrada (i,j) se tiene el número de grupos simulados
#' para la hora i, y la materia j.
#' 2) mat_prof_TC: Matriz de 2 columnas con el nombre de los profesores de
#' tiempo completo y el número de materias asignadas.
#' 3) mat_prof_asig: Matriz de 2 columnas con el nombre de los profesores de
#' asignatura y el número de materias asignadas.
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
#' 
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return lista_info_esqueleto: Lista con las matrices:
#' 1) mat_esqueleto: Matriz de 15 renglones (horas) y 201 columnas
#' (materias). En la entrada (i,j) se tiene el número de grupos simulados
#' para la hora i, y la materia j.
#' 2) mat_prof_TC: Matriz de 2 columnas con el nombre de los profesores de
#' tiempo completo y el número de materias asignadas.
#' 3) mat_prof_asig: Matriz de 2 columnas con el nombre de los profesores
#' de asignatura y el número de materias asignadas.
#' 4) 
#'
#' @examples
#' lista_info_esqueleto <- gen_esqueleto(mat_demanda_alumnos,mat_solicitudes,param)
#' 
gen_esqueleto <- function(mat_demanda_alumnos,mat_solicitudes,param){
  ptm <- proc.time()# Start the clock!
  #Se definen las variables que se van a utilizar
  num_max_asig <- param$num_max_asig
  mat_demanda_aux <- mat_demanda_alumnos
  mat_solicitudes_aux <- mat_solicitudes[as.numeric(mat_solicitudes[,5])>0,]
  mat_esqueleto <- matrix(0,nrow = length(param$Horas),
                          ncol = length(param$vec_nom_materias_total))
  rownames(mat_esqueleto) <- 1:length(param$Horas)
  colnames(mat_esqueleto) <- 1:length(param$vec_nom_materias_total)
  
  ##### Profesores de tiempo completo
  mat_solicitudes_TC <- mat_solicitudes_aux[mat_solicitudes_aux[,2]==1,]
  mat_prof_TC <- cbind(unique(mat_solicitudes_TC[,1]),
                       rep(0,length(unique(mat_solicitudes_TC[,1]))))
  lista_ciclo_TC <- ciclo_esqueleto(param$cota_TC,mat_solicitudes_TC,mat_prof_TC,
                                    mat_demanda_aux,num_max_asig,mat_demanda_alumnos)
  mat_prof_TC <- lista_ciclo_TC[[2]]
  colnames(mat_prof_TC) <- c("Profesor","Materias_Asig")
  mat_solicitudes_TC <- lista_ciclo_TC[[4]]
  num_alum_sim_TC <- lista_ciclo_TC[[5]]
  mat_E_TC <- lista_ciclo_TC[[6]]
  
  ##### Profesores de asignatura
  mat_solicitudes_asignatura <- mat_solicitudes_aux[mat_solicitudes_aux[,2]==0,]
  mat_prof_asig <- cbind(unique(mat_solicitudes_asignatura[,1]),
                         rep(0,length(unique(mat_solicitudes_asignatura[,1]))))
  
  lista_ciclo_asig <- ciclo_esqueleto(param$cota_asig,mat_solicitudes_asignatura,
                                      mat_prof_asig,lista_ciclo_TC[[3]],
                                      num_max_asig,mat_demanda_alumnos)
  mat_prof_asig <- lista_ciclo_asig[[2]]
  colnames(mat_prof_asig) <- c("Profesor","Materias_Asig")
  mat_solicitudes_asignatura <- lista_ciclo_asig[[4]]
  num_alum_sim_asig <- lista_ciclo_asig[[5]]
  mat_E_asig <- lista_ciclo_asig[[6]]
  
  #Se suman los grupos de los profesores de tiempo completo y los de asignatura
  mat_esqueleto <- lista_ciclo_TC[[1]] + lista_ciclo_asig[[1]]
  rownames(mat_esqueleto) <- param$nombre_hrs
  colnames(mat_esqueleto) <- param$vec_nom_materias_total
  
  #Se suma el número de alumnos simulados por hora y por materia
  mat_E <- mat_E_TC + mat_E_asig
  rownames(mat_E) <- param$nombre_hrs
  colnames(mat_E) <- param$vec_nom_materias_total
  
  #Se suma el número de alumnos simulados
  num_alum_simulados <- num_alum_sim_TC + num_alum_sim_asig
  
  lista_info_esqueleto <- list()
  lista_info_esqueleto[[1]] <- mat_esqueleto
  lista_info_esqueleto[[2]] <- mat_prof_TC
  lista_info_esqueleto[[3]] <- mat_prof_asig
  lista_info_esqueleto[[4]] <- lista_ciclo_asig[[3]]
  lista_info_esqueleto[[5]] <- mat_solicitudes_TC
  lista_info_esqueleto[[6]] <- mat_solicitudes_asignatura
  lista_info_esqueleto[[7]] <- num_alum_simulados
  lista_info_esqueleto[[8]] <- mat_E
  names(lista_info_esqueleto) <- c("mat_esqueleto","mat_prof_TC",
                                   "mat_prof_asig","mat_demanda_aux",
                                   "mat_solicitudes_TC",
                                   "mat_solicitudes_asignatura",
                                   "num_alum_sim_TC","mat_E")
  
  cat("\nLa función gen_esqueleto tardó: ",(proc.time()-ptm)[3],
      " segundos\n")#19.48seg
  return(lista_info_esqueleto)
}


##### **AQUÍ** #####




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
  ## Se carga el vector que contiene 201 materias para que todas las listas
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





