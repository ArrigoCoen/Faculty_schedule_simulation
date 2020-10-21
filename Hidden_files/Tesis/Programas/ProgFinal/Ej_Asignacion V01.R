##########################################################################
#' En este programa se encuentran los ejemplos de las diferentes funciones
#' que se encuentran en el archivo "Fn_Asignación.R"
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Programas/Prog V10")
source("Fn_Asignacion.R")

## Nombres para pruebas:
# profesor <- "Margarita Elvira Chávez Cano"
# materia <- "Estadística I"
# materia <- "Inferencia Estadística"
# materia <- "Estadística II"
# materia <- "Modelos no Paramétricos y de Regresión"
# materia <- "Estadística III"
# materia <- "Modelos de Supervivencia y de Series de Tiempo"
# 
# profesor <- "Claudia Orquídea López Soto"
# materia <- "Investigación de Operaciones"

# list_url -------------------------------------------------------------------
## Se definen las listas generales de parámetros que se van a utilizar:
list_url <- list()
list_url$sem_ini = 20081 # Datos SUPER GRANDE
# list_url$sem_ini = 20151 # Datos GRANDE
# list_url$sem_ini = 20172 # Datos Mediana 
# list_url$sem_ini = 20192 # Datos Chica 
# list_url$sem_fin = 20192
list_url$sem_fin = 20201
list_url$sem_actual = 20201
list_url$Actualiza_RAW_url = TRUE
list_url$Actualiza_limpia_base_url = TRUE
list_url$Actualiza_elimina_grupos_con_0 = TRUE
list_url$Salvar_URL_RData = TRUE
list_url$usar_vec_corto_num_materia = TRUE
list_url$elimina_pags_con_0_grupos = TRUE
# list_url$Carpeta_RData = "Archivos RData V01"
list_url$Carpeta_RData = "Archivos RData"

list_url$utilizar_RAW_anterior = T
list_url$usa_grupos_salvados = T
list_url$usa_vec_con_salon = T
list_url$usa_vec_con_info_salvados = T


list_url$planes_estudio = c(119,1176,2017,218,1556,217,2055)
list_url$file_name <- paste0(list_url$Carpeta_RData,"/Dat_URL_",list_url$sem_ini,"_",list_url$sem_fin,".RData")
list_url$file_name_RAW <- paste0(list_url$Carpeta_RData,"/Lista_RAW_Dat_URL_",list_url$sem_ini,"_",list_url$sem_fin,".RData")
list_url$nombres_carrera_plan <- c("Actuaría (plan 2000)",
                                   "Actuaría (plan 2006)",
                                   "Actuaría (plan 2015)",
                                   "Ciencias de la Computación (plan 1994)",
                                   "Ciencias de la Computación (plan 2013)",
                                   "Matemáticas (plan 1983)",
                                   "Matemáticas Aplicadas (plan 2017)")
list_url$mat_ubicaciones_url <- matrix(c("Materia"            ,'#info-contenido h2', T,F,
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
list_url$colnames_mat_posibles_url <- c("Semestre","Plan","Materia","URL","Grupos x pag","url_con_salon")
list_url$ncol_mat_posibles_url <- length(list_url$colnames_mat_posibles_url)
list_url$nrow_mat_posibles_url <- 200000
# list_url$nrow_mat_posibles_url = dim(list_url$mat_posibles_url)[1]
list_url$mat_RAW_url <- matrix(0,list_url$nrow_mat_posibles_url,list_url$ncol_mat_posibles_url)
list_url$mat_posibles_url <- list_url$mat_RAW_url


list_url$ncol_mat_Grande <- 13
list_url$mat_Grande <- matrix(0,0,list_url$ncol_mat_Grande)
list_url$mat_Grande_con_url <- matrix(0,0,list_url$ncol_mat_Grande+1)

list_url$semestres_reales <- list_url$sem_fin
list_url$plan_reales <- 2017
list_url$num_mat_reales <- 1
list_url$num_grupos <- rep(-1,list_url$nrow_mat_posibles_url)
list_url$url_con_salon <- NA

list_url$mat_paginas_error <- matrix(0,1,4)


list_url$indicadoras_actualiza_col_j_mat_Grande = rep(T,13)


colnames(list_url$mat_ubicaciones_url) <- c("Nombre columna","Ubicacion en pagina","Repetir","Elimina salto")
colnames(list_url$mat_Grande) <- c("Materia", "Profesor","Horario","Lugares","Alumnos","Salon",
                                   "Grupo","Carrera","Plan","Semestre","Cambios","Turno","Semestre_de_materia")

colnames(list_url$mat_paginas_error) <- c("Columna","length(vec)","num_gpo","Pagina")

if(F) {
  list_url$utilizar_RAW_anterior = F
  list_url$usa_grupos_salvados = F
  list_url$usa_vec_con_salon = F
  list_url$usa_vec_con_info_salvados = F
}
# Valida_list_url(list_url)


# param -------------------------------------------------------------------
param <- list()
param$sem_ini = 20081##Inicio de información real
param$sem_fin = 20201##Fin de información real
param$sem_sig = 20202##Semestre de simulación
# param$sem_totales <- (20081:param$sem_sig)[(20081:param$sem_sig)%% 10>0 
#                                            &(20081:param$sem_sig)%% 10<3]
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

# Actualización de parámetros ---------------------------------------------
load(paste0("Archivos RData/Lista_RAW_Dat_URL_20081_20201.RData"))
# list_url$mat_posibles_url = list_url$mat_posibles_url
## Se actualizan algunos parámetros
sem_ini <- 20081
# sem_ini <- 20131
# sem_fin <- 20192
sem_fin <- 20201
sem_sig  <-  20202
param$sem_ini = sem_ini
param$sem_fin = sem_fin
param$sem_sig = sem_sig
list_url$sem_ini = sem_ini
list_url$sem_actual = sem_fin
list_url$sem_fin = sem_fin

list_url$file_name = paste0(list_url$Carpeta_RData,"/Dat_URL_",list_url$sem_ini,
                            "_",list_url$sem_fin,".RData")
list_url$file_name_RAW = paste0(list_url$Carpeta_RData,"/Lista_RAW_Dat_URL_",
                                list_url$sem_ini,"_",list_url$sem_fin,".RData")
## Se crea el vector para los semestres pares e impares
semestres = (sem_ini:sem_fin)[(sem_ini:sem_fin) %% 10>0 &(sem_ini:sem_fin) %% 10<3]
param$Semestres = semestres
n_semestres_anteriores <- length(semestres)
param$nombre_sem = as.character(semestres)

load(file = paste0("Matrices m_grande_total/m_grande_total_",
                   param$sem_ini,"_",param$sem_fin,".RData"))
param$m_grande_total = m_grande_total

load(file = "vec_nom_materias_total.RData")
param$vec_nom_materias_total = vec_nom_materias_total



# posibles_url ------------------------------------------------------------
#' Función que arroja la lista "lista_url"  dentro de la cual se encuentra
#' la matriz con las posibles URL de las páginas de horarios de la FC. Dicha
#' matriz tiene 5 columnas: Semestre, Plan, Materia, URL, Grupos por página,
#' url_con_salon.
#' 
list_url <- posibles_url(list_url)
mat_posibles_url <- list_url$mat_posibles_url
View(mat_posibles_url)


# Actualiza_list_url ------------------------------------------------------
#' Generador de archivo .RData con información de semestres sacada de url.
#' Llena las columnas "Grupos por página" y "url_con_salon" de la matriz
#' "mat_posibles_url" para que tenga completas sus 6 columnas.
#' 
list_url <- Actualiza_list_url(list_url)
mat_posibles_url <- list_url$mat_posibles_url
View(mat_posibles_url)


# gen_m_grande_1_sem -------------------------------------------------------
#' Función que recibe como parámetro el semestre del que se desea obtener
#' la información y genera un archivo de tipo ".Rdata" con la matriz 
#' "m_grande" de dicho semestre. Regresa el nombre de la ubicación en la
#' que se encuentra guardado el archivo.
#' 
sem_info <- 20102
sem_info <- 20182
directorio_info <- gen_m_grande_1_sem(sem_info,list_url)
load(directorio_info)
View(m_grande)

## Todos los semestres
# sem_ini <- 20081
# sem_fin <- 20202
semestres = (20081:20202)[(20081:20202) %% 10>0 &(20081:20202) %% 10<3]
# semestres = (sem_ini:sem_fin)[(sem_ini:sem_fin) %% 10>0 &(sem_ini:sem_fin) %% 10<3]

for(d in 1:length(semestres)){
  gen_m_grande_1_sem(semestres[d],list_url)
}


# actualiza_m_grande_1_sem ------------------------------------------------
#' Title actualiza_m_grande_1_sem: Función que actualiza la matriz
#' "m_grande" de "sem_info".
#' - Se unen las materias iguales en un solo renglón
#' - Se conserva el nombre más reciente de la materia repetida
#' - Se verifica que las matrices sólo tengan datos del semestre
#' correspondiente.
#'
sem_info <- 20081
sem_info <- 20101
sem_info <- 20111
sem_info <- 20121
sem_info <- 20122
sem_info <- 20131
sem_info <- 20132
sem_info <- 20141
sem_info <- 20142
sem_info <- 20151
sem_info <- 20152
sem_info <- 20161
sem_info <- 20162
sem_info <- 20171
sem_info <- 20172
sem_info <- 20181
sem_info <- 20182
sem_info <- 20191
sem_info <- 20192
sem_info <- 20201
m_grande <- actualiza_m_grande_1_sem(sem_info,param)
View(m_grande)

## Todos los semestres
semestres <- (20081:20201)[(20081:20201)%% 10>0 &(20081:20201)%% 10<3]
for(i in 1:length(param$nombre_sem)){
  cat("\n sem_info = ",param$nombre_sem[i])
  actualiza_m_grande_1_sem(param$nombre_sem[i],param)
}


# imprime_info_idiomas_1_sem ----------------------------------------------
#' Title imprime_info_idiomas_1_sem: Función que imprime la información de
#' "idioma". Arroja una variable binaria que indica si la matriz "m_grande"
#' del semestre "sem_info" requiere modificación.
#'
# nom_archivo <- "m_grande por semestre SIN INGLES/m_grande_SIN_ING_20182.RData"
idioma <- "Inglés"
sem_info <- 20151##No necesita modificación //0
sem_info <- 20182##Necesita modificación //1
nom_archivo <- paste0("m_grande por semestre SIN INGLES/m_grande_SIN_ING_",
                      sem_info,".RData")

(imprime_info_idiomas_1_sem(nom_archivo,sem_info,idioma))
# mod_1si_0no <- imprime_info_idiomas_1_sem(nom_archivo,sem_info,idioma)
# mod_1si_0no


# imprime_info_idiomas ----------------------------------------------------
#' Title imprime_info_idiomas: Función que imprime la información de los
#' idiomas de los semestres 2008-1 al 2020-1. Arroja un vector con los
#' semestres que requieren modificación de las materias de "idioma".
#'
idioma <- "Inglés"
idioma <- "Francés"
vec_sem_idiomas <- imprime_info_idiomas(idioma)
vec_sem_idiomas


# revisa_gpos_idiomas_1_sem -----------------------------------------------
#' Title revisa_gpos_idiomas_1_sem: Función en la que se identifican y
#' separan los grupos de idiomas que se imparten en días distintos pero en
#' el mismo horario.
#'
sem_info <- 20081
sem_info <- 20101
sem_info <- 20111
sem_info <- 20121
sem_info <- 20122
sem_info <- 20131
sem_info <- 20132
sem_info <- 20141
sem_info <- 20142
sem_info <- 20151
sem_info <- 20191
##Sem con modificaciones en "/Inglés,/Inglés/Inglés,..."
sem_info <- 20152
sem_info <- 20161
sem_info <- 20162
sem_info <- 20171
sem_info <- 20172
sem_info <- 20181
sem_info <- 20182
sem_info <- 20192
sem_info <- 20201

idioma <- "Inglés"

m_grande <- revisa_gpos_idiomas_1_sem(sem_info,idioma)
View(m_grande)

## Todos los semestres
semestres <- (20081:20201)[(20081:20201)%% 10>0 &(20081:20201)%% 10<3]
for(i in 1:length(semestres)){
  cat("\n sem_info = ",semestres[i])
  m_grande <- revisa_gpos_idiomas_1_sem(semestres[i],idioma)
}



# gen_m_grande -------------------------------------------------------
#' Title gen_m_grande: Función que recibe como parámetro el semestre
#' del que se desea obtener la información y genera un archivo de tipo
#' ".Rdata" con la matriz "m_grande" de dicho semestre. Regresa el
#' nombre de la ubicación en la que se encuentra guardado el archivo.
#' La matriz "m_grande" que se guarda está limpia y actualizada.
#'
vec_excepciones <- "Inglés"
sem_info <- 20182
m_grande <- gen_m_grande(sem_info,vec_excepciones,param)

## Todos los semestres
# semestres <- param$Semestres
for(d in 1:length(param$nombre_sem)){
  sem_info <- param$nombre_sem[d]
  m_grande <- gen_m_grande(sem_info,vec_excepciones,param)
  # View(m_grande)
}

# gen_m_grande_total ------------------------------------------------------
#' Title gen_m_grande_total: Función que genera la matriz "m_grande_total"
#' para un intervalo dado.
#'
# param$sem_ini = 20081##Inicio de información real
# param$sem_fin = 20201##Fin de información real
# param$sem_sig = 20202##Semestre de simulación
# param$sem_totales <- (20081:param$sem_sig)[(20081:param$sem_sig)%% 10>0 
#                                            &(20081:param$sem_sig)%% 10<3]
# param$Semestres = (param$sem_ini:param$sem_fin)[(param$sem_ini:param$sem_fin) 
#                                                 %% 10>0 &(param$sem_ini:param$sem_fin) %% 10<3]
# param$nombre_sem = as.character(param$Semestres)
# param$n_semestres_anteriores = length(param$Semestres)

param$sem_ini = 20081##Inicio de información real
param$sem_fin = 20192##Fin de información real
param$sem_sig = 20201##Semestre de simulación
param$sem_totales <- (20081:param$sem_sig)[(20081:param$sem_sig)%% 10>0 
                                           &(20081:param$sem_sig)%% 10<3]
param$Semestres = (param$sem_ini:param$sem_fin)[(param$sem_ini:param$sem_fin) 
                                                %% 10>0 &(param$sem_ini:param$sem_fin) %% 10<3]
param$nombre_sem = as.character(param$Semestres)
param$n_semestres_anteriores = length(param$Semestres)

vec_excepciones <- "Inglés"
m_grande_total <- gen_m_grande_total(vec_excepciones,param)
View(m_grande_total)

## Todos los semestres
semestres <- param$sem_totales
for(d in length(semestres):11){
  n_semestres_anteriores <- d-1
  param$Semestres = semestres[1:n_semestres_anteriores]
  param$sem_fin = semestres[d-1]
  param$sem_sig = semestres[d]
  m_grande_total <- gen_m_grande_total(vec_excepciones,param)
  # View(m_grande_total)
}


# gen_vec_nom_materias_total ----------------------------------------------
#' Title gen_vec_nom_materias_total: Función que carga la matriz
#' "m_grande_total" de los semestres 2008-1 a 2020-1 de la cual se va
#' obtiene la lista de nombres de las materias sin repetición, conservando
#' los nombres más recientes de las materias.
#' 
vec_nom_materias_total <- gen_vec_nom_materias_total()
length(vec_nom_materias_total) ## 333
View(vec_nom_materias_total)


# arroja_ind_col_SG -------------------------------------------------------
#' Función que recibe el nombre de la columna que se busca y devuelve el
#' número de columna en "mat_simula_grupos" con ese nombre.
#'
arroja_ind_col_SG("Materia") ##1
arroja_ind_col_SG("Horario") ##2
arroja_ind_col_SG("Grupos_Simulados") ##3
arroja_ind_col_SG("Alumnos_Simulados_Totales") ##4
arroja_ind_col_SG("col_1er_grupo") ##5
arroja_ind_col_SG("col_ult_grupo") ##24


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
arroja_ind_col_RG("Materia") ##1
arroja_ind_col_RG("Horario") ##2
arroja_ind_col_RG("Grupos_Reales") ##3
arroja_ind_col_RG("Alumnos_Reales_Totales") ##4
arroja_ind_col_RG("col_1er_grupo") ##5
arroja_ind_col_RG("col_ult_grupo") ##24


# arroja_ind_col_MG -------------------------------------------------------
#' Title arroja_ind_col_MG: Función que recibe el nombre de la columna que
#' se busca y devuelve el número de columna en "m_grande" con ese nombre.
#'
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

# arroja_ind_col_MG("Materia")##1
# arroja_ind_col_MG("Profesor")##2
# arroja_ind_col_MG("Horario")##3
# arroja_ind_col_MG("horario_num")##4
# arroja_ind_col_MG("Lugares")##5
# arroja_ind_col_MG("Alumnos")##6
# arroja_ind_col_MG("Salon")##7
# arroja_ind_col_MG("Grupo")##8
# arroja_ind_col_MG("Carrera")##9
# arroja_ind_col_MG("Plan")##10
# arroja_ind_col_MG("Semestre")##11
# arroja_ind_col_MG("Cambios")##12
# arroja_ind_col_MG("Turno")##13
# arroja_ind_col_MG("Semestre_de_materia")##14
# arroja_ind_col_MG("url")##15
# arroja_ind_col_MG("Act2000")##16
# arroja_ind_col_MG("Act2006")##17
# arroja_ind_col_MG("Act2015")##18
# arroja_ind_col_MG("CdC1994")##19
# arroja_ind_col_MG("CdC2013")##20
# arroja_ind_col_MG("Mat1983")##21
# arroja_ind_col_MG("MAp2017")##22
# arroja_ind_col_MG("NomMat_Act2000")##23
# arroja_ind_col_MG("NomMat_Act2006")##24
# arroja_ind_col_MG("NomMat_Act2015")##25
# arroja_ind_col_MG("NomMat_CdC1994")##26
# arroja_ind_col_MG("NomMat_CdC2013")##27
# arroja_ind_col_MG("NomMat_Mat1983")##28
# arroja_ind_col_MG("NomMat_MAp2017")##29
# arroja_ind_col_MG("URL_Act2000")##30
# arroja_ind_col_MG("URL_Act2006")##31
# arroja_ind_col_MG("URL_Act2015")##32
# arroja_ind_col_MG("URL_CdC1994")##33
# arroja_ind_col_MG("URL_CdC2013")##34
# arroja_ind_col_MG("URL_Mat1983")##35
# arroja_ind_col_MG("URL_MAp2017")##36


# checa_ind_materia -------------------------------------------------------
#' Title checa_ind_materia: Función que revisa en qué índices de la matriz
#' "m_grande_total" o "m_grande" coincide "materia" con los nombres que se
#' encuentran en las columnas:
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
load("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Programas/Prog V05/m_grande_total_20081_20201_V01.RData")
View(m_grande_total)
materia <- "Modelos no Paramétricos y de Regresión"
checa_ind_materia(materia,m_grande_total)


# extrae_alumnos ----------------------------------------------------------
#' Se extrae el número de alumnos por hora y por semestre de cada materia.
#' 
#' @param materia: Nombre de la materia de la cual se obtendrá el número de
#' grupos por semestre.
#' @examples materia <- "Probabilidad I"
#' @return mat_alumnos_x_hora_sem: Matriz que tiene como información del
#' número de alumnos por hora (renglones) y semestre (columna).
#' 
materia <- "Estadística I" ##Igual que "Inferencia Estadística"
mat_alumnos_x_hora_sem <- extrae_alumnos(materia,param)
View(mat_alumnos_x_hora_sem)


# estima_alumnos -----------------------------------------------------------
#' Función que arroja una matriz de 15 renglones (horas) y 3 columnas: cota1,
#' media, cota2; los cuales corresponden a la estimación del número de alumnos
#' que se tendrán en el siguiente semestre por hora.
#' 
materia <- "Estadística I" ##Igual que "Inferencia Estadística"
mat_alumnos_estimados <- estima_alumnos(materia,param)
View(mat_alumnos_estimados)


# simula_alumnos -----------------------------------------------------------
#' Se manda llamar la función "estima_alumnos" para simular una variable
#' aleatoria discreta en el intervalo de las cotas arrojadas por dicha función.
#' La función simula_alumnos regresa un vector con el número de alumnos
#' simulados por cada hora para el siguiente semestre.
#' 
materia <- "Estadística I" ##Igual que "Inferencia Estadística"
(vec_alumnos_simulados <- simula_alumnos(materia,param))


# simula_tamano_grupo -----------------------------------------------------
#'  Función que simula el tamaño de los grupos por materia en cada hora
#'  dependiendo del número de alumnos que se han tenido.
#'
materia <- "Estadística I" ##Igual que "Inferencia Estadística"
(sim_tam_gpo_x_hora <- simula_tamano_grupo(materia,param))


# gen_mat_simula_grupos_una_materia --------------------------------------
#' Title gen_mat_simula_grupos_una_materia: Función que guarda la matriz 
#' "mat_simula_grupos_una_materia" por materia, la cual contiene 24 columnas:
#' Materia, Horario, Número de grupos simulados, Número de alumnos simulados,
#' las últimas 20 columnas indican el número de simulaciones del tamaño de
#' grupo, en sus renglones se tiene el número de alumnos de cada grupo
#' simulado por hora.
#' 
materia <- "Modelos de Supervivencia y de Series de Tiempo"
materia <- "Probabilidad I"
mat_simula_grupos_una_materia <- gen_mat_simula_grupos_una_materia(materia,param)
View(mat_simula_grupos_una_materia)


# guarda_mat_simula_grupos_1_sem ------------------------------------------------
#' Title guarda_mat_simula_grupos_1_sem: Función que guarda la matriz 
#' "mat_simula_grupos" la cual contiene 24 columnas: Materia, Horario,
#' Número de grupos simulados, Número de alumnos simulados, las últimas 20
#' columnas indican el número de simulaciones del tamaño de grupo, en sus
#' renglones se tiene el número de alumnos de cada grupo simulado.
#'
## **La función se tarda alrededor de 1hora, se puede cargar la matriz
##**directamente en caso de qeurer verla sin correr la función.
mat_simula_grupos <- guarda_mat_simula_grupos_1_sem(param)
# load("mat_simula_grupos_20202.RData")
View(mat_simula_grupos)


# simula_grupos -----------------------------------------------------------
#' Función que arroja el vector con el número de grupos simulados por hora,
#' que depende del número de alumnos que se estimaron con modelo hw()
#' Holt-Winters y se simularon.
#'
materia <- "Estadística I" ##Igual que "Inferencia Estadística"
(vec_grupos_simulados <- simula_grupos(materia,param))


# gen_esqueleto -----------------------------------------------------------
#' Función que genera una matriz llamada "mat_esqueleto", del semestre actual
#' (sem_fin). Arroja un error en caso de no encontrar algún archivo que
#' requiera. La matriz tiene 15 renglones con las horas (7-8,8-9,...,21-22)
#' y tantas columnas como materias se tengan. Contiene el número de grupos
#' simulados para cada materia en cada hora.
#'
sem_fin <- 20201 ##Para obtener las simulaciones del 20202
sem_fin <- 20192 ##Para obtener las simulaciones del 20201
n_semestres_anteriores <- 25 ##2008-1 - 2020-2
n_semestres_anteriores <- 24 ##2008-1 - 2020-1
n_semestres_anteriores <- 15 ##2013-1 - 2020-1
directorio_info <- rep(0,n_semestres_anteriores)
for (k in 1:n_semestres_anteriores) {
  sem_info <- param$Semestres[k]
  directorio_info[k] <- paste0("m_grande por semestre/m_grande_",sem_info,".RData")
}
mat_esqueleto <- gen_esqueleto(sem_fin,n_semestres_anteriores,directorio_info,param)
View(mat_esqueleto)


# extrae_mat_x_prof -------------------------------------------------------
#' Esta función regresa una matriz de 16xnum_sem que contiene los nombres de
#' las materias que ha dado cada profesor por semestre. Se tienen 16 renglones
#' porque se tienen 8 columnas con nombres por 2 materias
#' 
profesor <- "Margarita Elvira Chávez Cano"
materia_x_profesor <- extrae_mat_x_prof(profesor,param)
View(materia_x_profesor)


# simula_eleccion_materia ---------------------------------------------------------
#' Función que simula varias elecciones de materias de cada profesor. Arroja un
#' vector con los nombres de las materias simuladas.
#' 
profesor <- "Margarita Elvira Chávez Cano"
(materias_simuladas <- simula_eleccion_materia(profesor,param))


# horario_de_materia_x_prof ---------------------------------------------
#' Esta función regresa una matriz de 2xnum_sem que contiene los horarios de las
#' materias que ha dado cada profesor por semestre.
#' 
profesor <- "Margarita Elvira Chávez Cano"
horario_x_profesor <- horario_de_materia_x_prof(profesor,param)
View(horario_x_profesor)


# simula_eleccion_horario ---------------------------------------------------------
#' Función que simula varias elecciones de horarios de cada profesor. Arroja un
#' vector con los nombres de los horarios simulados por profesor.
#' 
profesor <- "Margarita Elvira Chávez Cano"
(horarios_simulados <- simula_eleccion_horario(profesor,param))


# gen_solicitudes ---------------------------------------------------------
#' Función que guarda, en una matriz, de 12 columnas, la información
#' de las solicitudes de materia y de horario de todos los profesores las
#' cuales se generan por medio de las simulaciones de dichas elecciones
#' (materia y horario). En las primeras 6 columnas se tiene la información
#' de la simulación de elección de materias y en las últimas 6 columnas
#' se tiene la información de la simulación de elección de horarios,
#' la matriz puede no estar completamente llena),tiene como renglones
#' los nombres de los profesores.
#'
mat_solicitudes <- gen_solicitudes(param)
View(mat_solicitudes)


# asigna_una_mat_prof_hora ---------------------------------------------------
#' Función que genera la asignación de una  materia con profesor por hora,
#' dependiendo del número de grupos simulados para el siguiente semestre.
#' Arroja una matriz de 4 columnas (Materia,Profesor,Horario,Salón), la cual
#' tiene la información de las asignaciones generadas de las simulaciones tanto
#' del número de grupos como de las elecciones de los profesores.
#'
mat_solicitudes <- gen_solicitudes(param)
materia <- "Estadística I" ##Igual que "Inferencia Estadística"
mat_asignacion <- asigna_una_mat_prof_hora(mat_solicitudes,materia,param)
View(mat_asignacion)



# gen_asignacion ----------------------------------------------------------
#' Función que genera asignaciones de materia con profesor por hora,
#' dependiendo del número de grupos simulados para el siguiente semestre,
#' con la información de solicitudes que se obtiene de la función
#' "gen_solicitudes". Arroja una matriz de cuatro columnas (Materia, Profesor,
#' Horario, Salón) la cual contiene en el i-ésimo renglón la asignación
#' por materia, profesor y horario //está pendiente la asignación de salón//
#'
sem_fin <- 20201
n_semestres_anteriores <- 25
directorio_info <- rep(0,n_semestres_anteriores)
for (k in 1:25) {
  sem_info <- param$Semestres[k]
  directorio_info[k] <- paste0("m_grande por semestre/m_grande_",sem_info,".RData")
}
mat_esqueleto <- gen_esqueleto(sem_fin,n_semestres_anteriores,directorio_info,param)
mat_solicitudes <- gen_solicitudes(param)
mat_asignaciones <- gen_asignacion(mat_esqueleto,mat_solicitudes,param)
View(mat_asignaciones)



# gen_asignacion_completa -------------------------------------------------
#' Función que genera la asignación completa, tiene como parámetros el semestre
#' inicial y el final. Arroja la matriz "mat_asignaciones" con las asignaciones
#' de materia-profesor-horario(-salón)
#'
sem_ini <- 20081
sem_fin <- 20201
mat_asignaciones <- gen_asignacion_completa(sem_ini,sem_fin)
View(mat_asignaciones)


# arroja_error_espacio_en_mat ---------------------------------------------
#' Función que arroja una variable binaria la cual vale 1 si el número de
#' columnas para guardar la información es menor al número máximo de grupos
#' reales (i.e. si hay un error), vale 0 si no hay error. Revisa si hay
#' espacio suficiente para guardar la información de los grupos reales.
#'
sem_info <- 20152
arroja_error_espacio_en_mat(sem_info)


# heatmap -----------------------------------------------------------------
## Se carga la matriz que se va a utilizar como ejemplo
load("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Programas/Prog V05/mat_varianza_gpos_sim.RData")
dim(mat_varianza_gpos_sim)
mat_heat <- matrix(0,nrow = dim(mat_varianza_gpos_sim)[1],ncol = 20)

## SE convierten los datos en valores de tipo numérico
for(d in 4:23){
  mat_heat[,d-3] <- as.numeric(mat_varianza_gpos_sim[,d])
}

# df_heat <- as.data.frame()
# mat_heat <- as.numeric(df_heat)
View(mat_heat)
colnames(mat_heat) <- c("Var(Gpo_1)","Var(Gpo_2)","Var(Gpo_3)","Var(Gpo_4)","Var(Gpo_5)",
                        "Var(Gpo_6)","Var(Gpo_7)","Var(Gpo_8)","Var(Gpo_9)","Var(Gpo_10)",
                        "Var(Gpo_11)","Var(Gpo_12)","Var(Gpo_13)","Var(Gpo_14)",
                        "Var(Gpo_15)","Var(Gpo_16)","Var(Gpo_17)","Var(Gpo_18)",
                        "Var(Gpo_19)","Var(Gpo_20)")
rownames(mat_heat) <- c("2013-2","2014-1","2014-2","2015-1","2015-2","2016-1","2016-2",
                        "2017-1","2017-2","2018-1","2018-2","2019-1","2019-2","2020-1")

# Most basic Heatmap:
# How to do it: below is the most basic heatmap you can build in base R, using
# the heatmap() function with no parameters. Note that it takes as input a
# matrix. If you have a data frame, you can convert it to a matrix with
# as.matrix(), but you need numeric variables only.
#
# How to read it: each column is a variable. Each observation is a row. Each
# square is a value, the closer to yellow the higher. You can transpose the
# matrix with t(data) to swap X and Y axis.

# Default Heatmap
heatmap(mat_heat)

# Note: as you can see this heatmap is not very insightful: all the variation
# is absorbed by the hp and disp variables that have very high values compared
# to the others. We need to normalize the data, as explained in the next section.


# Normalization:
# Normalizing the matrix is done using the scale argument of the heatmap()
# function. It can be applied to row or to column. Here the column option is
# chosen, since we need to absorb the variation between column.

# Use 'scale' to normalize
heatmap(mat_heat, scale="column")


# Dendrogram and Reordering:
# You may have noticed that order of both rows and columns is different
# compare to the native mtcar matrix. This is because heatmap() reorders both
# variables and observations using a clustering algorithm: it computes the
# distance between each pair of rows and columns and try to order them by
# similarity.
# Moreover, the corresponding dendrograms are provided beside the heatmap.
# We can avoid it and just visualize the raw matrix: use the Rowv and Colv
# arguments as follow.

# No dendrogram nor reordering for neither column or row
heatmap(mat_heat, Colv = NA, Rowv = NA, scale="column")


# Color palette:
# There are several ways to custom the color palette:
# a) use the native palettes of R: terrain.color(), rainbow(), heat.colors(),
#topo.colors() or cm.colors()
# b) use the palettes proposed by RColorBrewer. See list of available palettes:
# https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html

# 1: native palette from R
heatmap(mat_heat, scale="column", col = cm.colors(256)) ##Azules/Morados
heatmap(mat_heat, scale="column", col = terrain.colors(256)) ##Verdes/Rosas
# 2: Rcolorbrewer palette
# library(RColorBrewer)
coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25) ##Morados/Verdes
heatmap(mat_heat, scale="column", col = coul)


# Custom Layout:
# You can custom title & axis titles with the usual main and xlab/ylab
#arguments (left). You can also change labels with labRow/colRow and their
#size with cexRow/cexCol.

# Add classic arguments like main title and axis title
heatmap(mat_heat, Colv = NA, Rowv = NA, scale="column", col = coul, xlab="variable", ylab="car", main="heatmap")
# Custom x and y labels with cexRow and labRow (col respectively)
heatmap(mat_heat, scale="column", cexRow=1.5, labRow=paste("new_", rownames(data),sep=""), col= colorRampPalette(brewer.pal(8, "Blues"))(25))


# Add color beside heatmap:
# Often, heatmap intends to compare the observed structure with an expected
#one. You can add a vector of color beside the heatmap to represents the
#expected structure using the RowSideColors argument.

# Example: grouping from the first letter:
# my_group <- as.numeric(as.factor(substr(rownames(mat_heat), 1 , 1)))
# my_group <- as.numeric(as.factor(substr(colnames(mat_heat), 1 , 1)))
my_group <- as.numeric(as.factor(sample(1:10,size = length(rownames(mat_heat)),replace = T)))
colSide <- brewer.pal(9, "Set1")[my_group]
colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
heatmap(mat_heat, Colv = NA, Rowv = NA, scale="column" , RowSideColors=colSide, col=colMain   )


# gen_mat_real_grupos_una_materia ----------------------------------------
#' Title gen_mat_real_grupos_una_materia: Función que guarda la matriz 
#' "mat_real_grupos_una_materia" la cual contiene 24 columnas: Materia,
#' Horario, Número de grupos reales, Número de alumnos reales, las últimas
#' 20 columnas indican el número de simulaciones del tamaño de grupo, en sus
#' renglones se tiene el número real de alumnos de cada grupo por hora.
#' Fijando materia y semestre
#'
materia <- "Modelos de Supervivencia y de Series de Tiempo"
sem_info <- 20201
mat_real_grupos_una_materia <- gen_mat_real_grupos_una_materia(materia,sem_info,param)
View(mat_real_grupos_una_materia)

# guarda_mat_real_grupos_x_sem --------------------------------------------------
#' Title guarda_mat_real_grupos_x_sem: Función que guarda la matriz 
#' "mat_real_grupos" la cual contiene 24 columnas: Materia, Horario,
#' Número de grupos reales, Número de alumnos reales, las últimas 20
#' columnas indican el número de simulaciones del tamaño de grupo, en sus
#' renglones se tiene el número real de alumnos de cada grupo por hora.
#'
sem_info <- 20201
mat_real_grupos <- guarda_mat_real_grupos_x_sem(sem_info,param)
View(mat_real_grupos)


# datos_num_max_de_gpos_sim --------------------------------------------------
#' Función que regresa un vector con los valores del número máximo de grupos
#' simulados por semestre considerando que están dividos por materia y horario.
#'
sem_ini <- 20132
sem_fin <- 20202
datos_num_max_de_gpos_sim(sem_ini,sem_fin)


# datos_num_max_de_gpos_real --------------------------------------------------
#' Función que regresa un vector con los valores del número máximo de grupos
#' reales por semestre considerando que están dividos por materia y horario.
#'
sem_ini <- 20081
sem_fin <- 20201
datos_num_max_de_gpos_real(sem_ini,sem_fin)


# guarda_mat_diferencias --------------------------------------------------
#' Función en la que se obtienen todas las matrices "mat_diferencias" para
#' cada semestre desde "sem_ini" hasta "sem_fin".
#' 
sem_ini <- 20132
sem_fin <- 20201
guarda_mat_diferencias(sem_ini,sem_fin)


# guarda_mat_esp_dif ------------------------------------------------------
#' Title guarda_mat_esp_dif: Función que guarda la matriz "mat_esp_dif" la cual
#' tiene la esperanza de los datos de los valores de la matriz de diferencias.
sem_ini <- 20132
sem_fin <- 20201
guarda_mat_esp_dif(sem_ini,sem_fin)

# guarda_mat_var_dif ------------------------------------------------------
#' Title guarda_mat_var_dif: Función que guarda la matriz "mat_var_dif" la
#' cual tiene la varianza de los datos de los valores de la matriz de
#' diferencias.
#' 
sem_ini <- 20132
sem_fin <- 20201
guarda_mat_var_dif(sem_ini,sem_fin)


# guarda_mat_sd_dif ------------------------------------------------------
#' Función que guarda la matriz "mat_sd_dif" la cual tiene la desviación
#' estándar de los datos de los valores de la matriz de diferencias.
#' 
sem_ini <- 20132
sem_fin <- 20201
guarda_mat_sd_dif(sem_ini,sem_fin)


# guarda_mat_esp_sim ------------------------------------------------------
#' Función que guarda la matriz "mat_esp_sim" la cual tiene la esperanza de
#' los datos de los valores de la matriz de datos simulados.
#' 
sem_ini <- 20132
sem_fin <- 20201
guarda_mat_esp_sim(sem_ini,sem_fin)


# guarda_mat_var_sim ------------------------------------------------------
#' Función que guarda la matriz "mat_var_sim" la cual tiene la varianza de
#' los datos de los valores de la matriz de datos simulados.
#' 
sem_ini <- 20132
sem_fin <- 20201
guarda_mat_var_sim(sem_ini,sem_fin)


# guarda_mat_sd_sim ------------------------------------------------------
#' Función que guarda la matriz "mat_sd_sim" la cual tiene la desviación
#' estándar de los datos de los valores de la matriz de datos simulados.
#' 
sem_ini <- 20132
sem_fin <- 20201
guarda_mat_sd_sim(sem_ini,sem_fin)


# gen_mat_n_sim_1_materia --------------------------------------------------
#' Title gen_mat_n_sim_1_materia: Función que hace "n" simulaciones fijando un
#' semestre y una materia; arroja una matriz con el número de alumnos por
#' grupo ordenados de mayor a menor por cada hora. Las "n" matrices generadas
#' se guardan en una lista llamada "lista_mat_n_sim". Cada matriz generada
#' tiene 24 columnas: Materia, Horario, Número total de grupos simulados,
#' Número total de alumos simulados, las siguientes 20 columnas contienen
#' el número de alumnos simulados, ordenados de mayor a menor. Las matrices
#' generadas se guardan en una lista llamada "lista_mat_n_sim".
#'
load("vec_nom_materias_total.RData")
Materias <- vec_nom_materias_total
materia <- "Modelos de Supervivencia y de Series de Tiempo"
num_materia <- 285
num_sim <- 10

# param$sem_ini = 20081##Inicio de información real
# param$sem_fin = 20192##Fin de información real
# param$sem_sig = 20201##Semestre de simulación
# param$sem_fin = 20201##Fin de información real
# param$sem_sig = 20202##Semestre de simulación
# param$sem_totales <- (20081:param$sem_sig)[(20081:param$sem_sig)%% 10>0 
#                                            &(20081:param$sem_sig)%% 10<3]
# param$Semestres = (param$sem_ini:param$sem_fin)[(param$sem_ini:param$sem_fin) 
#                                                 %% 10>0 &(param$sem_ini:param$sem_fin) %% 10<3]
# param$nombre_sem = as.character(param$Semestres)
# param$n_semestres_anteriores = length(param$Semestres)

lista_mat_n_sim <- gen_mat_n_sim_1_materia(materia,num_materia,
                                           num_sim,param)

# gen_mat_n_sim_1_sem -----------------------------------------------------------
#' Title gen_mat_n_sim_1_sem: Función que arroja una lista con las
#' listas de matrices obtenidas en "gen_mat_n_sim_1_materia" para "sem_sig".
#'
n_semestres_anteriores <- 10
sem_sig <- 20202
num_sim <- 10

lista_n_sim_por_sem <- gen_mat_n_sim_1_sem(n_semestres_anteriores,sem_sig,
                                           num_sim,param)


# gen_mat_n_sim_n_sem -----------------------------------------------------------
#' Title gen_mat_n_sim_n_sem: Función que manda a llamar a la función
#' "gen_mat_n_sim_1_sem" para cada semestre de un vector dado. Se genera
#' una lista de listas para cada semestre en "vec_sem_sig". Esta función se
#' utiliza principalmente para hacer pruebas de que el modelo funciona.
#'
# Ej.1
num_sim <- 10
sem_ini <- 20182
sem_fin <- 20201
vec_sem_sig <- (sem_ini:sem_fin)[(sem_ini:sem_fin)%% 10>0 
                                 &(sem_ini:sem_fin)%% 10<3]
vec_sem_ant <- c(10,11,12,13)##Se inicia en 2013-1
vec_sem_ant <- c(7,8,9,10)##Se inicia en 2015-1

# Ej.2
num_sim <- 10
vec_sem_sig <- c(20131,20152,20182,20201)
vec_sem_ant <- c(10,15,21,24)##Se inicia en 2008-1

# Ej.3
num_sim <- 10
vec_sem_sig <- c(20131,20152,20182,20201)
vec_sem_ant <- rep(5,length(vec_sem_sig)) #Se toman los 5 semestres anteriores


# gen_mat_esp_alum_x_materia_1_sem ------------------------------------------
#' Title gen_mat_esp_alum_x_materia_1_sem: Función que genera una matriz con
#' 20 columnas que contiene la esperanza por cada entrada de las "n" matrices
#' generadas en la función "gen_mat_n_sim_1_sem".
#'
mat_esp_alum_x_materia_1_sem <- gen_mat_esp_alum_x_materia_1_sem(lista_mat_n_sim,param)
View(mat_esp_alum_x_materia_1_sem)


# gen_mat_var_alum_x_materia_1_sem ------------------------------------------
#' Title gen_mat_var_alum_x_materia_1_sem: Función que genera una matriz con
#' 20 columnas que contiene la esperanza por cada entrada de las "n" matrices
#' generadas en la función "gen_mat_n_sim_1_sem".
#'
mat_var_alum_x_materia_1_sem <- gen_mat_var_alum_x_materia_1_sem(lista_mat_n_sim,param)
View(mat_var_alum_x_materia_1_sem)
colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
heatmap(mat_var_alum_x_materia_1_sem, Colv = NA, Rowv = NA, scale="none",col=colMain)


# gen_mat_dif_alum_x_materia_1_sem ----------------------------------------------------
#' Title gen_mat_dif_alum_x_materia_1_sem: Función que genera una matriz con 20 columnas
#' y 15 renglones que contiene la diferencia entre los valores reales menos
#' la esperanza, para cada entrada, fijando materia y semestre.
#' 
mat_dif_alum_x_materia_1_sem <- gen_mat_dif_alum_x_materia_1_sem(mat_esp_alum_x_materia_1_sem,mat_real_grupos_una_materia)
View(mat_dif_alum_x_materia_1_sem)
colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
heatmap(mat_dif_alum_x_materia_1_sem, Colv = NA, Rowv = NA, scale="none",col=colMain,main = "Modelos de Supervivencia y de Series de Tiempo")


# guarda_una_fig_heatmap --------------------------------------------------
#' Title guarda_una_fig_heatmap: Función que guarda una imagen de tipo "jpeg"
#' de las gráficas de heatmap para las matrices de datos generadas.
#'
matriz <- mat_dif_alum_x_materia_1_sem
Materias <- unique(param$m_grande_total[,num_col_Materia])
num_materia <- 5
sem_siguiente <- 20201
nom_archivo <- paste0("Figuras/Matrices Simuladas/PRUEBA_heatmap_dif_",
                      num_materia,"_sem_",sem_siguiente,".jpeg")
guarda_una_fig_heatmap(matriz,Materias,num_materia,nom_archivo)


# guarda_fig_heatmap -----------------------------------------------------
#' Title guarda_fig_heatmap: Función que guarda todas las imágenes de tipo
#' "jpeg" de las gráficas heatmap para las matrices de datos generadas.
#'
param$sem_siguiente <- 20201
num_sim <- 10
guarda_fig_heatmap(num_sim,param)


# gen_mat_max_num_gpos_real -----------------------------------------------
#' Title gen_mat_max_num_gpos_real: Función que guarda y genera la matriz
#' "mat_max_num_gpos_real" que tiene 4 columnas (Semestre, Materia, Horario, 
#' Número de grupos) que contiene la información del máximo número de
#' grupos reales por semestre y por hora.
#'
gen_mat_max_num_gpos_real(param)


# gen_mat_max_num_gpos_sim ------------------------------------------------
#' Title gen_mat_max_num_gpos_sim: Función que guarda y genera la matriz
#' "mat_max_num_gpos_sim" que tiene 4 columnas (Semestre, Materia, Horario, 
#' Número de grupos) que contiene la información del máximo número de
#' grupos simulados por semestre y por hora.
#'
sem_ini <- 20131
sem_fin <- 20201
mat_max_num_gpos_sim <- gen_mat_max_num_gpos_sim(sem_ini,sem_fin,param)


# gen_mat_dif_total_alumnos_x_sem -----------------------------------------
#' Title gen_mat_dif_total_alumnos_x_sem: Función que genera una matriz con 15
#' columnas y 15 renglones, llamada "mat_dif_total_alumnos_x_sem". La matriz
#' contiene la diferencia entre los valores del total de alumnos reales
#' menos los simulados para cada hora, fijando el semestre.
#' Columnas: Semestres simulados, Renglones: Horas 7-8,8-9,...,20-21,21-22
sem_ini <- 20131
sem_fin <- 20201

mat_dif_total_alumnos_x_sem <- gen_mat_dif_total_alumnos_x_sem(sem_ini,sem_fin,param)
View(mat_dif_total_alumnos_x_sem)

## Para guardar gráficas de R como imagen .jpeg se manda llamar la función
## jpeg() antes de generar una gráfica. Al hacer esto, le indicamos a R
##que en lugar de mandar nuestro gráfico a una ventana del escritorio, lo
##mande a un dispositivo gráfico distinto.
## La función dev.off(), se utiliza para cerrar el dispositivo gráfico
##elegido y así poder crear más gráficos después.
nom_archivo <- paste0("Figuras/Matrices Simuladas/heatmap_dif_total_de_alumnos_x_sem.jpeg")
# nom_archivo <- "Diferencia Total de alumnos_x_sem"
jpeg(filename = nom_archivo, width = 800, height = 700)
colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
heatmap(mat_dif_total_alumnos_x_sem, Colv = NA, Rowv = NA, scale="none",
        col=colMain,main = "Diferencia Total de alumnos_x_sem")
dev.off()


# gen_mat_dif_total_gpos_x_sem -----------------------------------------
#' Title gen_mat_dif_total_gpos_x_sem: Función que genera una matriz con 15
#' columnas y 15 renglones, llamada "mat_dif_total_gpos_x_sem". La matriz
#' contiene la diferencia entre los valores del total de grupos reales
#' menos los simulados para cada hora, fijando el semestre.
#' Columnas: Semestres simulados, Renglones: Horas 7-8,8-9,...,20-21,21-22
#' 
sem_ini <- 20131
sem_fin <- 20201

mat_dif_total_gpos_x_sem <- gen_mat_dif_total_gpos_x_sem(sem_ini,sem_fin,param)
View(mat_dif_total_gpos_x_sem)

## Para guardar gráficas de R como imagen .jpeg se manda llamar la función
## jpeg() antes de generar una gráfica. Al hacer esto, le indicamos a R
##que en lugar de mandar nuestro gráfico a una ventana del escritorio, lo
##mande a un dispositivo gráfico distinto.
## La función dev.off(), se utiliza para cerrar el dispositivo gráfico
##elegido y así poder crear más gráficos después.
nom_archivo <- paste0("Figuras/Matrices Simuladas/heatmap_dif_total_de_gpos_x_sem.jpeg")
# nom_archivo <- "Diferencia Total de gpos_x_sem"
jpeg(filename = nom_archivo, width = 800, height = 700)
colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
heatmap(mat_dif_total_gpos_x_sem, Colv = NA, Rowv = NA, scale="none",
        col=colMain,main = "Diferencia Total de gpos_x_sem")
dev.off()


# gen_mat_nom_cap_salon_x_sem ---------------------------------------------
#' Title gen_mat_nom_cap_salon_x_sem: Función que genera la matriz llamada
#' "mat_nom_cap_salon" que contiene en su primer columna los nombres de los
#' salones de la facultad y en las siguientes columas se tienen todas sus
#' capacidades por semestre, desde el 2013-1 hasta el 20201.
#' 
mat_nom_cap_salon <- gen_mat_nom_cap_salon_x_sem()
View(mat_nom_cap_salon)


# selecciona_cap_salon ----------------------------------------------------
#' Title selecciona_cap_salon: Función que arroja una matriz de dos
#' columnas, en la primera contiene los nombres de los salones de la
#' facultad y en la segunda la capacidad máxima de cada salón de
#' "mat_nom_cap_salon".
#' 
mat_cap_salon <- selecciona_cap_salon(mat_nom_cap_salon)
View(mat_cap_salon)


# gen_mat_dif_relativas ---------------------------------------------------
#' Title gen_mat_dif_relativas: Función que recibe dos matrices, una
#' matriz de diferencias absolutas y una matriz de valores reales, arroja
#' una matriz con las diferencias relativas correspondientes y guarda la
#' gráfica "heatmap" de ella.
#'
sem_ini <- 20131
sem_fin <- 20201
mat_dif_total_alumnos_x_sem <- gen_mat_dif_total_alumnos_x_sem(sem_ini,sem_fin,param)
# View(mat_dif_total_alumnos_x_sem)
mat_dif_total_gpos_x_sem <- gen_mat_dif_total_gpos_x_sem(sem_ini,sem_fin,param)
# View(mat_dif_total_gpos_x_sem)

#Se definen las matrices de valores reales
intervalo_sem <- (sem_ini:sem_fin)[(sem_ini:sem_fin)%% 10>0 &(sem_ini:sem_fin)%% 10<3]
mat_aux_real_alum <- matrix(0,nrow = 15,ncol = length(intervalo_sem))
mat_aux_real_gpos <- matrix(0,nrow = 15,ncol = length(intervalo_sem))
nom_col_real_alum <- "Alumnos_Reales_Totales"
nom_col_real_gpos <- "Grupos_Reales"

##Se obtienen las matrices que contienen los datos de alumnos totales
##reales por semestre
for(s in 1:length(intervalo_sem)){#Recorre los semestres
  nom_archivo_real <- paste0("mat_real_grupos por semestre/mat_real_grupos_",
                             intervalo_sem[s],".RData")
  load(nom_archivo_real)
  mat_aux_real_alum[,s] <- gen_vec_suma_datos_real_1_sem(mat_real_grupos,nom_col_real_alum,param)
  mat_aux_real_gpos[,s] <- gen_vec_suma_datos_real_1_sem(mat_real_grupos,nom_col_real_gpos,param)
  
}##Fin for s
rownames(mat_aux_real_alum) <- param$nombre_hrs
colnames(mat_aux_real_alum) <- intervalo_sem
rownames(mat_aux_real_gpos) <- param$nombre_hrs
colnames(mat_aux_real_gpos) <- intervalo_sem
# View(mat_aux_real_alum)
# View(mat_aux_real_gpos)

nom_archivo <- "dif_relativa_total_de_alumnos_x_sem"
mat_dif_relativas_alum <- gen_mat_dif_relativas(mat_dif_total_alumnos_x_sem,mat_aux_real_alum,nom_archivo)
View(mat_dif_relativas_alum)

nom_archivo <- "dif_relativa_total_de_gpos_x_sem"
mat_dif_relativas_gpos <- gen_mat_dif_relativas(mat_dif_total_gpos_x_sem,mat_aux_real_gpos,nom_archivo)
View(mat_dif_relativas_gpos)


# gen_mat_materias_rep_1_sem ----------------------------------------------
#' Title gen_mat_materias_rep_1_sem: Función que recibe como parámetros
#' "sem_info" y "profesor", arroja una matriz de cuatro columnas (semestre,
#' profesor, materia, hora) en caso de que "profesor" tenga más de una
#' materia asignada a la misma hora.
#'
sem_info <- 20152
# sem_info <- semestres[s]
profesor <- "Arrigo Coen Coria"
profesor <- "Hugo Arizmendi Peimbert"
# profesor <- Profesores[p]
mat_materias_rep_1_sem <- gen_mat_materias_rep_1_sem(sem_info,profesor)
View(mat_materias_rep_1_sem)


# gen_mat_materias_rep ----------------------------------------------------
#' Title gen_mat_materias_rep: Función que recibe como parámetros
#' "sem_ini" y "sem_fin", arroja una matriz de cuatro columnas (semestre,
#' profesor, materia, hora) en caso de que "profesor" tenga más de una
#' materia asignada a la misma hora, para todos los profesores en el
#' intervalo de semestres de "sem_ini" hasta "sem_fin".
#'
mat_materias_rep <- gen_mat_materias_rep(param)
View(mat_materias_rep)
# save(mat_materias_rep, file = "mat_materias_rep_20081_20192.RData")
# save(mat_materias_rep, file = "mat_materias_rep_20081_20201.RData")


# gen_vec_nom_materias_total ----------------------------------------------
#' Title gen_vec_nom_materias_total: Función que carga la matriz
#' "m_grande_total" de los semestres 2008-1 a 2020-1 de la cual se va
#' obtiene la lista de nombres de las materias sin repetición, conservando
#' los nombres más recientes de las materias.
#'
vec_nom_materias_total <- gen_vec_nom_materias_total()
# length(vec_nom_materias_total) ## 343 //V02
# length(vec_nom_materias_total) ## 248 //V03
# length(vec_nom_materias_total) ## 349 //V04
length(vec_nom_materias_total) ## 346
View(vec_nom_materias_total)


# checa_nom_1_materia_en_vec ----------------------------------------------
#' Title checa_nom_1_materia_en_vec: Función que recibe como parámetros el
#' nombre de una materia y el vector con los nombres de las materias e
#' imprime una lista con los diferentes nombres que pudiera tener "materia"
#' en "vec_nom_materias_total"
#' Por ejemplo "Estadística III" y "Modelos de Supervivencia y de Series
#' de Tiempo".
#'
load("vec_nom_materias_total.RData")
View(vec_nom_materias_total)
materia <- "Modelos no Paramétricos y de Regresión"
materia <- "Estadística III"
materia <- "Modelos de Supervivencia y de Series de Tiempo"
materia <- "Ingeniería de Software/Ingeniería de Software/Ingeniería de Software
/Ingeniería de Software/Ingeniería de Software/Ingeniería de Software"
checa_nom_1_materia_en_vec(materia,vec_nom_materias_total)

## Ejemplo con una muestra aleatoria de materias
#' Se carga la matriz m_grande_total de 2008-1 a 2020-1 de la cual
#' se va a obtener la lista de nombres que se desea
load("Matrices m_grande_total/m_grande_total_20081_20201.RData")
#Se definen las variables que se van a utilizar
num_col_materia <- arroja_ind_col_MG("Materia")##1
vec_materias <- unique(m_grande_total[,num_col_materia])##531
vec_ej <- sample(vec_materias,5)
vec_ej <- sample(vec_materias,10)
for(d in 1:length(vec_ej)){
  checa_nom_1_materia_en_vec(vec_ej[d],vec_nom_materias_total)
}


# checa_nom_materias_en_vec -----------------------------------------------
#' Title checa_nom_materias_en_vec: Función que recibe como parámetro el
#' vector con los nombres de las materias e imprime una lista con los
#' diferentes nombres que pudiera tener cada "materia" en "vec_nom_materias_total"
#' Por ejemplo "Estadística III" y "Modelos de Supervivencia y de Series
#' de Tiempo".
#'
load("vec_nom_materias_total.RData")
View(vec_nom_materias_total)
checa_nom_materias_en_vec(vec_nom_materias_total)




# gen_m_grande -------------------------------------------------------
#' Title gen_m_grande: Función que recibe como parámetro el semestre
#' del que se desea obtener la información y genera un archivo de tipo
#' ".Rdata" con la matriz "m_grande" de dicho semestre. Regresa el
#' nombre de la ubicación en la que se encuentra guardado el archivo.
#' La matriz "m_grande" que se guarda está limpia y actualizada.
#'
vec_excepciones <- "Inglés"
sem_info <- 20182
m_grande <- gen_m_grande(sem_info,vec_excepciones,param)

## Todos los semestres
# semestres <- param$Semestres
for(d in 1:length(param$nombre_sem)){
  sem_info <- param$nombre_sem[d]
  m_grande <- gen_m_grande(sem_info,vec_excepciones,param)
  # View(m_grande)
}

# gen_m_grande_total ------------------------------------------------------
#' Title gen_m_grande_total: Función que genera la matriz "m_grande_total"
#' para un intervalo dado.
#'
# param$sem_ini = 20081##Inicio de información real
# param$sem_fin = 20201##Fin de información real
# param$sem_sig = 20202##Semestre de simulación
# param$sem_totales <- (20081:param$sem_sig)[(20081:param$sem_sig)%% 10>0 
#                                            &(20081:param$sem_sig)%% 10<3]
# param$Semestres = (param$sem_ini:param$sem_fin)[(param$sem_ini:param$sem_fin) 
#                                                 %% 10>0 &(param$sem_ini:param$sem_fin) %% 10<3]
# param$nombre_sem = as.character(param$Semestres)
# param$n_semestres_anteriores = length(param$Semestres)

param$sem_ini = 20081##Inicio de información real
param$sem_fin = 20192##Fin de información real
param$sem_sig = 20201##Semestre de simulación
param$sem_totales <- (20081:param$sem_sig)[(20081:param$sem_sig)%% 10>0 
                                           &(20081:param$sem_sig)%% 10<3]
param$Semestres = (param$sem_ini:param$sem_fin)[(param$sem_ini:param$sem_fin) 
                                                %% 10>0 &(param$sem_ini:param$sem_fin) %% 10<3]
param$nombre_sem = as.character(param$Semestres)
param$n_semestres_anteriores = length(param$Semestres)

vec_excepciones <- "Inglés"
m_grande_total <- gen_m_grande_total(vec_excepciones,param)
View(m_grande_total)

## Todos los semestres
semestres <- param$sem_totales
for(d in length(semestres):11){
  n_semestres_anteriores <- d-1
  param$Semestres = semestres[1:n_semestres_anteriores]
  param$sem_fin = semestres[d-1]
  param$sem_sig = semestres[d]
  m_grande_total <- gen_m_grande_total(vec_excepciones,param)
  # View(m_grande_total)
}


# arroja_ind_col_SG -------------------------------------------------------
#' Función que recibe el nombre de la columna que se busca y devuelve el
#' número de columna en "mat_simula_grupos" con ese nombre.
#'
arroja_ind_col_SG("Materia") ##1
arroja_ind_col_SG("Horario") ##2
arroja_ind_col_SG("Grupos_Simulados") ##3
arroja_ind_col_SG("Alumnos_Simulados_Totales") ##4
arroja_ind_col_SG("col_1er_grupo") ##5
arroja_ind_col_SG("col_ult_grupo") ##24


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
arroja_ind_col_RG("Materia") ##1
arroja_ind_col_RG("Horario") ##2
arroja_ind_col_RG("Grupos_Reales") ##3
arroja_ind_col_RG("Alumnos_Reales_Totales") ##4
arroja_ind_col_RG("col_1er_grupo") ##5
arroja_ind_col_RG("col_ult_grupo") ##24


# arroja_ind_col_MG -------------------------------------------------------
#' Title arroja_ind_col_MG: Función que recibe el nombre de la columna que
#' se busca y devuelve el número de columna en "m_grande" con ese nombre.
#'
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
num_col_Num_materia <- arroja_ind_col_MG("Num_materia")##37

# checa_ind_materia -------------------------------------------------------
#' Title checa_ind_materia: Función que revisa en qué índices de la matriz
#' "m_grande_total" o "m_grande" coincide "materia" con los nombres que se
#' encuentran en las columnas:
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
load("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Programas/Prog V05/m_grande_total_20081_20201_V01.RData")
View(m_grande_total)
materia <- "Modelos no Paramétricos y de Regresión"
checa_ind_materia(materia,m_grande_total)


# gen_vec_1_sem_k_info ----------------------------------------------------
#' Title gen_vec_1_sem_k_info: Función que genera el vector
#' "vec_1_sem_k_info" con los "k_sem_ant" semestres de los que se quiere
#' obtener la información para realizar la simulación de "sem_sig".
#'
sem_sig <- "20182"
k_sem_ant <- 10
gen_vec_1_sem_k_info(sem_sig,k_sem_ant,param)

sem_sig <- "20201"
k_sem_ant <- 5
gen_vec_1_sem_k_info(sem_sig,k_sem_ant,param)


# gen_vec_s_sem_k_info ----------------------------------------------------
#' Title gen_vec_s_sem_k_info: Función que genera el vector
#' "vec_s_sem_k_info" con los "k_sem_ant" semestres de los que se quiere
#' obtener la información para realizar la simulación de los semestres en
#' el vector "vec_sem_sig".
#'
# Start the clock!
ptm <- proc.time()
vec_sem_sig <- c(20181,20182,20191)
k_sem_ant <- 8
gen_vec_s_sem_k_info(vec_sem_sig,k_sem_ant,param)
cat("La función gen_vec_s_sem_k_info tardó: ",(proc.time()-ptm)[3]," segundos\n")
######### 0.03 seg

# Start the clock!
ptm <- proc.time()
vec_sem_sig <- c(20191,20192,20201)
k_sem_ant <- 5
gen_vec_s_sem_k_info(vec_sem_sig,k_sem_ant,param)
cat("La función gen_vec_s_sem_k_info tardó: ",(proc.time()-ptm)[3]," segundos\n")
######### 0.05 seg


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
param_sim$vec_sem_sig = c(20181,20182,20191)
param_sim$k_sem_ant = 5
Materias <- c("Estadística III","Seminario de Ciencias Computacionales",
              "Cálculo Diferencial e Integral I","Mecánica Cuántica",
              "Teoría del Seguro")
# Start the clock!
ptm <- proc.time()
m_filtrada <- gen_mat_m_filtrada(Materias,param,param_sim)
View(m_filtrada)
cat("La función gen_mat_m_filtrada tardó: ",(proc.time()-ptm)[3]," segundos\n")
######### 1.58 seg
# 5 materias: "Estadística III","Seminario de Ciencias Computacionales",
#"Cálculo Diferencial e Integral I","Mecánica Cuántica", "Teoría del Seguro"
# 3 semestres: 20181, 20182, 20191
# m_filtrada tiene 263 renglones
# 5 semestres de información


param_sim$vec_sem_sig = c(20181,20182,20191)
param_sim$k_sem_ant = 5
Materias <- c("Estadística III","Seminario de Ciencias Computacionales",
              "Cálculo Diferencial e Integral I","Mecánica Cuántica",
              "Teoría del Seguro")
# Start the clock!
ptm <- proc.time()
m_filtrada <- gen_mat_m_filtrada(Materias,param,param_sim)
View(m_filtrada)
cat("La función gen_mat_m_filtrada tardó: ",(proc.time()-ptm)[3]," segundos\n")
######### 2.13 seg
# 5 materias: "Estadística III","Seminario de Ciencias Computacionales",
#"Cálculo Diferencial e Integral I","Mecánica Cuántica", "Teoría del Seguro"
# 3 semestres: 20181, 20182, 20191
# m_filtrada tiene 263 renglones
# 5 semestres de información


param_sim$vec_sem_sig = c(20181,20182,20191)
param_sim$k_sem_ant = 5
Materias <- c("Inglés I","Ecuaciones Diferenciales II",
              "Cálculo Diferencial e Integral III","Inferencia Estadística",
              "Temas Selectos de Investigación de Operaciones")
# Start the clock!
ptm <- proc.time()
m_filtrada <- gen_mat_m_filtrada(Materias,param,param_sim)
View(m_filtrada)
cat("La función gen_mat_m_filtrada tardó: ",(proc.time()-ptm)[3]," segundos\n")
######### 1.61 seg
# 5 materias: "Inglés I","Ecuaciones Diferenciales II",
# "Cálculo Diferencial e Integral III","Inferencia Estadística",
# "Temas Selectos de Investigación de Operaciones"
# 3 semestres: 20181, 20182, 20191
# m_filtrada tiene 200 renglones
# 5 semestres de información


# gen_sub_m_filtrada ------------------------------------------------------
#' Title gen_sub_m_filtrada: Función que regresa la matriz 
#' "sub_m_filtrada" la cual es una submatriz de "m_filtrada" con la
#' información de "materia" y de los "k_sem_ant" de "sem_sig".
#' Si sem_sig = 20182 y k_sem_ant = 5 => sub_m_filtrada tendrá la información
#' de "materia" de los semestres 2016-1 al 2018-1.
#'
materia <- "Estadística III"
# k_sem_ant <- 5
sem_sig <- 20182
# Start the clock!
ptm <- proc.time()
sub_m_filtrada <- gen_sub_m_filtrada(materia,sem_sig,param,param_sim)
View(sub_m_filtrada)
cat("La función gen_sub_m_filtrada tardó: ",(proc.time()-ptm)[3]," segundos\n")
######### 0.64 seg
# 1 materias: "Estadística III"
# 1 semestres: 20182
# sub_m_filtrada tiene 51 renglones
# 5 semestres de información

materia <- "Seminario de Ciencias Computacionales"
k_sem_ant <- 5
sem_sig <- 20182
# Start the clock!
ptm <- proc.time()
sub_m_filtrada <- gen_sub_m_filtrada(materia,sem_sig,param,param_sim)
View(sub_m_filtrada)
cat("La función gen_sub_m_filtrada tardó: ",(proc.time()-ptm)[3]," segundos\n")
######### 0.2 seg
# 1 materias: "Seminario de Ciencias Computacionales"
# 1 semestres: 20182
# sub_m_filtrada tiene 2 renglones
# 5 semestres de información

materia <- "Cálculo Diferencial e Integral I"
sem_sig <- 20182
# Start the clock!
ptm <- proc.time()
sub_m_filtrada <- gen_sub_m_filtrada(materia,sem_sig,param,param_sim)
View(sub_m_filtrada)
cat("La función gen_sub_m_filtrada tardó: ",(proc.time()-ptm)[3]," segundos\n")
######### 0.63 seg
# 1 materias: "Cálculo Diferencial e Integral I"
# 1 semestres: 20182
# sub_m_filtrada tiene 165 renglones
# 5 semestres de información

materia <- "Mecánica Cuántica"
k_sem_ant <- 5
sem_sig <- 20182
# Start the clock!
ptm <- proc.time()
sub_m_filtrada <- gen_sub_m_filtrada(materia,sem_sig,param,param_sim)
View(sub_m_filtrada)
cat("La función gen_sub_m_filtrada tardó: ",(proc.time()-ptm)[3]," segundos\n")
######### 0.57 seg
# 5 materias: "Mecánica Cuántica"
# 1 semestres: 20182
# sub_m_filtrada tiene 48 renglones
# 5 semestres de información

materia <- "Teoría del Seguro"
k_sem_ant <- 10
sem_sig <- 20182
# Start the clock!
ptm <- proc.time()
sub_m_filtrada <- gen_sub_m_filtrada(materia,sem_sig,param,param_sim)
View(sub_m_filtrada)
cat("La función gen_sub_m_filtrada tardó: ",(proc.time()-ptm)[3]," segundos\n")
######### 1.22 seg
# 5 materias: "Teoría del Seguro"
# 1 semestres: 20182
# sub_m_filtrada tiene 124 renglones
# 10 semestres de información


materia <- "Estadística III"
sem_sig <- 20182
# Start the clock!
ptm <- proc.time()
sub_m_filtrada <- gen_sub_m_filtrada(materia,sem_sig,param,param_sim)
View(sub_m_filtrada)
cat("La función gen_sub_m_filtrada tardó: ",(proc.time()-ptm)[3]," segundos\n")
######### 0.08 seg
######### 0.11 seg
# 1 materias: "Estadística III"
# 1 semestres: 20182
# sub_m_filtrada tiene 32 renglones
# 5 semestres de información


materia <- "Teoría del Seguro"
k_sem_ant <- 10
sem_sig <- 20182
# Start the clock!
ptm <- proc.time()
sub_m_filtrada <- gen_sub_m_filtrada(materia,sem_sig,param,param_sim)
View(sub_m_filtrada)
cat("La función gen_sub_m_filtrada tardó: ",(proc.time()-ptm)[3]," segundos\n")
######### 0.03 seg
# 1 materias: "Teoría del Seguro"
# 1 semestres: 20182
# sub_m_filtrada tiene 32 renglones
# 10 semestres de información


# Start the clock!
ptm <- proc.time()
materia <- "Cálculo Diferencial e Integral I"
sem_sig <- 20182
sub_m_filtrada <- gen_sub_m_filtrada(materia,sem_sig,param,param_sim)
View(sub_m_filtrada)
cat("La función gen_sub_m_filtrada tardó: ",(proc.time()-ptm)[3]," segundos\n")
######### 0.1 seg
# 1 materia: "Cálculo Diferencial e Integral I"
# 1 semestres: 20182
# sub_m_filtrada tiene 89 renglones
# 5 semestres de información


# Start the clock!
ptm <- proc.time()
materia <- "Cálculo Diferencial e Integral I"
sem_sig <- 20182
param_sim$k_sem_ant = 10
sub_m_filtrada <- gen_sub_m_filtrada(materia,sem_sig,param,param_sim)
View(sub_m_filtrada)
cat("La función gen_sub_m_filtrada tardó: ",(proc.time()-ptm)[3]," segundos\n")
######### 0.11 seg
# 1 materia: "Cálculo Diferencial e Integral I"
# 1 semestres: 20182
# sub_m_filtrada tiene 89 renglones 
# 10 semestres de información ##Se hizo la prueba con m_filtrada de k_sem_ant = 5




# gen_mat_alumnos_corregidos ----------------------------------------------
#' Title gen_mat_alumnos_corregidos: Función que regresa la matriz 
#' "mat_alumnos_corregidos" de 15 renglones (horas) y k+s-1 columnas, la
#' cual tiene el número total de alumnos por hora y por semestre. Se suma
#' el número de alumnos en los semestres repetidos por cada hora. Hay
#' ceros en los smestres y horas en donde no hay información.
#'
vec_sem_sig <- c(20182,20191,20192,20201)
k_sem_ant <- 5
vec_s_sem_k_info <- gen_vec_s_sem_k_info(vec_sem_sig,k_sem_ant,param)
Materias <- c("Inglés I","Ecuaciones Diferenciales II",
              "Temas Selectos de Investigación de Operaciones")
m_filtrada <- gen_mat_m_filtrada(Materias,param,param_sim)
param_sim$m_filtrada = m_filtrada
param_sim$k_sem_ant = k_sem_ant
param_sim$vec_sem_sig = vec_sem_sig

# Start the clock!
ptm <- proc.time()
mat_alumnos_corregidos <- gen_mat_alumnos_corregidos(vec_s_sem_k_info,
                                                     param,param_sim)
View(mat_alumnos_corregidos)
cat("La función gen_mat_alumnos_corregidos tardó: ",(proc.time()-ptm)[3]," segundos\n")
######### 0.16 seg
# 3 materias: "Inglés I","Ecuaciones Diferenciales II",
#"Temas Selectos de Investigación de Operaciones"
# 4 semestres: 20181, 20182, 20191
# 5 semestres de información: 20161 20162 20171 20172 20181 20182 20191 20192
# m_filtrada tiene 75 renglones


vec_sem_sig <- c(20182,20191,20192,20201)
k_sem_ant <- 5
vec_s_sem_k_info <- gen_vec_s_sem_k_info(vec_sem_sig,k_sem_ant,param)
Materias <- c("Estadística III","Seminario de Ciencias Computacionales",
              "Cálculo Diferencial e Integral I","Mecánica Cuántica",
              "Teoría del Seguro")
m_filtrada <- gen_mat_m_filtrada(Materias,param,param_sim)
param_sim$m_filtrada = m_filtrada
param_sim$k_sem_ant = k_sem_ant
param_sim$vec_sem_sig = vec_sem_sig

# Start the clock!
ptm <- proc.time()
mat_alumnos_corregidos <- gen_mat_alumnos_corregidos(vec_s_sem_k_info,param,param_sim)
View(mat_alumnos_corregidos)
cat("La función gen_mat_alumnos_corregidos tardó: ",(proc.time()-ptm)[3]," segundos\n")
######### 0.04 seg
######### 0.06 seg
# 5 materias: "Estadística III","Seminario de Ciencias Computacionales",
#"Cálculo Diferencial e Integral I","Mecánica Cuántica", "Teoría del Seguro"
# 4 semestres: 20181, 20182, 20191
# 5 semestres de información: 20161 20162 20171 20172 20181 20182 20191 20192
# m_filtrada tiene 321 renglones



# Start the clock!
ptm <- proc.time()
vec_sem_sig <- c(20182,20191,20192,20201)
k_sem_ant <- 10
vec_s_sem_k_info <- gen_vec_s_sem_k_info(vec_sem_sig,k_sem_ant,param)
Materias <- c("Inglés I","Ecuaciones Diferenciales II",
              "Cálculo Diferencial e Integral III","Inferencia Estadística",
              "Temas Selectos de Investigación de Operaciones")
m_filtrada <- gen_mat_m_filtrada(Materias,param,param_sim)
param_sim$m_filtrada = m_filtrada
param_sim$k_sem_ant = k_sem_ant
param_sim$vec_sem_sig = vec_sem_sig
mat_alumnos_corregidos <- gen_mat_alumnos_corregidos(vec_s_sem_k_info,param,param_sim)
View(mat_alumnos_corregidos)
cat("\n El proceso tardó: ",(proc.time()-ptm)[3]," segundos\n")
######### 2 seg ERROR
######### 3.23 seg
# 5 materias: "Inglés I","Ecuaciones Diferenciales II",
# "Cálculo Diferencial e Integral III","Inferencia Estadística",
# "Temas Selectos de Investigación de Operaciones"
# 4 semestres: 20181, 20182, 20191
# 10 semestres de información: 20161 20162 20171 20172 20181 20182 20191 20192
# m_filtrada tiene 248 renglones


# gen_mat_1_sim_m_materias_1_sem ------------------------------------------
#' Title gen_mat_1_sim_m_materias_1_sem: Función que genera el vector
#' "vec_sim_1_sem", el cual tiene la simulación de un semestre.
#' 
vec_sem_sig <- 20182
k_sem_ant <- 5

param_sim$vec_sem_sig = vec_sem_sig
param_sim$k_sem_ant = k_sem_ant
param_sim$Materias = c("Cálculo Diferencial e Integral I")
param_sim$num_sim = 10
m_filtrada <- gen_mat_m_filtrada(param,param_sim)
# View(m_filtrada)
param_sim$m_filtrada = m_filtrada

vec_s_sem_k_info <- gen_vec_s_sem_k_info(vec_sem_sig,k_sem_ant,param)
mat_al_1_sem <- gen_mat_alumnos_corregidos(vec_s_sem_k_info,param,param_sim)
(vec_sim_1_sem <- gen_mat_1_sim_m_materias_1_sem(mat_al_1_sem,param))

####
##Se definen las variables que se van a utilizar:
param$q1 = 60
param$q2 = 99
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
    vec_sim_1_sem[r] <- sample(ceiling(cota1):ceiling(cota2),1)
    # vec_sim_1_sem[r] <- sample(ceiling(media):ceiling(cota2),1)
    # vec_sim_1_sem[r] <- sample(ceiling(cota1):ceiling(media),1)
  }
}#Fin for(r)






# gen_mat_1_sim_m_materias_s_sem ------------------------------------------
#' Title gen_mat_1_sim_m_materias_s_sem: Función que genera la matriz
#' "mat_1_sim_m_materias_s_sem", la cual tiene la simulación de s semestres.
#'
# Start the clock!
ptm <- proc.time()
# vec_s_sem_k_info <- gen_vec_s_sem_k_info(param_sim$vec_sem_sig,
#                                          param_sim$k_sem_ant,param)
mat_1_sim_m_materias_s_sem <- gen_mat_1_sim_m_materias_s_sem(param,param_sim)
View(mat_1_sim_m_materias_s_sem)
cat("La función gen_mat_1_sim_m_materias_s_sem tardó: ",
    (proc.time()-ptm)[3]," segundos\n")
######### 0.92 seg
# 5 materias: "Estadística III","Seminario de Ciencias Computacionales",
#"Cálculo Diferencial e Integral I","Mecánica Cuántica", "Teoría del Seguro"
# 3 semestres: 20181, 20182, 20191
# m_filtrada cargada en param_sim
# 5 semestres de información

vec_s_sem_k_info <- gen_vec_s_sem_k_info(param_sim$vec_sem_sig,
                                         param_sim$k_sem_ant,param)
# Start the clock!
ptm <- proc.time()
mat_1_sim_m_materias_s_sem <- gen_mat_1_sim_m_materias_s_sem(vec_s_sem_k_info,
                                                             param,param_sim)
# View(mat_1_sim_m_materias_s_sem)
cat("La función gen_mat_1_sim_m_materias_s_sem tardó: ",
    (proc.time()-ptm)[3]," segundos\n")
######### 1.41 seg CONTANDO vec_s_sem_k_info
######### 1.19 seg
######### 1.27 seg
######### 1.18 seg
# 5 materias: "Estadística III","Seminario de Ciencias Computacionales",
#"Cálculo Diferencial e Integral I","Mecánica Cuántica", "Teoría del Seguro"
# 4 semestres: 20181, 20182, 20191
# m_filtrada cargada en param_sim
# 5 semestres de información



# extrae_alumnos ----------------------------------------------------------
#' Se extrae el número de alumnos por hora y por semestre de cada materia.
#' 
#' @param materia: Nombre de la materia de la cual se obtendrá el número de
#' grupos por semestre.
#' @examples materia <- "Probabilidad I"
#' @return mat_alumnos_x_hora_sem: Matriz que tiene como información del
#' número de alumnos por hora (renglones) y semestre (columna).
#' 
materia <- "Estadística I" ##Igual que "Inferencia Estadística"
mat_alumnos_x_hora_sem <- extrae_alumnos_1_materia(materia,param)
View(mat_alumnos_x_hora_sem)


# estima_alumnos -----------------------------------------------------------
#' Función que arroja una matriz de 15 renglones (horas) y 3 columnas: cota1,
#' media, cota2; los cuales corresponden a la estimación del número de alumnos
#' que se tendrán en el siguiente semestre por hora.
#' 
materia <- "Estadística I" ##Igual que "Inferencia Estadística"
mat_alumnos_x_hora_sem <- extrae_alumnos_1_materia(materia,param)
mat_alumnos_estimados <- estima_alumnos_1_materia(mat_alumnos_x_hora_sem,param)
View(mat_alumnos_estimados)


# simula_alumnos -----------------------------------------------------------
#' Se manda llamar la función "estima_alumnos" para simular una variable
#' aleatoria discreta en el intervalo de las cotas arrojadas por dicha función.
#' La función simula_alumnos regresa un vector con el número de alumnos
#' simulados por cada hora para el siguiente semestre.
#' 
materia <- "Estadística I" ##Igual que "Inferencia Estadística"
(vec_alumnos_simulados <- simula_alumnos(materia,param))


# simula_tamano_grupo -----------------------------------------------------
#'  Función que simula el tamaño de los grupos por materia en cada hora
#'  dependiendo del número de alumnos que se han tenido.
#'
materia <- "Estadística I" ##Igual que "Inferencia Estadística"
(sim_tam_gpo_x_hora <- simula_tamano_grupo(materia,param))


# gen_mat_simula_grupos_una_materia --------------------------------------
#' Title gen_mat_simula_grupos_una_materia: Función que guarda la matriz 
#' "mat_simula_grupos_una_materia" por materia, la cual contiene 24 columnas:
#' Materia, Horario, Número de grupos simulados, Número de alumnos simulados,
#' las últimas 20 columnas indican el número de simulaciones del tamaño de
#' grupo, en sus renglones se tiene el número de alumnos de cada grupo
#' simulado por hora.
#' 
materia <- "Modelos de Supervivencia y de Series de Tiempo"
materia <- "Probabilidad I"
mat_simula_grupos_una_materia <- gen_mat_simula_grupos_una_materia(materia,param)
View(mat_simula_grupos_una_materia)


# guarda_mat_simula_grupos_1_sem ------------------------------------------------
#' Title guarda_mat_simula_grupos_1_sem: Función que guarda la matriz 
#' "mat_simula_grupos" la cual contiene 24 columnas: Materia, Horario,
#' Número de grupos simulados, Número de alumnos simulados, las últimas 20
#' columnas indican el número de simulaciones del tamaño de grupo, en sus
#' renglones se tiene el número de alumnos de cada grupo simulado.
#'
## **La función se tarda alrededor de 1hora, se puede cargar la matriz
##**directamente en caso de qeurer verla sin correr la función.
mat_simula_grupos <- guarda_mat_simula_grupos_1_sem(param)
# load("mat_simula_grupos_20202.RData")
View(mat_simula_grupos)


# simula_grupos -----------------------------------------------------------
#' Función que arroja el vector con el número de grupos simulados por hora,
#' que depende del número de alumnos que se estimaron con modelo hw()
#' Holt-Winters y se simularon.
#'
materia <- "Estadística I" ##Igual que "Inferencia Estadística"
(vec_grupos_simulados <- simula_grupos(materia,param))


# gen_esqueleto -----------------------------------------------------------
#' Función que genera una matriz llamada "mat_esqueleto", del semestre actual
#' (sem_fin). Arroja un error en caso de no encontrar algún archivo que
#' requiera. La matriz tiene 15 renglones con las horas (7-8,8-9,...,21-22)
#' y tantas columnas como materias se tengan. Contiene el número de grupos
#' simulados para cada materia en cada hora.
#'
sem_fin <- 20201 ##Para obtener las simulaciones del 20202
sem_fin <- 20192 ##Para obtener las simulaciones del 20201
n_semestres_anteriores <- 25 ##2008-1 - 2020-2
n_semestres_anteriores <- 24 ##2008-1 - 2020-1
n_semestres_anteriores <- 15 ##2013-1 - 2020-1
directorio_info <- rep(0,n_semestres_anteriores)
for (k in 1:n_semestres_anteriores) {
  sem_info <- param$Semestres[k]
  directorio_info[k] <- paste0("m_grande por semestre/m_grande_",sem_info,".RData")
}
mat_esqueleto <- gen_esqueleto(sem_fin,n_semestres_anteriores,directorio_info,param)
View(mat_esqueleto)


# extrae_mat_x_prof -------------------------------------------------------
#' Esta función regresa una matriz de 16xnum_sem que contiene los nombres de
#' las materias que ha dado cada profesor por semestre. Se tienen 16 renglones
#' porque se tienen 8 columnas con nombres por 2 materias
#' 
profesor <- "Margarita Elvira Chávez Cano"
materia_x_profesor <- extrae_mat_x_prof(profesor,param)
View(materia_x_profesor)


# simula_eleccion_materia ---------------------------------------------------------
#' Función que simula varias elecciones de materias de cada profesor. Arroja un
#' vector con los nombres de las materias simuladas.
#' 
profesor <- "Margarita Elvira Chávez Cano"
(materias_simuladas <- simula_eleccion_materia(profesor,param))


# horario_de_materia_x_prof ---------------------------------------------
#' Esta función regresa una matriz de 2xnum_sem que contiene los horarios de las
#' materias que ha dado cada profesor por semestre.
#' 
profesor <- "Margarita Elvira Chávez Cano"
horario_x_profesor <- horario_de_materia_x_prof(profesor,param)
View(horario_x_profesor)


# simula_eleccion_horario ---------------------------------------------------------
#' Función que simula varias elecciones de horarios de cada profesor. Arroja un
#' vector con los nombres de los horarios simulados por profesor.
#' 
profesor <- "Margarita Elvira Chávez Cano"
(horarios_simulados <- simula_eleccion_horario(profesor,param))


# gen_solicitudes ---------------------------------------------------------
#' Función que guarda, en una matriz, de 12 columnas, la información
#' de las solicitudes de materia y de horario de todos los profesores las
#' cuales se generan por medio de las simulaciones de dichas elecciones
#' (materia y horario). En las primeras 6 columnas se tiene la información
#' de la simulación de elección de materias y en las últimas 6 columnas
#' se tiene la información de la simulación de elección de horarios,
#' la matriz puede no estar completamente llena),tiene como renglones
#' los nombres de los profesores.
#'
mat_solicitudes <- gen_solicitudes(param)
View(mat_solicitudes)


# asigna_una_mat_prof_hora ---------------------------------------------------
#' Función que genera la asignación de una  materia con profesor por hora,
#' dependiendo del número de grupos simulados para el siguiente semestre.
#' Arroja una matriz de 4 columnas (Materia,Profesor,Horario,Salón), la cual
#' tiene la información de las asignaciones generadas de las simulaciones tanto
#' del número de grupos como de las elecciones de los profesores.
#'
mat_solicitudes <- gen_solicitudes(param)
materia <- "Estadística I" ##Igual que "Inferencia Estadística"
mat_asignacion <- asigna_una_mat_prof_hora(mat_solicitudes,materia,param)
View(mat_asignacion)



# gen_asignacion ----------------------------------------------------------
#' Función que genera asignaciones de materia con profesor por hora,
#' dependiendo del número de grupos simulados para el siguiente semestre,
#' con la información de solicitudes que se obtiene de la función
#' "gen_solicitudes". Arroja una matriz de cuatro columnas (Materia, Profesor,
#' Horario, Salón) la cual contiene en el i-ésimo renglón la asignación
#' por materia, profesor y horario //está pendiente la asignación de salón//
#'
sem_fin <- 20201
n_semestres_anteriores <- 25
directorio_info <- rep(0,n_semestres_anteriores)
for (k in 1:25) {
  sem_info <- param$Semestres[k]
  directorio_info[k] <- paste0("m_grande por semestre/m_grande_",sem_info,".RData")
}
mat_esqueleto <- gen_esqueleto(sem_fin,n_semestres_anteriores,directorio_info,param)
mat_solicitudes <- gen_solicitudes(param)
mat_asignaciones <- gen_asignacion(mat_esqueleto,mat_solicitudes,param)
View(mat_asignaciones)



# gen_asignacion_completa -------------------------------------------------
#' Función que genera la asignación completa, tiene como parámetros el semestre
#' inicial y el final. Arroja la matriz "mat_asignaciones" con las asignaciones
#' de materia-profesor-horario(-salón)
#'
sem_ini <- 20081
sem_fin <- 20201
mat_asignaciones <- gen_asignacion_completa(sem_ini,sem_fin)
View(mat_asignaciones)


# arroja_error_espacio_en_mat ---------------------------------------------
#' Función que arroja una variable binaria la cual vale 1 si el número de
#' columnas para guardar la información es menor al número máximo de grupos
#' reales (i.e. si hay un error), vale 0 si no hay error. Revisa si hay
#' espacio suficiente para guardar la información de los grupos reales.
#'
sem_info <- 20152
arroja_error_espacio_en_mat(sem_info)


# heatmap -----------------------------------------------------------------
## Se carga la matriz que se va a utilizar como ejemplo
load("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Programas/Prog V05/mat_varianza_gpos_sim.RData")
dim(mat_varianza_gpos_sim)
mat_heat <- matrix(0,nrow = dim(mat_varianza_gpos_sim)[1],ncol = 20)

## SE convierten los datos en valores de tipo numérico
for(d in 4:23){
  mat_heat[,d-3] <- as.numeric(mat_varianza_gpos_sim[,d])
}

# df_heat <- as.data.frame()
# mat_heat <- as.numeric(df_heat)
View(mat_heat)
colnames(mat_heat) <- c("Var(Gpo_1)","Var(Gpo_2)","Var(Gpo_3)","Var(Gpo_4)","Var(Gpo_5)",
                        "Var(Gpo_6)","Var(Gpo_7)","Var(Gpo_8)","Var(Gpo_9)","Var(Gpo_10)",
                        "Var(Gpo_11)","Var(Gpo_12)","Var(Gpo_13)","Var(Gpo_14)",
                        "Var(Gpo_15)","Var(Gpo_16)","Var(Gpo_17)","Var(Gpo_18)",
                        "Var(Gpo_19)","Var(Gpo_20)")
rownames(mat_heat) <- c("2013-2","2014-1","2014-2","2015-1","2015-2","2016-1","2016-2",
                        "2017-1","2017-2","2018-1","2018-2","2019-1","2019-2","2020-1")

# Most basic Heatmap:
# How to do it: below is the most basic heatmap you can build in base R, using
# the heatmap() function with no parameters. Note that it takes as input a
# matrix. If you have a data frame, you can convert it to a matrix with
# as.matrix(), but you need numeric variables only.
#
# How to read it: each column is a variable. Each observation is a row. Each
# square is a value, the closer to yellow the higher. You can transpose the
# matrix with t(data) to swap X and Y axis.

# Default Heatmap
heatmap(mat_heat)

# Note: as you can see this heatmap is not very insightful: all the variation
# is absorbed by the hp and disp variables that have very high values compared
# to the others. We need to normalize the data, as explained in the next section.


# Normalization:
# Normalizing the matrix is done using the scale argument of the heatmap()
# function. It can be applied to row or to column. Here the column option is
# chosen, since we need to absorb the variation between column.

# Use 'scale' to normalize
heatmap(mat_heat, scale="column")


# Dendrogram and Reordering:
# You may have noticed that order of both rows and columns is different
# compare to the native mtcar matrix. This is because heatmap() reorders both
# variables and observations using a clustering algorithm: it computes the
# distance between each pair of rows and columns and try to order them by
# similarity.
# Moreover, the corresponding dendrograms are provided beside the heatmap.
# We can avoid it and just visualize the raw matrix: use the Rowv and Colv
# arguments as follow.

# No dendrogram nor reordering for neither column or row
heatmap(mat_heat, Colv = NA, Rowv = NA, scale="column")


# Color palette:
# There are several ways to custom the color palette:
# a) use the native palettes of R: terrain.color(), rainbow(), heat.colors(),
#topo.colors() or cm.colors()
# b) use the palettes proposed by RColorBrewer. See list of available palettes:
# https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html

# 1: native palette from R
heatmap(mat_heat, scale="column", col = cm.colors(256)) ##Azules/Morados
heatmap(mat_heat, scale="column", col = terrain.colors(256)) ##Verdes/Rosas
# 2: Rcolorbrewer palette
# library(RColorBrewer)
coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25) ##Morados/Verdes
heatmap(mat_heat, scale="column", col = coul)


# Custom Layout:
# You can custom title & axis titles with the usual main and xlab/ylab
#arguments (left). You can also change labels with labRow/colRow and their
#size with cexRow/cexCol.

# Add classic arguments like main title and axis title
heatmap(mat_heat, Colv = NA, Rowv = NA, scale="column", col = coul, xlab="variable", ylab="car", main="heatmap")
# Custom x and y labels with cexRow and labRow (col respectively)
heatmap(mat_heat, scale="column", cexRow=1.5, labRow=paste("new_", rownames(data),sep=""), col= colorRampPalette(brewer.pal(8, "Blues"))(25))


# Add color beside heatmap:
# Often, heatmap intends to compare the observed structure with an expected
#one. You can add a vector of color beside the heatmap to represents the
#expected structure using the RowSideColors argument.

# Example: grouping from the first letter:
# my_group <- as.numeric(as.factor(substr(rownames(mat_heat), 1 , 1)))
# my_group <- as.numeric(as.factor(substr(colnames(mat_heat), 1 , 1)))
my_group <- as.numeric(as.factor(sample(1:10,size = length(rownames(mat_heat)),replace = T)))
colSide <- brewer.pal(9, "Set1")[my_group]
colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
heatmap(mat_heat, Colv = NA, Rowv = NA, scale="column" , RowSideColors=colSide, col=colMain   )


# gen_mat_real_grupos_una_materia ----------------------------------------
#' Title gen_mat_real_grupos_una_materia: Función que guarda la matriz 
#' "mat_real_grupos_una_materia" la cual contiene 24 columnas: Materia,
#' Horario, Número de grupos reales, Número de alumnos reales, las últimas
#' 20 columnas indican el número de simulaciones del tamaño de grupo, en sus
#' renglones se tiene el número real de alumnos de cada grupo por hora.
#' Fijando materia y semestre
#'
materia <- "Modelos de Supervivencia y de Series de Tiempo"
sem_info <- 20201
mat_real_grupos_una_materia <- gen_mat_real_grupos_una_materia(materia,sem_info,param)
View(mat_real_grupos_una_materia)

# guarda_mat_real_grupos_x_sem --------------------------------------------------
#' Title guarda_mat_real_grupos_x_sem: Función que guarda la matriz 
#' "mat_real_grupos" la cual contiene 24 columnas: Materia, Horario,
#' Número de grupos reales, Número de alumnos reales, las últimas 20
#' columnas indican el número de simulaciones del tamaño de grupo, en sus
#' renglones se tiene el número real de alumnos de cada grupo por hora.
#'
sem_info <- 20201
mat_real_grupos <- guarda_mat_real_grupos_x_sem(sem_info,param)
View(mat_real_grupos)

##Todos los semestres
for(d in 1:(length(param$sem_totales)-1)){
  sem_info <- param$sem_totales[d]
  cat("\nsem_info = ",sem_info)
  mat_real_grupos <- guarda_mat_real_grupos_x_sem(sem_info,param)
  # View(mat_real_grupos)
}


# datos_num_max_de_gpos_sim --------------------------------------------------
#' Función que regresa un vector con los valores del número máximo de grupos
#' simulados por semestre considerando que están dividos por materia y horario.
#'
sem_ini <- 20132
sem_fin <- 20202
datos_num_max_de_gpos_sim(sem_ini,sem_fin)


# datos_num_max_de_gpos_real --------------------------------------------------
#' Función que regresa un vector con los valores del número máximo de grupos
#' reales por semestre considerando que están dividos por materia y horario.
#'
sem_ini <- 20081
sem_fin <- 20201
datos_num_max_de_gpos_real(sem_ini,sem_fin)


# guarda_mat_diferencias --------------------------------------------------
#' Función en la que se obtienen todas las matrices "mat_diferencias" para
#' cada semestre desde "sem_ini" hasta "sem_fin".
#' 
sem_ini <- 20132
sem_fin <- 20201
guarda_mat_diferencias(sem_ini,sem_fin)


# guarda_mat_esp_dif ------------------------------------------------------
#' Title guarda_mat_esp_dif: Función que guarda la matriz "mat_esp_dif" la cual
#' tiene la esperanza de los datos de los valores de la matriz de diferencias.
sem_ini <- 20132
sem_fin <- 20201
guarda_mat_esp_dif(sem_ini,sem_fin)

# guarda_mat_var_dif ------------------------------------------------------
#' Title guarda_mat_var_dif: Función que guarda la matriz "mat_var_dif" la
#' cual tiene la varianza de los datos de los valores de la matriz de
#' diferencias.
#' 
sem_ini <- 20132
sem_fin <- 20201
guarda_mat_var_dif(sem_ini,sem_fin)


# guarda_mat_sd_dif ------------------------------------------------------
#' Función que guarda la matriz "mat_sd_dif" la cual tiene la desviación
#' estándar de los datos de los valores de la matriz de diferencias.
#' 
sem_ini <- 20132
sem_fin <- 20201
guarda_mat_sd_dif(sem_ini,sem_fin)


# guarda_mat_esp_sim ------------------------------------------------------
#' Función que guarda la matriz "mat_esp_sim" la cual tiene la esperanza de
#' los datos de los valores de la matriz de datos simulados.
#' 
sem_ini <- 20132
sem_fin <- 20201
guarda_mat_esp_sim(sem_ini,sem_fin)


# guarda_mat_var_sim ------------------------------------------------------
#' Función que guarda la matriz "mat_var_sim" la cual tiene la varianza de
#' los datos de los valores de la matriz de datos simulados.
#' 
sem_ini <- 20132
sem_fin <- 20201
guarda_mat_var_sim(sem_ini,sem_fin)


# guarda_mat_sd_sim ------------------------------------------------------
#' Función que guarda la matriz "mat_sd_sim" la cual tiene la desviación
#' estándar de los datos de los valores de la matriz de datos simulados.
#' 
sem_ini <- 20132
sem_fin <- 20201
guarda_mat_sd_sim(sem_ini,sem_fin)


# gen_mat_n_sim_1_materia --------------------------------------------------
#' Title gen_mat_n_sim_1_materia: Función que hace "n" simulaciones fijando un
#' semestre y una materia; arroja una matriz con el número de alumnos por
#' grupo ordenados de mayor a menor por cada hora. Las "n" matrices generadas
#' se guardan en una lista llamada "lista_mat_n_sim". Cada matriz generada
#' tiene 24 columnas: Materia, Horario, Número total de grupos simulados,
#' Número total de alumos simulados, las siguientes 20 columnas contienen
#' el número de alumnos simulados, ordenados de mayor a menor. Las matrices
#' generadas se guardan en una lista llamada "lista_mat_n_sim".
#'
load("vec_nom_materias_total.RData")
Materias <- vec_nom_materias_total
materia <- "Modelos de Supervivencia y de Series de Tiempo"
num_materia <- 285
num_sim <- 10

# param$sem_ini = 20081##Inicio de información real
# param$sem_fin = 20192##Fin de información real
# param$sem_sig = 20201##Semestre de simulación
# param$sem_fin = 20201##Fin de información real
# param$sem_sig = 20202##Semestre de simulación
# param$sem_totales <- (20081:param$sem_sig)[(20081:param$sem_sig)%% 10>0 
#                                            &(20081:param$sem_sig)%% 10<3]
# param$Semestres = (param$sem_ini:param$sem_fin)[(param$sem_ini:param$sem_fin) 
#                                                 %% 10>0 &(param$sem_ini:param$sem_fin) %% 10<3]
# param$nombre_sem = as.character(param$Semestres)
# param$n_semestres_anteriores = length(param$Semestres)

lista_mat_n_sim <- gen_mat_n_sim_1_materia(materia,num_materia,
                                           num_sim,param)

# gen_mat_n_sim_1_sem -----------------------------------------------------------
#' Title gen_mat_n_sim_1_sem: Función que arroja una lista con las
#' listas de matrices obtenidas en "gen_mat_n_sim_1_materia" para "sem_sig".
#'
n_semestres_anteriores <- 10
sem_sig <- 20202
num_sim <- 10

lista_n_sim_por_sem <- gen_mat_n_sim_1_sem(n_semestres_anteriores,sem_sig,
                                           num_sim,param)


# gen_mat_n_sim_n_sem -----------------------------------------------------------
#' Title gen_mat_n_sim_n_sem: Función que manda a llamar a la función
#' "gen_mat_n_sim_1_sem" para cada semestre de un vector dado. Se genera
#' una lista de listas para cada semestre en "vec_sem_sig". Esta función se
#' utiliza principalmente para hacer pruebas de que el modelo funciona.
#'
# Ej.1
num_sim <- 10
sem_ini <- 20182
sem_fin <- 20201
vec_sem_sig <- (sem_ini:sem_fin)[(sem_ini:sem_fin)%% 10>0 
                                 &(sem_ini:sem_fin)%% 10<3]
vec_sem_ant <- c(10,11,12,13)##Se inicia en 2013-1
vec_sem_ant <- c(7,8,9,10)##Se inicia en 2015-1

# Ej.2
num_sim <- 10
vec_sem_sig <- c(20131,20152,20182,20201)
vec_sem_ant <- c(10,15,21,24)##Se inicia en 2008-1

# Ej.3
num_sim <- 10
vec_sem_sig <- c(20131,20152,20182,20201)
vec_sem_ant <- rep(5,length(vec_sem_sig)) #Se toman los 5 semestres anteriores


# gen_mat_esp_alum_x_materia_1_sem ------------------------------------------
#' Title gen_mat_esp_alum_x_materia_1_sem: Función que genera una matriz con
#' 20 columnas que contiene la esperanza por cada entrada de las "n" matrices
#' generadas en la función "gen_mat_n_sim_1_sem".
#'
mat_esp_alum_x_materia_1_sem <- gen_mat_esp_alum_x_materia_1_sem(lista_mat_n_sim,param)
View(mat_esp_alum_x_materia_1_sem)


# gen_mat_var_alum_x_materia_1_sem ------------------------------------------
#' Title gen_mat_var_alum_x_materia_1_sem: Función que genera una matriz con
#' 20 columnas que contiene la esperanza por cada entrada de las "n" matrices
#' generadas en la función "gen_mat_n_sim_1_sem".
#'
mat_var_alum_x_materia_1_sem <- gen_mat_var_alum_x_materia_1_sem(lista_mat_n_sim,param)
View(mat_var_alum_x_materia_1_sem)
colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
heatmap(mat_var_alum_x_materia_1_sem, Colv = NA, Rowv = NA, scale="none",col=colMain)


# gen_mat_dif_alum_x_materia_1_sem ----------------------------------------------------
#' Title gen_mat_dif_alum_x_materia_1_sem: Función que genera una matriz con 20 columnas
#' y 15 renglones que contiene la diferencia entre los valores reales menos
#' la esperanza, para cada entrada, fijando materia y semestre.
#' 
mat_dif_alum_x_materia_1_sem <- gen_mat_dif_alum_x_materia_1_sem(mat_esp_alum_x_materia_1_sem,mat_real_grupos_una_materia)
View(mat_dif_alum_x_materia_1_sem)
colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
heatmap(mat_dif_alum_x_materia_1_sem, Colv = NA, Rowv = NA, scale="none",col=colMain,main = "Modelos de Supervivencia y de Series de Tiempo")


# guarda_una_fig_heatmap --------------------------------------------------
#' Title guarda_una_fig_heatmap: Función que guarda una imagen de tipo "jpeg"
#' de las gráficas de heatmap para las matrices de datos generadas.
#'
matriz <- mat_dif_alum_x_materia_1_sem
Materias <- unique(param$m_grande_total[,num_col_Materia])
num_materia <- 5
sem_siguiente <- 20201
nom_archivo <- paste0("Figuras/Matrices Simuladas/PRUEBA_heatmap_dif_",
                      num_materia,"_sem_",sem_siguiente,".jpeg")
guarda_una_fig_heatmap(matriz,Materias,num_materia,nom_archivo)


# guarda_fig_heatmap -----------------------------------------------------
#' Title guarda_fig_heatmap: Función que guarda todas las imágenes de tipo
#' "jpeg" de las gráficas heatmap para las matrices de datos generadas.
#'
param$sem_siguiente <- 20201
num_sim <- 10
guarda_fig_heatmap(num_sim,param)


# gen_mat_max_num_gpos_real -----------------------------------------------
#' Title gen_mat_max_num_gpos_real: Función que guarda y genera la matriz
#' "mat_max_num_gpos_real" que tiene 4 columnas (Semestre, Materia, Horario, 
#' Número de grupos) que contiene la información del máximo número de
#' grupos reales por semestre y por hora.
#'
gen_mat_max_num_gpos_real(param)


# gen_mat_max_num_gpos_sim ------------------------------------------------
#' Title gen_mat_max_num_gpos_sim: Función que guarda y genera la matriz
#' "mat_max_num_gpos_sim" que tiene 4 columnas (Semestre, Materia, Horario, 
#' Número de grupos) que contiene la información del máximo número de
#' grupos simulados por semestre y por hora.
#'
sem_ini <- 20131
sem_fin <- 20201
mat_max_num_gpos_sim <- gen_mat_max_num_gpos_sim(sem_ini,sem_fin,param)


# gen_mat_dif_total_alumnos_x_sem -----------------------------------------
#' Title gen_mat_dif_total_alumnos_x_sem: Función que genera una matriz con 15
#' columnas y 15 renglones, llamada "mat_dif_total_alumnos_x_sem". La matriz
#' contiene la diferencia entre los valores del total de alumnos reales
#' menos los simulados para cada hora, fijando el semestre.
#' Columnas: Semestres simulados, Renglones: Horas 7-8,8-9,...,20-21,21-22
sem_ini <- 20131
sem_fin <- 20201

mat_dif_total_alumnos_x_sem <- gen_mat_dif_total_alumnos_x_sem(sem_ini,sem_fin,param)
View(mat_dif_total_alumnos_x_sem)

## Para guardar gráficas de R como imagen .jpeg se manda llamar la función
## jpeg() antes de generar una gráfica. Al hacer esto, le indicamos a R
##que en lugar de mandar nuestro gráfico a una ventana del escritorio, lo
##mande a un dispositivo gráfico distinto.
## La función dev.off(), se utiliza para cerrar el dispositivo gráfico
##elegido y así poder crear más gráficos después.
nom_archivo <- paste0("Figuras/Matrices Simuladas/heatmap_dif_total_de_alumnos_x_sem.jpeg")
# nom_archivo <- "Diferencia Total de alumnos_x_sem"
jpeg(filename = nom_archivo, width = 800, height = 700)
colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
heatmap(mat_dif_total_alumnos_x_sem, Colv = NA, Rowv = NA, scale="none",
        col=colMain,main = "Diferencia Total de alumnos_x_sem")
dev.off()


# gen_mat_dif_total_gpos_x_sem -----------------------------------------
#' Title gen_mat_dif_total_gpos_x_sem: Función que genera una matriz con 15
#' columnas y 15 renglones, llamada "mat_dif_total_gpos_x_sem". La matriz
#' contiene la diferencia entre los valores del total de grupos reales
#' menos los simulados para cada hora, fijando el semestre.
#' Columnas: Semestres simulados, Renglones: Horas 7-8,8-9,...,20-21,21-22
#' 
sem_ini <- 20131
sem_fin <- 20201

mat_dif_total_gpos_x_sem <- gen_mat_dif_total_gpos_x_sem(sem_ini,sem_fin,param)
View(mat_dif_total_gpos_x_sem)

## Para guardar gráficas de R como imagen .jpeg se manda llamar la función
## jpeg() antes de generar una gráfica. Al hacer esto, le indicamos a R
##que en lugar de mandar nuestro gráfico a una ventana del escritorio, lo
##mande a un dispositivo gráfico distinto.
## La función dev.off(), se utiliza para cerrar el dispositivo gráfico
##elegido y así poder crear más gráficos después.
nom_archivo <- paste0("Figuras/Matrices Simuladas/heatmap_dif_total_de_gpos_x_sem.jpeg")
# nom_archivo <- "Diferencia Total de gpos_x_sem"
jpeg(filename = nom_archivo, width = 800, height = 700)
colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
heatmap(mat_dif_total_gpos_x_sem, Colv = NA, Rowv = NA, scale="none",
        col=colMain,main = "Diferencia Total de gpos_x_sem")
dev.off()


# gen_mat_nom_cap_salon_x_sem ---------------------------------------------
#' Title gen_mat_nom_cap_salon_x_sem: Función que genera la matriz llamada
#' "mat_nom_cap_salon" que contiene en su primer columna los nombres de los
#' salones de la facultad y en las siguientes columas se tienen todas sus
#' capacidades por semestre, desde el 2013-1 hasta el 20201.
#' 
mat_nom_cap_salon <- gen_mat_nom_cap_salon_x_sem()
View(mat_nom_cap_salon)


# selecciona_cap_salon ----------------------------------------------------
#' Title selecciona_cap_salon: Función que arroja una matriz de dos
#' columnas, en la primera contiene los nombres de los salones de la
#' facultad y en la segunda la capacidad máxima de cada salón de
#' "mat_nom_cap_salon".
#' 
mat_cap_salon <- selecciona_cap_salon(mat_nom_cap_salon)
View(mat_cap_salon)


# gen_mat_dif_relativas ---------------------------------------------------
#' Title gen_mat_dif_relativas: Función que recibe dos matrices, una
#' matriz de diferencias absolutas y una matriz de valores reales, arroja
#' una matriz con las diferencias relativas correspondientes y guarda la
#' gráfica "heatmap" de ella.
#'
sem_ini <- 20131
sem_fin <- 20201
mat_dif_total_alumnos_x_sem <- gen_mat_dif_total_alumnos_x_sem(sem_ini,sem_fin,param)
# View(mat_dif_total_alumnos_x_sem)
mat_dif_total_gpos_x_sem <- gen_mat_dif_total_gpos_x_sem(sem_ini,sem_fin,param)
# View(mat_dif_total_gpos_x_sem)

#Se definen las matrices de valores reales
intervalo_sem <- (sem_ini:sem_fin)[(sem_ini:sem_fin)%% 10>0 &(sem_ini:sem_fin)%% 10<3]
mat_aux_real_alum <- matrix(0,nrow = 15,ncol = length(intervalo_sem))
mat_aux_real_gpos <- matrix(0,nrow = 15,ncol = length(intervalo_sem))
nom_col_real_alum <- "Alumnos_Reales_Totales"
nom_col_real_gpos <- "Grupos_Reales"

##Se obtienen las matrices que contienen los datos de alumnos totales
##reales por semestre
for(s in 1:length(intervalo_sem)){#Recorre los semestres
  nom_archivo_real <- paste0("mat_real_grupos por semestre/mat_real_grupos_",
                             intervalo_sem[s],".RData")
  load(nom_archivo_real)
  mat_aux_real_alum[,s] <- gen_vec_suma_datos_real_1_sem(mat_real_grupos,nom_col_real_alum,param)
  mat_aux_real_gpos[,s] <- gen_vec_suma_datos_real_1_sem(mat_real_grupos,nom_col_real_gpos,param)
  
}##Fin for s
rownames(mat_aux_real_alum) <- param$nombre_hrs
colnames(mat_aux_real_alum) <- intervalo_sem
rownames(mat_aux_real_gpos) <- param$nombre_hrs
colnames(mat_aux_real_gpos) <- intervalo_sem
# View(mat_aux_real_alum)
# View(mat_aux_real_gpos)

nom_archivo <- "dif_relativa_total_de_alumnos_x_sem"
mat_dif_relativas_alum <- gen_mat_dif_relativas(mat_dif_total_alumnos_x_sem,mat_aux_real_alum,nom_archivo)
View(mat_dif_relativas_alum)

nom_archivo <- "dif_relativa_total_de_gpos_x_sem"
mat_dif_relativas_gpos <- gen_mat_dif_relativas(mat_dif_total_gpos_x_sem,mat_aux_real_gpos,nom_archivo)
View(mat_dif_relativas_gpos)


# gen_mat_materias_rep_1_sem ----------------------------------------------
#' Title gen_mat_materias_rep_1_sem: Función que recibe como parámetros
#' "sem_info" y "profesor", arroja una matriz de cuatro columnas (semestre,
#' profesor, materia, hora) en caso de que "profesor" tenga más de una
#' materia asignada a la misma hora.
#'
sem_info <- 20152
# sem_info <- semestres[s]
profesor <- "Arrigo Coen Coria"
profesor <- "Hugo Arizmendi Peimbert"
# profesor <- Profesores[p]
mat_materias_rep_1_sem <- gen_mat_materias_rep_1_sem(sem_info,profesor)
View(mat_materias_rep_1_sem)


# gen_mat_materias_rep ----------------------------------------------------
#' Title gen_mat_materias_rep: Función que recibe como parámetros
#' "sem_ini" y "sem_fin", arroja una matriz de cuatro columnas (semestre,
#' profesor, materia, hora) en caso de que "profesor" tenga más de una
#' materia asignada a la misma hora, para todos los profesores en el
#' intervalo de semestres de "sem_ini" hasta "sem_fin".
#'
mat_materias_rep <- gen_mat_materias_rep(param)
View(mat_materias_rep)
# save(mat_materias_rep, file = "mat_materias_rep_20081_20192.RData")
# save(mat_materias_rep, file = "mat_materias_rep_20081_20201.RData")


# gen_vec_nom_materias_total ----------------------------------------------
#' Title gen_vec_nom_materias_total: Función que carga la matriz
#' "m_grande_total" de los semestres 2008-1 a 2020-1 de la cual se va
#' obtiene la lista de nombres de las materias sin repetición, conservando
#' los nombres más recientes de las materias.
#'
vec_nom_materias_total <- gen_vec_nom_materias_total()
# length(vec_nom_materias_total) ## 343 //V02
# length(vec_nom_materias_total) ## 248 //V03
# length(vec_nom_materias_total) ## 349 //V04
length(vec_nom_materias_total) ## 346
View(vec_nom_materias_total)


# checa_nom_1_materia_en_vec ----------------------------------------------
#' Title checa_nom_1_materia_en_vec: Función que recibe como parámetros el
#' nombre de una materia y el vector con los nombres de las materias e
#' imprime una lista con los diferentes nombres que pudiera tener "materia"
#' en "vec_nom_materias_total"
#' Por ejemplo "Estadística III" y "Modelos de Supervivencia y de Series
#' de Tiempo".
#'
load("vec_nom_materias_total.RData")
View(vec_nom_materias_total)
materia <- "Modelos no Paramétricos y de Regresión"
materia <- "Estadística III"
materia <- "Modelos de Supervivencia y de Series de Tiempo"
materia <- "Ingeniería de Software/Ingeniería de Software/Ingeniería de Software
/Ingeniería de Software/Ingeniería de Software/Ingeniería de Software"
checa_nom_1_materia_en_vec(materia,vec_nom_materias_total)

## Ejemplo con una muestra aleatoria de materias
#' Se carga la matriz m_grande_total de 2008-1 a 2020-1 de la cual
#' se va a obtener la lista de nombres que se desea
load("Matrices m_grande_total/m_grande_total_20081_20201.RData")
#Se definen las variables que se van a utilizar
num_col_materia <- arroja_ind_col_MG("Materia")##1
vec_materias <- unique(m_grande_total[,num_col_materia])##531
vec_ej <- sample(vec_materias,5)
vec_ej <- sample(vec_materias,10)
for(d in 1:length(vec_ej)){
  checa_nom_1_materia_en_vec(vec_ej[d],vec_nom_materias_total)
}


# checa_nom_materias_en_vec -----------------------------------------------
#' Title checa_nom_materias_en_vec: Función que recibe como parámetro el
#' vector con los nombres de las materias e imprime una lista con los
#' diferentes nombres que pudiera tener cada "materia" en "vec_nom_materias_total"
#' Por ejemplo "Estadística III" y "Modelos de Supervivencia y de Series
#' de Tiempo".
#'
load("vec_nom_materias_total.RData")
View(vec_nom_materias_total)
checa_nom_materias_en_vec(vec_nom_materias_total)

