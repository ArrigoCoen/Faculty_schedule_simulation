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
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
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

# "Estadística I" - "Inferencia Estadística"
# "Estadística II" - "Modelos no Paramétricos y de Regresión"
# "Estadística III" - "Modelos de Supervivencia y de Series de Tiempo"

# gen_m_grande_total ------------------------------------------------------
#' Title gen_m_grande_total: Función que genera la matriz "m_grande_total"
#' para un intervalo dado.
#'
# param$sem_ini = 20151##Inicio de información real
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
num_col_NumMateria <- arroja_ind_col_MG("Num_materia")##37


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


# arroja_num_materia ------------------------------------------------
#' Title arroja_num_materia: Función que recibe el nombre de la
#' materia que se busca y devuelve el número de materia en "vec_nom_materias"
#' con ese nombre.
#'
materia <- "Estadística I"
arroja_num_materia(materia,param)


# agrega_nom_1_materia_en_vec ---------------------------------------------
#' Title agrega_nom_1_materia_en_vec: Función que recibe como parámetros el
#' nombre de una materia y el vector con los nombres de las materias y
#' guarda la materia en caso de dar la opción de "SI". También imprime una
#' lista con los diferentes nombres que pudiera tener "materia" en
#' "vec_nom_materias_total"
#' Por ejemplo "Estadística III" y "Modelos de Supervivencia y de Series
#' de Tiempo".
materia <- "Taller de Redacción"
vec_nom_materias_total <- param$vec_nom_materias_total
agrega_nom_1_materia_en_vec(materia,vec_nom_materias_total)

load(file = "vec_nom_materias_total.RData")
param$vec_nom_materias_total = vec_nom_materias_total
vec_nom_materias_total <- param$vec_nom_materias_total
materia <- "Solución Numérica de Ecuaciones Diferenciales Ordinarias"
agrega_nom_1_materia_en_vec(materia,vec_nom_materias_total)

load(file = "vec_nom_materias_total.RData")
param$vec_nom_materias_total = vec_nom_materias_total
vec_nom_materias_total <- param$vec_nom_materias_total


# carga_info_prof_tiempo_completo -----------------------------------------
#' Title carga_info_prof_tiempo_completo: Función que extrae los nombres de
#' los profesores de tiempo completo del Departamento de Matemáticas.
#'
vec_prof_TC <- carga_info_prof_tiempo_completo()
length(vec_prof_TC)##94

# gen_mat_nom_prof_total --------------------------------------------------
#' Title gen_mat_nom_prof_total: Función que carga la matriz
#' "m_grande_total" de los semestres 2015-1 a 2020-1 de la cual se va
#' obtiene la lista de nombres de los profesores sin repetición. La matriz
#' "mat_nom_prof_total" tiene 2 columnas, en la primera se tiene el nombre
#' de los profesores y en la segunda se tiene un 1 si el profesor es de
#' tiempo completo y 0 si no.
#'
mat_nom_prof_total <- gen_mat_nom_prof_total()
sum(as.numeric(mat_nom_prof_total[,2]))#94
dim(mat_nom_prof_total)#1387 2
View(mat_nom_prof_total)


# gen_mat_materias_x_carrera ----------------------------------------------
#' Title gen_mat_materias_x_carrera: Función que genera la lista con las
#' matrices que tienen los nombres de las materias de cada carrera y el
#' número de materia para cada materia.
#'

gen_mat_materias_x_carrera(param)
load("lista_mat_materias_x_carrera.RData")
mat_materias_act <- lista_mat_materias_x_carrera[[1]]
View(mat_materias_act)
mat_materias_CdC <- lista_mat_materias_x_carrera[[2]]
View(mat_materias_CdC)
mat_materias_mate <- lista_mat_materias_x_carrera[[3]]
View(mat_materias_mate)
mat_materias_mateAp <- lista_mat_materias_x_carrera[[4]]
View(mat_materias_mateAp)


# gen_mat_alumnos_corregidos ----------------------------------------------
#' Title gen_mat_alumnos_corregidos: Función que regresa la matriz 
#' "mat_alumnos_corregidos" de 15 renglones (horas) y k+s-1 columnas, la
#' cual tiene el número total de alumnos por hora y por semestre. Se suma
#' el número de alumnos en los semestres repetidos por cada hora. Hay
#' ceros en los semestres y horas en donde no hay información.
#'
param_sim$vec_sem_sig <- 20202
# param_sim$vec_sem_sig <- c(20182,20191,20192,20201)
# param_sim$Materias = "Estadística III" ##Puede ser una o más materias
param_sim$Materias = "Modelos de Supervivencia y de Series de Tiempo"
vec_s_sem_k_info <- gen_vec_s_sem_k_info(param_sim$vec_sem_sig,
                                         param_sim$k_sem_ant,param)
param_sim$m_filtrada <- gen_mat_m_filtrada(param,param_sim)
mat_alumnos_corregidos <- gen_mat_alumnos_corregidos(vec_s_sem_k_info,
                                                     param,param_sim)
mat_alumnos_corregidos


# simula_alumnos ----------------------------------------------------------
#' Title simula_alumnos: Función que genera el vector "vec_sim_1_sem", el
#' cual tiene la simulación de un semestre.
#'
(vec_sim_1_sem <- simula_alumnos(mat_alumnos_corregidos,param))


# gen_mat_demanda_alumnos -------------------------------------------------
#' Title gen_mat_demanda_alumnos: Función que genera la matriz
#' "mat_demanda_alumnos" con 15 renglones (horas) y 333 columnas (materias).
#' En la entrada (i,j) se tiene el número de alumnos simulados para la hora
#' i, y la materia j.
#'
mat_demanda_alumnos <- gen_mat_demanda_alumnos(param,param_sim)
View(mat_demanda_alumnos)


# gen_solicitudes_1_profesor ----------------------------------------------
#' Title gen_solicitudes_1_profesor: Función que genera la solicitud de 
#' un solo profesor. Arroja la matriz "mat_1_solicitud" de 4 columnas
#' (Profesor,TC,Materia, Horario) y 6 renglones que tiene la información de
#' la solicitud de "nom_prof". Se eligen 2 materias y hasta 3 diferentes
#' horarios.
#'
nom_prof<- "Arrigo Coen Coria"
tipo_prof <- 0
gen_solicitudes_1_profesor(nom_prof,tipo_prof,param)
nom_prof<- "Margarita Elvira Chávez Cano"
tipo_prof <- 1
mat_1_solicitud <- gen_solicitudes_1_profesor(nom_prof,tipo_prof,param)
View(mat_1_solicitud)


# gen_solicitudes ---------------------------------------------------------
#' Title gen_solicitudes: Función que genera la solicitud de todos los
#' profesores en la matriz "mat_nom_prof_total". Arroja la matriz
#' "mat_solicitudes" de 4 columnas (Profesor,TC,Materia, Horario). Tiene la
#' información de las solicitudes de los profesores. Se eligen hasta dos
#' materias y hasta 3 diferentes horarios. Se quitan los renglones repetidos.
#'
mat_solicitudes <- gen_solicitudes(param)#8.56 seg
View(mat_solicitudes)


# simula_alum_x_profesor --------------------------------------------------
#' Title simula_alum_x_profesor: Función que simula el número de alumnos
#' para un profesor y una materia. Se obtiene la información del número de
#' alumnos que ha tenido el profesor (del 2015-1 al 2020-1), se toma el
#' mín y el máx, se simula una uniforme en ese intervalo, se redondea el
#' valor con la función ceiling y así se obtiene el valor simulado.
#'
renglon <- c("Gerardo Sánchez Licea",1,"Análisis Matemático I",
             30,12)
(num_alum_x_profesor <- simula_alum_x_profesor(renglon,param))


# gen_esqueleto -----------------------------------------------------------
#' Title gen_esqueleto: Función que arroja una lista con las matrices:
#' 1) mat_esqueleto: Matriz de 15 renglones (horas) y 333 columnas
#' (materias). En la entrada (i,j) se tiene el número de grupos simulados
#' para la hora i, y la materia j.
#' 2) mat_prof_TC: Matriz de 2 columnas con el nombre de los profesores de
#' tiempo completo y el número de materias asignadas.
#' 3) mat_prof_asig: Matriz de 2 columnas con el nombre de los profesores de
#' asignatura y el número de materias asignadas.
#'
lista_info_esqueleto <- gen_esqueleto(mat_demanda_alumnos,mat_solicitudes,
                                      param)#19.48seg
View(lista_info_esqueleto[[1]])
View(lista_info_esqueleto[[2]])
View(lista_info_esqueleto[[3]])



