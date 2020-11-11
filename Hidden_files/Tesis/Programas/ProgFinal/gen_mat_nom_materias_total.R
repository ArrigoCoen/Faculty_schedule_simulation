##########################################################################
#' En este programa se encuentra la funciónque genera la matriz
#' "mat_nom_materias_total" que contiene los nombres de las materias que se
#' van a utilizar en la simulación y todos los posibles nombres con los que
#' aparece.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")



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
  save(mat_nom_materias_total, file = "mat_nom_materias_total.RData")
  
  return(mat_nom_materias_total)
}




# PRUEBAS -----------------------------------------------------------------
mat_nom_materias_total <- gen_mat_nom_materias_total(param,param_sim)
View(mat_nom_materias_total)

# vec_ind_retirados <- c(148,288,257,258,278,192,60,53,311,300,152,330,
#                        106,123,169,241,269,301,51,118,284,240,220,287,
#                        79,98,127,297,143,298,142,303,323,333,306,176,
#                        275,304,184,326,307,97,289,308,101,222,234,245,
#                        205,243,247,242,254,263,163,133,162,190,259,191,
#                        217,228,252,166,244,251,113,119,128,167,272,285,
#                        147,138,155,146,159,305,161,321,164,282,291,192,
#                        265,283,294,324,188,213,312,276,227,302,195,280,
#                        198,210,207,209,208,231,211,214,215,216,224,229,
#                        286,232,233,248,255,271,268,273,274,290,292,293,
#                        313,316,317,319)
