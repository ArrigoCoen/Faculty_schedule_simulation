##########################################################################
#' En este programa se encuentra las funciones encargadas de corregir los
#' nombres de las materias para que no haya repeticiones.
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
  
  for(d in 1:length(vec_nom_materias_total)){
    cat("\n = ",d)
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
  
}




# PRUEBAS -----------------------------------------------------------------
mat_nombres_carreras <- mat_nombres_carreras[order(mat_nombres_carreras[,2]),]

materias_extras <- setdiff(1:length(vec_nom_materias_total),mat_nombres_carreras[,2])
length(materias_extras)#104
View(vec_nom_materias_total[materias_extras])

mat_nom_materias[223,3] <- "Sistemas Dinámicos Discretos II"
View(mat_nom_materias)


vec_nom_materias_aux <- vec_nom_materias_total
mat_nom_materias_total <- data.frame(Materia = 0,
                                     Num_Materia = 1:length(vec_nom_materias_total),
                                     Nom1 = 0,Nom2 = 0,Nom3 = 0,Nom4 = 0,Nom5 = 0,
                                     Nom6 = 0,Nom7 = 0,Nom8 = 0,Nom9 = 0,Nom10 = 0,
                                     Nom11 = 0,Nom12 = 0,Nom13 = 0,Nom14 = 0,Nom15 = 0,
                                     Nom16 = 0,Nom17 = 0,Nom18 = 0,Nom19 = 0,Nom20 = 0)

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

nom_rec_bus_info_text <- mat_nom_materias[c(3,257),]
nom_rec_bus_info_text <- nom_rec_bus_info_text[,c(1,3:20)]
nom_rec_bus_info_text <- unique(nom_rec_bus_info_text[nom_rec_bus_info_text!=0])
mat_nom_materias_total[3,1:(2+length(nom_rec_bus_info_text))] <- 
  c("Recuperación y Búsqueda de Información en Textos",3,nom_rec_bus_info_text)
mat_nom_materias_total[c(257),] <- rep("X",dim(mat_nom_materias_total)[2])

mat_nom_materias_total[4,] <- mat_nom_materias[4,]
mat_nom_materias_total[4,8] <- "Temas Selectos de Ingeniería de Software A"
mat_nom_materias_total[c(192,258,278),] <- rep("X",dim(mat_nom_materias_total)[2])

mat_nom_materias_total[60,5] <- "Probabilidad y Estadística"
mat_nom_materias_total[5,] <- rep("X",dim(mat_nom_materias_total)[2])

mat_nom_materias_total[260,] <- rep("X",dim(mat_nom_materias_total)[2])

mat_nom_materias_total[11,4] <- "Electromagnetismo II/Electromagnetismo II/Electromagnetismo II"

nom_fun_esp <- mat_nom_materias[c(12,53),]
nom_fun_esp <- nom_fun_esp[,c(1,3:20)]
nom_fun_esp <- unique(nom_fun_esp[nom_fun_esp!=0])
mat_nom_materias_total[12,1:(2+length(nom_fun_esp))] <- 
  c("Funciones Especiales y Transformadas Integrales",12,nom_fun_esp)
mat_nom_materias_total[53,] <- rep("X",dim(mat_nom_materias_total)[2])

mat_nom_materias_total[16,4] <- "Mecánica Cuántica/Mecánica Cuántica/Mecánica Cuántica"

mat_nom_materias_total[24,c(5,6)] <- 0

mat_nom_materias_total[26,c(5,6)] <- 0

mat_nom_materias_total[27,c(5,6)] <- 0
mat_nom_materias_total[310,c(4,5)] <- 0

mat_nom_materias_total[34,] <- mat_nom_materias[34,]
mat_nom_materias_total[34,5] <- 0

mat_nom_materias_total[36,1] <- "Introducción a las Matemáticas Discretas"
mat_nom_materias_total[311,] <- rep("X",dim(mat_nom_materias_total)[2])




# vec_nom_materias_aux <- vec_nom_materias_aux[-c(1,148,288,3,257)]
#



























# arroja_nom_correcto -----------------------------------------------------
arroja_nom_correcto <- function(param){
  #Se definen las variables que se van a utilizar
  vec_nom_materias_total <- param$vec_nom_materias_total
  m_grande_2015 <- param$m_grande_2015
  View(vec_nom_materias_total)
  View(m_grande_2015)
  
  df_nom_materias_total <- data.frame(Num_Materia = 1:length(vec_nom_materias_total),
                                      Materia = vec_nom_materias_total)
  
  
  
}


# corrige_nom_materias ----------------------------------------------------
corrige_nom_materias <- function(param){
  #Se definen las variables que se van a utilizar
  vec_nom_materias_total <- param$vec_nom_materias_total
  
  View(vec_nom_materias_total)
  View(m_grande_2015)
  
  
  
  
  
}



# actualiza_col_num_materia -----------------------------------------------
actualiza_col_num_materia <- function(){
  #Se definen las variables que se van a utilizar
  
  
}