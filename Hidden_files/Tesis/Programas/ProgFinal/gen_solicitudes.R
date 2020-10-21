##########################################################################
#' En este programa se encuentran las funciones encargadas de generar la
#' matriz con las solicitudes de los profesores.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")


















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