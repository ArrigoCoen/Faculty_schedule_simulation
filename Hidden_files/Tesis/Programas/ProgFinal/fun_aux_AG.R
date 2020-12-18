##########################################################################
#' En este programa se encuentran las funciones que se utilizan en el
#' algoritmo genético de las asignaciones.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")



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
#' 4) lista_esq_aux: Lista de matrices esq_aux
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
  lista_esq_aux <- list()
  calif_asignacion_inicial <- rbind(1:tam_poblacion,rep(0,tam_poblacion))
  nombres_mat_calif <- rep(0,tam_poblacion)
  
  for(n in 1:tam_poblacion){
    cat("\n n = ", n)
    lista_asignacion <- gen_asignacion(mat_esqueleto,mat_solicitudes_real,
                                       param)#22.66 seg
    mat_asignacion <- lista_asignacion[[1]]
    lista_esq_aux[[n]] <- lista_asignacion[[2]]
    #Calificamos
    lista_calif_asignacion <- califica_asignacion(mat_solicitudes_real,
                                                  lista_asignacion,
                                                  param)
    mat_calif_asig_x_gpo <- lista_calif_asignacion[[1]]
    poblacion_inicial[[n]] <- mat_calif_asig_x_gpo
    calif_asignacion_inicial[2,n] <- lista_calif_asignacion[[2]]
    mat_calif_x_generacion[n,1:length(
      mat_calif_asig_x_gpo[,5])] <- mat_calif_asig_x_gpo[,5]
    # nombres_mat_calif[n] <- paste0("Asignación ",n)
  }
  
  ## 3) Ordenar de menor a mayor y definir la probabilidad acumulada
  calif_asignacion_inicial <- calif_asignacion_inicial[,order(
    calif_asignacion_inicial[2,])]
  mat_calif_asig_ini <- data.frame(ind_Asig = calif_asignacion_inicial[1,],
                                   Calif = calif_asignacion_inicial[2,],
                                   Prob_Ac = 0)
  mat_calif_asig_ini[1,3] <- (2*1)/(tam_poblacion*(tam_poblacion+1))
  for(r in 2:tam_poblacion){
    prob <- (2*r)/(tam_poblacion*(tam_poblacion+1))
    mat_calif_asig_ini[r,3] <- mat_calif_asig_ini[(r-1),3] + prob
  }
  
  lista_info_inicial <- list()
  lista_info_inicial[[1]] <- mat_calif_asig_ini#Matriz con las calificaciones de las asignaciones
  lista_info_inicial[[2]] <- poblacion_inicial#Lista de matrices con asignación y calificaciones
  lista_info_inicial[[3]] <- mat_calif_x_generacion#Matriz con calificaciones 1 generación
  lista_info_inicial[[4]] <- lista_esq_aux#Lista de matrices esq_aux
  
  cat("\nLa función poblacion_calif_iniciales tardó: ",
      (proc.time()-ptm)[3]/60," minutos\n")
  return(lista_info_inicial)
}




# elige_padres ------------------------------------------------------------
#' Title elige_padres: Función encargada de elegir 2 padres diferentes. Con
#' probabilidad de elección de 2i/(n*(n+1)), donde i = posición en la tabla
#' con respecto a la calificación. Entre mejor calificación, más probabilidad
#' de ser elegido.
#'
#' @param mat_calif_asig: Matriz de 3 columnas (ind_Asig,Calif,Prob_Ac)
#'
#' @return ind_padres: Vector de 2 entradas con los índices de las
#' asignaciones que se toman como padres.
#'
#' @examples
#' ind_padres <- elige_padres(mat_calif_asig)
#' 
elige_padres <- function(mat_calif_asig){
  #Se definen las variables que se van a utilizar
  (r_num_padre1 <- runif(1))
  (r_num_padre2 <- runif(1))
  ind_padres <- c(0,0)
  vec_prob_ac <- c(0,mat_calif_asig$Prob_Ac)
  padres_iguales <- 1
  
  while(padres_iguales == 1){
    #' Las asignaciones están ordenadas por calificación, pero no se ordenó
    #' la lista en la que están guardadas, por lo que se toma el índide
    #' de la asignación de acuerdo a la matriz "mat_calif_asig" que
    #' contiene esa información.
    for(r in 1:dim(mat_calif_asig)[1]){
      if(r_num_padre1>=vec_prob_ac[r] && 
         r_num_padre1<vec_prob_ac[(r+1)]){
        ind_padres[1] <- mat_calif_asig[r,1]
      }
      if(r_num_padre2>=vec_prob_ac[r] && 
         r_num_padre2<vec_prob_ac[(r+1)]){
        ind_padres[2] <- mat_calif_asig[r,1]
      }
    }#Fin for(r)
    if(ind_padres[1] == ind_padres[2]){
      (r_num_padre1 <- runif(1))
      (r_num_padre2 <- runif(1))
    }else{
      padres_iguales <- 0
    }
  }#Fin while()
  return(ind_padres)
}




# elige_gen ---------------------------------------------------------------
#' Title elige_gen: Función que elige un gen de un padre previamente
#' seleccionado. Un gen es un vector de 4 entradas (Materia,Profesor,TC,
#' Horario)
#'
#' @param padre_elegido: Asignación seleccionada para elegir un gen para el hijo.
#'
#' @return gen_elegido: Vector de 4 entradas (Materia,Profesor,TC,Horario)
#' con la información del gen del padre elegido.
#'
#' @examples
#' gen_elegido <- elige_gen(padre_elegido)
#' 
elige_gen <- function(padre_elegido){
  #Se definen las variables que se van a utilizar
  (r_num_gen <- runif(1))
  vec_prob_ac <- c(0,padre_elegido$Prob_Ac)
  
  for(r in 1:dim(padre_elegido)[1]){
    if(r_num_gen>=vec_prob_ac[r] && 
       r_num_gen<vec_prob_ac[(r+1)]){
      gen_elegido <- padre_elegido[r,1:4]
    }
  }#Fin for(r)
  return(gen_elegido)
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
  if(r_num_TC < param$elige_TC){
    mat_solicitudes <- mat_prof_TC
  }else{
    mat_solicitudes <- mat_prof_asig
  }
  
  (r_num_gen <- sample(1:dim(mat_solicitudes)[1],size = 1))
  (gen_elegido <- mat_solicitudes[r_num_gen,c(3,1,2,5)])
  prof <- as.character(gen_elegido[2])
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
#' con esa materia.
#'
#' @param padre_1: Asignación elegida para crear un hijo.
#' @param padre_2: Asignación elegida para crear un hijo.
#' @param gen_elegido: Vector de 4 entradas (Materia,Profesor,TC,Horario)
#' con la información del gen del padre elegido.
#'
#' @return lista_padres: Lista con los 2 padres actualizados.
#'
#' @examples
#' lista_padres <- ajusta_genes_padres(padre_1,padre_2,gen_elegido)
#' 
ajusta_genes_padres <- function(padre_1,padre_2,gen_elegido){
  #' Se quita la información de ese profesor a esa hora y
  #' ese profesor con esa materia.
  
  #' Padre 1
  (ind_prof_1 <- which(padre_1[,2] == as.character(gen_elegido[2])))
  (ind_hora_1 <- which(padre_1[,4] == as.character(gen_elegido[4])))
  (ind_materia_1 <- which(padre_1[,1] == as.character(gen_elegido[1])))
  (ind_1 <- intersect(ind_prof_1,union(ind_hora_1,ind_materia_1)))
  if(length(ind_1) > 0){
    padre_1 <- padre_1[-ind_1,]
  }
  
  #' Padre 2
  (ind_prof_2 <- which(padre_2[,2] == as.character(gen_elegido[2])))
  (ind_hora_2 <- which(padre_2[,4] == as.character(gen_elegido[4])))
  (ind_materia_2 <- which(padre_2[,1] == as.character(gen_elegido[1])))
  (ind_2 <- intersect(ind_prof_2,union(ind_hora_2,ind_materia_2)))
  if(length(ind_2) > 0){
    padre_2 <- padre_2[-ind_2,]
  }
  
  lista_padres <- list()
  lista_padres[[1]] <- padre_1
  lista_padres[[2]] <- padre_2
  return(lista_padres)
}




# gen_esq_hijo ------------------------------------------------------------
#' Title gen_esq_hijo: Función que genera el esqueleto de horarios del
#' hijo. Se define a partir de las asignaciones hechas en el hijo.
#'
#' @param hijo: Asignación que se crea a partir de 2 padres.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return esq_hijo: Matriz con el esqueleto de horarios del hijo.
#'
#' @examples
#' esq_hijo <- gen_esq_hijo(hijo,param)
#' 
gen_esq_hijo <- function(hijo,param){
  ptm <- proc.time()# Start the clock!
  #Se definen las variables que se van a utilizar
  esq_hijo <- matrix(0,nrow = length(param$Horas),
                     ncol = length(param$vec_nom_materias_total))
  rownames(esq_hijo) <- param$nombre_hrs
  colnames(esq_hijo) <- param$vec_nom_materias_total
  hijo <- data.frame(hijo,Num_Materia = 0)
  
  for(r in 1:dim(hijo)[1]){
    materia <- hijo$Materia[r]
    hijo$Num_Materia[r] <- arroja_num_materia(materia)
  }
  
  for(m in 1:length(param$vec_nom_materias_total)){
    materia <- param$vec_nom_materias_total[m]
    cat("\n Materia ",m,": ",materia)
    mat_materia <- hijo %>% filter(Materia == materia)
    for(h in 1:length(param$Horas)){
      hora <- param$Horas[h]
      mat_hora <- mat_materia %>% filter(Horario == hora)
      esq_hijo[h,m] <- dim(mat_hora)[1]
    }
  }
  cat("\nLa función gen_esq_hijo tardó: ",
      (proc.time()-ptm)[3]/60," minutos\n")
  return(esq_hijo)
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
                                   Calif = calif_asignacion[2,],
                                   Prob_Ac = 0)
  mat_calif_asig[1,3] <- (2*1)/(tam_poblacion*(tam_poblacion+1))
  for(r in 2:tam_poblacion){
    prob <- (2*r)/(tam_poblacion*(tam_poblacion+1))
    mat_calif_asig[r,3] <- mat_calif_asig[(r-1),3] + prob
  }
  
  lista_info <- list()
  lista_info[[1]] <- mat_calif_asig#Matriz con las calificaciones de las asignaciones
  lista_info[[2]] <- poblacion#Lista de matrices con asignación y calificaciones
  lista_info[[3]] <- mat_calif_x_generacion#Matriz con calificaciones 1 generación
  
  cat("\nLa función califica_ordena_asig tardó: ",
      (proc.time()-ptm)[3]/60," minutos\n")

  return(lista_info)
}


















