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
#' con esa materia. Se tiene una cota para que el número de grupos del hijo
#' no supere el número de grupos de mat_esqueleto.
#'
#' @param esq_hijo: Matriz de 15 renglones (horas) y 203 columnas
#' (materias). En la entrada (i,j) se tiene el número de grupos del hijo
#' para la hora i, y la materia j.
#' @param padre_1: Asignación elegida para crear un hijo.
#' @param padre_2: Asignación elegida para crear un hijo.
#' @param gen_elegido: Vector de 4 entradas (Materia,Profesor,TC,Horario)
#' con la información del gen del padre elegido.
#' @param mat_esqueleto: Matriz de 15 renglones (horas) y 203 columnas
#' (materias). En la entrada (i,j) se tiene el número de grupos simulados
#' para la hora i, y la materia j.
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
  cat("\n El padre 1 tiene ",dim(padre_1)[1]," genes. \n El padre 2 tiene ",
      dim(padre_2)[1]," genes")
  
  (num_materia_gen <- arroja_num_materia(as.character(gen_elegido[1])))
  (ind_hora_gen <- which(7:21 == as.numeric(gen_elegido[4])))
  ind_elim_1 <- numeric(0)
  ind_elim_2 <- numeric(0)
  if(esq_hijo[ind_hora_gen,num_materia_gen] >= mat_esqueleto[ind_hora_gen,
                                                             num_materia_gen]){
    cat("\nEl hijo tiene ",esq_hijo[ind_hora_gen,num_materia_gen],
        " grupos. \nEl esqueleto tiene ",
        mat_esqueleto[ind_hora_gen,num_materia_gen],"grupos.")
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
  }
  
  cat("\n El padre 1 tiene ",dim(padre_1)[1]," genes. \n El padre 2 tiene ",
      dim(padre_2)[1]," genes\n\n")
  
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


















