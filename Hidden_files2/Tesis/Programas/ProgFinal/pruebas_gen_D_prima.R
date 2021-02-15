##########################################################################
#' En este programa se encuentran algunas pruebas de la función
#' normalmixEM() para los datos de "Probabilidad I" y de "Administración
#' Actuarial del Riesgo". Con funciones.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")


# Parámetros iniciales ----------------------------------------------------
# param_sim$vec_sem_sig = 20202
# param_sim$k_sem_ant = 5
vec_s_sem_k_info <- gen_vec_s_sem_k_info(param_sim$vec_sem_sig,
                                         param_sim$k_sem_ant,param)
materia <- "Probabilidad I"
param_sim$Materias = materia
param_sim$m_filtrada <- gen_mat_m_filtrada(param,param_sim)
mat_al_corregidos_proba <- gen_mat_alumnos_corregidos(vec_s_sem_k_info,
                                                      param,param_sim)
# View(mat_al_corregidos_proba)

set.seed(1806)
(d_proba <- simula_alumnos(mat_al_corregidos_proba,param))
set.seed(8654)
(d_prima_proba <- simula_alumnos(mat_al_corregidos_proba,param))

wait_proba = c(as.vector(mat_al_corregidos_proba),d_prima_proba)
mixmdl_proba = normalmixEM(wait_proba,mean = mean(wait_proba))#16 iteraciones
mixmdl_proba$loglik#-511.6696

materia <- "Administración Actuarial del Riesgo"
# mat_nom_materias_total[1,1]#"Administración Actuarial del Riesgo"
param_sim$Materias = materia
param_sim$m_filtrada <- gen_mat_m_filtrada(param,param_sim)
mat_al_corregidos_adm_riesgo <- gen_mat_alumnos_corregidos(vec_s_sem_k_info,
                                                           param,param_sim)
# View(mat_al_corregidos_adm_riesgo)

set.seed(1806)
(d_adm_riesgo <- simula_alumnos(mat_al_corregidos_adm_riesgo,param))
set.seed(8654)
(d_prima_adm_riesgo <- simula_alumnos(mat_al_corregidos_adm_riesgo,param))

wait_adm_riesgo = c(as.vector(mat_al_corregidos_adm_riesgo),d_prima_adm_riesgo)
mixmdl_adm_riesgo = normalmixEM(wait_adm_riesgo,mean = mean(wait_adm_riesgo))#26it
mixmdl_adm_riesgo$loglik#-419.9702


mixmdl <- list()
mixmdl[[1]] <- mixmdl_proba
mixmdl[[2]] <- mixmdl_adm_riesgo

D <- cbind(d_proba,d_adm_riesgo)
D
D_prima <- cbind(d_prima_proba,d_prima_adm_riesgo)
D_prima

mat_calif_x_gpo <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])


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
          #La calificación es negativa si faltan alumnos
          #La calificación es positiva si sobran alumnos
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


# Ej.  --------------------------------------------------------------------

calif_esq <- actualiza_calif_esqueleto(D,D_prima,mat_calif_x_gpo)
mat_calif_x_gpo <- calif_esq[[1]]
vec_calif_x_materia <- calif_esq[[2]]



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

# Ej. ---------------------------------------------------------------------
cota <- 500

D_prima <- actualiza_D_prima(cota,D,D_prima,mixmdl,calif_esq)
D_prima







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
#' Title gen_D_prima: Función que actualiza "D_prima".
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


# Ej. ---------------------------------------------------------------------
cota <- 1000
D_prima_inicial <- D_prima
D_prima <- gen_D_prima(D,D_prima_inicial,mixmdl,cota)
D_prima



# Ej. todas las materias --------------------------------------------------
set.seed(1806)
D <- gen_mat_demanda_alumnos(param,param_sim)#39.95 seg
View(D)

set.seed(8654)
D_prima_inicial <- gen_mat_demanda_alumnos(param,param_sim)#39.95 seg
View(D_prima_inicial)


# param_sim$vec_sem_sig = 20202
# param_sim$k_sem_ant = 5
vec_s_sem_k_info <- gen_vec_s_sem_k_info(param_sim$vec_sem_sig,
                                         param_sim$k_sem_ant,param)
lista_mod_y_wait <- gen_normalmixEM_inicial(vec_s_sem_k_info,D_prima_inicial,
                                          param,param_sim)

cota <- 1000

ptm <- proc.time()# Start the clock!
D_prima <- gen_D_prima(D,D_prima_inicial,lista_mod_y_wait,cota)
cat("\nLa función gen_D_prima tardó: ",(proc.time()-ptm)[3],
    " segundos\n")##
View(D_prima)

D_menosDprima <- D - D_prima
colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
heatmap(D_menosDprima, Colv = NA, Rowv = NA, scale="none",col=colMain)

D_prima_inicial_menos_Dprima <- D_prima_inicial - D_prima
colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
heatmap(D_prima_inicial_menos_Dprima, Colv = NA, Rowv = NA, scale="none",col=colMain)









