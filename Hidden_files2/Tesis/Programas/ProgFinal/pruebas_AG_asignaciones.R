##########################################################################
#' En este programa se encuentra las pruebas de la función que aplica el
#' algoritmo genético a las asignaciones para encontrar una buena
#' asignación.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")


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


# AG_asignaciones ---------------------------------------------------------
AG_asignaciones <- function(mat_esqueleto,mat_solicitudes_real,param){
  ptm <- proc.time()# Start the clock!
  #Se definen las variables que se van a utilizar
  tam_poblacion <- param$tam_poblacion
  num_generaciones <- param$num_generaciones
  prob_mutacion <- param$prob_mutacion
  n_cols_mat_calif <- param$n_cols_mat_calif
  matrices_calif_x_generacion <- list()
  mejores_asig <- list()
  
  # ptm <- proc.time()# Start the clock!
  for(g in 1:num_generaciones){
    cat("\n *** GENERACIÓN ",g," ***")
    
    # ptm <- proc.time()# Start the clock!
    if(g == 1){#Población inicial
      ### 1) Generar población inicial y 2) Calificar
      lista_info_inicial <- poblacion_calif_iniciales(mat_esqueleto,
                                                      mat_solicitudes_real,
                                                      param)#5.22 min
      mat_calif_asig <- lista_info_inicial[[1]]
      poblacion <- lista_info_inicial[[2]]
      
      ### 12) Guardar una matriz con la calificación x gpo. de las
      #' asignaciones (como xiii de T45)
      matrices_calif_x_generacion[[g]] <- lista_info_inicial[[3]]
      
      ### 11) Guardar la mejor asignación de la generación
      ind_mejor_asig <- mat_calif_asig[tam_poblacion,1]
      mejores_asig[[g]] <- list(mat_calif_asig,
                                poblacion[[ind_mejor_asig]])
      
    }else{
      lista_info <- califica_ordena_asig(poblacion_nueva,param)
      mat_calif_asig <- lista_info[[1]]
      poblacion <- lista_info[[2]]
      
      ### 12) Guardar una matriz con la calificación x gpo. de las
      #' asignaciones (como xiii de T45)
      matrices_calif_x_generacion[[g]] <- lista_info[[3]]
      
      ### 11) Guardar la mejor asignación de la generación
      ind_mejor_asig <- mat_calif_asig[tam_poblacion,1]
      mejores_asig[[g]] <- list(mat_calif_asig,
                                poblacion[[ind_mejor_asig]])
    }
    
    for(n in 1:tam_poblacion){
      cat("\n *** HIJO ",n," ***")
      if(n == 1){poblacion_nueva <- list()}
      hijo <- data.frame(Materia = 0, Profesor = 0,TC = 0,
                         Horario = 0)
      ### 4) Elegir 2 padres con prob = 2i/(n*(n+1))
      (ind_padres <- elige_padres(mat_calif_asig))
      
      padre_1 <- poblacion[[ind_padres[1]]]
      padre_2 <- poblacion[[ind_padres[2]]]
      while(dim(padre_1)[1]!=0 && dim(padre_2)[1]!=0){
        # Repetir hasta que uno de los padres se quede sin genes.
        # cat("\n dim(padre_1)",dim(padre_1)[1])
        # cat("\n dim(padre_2)",dim(padre_2)[1])
        
        ### 5) Con prob = 0.5 se elige un padre
        (r_num_elige_padre <- runif(1))
        if(r_num_elige_padre < 0.5){
          padre_elegido <- padre_1
        }else{
          padre_elegido <- padre_2
        }
        
        ### 6) Elegir un gen (grupo) del padre seleccionado con prob = 2i/(n*(n+1))
        (gen_elegido <- elige_gen(padre_elegido))
        
        ### 7) Mutación
        (r_num_muta <- runif(1))
        if(r_num_muta < prob_mutacion){
          cat("\n Entra a mutación")
          (gen_elegido <- elige_gen_de_solicitud(mat_solicitudes_real,hijo,param))
        }
        hijo <- rbind(hijo,gen_elegido)
        hijo <- unique(hijo)#Para evitar repeticiones en los grupos
        
        ### 8) Ajustar información de los padres con respecto al nuevo gen del hijo
        lista_padres <- ajusta_genes_padres(padre_1,padre_2,gen_elegido)
        padre_1 <- lista_padres[[1]]
        padre_2 <- lista_padres[[2]]
      }#Fin while()
      #' Quitamos el renglón de ceros inicial
      hijo <- unique(hijo)#Para evitar repeticiones en los grupos
      hijo <- hijo %>% filter(Profesor != 0)
      
      ### 9) "Pegar" los genes restantes del otro padre al hijo
      if(dim(padre_1)[1] > 0){hijo <- rbind(hijo,padre_1)}
      if(dim(padre_2)[1] > 0){hijo <- rbind(hijo,padre_2)}
      
      ### 2) Calificar y 3) Ordenar las calificaciones del hijo
      esq_hijo <- gen_esq_hijo(hijo,param)
      lista_hijo <- list(hijo,esq_hijo)
      lista_calif_hijo <- califica_asignacion(mat_solicitudes_real,
                                              lista_hijo,param)
      poblacion_nueva[[n]] <- lista_calif_hijo
    }#Fin for(n)
    # cat("\nEl ciclo tardó: ",(proc.time()-ptm)[3]/60,
    #     " minutos. Para 1 generación \n")
    
  }#Fin for(g)
  # cat("\nEl ciclo tardó: ",(proc.time()-ptm)[3]/60,
  #     " minutos. Para ",num_generaciones," generaciones \n")
  ##126.403  min = 2hrs 6.4min - 5 generaciones
  
  # View(matrices_calif_x_generacion)
  # View(mejores_asig)
  # View(mejores_asig[[num_generaciones]])
  # View(mejores_asig[[num_generaciones]][[1]])
  # View(mejores_asig[[num_generaciones]][[2]])
  # View(mejores_asig[[1]][[1]])
  # mejores_asig[[1]][[1]]
  
  ### 13) Hacer heatmap de la matriz en 12)
  
  for(g in 1:num_generaciones){
    colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
    heatmap(matrices_calif_x_generacion[[g]][,1:650],
            Colv = NA, Rowv = NA,
            scale="none",col=colMain)
  }
  
  ### 14) Se define la asignación final
  mejor_asig <- mejores_asig[[num_generaciones]][[2]]
  mat_asignacion_final <- cbind(mejor_asig$Materia,
                                mejor_asig$Profesor,
                                mejor_asig$Horario)
  colnames(mat_asignacion_final) <- c("Materia","Profesor","Horario")
  # View(mat_asignacion_final)
  
  cat("\nLa función AG_asignaciones tardó: ",(proc.time()-ptm)[3]/60,
      " minutos\n")
  return(mat_asignacion_final)
}





# ajusta_genes_padres -----------------------------------------------------
padre_1 <- as.data.frame(rbind(c("Análisis Numérico","Antonio Carrillo Ledesma",0,7),
                               c("Administración Actuarial del Riesgo",
                                 "Oscar Lucio Cano Vaca",0,20),
                               c("Análisis Numérico","Antonio Carrillo Ledesma",0,8),
                               c("Seminario de Geometría A","Efraín Vega Landa",0,13),
                               c("Variable Compleja I","Alejandro Gaona Ordoñez",0,18)))
padre_2 <- as.data.frame(rbind(c("Análisis Numérico","Antonio Carrillo Ledesma",0,7),
                               c("Matemáticas Actuariales para Seguro de Daños, Fianzas y Reaseguro",
                                 "Oscar Lucio Cano Vaca",0,20),
                               c("Variable Compleja I","Alejandro Gaona Ordoñez",0,18)))


### Ambos padres tienen el mismo gen
gen_elegido <- c("Análisis Numérico","Antonio Carrillo Ledesma",0,7)

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


### Ambos padres tienen mismo profesor, misma hora pero diferente materia
gen_elegido <- c("Administración Actuarial del Riesgo",
                 "Oscar Lucio Cano Vaca",0,20)

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


### Un padre tiene el gen_elegido y el otro no
gen_elegido <- c("Seminario de Geometría A","Efraín Vega Landa",0,13)

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

### Ningún padre tiene el gen_elegido
gen_elegido <- c("Ecuaciones Diferenciales I","Alejandro Gaona Ordoñez",0,15)

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
