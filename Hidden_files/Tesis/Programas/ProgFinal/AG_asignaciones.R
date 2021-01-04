##########################################################################
#' En este programa se encuentra la función que aplica el algoritmo
#' genético a las asignaciones para encontrar una buena asignación.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")


# AG_asignaciones ---------------------------------------------------------
#' Title AG_asignaciones: Función que aplica el algoritmo genético a las
#' asignaciones para encontrar una buena asignación.
#'
#' @param mat_esqueleto: Matriz de 15 renglones (horas) y 203 columnas
#' (materias). En la entrada (i,j) se tiene el número de grupos simulados
#' para la hora i, y la materia j.
#' @param mat_solicitudes_real: Matriz de 5 columnas (Profesor,TC,Materia,
#' Num_Materia,Horario) y 6 renglones que tiene la información de la
#' solicitud de "nom_prof". Se eligen 2 materias y hasta 3 diferentes
#' horarios. Se quitan los renglones repetidos. Se hace una "intersección"
#' con los grupos simulados en la matriz "mat_esqueleto" y así se obtienen
#' las solicitudes pseudo-reales de los profesores.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
#' @return mat_asignacion_final: Matriz de 3 columnas (Materia,Profesor,
#' Horario). Contiene la asignación final encontrada con el algoritmo
#' genético.
#'
#' @examples
#' mat_asignacion_final <- AG_asignaciones(mat_esqueleto,
#' mat_solicitudes_real,mat_esqueleto_cotas,param)
#' 
AG_asignaciones <- function(mat_esqueleto,mat_solicitudes_real,
                            mat_esqueleto_cotas,param){
  ptm <- proc.time()# Start the clock!
  #Se definen las variables que se van a utilizar
  (tam_poblacion <- param$tam_poblacion)
  (num_generaciones <- param$num_generaciones)
  prob_mutacion <- param$prob_mutacion
  n_cols_mat_calif <- param$n_cols_mat_calif
  matrices_calif_x_generacion <- list()
  mejores_asig <- list()
  vec_prob_asig <- (2*(1:tam_poblacion))/(tam_poblacion*(tam_poblacion+1))
  calif_mejor_elem <- rep(0,(num_generaciones+1))
  colMain <- colorRampPalette(brewer.pal(8, "Blues"))(25)
  mat_calif_generaciones <- matrix(0,nrow = tam_poblacion,
                                   ncol = (num_generaciones+1))
  mat_num_genes <- matrix(0,nrow = tam_poblacion,
                          ncol = (num_generaciones+1))
  ptm <- proc.time()# Start the clock!
  # g <- 1
  # g <- 2
  for(g in 1:num_generaciones){
    cat("\n *** GENERACIÓN ",g," ***")
    
    # ptm <- proc.time()# Start the clock!
    if(g == 1){#Población inicial
      ### 1) Generar población inicial y 2) Calificar
      lista_info_inicial <- poblacion_calif_iniciales(mat_esqueleto,
                                                      mat_solicitudes_real,
                                                      param)#5.22/4.82 min
      mat_calif_asig <- lista_info_inicial[[1]]
      # pob_ini <- lista_info_inicial[[2]]
      poblacion <- lista_info_inicial[[2]]
      for (p in 1:tam_poblacion) {
        mat_num_genes[p,g] <- dim(poblacion[[p]])[1]
      }
      
      
      ### 12) Guardar una matriz con la calificación x gpo. de las
      #' asignaciones (como xiii de T45)
      matrices_calif_x_generacion[[g]] <- lista_info_inicial[[3]]
      
      ### 13) Hacer heatmap de la matriz en 12)
      heatmap(matrices_calif_x_generacion[[g]][,1:650],
              Colv = NA, Rowv = NA,
              main = paste0("Calificaciones ordenadas de generación ",g),
              scale="none",col=colMain)
      
      ### 11) Guardar la mejor asignación de la generación
      ind_mejor_asig <- mat_calif_asig[tam_poblacion,1]
      mejores_asig[[g]] <- list(mat_calif_asig,
                                poblacion[[ind_mejor_asig]])
      
      #Graficar datos
      calif_mejor_elem[g] <- mat_calif_asig[tam_poblacion,2]
      plot(calif_mejor_elem[1:g],main = "Calificaciones del mejor elemento",
           xlab = "Generación",ylab = "Calificación")
      mat_calif_generaciones[,g] <- mat_calif_asig[,2]
      matplot(mat_calif_generaciones[,1:g],type = "l",
              main = "Calificaciones de las asignaciones por generación",
              xlab = "Asignaciones",ylab = "Calificaciones")
      
      
      matplot(mat_num_genes[,1:g],type = "l",
              main = "Número de genes por generación",
              xlab = "Asignaciones",ylab = "Número de genes")
    }#Termina población inicial
    
    # ### 13) Hacer heatmap de la matriz en 12)
    # heatmap(matrices_calif_x_generacion[[g]][,1:650],
    #         Colv = NA, Rowv = NA,
    #         main = paste0("Calificaciones ordenadas de generación ",g),
    #         scale="none",col=colMain)
    # 
    # ### 11) Guardar la mejor asignación de la generación
    # ind_mejor_asig <- mat_calif_asig[tam_poblacion,1]
    # mejores_asig[[g]] <- list(mat_calif_asig,
    #                           poblacion[[ind_mejor_asig]])
    # 
    # #Graficar datos
    # calif_mejor_elem[g] <- mat_calif_asig[tam_poblacion,2]
    # plot(calif_mejor_elem[1:g],main = "Calificaciones del mejor elemento",
    #      xlab = "Generación",ylab = "Calificación")
    # mat_calif_generaciones[,g] <- mat_calif_asig[,2]
    # matplot(mat_calif_generaciones[,1:g],type = "l",
    #         main = "Calificaciones de las asignaciones por generación",
    #         xlab = "Asignaciones",ylab = "Calificaciones")
    
    # save(calif_mejor_elem,file = "calif_mejor_elem.RData")
    # save(mat_calif_generaciones,file = "mat_calif_generaciones.RData")
    # ptm_pob <- proc.time()# Start the clock!
    for(n in 1:tam_poblacion){
      cat("\n *** HIJO ",n," ***")
      if(n == 1){poblacion_nueva <- list()}
      hijo <- data.frame(Materia = 0, Profesor = 0,TC = 0,
                         Horario = 0)
      esq_hijo <- matrix(0,nrow = length(param$Horas),
                         ncol = length(param$vec_nom_materias_total))
      ### 4) Elegir 2 padres con prob = 2i/(n*(n+1))
      #' La selección es por "Rank Selection":
      #' a) Elegir 2 individuos aleatoriamente
      #' b) La asignación con la calificación más alta es el padre 1
      #' c) Repetir a) y b) para el padre 2
      #' Nota: Se toman los índices y no los números de las asignaciones
      #' porque se tiene el vector de probabilidades que depende de
      #' la posición en la que se encuentren las asignaciones.
      ind_padres <- c(0,0)
      (ind_mat_1 <- sample(x = 1:tam_poblacion,size = 2,
                         prob = vec_prob_asig))
      (ind_padres[1] <- max(ind_mat_1))
      (ind_mat_2 <- sample(x = 1:tam_poblacion,size = 2,
                           prob = vec_prob_asig))
      
      while(max(ind_mat_2) == max(ind_mat_1)){
        #' Para no tener al mismo padre dos veces
        (ind_mat_2 <- sample(x = 1:tam_poblacion,size = 2,
                             prob = vec_prob_asig))
        # cat("\n max(ind_mat_1) = ",max(ind_mat_1))
        # cat("\n max(ind_mat_2) = ",max(ind_mat_2))
      }
      (ind_padres[2] <- max(ind_mat_2))
      
      padre_1 <- poblacion[[ind_padres[1]]]
      padre_2 <- poblacion[[ind_padres[2]]]
      
      # write.csv(poblacion[[ind_padres[1]]], file = "padre_1.csv")
      # write.csv(poblacion[[ind_padres[2]]], file = "padre_2.csv")
      
      while(dim(padre_1)[1]!=0 && dim(padre_2)[1]!=0){
        # Repetir hasta que uno de los padres se quede sin genes.
        
        ### 5) Con prob = 0.5 se elige un padre
        (ind_padre_elegido <- sample(x=1:2,size = 1))
        cat("\n Se eligió al padre ",ind_padre_elegido)
        cat(paste("Se eligió al padre ",ind_padre_elegido),
            file="outfile.txt",sep="\n",append=TRUE)
        
        if(ind_padre_elegido == 1){
          padre_elegido <- padre_1
        }else{
          padre_elegido <- padre_2
        }
        
        ### 6) Elegir un gen (grupo) del padre seleccionado con prob = 2i/(n*(n+1))
        (num_genes <- dim(padre_elegido)[1])
        vec_prob_genes <- (2*(1:num_genes))/(num_genes*(num_genes+1))
        (ind_gen <- sample(x = 1:num_genes,
                              size = 1,
                              prob = vec_prob_genes))
        (gen_elegido <- padre_elegido[ind_gen,1:4])
        
        ### 7) Mutación
        (r_num_muta <- runif(1))
        if(r_num_muta < prob_mutacion){
          cat("\n Entra a mutación")
          cat("Entra a mutación",file="outfile.txt",sep="\n",append=TRUE)
          
          (gen_elegido <- elige_gen_de_solicitud(mat_solicitudes_real,
                                                 hijo,param))
        }
        hijo <- rbind(hijo,gen_elegido)
        (num_materia_gen <- arroja_num_materia(as.character(gen_elegido[1])))
        (ind_hora_gen <- which(7:21 == as.numeric(gen_elegido[4])))
        esq_hijo[ind_hora_gen,num_materia_gen] <- esq_hijo[ind_hora_gen,
                                                           num_materia_gen] + 1
        
        ### 8) Ajustar información de los padres con respecto al nuevo
        ###gen del hijo
        # (num_materia_gen <- arroja_num_materia(as.character(gen_elegido[1])))
        # num_max_gpos[2,num_materia_gen] <- num_max_gpos[2,num_materia_gen] + 1
        lista_padres <- ajusta_genes_padres(esq_hijo,padre_1,padre_2,
                                            gen_elegido,mat_esqueleto)
        # lista_padres <- ajusta_genes_padres(esq_hijo,padre_1,padre_2,
        #                                     gen_elegido,mat_esqueleto_cotas)
        padre_1 <- lista_padres[[1]]
        padre_2 <- lista_padres[[2]]
      }#Fin while()
      #' Quitamos el renglón de ceros inicial
      hijo <- unique(hijo)#Para evitar repeticiones en los grupos
      hijo <- hijo %>% filter(Profesor != 0)
      
      ### 9) Añadir los genes restantes del otro padre al hijo
      if(dim(padre_1)[1] > 0){hijo <- rbind(hijo,padre_1[,1:4])}
      if(dim(padre_2)[1] > 0){hijo <- rbind(hijo,padre_2[,1:4])}
      
      ### 2) Calificar y 3) Ordenar las calificaciones del hijo
      # esq_hijo <- gen_esq_hijo(hijo,param)#Grupos con profesor en el hijo
      lista_hijo <- list(hijo,esq_hijo)
      lista_calif_hijo <- califica_asignacion(mat_esqueleto,
                                              mat_solicitudes_real,
                                              lista_hijo,param)
      poblacion_nueva[[n]] <- lista_calif_hijo
    }#Fin for(n)
    # cat("\nEl ciclo tardó: ",(proc.time()-ptm_pob)[3]/60,
    #     " minutos. Para 1 generación \n")#15/37.83min
    
    lista_info <- califica_ordena_asig(poblacion_nueva,param)
    mat_calif_asig <- lista_info[[1]]
    poblacion <- lista_info[[2]]
    for (p in 1:tam_poblacion) {
      mat_num_genes[p,(g+1)] <- dim(poblacion[[p]])[1]
    }
    ### 12) Guardar una matriz con la calificación x gpo. de las
    #' asignaciones (como xiii de T45)
    matrices_calif_x_generacion[[(g+1)]] <- lista_info[[3]]
    
    ### 13) Hacer heatmap de la matriz en 12)
    heatmap(matrices_calif_x_generacion[[(g+1)]][,1:650],
            Colv = NA, Rowv = NA,
            main = paste0("Calificaciones ordenadas de generación ",g+1),
            scale="none",col=colMain)
    
    ### 11) Guardar la mejor asignación de la generación
    ind_mejor_asig <- mat_calif_asig[tam_poblacion,1]
    mejores_asig[[(g+1)]] <- list(mat_calif_asig,
                              poblacion[[ind_mejor_asig]])
    
    #Graficar datos
    calif_mejor_elem[(g+1)] <- mat_calif_asig[tam_poblacion,2]
    plot(calif_mejor_elem[1:(g+1)],
         main = "Calificaciones del mejor elemento",
         xlab = "Generación",ylab = "Calificación")
    mat_calif_generaciones[,(g+1)] <- mat_calif_asig[,2]
    matplot(mat_calif_generaciones[,1:(g+1)],type = "l",
            main = "Calificaciones de las asignaciones por generación",
            xlab = "Asignaciones",ylab = "Calificaciones")
    
    matplot(mat_num_genes[,1:(g+1)],type = "l",
            main = "Número de genes por generación",
            xlab = "Asignaciones",ylab = "Número de genes")
  }#Fin for(g)
  cat("\nEl ciclo tardó: ",(proc.time()-ptm)[3]/60,
      " minutos. Para ",num_generaciones," generaciones \n")
  ##126.403 min = 2hrs 6.4min - 5 generaciones
  ##47.87 min - 3 generaciones
  ##159.75 min - 6 generaciones
  ##171.17 min = 2hrs 51.17min - 6 generaciones
  ##147.55 min = 2hrs 51.17min - 6 generaciones
  ##71.68 min = 1hr 11.68min - 3 generaciones
  ## min = hr min - 3 generaciones
  
  # View(matrices_calif_x_generacion)
  # View(mejores_asig)
  # View(mejores_asig[[num_generaciones]])
  # View(mejores_asig[[num_generaciones]][[1]])
  # View(mejores_asig[[num_generaciones]][[2]])
  # View(mejores_asig[[1]][[1]])
  # mejores_asig[[1]][[1]]
  # mejores_asig[[2]][[1]]
  # mejores_asig[[3]][[1]]
  # mejores_asig[[4]][[1]]
  # mejores_asig[[5]][[1]]
  # mejores_asig[[6]][[1]]
  
  ### 14) Se define la asignación final
  mejor_asig <- mejores_asig[[(num_generaciones+1)]][[2]]
  mat_asignacion_final <- cbind(mejor_asig$Materia,
                                mejor_asig$Profesor,
                                mejor_asig$Horario)
  colnames(mat_asignacion_final) <- c("Materia","Profesor","Horario")
  # View(mat_asignacion_final)
  # save(mat_asignacion_final,file = "mat_asignacion_final.RData")
  
  list_asignacion_final <- list()
  list_asignacion_final[[1]] <- mat_asignacion_final
  list_asignacion_final[[2]] <- calif_mejor_elem #Vector con calificaciones
  #de los mejores elementos por generación
  list_asignacion_final[[3]] <- mat_calif_generaciones #Matriz con
  #calificaciones de todos los elementos de todas las generaciones
  list_asignacion_final[[4]] <- matrices_calif_x_generacion #Lista de
  #' tamaño num_generaciones+1 con las matrices de calificaciones ordenadas
  #' por generación .
  list_asignacion_final[[5]] <- mejores_asig #Lista de tamaño
  #' num_generaciones+1 con la información de los mejores hijos de
  #' cada generación.
  names(list_asignacion_final) <- c("mat_asignacion_final",
                                    "calif_mejor_elem",
                                    "mat_calif_generaciones",
                                    "matrices_calif_x_generacion",
                                    "mejores_asig")
  cat("\nLa función AG_asignaciones tardó: ",(proc.time()-ptm)[3]/60,
      " minutos\n")
  return(list_asignacion_final)
}


# Ej. ---------------------------------------------------------------------

list_asignacion_final <- AG_asignaciones(mat_esqueleto,mat_solicitudes_real,
                                         mat_esqueleto_cotas,
                                         param)#186.64 min
mat_asignacion_final <- list_asignacion_final[[1]]
calif_mejor_elem <- list_asignacion_final[[2]]
mat_calif_generaciones <- list_asignacion_final[[3]]
matrices_calif_x_generacion <- list_asignacion_final[[4]]
mejores_asig <- list_asignacion_final[[5]]


View(mat_asignacion_final)


View(mejores_asig)#Lista con la info de los mejores hijos x generación





# Tabla con info de AG ----------------------------------------------------
mat_info_AG <- data.frame(Num_generaciones = c(3,3,5,6),
                          Tam_pob = c(5,15,10,14), Tiempo = 0,
                          Mejor_calif = 0, Num_genes_asig_fin = 0,
                          Calif_asig_fin = 0, Prom_genes_gen1 = 0,
                          Prom_genes_generaciones = 0)






