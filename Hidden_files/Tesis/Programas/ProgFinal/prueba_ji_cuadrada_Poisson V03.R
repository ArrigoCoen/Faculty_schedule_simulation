##########################################################################
#' En este programa se encuentra un ejemplo de la obtención de los valores
#' de la prueba de la Ji-Cuadrada para la bondad de ajuste.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Programas/ProgFinal V01")


# obtener_Ti --------------------------------------------------------------
#' Title obtener_Ti: Función en la que se disminuyen las clases para obtemer
#' los valores finales.
#'
#' @param tabla_clases: Tabla que contiene la información por intervalo.
#'
#' @return tabla_clases: Tabla que contiene la información por intervalo.
#'
#' @examples
#' tabla_clases <- obtener_Ti(tabla_clases) 
#' 
obtener_Ti <- function(tabla_clases){
  ##Se definen las variables que se van a utilizar
  mat_nom_clases <- matrix(c(0,15,
                             16,30,
                             31,45,
                             46,60,
                             61,79,
                             80,360),ncol = 2,byrow = T)
  vec_nom_clases <- rep(0,dim(mat_nom_clases)[1])
  for(d in 1:length(vec_nom_clases)){
    vec_nom_clases[d] <- paste0("[",mat_nom_clases[d,1],",",mat_nom_clases[d,2],"]")
  }
  tab_aux <- matrix(0,nrow = length(vec_nom_clases),ncol = 3)
  rownames(tab_aux) <- vec_nom_clases
  colnames(tab_aux) <- c("Oi","ei","Ti")
  
  #Lenamos el primer renglón
  # entrada_1 <- tabla_clases[tabla_clases[,1],]
  o1 <- sum(tabla_clases[1:16,2])
  e1 <- sum(tabla_clases[1:16,4])
  T1 <- (o1-e1)^{2}/e1
  tab_aux[1,] <- c(o1,e1,T1)
  
  for(r in 2:length(vec_nom_clases)){
    mat_datos_aux_1 <- tabla_clases[tabla_clases[,1]>=mat_nom_clases[r,1],]
    mat_datos_aux_2 <- mat_datos_aux_1[mat_datos_aux_1[,1]<=mat_nom_clases[r,2],]
    
    oi <- sum(mat_datos_aux_2[,2])
    ei <- sum(mat_datos_aux_2[,4])
    Ti <- (o1-e1)^{2}/e1
    tab_aux[r,] <- c(oi,ei,Ti)
  }
  
  
  
  for(c in 2:k){#Recorre las columnas
    mat_datos_aux_1 <- frec_datos[as.numeric(frec_datos$NumAlum)>vec_seq[c],]
    mat_datos_aux_2 <- mat_datos_aux_1[as.numeric(mat_datos_aux_1[,1])<=vec_seq[c+1],]
    tabla_clases[1,c] <- sum(mat_datos_aux_2[,2])
  }
  
  
  
  renglon_Oi <- tabla_clases[1,]
  renglon_ei <- tabla_clases[3,]
  renglon_Ti <- rep(0,length(renglon_ei))
  
  for(d in 1:length(renglon_Ti)){
    renglon_Ti[d] <- (renglon_Oi[d]-renglon_ei[d])^{2}/renglon_ei[d]
  }
  
  tabla_clases <- rbind(tabla_clases,renglon_Ti)
  
  return(tabla_clases)
}


# prueba_ji_cuadrada_Poisson ----------------------------------------------
#' Title prueba_ji_cuadrada_Poisson: Función que arroja el estadístico T
#' de la prueba de la Ji-Cuadrada para Bondad de Ajuste. Veremos si los
#' datos tienen una distribución Poisson.
#'
#' @return est_T: Estadístico T de la prueba de la Ji-Cuadrada para Bondad
#' de Ajuste.
#'
#' @examples
#' est_T <- prueba_ji_cuadrada_Poisson()
#' 
prueba_ji_cuadrada_Poisson <- function(){
  #' Se cargan los datos del semestre 2008-1 al 2020-1 del número de alumnos
  #' por grupo.
  load("vec_al_x_gpo_todos_sem.RData")
  
  ##Se definen las variables que se van a utilizar
  datos <- as.data.frame(table(vec_al_x_gpo_todos_sem))
  frec_datos <- data.frame(NumAlum = as.numeric(as.character(datos$vec_al_x_gpo_todos_sem)),
                           Freq = datos$Freq)
  colnames(frec_datos) <- c("NumAlum","Freq")
  k <- 81
  vec_nom_clases <- rep(0,k)
  for(d in 1:length(vec_nom_clases)){
    vec_nom_clases[d] <- paste0("Clase_",d)
  }
  
  #' La siguiente tabla va a contener la información por clase:
  tabla_clases <- as.data.frame(matrix(0,nrow = k,ncol = 4))
  rownames(tabla_clases) <- vec_nom_clases
  colnames(tabla_clases) <- c("NumAlum","Oi","pi","ei")
  
  # Se ponen los valores en la primer columna
  # tabla_clases[,1] <- c(frec_datos[1:(k-1),1],paste0(">=",frec_datos[k,1]))
  tabla_clases[,1] <- frec_datos[1:k,1]
  
  # Se obtiene la columna de las Oi's
  tabla_clases[,2] <- c(frec_datos[1:(k-1),2],
                        sum(frec_datos[k:dim(frec_datos)[1],2]))
  n <- sum(tabla_clases[,2])
  
  EMV_lambda <- mean(frec_datos[,1])#97.7619
  # EMV_lambda <- sum(tabla_clases[,1]*tabla_clases[,2])/n#32.72301
  
  # Se obtiene la columna de las pi's
  tabla_clases[,3] <- c(dpois(x = frec_datos[1:(k-1),1], lambda = EMV_lambda),
                        ppois(frec_datos[(k-1),1],lambda = EMV_lambda,lower.tail = F))
  
  # Se obtiene la columna de las ei's
  tabla_clases[,4] <- n*tabla_clases[,3]
  
  
  # Se obtiene la columna de las Ti's
  tabla_clases <- obtener_Ti(tabla_clases)
  rownames(tabla_clases) <- c("Oi","pi","ei","Ti")
  
  est_T <- sum(tabla_clases[4,])
  return(est_T)
}



# Ej. ---------------------------------------------------------------------
qchisq(0.95,14)
qchisq(0.95,4)
# View(tabla_clases)


View(t(tabla_clases))







