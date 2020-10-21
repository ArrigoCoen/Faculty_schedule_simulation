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



# Obtención de Oi's -------------------------------------------------------
#' Title obtener_Oi: Función que arroja la tabla "tabla_clases" con el
#' renglón de Oi's.
#'
#' @param frec_datos: Matriz con los datos del semestre 2008-1 al 2020-1
#' del número de alumnos por grupo.
#' @param tabla_clases: Tabla que contiene la información por intervalo. Los
#' intervalos son: [0-10],[11-20],...,[121,130],[131,360]
#' Son 14 intervalos.
#' @param k: Número de celdas o clases en las que se dividen los datos.
#'
#' @return tabla_clases: Tabla con 14 columnas que contiene las frecuencias
#' de los datos por intervalos de longitud 10. i.e. en la primer columna
#' se tiene el número de grupos con número de alumnos entre 0 y 10, en la
#' 2° el # de grupos con # de alumnos entre 11 y 20, así sucesivamente. El
#' intervalo 14 tiene la información del número de grupos con # de alumnos
#' mayor a 130.
#'
#' @examples
#' tabla_clases <- obtener_Oi()
#' 
obtener_Oi <- function(frec_datos,tabla_clases,k){
  vec_seq <- c(seq(0,130,by = 10),360)
  
  for(c in 2:k){#Recorre las columnas
    mat_datos_aux_1 <- frec_datos[as.numeric(frec_datos$NumAlum)>vec_seq[c],]
    mat_datos_aux_2 <- mat_datos_aux_1[as.numeric(mat_datos_aux_1[,1])<=vec_seq[c+1],]
    tabla_clases[1,c] <- sum(mat_datos_aux_2[,2])
  }
  
  return(tabla_clases)
}



# obtener_pi --------------------------------------------------------------
#' Title obtener_pi: Función que arroja el renglón con las probabilidades de
#' cada intervalo.
#'
#' @param k: Número de celdas o clases en las que se dividen los datos.
#' @param EMV_lambda: Estimador máximo verosímil de lambda.
#'
#' @return renglon_pi: Vector con las probabilidades de cada intervalo.
#'
#' @examples
#' renglon_pi <- obtener_pi(k,EMV_lambda)
#' 
obtener_pi <- function(k,EMV_lambda){
  ##Se definen las variables que se van a utilizar
  renglon_pi <- rep(0,k)
  mat_intervalos <- matrix(c(11,20,
                             21,30,
                             31,40,
                             41,50,
                             51,60,
                             61,70,
                             71,80,
                             81,90,
                             91,100,
                             101,110,
                             111,120,
                             121,130),
                           ncol = 2,byrow = T)
  
  #Se llenan la primera y la última entradas del vector
  # renglon_pi[1] <- ppois(vec_seq[1],lambda = EMV_lambda)
  renglon_pi[1] <- sum(dpois(x = 0:10, lambda = EMV_lambda))
  # renglon_pi[k] <- 1 - ppois(130,lambda = EMV_lambda)
  renglon_pi[k] <- ppois(130,lambda = EMV_lambda,lower.tail = F)
  
  for(r in 1:dim(mat_intervalos)[1]){
    renglon_pi[r+1] <- sum(dpois(x = mat_intervalos[r,1]:mat_intervalos[r,2],
                                 lambda = EMV_lambda))
    
  }
  
  return(renglon_pi)
}



# obtener_Ti --------------------------------------------------------------
#' Title obtener_Ti: Función que arroja el renglón con las estadísticas de
#' cada intervalo.
#'
#' @param tabla_clases: Tabla que contiene la información por intervalo. Los
#' intervalos son: [0-10],[11-20],...,[121,130],[131,360]
#' Son 14 intervalos.
#'
#' @return tabla_clases: Tabla que contiene la información por intervalo. Los
#' intervalos son: [0-10],[11-20],...,[121,130],[131,360]
#' Son 14 intervalos.
#'
#' @examples
#' tabla_clases <- obtener_Ti(tabla_clases) 
#' 
obtener_Ti <- function(tabla_clases){
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
#' Title prueba_ji_cuadrada_Poisson: 
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
  # frec_datos$NumAlum
  # frec_datos$Freq
  n <- dim(frec_datos)[1]
  
  #' La siguiente tabla va a contener la información por intervalo. Los
  #' intervalos son: [0-10],[11-20],...,[121,130],[131,360]
  #' Son 14 intervalos.
  tabla_clases <- data.frame(Int_1=sum(frec_datos[1:11,2]),
                             Int_2=0,Int_3=0,Int_4=0,Int_5=0,
                             Int_6=0,Int_7=0,Int_8=0,Int_9=0,Int_10=0,
                             Int_11=0,Int_12=0,Int_13=0,Int_14=0)
  # vec_seq <- c(seq(0,130,by = 10),360)
  k <- dim(tabla_clases)[2]
  
  # Se obtiene el renglón de las Oi's
  tabla_clases <- obtener_Oi(frec_datos,tabla_clases,k)
  EMV_lambda <- sum(tabla_clases[1,])/n
  # EMV_lambda <- 34.18746
  
  # Se obtiene el renglón de las pi's
  renglon_pi <- obtener_pi(k,EMV_lambda)
  tabla_clases <- rbind(tabla_clases,renglon_pi)
  
  # Se obtiene el renglón de las ei's
  renglon_ei <- n*renglon_pi
  tabla_clases <- rbind(tabla_clases,renglon_ei)
  
  # Se obtiene el renglón de las Ti's
  tabla_clases <- obtener_Ti(tabla_clases)
  rownames(tabla_clases) <- c("Oi","pi","ei","Ti")
  
  est_T <- sum(tabla_clases[4,])
  return(est_T)
}








qchisq(0.95,14)
View(tabla_clases)








# División de 36 intervalos -----------------------------------------------
#' #' La siguiente tabla va a contener las frecuencias de los datos por
#' #' intervalos de longitud 10. i.e. en la primer columna se va tener el
#' #' número de grupos con número de alumnos entre 0 y 10, en la 2° el # de
#' #' grupos con # de alumnos entre 11 y 20, así sucesivamente.
#' tabla_clases <- data.frame(Int_1=sum(frec_datos[1:11,2]),
#'                            Int_2=0,Int_3=0,Int_4=0,Int_5=0,
#'                            Int_6=0,Int_7=0,Int_8=0,Int_9=0,Int_10=0,
#'                            Int_11=0,Int_12=0,Int_13=0,Int_14=0,Int_15=0,
#'                            Int_16=0,Int_17=0,Int_18=0,Int_19=0,Int_20=0,
#'                            Int_21=0,Int_22=0,Int_23=0,Int_24=0,Int_25=0,
#'                            Int_26=0,Int_27=0,Int_28=0,Int_29=0,Int_30=0,
#'                            Int_31=0,Int_32=0,Int_33=0,Int_34=0,Int_35=0,
#'                            Int_36=0)
#' vec_seq <- seq(0,360,by = 10)
#' k <- dim(tabla_clases)[2]
#' for(c in 2:k){#Recorre las columnas
#'   # datos_aux <- frec_datos[frec_datos[1,]>vec_seq[c] && frec_datos[1,]<=vec_seq[c+1],]
#'   mat_datos_aux_1 <- frec_datos[as.numeric(frec_datos[,1])>vec_seq[c],]
#'   mat_datos_aux_2 <- datos_aux_1[as.numeric(datos_aux_1[,1])<=vec_seq[c+1],]
#'   
#'   tabla_clases[1,c] <- sum(mat_datos_aux_2[,2])
#' }

