##########################################################################
#' En este programa se encuentra la funciones que generan una matriz con
#' las asignaciones de Materia-Profesor-Horario.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")


# asignacion_1_materia ---------------------------------------------------
#' Title: asignacion_1_materia: Función que genera la asignación de una 
#' materia con profesor por hora, dependiendo del número de grupos simulados
#' para el siguiente semestre.
#'
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#' @return mat_asignacion: Matriz de 4 columnas (Materia,Profesor,Horario,
#' Salón), la cual tiene la información de las asignaciones generadas de
#' las simulaciones tanto del número de grupos como de las elecciones de
#' los profesores.
#'
asignacion_1_materia <- function(mat_solicitudes,materia,param){
  # m_grande_total = param$m_grande_total
  # num_col <- arroja_ind_col_MG("Profesor")
  # Profesores <- sort(unique(m_grande_total[,2]))
  # Profesores <- unique(m_grande_total[,num_col])
  #Quitamos las entradas que sean iguales a cero en caso de existir
  # Profesores <- Profesores[Profesores!=0]
  mat_nom_prof_total <- param$mat_nom_prof_total
  Profesores <- mat_nom_prof_total[,1]
  num_profes <- length(Profesores)
  # mat_prof_materia_horario <- cbind(Profesores,mat_solicitudes)
  # colnames(mat_prof_materia_horario) <- c("Profesores","Materia_1","Materia_2",
  #                                         "Materia_3","Materia_4","Materia_5",
  #                                         "Materia_6","Horario_1","Horario_2",
  #                                         "Horario_3","Horario_4","Horario_5",
  #                                         "Horario_6")
  # gpos_simulados <- simula_grupos(materia,param)
  
  mat_asignacion <- matrix(0,nrow = sum(gpos_simulados),ncol = 4)
  colnames(mat_asignacion) <- c("Materia","Profesor","Horario","Salón")
  
  mat_asignacion[,1] <- materia ##Se llena la columna "Materia"
  
  ##Se llena la columna "Horario"
  if(sum(gpos_simulados)==0){
    vec_num_horas <- rep(0,sum(gpos_simulados))
    ## Marcamos con -1 la variable para que al generar la matriz con toda la
    ##información no haya error.
    mat_asignacion <- -1
  }else{
    i <- 1
    vec_num_horas <- rep(0,sum(gpos_simulados))
    for(k in 1:length(gpos_simulados)) {
      # cat("\n k = ",k)
      if(gpos_simulados[k]>0){
        # cat("\n  i = ",i)
        n_gpos <- gpos_simulados[k]
        mat_asignacion[i:(i+n_gpos-1),3] <- param$nombre_hrs[k]
        vec_num_horas[i:(i+n_gpos-1)] <- param$Horas[k]
        i <- i + n_gpos}}}
  
  ##Se llena la columna "Profesor"
  if(length(mat_asignacion)>1){##Verifica que haya información en la matriz
    datos_de_materia <- matrix(0,ncol = 7,nrow = num_profes)
    colnames(datos_de_materia) <- c("Profesor","Horario1","Horario2",
                                    "Horario3","Horario4","Horario5","Horario6")
    for(m in 1:num_profes){
      if(mat_solicitudes[m,1]==materia||
         mat_solicitudes[m,2]==materia||
         mat_solicitudes[m,3]==materia||
         mat_solicitudes[m,4]==materia||
         mat_solicitudes[m,5]==materia||
         mat_solicitudes[m,6]==materia){
        datos_de_materia[m,] <- mat_prof_materia_horario[m,c(1,8:13)]}}
    datos_de_materia <- unique(datos_de_materia)
    
    for(k in 1:length(vec_num_horas)){
      # cat("\nk = ",k)
      while(mat_asignacion[k,2] == 0){
        for(i in 1:dim(datos_de_materia)[1]){
          if(datos_de_materia[i,1] != 0){
            for(j in 2:7){
              if(datos_de_materia[i,j]==vec_num_horas[k] && mat_asignacion[k,2]==0){
                mat_asignacion[k,2] <- datos_de_materia[i,1]
                ##Parar evitar que un mismo profesor dé más de una clase a la misma
                #hora, se sustituye el valor de la matriz de datos por un cero.
                datos_de_materia[i,j] <- 0}}}}
        if(mat_asignacion[k,2] == 0){
          mat_asignacion[k,2] <- -1
        }
      }##Fin de while
    }##Fin de for(k in 1:length(vec_num_horas))
  }##Fin de if(length(mat_asignacion)>1)
  
  return(mat_asignacion)
}


# gen_asignacion ----------------------------------------------------------
#' Title gen_asignacion: Función que genera asignaciones de materia con
#' profesor por hora, dependiendo del número de grupos simulados para el
#' siguiente semestre, con la información de solicitudes que se obtiene de
#' la función "gen_solicitudes". 
#'
#' @param mat_solicitudes: Matriz de 12 columnas que contiene la
#' información de las solicitudes de materia y de horario de todos los
#' profesores, en las primeras 6 columnas se tiene la información
#' de la simulación de elección de materias y en las últimas 6 columnas
#' se tiene la información de la simulación de elección de horarios,
#' la matriz puede no estar completamente llena),tiene como renglones
#' los nombres de los profesores.
#' @param mat_esqueleto: Matriz de 15 renglones con las horas (7-8,8-9,...,
#' 21-22) y tantas columnas como materias impartidas en el semestre actual.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' 
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90)
#'
#' @return mat_asignaciones: Matriz de cuatro columnas (Materia, Profesor,
#' Horario, Salón) la cual contiene en el i-ésimo renglón la asignación
#' por materia, profesor y horario //está pendiente la asignación de salón//
#'
gen_asignacion <- function(mat_esqueleto,mat_solicitudes,param){
  # Start the clock!
  ptm <- proc.time()
  
  ##Se inicializan las variables
  # m_grande_total = param$m_grande_total
  num_col <- arroja_ind_col_MG("Materia")
  # Materias <- sort(unique(m_grande_total[,1]))
  # Materias <- unique(m_grande_total[,num_col])
  # load("vec_nom_materias_total.RData")
  Materias <- param$vec_nom_materias_total
  #Quitamos las entradas que sean iguales a cero en caso de existir
  # Materias <- Materias[Materias!=0]
  num_materias <- length(Materias)
  
  # Para inicializar la matriz que se va a llenar con la información,
  #se suman los grupos totales obtenidos en la matriz "mat_esqueleto"
  #y se le suman 1000 renglones para tener una base y que la matriz no
  #incremente su tamaño en cada iteración de llenado.
  gpos_simulados <- sum(mat_esqueleto)
  mat_asignaciones <- matrix(0,nrow = gpos_simulados+1000, ncol = 4)
  
  i <- 1
  vec_para_for <- 1:num_materias
  pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
  for(m in vec_para_for){
    setTxtProgressBar(pb, m)
    # cat("\n Materia = ",m," de ",num_materias)
    # cat("\n  i = ",i)
    mat_asignacion <- asignacion_1_materia(mat_solicitudes,Materias[m],param)
    num_renglones <- dim(mat_asignacion)[1]
    
    if(length(mat_asignacion)==1){
      ## Si "mat_asignacion" tiene longitud de 1, implica que no hay grupos
      ##simulados para dicha materia
      i <- i + 1
    }else{
      for(d in 1:num_renglones){
        # cat("\n   d = ",d)
        mat_asignaciones[i,] <- mat_asignacion[d,]
        i <- i + 1
      }
    }
  }
  close(pb)
  
  ## Se eliminan los ceros de la matriz
  #No utilizar unique() porque no se conserva el número de grupos que
  #se requieren
  # mat_asignaciones <- unique(mat_asignaciones)
  mat_asignaciones <- matrix(mat_asignaciones[mat_asignaciones[,1]!=0],
                             ncol = 4)
  colnames(mat_asignaciones) <- c("Materia","Profesor","Horario","Salón")
  
  cat("La función gen_asignaciones tomó: ", (proc.time()-ptm)[3]/60," minutos\n\n\n" )
  
  ##ELIMINAR LOS PROFESORES QUE NO ESTÁN DISPONIBLES PARA DAR CLASES##
  return(mat_asignaciones)
}





# Pruebas -----------------------------------------------------------------
#' Tenemos de información mat_esqueleto y mat_solicitudes.
#' mat_esqueleto: Matriz de 15x203 con el número de grupos x hora y x materia
#' mat_solicitudes: Matriz de 5 columnas (Profesor,TC,Materia,numMateria,Horario)

mat_asignacion <- data.frame(Materia = 0, Profesor = 0, Horario = 0)
Materias <- param$vec_nom_materias_total
m <- 5
(materia <- Materias[m])
mat_aux_solicitud <- mat_solicitudes %>% filter(Materia == materia)

for(h in 1:length(param$Horas)){
  m_aux_esq <- mat_esqueleto[,m]
  num_gpos <- m_aux_esq[h]
  if(num_gpos > 0){#Si el # de gpos. simulados > 0
    m_aux <- mat_aux_solicitud %>% filter(Horario == param$Horas[h])
    
    if(dim(m_aux)[1] > num_gpos){
      mat_asignacion <- rbind(mat_asignacion,m_aux[sample(1:dim(m_aux)[1],
                                                          size = num_gpos),
                                                   c(3,1,5)])
    }
    
    renglon <- c(materia,,)
    mat_asignacion <- rbind(mat_asignacion,)
  }
}


# ajusta_mat_solicitudes --------------------------------------------------
#' Title ajusta_mat_solicitudes: Función que agrega más posibles horarios a
#' los profesores de tiempo completo.
#'
#' @param mat_solicitudes: Matriz de 5 columnas (Profesor,TC,Materia,
#' Num_Materia,Horario) que tiene la información de las solicitudes de los
#' profesores.
#'
#' @return mat_solicitudes_ajustada: Matriz de 5 columnas (Profesor,TC,
#' Materia,Num_Materia,Horario) que tiene la información de las solicitudes
#' de los profesores. Se agregan más posibles horarios a los profesores de
#' tiempo completo.
#'
#' @examples
ajusta_mat_solicitudes <- function(mat_solicitudes){
  #Se definen las variables que se van a utilizar
  mat_solicitudes_TC <- mat_solicitudes %>% filter(TC == 1)
  mat_solicitudes_asig <- mat_solicitudes %>% filter(TC == 0)
  mat_TC <- data.frame(Profesor = 0,TC = 0,Materia = 0,Num_Materia = 0,
                       Horario = 0)
  
  for(r in 1:dim(mat_solicitudes_TC)[1]){
    hora <- as.numeric(mat_solicitudes_TC[r,5])
    if(hora == 7){
      r_aux <- mat_solicitudes_TC[r,]
      r_aux[5] <- hora + 1
    }else if(hora == 21){
      r_aux <- mat_solicitudes_TC[r,]
      r_aux[5] <- hora - 1
    }else  if(hora<21 && hora>7){
      r_aux_1 <- mat_solicitudes_TC[r,]
      r_aux_2 <- mat_solicitudes_TC[r,]
      r_aux_1[5] <- hora - 1
      r_aux_2[5] <- hora + 1
      r_aux <- rbind(r_aux_1,r_aux_2)
    }
    mat_aux <- rbind(mat_solicitudes_TC[r,],r_aux)
    mat_TC <- rbind(mat_TC,mat_aux)
  }#Fin for(r)
  mat_TC <- mat_TC %>% filter(Horario != 0)
  
  mat_solicitudes_ajustada <- rbind(mat_TC,mat_solicitudes_asig)
  mat_solicitudes_ajustada <- unique(mat_solicitudes_ajustada)
  
  return(mat_solicitudes_ajustada)
}


# Ej. ---------------------------------------------------------------------

mat_solicitudes_ajustada <- ajusta_mat_solicitudes(mat_solicitudes)
View(mat_solicitudes_ajustada)





# cuenta_asignaciones -----------------------------------------------------
cuenta_asignaciones <- function(mat_aux_solicitud,mat_asig){
  for(d in 1:dim(mat_asig)[1]){
    ind_prof <- which(mat_aux_solicitud[,1] == mat_asig[d,2])
    mat_aux_solicitud[ind_prof,6] <- as.numeric(mat_aux_solicitud[ind_prof,6]) + 1
    ind_2 <- which(mat_aux_solicitud[ind_prof,5] == mat_asig[d,3])
    mat_aux_solicitud[ind_prof[ind_2],6] <- 10
  }
  
  return(mat_aux_solicitud)
}



# Pruebas2 -----------------------------------------------------------------
#' Tenemos de información mat_esqueleto y mat_solicitudes_ajustada.
#' mat_esqueleto: Matriz de 15x203 con el número de grupos x hora y x materia
#' mat_solicitudes_ajustada: Matriz de 5 columnas (Profesor,TC,Materia,
#' numMateria,Horario). Tiene más posibles horarios para los profesores de TC.

mat_asignacion <- data.frame(Materia = 0, Profesor = 0, Horario = 0)
Materias <- param$vec_nom_materias_total
Num_Asig <- rep(0,dim(mat_solicitudes_ajustada)[1])
mat_solicitudes_aj <- cbind(mat_solicitudes_ajustada,Num_Asig)
mat_esqueleto_aux <- mat_esqueleto

# m <- 5
for(m in 1:length(Materias)){
  materia <- Materias[m]
  cat("\nMateria ",m,": ",materia)
  mat_aux_solicitud <- mat_solicitudes_aj %>% filter(Materia == materia)
  for(h in 1:length(param$Horas)){
    m_aux_esq <- mat_esqueleto[,m]
    num_gpos <- m_aux_esq[h]
    if(num_gpos > 0){#Si el # de gpos. simulados > 0
      m_aux <- mat_aux_solicitud%>% filter(as.numeric(Num_Asig)<2) %>% filter(Horario==param$Horas[h])
      
      if(dim(m_aux)[1] >= num_gpos){
        mat_asig <- m_aux[sample(1:dim(m_aux)[1],size = num_gpos),c(3,1,5)]
        mat_asignacion <- rbind(mat_asignacion,mat_asig)
        mat_esqueleto_aux[h,m] <- mat_esqueleto_aux[h,m] - num_gpos
      }else{
        mat_asig <- m_aux[,c(3,1,5)]
        mat_asignacion <- rbind(mat_asignacion,mat_asig)
        mat_esqueleto_aux[h,m] <- mat_esqueleto_aux[h,m] - dim(m_aux)[1]
      }
    }
    mat_solicitudes_aj <- cuenta_asignaciones(mat_solicitudes_aj,mat_asig)
  }#Fin for(h)
}
mat_asignacion <- mat_asignacion %>% filter(Materia != 0)
View(mat_asignacion)

sum(mat_esqueleto_aux)#352
sum(mat_esqueleto)#1013




