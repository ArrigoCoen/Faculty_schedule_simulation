# Limpia variables  y consola ---------------------------------------------
rm(list=ls())  #Borra todo
cat("\014") #Borra consola 

#Se establece el directorio de trabajo
setwd("C:/Users/MiRi/Dropbox/Carpeta compartida MIri/Programas/Prog V01")

source("Fn_Horario.R")

# posibles_url_platica ------------------------------------------------------------
#' Title: posibles_url_platica
#' En esta función se genera una matriz de 4 columnas que contiene la información
#' de las posibles páginas de los horarios que tienen información.
#' 
#' @param sem_ini: Número que representa el semestre en el cual se desea
#' iniciar la búsqueda de información.
#' @param sem_fin: Número que representa el semestre en el cual se desea
#' finalizar la búsqueda de información.
#' 
#' @example sem_ini = 20081
#' @example sem_fin = 20201
#'
#' @return mat_posibles_url_platica: Matriz con 4 columnas:
#' 1) Semestre
#' 2) Plan
#' 3) Número de materia
#' 4) Posibles url
posibles_url_platica = function(sem_ini,sem_fin){
  # **Planes de Estudio:
  # *Actuaría:
  #   2015 ---- num1 = 2017
  
  ## Sólo se van a tomar en cuenta los planes de estudio vigentes
  planes_estudio = 2017
  
  #Inicializamos las variables:
  i = 0
  intervalo_num_materia = c(92,625)
  mat_posibles_url_platica = matrix(0, nrow = 25,ncol = 4)
  
  ## Se crean los vectores para los semestres pares e impares
  sem_impar = seq(from = sem_ini, to = sem_fin, by = 10) 
  sem_par = seq(from = (sem_ini+1), to = sem_fin, by = 10)
  # semestres = sort(c(sem_impar,sem_par))
  semestres = c(sem_impar,sem_par)
  
  vec_para_for <- 1:length(semestres)
  # pb <- txtProgressBar(min = 1, max = length(vec_para_for), style = 3, width = 60)
  # ind_pb = 1
  
  for(num_materia in intervalo_num_materia) {
    # setTxtProgressBar(pb, ind_pb)
    for(plan in planes_estudio) {
      for(sem in semestres){
        url = paste0("http://www.fciencias.unam.mx/docencia/horarios/",sem,"/",plan,
                     "/",num_materia)
        #Probamos si la página existe:
        tryCatch({
          read_html(url)
          i <- i+1
          mat_posibles_url_platica[i,] <- c(sem,plan,num_materia,url)
          # cat("i=",i,"\n")
          # on.exit(close(read_html(url)))
        }, 
        error=function(e){})}}
    # ind_pb = ind_pb +1
  }
  
  ##Quitamos los renglones vacíos de la matriz:
  mat_posibles_url_platica = mat_posibles_url_platica[1:i,]
  
  return(mat_posibles_url_platica)
}


# Función extrae_datos_pagina ----------------------------------------------
# Función en la cual se extrae la información de las páginas de la facultad
#de Ciencias. Regresa una matriz de 10 columnas con información de la página.
#' Title:extrae_datos_pagina
#' @param url: Dirección web de horarios de fciencias 
#' @param tipo_pagina: Variable binaria la cual indica si la página que se 
#' recibe como parámetro (url) es de algún semestre anterior (0) o del 
#' semestre actual (1), o de algún semestre anterior al 2018_1, a partir de éste.
#' 
#' @example: "http://www.fciencias.unam.mx/docencia/horarios/20151/2017/1707"
#' @example: tipo_pagina = {-1,0,1}
#'
#' @return mat_datos: Matriz con 10 columnas cuyos nombres son:
#' Materia
#' Profesor
#' Horario
#' Lugares
#' Alumnos
#' Salón
#' Grupo
#' Carrera
#' Plan
#' Semestre
#'
extrae_datos_pagina_platica = function(url,tipo_pagina){
  webpage <- read_html(url)
  # Profesor
  profesor_data_html <- html_nodes(webpage,'tr:nth-child(1) td:nth-child(2) a')
  profesor <- html_text(profesor_data_html)
  
  #Materia
  materia_data_html <- html_nodes(webpage,'#info-contenido h2')
  materia <- html_text(materia_data_html)
  materia = rep(materia,length(profesor))
  ##Se guarda la materia y el semestre en el mismo vector
  
  # Horario
  horario_data_html <- html_nodes(webpage,'tr:nth-child(1) td:nth-child(4)')
  horario <- html_text(horario_data_html)
  horario<-gsub("\n","",horario)
  
  # Salón
  salon_data_html <- html_nodes(webpage,'tr:nth-child(1) td~ td+ td a')
  salon <- html_text(salon_data_html)
  
  # Carrera
  carrera_data_html <- html_nodes(webpage,'h1')
  carrera <- html_text(carrera_data_html)
  carrera = rep(carrera, length(profesor)) ### FALTA SEPARAR CARRERA
  ##Se guarda la carrera y el plan en el mismo vector
  
  # Plan
  Carrera_Plan_data_html <- html_nodes(webpage,'h1')
  Carrera_Plan <- html_text(Carrera_Plan_data_html)
  # el símbolo "+" hace que se haga un solo número
  plan = regmatches(Carrera_Plan, gregexpr("[[:digit:]]+",Carrera_Plan)) 
  plan = rep(as.numeric(plan),length(profesor))
  
  # Semestre
  semestre = materia ### FALTA SEPARAR MATERIA DEL SEMESTRE
  
  if(tipo_pagina == 0){
    #Extracción de datos de páginas pasadas
    # Lugares
    # Capacidad_Salon_data_html <- html_nodes(webpage,'div:nth-child(23)  , 
    #.menu-horizontal+ div , div:nth-child(8) , #form+ div')
    # Capacidad_Salon_data_html <- html_nodes(webpage,'table+ div , 
    #form+ div')
    Capacidad_Salon_data_html <- html_nodes(webpage,'.menu-horizontal+ div ,table+ div , 
                                            #form+ div')
    Capacidad_Salon <- html_text(Capacidad_Salon_data_html)
    
    #Eliminamos los datos que no nos interesan
    indices = 0
    for(a in 1:length(Capacidad_Salon)){
      if(Capacidad_Salon[a] == "Presentación"){
        indices = c(indices,a)
      }
    }
    if(length(indices) > 1){
      indices = indices[-1]
      Capacidad_Salon = Capacidad_Salon[-indices]
    }
    
    mat_group_lug_alum = extrae_mat_group_lug_alum(Capacidad_Salon,tipo_pagina)
    lugares = mat_group_lug_alum[,2]
    # Alumnos
    alumnos  = mat_group_lug_alum[,3]
    # Grupo
    grupo = mat_group_lug_alum[,1]
  }else if(tipo_pagina == 1){
    #Extracción de datos de página actual:
    
    # Lugares
    Capacidad_Salon_data_html <- html_nodes(webpage,'#info-contenido div')
    Capacidad_Salon <- html_text(Capacidad_Salon_data_html)
    Capacidad_Salon = Capacidad_Salon[-c(1,2)]
    
    indices = 0
    #Eliminamos los datos que no nos interesan
    for(a in 1:length(Capacidad_Salon)){
      if(Capacidad_Salon[a] == "Presentación"){
        indices = c(indices,a)
      }
    }
    if(length(indices) > 1){
      indices = indices[-1]
      Capacidad_Salon = Capacidad_Salon[-indices]
    }
    mat_group_lug_alum = extrae_mat_group_lug_alum(Capacidad_Salon,tipo_pagina)
    lugares = mat_group_lug_alum[,2]
    # Alumnos
    alumnos  = mat_group_lug_alum[,3]
    # Grupo
    grupo = mat_group_lug_alum[,1]
  }else if(tipo_pagina == -1){
    #Extracción de datos de páginas pasadas
    # Alumnos
    Capacidad_Salon_data_html <- html_nodes(webpage,'.menu-horizontal+ div ,
                                            table+ div , #form+ div')
    Capacidad_Salon <- html_text(Capacidad_Salon_data_html)
    
    #Eliminamos los datos que no nos interesan
    indices = 0
    for(a in 1:length(Capacidad_Salon)){
      if(Capacidad_Salon[a] == "Presentación"){
        indices = c(indices,a)
      }
    }
    if(length(indices) > 1){
      indices = indices[-1]
      Capacidad_Salon = Capacidad_Salon[-indices]
    }
    mat_group_alum = extrae_mat_group_lug_alum(Capacidad_Salon,tipo_pagina)
    alumnos  = mat_group_alum[,2]
    # Grupo
    grupo = mat_group_alum[,1]
    # Lugares
    lugares = rep(-1,length(profesor))
  }
  
  # data_frame_datos = data.frame(materia,profesor,horario,lugares,alumnos,
  # salon,grupo,carrera,plan,semestre)
  mat_datos = matrix(c(materia,profesor,horario,lugares,alumnos,salon,
                       grupo,carrera,plan,semestre),ncol = 10,byrow = F)
  colnames(mat_datos) <- c("Materia","Profesor","Horario","Lugares","Alumnos","Salon",
                           "Grupo","Carrera","Plan","Semestre")
  #View(mat_datos)
  return(mat_datos)
}


# Posibles url para Proba I ---------------------------------------------
##Se le pasa como parámetro la matriz obtenida en la función
m4_posibles_url_platica <- limpia_base_url(posibles_url_platica(20151,20201))
colnames(m4_posibles_url_platica) <- c("Semestre","Plan","Materia","URL")
View(m4_posibles_url_platica)


# vectores_url ------------------------------------------------------------
## Haremos  vectores que contienen los 4 grupos de urls (G1, G2, G3, G4)
(url_calculo_impar <- m4_posibles_url_platica[1:6,4])
(url_calculo_par <- m4_posibles_url_platica[7:11,4])
(url_proba_impar <- m4_posibles_url_platica[12:17,4])
(url_proba_par <- m4_posibles_url_platica[18:22,4])



# m12 para Proba I ------------------------------------------------------------------
# if(m4_posibles_url_platica[k,1] == sem_fin){
#   tipo_pagina = 1 #Materias del semestre actual
# }else if(m4_posibles_url_platica[k,1] > 20181 && m4_posibles_url_platica[k,1] < sem_fin){
#   tipo_pagina = 0 #Materias de semestres entre el 2018-1 al anterior al actual
# }else if(m4_posibles_url_platica[k,1] <= 20181){
#   tipo_pagina = -1#Materias de semestres anteriores al 2018-1
# }


m10_proba_par <- extrae_datos_pagina_platica(url_proba_par[5],0)
View(m10_proba_par)

# m10_proba_impar[17,6] <- -1
# m10_proba_impar[1,7] <- 9007
# m10_proba_impar[2:17,7] <- m10_proba_impar[1:16,7]


cambios <- c(rep("1,2,",8))
turno <- c(1,1,1,1,1,0,0,0)

# m12_proba_platica_TOTAL = matrix(0,nrow = 500,ncol = 12,byrow = F)

m12_proba_platica_TOTAL[122:129,] = matrix(c(m10_proba_par,cambios,turno),ncol = 12,byrow = F)
colnames(m12_proba_platica_TOTAL) <- c("Materia","Profesor","Horario","Lugares","Alumnos",
                          "Salon","Grupo","Carrera","Plan","Semestre","Cambios","Turno")

m12_proba_platica_TOTAL <- m12_proba_platica_TOTAL[1:129,]
m12_proba_platica_TOTAL[,5] <- as.integer(m12_proba_platica_TOTAL[,5])
View(m12_proba_platica_TOTAL)
save(m12_proba_platica_TOTAL, file = "m12_proba_platica_TOTAL.RData")

load("m12_proba_platica_TOTAL.RData")

# m12_proba_impar <- m12_proba_platica_TOTAL[1:90,]
# m12_proba_impar[,5] <- as.integer(m12_proba_impar[,5])
View(m12_proba_impar)
# save(m12_proba_impar, file = "m12_proba_impar.RData")

# m12_proba_par <- m12_proba_platica_TOTAL[91:129,]
# m12_proba_par[,5] <- as.integer(m12_proba_par[,5])
View(m12_proba_par)
# save(m12_proba_par, file = "m12_proba_par.RData")



# Gráficas ----------------------------------------------------------------
## Los semestres con los que estaremos trabajando son desde el 2015-1 hasta
##el 2020-1
## Se crean los vectores para los semestres pares e impares
sem_impar = seq(from = 20151, to = 20201, by = 10) 
sem_par = seq(from = (20151+1), to = 20201, by = 10)
# semestres = as.character(sort(c(sem_impar,sem_par)))
semestres = sort(c(sem_impar,sem_par))

##Sumamos el número de alumnos por semestre
num_alumnos_probaI_20151 <- sum(as.integer(m12_proba_impar[1:12,5]))
num_alumnos_probaI_20161 <- sum(as.integer(m12_proba_impar[13:29,5]))
num_alumnos_probaI_20171 <- sum(as.integer(m12_proba_impar[30:44,5]))
num_alumnos_probaI_20181 <- sum(as.integer(m12_proba_impar[45:59,5]))
num_alumnos_probaI_20191 <- sum(as.integer(m12_proba_impar[60:75,5]))
num_alumnos_probaI_20201 <- sum(as.integer(m12_proba_impar[76:90,5]))

num_alumnos_probaI_20152 <- sum(as.integer(m12_proba_par[1:7,5]))
num_alumnos_probaI_20162 <- sum(as.integer(m12_proba_par[8:15,5]))
num_alumnos_probaI_20172 <- sum(as.integer(m12_proba_par[16:24,5]))
num_alumnos_probaI_20182 <- sum(as.integer(m12_proba_par[25:31,5]))
num_alumnos_probaI_20192 <- sum(as.integer(m12_proba_par[32:39,5]))

num_alumnos_probaI_impar <- c(num_alumnos_probaI_20151,num_alumnos_probaI_20161,
      num_alumnos_probaI_20171,num_alumnos_probaI_20181,num_alumnos_probaI_20191,
      num_alumnos_probaI_20201)
num_alumnos_probaI_par <- c(num_alumnos_probaI_20152,num_alumnos_probaI_20162,
      num_alumnos_probaI_20172,num_alumnos_probaI_20182,num_alumnos_probaI_20192)

num_alumnos_probaI <- c(num_alumnos_probaI_20151,num_alumnos_probaI_20152,
                        num_alumnos_probaI_20161,num_alumnos_probaI_20162,
                        num_alumnos_probaI_20171,num_alumnos_probaI_20172,
                        num_alumnos_probaI_20181,num_alumnos_probaI_20182,
                        num_alumnos_probaI_20191,num_alumnos_probaI_20192,
                        num_alumnos_probaI_20201)

# plot(semestres, num_alumnos_probaI_impar, type="overplotted",pch=1, col="blue", 
#      xlab="Semestre",ylab="Número de alumnos",main="Número de alumnos por semestre")
# #,ylim=c(10,60)
# lines(semestres,num_alumnos_probaI_par,type="overplotted",pch=2,col="red")
# legend("topleft",legend=c("Clase1","Clase2"),pch=c(1,2),col=c("blue","red"))

# plot(semestres,num_alumnos_probaI,type = "o")
plot(sem_impar,num_alumnos_probaI_impar,type = "o")
# lines(c(sem_par,20202),c(num_alumnos_probaI_par,0),type="overplotted",pch=2,col="red")
plot(sem_par,num_alumnos_probaI_par,type = "o")




# Gráfica por turno -------------------------------------------------------
mat_alumnos_turno <- m12_proba_platica_TOTAL[,c(5,12)]
mat_alumnos_turno <- matrix(as.integer(mat_alumnos_turno),ncol = 2)
vec_turno <- mat_alumnos_turno[,2]
View(mat_alumnos_turno)

vec_alumnos_matutino <- mat_alumnos_turno[vec_turno>0,1]

## En este caso hay un grupo que no tiene horario, por lo que se le puso un
# -1 y se está metiendo la información en el turno vespertino
vec_alumnos_vespertino <- mat_alumnos_turno[vec_turno<=0,1]





# Estadísticas descriptivas (estadisticas basicas) ------------------------
# install.packages('pastecs')
# install.packages('Hmisc')
library(pastecs)
library(Hmisc)

summary(m12_proba_platica_TOTAL)
stat.desc(m12_proba_platica_TOTAL) 
describe(m12_proba_platica_TOTAL)






# m12 para Cálculo II ------------------------------------------------------------------
# m12_calculo_impar <- extrae_datos_pagina(url,tipo_pagina)
# m10_calculo_impar <- extrae_datos_pagina_platica(url_calculo_impar[1],-1)
# View(m10_calculo_impar)





