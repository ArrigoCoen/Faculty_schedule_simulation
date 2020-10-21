# url <- "http://www.fciencias.unam.mx/docencia/horarios/20151/2017/92"
url <- "http://www.fciencias.unam.mx/docencia/horarios/20172/2017/625"
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
# salon_data_html <- html_nodes(webpage,'tr:nth-child(1) td~ td+ td a')
salon_data_html <- html_nodes(webpage,'tr:nth-child(1) td~ td+ td a , td:nth-child(4) a')
# salon_data_html <- html_nodes(webpage,'td:nth-child(4) a')
# salon_data_html <- html_nodes(webpage,'table:nth-child(31) td:nth-child(4) a , 
        # tr:nth-child(1) td~ td+ td a , table:nth-child(28) td:nth-child(4) a')
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
  
  # mat_group_lug_alum = extrae_mat_group_lug_alum(Capacidad_Salon,tipo_pagina)
  mat_group_lug_alum = extrae_mat_group_lug_alum(Capacidad_Salon,0)
  lugares = mat_group_lug_alum[,2]
  # Alumnos
  alumnos = mat_group_lug_alum[,3]
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
# mat_datos = matrix(c(materia,profesor,horario,lugares,alumnos,salon,grupo,carrera,plan,
#                      semestre),ncol = 10,byrow = F)
m10_proba_par = matrix(c(materia,profesor,horario,lugares,alumnos,salon,grupo,carrera,plan,
                     semestre),ncol = 10,byrow = F)
colnames(mat_datos) <- c("Materia","Profesor","Horario","Lugares","Alumnos","Salon",
                         "Grupo","Carrera","Plan","Semestre")