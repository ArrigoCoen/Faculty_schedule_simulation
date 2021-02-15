##########################################################################
#' En este programa se encuentra el código que guarda el número de grupos
#' reales que hay por materia en el semestre 2020-1.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
# rm(list=ls())  # Borra variables
# cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")

#' Se cargan los datos
load("m_grande por semestre/m_grande_20201.RData")

# Esqueleto 2020-1 real ---------------------------------------------------
mat_esq_20201_real <- matrix(0,nrow = length(param$Horas),
                   ncol = length(param$vec_nom_materias_total))
rownames(mat_esq_20201_real) <- param$nombre_hrs
colnames(mat_esq_20201_real) <- param$vec_nom_materias_total


for(m in 1:length(param$vec_nom_materias_total)){
  materia <- param$vec_nom_materias_total[m]
  cat("\n Materia ",m,": ",materia)
  mat_materia <- m_grande %>% filter(Num_materia == m)
  for(h in 1:length(param$Horas)){
    hora <- param$Horas[h]
    mat_hora <- mat_materia %>% filter(horario_num == hora)
    mat_esq_20201_real[h,m] <- dim(mat_hora)[1]
  }
}

View(mat_esq_20201_real)
save(mat_esq_20201_real,file = "mat_esq_20201_real.RData")
num_gpos_20201_real <- colSums(mat_esq_20201_real)
View(num_gpos_20201_real)

(num_gpos_total_20201_real <- sum(mat_esq_20201_real))#747





