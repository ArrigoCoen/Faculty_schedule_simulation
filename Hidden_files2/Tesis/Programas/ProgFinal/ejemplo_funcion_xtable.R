##########################################################################
#' En este programa se encuentran ejemplos del uso de la función que
#' convierte matrices de R en una tabla para LaTeX.
#' https://binfalse.de/2010/10/12/export-r-data-to-tex-code/
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")

install.packages('xtable')
library('xtable')

# Ej. de función xtable ---------------------------------------------------

a <- matrix(rnorm(25), 5 ,5)
x <- xtable(a)
print(x, floating=FALSE, tabular.environment="bmatrix", hline.after=NULL,
        include.rownames=FALSE, include.colnames=FALSE)

# Ej. de función xtable ---------------------------------------------------
m <- matrix(rnorm(25,5,1),5,5)
m
xtable(m)


# Datos tesis -------------------------------------------------------------

load("mat_asignacion_final.RData")

xtable(mat_asignacion_final)




# Datos tesis ordenados ---------------------------------------------------
xtable(mat_asignacion_final[order(mat_asignacion_final[,1]),])

head(mat_asignacion_final[order(mat_asignacion_final[,1], mat_asignacion_final[,3]),])
head(mat_asignacion_final[order(mat_asignacion_final[,1], as.numeric(mat_asignacion_final[,3])),])


xtable(mat_asignacion_final[order(mat_asignacion_final[,1],
                                  as.numeric(mat_asignacion_final[,3])),])




xtable(mat_info_AG)#Resumen de pruebas AG










