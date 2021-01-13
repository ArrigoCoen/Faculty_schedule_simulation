##########################################################################
#' En este programa se encuentra el código con un ejemplo de lectura de un
#' archivo de excel con varias hojas.
#' http://www.sthda.com/english/wiki/reading-data-from-excel-files-xls-xlsx-into-r
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")


# Write the first data set in a new workbook
# write.csv(mat_nom_materias_total,
#           file = "mat_nom_materias_total.csv",
#           row.names = FALSE)
# write.csv(mat_nom_prof_total,
#           file = "mat_nom_prof_total.csv",
#           row.names = FALSE)

# Installing and loading readxl package -----------------------------------
# install.packages("readxl")
library("readxl")


# Using readxl package ----------------------------------------------------
#' Read both xls and xlsx files
# xls files
# my_data <- read_excel("my_file.xls")
# xlsx files
my_data <- read_excel("horario.xlsx", sheet = "Horario")

my_data[15,]
my_data[15,1]


my_data2 <- read_excel("horario.xlsx", sheet = "Materias")
my_data2[15,]
my_data2[15,1]


my_data3 <- read_excel("horario.xlsx", sheet = "Profesores")
my_data3[15,]
my_data3[15,1]
dim(my_data3)

