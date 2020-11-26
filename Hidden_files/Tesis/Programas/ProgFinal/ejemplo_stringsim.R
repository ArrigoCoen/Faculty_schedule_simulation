##########################################################################
#' En este programa se encuentra un ejemplo del uso de la función
#' stringsim() que compara dos strings.
#' https://www.rdocumentation.org/packages/stringdist/versions/0.9.6.3/topics/stringsim
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")


# stringsim(a,b) ----------------------------------------------------------
# install.packages("stringdist")
# library("stringdist")
# Calculate the similarity using the default method of optimal string alignment
stringsim("ca", "abc")#0
stringsim("abcd", "abc")#0.75
stringsim("María Asunción Begoña Fernández Fernández", "Begoña Fernández Fernández")#0.6341463
stringsim("María Asunción Begoña Fernández Fernández", "Begoña Fernandez Fernandez")#0.5853659
stringsim("Begoña Fernández Fernández", "Begoña Fernandez Fernandez")#0.9230769



# Ejemplo de función para corregir nombres --------------------------------
vec_names <- c("sau","saoeuhs","soasnuot")

cat("corrector_de_nombres_profesores <- function(name) {")
umbral_nombre <- .5
for(name1 in vec_names) for(name2>name1) if(stringsim(name1,name2)>umbral_nombre) 
  cat("if(name==",name2,") return(",name1,")")
cat("}")