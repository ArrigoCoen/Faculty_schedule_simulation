##########################################################################
#' En este programa se encuentran ejemplos para guardar impresiones en un
#' archivo .txt
#' https://stackoverflow.com/questions/2470248/write-lines-of-text-to-a-file-in-r
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola

# Ej. ---------------------------------------------------------------------
#I would use the cat() command as in this example:
cat("Hello",file="outfile.txt",sep="\n")
cat("World",file="outfile.txt",sep="\n",append=TRUE)

#You can then view the results from with R with
file.show("outfile.txt")
