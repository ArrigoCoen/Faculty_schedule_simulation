# Limpia variables  y consola ---------------------------------------------
rm(list=ls())  #Borra todo
cat("\014") #Borra consola 


# Cambiar la dirección por la de tu computadora
# setwd("/Users/arrigocoen/Dropbox/Thesis/2019/Miriam/Carpeta compartida MIri/Programas/Prog V01")
# setwd("C:/Users/MiRi/Dropbox/Carpeta compartida MIri/Programas/Prog V01")
# setwd("C:/Users/MiRi/Dropbox/Carpeta compartida MIri/Aquelarre/Codigos en R")
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Programas/Prog V01")
# setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Aquelarre/Codigos en R")

# Cargamos la matriz m12 --------------------------------------------------
load("m12_proba_platica_TOTAL.RData")
load("m12_proba_impar_platica.RData")
load("m12_proba_par_platica.RData")

# Datos a utilizar --------------------------------------------------------
vec_alumnos_par <- as.integer(m12_proba_par[,5])
vec_alumnos_impar <- as.integer(m12_proba_impar[,5])

mat_alumnos_turno <- m12_proba_platica_TOTAL[,c(5,12)]
mat_alumnos_turno <- matrix(as.integer(mat_alumnos_turno),ncol = 2)
vec_turno <- mat_alumnos_turno[,2]
colnames(mat_alumnos_turno) <- c("Alumnos","Turno")
# View(mat_alumnos_turno)
vec_alumnos_matutino <- mat_alumnos_turno[vec_turno>0,1]
## En este caso hay un grupo que no tiene horario, por lo que se le puso un
# -1 y se est? metiendo la informaci?n en el turno vespertino
vec_alumnos_vespertino <- mat_alumnos_turno[vec_turno<=0,1]



# Histograma doble: Semestres Par e Impar ----------------------------------


val_ylim <- c(0,0.02)
lwd_dens <- 6

# Solo Par
hist(vec_alumnos_par,   col=rgb(0,0,1,1/4),breaks = seq(0,200,by = 10),freq = F,ylim=val_ylim,ylab = "Densidad",
     main = "Histograma de vector vec_alumnos_par",xlab = " Número alumnos")
lines(density(vec_alumnos_par),col="red",lwd=lwd_dens)

# Solo Impar
hist(vec_alumnos_impar, col=rgb(1,0,0,1/4),breaks = seq(0,200,by = 10),freq = F,ylim=val_ylim,ylab = "Densidad",
     main = "Histograma de vector vec_alumnos_impar",xlab = " Número alumnos")
lines(density(vec_alumnos_impar),col="blue",lwd=lwd_dens)

# Juntos
hist(vec_alumnos_par,   col=rgb(1,0,0,1/4),breaks = seq(0,200,by = 10),freq = F,ylim=val_ylim,ylab = "Densidad",
     main="Histograma semestres par e impar",xlab = " Número alumnos")
lines(density(vec_alumnos_par),col="red",lwd=lwd_dens)
hist(vec_alumnos_impar, col=rgb(0,0,1,1/4),breaks = seq(0,200,by = 10),freq = F,add=TRUE)
lines(density(vec_alumnos_impar),col="blue",lwd=lwd_dens)






# Histograma doble: Turnos ------------------------------------------------

lwd_dens <- 6

# Solo Matutino
hist(vec_alumnos_matutino,   col=rgb(0,0,1,1/4),breaks = seq(0,200,by = 10),freq = F,ylim=c(0,0.025),ylab = "Densidad",
     main = "Histograma de vector vec_alumnos_matutino",xlab = " Número alumnos")
lines(density(vec_alumnos_matutino),col="blue",lwd=lwd_dens)

# Solo Vespertino
hist(vec_alumnos_vespertino, col=rgb(1,0,0,1/4),breaks = seq(0,200,by = 10),freq = F,ylim=c(0,0.025),ylab = "Densidad",
     main = "Histograma de vector vec_alumnos_vespertino",xlab = " Número alumnos")

lines(density(vec_alumnos_vespertino),col="red",lwd=lwd_dens)

# Juntos
hist(vec_alumnos_matutino,   col=rgb(0,0,1,1/4),breaks = seq(0,200,by = 10),freq = F,ylim=c(0,0.025),ylab = "Densidad",
     main="Histograma turnos matutino y vespertino",xlab = " Número alumnos")
lines(density(vec_alumnos_matutino),col="blue",lwd=lwd_dens)
hist(vec_alumnos_vespertino, col=rgb(1,0,0,1/4),breaks = seq(0,200,by = 10),freq = F,add=TRUE)
lines(density(vec_alumnos_vespertino),col="red",lwd=lwd_dens)


