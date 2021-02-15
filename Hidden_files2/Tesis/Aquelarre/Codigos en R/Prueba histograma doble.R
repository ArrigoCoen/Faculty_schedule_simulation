
# Ej. Histograma doble ----------------------------------------------------
set.seed(42)
p1 <- hist(rnorm(500,4))                     # centered at 4
p2 <- hist(rnorm(500,6))                     # centered at 6
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,10))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,10), add=T)  # second



# Histograma doble: Semestres Par e Impar ----------------------------------
p1 <- hist(vec_alumnos_par)
p2 <- hist(vec_alumnos_impar)
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,180),ylim=c(0,31),
      xlab = "Número de alumnos",ylab="Frecuencia")  # first histogram
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,180),ylim=c(0,31), add=T)  # second



# Histograma doble: Turnos ------------------------------------------------
p3 <- hist(vec_alumnos_matutino)
p4 <- hist(vec_alumnos_vespertino)
plot( p3, col=rgb(0,0,1,1/4), xlim=c(0,180),ylim=c(0,25),
      xlab = "Número de alumnos",ylab="Frecuencia")  # first histogram
plot( p4, col=rgb(1,0,0,1/4), xlim=c(0,180),ylim=c(0,25), add=T)  # second





