##########################################################################
#' En este programa se encuentran ejemplos de las funciones wo() e 
#' isSeasonal() que prueban la estacionalidad de los datos dados.
#' https://cran.r-project.org/web/packages/seastests/vignettes/seastests-vignette.html
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola


install.packages('seastests')
library(seastests)



# Ej. internet ------------------------------------------------------------
set.seed(5)
x1 = 1:96/20 + ts(rnorm(96, 100, 1), start=c(2015,1), frequency=12)
x2 = 1:96/20 + ts(rnorm(96, 100, 1) + 
                    ts(sin((2*pi*rep(1:12, 8))/12),frequency=12),
                  start=c(2015,1), frequency=12)


ts.plot(x1,x2, col=c("blue", "red"), main="Some simple seasonal series")
legend("topleft", c("Non seasonal series", "Seasonal series"),
       col=c("blue", "red"), lty=1)
cat("\n Testing the non-seasonal series")
summary(wo(x1))
summary(wo(decompose(x1)$seasonal))
summary(wo(decompose(x1)$x))

plot(decompose(x1))

cat("\nTesting the seasonal series")
summary(wo(x2))
summary(wo(decompose(x2)$seasonal))
summary(wo(decompose(x2)$x))

plot(decompose(x2))

print("Test using the non-seasonal series")
isSeasonal(x1)#FALSE
print("Test using the seasonal series")
isSeasonal(x2)#TRUE






