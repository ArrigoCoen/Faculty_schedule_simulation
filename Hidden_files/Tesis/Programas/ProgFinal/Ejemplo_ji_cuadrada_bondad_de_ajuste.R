##########################################################################
#' En este programa se encuentra un ejemplo de la obtención de los valores
#' de la prueba de la Xi cuadrada para la bondad de ajuste.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Programas/ProgFinal V01")




# Ejemplo Xi cuadrada -----------------------------------------------------
### Hogg pág. 285 Ej. 4.7.1
#' Example 4.7.1. One of the first six positive integers is to be chosen by
#' a random experiment (perhaps by the cast of a die). Let Ai = {x : x = i},
#' i = 1, 2, . . . , 6. The hypothesis H0 : P(Ai) = pi0 = 1/6,
#' i = 1, 2, . . . , 6, is tested, at the approximate 5% significance level,
#' against all alternatives. To make the test, the random experiment is
#' repeated under the same conditions, 60 independent times. In this example,
#' k = 6 and npi0 = 60(1/6) = 10, i = 1, 2, . . . , 6. Let Xi denote the
#' frequency with which the random experiment terminates with the outcome in
#' Ai, i = 1, 2, . . . , 6, and let Q5 = \sum_{i=1}^{6} (Xi - 10)^2/10.
#' Since there are 6 ??? 1 = 5 degrees of freedom, the critical value for a
#' level alpha = 0.05 test is qchisq(0.95,5) = 11.0705. Now suppose that the
#' experimental frequencies of A1,A2, . . . , A6 are, respectively, 13, 19,
#' 11, 8, 5, and 4. The observed value of Q5 is 15.6
#' Since 15.6 > 11.0705, the hypothesis P(Ai) = 1/6, i = 1, 2, . . . , 6,
#' is rejected at the (approximate) 5% significance level.
ps <- rep(1/6,6)#Probabilidades
x <- c(13,19,11,8,5,4)#Frecuencia
chisq.test(x,p=ps)
qchisq(0.95,5)
# qchisq(0.95,55)
# qchisq(0.95,36)

# 1- pchisq(.95,5)







