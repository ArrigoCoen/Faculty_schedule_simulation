# Estadísticas descriptivas (estadísticas básicas) ------------------------
#En este código se cargan las matrices que contienen la información de Proba I
#utilizadas para la pláica del Aquelarre Matemático 2019 y se hace un análisis
#estadístico con dicha información.
load("m12_proba_platica_TOTAL.RData")
load("m12_proba_impar.RData")
load("m12_proba_par.RData")


# Datos a utilizar --------------------------------------------------------
##Guardamos sólo los datos que vamos a analizar:
vec_alumnos_par <- as.integer(m12_proba_par[,5])
vec_alumnos_impar <- as.integer(m12_proba_impar[,5])
vec_alumnos_sem_par <- c(410,490,488,528,366)
vec_alumnos_sem_impar <- c(851,840,798,682,877,944)

mat_alumnos_turno <- m12_proba_platica_TOTAL[,c(5,12)]
mat_alumnos_turno <- matrix(as.integer(mat_alumnos_turno),ncol = 2)
vec_turno <- mat_alumnos_turno[,2]
colnames(mat_alumnos_turno) <- c("Alumnos","Turno")
View(mat_alumnos_turno)
vec_alumnos_matutino <- mat_alumnos_turno[vec_turno>0,1]
## En este caso hay un grupo que no tiene horario, por lo que se le puso un
# -1 y se está metiendo la información en el turno vespertino
vec_alumnos_vespertino <- mat_alumnos_turno[vec_turno<=0,1]
vec_alumnos_matutino_ac <- c(678,327,676,418,582,329,510,309,706,233,689)
vec_alumnos_vespertino_ac <- c(173,83,164,72,216,159,172,219,171,133,255)


# Histograma --------------------------------------------------------------
hist(vec_alumnos_par)
hist(vec_alumnos_impar)
hist(c(vec_alumnos_par,vec_alumnos_impar))

# Análisis ----------------------------------------------------------------
install.packages('pastecs')
install.packages('Hmisc')
library(pastecs)
library(Hmisc)

summary(vec_alumnos_par)
summary(vec_alumnos_impar)
summary(vec_alumnos_matutino)
summary(vec_alumnos_vespertino)

stat.desc(vec_alumnos_par)
stat.desc(vec_alumnos_impar) 
stat.desc(vec_alumnos_matutino) 
stat.desc(vec_alumnos_vespertino) 

describe(vec_alumnos_par)
describe(vec_alumnos_impar)
describe(vec_alumnos_matutino)
describe(vec_alumnos_vespertino)



# AR ----------------------------------------------------------------------
alum_impar_mat <- c(678,676,582,510,706,689)
alum_impar_vesp <- c(173,164,216,172,171,255)
alum_par_mat <- c(327,418,329,309,233)
alum_par_vesp <- c(83,72,159,219,133)



ar(alum_impar_mat)
ar(alum_impar_vesp)
ar(alum_par_mat)
ar(alum_par_vesp)




# Análisis por grupos Gi ---------------------------------------------------
# G1 <- c(70,24,163,134,66,69,79,73,62,99,55,82,57,59,46,105,60,51,50,84,48,
#         53,75,97,73,16,86,51,40,45,33,27,124,97,35,58,178,50,92,116,20,83,
#         52,16,99,62,99,55,82,57,59,46,105,60,51,50,84,48,53,75,97,73,16,86,
#         51,40,45,33,27,124,97,35,58,178,50,92,116,20,83,52,16,99,81,36,53,
#         54,30,85,152,48,63,87)
# G2 <- c(62,18,45,48,15,15,12,51,19,49,3,41,16,46,53,52,8,19,48,48,25,24,8,
#         12,13,44,26,27,8,41,29,63,51,22,90)
# G3 <- c(30,63,116,49,69,19,61,97,72,80,89,111,62,27,66,63,85,92,60,72,24,
#         59,64,41,45)
# G4 <- c(38,45,20,52,30,39,60,30,113,38,68,44,39,50)

G1 <- c(678,676,582,510,706,689)
G2 <- c(173,164,216,172,171,255)
G3 <- c(327,418,329,309,233)
G4 <- c(83,72,159,219,133)


summary(G1)
summary(G2)
summary(G3)
summary(G4)

stat.desc(G1)
stat.desc(G2) 
stat.desc(G3) 
stat.desc(G4) 

describe(G1)
describe(G2)
describe(G3)
describe(G4)


