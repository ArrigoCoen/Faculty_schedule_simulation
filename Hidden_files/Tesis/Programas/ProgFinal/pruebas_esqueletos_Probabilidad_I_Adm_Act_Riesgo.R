##########################################################################
#' En este programa se encuentran algunas pruebas de la función
#' normalmixEM() para los datos de "Probabilidad I" y de "Administración
#' Actuarial del Riesgo".
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")


# Parámetros iniciales ----------------------------------------------------
# param_sim$vec_sem_sig = 20202
# param_sim$k_sem_ant = 5
vec_s_sem_k_info <- gen_vec_s_sem_k_info(param_sim$vec_sem_sig,
                                         param_sim$k_sem_ant,param)

# Probabilidad I ----------------------------------------------------------
materia <- "Probabilidad I"
param_sim$Materias = materia
param_sim$m_filtrada <- gen_mat_m_filtrada(param,param_sim)
mat_al_corregidos_proba <- gen_mat_alumnos_corregidos(vec_s_sem_k_info,
                                                      param,param_sim)
View(mat_al_corregidos_proba)

set.seed(1806)
(d_proba <- simula_alumnos(mat_al_corregidos_proba,param))
set.seed(8654)
(d_prima_proba <- simula_alumnos(mat_al_corregidos_proba,param))

wait_proba = c(as.vector(mat_al_corregidos_proba),d_prima_proba)
mixmdl_proba = normalmixEM(wait_proba,mean = mean(wait_proba))#16 iteraciones
mixmdl_proba$loglik#-511.6696

hist(mixmdl_proba$x,freq = F)
lines(density(rnorm(1000,mean = mixmdl_proba$mu,sd = mixmdl_proba$sigma)), lty=1,
      lwd=2,col = "blue")
lines(density(mixmdl_proba$x), lty=1,lwd=2,col = "green")
legend(300,0.006,c("GMM","density()"),bty = "n",
       col=c("blue","green"),lty=c(1,1),
       cex=1.1,lwd=2)


#' 1 si faltan alumnos con respecto a d
#' -1 si sobran alumnos con respecto a d
#' 0 e.o.c.
# calif_A_proba <- s
# calif_B_proba <- rep(0,length(d_prima_proba))
# calif_proba <- s


# Administración Actuarial del Riesgo -------------------------------------
materia <- "Administración Actuarial del Riesgo"
# mat_nom_materias_total[1,1]#"Administración Actuarial del Riesgo"
param_sim$Materias = materia
param_sim$m_filtrada <- gen_mat_m_filtrada(param,param_sim)
mat_al_corregidos_adm_riesgo <- gen_mat_alumnos_corregidos(vec_s_sem_k_info,
                                                           param,param_sim)
View(mat_al_corregidos_adm_riesgo)

set.seed(1806)
(d_adm_riesgo <- simula_alumnos(mat_al_corregidos_adm_riesgo,param))
set.seed(8654)
(d_prima_adm_riesgo <- simula_alumnos(mat_al_corregidos_adm_riesgo,param))

wait_adm_riesgo = c(as.vector(mat_al_corregidos_adm_riesgo),d_prima_adm_riesgo)
mixmdl_adm_riesgo = normalmixEM(wait_adm_riesgo,mean = mean(wait_adm_riesgo))#26it
mixmdl_adm_riesgo$loglik#-419.9702

hist(mixmdl_adm_riesgo$x,freq = F)
lines(density(rnorm(1000,mean = mixmdl_adm_riesgo$mu,sd = mixmdl_adm_riesgo$sigma)), lty=1,
      lwd=2,col = "blue")
lines(density(mixmdl_adm_riesgo$x), lty=1,lwd=2,col = "green")
legend(60,0.04,c("GMM","density()"),bty = "n",
       col=c("blue","green"),lty=c(1,1),
       cex=1.1,lwd=2)

# Ambas materias ----------------------------------------------------------
D <- cbind(d_proba,d_adm_riesgo)
D_prima <- cbind(d_prima_proba,d_prima_adm_riesgo)

#' 1 si faltan alumnos con respecto a d
#' -1 si sobran alumnos con respecto a d
#' 0 e.o.c.
calif_A <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])
calif_B <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])
calif <- matrix(0, nrow = dim(D)[1], ncol = dim(D)[2])

for(c in 1:dim(D)[2]){#Recorre columnas
  for(r in 1:dim(D)[1]){#Recorre renglones
    if(D_prima[r,c] < D[r,c]){#Si faltan alumnos
      calif_B[r,c] <- 1
    }else if(D_prima[r,c] > D[r,c]){#Si sobran alumnos
      calif_B[r,c] <- -1
    }
  }
}

calif <- calif_A + calif_B

# wait <- list()
# wait[[1]] <- wait_proba
# wait[[2]] <- wait_adm_riesgo
mixmdl <- list()
mixmdl[[1]] <- mixmdl_proba
mixmdl[[2]] <- mixmdl_adm_riesgo
cota <- 500
cont_1 <- 1
cont_2 <- 1

#' Para este punto ya comparamos D y D_prima. Se redefine D_prima.
for(c in 1:dim(D)[2]){#Recorre columnas
  # cat("\n c = ",c)
  for(h in 1:length(param$Horas)){#Recorre las horas (renglones)
    # cat("\n h = ",h)
    (rand_num <- ceiling(rnorm(1,mixmdl[[c]]$mu,mixmdl[[c]]$sigma)))
    if(calif[h,c] > 0){#Si faltan alumnos
      while(rand_num <= D[h,c]){
        (rand_num <- ceiling(rnorm(1,mixmdl[[c]]$mu,mixmdl[[c]]$sigma)))
        cont_1 <- cont_1 + 1#Para no tener ciclo infinito
        if(cont_1 >= cota){
          break;
        }
      }
      cont_1 <- 1#Reiniciamos el contador
      D_prima[h,c] <- max(0,rand_num)
    }
    if(calif[h,c] < 0 && D[h,c]>0){#Si sobran alumnos
      #'La 2° cond. es para que no haya alumnos negativos
      while(rand_num > D[h,c]){
        (rand_num <- ceiling(rnorm(1,mixmdl[[c]]$mu,mixmdl[[c]]$sigma)))
        cont_2 <- cont_2 + 1#Para no tener ciclo infinito
        if(cont_2 >= cota){
          break;
        }
      }
      cont_2 <- 1#Reiniciamos el contador
      D_prima[h,c] <- max(0,rand_num)
    }
  }#fin for(h)
}#Fin for(c)

#Actualizamos laa calificaciones
calif_A <- calif
for(c in 1:dim(D)[2]){#Recorre columnas
  for(r in 1:dim(D)[1]){#Recorre renglones
    if(D_prima[r,c] < D[r,c]){#Si faltan alumnos
      calif_B[r,c] <- 1
    }else if(D_prima[r,c] > D[r,c]){#Si sobran alumnos
      calif_B[r,c] <- -1
    }
  }
}

calif <- calif_A + calif_B





cont_1 <- 1
cont_2 <- 1
#' Para este punto ya comparamos D y D_prima. Se redefine D_prima.
for(c in 1:dim(D)[2]){#Recorre columnas
  # cat("\n c = ",c)
  for(h in 1:length(param$Horas)){#Recorre las horas (renglones)
    # cat("\n h = ",h)
    (rand_num <- ceiling(rnorm(1,mixmdl[[c]]$mu,mixmdl[[c]]$sigma)))
    if(calif[h,c] > 0){#Si faltan alumnos
      while(rand_num <= D[h,c]){
        (rand_num <- ceiling(rnorm(1,mixmdl[[c]]$mu,mixmdl[[c]]$sigma)))
        cont_1 <- cont_1 + 1#Para no tener ciclo infinito
        if(cont_1 >= cota){
          break;
        }
      }
      cont_1 <- 1#Reiniciamos el contador
      D_prima[h,c] <- max(0,rand_num)
    }
    if(calif[h,c] < 0 && D[h,c]>0){#Si sobran alumnos
      #'La 2° cond. es para que no haya alumnos negativos
      while(rand_num > D[h,c]){
        (rand_num <- ceiling(rnorm(1,mixmdl[[c]]$mu,mixmdl[[c]]$sigma)))
        cont_2 <- cont_2 + 1#Para no tener ciclo infinito
        if(cont_2 >= cota){
          break;
        }
      }
      cont_2 <- 1#Reiniciamos el contador
      D_prima[h,c] <- max(0,rand_num)
    }
  }#fin for(h)
}#Fin for(c)

