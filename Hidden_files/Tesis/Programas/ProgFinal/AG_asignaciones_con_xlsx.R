##########################################################################
#' En este programa se encuentra la función que aplica el algoritmo
#' genético a las asignaciones para encontrar una buena asignación. La
#' función lee un documento de excel con asignaciones previas, las quita
#' de las solicitudes más la respectiva información y genera una
#' asignación.
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")


# AG_asignaciones_con_xlsx ------------------------------------------------
AG_asignaciones_con_xlsx <- function(mat_esqueleto,mat_solicitudes_real,
                                     con_xlsx_1_sin_xlsx_0,param){
  
  if(con_xlsx_1_sin_xlsx_0 == 0){
    #Si no se carga el archivo de excel
    list_asignacion_final <- AG_asignaciones(mat_esqueleto,
                                             mat_solicitudes_real,param)
  }else{
    #Si se carga el archivo de excel
    asig_fijas <- read_excel("horario.xlsx", sheet = "Horario")
    mat_solicitudes_restantes <- mat_solicitudes_real
    mat_esqueleto_restante <- mat_esqueleto
    
    for(r in 1:dim(asig_fijas)[1]){
      (materia <- as.character(asig_fijas[r,1]))
      (prof <- as.character(asig_fijas[r,2]))
      (hora <- as.character(asig_fijas[r,3]))
      
      #Se eliminan las solicitudes
      (ind_prof <- which(mat_solicitudes_restantes[,1] == prof))
      (ind_hora <- which(mat_solicitudes_restantes[,5] == hora))
      (ind_materia <- which(mat_solicitudes_restantes[,3] == materia))
      (elim_hora_prof <- intersect(ind_prof,ind_hora))
      (elim_materia_prof <- intersect(ind_prof,ind_materia))
      (ind_elim <- union(elim_hora_prof,elim_materia_prof))
      if(length(ind_elim) > 0){
        mat_solicitudes_restantes <- mat_solicitudes_restantes[-ind_elim,]
      }
      
      #Se eliminan los grupos en el esqueleto
      (h_hora <- which(7:21 == as.numeric(hora)))
      (j_materia <- arroja_num_materia(materia))
      mat_esqueleto_restante[h_hora,j_materia] <- mat_esqueleto_restante[h_hora,
                                                                         j_materia] - 1
      
    }#Fin for(r)
    list_asignacion_final <- AG_asignaciones(mat_esqueleto_restante,
                                             mat_solicitudes_restantes,param)
    list_asignacion_final[[1]] <- rbind(list_asignacion_final[[1]],asig_fijas)
  }#Fin else
  
  
  return(list_asignacion_final)
}




# Ej. ---------------------------------------------------------------------
load(file = "mat_info_AG.RData")
mat_info_AG
mat_info_AG <- rbind(mat_info_AG,list_asignacion_final$mat_info_AG)
save(mat_info_AG,file = "mat_info_AG.RData")

list_asignacion_final$mat_info_AG <- mat_info_AG
dat_sem_20202_g03_n05_m004_U655 <- list_asignacion_final
save(dat_sem_20202_g03_n05_m004_U655,file = "dat_sem_20202_g03_n05_m004_U-655.RData")




