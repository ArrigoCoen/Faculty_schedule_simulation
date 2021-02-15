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
#' Title AG_asignaciones_con_xlsx: Función que aplica el algoritmo genético
#' a las asignaciones para encontrar una buena asignación. La función tiene
#' la opción de leer un documento de excel con asignaciones previas. En caso
#' de que si lea el documento, quita las asignaciones previas de las
#' solicitudes más la respectiva información y genera una asignación.
#'
#' @param mat_esqueleto: Matriz de 15 renglones (horas) y 203 columnas
#' (materias). En la entrada (i,j) se tiene el número de grupos simulados
#' para la hora i, y la materia j.
#' @param mat_solicitudes_real: Matriz de 5 columnas (Profesor,TC,Materia,
#' Num_Materia,Horario) y 6 renglones que tiene la información de la
#' solicitud de "nom_prof". Se eligen 2 materias y hasta 3 diferentes
#' horarios. Se quitan los renglones repetidos. Se hace una "intersección"
#' con los grupos simulados en la matriz "mat_esqueleto" y así se obtienen
#' las solicitudes pseudo-reales de los profesores.
#' @param con_xlsx_1_sin_xlsx_0: Variable binaria que vale 1 si se debe
#' de leer el archivo xlsx y 0 si no.
#' @param param: Lista con los diferentes parámetros que se utilizan en las
#' funciones que se mandan llamar.
#' @example param <- list(nombre_hrs = c("7-8","8-9"),nombre_sem = c("2015-1",
#' "2015-2"),Semestres = c(20192,20201),Horas = c(7,8,9,10),q1 = 80, q2 = 90,
#' m_grande_total)
#'
#' @return list_asignacion_final: Lista de 12 elementos:
#' 1) mat_asignacion_final: Matriz de 3 columnas (Materia,Profesor,
#' Horario). Contiene la asignación final encontrada con el algoritmo
#' genético.
#' 2) calif_mejor_elem: Vector con calificaciones de los mejores elementos
#' por generación.
#' 3) mat_calif_generaciones: Matriz con calificaciones de todos los
#' elementos de todas las generaciones.
#' 4) matrices_calif_x_generacion: Lista de tamaño num_generaciones+1
#' con las matrices de calificaciones ordenadas por generación.
#' 5) mejores_asig: Lista de tamaño num_generaciones+1 con la información
#' de los mejores hijos de cada generación.
#' 6) mat_num_genes: Matriz con el número de genes de todos los elementos
#' por generación.
#' 7) mat_esqueleto
#' 8) mat_solicitudes_real
#' 9) param
#' 10) vec_info_AG: Vector con información del AG y sus resultados.
#' 11) esq_asig_final: Esqueleto de la asignación final.
#' 12) info_gpos_sin_asig: Matriz con las columnas: mat_esq (gpos. por
#' materia en mat_esqueleto), esq_asig_fin (gpos. x materia en
#' esq_asig_final), gpos_sin_asig (gpos. sin asignación x materia),
#' dif_rel (diferencia relativa x materia).
#'
#' @examples
#' list_asignacion_final <- AG_asignaciones_con_xlsx(mat_esqueleto,
#' mat_solicitudes_real,mat_esqueleto_cotas,param)
#' 
AG_asignaciones_con_xlsx <- function(mat_esqueleto,mat_solicitudes_real,
                                     con_xlsx_1_sin_xlsx_0,param){
  if(con_xlsx_1_sin_xlsx_0 == 0){
    #' Si no se carga el archivo de excel se realiza la asignación
    #' con el AG de manera norma. Sin modificar las matrices con el
    #' esqueleto y las solicitudes.
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




