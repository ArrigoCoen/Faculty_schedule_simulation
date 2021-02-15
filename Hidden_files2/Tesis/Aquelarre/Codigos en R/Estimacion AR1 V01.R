# AR ----------------------------------------------------------------------



#' Ajuste a un paso de AR(1)
#'
#' @param vector vector que se le ajusta un AR(1)
#'
#' @return regresa un texto con la estimación
#' @export
#'
#' @examples
funcion_AR1_un_paso <- function(vector, solo_resumen=F) {
  if(solo_resumen==F){
  cat("la media es ",mean(vector),"\n")
  cat("la varianza es ",var(vector),"\n")
  
  # Fit an AR model to Nile
  AR_fit <-arima(vector, order  = c(1,0,0))
  print(AR_fit)
  
  # Use predict() to make a 1-step forecast
  predict_AR <- predict(AR_fit , n.ahead = 1)
  # Valor predecido
  # predict_AR$pred[1]
  # Run to plot the Nile series plus the forecast and 95% prediction intervals
  # ts.plot(vector, xlim = c(0,8),ylim=c(300,1000))
  AR_fit <-arima(vector, order  = c(1,0,0))
  AR_forecast <- predict(AR_fit, n.ahead = 1)$pred
  AR_forecast_se <- predict(AR_fit, n.ahead = 1)$se
  # points(AR_forecast, type = "l", col = 2)
  # points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
  # points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)
  cat("=> Se tiene el valor esperado ", AR_forecast," y \n el intervalo a 95% es (",
      AR_forecast - 2*AR_forecast_se,",",AR_forecast + 2*AR_forecast_se,").\n")
  }
  if(solo_resumen==T){
    
    # Fit an AR model to Nile
    AR_fit <-arima(vector, order  = c(1,0,0))
    # print(AR_fit)
    
    # Use predict() to make a 1-step forecast
    predict_AR <- predict(AR_fit , n.ahead = 1)
    # Valor predecido
    # predict_AR$pred[1]
    # Run to plot the Nile series plus the forecast and 95% prediction intervals
    # ts.plot(vector, xlim = c(0,8),ylim=c(300,1000))
    AR_fit <-arima(vector, order  = c(1,0,0))
    AR_forecast <- predict(AR_fit, n.ahead = 1)$pred
    AR_forecast_se <- predict(AR_fit, n.ahead = 1)$se
    # points(AR_forecast, type = "l", col = 2)
    # points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
    # points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)
    cat("=> Se tiene el valor esperado ", AR_forecast," y \n el intervalo a 95% es (",
        AR_forecast - 2*AR_forecast_se,",",AR_forecast + 2*AR_forecast_se,").\n")
    
  }
}

alum_impar_mat <- c(678,676,582,510,706,689)
alum_impar_vesp <- c(173,164,216,172,171,255)
alum_par_mat <- c(327,418,329,309,233)
alum_par_vesp <- c(83,72,159,219,133)


funcion_AR1_un_paso(alum_impar_mat)
funcion_AR1_un_paso(alum_impar_vesp)
funcion_AR1_un_paso(alum_par_mat)
funcion_AR1_un_paso(alum_par_vesp)

if(1){
funcion_AR1_un_paso(alum_impar_mat,T)
funcion_AR1_un_paso(alum_impar_vesp,T)
funcion_AR1_un_paso(alum_par_mat,T)
funcion_AR1_un_paso(alum_par_vesp,T)
}




# lksjdfka ----------------------------------------------------------------

