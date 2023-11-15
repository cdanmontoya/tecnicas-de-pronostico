residuos = ts(residuos, frequency = 12)
modelo_auto_arima <- auto.arima(residuos, stationary = TRUE, seasonal = TRUE, ic = "aicc")

summary(modelo_auto_arima)