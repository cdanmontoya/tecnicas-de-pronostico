modelo_sarma <- arima(
  residuos,
  order = c(2, 0, 0),
  seasonal = list(order = c(1, 0, 2), period = 12),
  include.mean = TRUE
)
    
coeftest(modelo_sarma)

residuos_sarma = residuals(modelo_sarma)

TSA::acf(residuos_sarma, 36, ci.type = "ma", drop.lag.0 = TRUE)
Box.test(residuos_sarma,lag = 36, type= "Ljung-Box")