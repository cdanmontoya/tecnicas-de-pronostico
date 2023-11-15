residuos <- ts(residuals(mod.cuadratico), frequency = m)

# Gráfica de la serie de residuos
ts.plot(residuos)
abline(h=mean(residuos), col="red", lty=2)

# ACF de los residuos
TSA::acf(residuos, 36, ci.type = "ma", drop.lag.0 = TRUE)
Box.test(residuos,lag = 36, type= "Ljung-Box")