#TRABAJO N2
require(TSA)
require(dynlm)
require(lmtest)

##FRACMENTO DEL TRABAJO ANTERIOR
y = ts(datos.precio_imputado$y$Series, frequency=12)
t = seq(1, length(y))
t2 = t^2
t3 = t^3
It = seasonaldummy(y)
mod.cuadratico = lm(y ~ t + t2 + It)
M.cuadratico = medidas(mod.cuadratico, y, 3)

dy = diff(y, 1, 1)
plot(dy)
plot(y)

M.cuadratico

#comparación graficos acf, pacf

#Con base en la ACF muestral probamos si una serie de tiempo proviene de
#un proceso de ruido blanco

par(mfrow=c(2,2))
plot(y, main="")
acf(y,30,ci.type="ma",drop.lag.0 = TRUE)
pacf(y,30)
plot(dy)

#su ACF muestral indica que no se cumple la ergodicidad: 
#En los k sucesivos decaelento a causa de la tendenciA

library(aTSA)
aTSA::adf.test(y)


#ACF de resiudales del modelo cuadratico

residuos <- residuals(mod.cuadratico)

# ACF de la serie original
TSA::acf(y, 30, ci.type = "ma", drop.lag.0 = TRUE)



# Gráfica de la serie de residuos
ts.plot(residuos)


# ACF de los residuos
TSA::acf(residuos, 30, ci.type = "ma", drop.lag.0 = TRUE)
#los errores del modelo no  son ruido blanco, la acf indica que el proceso es
#ergódico, siguiendo un patron tipo cola exponencial sinusoidal amortiguada

Box.test(residuos,lag = 15, type= "Ljung-Box")

#No es ruido blanco,se rechaza la hipotesis nula.





#2.

# Identificar un posible modelo ARMA-SARMA con auto.arima
modelo_auto_arima <- auto.arima(y, stationary = TRUE, seasonal = TRUE, ic = "aicc")
summary(modelo_auto_arima)


modelo_arima <- arima(y, order = c(2, 0, 0), seasonal = list(order = c(2, 0, 0), period = 12))

summary(modelo_arima)
#DurbinWatson_test <- dw(modelo_arima)


coeftest(modelo_arima)

#at=ts(modelo_arima$residuals,frequency=12)
#pruebas de no linealidad (no aplica es solo para red neuronal)
#terasvirta.test(at)
#white.test(at)
#grafica
#ts.plot(at)


#3. # Calcular el AIC del modelo cuadrático
AIC_cuadratico <- AIC(mod.cuadratico)

# Calcular el AIC del modelo ARIMA sugerido por auto.arima
AIC_auto_arima <- AIC(modelo_auto_arima)

