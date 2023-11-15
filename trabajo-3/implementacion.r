library(readxl)
library(pastecs)
library(forecast)
library(xtable)
library(strucchange)
require(TSA)
require(dynlm)
require(lmtest)
require(timsac)
require(xts)
library(uroot)
​
# Cargando datos.
datos <- read_excel("precio.naranja.usa.ciudades.xls", skip = 10)
colnames(datos)[1] ="fecha"
colnames(datos)[2] ="precio"
​
​
# Imputación de valores ausentes.
n = length(datos$precio)
x = seq(1,n)
datos.precio_imputado = regul(
  x,
  y=datos$precio,
  xmin=min(x),
  n=length(x),
  units="months",
  deltat=1,
  methods="spline",
  rule=2,
  f=0.5,
  periodic=FALSE,
  window=(max(x) - min(x))/(n - 1),
  split=100,
  specs=NULL
)
​
​
# Definiendo como serie de tiempo.
y = ts(datos.precio_imputado$y$Series, frequency=12)
​
# Data para validación cruzada.
m = 12
T = length(y)
yi = ts(y[1:(T-m)], frequency = m)
yf = ts(y[(T-m+1):T], frequency = m)
​
#1. (34/34) Estime un modelo ARIMA-SARIMA para la serie original, con base
#en el resultado de auto.arima(). Con el modelo escogido valide los residuos
#con la fac y la prueba Ljung-Box. Reporte los resultados.
​
#Gráfica de la serie Precios Naranja USD
ts.plot(yi) # Tendencia creciente y varianza no constante. Parece marcha aleatoria con tendencia: esto justifica pensar en integración (sacar la diferencia).
​
# Autocorrelograma.
par(mfrow=c(1,2))
TSA::acf(yi, 36, ci.type = "ma", drop.lag.0 = TRUE) # Es persistente, lo que sugiere una marcha aleatoria.
pacf(yi,36) # Se observa la convergencia inmediata típica de un proceso con raíz unitaria.
​
# Diferenciando y_t.
dyi = diff(yi,1,1) # Diferencia ordinaria.
​
# Graficando dyi.
par(mfrow=c(2,2))
ts.plot(yi,main='(A)') # Serie original.
ts.plot(dyi,main='(B)') # Se elimina tendencia.
TSA::acf(dyi, 36, ci.type = "ma", drop.lag.0 = TRUE) ##############################################################################################################################################################3
pacf(dyi,36) # Notamos presencia de un posible modelo, es decir, puede que no hay ruido blanco.
​
# Usamos auto.arima() para que nos sugiera un modelo sobre dyi.
auto.arima(dyi, stationary = TRUE, seasonal = TRUE, ic = "aicc") # Sugiere: ARIMA(4,0,0)(2,0,0)[12] with non-zero mean.
​
# Como sugiere SARMA para dyi, entonces cambiamos estrategia para un SARIMA. Miremos la serie y sus diferenciadas ordinaria (ut) y estacional (wt).
# Probablemente al tener tendencia estocástica y otra de estacionalidad, entonces nos qudemos con wt.
ut = diff(yi, 12, 1) # Diferencia estacional.
wt = diff(diff(yi, 1, 1), 12, 1) # Diferencia estacional y ordinaria.
​
# Graficando las diferencias ordinaria y estacional.
par(mfrow=c(2,2))
ts.plot(yi,main='(A)') # Serie original con tendencia estocástica y estacionalidad.
ts.plot(ut,main='(B)') # Se elimina estacionalidad.
ts.plot(wt,main='(C)') # Se elimina tendencia y un poco de la estacionalidad.
TSA::acf(wt, 36, main='(D)', ci.type = "ma", drop.lag.0 = TRUE) # Tiene rezagos significativos en 1, 12 y 13, por lo que puede existir un proceso MA(q).
​
# Recibiendo la sugerencia del modelo.
auto.arima(wt, stationary = TRUE, seasonal = TRUE, ic = "aicc") # Sugiere: ARIMA(3,0,0)(0,0,2)[12] with zero mean.
​
# Estimando el modelo.
modelo <- arima(wt, order = c(3, 0, 0), seasonal = list(order = c(2, 0, 0), period = 12))
​
# Autocorrelograma y autocorrelograma parcial para observar si puede ser ruido blanco.
residuos_modelo <- ts(residuals(modelo),frequency = 12)
par(mfrow=c(1,2))
TSA::acf(residuos_modelo, 36, ci.type = "ma", drop.lag.0 = TRUE) 
pacf(residuos_modelo,36) # Notamos presencia de un posible modelo por el traslape de los límites de Barlett, es decir, no hay ruido blanco.
​
# Evaluamos, verificamos que sus residuos sean ruido blanco. Primero gráficamente y luego con Ljung-Box.
# Prueba Ljung-Box (H_0: ruido blanco. p-value < 0.05 rechaza la nula).
Box.test(residuos_modelo,lag = 36, type= "Ljung-Box") # Evidencia de que no son ruido blanco. Por lo tanto el modelo SARIMA no se puede usar para pronosticar.
​
​
# 2. (33/33) Realice las Pruebas de raiz unitaria estacional: 1)Dickey­Fuller au­
# mentada, 2) HEGY. Concluya sobre si existen ra´ıces unitaria ordinaria y esta­
#cionales.
​
# La prueba de HEGY: H0: serie estacionaria cointegrada.
hegy.test(yi,deterministic=c(1,0,0)) # Se rechazan todas las H0 de presencia de raíces unitarias estacionales y se clasifica como una srie "estacionaria estacional".
​
​
#3. (33/33)Calcule los pron´osticos para la validaci´on cruzada con los 2modelos: 1)
#el que mejor pronostic´o en los Trabajos No 1 y 2, versus 2) el modelo ARIMA
#SARIMA encontrado en el punto 1). Reporte MAPE, RMSE, U­Theil para
#ambos conjuntos de pron´osticos. Concluya cu´al modelo pronostic´o mejor.
​
​
​
​
​
​
​
​
​
​
​
​
​
​
​
​
​
​
​
​
​
​
​
​
# Diferencia ordinaria, diferencia estacional y transformada (filtro lineal).
dyo=diff(yi,1,1)
dys=diff(yi,12,1)
dyos=diff(diff(yi,12,1),1,1)
​
​
par(mfrow=c(2,2))
ts.plot(y,main='(A)')
ts.plot(dyo,main='(B)')
ts.plot(dys,main='(C)')
ts.plot(dyos,main='(D)') # Con dyos eliminamos tendencia estocástica y estacionalidad. En las otras parece mantenerse estacionalidad.
​
# Autocorrelograma y autocorrelograma parcial.
par(mfrow=c(1,2))
TSA::acf(dyos, 36, ci.type = "ma", drop.lag.0 = TRUE) 
pacf(dyos,36) # Notamos presencia de un posible modelo, es decir, no hay ruido blanco.
​
​
# Miramos que modelo nos sugiere autoarima.
modelo_auto_arima <- auto.arima(dyos, stationary = TRUE, seasonal = TRUE, ic = "aicc")
modelo_auto_arima
​
# Estimamos el modelo sugerido.
modelo_sarima<-arima(y, order = c(3, 0, 0), seasonal = list(order = c(0, 0, 2), period = 12))
​
# Evaluamos que sus residuos sean ruido blanco. Primero gráficamente y luego con Ljung-Box.
residuos_sarima <- ts(residuals(modelo_sarima),frequency = 12)
​
# Autocorrelograma y autocorrelograma parcial.
par(mfrow=c(1,2))
TSA::acf(residuos_sarima, 36, ci.type = "ma", drop.lag.0 = TRUE) 
pacf(residuos_sarima,36)
​
# Prueba Ljung-Box (H_0: ruido blanco. p-value < 0.05 rechaza la nula).
Box.test(residuos_sarima,lag = 36, type= "Ljung-Box") # Evidencia de que son ruido blanco. Favorece al modelo.
​
​
​
​
# Prueba Dickey-Fuller.
aTSA::adf.test(dyos)
​
# Prueba HEGY.
library(uroot)
hegy.test(y,deterministic=c(1,0,0))
#Se rechaza la h0 de raices unitarias 
​
​
​
​
​
​
m3.pred = predict(modelo_sarima,n.ahead = length(yf))
prons.sa = m3.pred$pred
​
residuos<-ts(residuals(mod.cuadratico),frequency=12)
modelo_sarma <- arima(residuos,order = c(2, 0, 0),seasonal = list(order = c(1, 0, 2), period = 12), include.mean = TRUE)
residuos_sarma = residuals(modelo_sarma)
​
​
y_estimado = predict(mod.cuadratico, data.frame(ti = tf, ti2 = tf2, Iti = I(Itf)))
y_estimado2=forecast(modelo_auto_arima,h=12)
y_estimado2=y_estimado2$mean
​
e_estimado = predict(modelo_sarma, n.ahead=12)$pred
​
y_total = y_estimado + e_estimado
​
​
​
​
# Calcular la precisión del modelo para la serie total y la serie estimada
accuracy_total <- accuracy(ts(y_total, frequency=12), yf)
accuracy_estimado <- accuracy(ts(y_estimado, frequency=12), yf)
​
# Establecer el diseño del gráfico
par(mfrow=c(1,1))
​
# Graficar los resultados
plot(tf, yf, type = 'o', lwd=2, ylim = c(1.3, 2), xlab = "Tiempo", ylab = "Precio estimado")
lines(tf, y_estimado, col='Blue')
lines(tf, y_total, col='red')
lines(tf, prons.sa, col='green')
​
# Agregar leyenda al gráfico
legend(
  "topright",
  legend = c("Cuadrático", "Cuadrático + SARMA","SARIMA"),
  col = c("blue", "red", "green"),
  lty = 1,
  lwd = 2
)
​
​
​
​
​
​
#Gráfica de ACF
TSA::acf(y, 36, ci.type = "ma", drop.lag.0 = TRUE) 
#Sugiere un proceso integrado debido a que la ACF es persistente
​
#Se realiza la primera diferencia
dy=diff(y,1,1)
ts.plot(dy)
#Eliminamos la tendencia, pero se observa una varianza no constante.
​
par(mfrow=c(1,2))
TSA::acf(dy, 36, ci.type = "ma", drop.lag.0 = TRUE) 
pacf(dy)
​
​
​
​
# Gráfica de la serie de 
ts.plot(dy)
#Media alrededor de cero, la varianza no es clara, puede heterocedastica
​
# ACF de los residuos
​
dyos=diff(diff(y,12,1),1,1)
dys=diff(y,12,1)
dyo=diff(y,1,1)
dyos=ts(dyos,frequency = 12)
​
​
par(mfrow=c(2,2))
ts.plot(y,main='(A)')
ts.plot(dyo,main='(B)')
ts.plot(dys,main='(C)')
ts.plot(dyos,main='(D)')
​
​
par(mfrow=c(1,2))
TSA::acf(dyos, 36, ci.type = "ma", drop.lag.0 = TRUE) 
pacf(dyos,36)
​
aTSA::adf.test(y)
​
modelo_auto_arima <- auto.arima(dyos, stationary = TRUE, seasonal = TRUE, ic = "aicc")
modelo_auto_arima
​
residuos_auto_arima <- residuals(modelo_auto_arima)
​
# Gráfica de la serie de residuos
ts.plot(residuos_auto_arima)
abline(h = mean(residuos_auto_arima), col = "red", lty = 2)
​
​
​
​
​
#3. (33/33)Calcule los pron´osticos para la validaci´on cruzada con los 2modelos: 1)
#el que mejor pronostic´o en los Trabajos No 1 y 2, versus 2) el modelo ARIMA
#SARIMA encontrado en el punto 1). Reporte MAPE, RMSE, U­Theil para
#ambos conjuntos de pron´osticos. Concluya cu´al modelo pronostic´o mejor.
​
​
modelo_sarma <- arima(residuos,order = c(2, 0, 0),seasonal = list(order = c(1, 0, 2), period = 12), include.mean = TRUE)
residuos_sarma = residuals(modelo_sarma)
​
​
y_estimado = predict(modelo_auto_arima, data.frame(ti = tf, ti2 = tf2, Iti = I(Itf)))
y_estimado=forecast(modelo_auto_arima,h=12)
y_estimado=y_estimado$mean
​
e_estimado = predict(modelo_sarma, n.ahead=12)$pred
​
y_total = y_estimado + e_estimado
​
str(y_estimado)