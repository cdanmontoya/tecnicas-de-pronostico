#install.packages("aTSA")
#install.packages("TSA")
#install.packages("dynlm")
#install.packages("lmtest")
#install.packages("timsac")
#install.packages("xts")

setwd("~/Documents/unal/tecnicas-de-pronostico/trabajo-1/")

# Cargar librerías
library(readxl)
library(pastecs)
library(forecast)
library(xtable)
library(strucchange)

source("medidas.r")
source("medidas.yest.r")
source("medidas.hw.r")

#TRABAJO N2

require(TSA)
require(dynlm)
require(lmtest)
require(timsac)
require(xts)

datos <- read_excel("precio.naranja.usa.ciudades.xls", skip = 10)
colnames(datos)[1] ="fecha"
colnames(datos)[2] ="precio"

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




##FRACMENTO DEL TRABAJO ANTERIOR
m = 12
y = ts(datos.precio_imputado$y$Series, frequency=m)
T = length(y)
yi = ts(y[1:(T-m)], frequency = m)
yf = ts(y[(T-m+1):T], frequency = m)

ti = seq(1,length(yi))
ti2 = ti^2

tf = seq(T-m+1,T)
tf2 = tf*tf

Iti = seasonaldummy(yi)
Itf = seasonaldummy(yi, m)

mod.cuadratico = lm(yi ~ ti + ti2 + Iti)




#1. 

residuos <- ts(residuals(mod.cuadratico), frequency = m)
#Dado que viene de la regresión del modelo, se cumple que tiene media cero

# Gráfica de la serie de residuos
ts.plot(residuos)
abline(h=mean(residuos), col="red", lty=2)


#Media alrededor de cero, la varianza no es clara, puede heterocedastica



# ACF de los residuos
TSA::acf(residuos, 36, ci.type = "ma", drop.lag.0 = TRUE)

#La ACF de los residuos proporciona información sobre las autocorrelaciones 
#en los residuos, lo que puede ayudar a identificar patrones en los errores
#que el modelo no ha capturado.
#los errores del modelo no  son ruido blanco, la acf indica que el proceso es
#ergódico, siguiendo un patron tipo cola exponencial sinusoidal amortiguada

Box.test(residuos,lag = 36, type= "Ljung-Box")

#No es ruido blanco,se rechaza la hipotesis nula.
#La hipótesis nula de que el residuo estructural distribuye como un
#ruido blanco con media cero y varianza constannte es rechazada  obtenido 
#el valor-p menor a 0.05



#2.
# residuos = ts(residuos, frequency = m)
# Identificar un posible modelo ARMA-SARMA con auto.arima
modelo_auto_arima <- auto.arima(residuos, stationary = TRUE, seasonal = TRUE, ic = "aicc")
summary(modelo_auto_arima)

#se sugiere un AR(2), pero cuando lo corremos nos dice que no es ruido blanco
# por lo que luego intentamos modelar los residuos como un SARMA. 
# realizamos un barrido para determinar el valor adecuado de la componente estacional


modelo_sarma <- arima(
  residuos,
  order = c(2, 0, 0),
  seasonal = list(order = c(1, 0, 2), period = 12),
  include.mean = TRUE
)

summary(modelo_sarma)




#GRÁFICOS



coeftest(modelo_sarma)

residuos_sarma = residuals(modelo_sarma)

TSA::acf(residuos_sarma, 36, ci.type = "ma", drop.lag.0 = TRUE)
Box.test(residuos_sarma,lag = 36, type= "Ljung-Box")

plot(residuos_sarma)
abline(h=mean(residuos), col="red", lty=2)
acf(residuos_sarma)
pacf(residuos_sarma)

qqplot(qnorm(ppoints(length(residuos_sarma)), mean = mean(residuos_sarma), sd = sd(residuos_sarma)), residuos_sarma,
       main = "Gráfico cuantil-cuantil de los residuos")
abline(0, 1, col = "red")



# ---------------------


    


#3. # Calcular el AIC del modelo cuadrático

y_estimado = predict(mod.cuadratico, data.frame(ti = tf, ti2 = tf2, Iti = I(Itf)))
e_estimado = predict(modelo_sarma, n.ahead=12)$pred

y_total = y_estimado + e_estimado

accuracy(ts(y_total, frequency=12), yf)
accuracy(ts(y_estimado, frequency=12), yf)

par(mfrow=c(1,1))
plot(tf, yf, type = 'o', lwd=2, ylim = c(1.4, 1.8), xlab = "Tiempo", ylab = "Precio estimado")
lines(tf, yf, col='black')
lines(tf, y_estimado, col='red')
lines(tf, y_total, col='magenta')

# Agregar etiquetas a puntos específicos
legend("topright", legend = c("Datos reales", "Cuadrático", "Cuadrático + SARMA"), col = c("black", "red", "magenta"), lty = 1, lwd = 2)



       