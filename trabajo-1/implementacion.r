## Instalación de librerías
#install.packages("readxl")
#install.packages("pastecs")
#install.packages("forecast")
#install.packages("xtable")
#install.packages("strucchange")

# Cargar librerías
library(readxl)
library(pastecs)
library(forecast)
library(xtable)
library(strucchange)

source("medidas.r")
source("medidas.hw.r")

datos <- read_excel("precio.naranja.usa.ciudades.xls", skip = 10)
colnames(datos)[1] ="fecha"
colnames(datos)[2] ="precio"


np = nrow(datos)
#-----------------------convierte fecha a formato de R
fechas = as.Date(datos$fecha,format="%Y/%m/%d")
ejex.mes = seq(fechas[1],fechas[np], "months")
ejex.anio = seq(fechas[1],fechas[np],"years")

plot(
  fechas,
  datos$precio,
  xaxt="n",
  panel.first = grid(),
  type='l',
  lwd=2,
  ylab='Costo promedio ($USD)'
)
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.anio, labels = FALSE, tcl = -0.2)

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

layout(1:2)
ts.plot(datos$precio, ylab="original")
ts.plot(datos.precio_imputado$y$Series,ylab="imputada")

year = 25
plot(
  fechas,
  datos.precio_imputado$y$Series,
  xaxt="n",
  panel.first = grid(),
  type='l',
  lwd=2,
  ylab='Costo promedio ($USD)',
  xlim = c(fechas[1 + 12*year], fechas[12 + 12*year])
)
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.anio, labels = FALSE, tcl = -0.2)


y = ts(datos.precio_imputado$y$Series, frequency=12)
lny = log(y)

t = seq(1, length(y))
t2 = t^2
t3 = t^3
It = seasonaldummy(y)

mod.cuadratico = lm(y ~ t + t2 + It)

mod.cubico = lm(y ~ t + t2 + t3 + It)

mod.loglineal = lm(lny ~ t + It)
Xt.lin = cbind(rep(1, n), t, It)
Ds.lin = data.frame(y, Xt.lin)
theta.0.lin = coef(mod.loglineal)
mod.exponencial_lineal = nls(y ~ exp(Xt.lin%*%theta), data = Ds.lin, start = list(theta = theta.0.lin))

mod.logcuadratico = lm(lny ~ t + t2 + It)
Xt.cuad = cbind(rep(1, n), t, t2, It)
Ds.cuad = data.frame(y, Xt.cuad)
theta.0.cuad = coef(mod.logcuadratico)
mod.exponencial_cuadratico = nls(y ~ exp(Xt.cuad%*%theta), data = Ds.cuad, start = list(theta = theta.0.cuad))

mod.hw = hw(y, seasonal = "add", damped = TRUE)

mod.nnar = nnetar(y, lambda=0)


