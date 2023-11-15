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