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
legend(
    "topright",
    legend = c("Datos reales", "Cuadrático", "Cuadrático + SARMA"),
    col = c("black", "red", "magenta"), 
    lty = 1, 
    lwd = 2
)