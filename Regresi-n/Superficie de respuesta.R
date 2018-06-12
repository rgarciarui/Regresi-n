# Seccion 7.4 de MPV
# Ejemplo superficie respuesta

# -----------------------------------------------------------------
# Los datos
# -----------------------------------------------------------------
temp <- c(200, 250, 200, 250, 189.65, 260.35, 
          225, 225, 225, 225, 225, 225)

conc <- c(15, 15, 25, 25, 20, 20, 12.93, 27.07,
          20, 20, 20, 20)

rend <- c(43, 78, 69, 73, 48, 76, 65, 74, 76, 79, 83, 81)

# -----------------------------------------------------------------
# Diagramas de dispersión 3d
# -----------------------------------------------------------------

library(scatterplot3d)
scatterplot3d(x=temp, y=conc, z=rend, pch=16, cex.lab=1.5,
              highlight.3d=TRUE, type="h")

require(rgl)
plot3d(x=temp, y=conc, z=rend, lwd=2, col='pink',
       xlab='Temperatura', ylab='Concentracion', type='s',
       zlab='Rendimiento')

# -----------------------------------------------------------------
# Modelo polinomial ajustado
# -----------------------------------------------------------------
mod <- lm(rend ~ temp + conc + I(temp^2) + I(conc^2) + temp * conc)
summary(mod)
coef(mod)

# Analisis de residuales
par(mfrow=c(2, 2))
plot(mod, pch=19)

# -----------------------------------------------------------------
# Construyendo la superficie de respuesta
# -----------------------------------------------------------------

# Forma 1: manualmente

# Se crean los valores de las variables para la rejilla
# para cada variable se toma desde el minimo hasta
# maximo.
Temperatura   <- seq(from=189.65, to=260.35, length.out=30)
Concentracion <- seq(from=12.93, to=27.07, length.out=30)

# Rend es la funcion a dibujar, se puede colocar la ecuacion de la
# superficie o se coloca coef(mod) * vector(variables_en_orden)
Rend <- function(temp, conc) {
  res <- coef(mod) * c(1, temp, conc, temp^2, conc^2, temp * conc)
  sum(res)
}

# Otra forma de crear la funcion Rend medio estimado es asi:
Rend <- function(temp, conc) {
  -1105.559 + 8.024 * temp + 22.994 * conc + -0.014 * temp^2 + 
    -0.205 * conc^2 + -0.062 * temp * conc
}

# La funcion a dibujar debe estar vectorizada
Rend <- Vectorize(Rend)

# La matriz Rendimiento con las alturas se crea con outer
Rendimiento <- outer(Temperatura, Concentracion, Rend)

# Para dibujar la superficie de respuesta
persp(x=Temperatura, y=Concentracion, z=Rendimiento,
      theta=40, phi=30, ticktype = "detailed", col='salmon1')

# Para obtener el grafico de contorno
contour(x=Temperatura, y=Concentracion, z=Rendimiento,
        nlevels=10, col=gray(0.3), lwd=2, lty='solid',
        xlab='Temperatura', ylab='Concentracion', las=1)

# Para obtener un grafico de calor
filled.contour(x=Temperatura, y=Concentracion, z=Rendimiento,
               nlevels=10,
               xlab='Temperatura', ylab='Concentracion', las=1,
               color.palette = cm.colors)

# Para obtener una superficie que se puede mover con el mouse
persp3d(x=Temperatura, y=Concentracion, z=Rendimiento, 
        col="lightblue", back = "lines",
        xlab='Temperatura', ylab='Concentracion')

# Forma cool: usando el paquete plotly

require(plotly)

plot_ly(z = Rendimiento, x=Temperatura, y=Concentracion, type = "surface") %>%
  layout(title = "Superficie del Rendimiento",
         scene = list(
           xaxis = list(title = "Temperatura"), 
           yaxis = list(title = "Concentracion"), 
           zaxis = list(title = "Rendimiento")))

# Para consultar otros ejemplos de graficos con plotly corra la linea
browseURL('https://plot.ly/r/')


# Forma 2: usando el paquete rsm

library(rsm)
image(mod, conc ~ temp)
contour(mod, conc ~ temp)
persp(mod, conc ~ temp, zlab = "Rendimiento")

persp(mod, temp ~ conc, col = "blue",
      bounds = list(temp=c(150, 300), conc=c(10, 30)),
      zlab = "Rendimiento", 
      contours = list(z="bottom", col="blue"),
      theta = -145, phi = 35)

# Nota: si va a usar rsm debe leer con detalle la documentacion, ojo.

# -----------------------------------------------------------------
# Encontrando las coordenadas de temp y conc que maximizan el rend
# -----------------------------------------------------------------

# Funcion que entrega -rendimiento segun un valor de temp y conc
# Maximizar f(x) es lo mismo que minimizar -f(x)
# El argumento de minus_rend DEBE ser un vector !!!

minus_rend <- function(x) {
  temp <- x[1]
  conc <- x[2]
  new.data <- data.frame(temp=c(1, temp), conc=c(1, conc))
  -predict(mod, new.data)[2]
}

inicio <- c(192, 15)  # valores iniciales para la busqueda
names(inicio) <- c('Temperatura', 'Concentracion') # Colocando nombres

res <- nlminb(start=inicio, objective=minus_rend,
              lower=c(189.65, 12.93), # minimos de las variables
              upper=c(260.35, 27.07), # maximos de las variables
              control=list(trace=1))

res # Para ver todo el contenido de res
res$par  # Valores optimos
-res$objective  # Valor del objetivo

# -----------------------------------------------------------------
# Como evoluciona la busqueda?
# -----------------------------------------------------------------

# En camino se captura la salida de nlminb para dibujar la ruta
# de optimizacion
camino <- capture.output(a <- nlminb(start=inicio, objective=minus_rend,
                                     lower=c(180, 10), upper=c(260, 30),
                                     control=list(trace=1)))
camino

steps <- length(camino)
Trace <- matrix(0, ncol=2, nrow=steps)
for (i in 1:steps) {
  val <- c(as.numeric(substr(camino[i], 22, 28)),
           as.numeric(substr(camino[i], 31, 37)))
  Trace[i, ] <- val
}

Trace

# Dibujando el contorno
contour(x=Temperatura, y=Concentracion, z=Rendimiento,
        nlevels=10, col=gray(0.3), lwd=2, lty='solid',
        xlab='Temperatura', ylab='Concentracion', las=1)

points(res$par[1], res$par[2], pch=19, cex=2, col='red')
abline(v=res$par[1], lty="dotted", col='tomato')
abline(h=res$par[2], lty="dotted", col='tomato')
lines(Trace, pch=19, col='red', type='b')
points(inicio[1], inicio[2], col='blue', pch=19, cex=2)
text(x=Trace[9, 1], y=Trace[9, 2], 'Aqui el maximo',
     col='purple')

