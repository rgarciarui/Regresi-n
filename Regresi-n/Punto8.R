require(MPV)
datos <- softdrink
head(datos)
attach(datos)
require(rgl)
plot3d(x=x1,y=x2,z=y, type = 's', col='blue',
       xlab='Cantidad',
       ylab='Distancia (pies)',
       zlab='Tiempo (min)')
colnames(datos) <- c('Tiempo', 'Cantidad', 'Distancia')
mod2 <- lm(data = datos, Tiempo ~ Cantidad+ Distancia+ Cantidad*Distancia)
summary(mod2)
mod1 <- lm(Tiempo ~ Cantidad + Distancia, data = datos)
summary(mod1)

##El modelo que tiene el R2 ajustado mas alto es mod2 

fun <- function(x1, x2) sum(coef(mod2) * c(1, x1, x2))
fun <- Vectorize(fun)
x1 <- seq(from=2, to=30, length.out=10)
x2 <- seq(from=36, to=1460, length.out=10)
y <- outer(x1, x2, fun)

# Superficie en 3d
persp(x1, x2, y, theta=30, phi=30,
      ticktype = "detailed", nticks=3,
      col='lightblue', border='blue',
      xlab='Cantidad', ylab='Distancia (pies)',
      zlab='Tiempo (min)')

