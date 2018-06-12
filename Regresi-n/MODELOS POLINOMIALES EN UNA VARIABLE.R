# Datos
concentracion <- c(1, 1.5, 2, 3, 4, 4.5, 5, 5.5, 6, 6.5, 7:15)

resistencia <- c(6.3, 11.1, 20, 24, 26.1, 30, 33.8, 34, 38.1,
                 39.9, 42, 46.1, 53.1,52, 52.5, 48, 42.8, 27.8,21.9)

# Diagrama de dispersion
plot(resistencia ~ concentracion, pch=19, las=1,
     xlab='Concentracion de madera dura (%)',
     ylab='Resistencia a la tension')

# modelo lineal
y <- resistencia
x <- concentracion
mod1 <- lm(y ~ x)
summary(mod1)

fun <- function(x) sum(coef(mod1) * c(1, x))
fun <- Vectorize(fun)
curve(fun, from=1, to=15, col='red', lwd=3, add=T)

# modelo cuadratico
mod2 <- lm(y ~ x + I(x^2))
summary(mod2)

fun <- function(x) sum(coef(mod2) * c(1, x, x^2))
fun <- Vectorize(fun)
curve(fun, from=1, to=15, col='blue', lwd=3, add=T)

# Anova
anova(mod1, mod2)

# Residuales
par(mfrow=c(2, 2))
plot(mod1, pch=19)
plot(mod2, pch=19)
