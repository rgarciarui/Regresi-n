# Diferencia entre polinomios no ortogonales y ortogonales ----------------
x <- c(5, 3, 6, 1, 5, 8, 5, 6, 5)  # Una variable cualquiera

mat1 <- model.matrix( ~ x + I(x^2) + I(x^3))
mat2 <- model.matrix( ~ poly(x, degree=3))

# Matrices de diseno
mat1
mat2

# Matriz de correlaciones
round(cor(mat1[, -1]), digits=2)
round(cor(mat2[, -1]), digits=2)


# Dibujando los dos tipos de polinomios -----------------------------------
poten <- function(x, k) x ^ k

paleta <- c("#000040", "#0063FF", "#13F240", "#FFE201", "#FF3300")

par(mfrow=c(1, 2))
curve(poten(x, k=1), from=-3, to=3, ylim=c(-10, 15),
      main='Polinomios usuales', col=paleta[1], ylab='', lwd=2)
for (i in 2:5) curve(poten(x, k=i), from=-3, to=3, add=T, col=paleta[i], lwd=2)

legend('bottomright', legend=1:5, col=1:5, lwd=2, bty='n', cex=0.7)

xx <- seq(from=-3, to=3, by=0.1)
dt <- poly(xx, degree=5)
plot(x=xx, y=dt[, 1], type='l', col=paleta[1], lwd=2,
     ylab='', xlab='x', main='Polinomios ortogonales')
for (i in 2:5) lines(x=xx, y=dt[, i], type='l', col=paleta[i], lwd=2)


# Ejemplo de regresion con polinomio ortogonal ----------------------------
n <- 1000
x <- runif(n=n, min=0, max=30)
y <- rnorm(n=n, mean= -3 -2 * x + 1 * x^2, sd=60)

# Ajustando los modelos
mod1 <- lm(y ~ poly(x, degree=2, raw=TRUE))   # Polinomio NO ortogonal
mod2 <- lm(y ~ poly(x, degree=2, raw=FALSE))  # Polinomio ortogonal

# comparando los resultados
summary(mod1)
summary(mod2)

# Agregando las líneas ajustadas
par(mfrow=c(1, 1))
plot(x, y, las=1)
lines(x=sort(x), y=fitted.values(mod1)[order(x)], col='blue', lwd=6)
lines(x=sort(x), y=fitted.values(mod2)[order(x)], col='red', lwd=2)
legend('topleft', legend=c('Ajuste polinomial', 
                           'Ajuste polinomial ortogonal'),
       lwd=c(6, 2), col=c('blue', 'red'))

# Estimando el valor de y para x = 20
c(1, 20, 400) %*% coef(mod1) # usando mod1
c(1, 20, 400) %*% coef(mod2) # usando mod2    Error!!!

new.data <- data.frame(x=20)
predict(mod2, new.data)      # Ahora si el valor correcto


x <- 1:5

X1 <- poly(x, degree=3, raw=TRUE)
X1
cor(X1)

X2 <- poly(x, degree=3, raw=FALSE)
X2
cor(X2)
