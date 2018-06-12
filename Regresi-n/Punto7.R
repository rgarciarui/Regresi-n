##--Primer punto
n <- 500
sigma <- 9
x <- rpois(n=n, lambda = 5)
mu <- 36 - 12 *x + I(x^2)
y <- rnorm(n=n, mean = mu, sd = sigma)
plot(x, y)
##--Segundo punto
mod <- lm(y ~ x)
summary(mod)
##--Tercer punto (graficos de residuales)
par(mfrow=c(1,3)) #Para dividir la ventana grafica
plot(mod, which=1:3) #Para dibujar los tres primeros
###---Incluir una potencia en el modelo
mod2<-lm(y~x+I(x^2)) #para incluir una potencia de una variable se usa la función I()
plot(mod2,which=1:3)
plot(x, y)

