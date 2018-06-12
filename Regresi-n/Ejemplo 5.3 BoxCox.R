#----------------------------------------------------------------------
#----------------------------------------------------------------------
#--------------------------- Transformacion Box-Cox -------------------
#------------------- para el ejemplo 5.3 de Montgomery et al.----------
#----------------------------------------------------------------------
# Para cargar y disponer de los datos usamos
require(gapminder)
gapminder
head(datos)
attach(datos)
which(gapminder,plot(x=gdpPercap,y=lifeExp))
#----------------------------------------------------------------------
# Encontrando el lambda manualmente, usando la definicion
#----------------------------------------------------------------------
# La siguiente funcion transforma la y para un valor de lambda 
transf <- function(y, lambda) {
  y.dot <- exp(mean(log(y)))
  if (lambda!=0) y.transf <- (y^lambda-1)/(lambda*y.dot^(lambda-1))
  else y.transf <- y.dot * log(y)
  y.transf
}

# Hagamos una pruebas
transf(kWh, 2)
transf(kWh, 0)
transf(kWh, -1)

# Esta funcion recibe un lambda y entrega el valor de loglik
BoxCox <- function(lambda) {
  mod <- lm(transf(kW, lambda) ~ kWh)
  logLik(mod)
}

lambdas <- seq(-2, 2, by=0.01)
ll <- NULL
for (i in 1:length(lambdas)) ll[i] <- BoxCox(lambdas[i])
plot(x=lambdas, y=ll, type='b')
lambdas[which.max(ll)]

#----------------------------------------------------------------------
# Ajustando el modelo sin transformar
#----------------------------------------------------------------------

# Diagrama de dispersion
plot(x=kW, y=kWh, pch=19)
# Ajustamos el modelo 1
mod1 <- lm(kWh ~ kW)
# Tabla de resultados
summary(mod1)

#----------------------------------------------------------------------
# Encontrando el lambda con la funcion boxcox
#----------------------------------------------------------------------
# Cargamos la libreria MASS para poder usar la funcion boxcox
library(MASS)
library(model)
require(model)
# Usando la funcion boxcox() sobre el modelo ajustado tenemos:
bc <- boxcox(mod1)
# Para encontrar el valor de lambda que maximiza log-likelihood hacemos
lambda <- bc$x[which.max(bc$y)]
lambda

#----------------------------------------------------------------------
# Ajustando el modelo transformado
#----------------------------------------------------------------------
y.transf <- kWh ^ lambda
plot(x=kW, y=y.transf, pch=19)
mod2 <- lm(y.transf ~ kW)
summary(mod2)

