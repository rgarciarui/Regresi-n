### PUNTO 11, ANALISIS DE REGRECION TALLER CAPITULO MODELOS POLINOMIALES
require(MASS)
require(splines)
Cars93
# sacando las variables a usar
carros <- data.frame(MPGcity = Cars93$MPG.city, horsepower = Cars93$Horsepower,
                     length = Cars93$Length)
carros
require(scatterplot3d)
with(carros, scatterplot3d(horsepower, length, MPGcity))
######################################################################
##### punto 11.a
# modelo 1
mod1 <- lm(MPGcity ~ horsepower + length, data = carros)
summary(mod1)
# modelo 2 con polinomios ortogonales
mod2 <- lm(MPGcity ~ poly(horsepower, degree = 2, raw = FALSE) + poly(length, degree = 2, raw = FALSE), 
           data = carros)
summary(mod2)
# modelo 3 usando loess
mod3 <- loess(MPGcity ~ horsepower + length, data = carros)
summary(mod3)
####### punto 11.b
plot(mod1$fitted.values, carros$MPGcity, pch = 19)
##############################################################
####### correlaciones punto 11.d
#son correlaciones entre valores ajustados vs valores reales
cormod1 <- cor(mod1$fitted.values, carros$MPGcity)
cormod1
cormod2 <- cor(mod2$fitted.values, carros$MPGcity)
cormod2
cormod3 <- cor(mod3$fitted, carros$MPGcity)
cormod3
###############################################################
###### funcion para valor optimo punto 11.f
aux <- function(x){#funcion me entrega correlacion cuando le agrego un valor a span
  modelo <- loess(MPGcity ~ horsepower + length, span = x, data = carros)
  cor(carros$MPGcity, modelo$fitted)
}
aux <- Vectorize(aux) #debemos vectorizar la funcion
a <- seq(from=0.1, to=0.9, by = 0.0001) #rejilla de numeros de span
mejor <- aux(a) #vector de correlaciones
max(mejor)## el de mayor correlacion
which.max(mejor)# cual es el que tiene mayor correlacion = 0.9055106
a # en esta matriz buscar la posicion 399, ese es el span que es span = 0.1398
a[399]
######################################################################
####grafico punto 11.f
span <- seq(from = 0.1, to = 0.9, by = 0.01)
plot(x = span, y = aux(span), las=1, pch = 20, ylab = "correlacion")
optimize(aux, span, lower = min(span), upper = max(span),
         maximum = TRUE)
### optimizar span y degree punto 11.j CON FUNCION OPTIMIZE
# buscando la mejor combinacion con funcion OPTIMIZE
aux1 <- function(x) { #con degree = 1
  modelo1 <- loess(MPGcity ~ horsepower + length, span = x, degree = 1, data = carros)
  cor(carros$MPGcity, modelo1$fitted)
}
aux1 <- Vectorize(aux1)
span <- seq(from = 0.1, to = 0.9, by = 0.01)
de1 <- optimize(aux1, span, lower = min(span), upper = max(span), maximum = TRUE)

aux2 <- function(x) { #con degree = 2
  modelo2 <- loess(MPGcity ~ horsepower + length, span = x, degree = 2, data = carros)
  cor(carros$MPGcity, modelo2$fitted)
}
aux2 <- Vectorize(aux2)
de2 <- optimize(aux2, span, lower = min(span), upper = max(span), maximum = TRUE)

### resultados de las dos funciones
data.frame(degree1 = c(de1$maximum, de1$objective), degree2 = c(de2$maximum, de2$objective))
# ese dataframe nos dice que la mejor combinacion es un degree = 1, span = 0.1105937
# para una correlacion de 0.9231195
############################################################################

# hacerlo con funcion outer
formaouter <- function(span, degree){
  modloess <- loess(MPGcity ~ horsepower + length, span = span, degree = degree, data = carros)
  cor(carros$MPGcity, modloess$fitted)
}
formaouter <- Vectorize(formaouter)
span <- seq(from = 0.1, to = 0.99, by = 0.01)
degree <- c(1,2)
mejor <- outer(span, degree, formaouter)
max(mejor) # cual es la mayor correlacion?
which.max(mejor) # posicion 1, donde esta la mayor correlacion
mejor # matrix donde estan todas las correlaciones la maxima es la primera o sea span = 0.10 y degree = 1
span # el primer termino de este vector es el mejor span




####para la optimizacion del df
opdf <- function(df1, df2){
  mode1 <- lm(carros$MPGcity ~ bs(carros$horsepower, df=df1)+
                   bs(carros$length, df=df2))
  cor(carros$MPGcity, mode1$fitted.values)
}
opdf <- Vectorize(opdf)
df1 <- seq(from = 3, to = 10, by = 1)
df2 <- seq(from = 3, to = 10, by = 1)
mejordf <- outer(df1, df2, opdf)

max(mejordf) # cual es la mayor correlacion?
which.max(mejordf) # posicion 1, donde esta la mayor correlacion
mejordf # matrix donde estan todas las correlaciones la maxima es la primera o sea span = 0.10 y degree = 1
mejordf[64] # el primer termino de este vector es el mejor span
m
which.max(opdf(0.8803619))
