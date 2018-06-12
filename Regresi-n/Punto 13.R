require(MASS)
# sacando las variables a usar
carros <- data.frame(MPGcity = Cars93$MPG.city, horsepower = Cars93$Horsepower,
                     length = Cars93$Length)
carros
require(splines)
## SOLUCION LITERAL B)
# para modelo con splines basicos bs()
aux <- function(df1, df2){ 
  modelobs <- lm(MPGcity ~ bs(horsepower, df = df1) + bs(length, df = df2), data = carros)
  cor(carros$MPGcity, modelobs$fitted.values)
}
aux <- Vectorize(aux)
df1 <- seq(3,10, 1) # valores de df(en bs empieza desde 3 y va hasta donde sea en este 10)
df2 <- seq(3,10,1) 
corbs <- outer(df1, df2, aux) # matriz de correlaciones con df1 y df2
max(corbs) # maxima correlacion 0.8803619
which.max(corbs) # combinacion df ubicacion: 64 o sea df1 = 10, df2 = 10

# para modelo con splines naturales ns()
aux1 <- function(df1, df2){
  modelons <- lm(MPGcity ~ ns(horsepower, df = df1) + ns(length, df = df2), data = carros)
  cor(carros$MPGcity, modelons$fitted.values)
}
aux1 <- Vectorize(aux1)
df1 <- seq(1, 10, 1)
df2 <- seq(1, 10, 1)
corns <- outer(df1, df2, aux1)
max(corns) # maxima correlacion 0.8688876
which.max(corns) # combinacion df ubicacion: 78, o sea df1 = 8, df2 = 8

## LITERAL B MINIMIZAR SSE
# Para modelo con splines basicos bs()
calcSSE <- function(df1, df2){
  mod <- lm(MPGcity ~ bs(horsepower, df = df1) + bs(length, df = df2), data = carros)
  res <- mod$residuals
  if((sum(res, na.rm = T) > 0)){
    sse <- sum(res^2)
  } else {
    sse <- 99999
  }
  return(sse)
}
calcSSE <- Vectorize(calcSSE)
df1 <- seq(3,10,1)
df2 <- seq(3,10,1)
SSE <- outer(df1, df2, calcSSE)
min(SSE) # minimo SSE = 653.6453
which.min(SSE) # ubicacion 64, o sea df1 = 8, df2 = 8
SSE # matriz donde estan todos los SSE y donde buscamos el menor 

#para modelo con splines naturales bn()
#?
#
#