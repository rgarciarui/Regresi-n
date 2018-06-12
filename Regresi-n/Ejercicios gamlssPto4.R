###Crear una funcion con respuesta data.frame
df.gen <- function(n){
  x1 <- runif(n=n, 0,1)
  x2 <- rbinom(n=n, size = 5, prob = 0.1)
  lamb <- -2 + 2.5*x1 + 1.5*x2
  y <- rpois(n=n, lambda = exp(lamb))
  return(data.frame(y, x1, x2))
}
###Simular 10000 datos
daticos <- df.gen(n=10000)
###Vector real de parametros
tetha <- c(-2, 2.5, 1.5)
tetha
###Modelo de regresión
ajuste <- gamlss(y ~ x1 + x2, family= PO(mu.link = "log"), data=daticos)
###Vector de parametros estimado
coef(ajuste)
