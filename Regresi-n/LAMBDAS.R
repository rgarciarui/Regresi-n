url<- 'https://raw.githubusercontent.com/fhernanb/datos/master/aptos2015'
datos<- read.table(url, header = TRUE)
datos
summary(datos)
poblado<- subset(datos, ubicacion=="poblado")
View(poblado)
attach(poblado)
plot(x=poblado$precio, y=poblado$mt2, type='p')
mod0<- lm(precio~ mt2, data=poblado)
summary(mod0)
m5 <- fitDist(precio, data=datos, type="realline")
#############################
#LOG VEROSIMILITUD DELMODELO 0
logLik(mod0)
#############################
#MEJOR LAMBDA
lambda<--2
yt<- precio^lambda
mod<- lm(yt~mt2)
logLik(mod)

lambda<--1
yt<- precio^lambda
mod<- lm(yt~mt2)
logLik(mod)

lambda<-0
yt<- log(precio)
mod<- lm(yt~mt2)
logLik(mod)

lambda<-1
yt<- precio^lambda
mod<- lm(yt~mt2)
logLik(mod)

lambda<-2
yt<- precio^lambda
mod<- lm(yt~mt2)
logLik(mod)

#############################################################################
#############################################################################
######EJEMPLO DE LOS LAMBDAS
x1 <- c(35, 35, 35, 35, 35, 35, 35, 35, 55, 55, 55, 55, 55, 55, 55, 55, 25, 
        65, 45, 45, 45, 45, 45, 45, 45, 45)
x2 <- c(0.3, 0.3, 0.3, 0.3, 0.7, 0.7, 0.7, 0.7, 0.3, 0.3, 0.3, 0.3, 0.7, 0.7, 
        0.7, 0.7, 0.5, 0.5, 0.1, 0.9, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
y1 <- c(76.5, 76, 79.9, 83.5, 89.5, 84.2, 85.7, 99.5, 89.4, 97.5, 103.2, 108.7, 
        115.2, 111.5, 102.3, 108.1, 80.2, 89.1, 77.2, 85.1, 71.5, 84.5, 77.5, 79.2, 
        71, 90.2)


nueva_box_cox <- function(modelo, lambda = seq(-5, 5, 1/10)) {
  x.reg <- as.matrix(modelo$model[, 2:ncol(modelo$model)])
  w <- exp(sum(log(modelo$model[, 1]))/length(modelo$model[, 1]))
  ssr <- rep(NA, length(lambda))
  for (i in 1:length(lambda)) {
    if (lambda[i] == 0) {
      yl <- w * log(modelo$model[, 1])
      modl <- lm(yl ~ x.reg)
      ssr[i] <- sum(modl$residuals^2)
    } else {
      yl <- ((modelo$model[, 1]^lambda[i]) - 1)/(lambda[i] * w^(lambda[i] - 
                                                                  1))
      modl <- lm(yl ~ x.reg)
      ssr[i] <- sum(modl$residuals^2)
    }
  }
  lam_op <- lambda[ssr == min(ssr)]
  plot(lambda, ssr, type = "l", xlab = expression(lambda), ylab = "Suma de cuadrados del error")
  abline(v = lam_op, lty = 2, col = 2)
  legend("top", bty = "n", legend = substitute("lambda" == lam_op))
}
par(mfrow = c(1, 2))
library(MASS)
nueva_box_cox(lm(y1 ~ x1 + x2))
boxcox(lm(y1 ~ x1 + x2), lambda = seq(-2, 2, 1/10), plotit = TRUE)

bv = boxcox(lm(y1 ~ x1 + x2))
lambda = bv$x[which.max(bv$y)]
