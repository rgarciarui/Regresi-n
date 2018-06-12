require(rgl)
base <- read.delim(choose.files(),header = T)
attach(base)
mod1 <- lm(esfu ~ hume + poro)
summary(mod1)
plot(mod1)
plot.ts(residuals(mod1),ylim=c(min(residuals(mod1),-2*summary(mod1)$sigma),
                               max(residuals(mod1),2*summary(mod1)$sigma)),
        main="Residuos vs. t\nModelo Log polinomial grado cuatro con indicadoras")
abline(h=0,col=2)

plot(fitted(mod1),residuals(mod1),ylim=c(min(residuals(mod1),-2*summary(mod1)$sigma),
                                         max(residuals(mod1),2*summary(mod1)$sigma)),
     main="Residuos vs. ajustados\nModelo exponencial cuadr?atico estacional")
abline(h=0,col=2)
abline(h=c(-2*summary(mod1)$sigma,2*summary(mod1)$sigma),col=2)
hist(esfu)
plot(density(esfu))

Humedad <- seq(min(hume),max(hume),length.out = 30)
Porosidad <- seq(min(poro),max(poro),length.out=30)
Rend <- function(hume, poro) {
  res <- coef(mod1) * c(1, hume, poro)
  sum(res)
}
Rend <- Vectorize(Rend)
Esfuerzo <- outer(Humedad, Porosidad, Rend)
persp(x=Humedad, y=Porosidad, z=Esfuerzo,
      theta=40, phi=30, ticktype = "detailed", col='salmon1')

mod2 <- loess(esfu ~ hume + poro , data= base, 
              degree = 1,span = 0.5)

hum <- with(base, seq(min(hume), max(hume), len=10))
poros <- with(base, seq(min(poro), max(poro), len=10))
newdata <- expand.grid(hume=hum, poro=poros)
fit.esfu <- matrix(predict(mod2, newdata), 10, 10)

persp(x=hum, y=poros, z=fit.esfu, 
      theta=45, phi=35, ticktype="detailed", 
      xlab="Humedad", ylab="Porosidad", zlab="Esfuerzo",
      shade=0.2, col="lightblue", expand=0.7)
newdata <- data.frame(hume=0.4, poro=1.0)
predict(mod2, newdata)

names(mod1)
names(mod2)

x1 <- base$esfu
x2 <- fitted.values(mod1)
cor(x1,x2)

y1 <- fitted(mod2)
cor(x1,y1)

#PUNTO 3

t <- c(1,1,1,1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1)
c <- c(1,1,-1,-1,1,1,-1,-1,1,1,-1,-1,1,1,-1,-1)
pH <- c(1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1)
Cr3 <- c(75.2,11.4,62.7,82.9,99.5,2.5,62.3,73.4,74.8,5.4,56.4,83.5,99.5,17.3,68.9,74.2)
Cr6 <- c(26.1,16.2,81.3,67.3,7.7,11.7,84.6,65.1,21.6,27,80.9,65.1,13.1,15.8,83.7,66.1)
average <- c(75,8.4,59.6,83.2,99.5,9.9,65.6,73.8,23.9,21.6,81.1,66.2,10.4,13.7,84.1,65.6)
datos <- data.frame(datos,average)
write.csv(datos,file='base.csv')
mod3 <- lm(Cr3~t+c+pH+t*c+t*pH+c*pH,data = datos)
summary(mod3)
