pH <- c(3,3,11,11,3,3,11,11,7,7,7,7,7,7,7)
NM <- c(20,20,20,20,10,30,10,30,10,30,10,30,20,20,20)
Cat <- c(0.1,1,0.1,1,0.55,0.55,0.55,0.55,0.1,0.1,1,1,0.55,0.55,0.55)
Degradacion <- c(21,63.6,0,28.67,26.74,19.4,0.62,2.71,24.21,11.32,82.38,48.01,45.91,50.91,39.06)
base <- data.frame(Degradacion,pH,NM,Cat)

mod1 <- lm(Degradacion~pH+Cat+NM+I(pH^2)+I(Cat^2)+I(NM^2)+pH*Cat+pH*NM+Cat*NM)
summary(mod1)

mod2 <- lm(Degradacion~pH+Cat+NM+I(pH^2)+I(Cat^2)+I(NM^2))
summary(mod2)

mod3 <- lm(Degradacion~pH+Cat+NM+I(pH^2)+I(NM^2)) 
summary(mod3)
coef(mod3)
y1 <- coef(mod3)[1]+coef(mod3)[2]*4+coef(mod3)[3]*0.6+
  coef(mod3)[4]*10+coef(mod3)[5]*16+coef(mod3)[6]*100

y1
newdata <- data.frame(pH,NM,Cat)
ypredi <- predict(mod3,newdata )
cor(Degradacion,ypredi)
newdata1 <- data.frame(pH=c(4,5),NM=c(10,30),Cat=c(0.6,1)) 
predict(mod3,newdata =newdata1 )

min(pH)

minus_rend <- function(x) {
  Cat <- x[1]
  pH <- x[2]
  NM <- x[3]
  new.data <- data.frame(Cat=c(1, Cat), pH=c(1, pH),NM=c(1,NM))
  -predict(mod3, new.data)[2]
}
min()
inicio <- c(0, 0,0)  # valores iniciales para la busqueda
res <- nlminb(start=inicio, objective=minus_rend,
              lower=c(0.1, 3,10), # minimos de las variables
              upper=c(1, 11,30), # maximos de las variables
              control=list(trace=1))


res # Para ver todo el contenido de res
res$par  # Valores optimos
-res$objective  # Valor del objetivo

mod4 <- loess(Degradacion~pH+Cat+NM,span = 0.75,degree = 1)
newdata <- data.frame(pH,NM,Cat)
ypredi2 <- predict(mod4,newdata )
cor(ypredi2,Degradacion)
library(splines)
mod5 <- lm(Degradacion~ns(pH,df=2)+ns(Cat,df=2)+ns(NM,df=2))
ypredi3 <- predict(mod5,newdata )
cor(ypredi3,Degradacion)
newdata2 <- data.frame(pH=c(4,5),NM=c(10,30),Cat=c(0.6,1))
predict(mod5,newdata2)

mse1 <- round(mean((Degradacion - ypredi)^2), digits=2)
mse1

mse2 <- round(mean((Degradacion - ypredi2)^2), digits=2)
mse2

mse3 <- round(mean((Degradacion - ypredi3)^2), digits=2)
mse3

require(MASS)
help(Cars93)
datos <- Cars93
class(datos$Man.trans.avail)
trasmicion <- relevel(datos$Man.trans.avail,ref = "Yes")
mod6 <- lm(datos$MPG.city~ datos$Weight+trasmicion+datos$Weight*trasmicion)
summary(mod6)
coef(mod6)
coef(mod6)[1]+coef(mod6)[2]*3500+coef(mod6)[3]+coef(mod6)[4]*3500

