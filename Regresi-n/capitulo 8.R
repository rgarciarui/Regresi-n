require(MASS)
datos <- Cars93
attach(datos)
origin <- relevel(datos$Origin,ref= "USA")
airbags <- relevel(datos$AirBags,ref="None")
par(mfrow=c(1,2))

boxplot(Price ~ Origin, data= datos)
boxplot(Price~AirBags, data= datos)

mod <- lm(Price~ origin+airbags+MPG.city)
summary(mod)

qt(0.025,df=26)


###Punto 4
individuo <- (1:20)
peso <- c(55,91.7, 69.7,56.8,66.3,59.5,71.9,97.6,58,54.3,82,75.9,64,73,70,78,84.2,98.6,60.5,70.9)
altura <- c(1.6,1.79,1.67,1.64,1.66,1.70,1.69,1.83,1.63,1.59,1.74,1.68,1.70,1.78,1.74,1.70,1.77,1.83,1.66,1.66)
sexo <- c("m","h","h","m","h","m","h","h","m","m","h","h","m","m","m","h","h","h","m","h")
datos <- data.frame(individuo,peso,altura,sexo)

## a)
dat.h <- subset(datos, sexo== "h")
dat.m <- subset(datos, sexo =="m")
with(dat.h, plot(altura,peso, pch=3, ylim=c(50,100), xlim = c(1.55,1.85)))
with(dat.m, points(altura,peso,ylim=c(50,100), xlim = c(1.55,1.85)))

## b)
sexod <- relevel(datos$sexo, ref = "m")
mod <- lm(peso~ altura+ sexod + altura*sexod)
mod1 <- lm(peso~ altura,data = dat.h )
abline(mod)
abline(mod1)
summary(mod)

## c) ?????? 
dir()
