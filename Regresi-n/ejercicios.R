
######SEGUNDO PUNTO ARTICULO DE ROCAS
datos  <-read.table(file="https://raw.githubusercontent.com/fhernanb/datos/master/rocas", header=T)
require(rgl)
attach(datos)
plot3d(x=hume, y=esfu, z=poro, lwd=2, col='pink',
       xlab='Humedad', ylab='RCU', type='s',
       zlab='Porosidad')
mod1 <- lm(esfu ~ hume + poro, data = datos)
summary(mod1)

##Superficie de respuesta
Humedad   <- seq(from=min(datos$hume), to=max(datos$hume), length.out=30)
Porosidad <- seq(from=min(datos$poro), to=max(datos$poro), length.out=30)
Rend <- function(hume, poro) {
  res <- coef(mod1) * c(1, hume, poro)
  sum(res)
}
Rend <- Vectorize(Rend)
Esfuerzo <- outer(Humedad, Porosidad, Rend)
persp(x=Humedad, y=Porosidad, z=Esfuerzo,
      theta=40, phi=30, ticktype = "detailed", col='salmon1')

mod2 <- loess(esfu ~ hume + poro + hume*poro, data= datos, 
              degree = 1,span = 0.75)
summary(mod2)

hum <- with(datos, seq(min(hume), max(hume), len=10))
poros <- with(datos, seq(min(poro), max(poro), len=10))
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

x1 <- datos$esfu
x2 <- fitted.values(mod1)
cor(x1,x2)

y1 <- fitted(mod2)
cor(x1,y1)

## punto 3



#####PUNTO 4 SIMULACION
n <- 500
sig <- 3
x <- rpois(n=n, lambda = 5)
mu <- 36 - 12 *x + x^2
y <- rnorm(n=n, mean= mu, sd = sig)
plot(x,y)

mod1 <- lm(y~x)
summary(mod1)
par(mfrow=c(1,3))
plot(mod1, which = 1:3)

mod2 <- lm(y ~ x + I(x^2))
par(mfrow=c(1,3))
plot(mod2, which = 1:3)

plot(x,y)
fun <- function(x) sum(coef(mod2) * c(1, x, x^2))
fun <- Vectorize(fun)
curve(fun, from=0, to=15, col='blue', lwd=3, add=T)


#####PUNTO 5 REPLICAR EJEMPLO DE OPTIMIZACION DE SUPERFICIE DE RESPUESTA
f <- function(x,y) 2*x*(y**2)+2*(x**2)*y+x*y
x<- seq(-0.5,0.5, len=200)
y<- seq(-0.5,0.5, len=200)
z <- outer(x,y,f)

persp(x,y,z, theta=-30,phi=15,ticktype="detailed")

image(x,y,z) ## para ver los optimizadores

#derivada parcial con respecto a x y y
fx <- function(x,y,h=0.001) (f(x+h,y)-f(x,y))/h
fy <- function(x,y,h=0.001) (f(x,y+h)-f(x,y))/h

zfx <- outer(x,y,fx)
zfy <- outer(x,y,fy)

contour(x,y,zfx,level=0)
contour(x,y,zfy,level=0, add=T, col= "red")

x <- seq(-0.2,0,len=400)
y <- seq(-0.2,0,len=400)
z<- outer(x,y,f)

image(x,y,z)
contour(x,y,z,add=T)

##hallar los maximos de forma algebraica
fbb<-function(x) f(x[1],x[2]) #transforma de bivariada a univariada
optim(c(0.5,0.5), fbb ,control=list(fnscale=-1))  #????

fxb <- function(x) fx(x[1],x[2])
fyb <- function(x) fy(x[1],x[2])
sumssq <- function(x) fxb(x)**2+fyb(x)**2

optim(c(0.1,0.1),sumssq)


#####PUNTO 6
temp <- c(200, 250, 200, 250, 189.65, 260.35, 
          225, 225, 225, 225, 225, 225)

conc <- c(15, 15, 25, 25, 20, 20, 12.93, 27.07,
          20, 20, 20, 20)

rend <- c(43, 78, 69, 73, 48, 76, 65, 74, 76, 79, 83, 81)
datos <- data.frame(temp, conc, rend)
mod <- lm(rend ~ temp + conc + I(temp^2) + I(conc^2) + temp * conc)
minus_rend <- function(x) {
  temp <- x[1]
  conc <- x[2]
  new.data <- data.frame(temp=c(1, temp), conc=c(1, conc))
  -predict(mod, new.data)[2]
}

inicio <- c(192, 15)  # valores iniciales para la busqueda
names(inicio) <- c('Temperatura', 'Concentracion') # Colocando nombres

res <- nlminb(start=inicio, objective=minus_rend,
              lower=c(189.65, 12.93), # minimos de las variables
              upper=c(260.35, 27.07), # maximos de las variables
              control=list(trace=1))


res # Para ver todo el contenido de res
res$par  # Valores optimos
-res$objective  # Valor del objetivo
temp <- x1
conc <- x2
rend <- y

minusll <- function(theta, x2, x1,y) { 

  media <- theta[1] + theta[2] *x1+theta[3]*x2+theta[4]*I(x1^2)+
    theta[5]*I(x2^2)+theta[6]*x1*x2
  # Se define la media 
  desvi <- theta[7] # Se define la desviación. 
  -sum(dnorm(x=y, mean=media, sd=desvi, log=TRUE))
}

res1 <- optim(par=c(0, 0, 0,0,0,0,0),
              fn=minusll, method='L-BFGS-B', 
              lower=c(-Inf, -Inf,-Inf,-Inf,-Inf,-Inf,0 ),
              upper=c(Inf, Inf, Inf,Inf,Inf,Inf,Inf), y=y, x1=x1,x2=x2)


#####PUNTO 7
load(url('https://www.dropbox.com/s/ud32tbptyvjsnp4/data.R?dl=1'))
lw1 <- loess(y ~ x,data=data)
plot(y ~ x, data=data,pch=19,cex=0.1)
lines(data$y,lw1$fitted,col="blue",lwd=3)


lw1 <- loess(y ~ x,data=data)
plot(y ~ x, data=data,pch=19,cex=0.1)
j <- order(data$x)
lines(data$x[j],lw1$fitted[j],col="red",lwd=3 )

##### PUNTO 8
help("loess")

#####PUNTO 11
require(MASS)
datos <- Cars93
attach(datos)
## a)
mod1 <- lm(MPG.city ~ Horsepower+Length)
mod2 <- lm(MPG.city ~ poly(Horsepower, 
                           degree = 2, raw = FALSE)+ poly(Length, degree = 2, 
                                                          raw = FALSE))
mod3 <- loess(MPG.city~ Horsepower+Length, data= datos)
## b)
par(mfrow= c(1,3))
plot(fitted.values(mod1), MPG.city, main = "Modelo 1", pch=10)
plot(fitted.values(mod2), MPG.city, main = "Modelo 2",pch=10)
plot(fitted.values(mod3), MPG.city, main = "Modelo 3",pch=10)
## d)
cor(MPG.city, fitted.values(mod1))
cor(MPG.city, fitted.values(mod2))
cor(MPG.city, fitted.values(mod3))

span  <-  seq(from=0.1,  to=0.9,  by=0.01)  
plot(x=span,  y=aux(span),  las=1,  pch=20,  ylab='Correlacion') 
##f)

aux <- function(x){
  
    modelo <- loess(MPG.city~ Horsepower+Length, data= datos,span = x,degree = i)
    predicciones <- predict(modelo,datos[,c(13,19)])
    cor(MPG.city,predicciones)
    
}

aux <- Vectorize(aux)
span <- seq(from=0.1, to=0.9, by=0.01)
plot(x=span, y=aux(span), las=1, pch=20, ylab='Correlacion')
optimize(aux,c(min(span),max(span)), maximum=TRUE)  


aux(0.1954733,2)

#####PUNTO 12
calcSSE <- function(x){
  loessMod <- try(loess(MPG.city ~ Horsepower+ Length, data=datos, 
                        span=x, degree = 2), silent=T)
  res <- try(loessMod$residuals, silent=T)
  if(class(res)!="try-error"){
    if((sum(res, na.rm=T) > 0)){
      sse <- sum(res^2)  
    }
  }else{
    sse <- 99999
  }
  return(sse)
}
optim(par=c(0.5), calcSSE, method="SANN")

##Punto 12
splin <- function(x){
modelo2 <- lm(MPG.city ~ bs(Horsepower,df=7)+bs(Length,df=x))
predicciones <- predict(modelo2,datos[,c(13,19)])
cor(MPG.city,predicciones)
}
splin <- Vectorize(splin)
spl <- seq(from=3, to=7, by=1)
plot(x=spl, y=splin(spl), las=1, pch=20, ylab='Correlacion')
optimize(splin,c(3,7), maximum=TRUE)  


