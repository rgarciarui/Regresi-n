datos <-read.table(file='http://tinyurl.com/hwhb769', header=T)
attach(datos)
datos1 <- datos[-594, ]
##DIAGRAMA DE DISPERSION EN 3D CON SCATTERPLOT3D
library(scatterplot3d)
scatterplot3d(x=mt2,y=alcobas,z=precio,pch=16,cex.lab=1.5,highlight.3d=TRUE,type="h")
##DIAGRAMA DE DISPERSION CON RGL
library(rgl)
plot3d(x=mt2,y=alcobas,z=precio,
       type='s', col='lightblue',
       xlab='Cantidad',
       ylab='Distancia (pies)',
       zlab='Tiempo (min)', main='Muevaconelmouselafigura')

require(gamlss)
mod1 <- gamlss(data= datos1, precio ~ mt2+alcobas, sigma.formula = ~mt2+alcobas, 
               family = NO2)
summary(mod1)
#mu=39.227+3.07353MT2-30.85291ALCOBAS
#sigma=exp(3.1768+0.00968MT2)
mod2 <- gamlss(data= datos1, precio ~ mt2+alcobas, sigma.formula = ~mt2, 
               family = NO2)
summary(mod2)
