datos <- read.table('https://raw.githubusercontent.com/fhernanb/datos/master/lifespan', header = T, sep = '\t')
datos[datos == 0] <- NA


levels(datos$region)#para mirar los niveles de las regiones
datos11 <- subset(datos, year == 2000)#solo seleccionar los del año 2000
as.factor(datos$year)
datos1 <- na.omit(datos11)#eliminar los NA

with(datos1, symbols(x=income, y= life, circles = pop,
                     las=1, inches=.9, fg='darkblue', 
                     bg= 'lightblue',main='Radio = Tamaño poblacion'))
require(plotly)
plotly(data=datos1, x=~income, y=~life, color=~region, size=~pop)
require(gamlss)
## modelo de regresion
mod1 <- gamlss(data = datos1, life~income+pop, sigma.formula = ~income+pop,
               family = NO2)
summary(mod1)
mod2 <- gamlss(data = datos1, life~income, sigma.formula = ~income+pop,
               family = NO2)
summary(mod2)
mod3 <- gamlss(data = datos1, life~income, sigma.formula = ~income,
               family = NO2)
summary(mod3)
mod4 <- gamlss(data = datos1, life~income, family = NO2)
summary(mod4)

logLik(mod1)[2]
logLik(mod2)
logLik(mod3)
logLik(mod4)
help("GAIC")
GAIC(mod1,mod2,mod3,mod4, k=3)
#Mejor modelo mod4 porque tiene menor GAIC
-2*logLik(mod1)[1]+ 3*6
