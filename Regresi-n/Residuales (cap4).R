
a <- "https://raw.githubusercontent.com/fhernanb/datos/master/medidas_cuerpo2"
datos <- read.table(file=a, sep="\t", header=TRUE)
head(datos)
#---------------------------------------------------------------------------------
# This is a function to obtain a scatter plot with correlations
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste(prefix, txt, sep = "")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
#-------------------------------------------------------------------------------
# Creamos el grafico SOLO para las variables cuantitativas
pairs(datos[,c("Peso","Estatura","circun_cuello","circun_muneca")], 
      pch=19, las=1,
      upper.panel = panel.smooth, lower.panel = panel.cor)
#-------------------------------------------------------------------------------
# Ajustamos un modelo inicial con la funcion lm( )
mod1 <- lm(Peso ~ Estatura + circun_cuello + circun_muneca, data=datos)
# Para ver los resultados del modelo hacemos
summary(mod1)
# Modelo final luego de sacar las variables NO significativas
mod2 <- lm(Peso ~ circun_cuello, data=datos)
# Para ver los resultados del modelo final
summary(mod2)
#-------------------------------------------------------------------------------
# Grafico de dispersion con linea ajustada
# Primero convertimos las columnas de datos a variables para poderlas usar
attach(datos)
# Para construir el grafico de dispersion
plot(x=circun_cuello, y=Peso, pch=19, las=1,
     xlab="Circunferencia cuello (cm)", ylab="Peso (Kg)")
# Ahora agregamos la linea de tendencia
abline(mod2, lwd=3, col='blue2')
# por ultimo un texto con la ecuacion o modelo ajustado
text(x=33, y=95, expression(hat(Peso) == -44.61 + 3.10 * C.cuello), 
     cex=1.5, col='blue3' )
#-------------------------------------------------------------------------------
#------------ comprobacion de la adecuacion del modelo -------------------------
#-------------------------------------------------------------------------------

# Para obtener los valores hii de cada uno de los puntos usamos la funcion
# lm.influence y solicitamos el valor hat colocando al final $hat asi:
hii <- lm.influence(mod2)$hat
# para ver los valores hii hacemos
hii
# El valor hmax de referencia se obtiene como el maximo del vector hii
hmax <- max(hii)
# para ver hmax
hmax
# Para detectar la posicion del maximo de hii se usa la funcion which.max
which.max(hii)
# Para crear una tabla que muestre los regresores y su hii hacemos
cbind(Peso, Cuello=circun_cuello, hii=round(hii,2))
# La funcion round se usa para redondear con los decimales deseados
#-------------------------------------------------------------------------------
#----------------------     Analisis de residuos    ----------------------------
#-------------------------------------------------------------------------------
# Recordemos que en mod1 esta el modelo ajustado
# La desviacion estandar de los errores se extrae de mod1 asi:
sd_e <- summary(mod2)$sigma
sd_e
# Los residuales se pueden obtener asi:
ei <- residuals(mod2)
ei
# Los residuales estandarizados se obtienen asi:
di <- (ei-mean(ei)) / sd_e
di
# Los valores ajustados del modelo se obtienen asi:
y.ajus <- mod1$fitted.values
y.ajus
# Los residuales estudentizados
ri <- ei/sqrt(sd_e^2*(1-hii))
# Residual PRESS
PRESS <- ei/(1-hii)
# Para ver una tabla con los elementos anteriores ?tiles para el analisis
# usamos la fucion cbind, bind=pegar y la "c" quiere decir que vamos a
# pegar por columnas los vectores
cbind(Cuello=circun_cuello, Peso, 
      round(cbind(Y_ajust=y.ajus, ei, di, ri, PRESS), 
      digits=2))

#-------------- Grafico de normalidad de los errores ------------------------
# Ahora si el grafico
qqnorm(di, pch=19, main='Grafico de normalidad',
       xlab='Cuantiles teoricos', 
       ylab='Residuales estandarizados')
qqline(di, lty='longdash', col='blue2', lwd=2)

#----------------- Grafico de residuales vs valores ajustados ---------------
plot(x=y.ajus, y=di, pch=19, col='blue3', 
     main='Residuales estandarizados vs valores ajustados',
     ylab='Residuales estandarizados',
     xlab='Valores ajustados')

#-- Grafico de raiz cuadradada de residuales vs valores ajustados -----------
plot(x=y.ajus, y=sqrt(abs(di)), pch=19, col='blue3', 
     ylab=expression(sqrt(lRil)), 
     xlab='Valores ajustados')

#------------ Grafico de residuales estandarizados vs hii  ------- -----------
plot(x=hii, y=di, pch=19, col='blue', 
     ylab='Residuales estandarizados', 
     xlab=expression(h[ii]))

#-------------------- Graficos de residuales con R ---------------------------
# R nos entrega 4 graficos para analizar los residuales
# Vamos a dibujar los 4 graficos en la misma ventana grafica
# para esto vamos a dividirla en dos filas y dos columnas
par(mfrow=c(2, 2))
# Ahora solicitamos los 4 graficos
plot(mod2, pch=19)

# To obtain the second graph
par(mfrow=c(1,1))
plot(mod2, which=2, pch=19)

# To identify points with Cook's values > 4/(n-k-1) 
cutoff <- 4/((nrow(datos)-length(mod2$coefficients)-2)) 
plot(mod2, which=4, cook.levels=cutoff)
#-------------------------------------------------------------------------------
# Modelo final sin las observacion 11
mod3 <- lm(Peso ~ circun_cuello, data=datos[-11, ])
summary(mod3)

# Modelo final sin las observacion 8, 11, 12, 13
mod4 <- lm(Peso ~ circun_cuello, data=datos[-c(8,11,12,13), ])
summary(mod4)
#-------------------------------------------------------------------------------

par(mfrow=c(2, 2))
plot(mod4, pch=19)
require(model)
res()##todos los residuales