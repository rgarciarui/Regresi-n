# Los datos estan disponibles en el objeto softdrink del paquete MPV
require(MPV)
softdrink
datos <- softdrink
colnames(datos) <- c('tiempo', 'cantidad', 'distancia')
attach(datos)

# -------------------------------------------------------------------------
# Diagramas de dispersion en 2d -------------------------------------------
# -------------------------------------------------------------------------
pairs(datos)

# Mejorando el grafico
panel.reg <- function (x, y)
{
  points(x, y, pch=20)
  abline(lm(y ~ x), lwd=2, col='dodgerblue2')
}
# Funci?n para crear el histograma
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="dodgerblue2", ...)
}
# Funcion para obtener la correlacion
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * abs(r))
}

pairs(datos,
      upper.panel = panel.reg,
      diag.panel = panel.hist,
      lower.panel = panel.cor)

# -------------------------------------------------------------------------
# Diagramas de dispersion en 3d -------------------------------------------
# -------------------------------------------------------------------------

# FORMA 1 con el paquete scatterplot3d
library(scatterplot3d)
scatterplot3d(x=cantidad, y=distancia, z=tiempo, pch=16, cex.lab=1,
              highlight.3d=TRUE, type="h",
              xlab='Cantidad de cajas',
              ylab='Distancia (pies)',
              zlab='Tiempo (min)')

# FORMA 2 con el paquete rgl
library(rgl)
plot3d(x=cantidad, y=distancia, z=tiempo, type='s', col='pink',
       xlab='Cantidad',
       ylab='Distancia (pies)',
       zlab='Tiempo (min)')

# -------------------------------------------------------------------------
# Ajustando el modelo y agregando el plano --------------------------------
# -------------------------------------------------------------------------

# Ajustando el modelo de regresion
mod <- lm(tiempo ~ cantidad + distancia)
summary(mod)

# Vamos a agregar el plano de regresion al diagrama de la forma 1
# Para repetir el diagrama forma 1
mi_grafico_3d <- scatterplot3d(x=cantidad, y=distancia, z=tiempo, 
                               pch=16, cex.lab=1.5,highlight.3d=TRUE, 
                               type="h", main='Diagrama en 3d')
# Para agregar el plano usamos la funcion s3d$plane3d( )
# con argumento modelo ajustado. El resto de argumentos son opcionales
mi_grafico_3d$plane3d(mod, lty.box="solid", col='blue3')

# -------------------------------------------------------------------------
# Intervalos de confianza para betas --------------------------------------
# -------------------------------------------------------------------------

# Como obtener los IC en un modelo de regresion lineal multiple?
# Primero necesitamos los nombres de los coeficientes que 
# podemos extraer as:
betas <- names(coef(mod))
# Para ver los nombres que estan en el vector betas
betas

# Ahora usamos la funcion confint(object, parm, level = 0.95)
confint(object=mod, parm=betas, level=0.95)

# Es posible obtener IC con niveles de confianza diferentes
# modificando el parametro level
confint(object=mod, parm=betas, level=0.97)

# -------------------------------------------------------------------------
# Obtencion de la matriz H ------------------------------------------------
# -------------------------------------------------------------------------

# Para obtener los valores hii de cada uno de los puntos usamos la funcion
# lm.influence y solicitamos el valor hat colocando al final $hat asi:
hii <- lm.influence(mod)$hat

# para ver los valores hii hacemos
hii

# El valor hmax de referencia se obtiene como el maximo del vector hii
hmax <- max(hii)

# para ver hmax
hmax

# Para detectar la posicion del m?ximo de hii se usa la funcion which.max
which.max(hii)

# Para crear una tabla que muestre los regresores y su hii hacemos
cbind(cantidad, distancia, hii=round(hii, 2))
# La funcion round se usa para redondear con los decimales deseados

# Como obtener la matriz X?
# Se puede usar la funcion model.matrix aplicada al modelo ajustado
X <- model.matrix(mod)
X

# Tambien se puede obtener asi:
X <- model.matrix( ~ cantidad + distancia)
X

# Como se calcula el hoo para una nueva observacion?
# Supongamos que la nueva observacion es
x0 <- c(1, 20, 150)
h00 <- x0 %*% solve(t(X)%*%X) %*% x0
h00

# Cual es el centroide de los datos?
centro <- c(mean(cantidad), mean(distancia))
centro

# Donde se encuentra el centroide?
plot(cantidad, distancia, pch=19, cex.lab=1.5)
points(x=mean(cantidad), y=mean(distancia), pch=20, cex=3, col='blue')

# Donde se encuentra la nueva observacion?
points(x=20, y=150, pch=20, cex=3, col='red')

# Donde se encuentra la observacion con hmax?
points(x=cantidad[9], y=distancia[9], pch=20, cex=3, col='purple')


# -------------------------------------------------------------------------
# Incluyendo interaccion --------------------------------------------------
# -------------------------------------------------------------------------
modint <- lm(tiempo ~ cantidad * distancia)
summary(modint)

fun <- function(x1, x2) sum(coef(modint) * c(1, x1, x2 + x1 * x2))
fun <- Vectorize(fun)
x1 <- seq(from=2, to=30, length.out=10)
x2 <- seq(from=36, to=1460, length.out=10)
y <- outer(x1, x2, fun)
y
# Superficie en 3d
persp(x1, x2, y, theta=30, phi=30,
      ticktype = "detailed", nticks=3,
      col='lightblue', border='blue',
      xlab='Cantidad', ylab='Distancia (pies)',
      zlab='Tiempo (min)')
#prueba formal para la falta de ajuste
#Ho: E(y)=Bo+...+BjXj vs 
require(model)
lack.fit(modelo)#falta de ajuste


#anova

miAnova=function(modeloreg){
  SSq=unlist(anova(modeloreg)["Sum Sq"])
  DFq=unlist(anova(modeloreg)["Df"]) 
  k=length(SSq)-1 
  SSR=sum(SSq[1:k])
  SSE=SSq[(k+1)] 
  DF.SSR=sum(DFq[1:k]) 
  DF.SSE=DFq[(k+1)] 
  MSR=SSR/DF.SSR 
  MSE=SSE/DF.SSE 
  F0=MSR/MSE 
  VP=pf(F0,k,DF.SSE,lower.tail=F)
  result=data.frame(SumSq=c(SSR,SSE),Df=c(DF.SSR,DF.SSE),MeanSq=c(MSR,MSE),F0=c(round(F0,digits=3),' '), 
                    P.value=c(format(VP,scientific = TRUE,digits=3),' '),row.names =c("Modelo","Error")) 
  cat("Tabla ANOVA Modelo de Regresión","\n") 
  result 
}
miAnova(mod1)


###distancia de cook

requiere(MPV)
cooks.distance(mod)
