#---------------------------------------------------------------------------------
#--------------------------- Transformaciones ------------------------------------
#---------------------------------------------------------------------------------
# Script para el problema de ajustar Rooms vs Crews
# Enlace a los datos
ruta <- "http://www.stat.tamu.edu/~sheather/book/docs/datasets/cleaning.txt"
# Cargando los datos
datos <- read.table(ruta, header=T, sep="")
# Para ver el inicio de los datos
head(datos)
# Disponibilizando las variables
attach(datos)
#---------------------------------------------------------------------------------
# Diagrama de dispersi?n para las variables
plot(Crews,Rooms,xlab="Number of Crews",
     ylab="Number of Rooms Cleaned", pch=19)
abline(lsfit(Crews, Rooms), lwd=4, col="dodgerblue", lty="dotted")
#---------------------------------------------------------------------------------
# Regression output on pages 72 and 73
m1 <- lm(Rooms~Crews)
summary(m1)
# Calculado estimaciones puntuales y por IC para y cuando x=4 y x=16
predict(m1,newdata=data.frame(Crews=c(4,16)),
        interval="prediction",level=0.95)
#---------------------------------------------------------------------------------
# Figure 3.18 on page 75
# Gr?fico de residuales
par(mfrow=c(2,2))
plot(m1, pch=19)
#---------------------------------------------------------------------------------
# Regression output on page 77
# Transformando las variables del modelo
sqrtcrews <- sqrt(Crews)
sqrtrooms <- sqrt(Rooms)
# Para ajustar el modelo 2 con las variables transformadas
m2 <- lm(sqrtrooms~sqrtcrews)
# Para ver la tabla de resultados
summary(m2)
# Calculado estimaciones puntuales y por IC para y cuando x=4 y x=16
predict(m2,newdata=data.frame(sqrtcrews=c(2,4)),
        interval="prediction",level=0.95)
# Diagrama de dispersi?n con las variables transformadas
plot(x=sqrtcrews, y=sqrtrooms, pch=19,
     xlab=expression(sqrt(Screws)), ylab=expression(sqrt(Rooms)))
abline(lsfit(sqrtcrews, sqrtrooms), lwd=4, col="dodgerblue", lty="dotted")
# Figure 3.21 on page 78
par(mfrow=c(2,2))
plot(m2, pch=19)
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------

