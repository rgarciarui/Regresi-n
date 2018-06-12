datos <-read.table(file='http://tinyurl.com/hwhb769', header=T)
datos
attach(datos)
##---Primer punto
max(datos$mt2)
min(datos$mt2)
max(datos$alcobas)
min(datos$alcobas)
###---Segundo punto
with(datos, symbols(x=mt2, y=precio, circles=alcobas, 
                    las=1, inches=.2, fg='darkblue', main='Radio = N° alcobas'))
###---Tercer punto
which.max(datos$alcobas)
ndatos <- datos[-594, ]
###---Cuarto punto
with(ndatos, symbols(x=mt2, y=precio, circles=alcobas, 
                    las=1, inches=.2, fg='purple', main='Radio = N° alcobas'))
###---Quinto punto
mod <- lm(data=ndatos, precio~mt2+alcobas)
summary(mod)

###---Diagrama de contorno para miu????
