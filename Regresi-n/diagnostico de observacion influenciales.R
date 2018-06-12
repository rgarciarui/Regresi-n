dat<-softdrink
mod<-lm(formula = y~x1+x2,data = softdrink)

##todas esta funciones debe de tener un modelo predio##
###parametros##
n<-nrow(dat)
n<-as.numeric(n)
p<-ncol(dat)
p<-as.numeric(p)
#--------------------------------------------------------------------------------------------
#////////////////////////////////////////////////////////////////////////////////////////////
#--------------------------------------------------------------------------------------------

#punto de Balanceo 
hii<-lm.influence(model = mod)$hat
hii
#si hii> 2p/n se puede considerar punto de balanceo
cot <-2*(p/n) #Cota para definir puntos de balanceo
which(hii>cot) #Cuales observaciones superan la cota
#--------------------------------------------------------------------------------------------

##DISTANCIA DE COOK'S:
Di<-cooks.distance(mod)
Di<-data.frame(Di)
Di
#son puntos influenciales la observacion mayores que
q <-n-p-2
cota <-4/q
which(Di > cota) #Cuales observaciones son mayores que la cota
##grafica
plot(mod,which = 4)
abline(h=cota,col="red")
par(mfrow = c(2,2))
plot(mod)
#-------------------------------------------------------------------------------------------

##DFFIST
#para hallar puntos influenciales,|DFFTIS|>2(p/n)^1/2
DFFITS<-data.frame(dffits(mod))
cotad <- 2*(sqrt(p/n))
which(abs(DFFITS) > cotad) #Cuales observaciones son influenciales
#grafica
require(olsrr)

ols_plot_dffits(mod)
#------------------------------------------------------------------------------------------

##DFBETAS
#es para encontrar puntos influenciales.
#se sugiere que se investigue toda obsevacion |DFBESTAS|> 2/(n)^1/2
DFFBETAS <- data.frame(dfbetas(mod))
DFFBETAS
cotb <- 2/sqrt(n)
which(abs(DFFBETAS) > cotb)
#grafica
require(olsrr)
library(olssr)
ols_plot_dfbetas(mod)

#------------------------------------------------------------------------------------------
#COVRATIO
#se puede dedifinir la relacion de covarianzas.
#*COVRATIO > 1 la i'esima observacion mejora  la presicion de la estimacion. 
#* CAVARATIO < 1 la inclusion  del i'esimo punto degrada la precision.

cov<-data.frame(covratio(mod))
cov
which(cov>1)
#------------------------------------------------------------------------------------------
#//////////////////////////////////////////////////////////////////////////////////////////
#------------------------------------------------------------------------------------------
#para hacer todos lo valores de diagnostico para balenceo e influencia
influence.measures(mod)

