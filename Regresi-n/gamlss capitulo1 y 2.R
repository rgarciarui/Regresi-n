
#Capitulo 1
require(gamlss)
data <- rent
PPP <- par(mfrow=c(2,2))
plot(R~Fl, data=rent, col=gray(0.7), pch=15, cex=0.5)
plot(R~A, data=rent, col=gray(0.7), pch=15, cex=0.5)
plot(R~H, data=rent, col=gray(0.7), pch=15, cex=0.5)
plot(R~loc, data=rent, col=gray(0.7), pch=15, cex=0.5)
par(PPP)

##modelo
r1 <- gamlss(R ~ Fl+A+H+loc, family=NO, data=rent, trace=FALSE)  
l1 <- lm(R ~ Fl+A+H+loc,data=rent)#modelo con lm
coef(r1)#Coeficiente de modelo    
coef(l1)
##resid()
#Extraer el coeficiente ajustado sigma 
fitted(r1, "sigma")[1]
summary(r1)
Rsq(r1)##R^2 modelo
r2 <- gamlss(R ~ Fl+A+H+loc, family=GA, data=rent)
coef(r2)#coeficientes
coef(r2, "sigma")
deviance(r2)#desviacion

l2 <- glm(R ~ Fl+A+H+loc, family=Gamma(link="log"), data=rent)
coef(l2) ##modelo con glm

#modelo con gamlss
r22 <- gamlss(R ~ Fl+A+H+loc, family=IG, data=rent, trace=FALSE) 
##calidad relativa de los modelos

plot(r2)#grafico
GAIC(r1, r2, r22, k=0)##criterio

r4 <- gamlss(R ~ pb(Fl)+pb(A)+H+loc, sigma.fo=~pb(Fl)+pb(A)+H+loc, family=GA, data=rent, trace=FALSE) 
r5 <- gamlss(R ~ pb(Fl)+pb(A)+H+loc, sigma.fo=~pb(Fl)+pb(A)+H+loc, family=IG, data=rent, trace=FALSE)
AIC(r3, r4, r5)

term.plot(r4, pages=1, what="sigma", ask=FALSE)
drop1(r4, what="sigma")
#grafico de gusano
wp(r4, ylim.all=.6)

r6 <- gamlss(R ~ pb(Fl)+pb(A)+H+loc, sigma.fo=~pb(Fl)+pb(A)+H+loc, nu.fo=~1, family=BCCGo, data=rent, trace=FALSE)
#modelo donde se modeloa sigma
r7 <- gamlss(R ~ pb(Fl)+pb(A)+H+loc,sigma.fo=~pb(Fl)+pb(A)+H+loc,nu.fo=~pb(Fl)+pb(A)+H+loc, family=BCCGo, data=rent, trace=FALSE)
AIC(r4, r6, r7)

wp(r6, ylim.all=.6) ; title("r6: BCCG(mu, sigma)") 
wp(r7, ylim.all=.6) ; title("r7: BCCG(mu, sigma, nu)")

#Capitulo 2

library(gamlss) 
data(film90) 
plot(lborev1~lboopen, data=film90, col="lightblue", xlab="log opening revenue", ylab="log extra revenue")

m <- gamlss(lborev1~lboopen, data=film90, family=NO)
plot(lborev1~lboopen, data=film90, col = "lightblue")
lines(fitted(m)~film90$lboopen)

m00 <- gamlss(lborev1~lboopen+I(lboopen^2)+I(lboopen^3), data=film90, family=NO)

film90 <- transform(film90, lb2=lboopen^2, lb3=lboopen^3)
m002 <- gamlss(lborev1~lboopen + lb2 + lb3, data=film90, family=NO)
plot(lborev1~lboopen, col="lightblue", data=film90)
lines(fitted(m002)[order(film90$lboopen)]~ film90$lboopen[order(film90$lboopen)])
print(vcov(m00), digit=3)

print(vcov(m00, type="cor"), digit=3)#matriz de correlacion
print(vcov(m00, type="se"), digits=2)#error estandar
print(vcov(m00, type="se", robust=TRUE), digits=2)

#graficos de correlacion puntos
library(corrplot) 
col1 <- colorRampPalette(c("black","grey")) 
corrplot(vcov(m00, type="cor"), col=col1(2), outline=TRUE, tl.col = "black", addCoef.col = "white") 
corrplot(vcov(m0, type="cor"), col=col1(2), outline=TRUE, tl.col = "black", addCoef.col = "white")


#modelando niu y sigma

m3 <- gamlss(lborev1~pb(lboopen),sigma.formula=~pb(lboopen), data=film90, family=NO) 
plot(lborev1~lboopen, col="lightblue", data=film90) 
lines(fitted(m3)[order(film90$lboopen)]~ film90$lboopen[order(film90$lboopen)])
