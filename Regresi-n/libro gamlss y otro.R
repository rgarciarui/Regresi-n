n <- 10000
x1 <- runif(n=n, 0,1)
mu <- exp(-2 + 2*x1)
y <- rbinom(n=n, size = 1, prob = mu)
modg <- gamlss(y~ x1, family = BI )
summary(modg)
coef(modg)
require(gamlss)
help("gamlss")
fitted(modg, "sigma")
Rsq(modg)
plot(modg)
drop1(modg)
help(gamlss.add)

require(gamlss)
datos <- film90
plot(lborev1~lboopen, data=film90, col="lightblue",
     xlab="log opening revenue", ylab="log extra revenue")
m <- gamlss(lborev1~lboopen, data=film90, family=NO)
plot(lborev1~lboopen, data=film90, col = "lightblue")
lines(fitted(m)~film90$lboopen)

m00 <- gamlss(lborev1~lboopen+I(lboopen^2)+I(lboopen^3), data=film90,
              family=NO)
lines(fitted(m00)[order(film90$lboopen)]~
        film90$lboopen[order(film90$lboopen)])
summary(m)
print(vcov(m00), digit=3) ###Matriz de varianzas-covarianzas
m0 <- gamlss(lborev1~poly(lboopen,3), data=film90, 
             family=NO)##Usando polinomios ortogonales

library(corrplot)
col1 <- colorRampPalette(c("black","grey"))
par(mfrow= c(1,2))
corrplot(vcov(m00, type="cor"), col=col1(2), outline=TRUE,
         tl.col = "black", addCoef.col = "white")
corrplot(vcov(m0, type="cor"), col=col1(2), outline=TRUE,
         tl.col = "black", addCoef.col = "white")


plot(film90$lboopen, fitted(m,"mu"))
fitted(m,"sigma")
resid(m)
