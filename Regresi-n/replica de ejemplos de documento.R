library(car) # for data sets
plot(prestige ~ income, xlab="Average Income", ylab="Prestige", data=Prestige)
with(Prestige, lines(lowess(income, prestige, f=0.5, iter=0), lwd=2))
##El argumento f a lowess le da al tramo de la regresión local más suavidad; iter = 0 
##especifica que las regresiones locales no deberían reajustarse a observaciones 
##periféricas inferiores.

mod.lo <- loess(prestige ~income + education, span=.5, degree=1, data=Prestige)
summary(mod.lo)


inc <- with(Prestige, seq(min(income), max(income), len=25))
ed <- with(Prestige, seq(min(education), max(education), len=25))
newdata <- expand.grid(income=inc, education=ed)
fit.prestige <- matrix(predict(mod.lo, newdata), 25, 25)
persp(inc, ed, fit.prestige, theta=45, phi=30, ticktype="detailed",
      xlab="Income", ylab="Education", zlab="Prestige", expand=2/3, shade=0.5)

### Smoothing splines
mod.lo.inc <- loess(prestige ~ income, span=.7, degree=1,
                    data = Prestige) # omitting education
mod.lo.ed <- loess(prestige ~ education, span=.7, degree=1,
                   data=Prestige) # omitting income

plot(prestige ~ income, data=Prestige)
inc.100 <- with(Prestige, seq(min(income), max(income), len=100)) # 100 x-values
pres <- predict(mod.lo.inc, data.frame(income=inc.100)) # fitted values
lines(inc.100, pres, lty=2, lwd=2) # loess curve
lines(with(Prestige, smooth.spline(income, prestige, df=3.85),lwd=2)) # smoothing spline


##Regresion no parametrica aditiva
library(mgcv)
mod.gam <- gam(prestige ~ s(income) + s(education), data=Prestige)
summary(mod.gam)


fit.prestige <- matrix(predict(mod.gam, newdata), 25, 25)
persp(inc, ed, fit.prestige, theta=45, phi=30, ticktype="detailed",
      lab="Income", ylab="Education", zlab="Prestige", expand=2/3,
      shade=0.5)
plot(mod.gam)

### Regresion no parametrica generalizada
remove(list=objects()) # clean up everything
Mroz$k5f <- factor(Mroz$k5)
Mroz$k618f <- factor(Mroz$k618)
Mroz$k5f <- recode(Mroz$k5f, "3 = 2")
Mroz$k618f <- recode(Mroz$k618f, "6:8 = 5")
mod.1 <- gam(lfp ~s(age) + s(inc) + k5f + k618f + wc + hc,family=binomial, data=Mroz)
summary(mod.1)
