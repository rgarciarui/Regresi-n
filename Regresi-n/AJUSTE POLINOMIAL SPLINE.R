# Example 7.2 of MPV

# The data
drop <- c(8.33, 8.23, 7.17, 7.14, 7.31, 7.60, 7.94, 8.30, 8.76, 8.71, 9.71,
          10.26, 10.91, 11.67, 11.76, 12.81, 13.30, 13.88, 14.59,
          14.05, 14.48, 14.92, 14.37, 14.63, 15.18, 14.51, 14.34, 
          13.81, 13.79, 13.05, 13.04, 12.60, 12.05, 11.15, 11.15, 
          10.14, 10.08,9.78,9.80,9.95,9.51)
time <- seq(from=0, to=20, by=0.5)
dat <- data.frame(time=time, drop=drop)

# Scatterplot figure 7.6
plot(dat, ylab="Voltage drop", xlab="Time (seconds)", pch=19, 
     ylim=c(0,15), main="Figure 7.6")

abline(v= c(6.5, 13), lty="dotted", col='tomato')


## First try cubic polynomials
mod1 <- lm(drop ~ time + I(time^2) + I(time^3), data=dat)
summary(mod1)

# Now try cubic spline fitting
xplus <- function(x) ifelse(x >= 0, x, 0)  # Auxiliar function
time6.5 <- xplus(time - 6.5)
time13 <- xplus(time - 13)
mod2 <- lm(drop ~ time + I(time^2) + I(time^3) + 
             I(time6.5^3) + I(time13^3), data=dat)
summary(mod2)

# Adding fitted curves to the scatterplot
plot(dat, ylab="Voltage drop", xlab="Time (seconds)", pch=19,
     ylim=c(0, 15), main="Scatterplot with fitted models")
i <- order(time)
lines(time[i], fitted(mod1)[i], col=2, lwd=3)
lines(time[i], fitted(mod2)[i], col=4, lwd=3)
legend("bottomright", lwd=3, col=c(4,2), bty="n",
       legend=c("Cubic spline model", "Cubic polynomial model"))

# Residual plots
par(mfrow=c(1,2))
plot(mod1, 1, main="Cubic polynomial model", pch=20)
plot(mod2, 1, main="Cubic spline model", pch=20)

# Comparing the models
anova(mod1, mod2)

#------------------------------------------------------------------------------
# Now fitting the model using the bs function from splines package
# with 2 cutpoints at times 6.5 and 13
require(splines)
mod3 <- lm(drop ~ bs(time, knots=c(6.5, 13), degree=3))
summary(mod3)

# Plotting the Regression Line to the scatterplot   
plot(dat, ylab="Voltage drop", xlab="Time (seconds)", pch=19, 
     ylim=c(0,15), main="Figure 7.6")

lines(time[i], fitted(mod1)[i], col=2, lwd=3)
lines(time[i], fitted(mod2)[i], col=4, lwd=6)
lines(time[i], fitted(mod3)[i], col=5, lwd=2)
legend("bottomright", lwd=c(3, 6, 2), col=c(2, 4, 5),
       legend=c("Cubic polynomial model",
                "Cubic spline manually",
                "Using bs()"), bty="n")

# adding cutpoints
abline(v=c(6.5, 13), lty='dotted', col="tomato")

#------------------------------------------------------------------------------
# Now fitting the model using natural splines with ns function
# with 2 cutpoints at times 6.5 and 13
mod4 <- lm(drop ~ ns(time, knots=c(6.5, 13)))
summary(mod4)

# Plotting the Regression Line to the scatterplot   
plot(dat, ylab="Voltage drop", xlab="Time (seconds)", pch=19, 
     ylim=c(0,15), main="Figure 7.6")

lines(time[i], fitted(mod1)[i], col=2, lwd=3)
lines(time[i], fitted(mod2)[i], col=4, lwd=6)
lines(time[i], fitted(mod3)[i], col=5, lwd=2)
lines(time[i], fitted(mod4)[i], col=6, lwd=2)
legend("bottomright", lwd=c(3, 6, 2, 2), col=c(2, 4, 5, 6),
       legend=c("Cubic polynomial model",
                "Cubic spline manually",
                "Using bs()", "Using ns()"), bty="n")


#------------------------------------------------------------------------------
# Aqui se muestra como ajustar pero variando el tipo de spline

mod5 <- lm(drop ~ bs(time, knots=c(6.5, 13), degree=1))
mod6 <- lm(drop ~ bs(time, knots=c(6.5, 13), degree=2))
mod7 <- lm(drop ~ bs(time, knots=c(6.5, 13), degree=3))

# Plotting the Regression Line to the scatterplot   
i <- order(time)
par(mfrow=c(2, 2))
plot(dat, pch=19, main="Usando spline lineal")
lines(time[i], fitted(mod5)[i], col=2, lwd=2)
abline(v=c(6.5, 13), lty='dotted', col="tomato")

plot(dat, pch=19, main="Usando spline cuadrático")
lines(time[i], fitted(mod6)[i], col=4, lwd=2)
abline(v=c(6.5, 13), lty='dotted', col="tomato")

plot(dat, pch=19, main="Usando spline cúbico")
lines(time[i], fitted(mod7)[i], col=6, lwd=2)
abline(v=c(6.5, 13), lty='dotted', col="tomato")

#------------------------------------------------------------------------------
# spline versus natural spline

spline.usual <- lm(drop ~ bs(time, knots=c(6.5, 13)))
spline.natur <- lm(drop ~ ns(time, knots=c(6.5, 13)))

# Plotting the Regression Line to the scatterplot   
i <- order(time)
plot(dat, pch=19, main="bs versus ns")
lines(time[i], fitted(spline.usual)[i], col=2, lwd=2)
lines(time[i], fitted(spline.natur)[i], col=3, lwd=2)

legend("topright", lwd=2, col=2:3,
       legend=c("Spline usual", "Spline natural"), bty="n")

