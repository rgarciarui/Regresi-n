#--------------------------------------------------------------
# Ejemplo lowess
#--------------------------------------------------------------
library(car)  # To use the data set

plot(prestige ~ income, xlab="Average Income",
     ylab="Prestige", data=Prestige, pch=19)

# Fitting the model
mod1 <- lowess(x=Prestige$income, y=Prestige$prestige, f=2/3)
summary(mod1) # ????

# To include the line
lines(mod1, lwd=4, col='tomato')

# How to predict?
# mmmmm

#--------------------------------------------------------------
# Ejemplo loess
#--------------------------------------------------------------

# Plotting the data
library("plot3D")
scatter3D(x=Prestige$income,
          y=Prestige$education,
          z=Prestige$prestige, pch=20, col='blue',
          ylab='Income',
          xlab='Education',
          zlab='Prestige',
          colkey = FALSE, phi = 30, type = "h")


# Fitting the model
mod2 <- loess(prestige ~ income + education, data=Prestige, 
              degree=2, span=0.75)
summary(mod2)

# To obtain the surface
inc <- with(Prestige, seq(min(income), max(income), len=25))
edu <- with(Prestige, seq(min(education), max(education), len=25))
newdata <- expand.grid(income=inc, education=edu)
fit.prestige <- matrix(predict(mod2, newdata), 25, 25)

persp(x=inc, y=edu, z=fit.prestige, 
      theta=45, phi=30, ticktype="detailed", 
      xlab="Income", ylab="Education", zlab="Prestige",
      shade=0.2, col="lightblue", expand=0.7)

# How to predict?
newdata <- data.frame(income=c(5635, 12563, 18965),
                      education=c(8.5, 12.3, 14.8))
predict(mod2, newdata)

# scatter + surface
scatter3D(x=Prestige$income,
          y=Prestige$education,
          z=Prestige$prestige, ticktype = "detailed", pch = 20, 
          bty = "f", colkey = FALSE, phi = 30, type = "h",
          surf = list(x = inc, y = edu, z = fit.prestige,  
                      NAcol = "black", shade = 0.1))


####impacro de (f) in lowess

# fs is a sequence of possible values for the f parameter
fs <- seq(from=0.01, to=0.99, by=0.02) 

# To obtain the animation

for (i in 1:length(fs)) {
  plot(prestige ~ income, xlab="Average Income", ylab="Prestige",
       data=Prestige, main=paste("f =", fs[i]), pch=19)
  mod1 <- lowess(x=Prestige$income, y=Prestige$prestige, f=fs[i])
  with(Prestige, lines(mod1, lwd=4, col='tomato'))
  Sys.sleep(0.5)  # delay
}



### impacto de span en loess
# Example: impact of span parameter

library(car) # to use the data set

# spans is a sequence of possible values for the f parameter
spans <- seq(from=0.10, to=0.90, by=0.01)

# To obtain the animation

for (i in 1:length(spans)) {
  mod2 <- loess(prestige ~ income + education, 
                data=Prestige, degree=1, span=spans[i])
  
  # To create the surface we need a grid for each variable
  inc <- with(Prestige, seq(min(income), max(income), len=25))
  edu <- with(Prestige, seq(min(education), max(education), len=25))
  newdata <- expand.grid(income=inc, education=edu)
  fit.prestige <- matrix(predict(mod2, newdata), 25, 25)
  
  # To obtain the surface
  persp(x=inc, y=edu, fit.prestige, theta=45, 
        phi=30, main=paste("span =", spans[i]),
        ticktype="detailed", xlab="Income", ylab="Education", 
        zlab="Prestige", expand=2/3, shade=0.2, col="lightblue")
  Sys.sleep(0.5) # delay
}
