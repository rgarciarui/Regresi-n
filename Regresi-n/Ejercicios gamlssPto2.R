n <- 10000
x1 <- runif(n=n, 0,1)
x2 <- rbinom(n= n, size = 5, prob = 0.1)
mu <- -2 + 3*x1 + 4*x2
sigma <- exp(-2 + 2*x1 - 1.5*x2)
y <- rnorm(n=n, mean= mu, sd = sigma)
mod <- gamlss(y~ x1+x2, sigma.formula = ~x1+x2, family = NO)
coef(mod,what='mu')
coef(mod, what = 'sigma')
library(rgl)
plot3d(x=x1, y=x2, z=y, type='s', col='pink')
Rsq(mod)
plot(mod)
deviance(mod)

