require(MPV)
library(leaps)
mod1 <- regsubsets(y ~ ., data=table.b3, intercept=TRUE,
                   nbest=2, nvmax=4)

s <- table.b4
help("table.b4")

mod12 <- regsubsets(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data = s)

mod13 <- stepAIC(mod12,k=log(24))
summary(mod13)
dim(s)

summary(mod1)
plot(mod12, scale="adjr2", main=expression(R[Adj]^2))
plot(mod12, scale="bic", main='BIC')


par(mfrow=c(1, 2))
plot(mod1, scale="r2", main=expression(R^2))
plot(mod1, scale="Cp", main='Cp')