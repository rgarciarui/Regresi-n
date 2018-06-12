library(MPV)
base <- softdrink
head(softdrink)
dim(softdrink)
n <- nrow(softdrink)  # Numero de observaciobes
k <- ceiling(0.70 * n)# numero de observaciones 70%
set.seed(12345)   # To fix a seed
index <- sample(x=1:n, size=k)#generar numeros aleatorios
index
train <- softdrink[ index, ]# Train dataset
test  <- softdrink[-index, ]# Test dataset

# scaling
maxs <- apply(train, 2, max)
mins <- apply(train, 2, min)

train.scaled <- scale(train, center=mins, scale=maxs-mins)
train.scaled <- as.data.frame(train.scaled)  # To convert as dataframe

test.scaled <- scale(test[, -1], center=mins[-1], scale=maxs[-1]-mins[-1])
test.scaled <- as.data.frame(test.scaled)  # To convert as dataframe

library(neuralnet)
mod1 <- neuralnet(y ~ x1 + x2, data=train.scaled)#modelo

plot(mod1)#grafica 

##predicciones

pred <- compute(x=mod1, covariate=test.scaled)
y.pred1 <- pred$net.result * (max(train$y) + min(train$y)) + min(train$y)


##MSE

y.true <- test[, 1]
mse1 <- round(mean((y.true - y.pred1)^2), digits=2)
mse1

##Regresión lineal multiple

mod2 <- lm(y ~ x1 + x2, data=train)
y.pred2 <- predict.lm(object=mod2, newdata=test[, -1])
mse2 <- round(mean((y.true - y.pred2)^2), digits=2)
mse2

R2predi <- function(mod){
  a <- anova(mod)
  ssq <- a$`Sum Sq`
  sst <- sum(ssq)
  hii <- lm.influence(mod)$hat
  ei <- residuals(mod)
  PRESS <- sum((ei/(1-hii))^2)
  predi <- 1-(PRESS/sst)
  return(predi)
}

R2predi(mod2)
##graficas par

par(mfrow=c(1, 2))
plot(y=y.true, x=y.pred1, pch=19, las=1, main=paste('With nn MSE=', mse1),
     xlim=range(c(y.pred1, y.pred2)), ylim=range(y.true))
abline(a=0, b=1, lty='dashed', col=gray(0.8))
plot(y=y.true, x=y.pred2, pch=19, las=1, main=paste('With lm MSE=', mse2),
     xlim=range(c(y.pred1, y.pred2)), ylim=range(y.true))
abline(a=0, b=1, lty='dashed', col=gray(0.8))

