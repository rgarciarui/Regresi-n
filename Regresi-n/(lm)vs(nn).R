#Función para generar los datos
mydata <- function(n){
  x1 <- rgamma(n = n, shape = 1, scale = 2)
  x2 <- runif(n = n, min = 0, max = 1)
  y <- rnorm(n = n, mean = -2 + 4*x1 + x2, sd=5)
  data.frame(y = y, x1 = x1, x2 = x2)
}

enes <- c(5,10,50)     #Valores para el tamaño de la muestra
nrep <- 1000           #Número de repeticiones
temp <- matrix(data = NA, ncol = 3, nrow = nrep)    #to store temporal results
results <- data.frame()     #to store the complete results

library(neuralnet)

for(n in enes){
  for(j in 1:nrep){
    data <- mydata(n = n)
    m <- nrow(data)           # Number of observations
    k <- ceiling(0.70 * m)         # 70% of observations to train
    index <- sample(x = 1:m, size = k)
    train <- data[index, ]   # Train dataset
    test  <- data[-index, ]   # Test dataset
    y.true <- test[,1]
    mod1 <- lm(y~., data = train)
    y.pred1 <- predict.lm(object = mod1, newdata = test[,-1])
    mse1 <- mean((y.true - y.pred1)^2)
    
    #Red neuronal
    maxs <- apply(data, 2, max) 
    mins <- apply(data, 2, min)
    scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
    train_ <- scaled[index,]
    test_ <- scaled[-index,]
    
    nom <- names(train_)
    f <- as.formula(paste("y ~", paste(nom[!nom %in% "y"], collapse = " + ")))
    mod2 <- neuralnet(f, data = train_, hidden=c(5,3), linear.output=T)
    
    #Predicción 
    pred <- compute(mod2, within(test_, rm(y)))
    y.pred2 <- pred$net.result*(max(data$y)-min(data$y))+min(data$y)
    mse2 <- mean((y.true - y.pred2)^2)
    
    temp[j,]<-c(mse1, mse2, n)    
  }
  results <- rbind(results, temp)
}

#To put names to results object
colnames(results)<-c('MSE(lm)', 'MSE(nn)', 'n')
#To split results according to n
res<-split(x = results[,1:2], f = results$n)
#To obtain the MSE
lapply(res, colMeans)
