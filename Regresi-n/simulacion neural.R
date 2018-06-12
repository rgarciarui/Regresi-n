gen.dat <- function(n) {  # Function to generate the data 
  x <- rgamma(n=n, shape=2, scale=3)  
  y <- rnorm(n=n, mean=-3-5*x, sd=5)  
  data.frame(x=x, y=y) 
} 

theta <- c(-3, -5, 5) # True vector parameter 
enes <- c(5, 10, 50) # the values for sample size
nrep <- 10000 # number of repetions 
temp <- matrix(data=NA, ncol=4, nrow=nrep) # to store temporal results 
results <- data.frame()  # to store the complete results

for (n in enes) { 
  for (j in 1:nrep) { 
    datos <- gen.dat(n=n) 
    mod <- lm(y ~ x, data=datos) 
    theta.hat <- c(coef(mod), summary(mod)$sigma) 
    temp[j, ] <- c((theta.hat - theta)^2, n) # Applying the MSE definition 
  } 
  results <- rbind(results, temp) 
}

# To put names to results object 
colnames(results) <- c('beta0', 'beta1', 'sigma', 'n')
# To split results according to n 
res <- split(x=results[, 1:3], f=results$n) 
# To obtain the MSE 
lapply(res, colMeans) 


# To put names to results object colnames(results) <- c('beta0', 'beta1', 'sigma', 'n') 
