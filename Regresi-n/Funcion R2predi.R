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
