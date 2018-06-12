require(rpart)
require(rpart.plot)
library(ggplot2)
data("msleep")
df <- msleep[,c(3,6,10,11)]
str(df)
attach(df)
m1 <- rpart(sleep_total ~., data=df, method = "anova")
m1
rpart.plot(m1, type = 3, digits = 3, fallen.leaves = TRUE)
p1 <- predict(m1, df)
p1
MAE <- function(actual, predicted){
  mean(abs(actual - predicted))
}
MAE
MAE(df$sleep_total, p1)

