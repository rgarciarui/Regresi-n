require(MPV)
table.b3[22:26, ]
dt <- table.b3[-c(23,25), ]

full.model <- lm(y ~ ., data=dt)  #  ~ . sirve para incluir todas las x's
summary(full.model)

require(MASS)  # Para poder usar la funcion stepAIC
modback <- stepAIC(object=full.model, trace=TRUE, direction="backward", k=2)
modback$anova
summary(modback)

#Ahora aplicamos forward
empty.model <- lm(y ~ 1, data=dt)
horizonte <- formula(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11)
modforw <- stepAIC(empty.model, trace=FALSE, direction="forward", scope=horizonte)
modforw$anova
summary(modforw)

#Como x4 no es significativa la borramos del modelo
modforw <- update(modforw, y ~ x1)
summary(modforw)

## Aplicacion del metodo both
modboth <- stepAIC(empty.model, trace=FALSE, direction="both", scope=horizonte)
modboth$anova

##Comparacion de los modelos
summary(modback)$adj.r.squared
summary(modforw)$adj.r.squared

### Comparando el grafico de los residuales
par(mfrow=c(1, 2))
plot(modback, main="Backward", pch=19, cex=1, which=1)
plot(modforw, main="Forward", pch=19, cex=1, which=1)

###Normalidad de los residuales
par(mfrow=c(1, 2))
plot(modback, main="Backward", pch=19, which=2)
plot(modforw, main="Backward", pch=19, which=2)

extractAIC(fit=modback, k=2)
extractAIC(fit=modforw, k=2)
