### Ejemplo addterm() and dropterm ()
require(gamlss)
data("usair")
mod1<-gamlss(y~., data=usair, family=GA, trace=FALSE) 
summary(mod1)

#Ahora usamos drop1 () para verificar si se pueden eliminar los términos lineales:
dd<-drop1(mod1) 
dd
#O se puede usar
dd<-dropterm(mod1,test="Chisq")
dd

#uso de dropterm () cuando se muestra un suavizador y un factor en el modelo
data(aids) 
aids1<-gamlss(y~qrt+pb(x), data=aids, family=NBI, trace=FALSE) 
summary(aids1)
        
dropterm(aids1,test="Chisq")

#Agregar interacciones
add1(mod1, scope=~(x1+x2+x3+x4+x5+x6)^2)

#Si al agregar terminos suavizados son significativos
mod0 <- gamlss(y~1, data=usair,family=GA, trace=FALSE ) 
addterm(mod0, scope=~pb(x1)+pb(x2)+pb(x3)+pb(x4)+pb(x5)+pb(x6), test="Chisq")

mod2<-stepGAIC(mod1)
mod2$anova
mod21<-stepGAIC(mod1, k=log(length(usair$y)))
#INCLUYENDO TODAS LAS INTERACCIONES POSIBLES
mod3 <- stepGAIC(mod1, scope=list(lower=~1, upper=~(x1+x2+x3+x4+x5+x6)^2), k=log(41))
summary(mod3)
FORM <- as.formula("~(x1 + x2 + x3 + x4 + x5 + x6)^2 + pb(x1) + pb(x2) + 
                   pb(x3) + pb(x4) + pb(x5) + pb(x6)")
mod10<- stepGAIC(mod1, scope=list(lower=~1, upper=FORM), k=log(41))

#INCLUSION DE TERMINOS LINEALES EN SIGMA
mod4 <- stepGAIC(mod1, parameter="sigma", scope=~x1+x2+x3+x4+x5+x6, k=log(41))
mod4$anova

#EJEMPLO USANDO STEPGAICALL.A()
m1<-gamlss(y~1, data=usair, family=GA, trace=FALSE, n.cyc = 50) 
m2<-stepGAICAll.A(m1, scope=list(lower=~1, upper=~x1+x2+x3+x4+x5+x6), k=log(41))
summary(m2)

#EJEMPLO USANDO STEPGAICALL.B()
m4<-stepGAICAll.B(m1, scope=list(lower=~1, upper=~x1+x2+x3+x4+x5+x6), k=log(41))


#EJEMPLO FUNCION FIND.HIPER()
a1<-gamlss(y~pb(x), sigma.fo=~pb(x), data=abdom, family=LO, trace=FALSE) 
a2<- gamlss(y~pb(x, method="GCV"), sigma.fo=~pb(x, method="GCV"), 
            data=abdom, family=LO, trace=FALSE) 
a3<-gamlss(y~pb(x, method="GAIC", k=2), sigma.fo=~pb(x, method="GAIC",k=2), 
           data=abdom, family=LO, trace=FALSE)
# the effective degrees of freedom used 
edfAll(a1)
edfAll(a2)
edfAll(a3)


mod1 <- quote(gamlss(y ~ pb(x, df = p[1]), sigma.fo=~pb(x, df=p[2]), 
                     family = LO, data = abdom, trace=FALSE))
op <- find.hyper(model = mod1, par = c(3,3), lower = c(0,0), 
                 steps = c(0.1, 0.1), trace=FALSE)

datos <- usair
