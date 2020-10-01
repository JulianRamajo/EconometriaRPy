#
library(readr)
VENTAS <- read_csv("VENTAS.csv")
dim(VENTAS)
VENTAS
summary(VENTAS) 
#
## Gráfica de las variables
#
library("alr4")
scatterplotMatrix(~ V + P + A, id=list(n=3),
                  smooth=list(span=0.7), data=VENTAS)
pairs(~ V + P + A, data=VENTAS)
#
## Modelo econométrico
#
modelo.ventas <- lm(V ~ P + A, data=VENTAS)
S(modelo.ventas)
plot(allEffects(modelo.ventas), grid=TRUE, rug=TRUE)
confint(modelo.ventas, level=.95)
pred.ventas <- predict(modelo.ventas, newdata=data.frame(P=c(6), A=c(1.9)), se.fit=TRUE, interval="prediction", level=.95)
pred.ventas
#
## Diagnósticos de la regresión
# 
### No-linealidad
#
crPlots(modelo.ventas, smooth=list(span=0.7))
#
### Heteroscedasticidad (varianza no constante)
#
spreadLevelPlot(modelo.ventas, smooth=list(span=1))
ncvTest(modelo.ventas)
ncvTest(modelo.ventas, var.formula= ~ P + A)
#
### Datos atípicos
#
densityPlot(rstudent(modelo.ventas))
qqPlot(modelo.ventas, id=list(n=3))
outlierTest(modelo.ventas)
#
influencePlot(modelo.ventas, id=list(n=3))
#
avPlots(modelo.ventas, id=list(n=3, method="mahal")) # id=list(n=0) suprime etiquetas
#
modelo.ventas.2 <- update(modelo.ventas, subset=-c(3,9,38,39))
S(modelo.ventas.2)
compareCoefs(modelo.ventas, modelo.ventas.2)
#