#
## Lectura de datos
#
library(readr)
VENTAS <- read_csv("VENTAS_SUPER.csv")
dim(VENTAS)
VENTAS
summary(VENTAS) 
#
## Matriz de diagrama de puntos de las variables
#
library(alr4)
scatterplotMatrix(~ V + P + A, id=list(n=3), smooth=list(span=0.75), data=VENTAS)
#
## Modelo econométrico
#
modelo.ventas.1 <- lm(V ~ P + A, data=VENTAS)
S(modelo.ventas.1)
confint(modelo.ventas.1, level=.95)
plot(allEffects(modelo.ventas.1), grid=TRUE, rug=TRUE)
#
## Diagnósticos de la regresión
#
## Validación global de las hipótesis del MRL
#
library(gvlma)
gvmodel <- gvlma(modelo.ventas.1)
summary(gvmodel)
plot(gvmodel)
#
## Funcionamiento general del modelo
#
library(performance)
model_performance(modelo.ventas.1)
check_model(modelo.ventas.1)
#
## Test RESET de Ramsey de especificación funcional
#
library(lmtest)
resettest(modelo.ventas.1, power=2, type="fitted")
resettest(modelo.ventas.1, power=2:3, type="fitted")
#
## No linealidad
#
library(car)
residualPlots(modelo.ventas.1) # Gráficos estándar de los residuos 
avPlots(modelo.ventas.1) # Added-Variable plots
crPlots(modelo.ventas.1) # Component-plus-Residual plots (partial-residual plots)
ceresPlots(modelo.ventas.1) # CERES (Combining conditional Expectations and RESidual) plots
#
## Heteroscedasticidad (varianza no constante)
#
spreadLevelPlot(modelo.ventas, smooth=list(span=1))
ncvTest(modelo.ventas)
ncvTest(modelo.ventas, var.formula= ~ P + A)
#
## Normalidad y datos atípicos
#
densityPlot(residuals(modelo.ventas.1))
densityPlot(rstudent(modelo.ventas.1))
qqPlot(modelo.ventas.1, id=list(n=3)) # Outliers
outlierTest(modelo.ventas.1) # Outliers
influenceIndexPlot(modelo.ventas.1, id=list(n=3), vars=c("Studentized","hat","Cook")) # Outliers & Leverages
influencePlot(modelo.ventas.1, id=list(n=3)) # Outliers & Leverages
avPlots(modelo.ventas.1, id=list(n=3, method="mahal")) # id=list(n=0) suprime etiquetas
#
## Modelo alternativo
#
modelo.ventas.2 <- lm(V ~ P + A + I(A^2), data=VENTAS)
S(modelo.ventas.2)
confint(modelo.ventas.2, level=.95)
plot(Effect("P", modelo.ventas.2))
plot(Effect("A", modelo.ventas.2))
compareCoefs(modelo.ventas.1, modelo.ventas.2)
anova(modelo.ventas.1, modelo.ventas.2)
ccompare_performance(modelo.ventas.1, modelo.ventas.2, rank = TRUE)
plot(compare_performance(modelo.ventas.1, modelo.ventas.2, rank = TRUE))
test_performance(modelo.ventas.1, modelo.ventas.2)
test_wald(modelo.ventas.1, modelo.ventas.2)
test_bf(modelo.ventas.1, modelo.ventas.2)
test_vuong(modelo.ventas.1, modelo.ventas.2)
#