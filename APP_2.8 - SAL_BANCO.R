library(readr)
library(car)
library(MASS)
library(effects)
library(tidyverse)
library(RcmdrMisc)
library(sfsmisc)
#
SAL_BANCO <- read_csv("SAL_BANCO.csv")
str(SAL_BANCO)
head(SAL_BANCO, n=10)
dim(SAL_BANCO)
summary(SAL_BANCO)
#
# Matriz 'scatterplot' de los datos
scatterplotMatrix(~SALARIO + EDUC + EDAD + EXPER, data=SAL_BANCO, 
                  var.labels=c("Salario", "Nivel de educación", 
                               "Edad", "Experiencia"),
                  smooth=list(smoother=loessLine, var=FALSE, lwd.smooth=3), 
                  col="black")
# Análisis univariante de la variable SALARIO
#
par(mfrow=c(2, 2))
Hist(SAL_BANCO$SALARIO, xlab="SALARIO", ylab="Frecuencia", 
     col="gray", main="(a)")
Boxplot(~SALARIO, data=SAL_BANCO, main="(b)", ylab="SALARIO")
densityPlot(SAL_BANCO$SALARIO, from=0, normalize=TRUE, 
            xlab="SALARIO", main="(c)")
qqPlot(~SALARIO, data=SAL_BANCO, ylab="SALARIO", 
       xlab="Normal Quantiles", main="(d)",
       id=list(method=c(TRUE, rep(FALSE, 132), TRUE)), col.lines="black")
#
with(SAL_BANCO, hist(SALARIO))
with(SAL_BANCO, hist(log(SALARIO)))
# Simetría de los boxplots
symbox(~SALARIO, data=SAL_BANCO, xlab=expression("Potencias,"~lambda), ylab="", 
       powers = c(-1, -0.5, 0, 0.33, 0.5, 1))
mtext(2, 1, text=expression(t[BC]("SALARIO",~lambda)))
#
# Estimación del lambda de la transformación
#
S(pt <- powerTransform(SALARIO ~ 1, data=SAL_BANCO))
pt$lambda # estimated lambda
sqrt(pt$invHess) # SE
# Densidad de la variable SALARIO log-transformada
#
densityPlot(~log(SALARIO), data=SAL_BANCO, adjust=0.75, xlab="log(SALARIO)")
basicPowerAxis(0, side="above", at=c(1, 5, 10, 20, 50, 100), 
               axis.title="")
# Análisis bivariante
#
# Scatterplots
#
scatterplot(log(SALARIO) ~ EDUC, data=SAL_BANCO, smooth=list(smoother=loessLine, var=FALSE, 
                                                lwd.smooth=3), col="black",
            regLine=list(lwd=3),
            xlab="EDUC", 
            ylab="log(SALARIO)")
#
scatterplot(log(SALARIO) ~ EDAD, data=SAL_BANCO, smooth=list(smoother=loessLine, var=FALSE, 
                                                             lwd.smooth=3), col="black",
            regLine=list(lwd=3),
            xlab="EDAD", 
            ylab="log(SALARIO)")
#
scatterplot(log(SALARIO) ~ EDUC, data=SAL_BANCO, pch=".")
scatterplot(log(SALARIO) ~ EDAD, data=SAL_BANCO, pch=".")
#
pairs( ~ SALARIO + EDUC + EDAD, data=SAL_BANCO)
scatterplotMatrix( ~ SALARIO + EDUC + EDAD, data=SAL_BANCO)
library(PerformanceAnalytics)
chart.Correlation(SAL_BANCO, method="pearson", histogram=TRUE, pch=16)
#
# Gráficos bidimensionales
#
ggplot(SAL_BANCO, aes(x=EDUC, y=SALARIO)) + geom_point() + labs(title="Diagrama de puntos", x="Educación", y="Salario")
ggplot(SAL_BANCO, aes(x=EDAD, y=SALARIO)) + geom_point() + labs(title="Diagrama de puntos", x="Edad", y="Salario")
ggplot(SAL_BANCO, aes(x=as.factor(SEXO), y=SALARIO)) + geom_boxplot() + labs(title="Box plot", x="Sexo", y="Salario")
ggplot(SAL_BANCO, aes(x=as.factor(RAZA), y=SALARIO)) + geom_boxplot() + labs(title="Box plot", x="Raza", y="Salario")
#
ggplot(SAL_BANCO, aes(x=EDUC, y=SALARIO)) + geom_point() + geom_smooth(method = 'lm')
ggplot(SAL_BANCO, aes(x=EDAD, y=SALARIO)) + geom_point() + geom_smooth(method = 'lm')
#
# Variables de control (cambiar SEXO por RAZA para analizar los cambios)
#
ggplot(SAL_BANCO, aes(x=EDUC, y=SALARIO, color=as.factor(SEXO), shape=as.factor(SEXO))) + geom_point()
ggplot(SAL_BANCO, aes(x=EDUC, y=SALARIO, color=as.factor(SEXO), shape=as.factor(SEXO))) + geom_point() + geom_smooth(method = 'lm')
ggplot(SAL_BANCO, aes(x=EDAD, y=SALARIO, color=as.factor(SEXO), shape=as.factor(SEXO))) + geom_point()
ggplot(SAL_BANCO, aes(x=EDAD, y=SALARIO, color=as.factor(SEXO), shape=as.factor(SEXO))) + geom_point() + geom_smooth(method = 'lm')
#
ggplot(SAL_BANCO, aes(x=EDUC, y=SALARIO, color=as.factor(SEXO), size=EDAD)) + geom_point()
ggplot(SAL_BANCO, aes(x=EDUC, y=SALARIO, color=as.factor(RAZA), size=EDAD)) + geom_point()
#
ggplot(SAL_BANCO, aes(x=EDUC, y=SALARIO)) + geom_point() + facet_wrap(~ as.factor(SEXO))
ggplot(SAL_BANCO, aes(x=EDUC, y=SALARIO)) + geom_point() + facet_wrap(~ as.factor(RAZA))
#
ggplot(SAL_BANCO, aes(x=EDAD, y=SALARIO)) + geom_point() + facet_wrap(~ as.factor(SEXO))
ggplot(SAL_BANCO, aes(x=EDAD, y=SALARIO)) + geom_point() + facet_wrap(~ as.factor(RAZA))
#
# Modelo de regresión lineal
# R usa la notación de Wilkinson-Rogers para especificar modelos: variable respuesta ~ variables explicativas
# El símbolo (~) se lee como “es modelizada como función de” . 
# Otros símbolos utilizados son:
#  + inclusiónn de variable
#  - exclusión de variable (no substracción)
#  ∗ incluir variables y sus interacciones
#  : interaccionar dos variables
#  ∧ interacción de variables hasta un grado especificado (no un exponente)
#  Para obviar un símbolo de modelo, usar la función I().
# Ejemplos:
# y ~ x1 + x2 + x3 (regresión múltiple)
# y ~ . (regress y on all variables in data set)
# y ~ x1 + x2 - 1 (excluir la constante del modelo)
# y ~ x1 + x2 + x1:x2 (incluir interacción entre x1 y x2)
# y ~ x1 * x2 (incluir x1, x2 y su interacción - mismo resultado que el modelo anterior)
# y ~ x1 + x2 + x3 + x1:x2 + x1:x3 + x2:x3 + x1:x2:x3 (modelo con interacciones de doble y triple vía)
# y ~ x1 * x2 * x3 (igual que el modelo anterior)
# y ~ (x1 + x2 + x3)ˆ2 (interacciones de dbole-vía)
# y ~ x1 + I(x1ˆ2) + x2 (regresión cuadrática en x1 más variables x2)
# y ~ poly(x1, 2, raw = TRUE) + x2 (igual que el modelo anterior)
# Modelo lineal
S(lm_sal_0 <- lm(SALARIO ~ EDUC + EDAD, data = SAL_BANCO))
## Diagnósticos
# 'residuals' -errores estimados y `rstudent()` - residuos estudentizados
# `densityPlot()` chequeo de la distribución de los errores (densidades estimadas)
densityPlot(residuals(lm_sal_0))
densityPlot(rstudent(lm_sal_0))
spreadLevelPlot(lm_sal_0)
# `qqPlot()` chequeo de errores no-normales (comparación de los residuos estudentizados con una distribution t)
qqPlot(lm_sal_0)
# Chequeo de 'outliers' en la regresión
#
max(hatvalues(lm_sal_0))
which.max(hatvalues(lm_sal_0))
#
outlierTest(lm_sal_0)
#
max(cooks.distance(lm_sal_0))
which.max(cooks.distance(lm_sal_0))
#
max(abs(dffits(lm_sal_0)))
which.max(abs(dffits(lm_sal_0)))
#
# Medidas de influencia
S(influence.measures(lm_sal_0))
influenceIndexPlot(lm_sal_0, vars=c("Cook", "hat", "Studentized"))
influencePlot(lm_sal_0, xlab="Hat values")
# Gráficos de variable añadida, buscando casos influyentes
avPlots(lm_sal_0, id=list(cex=0.60, method="mahal"))
# Chequeo de no linealidad: gráficos de componente+residuo
crPlots(lm_sal_0, smooth=list(span=0.7))
# Chequeo de varianza no constante:
ncvTest(lm_sal_0)
ncvTest(lm_sal_0, var.formula= ~ EDUC + EDAD)
#
# Modelo log-lineal
#
S(lm_sal <- lm(log(SALARIO) ~ EDUC + EDAD, data = SAL_BANCO))
brief(lm_sal)
coef(lm_sal)
confint(lm_sal)
#
# Introducción de factores de control
# Boxplots
#
Boxplot(log(SALARIO) ~ SEXO, data=SAL_BANCO, id=list(location="lr"), 
        ylab="SALARIO", xlab="SEXO")
#
Boxplot(log(SALARIO) ~ SEXO, data=SAL_BANCO, id=list(location="lr"), 
        ylab="SALARIO", xlab="RAZA")
#
lm_sal_factors <- lm(log(SALARIO) ~ SEXO + RAZA + EDUC + EDAD, data = SAL_BANCO)
S(lm_sal_factors)
compareCoefs(lm_sal, lm_sal_factors)
#
anova(lm_sal_factors)
#
# Modelo cuadrático con factores de control
#
S(lm_sal_poly <- lm(log(SALARIO) ~ SEXO + RAZA + poly(EDUC,2, raw=TRUE) + poly(EDAD,2, raw=TRUE), data = SAL_BANCO))
# Comparación con el modelo simple
anova(lm_sal, lm_sal_poly)
# Chequeo por defecto
plot(lm_sal_poly)
# Chequeo de las hipótesis de linealidad (y varianza constante)
plot(lm_sal_poly, which = 1)
# Chequeo de la hipótesis de normalidad
plot(lm_sal_poly, which = 2)
# Chequeo de las hipótesis de varianza constante
plot(lm_sal_poly, which = 3)
# Chequeo de observaciones atípicas e influyentes
plot(lm_sal_poly, which = 5)
plot(lm_sal_poly, which = 4)
#
# Análisis de efectos
summary(lm_sal_poly)
plot(Effect("EDUC", lm_sal_poly))
plot(Effect("EDAD", lm_sal_poly))
#
# Modelo con efectos de interacción
#
lm_sal_poly_int <- lm(log(SALARIO) ~ SEXO + RAZA + poly(EDUC,2, raw=TRUE) + poly(EDAD,2, raw=TRUE) + EDUC:EDAD, data = SAL_BANCO)
S(lm_sal_poly_int)
plot(Effect("EDUC", lm_sal_poly_int))
plot(Effect("EDAD", lm_sal_poly_int))
plot(Effect(c("EDUC","EDAD"), lm_sal_poly_int))
#
# Diagnóstico del modelo
#
# Normalidad de los residuos
# Distribución de los residuos
library(tseries)
r <- resid(lm_sal_poly_int)
rbar <- mean(r)
sdr <- sd(r)
hist(r, col="grey", freq=FALSE, main="Distribución de los residuos",
     ylab="Density", xlab="residuos")
curve(dnorm(x, rbar, sdr), col=2, add=TRUE,
      ylab="Density", xlab="r")
# 
# Residuos estandarizados
stand<-function(x){m=mean(x) + s=(var(x)^0.5) + z=(x-m)/s + return(z)}
rs<-stand(r) 
hist(rs, freq=FALSE,
     main="Distribución de los residuos estandarizados")
#
qqnorm(rs) 
abline(0,1) 
#
jarque.bera.test(r) #(package 'tseries')
shapiro.test(r)
#
# Distribución de los residuos estudentizados
library(MASS)
rt <- studres(lm_sal_poly_int)
hist(rt, freq=FALSE,
     main="Distribución de los residuos estudentizados")
xfit<-seq(min(rt),max(rt),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit) 
#
# QQ plot de los residuos estudentizados
qqPlot(lm_sal_poly_int, main="QQ Plot")
#
# Multicolinealidad
# Matriz de correlaciones
cor(SAL_BANCO)
# Factores de inflación de la varianza
vif(lm_sal_poly_int) 
sqrt(vif(lm_sal_poly_int)) > 2 # problema
