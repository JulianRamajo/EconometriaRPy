library(readr)
library(car)
library(MASS)
library(effects)
library(tidyverse)
library(gvlma)
library(tseries)
library(stats)
library(lmtest)
#
PROD_EMP <- read_csv("PROD_EMP.csv")
head(PROD_EMP, n=10)
dim(PROD_EMP)
summary(PROD_EMP)
#
# Matriz 'scatterplot' de los datos
scatterplotMatrix(~Y + L + K, data=PROD_EMP, var.labels=c("Producción", "Empleo", "Stock de capital"), col="black")
# Función Cobb-Douglas
S(lm_cd <- lm(log(Y) ~ log(L) + log(K), data = PROD_EMP))
confint(lm_cd)
#
# Diagnósticos
#
# Validación general del modelo (gvlma)
gvmodel <- gvlma(lm_cd)
summary(gvmodel)
plot(gvmodel)
gvmodel.del <- deletion.gvlma(gvmodel)
summary(gvmodel.del)
#
# 'residuals' -errores estimados y `rstudent()` - residuos estudentizados
# `densityPlot()` chequeo de la distribución de los errores (densidades estimadas)
densityPlot(residuals(lm_cd))
densityPlot(rstudent(lm_cd))
# `qqPlot()` chequeo de errores no-normales (comparación de los residuos estudentizados con una distribution t)
qqPlot(lm_cd)
#
# Chequeo de 'outliers' en la regresión
#
max(hatvalues(lm_cd))
which.max(hatvalues(lm_cd))
#
outlierTest(lm_cd)
#
max(cooks.distance(lm_cd))
which.max(cooks.distance(lm_cd))
#
max(abs(dffits(lm_cd)))
which.max(abs(dffits(lm_cd)))
#
# Medidas de influencia
S(influence.measures(lm_cd))
influenceIndexPlot(lm_cd, vars=c("hat", "Studentized","Cook"))
influencePlot(lm_cd, xlab="Hat values")
# Gráficos de variable añadida, buscando casos influyentes
avPlots(lm_cd, id=list(cex=0.60, method="mahal"))
# Chequeo de no linealidad: gráficos de componente+residuo
crPlots(lm_cd, smooth=list(span=0.7))
#
ncvTest(lm_cd, var.formula= ~ log(L) + log(K))
#
# Normalidad de los residuos
# Distribución de los residuos
r <- resid(lm_cd)
rbar <- mean(r)
sdr <- sd(r)
rbar ; sdr
hist(r, col="grey", freq=FALSE, main="Distribución de los residuos",
     ylab="Density", xlab="r")
curve(dnorm(x, rbar, sdr), col=2, add=TRUE,
      ylab="Density", xlab="r")
# 
# Residuos estandarizados
rs<-((r-rbar)/sdr) 
hist(rs, freq=FALSE, main="Distribución de los residuos estandarizados",
     ylab="Density", xlab="rs")
curve(dnorm(x, 0, 1), col=2, add=TRUE,
      ylab="Density", xlab="rs")
#
qqnorm(rs) 
abline(0,1) 
#
jarque.bera.test(r) #(package 'tseries')
shapiro.test(r)
#
# Distribución de los residuos estudentizados
# 
n <- nobs(lm_cd)
k <- n-df.residual(lm_cd)
n ; k
rt <- studres(lm_cd)
hist(rt, freq=FALSE,
     main="Distribución de los residuos estudentizados")
xfit<-seq(min(rt),max(rt),length=40)
yfit<-dt(xfit,n-k-2)
lines(xfit, yfit) 
#
# QQ plot de los residuos estudentizados
qqPlot(lm_cd, main="QQ Plot")
#
# No linealidad
# Component+R plots
crPlots(lm_cd)
# Ceres plots
ceresPlots(lm_cd)
# Test RESET
resettest(lm_cd, power=2, type="fitted")
resettest(lm_cd, power=2:3, type="fitted")
#
# Chequeo de varianza no constante: heteroscedasticidad
r2 <- resid(lm_cd)^2
yhat <- fitted(lm_cd)
l_L <- log(PROD_EMP$L)
l_K <- log(PROD_EMP$K)
plot(l_L,r2, xlab="Empleo", ylab="r^2")
plot(l_K,r2, xlab="Stock de capital", ylab="r^2")
#
plot(yhat,r2, xlab="Valores estimados", ylab="r^2")
#
spreadLevelPlot(lm_cd)
#
ncvTest(lm_cd)
ncvTest(lm_cd, ~ log(L) + log(K) + I((log(L))^2) + I((log(K))^2) + I(log(L)*log(K)) ) # Test de White (score test)
ncvTest(lm_cd, ~ log(L) + log(K) )  # Test de Breusch-Pagan (las variables Zs pueden ser externas al modelo) (score test)
#
bptest(lm_cd) # Breusch-Pagan robusto (variante robusta de Koenker)
bptest(lm_cd, studentize = FALSE) # Breusch-Pagan estándar (escalado)
bptest(lm_cd, studentize = FALSE, varformula = ~ log(L), data = PROD_EMP ) #  
#
# Corrección de la heteroscedasticidad
#
# Errores estándar (SEs) robustos
S(lm_cd, vcov.=hccm(lm_cd, type = "hc1")) # hc1 (corrección de White)
#
# Modelo translog
#
S(lm_tl <- lm(log(Y) ~ log(L) + log(K) + I((log(L))^2) + I((log(K))^2) + I(log(L)*log(K)), data = PROD_EMP))
S(lm_tl, vcov.=hccm(lm_tl, type = "hc1")) # hc1 (corrección de White)



