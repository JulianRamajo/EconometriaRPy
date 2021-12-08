#
library(tidyverse)
#
SAL_ESP <- read_csv("SAL_ESP.csv")
#
SAL_ESP_ts <- ts(SAL_ESP[,2:5], start=c(1996,1), end=c(2014,3), frequency = 4)
plot(SAL_ESP_ts)
#
library(dynlm)
dyn_SAL_0 <- dynlm(RW ~ L(RW) + RP + RYL + U, data=SAL_ESP_ts) # MCO
summary(dyn_SAL_0)
#
library(car)
linearHypothesis(dyn_SAL_0, c("RP=0", "RYL=0"))
#
dyn_SAL <- dynlm(RW ~ L(RW) + U, data=SAL_ESP_ts)
summary(dyn_SAL)
#
# Cambio estructural a partir del año 2010
# Localización del punto de ruptura
Y2010Q1=match("2010Q1",SAL_ESP$obs)
Y2010Q1
#
post2010 <- factor(c(rep(0, 56), rep(1, 75-56)))
post2010
#
# Test de Chow
#
# Método 1 (ANOVA)
summary(dyn_SAL_int <-dynlm(RW ~ L(RW) + U + post2010/(L(RW) + U), data=SAL_ESP_ts))
anova(dyn_SAL, dyn_SAL_int)
# Método 2 (librería structchange)
library(strucchange)
sbtest <- Fstats(dyn_SAL,data=SAL_ESP_ts)
# Estadísticos de Chow recursivos (structural breaks)
sbtest[["formula"]][["index"]]
sbtest[["Fstats"]]
Fs_CHOW <- ts(sbtest$Fstats, start = c(1999,1), frequency=4)
Fs_CHOW
# Test de Chow de cambio estructura en 2010 Q1
Fs_CHOW[56-11] # 15% de 75 = 11
1-pchisq(Fs_CHOW[56-11],sbtest$nreg)
#
# Regresión diferenciada por tramos
#
#
D2010Q1 <- as.numeric(post2010)-1
D2010Q1
# Ecuación con cambio estructural
dyn_SAL_1 <- dynlm(RW ~ (L(RW) + U)*D2010Q1, data=SAL_ESP_ts) # Método 1
summary(dyn_SAL_1)
anova(dyn_SAL,dyn_SAL_1)
#
# Otros contrastes
#
# Normalidad de los errores
hist(dyn_SAL$residuals)
library(moments)
skewness(dyn_SAL$residuals)
agostino.test(dyn_SAL$residuals)
kurtosis(dyn_SAL$residuals)
anscombe.test(dyn_SAL$residuals)
jarque.test(as.vector(dyn_SAL$residuals))
# Test de autocorrelación
library(lmtest)
dwtest(dyn_SAL)
library(ecm)
durbinH(dyn_SAL, "L(RW)")
bgtest(dyn_SAL,order = 1)
bgtest(dyn_SAL,order = 2)
# Test de Engle (heteroscedasticidad ARCH)
library(FinTS)
ArchTest(dyn_SAL$residuals, lags = 1)
ArchTest(dyn_SAL$residuals, lags = 2)
# Test de Hausman (2 pasos)
Hausman_1 <- dynlm(U ~ L(RW) + L(U), data=SAL_ESP_ts)
summary(Hausman_1)
Hausman_2 <- dynlm(RW ~ L(RW) + U + Hausman_1$fitted.values, data=SAL_ESP_ts)
summary(Hausman_2)
linearHypothesis(Hausman_2, c("Hausman_1$fitted.values=0"))
# Método VI
dyn_SAL_VI <- dynlm(RW ~ L(RW) + Hausman_1$fitted.values, data=SAL_ESP_ts)
summary(dyn_SAL_VI)
compareCoefs(dyn_SAL,dyn_SAL_VI)
#