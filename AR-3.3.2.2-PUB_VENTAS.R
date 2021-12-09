#
library(tidyverse)
PUB_VENTAS <- read_csv("PUB_VENTAS.csv")
#
mod_VENTAS <- lm(VENTAS ~ TV*RADIO, data=PUB_VENTAS)
summary(mod_VENTAS)
#
library(scatterplot3d)
scatterplot3d(PUB_VENTAS$TV, PUB_VENTAS$RADIO, PUB_VENTAS$VENTAS, main="Scatterplot 3D VENTAS-PUB_TV-PUB_RADIO")
# Efectos marginales
library(car)
b <- coef(mod_VENTAS)
b
dVENTAS_dTV <- b[2] + b[4]*PUB_VENTAS$RADIO
dVENTAS_dRADIO <- b[3] + b[4]*PUB_VENTAS$TV
scatterplot(dVENTAS_dTV ~ PUB_VENTAS$RADIO)
scatterplot(dVENTAS_dRADIO ~ PUB_VENTAS$TV)
#
library(lmtest)
resettest(mod_VENTAS, power = 2)
# Normalidad de los errores
hist(mod_VENTAS$residuals)
library(car)
densityPlot(mod_VENTAS$residuals)
qqPlot(mod_VENTAS$residuals, distribution = "norm")
library(moments)
skewness(mod_VENTAS$residuals)
kurtosis(mod_VENTAS$residuals)
jarque.test(mod_VENTAS$residuals)
# Heteroscedasticidad
spreadLevelPlot(mod_VENTAS)
bptest(formula(mod_VENTAS), varformula= ~ TV*RADIO, data=PUB_VENTAS, studentize=T)
ncvTest(mod_VENTAS)
ncvTest(mod_VENTAS, var.formula=~ TV*RADIO, data=PUB_VENTAS)
bptest(formula(mod_VENTAS), varformula= ~ TV*RADIO, data=PUB_VENTAS, studentize=F)
# MCO corregidos (White)
summary(mod_VENTAS, vcov. = vcovHC(mod_VENTAS, type="HC1")) 
# MCP
summary(mod_l_resid2 <- lm( log(mod_VENTAS$residuals^2) ~ TV*RADIO, data=PUB_VENTAS) )  # Regresión auxiliar 
sigma2 <-exp(mod_sigma2$fitted.values)
summary(mod_VENTAS_het <- lm(VENTAS ~ TV*RADIO, weights = 1/sigma2, data=PUB_VENTAS)) # Mínimnos cuadrados con ponderaciones
# Comparación de resultaodos MCO-MCP
compareCoefs(mod_VENTAS, mod_VENTAS_het)
#