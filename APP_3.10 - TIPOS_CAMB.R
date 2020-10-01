#
library(car)
library(vars)
library(tseries)
library(dynlm)
library(FinTS)
#
load("TIPOS_CAMB.RData")
currencies_ts <- ts(currencies[,2:7], frequency=365, start=c(1998, 348)) 
plot(currencies_ts)
#
reur <- currencies_ts[,"reur"]
rgbp <- currencies_ts[,"rgbp"]
rjpy <- currencies_ts[,"rjpy"]
ts.plot(reur,rgbp,rjpy, ylab="Tipos de cambio euro, libra y yen")
#
vardat <- as.matrix(cbind(reur,rgbp,rjpy))
#
VARselect(vardat,lag.max = 7, type = "const") # type = c("const", "trend", "both", "none")
VAR2 <-  VAR(vardat,p=2)
summary(VAR2)
# IRFs
irf_VAR2 <- irf(VAR2)
plot(irf_VAR2)
# FEVDs
fevd_VAR2 <- fevd(VAR2)
plot(fevd_VAR2)
# Contrastes de causalidad de Granger
grangertest(reur ~ rgbp)
grangertest(reur ~ rjpy)
causality(VAR2,cause = c("rgbp","rjpy"))$Granger

grangertest(rgbp ~ reur)
grangertest(rgbp ~ rjpy)
causality(VAR2,cause = c("reur","rjpy"))$Granger

grangertest(rjpy ~ reur)
grangertest(rjpy ~ rgbp)
causality(VAR2,cause = c("reur","rgbp"))$Granger

# Modelización ARCH
hist(reur, main="", breaks=20, freq=FALSE, col="grey")
hist(rgbp, main="", breaks=20, freq=FALSE, col="grey")
hist(rjpy, main="", breaks=20, freq=FALSE, col="grey")
# Contraste de Engel de efectos ARCH
S(VAR2_reur <- dynlm(reur ~ L(reur,1:2) + L(rgbp,1:2) + L(rjpy,1:2)))
ArchTest(VAR2_reur$residuals , lags = 1)
#
# Estimación de modelos GARCH para los residuos del modelo
#
summary(garch(VAR2_reur$residuals,c(0,1)))
#
summary(garch(VAR2_reur$residuals,c(1,1)))

