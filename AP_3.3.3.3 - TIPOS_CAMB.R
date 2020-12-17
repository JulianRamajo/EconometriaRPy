#
library(car)
library(vars)
library(tseries)
library(dynlm)
#
load("TIPOS_CAMB.RData")
tipos_camb <- ts(monedas[,2:7], frequency=365, start=c(1998, 348)) 
plot(tipos_camb)
#
reur <- tipos_camb[,"reur"]
rgbp <- tipos_camb[,"rgbp"]
rjpy <- tipos_camb[,"rjpy"]
#
VAR_data <- as.matrix(cbind(reur,rgbp,rjpy))
#
VARselect(VAR_data,lag.max = 7, type = "const") # type = c("const", "trend", "both", "none")
VAR2 <-  VAR(VAR_data,p=2)
summary(VAR2)
# IRFs
irf_VAR2 <- irf(VAR2)
plot(irf_VAR2)
# FEVDs
fevd_VAR2 <- fevd(VAR2)
plot(fevd_VAR2)
# Contrastes de causalidad de Granger
causality(VAR2,cause = c("rgbp","rjpy"))$Granger
# grangertest(reur ~ rgbp)
# grangertest(reur ~ rjpy)
#
causality(VAR2,cause = c("reur","rjpy"))$Granger
# grangertest(rgbp ~ reur)
# grangertest(rgbp ~ rjpy)
#
causality(VAR2,cause = c("reur","rgbp"))$Granger
# grangertest(rjpy ~ reur)
# grangertest(rjpy ~ rgbp)
#
