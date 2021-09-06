#
library(car)
library(vars)
library(tseries)
library(dynlm)
library(FinTS)
library(rugarch)
library(rmgarch)
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
# Modelos VAR
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
# Modelos VAR con estructura GARCH (multivariante)
#
ArchTest(VAR2$varresult$reur$residuals , lags = 1)
ArchTest(VAR2$varresult$rgbp$residuals , lags = 1)
ArchTest(VAR2$varresult$rjpy$residuals , lags = 1)
# Modelos AR univariantes con correlación condicional (GARCH-Copula model)
VAR_GARCH_spec_1  <-  ugarchspec( 
  mean.model = list(armaOrder=c(2,0), include.mean = TRUE),
  variance.model = list (garchOrder = c(1,1), model = "sGARCH"))
Multi_spec_1  <-  multispec ( replicate (3 , VAR_GARCH_spec_1) )
Copula_GARCH_spec_1  <-  cgarchspec (Multi_spec_1 , VAR = F)
VAR_GARCH_fit_1 <-  cgarchfit ( Copula_GARCH_spec_1 , data = VAR_data )
show(VAR_GARCH_fit_1)
VAR_GARCH_fit_1@mfit$Rt  # Correlaciones condicionales entre los residuos estandarizados
# Modelo VAR con correlación condicional (GARCH-Copula model)
VAR_GARCH_spec_2  <-  ugarchspec(
  variance.model = list (garchOrder =c(1,1), model = "sGARCH"))
Multi_spec_2  <-  multispec ( replicate (3 , VAR_GARCH_spec_2) )
Copula_GARCH_spec_2  <-  cgarchspec (Multi_spec_2, VAR = TRUE, lag = 2)
VAR_GARCH_fit_2 <-  cgarchfit ( Copula_GARCH_spec_2 , data = VAR_data )
show(VAR_GARCH_fit_2)
VAR_GARCH_fit_2@mfit$Rt   # Correlaciones condicionales entre los residuos estandarizados
VAR_GARCH_fit_2@model[["varcoef"]]   # Coeficientes estimados del modelo VAR para la media condicional
VAR_GARCH_fit_2@mfit[["coef"]]       # Coeficientes estimados del modelo GARCH multivariante para la varianza condicional
#