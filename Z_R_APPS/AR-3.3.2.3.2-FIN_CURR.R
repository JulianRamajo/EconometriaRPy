#
library(tidyverse)
library(car)
library(AER)
library(tseries) # Para obtener datos financieros en internet
library(dynlm)
library(imputeTS)
library(FinTS)
library(fGarch)
library(rugarch)
library(vars)
library(rmgarch)
# Para buscar otros tipos de cambio usar la página: https://finance.yahoo.com/currencies
# Para encontrar los principales índices bursátiles usar la página: https://finance.yahoo.com/world-indices
# Y para las principales criptomonedas: https://finance.yahoo.com/cryptocurrencies
EURUSD <- get.hist.quote("EURUSD=X", start = "2003-12-01", end = "2021-12-31")
USDJPY <- get.hist.quote("JPY=X", start = "2003-12-01", end = "2021-12-31")
GBPUSD <- get.hist.quote("GBPUSD=X", start = "2003-12-01", end = "2021-12-31")
CURR <- merge(EURUSD$Close, USDJPY$Close, GBPUSD$Close)
autoplot(CURR) + facet_free()
#
# A partir de ahora se va a modelizar el tipo de cambio euro-dólar EUR/USD (EURUSD=X)
#
plot(EURUSD)
autoplot(EURUSD$Close)
# NAs 
statsNA(EURUSD$Close)
ggplot_na_distribution(EURUSD$Close)
ggplot_na_intervals(EURUSD$Close)
EurUsd <-  na.approx(EURUSD$Close) # Reemplaza NAs por valores interpolados linealmente 
# (la opción na.spline usa splines cúbicos para la interpolación)
statsNA(EurUsd)
autoplot(EurUsd)
class(EurUsd)
#
# Si se quieren guardar los valores de la variable EurUsd para luego usarlos de nuevo:
#
# write.zoo(EurUsd, "FIN_EURUSD.CSV", index.name = "Index")
#
# EurUsd <- read.zoo("FIN_EURUSD.CSV", header = FALSE, sep = " ", format = "%Y-%m-%d")
#
rEurUsd <- 100 * diff(log(EurUsd))
statsNA(rEurUsd)
autoplot(rEurUsd)
summary(rEurUsd)
#
# Modelo ARIMA para la variable rEurUsd (ARMA al trabajar con log_difs)
#
auto.arima(rEurUsd) # Para determinar el modelo ARIMA más adecuado
#
summary(rEurUsd_AR <- dynlm(rEurUsd ~ L(rEurUsd,1:2)))
resid2.rEurUsd <- resid(rEurUsd_AR)^2
#
# Contraste de Engel de efectos ARCH
#
resid2.rEurUsd_ARCH1 <- dynlm(resid2.rEurUsd ~ L(resid2.rEurUsd,1:1)) 
summary(resid2.rEurUsd_ARCH1)
ArchTest(rEurUsd_AR$residuals , lags = 1)
resid2.rEurUsd_ARCH2 <- dynlm(resid2.rEurUsd ~ L(resid2.rEurUsd,1:2)) 
summary(resid2.rEurUsd_ARCH2)
ArchTest(rEurUsd_AR$residuals , lags = 2)
resid2.rEurUsd_ARCH3 <- dynlm(resid2.rEurUsd ~ L(resid2.rEurUsd,1:3)) 
summary(resid2.rEurUsd_ARCH3)
ArchTest(rEurUsd_AR$residuals , lags = 3)
#
# Modelizaciónn GARCH 
#
# Librería fGarch
#
rEurUsd_ARMA_GARCH <- garchFit(formula = ~ arma(2,0) + garch(1,1), data = rEurUsd, 
                               cond.dist = "norm", trace = FALSE)
summary(rEurUsd_ARMA_GARCH)
# plot(rEurUsd_ARMA_GARCH) # Distintas gráficas asociadas al modelo estimado
#
# Librería rugarch
#
rEurUsd_ARMA_GARCH_spec <-  ugarchspec(
  mean.model = list(armaOrder=c(2,0), include.mean = TRUE),
  variance.model = list(model="sGARCH", garchOrder=c(1,1)),
  distribution.model ="norm")
rEurUsd_ARMA_GARCH_fit <- ugarchfit(spec = rEurUsd_ARMA_GARCH_spec, data = rEurUsd)
show(rEurUsd_ARMA_GARCH_fit)
# plot(rEurUsd_ARMA_GARCH_fit)
#
# Modelo VAR para las variables rEurUsd, rUsdJpy y rGbpUsd
#
EurUsd <-  na.approx(EURUSD$Close)
UsdJpy <-  na.approx(USDJPY$Close)
GbpUsd <-  na.approx(GBPUSD$Close)
#
rEurUsd <- 100 * diff(log(EurUsd))
rUsdJpy <- 100 * diff(log(UsdJpy))
rGbpUsd <- 100 * diff(log(GbpUsd))
#
VAR_data <- as.matrix(cbind(rEurUsd,rUsdJpy,rGbpUsd))
#
#
VARselect(VAR_data,lag.max = 5, type = "const") # type = c("const", "trend", "both", "none")
VAR2 <-  VAR(VAR_data,p=2)
summary(VAR2)
#
# IRFs
irf_VAR2 <- irf(VAR2)
plot(irf_VAR2)
# FEVDs
fevd_VAR2 <- fevd(VAR2)
plot(fevd_VAR2)
# Contrastes de causalidad de Granger (asociados al modelo VAR)
causality(VAR2,cause = c("rGbpUsd","rUsdJpy"))$Granger
causality(VAR2,cause = c("rEurUsd","rUsdJpy"))$Granger
causality(VAR2,cause = c("rEurUsd","rGbpUsd"))$Granger
# Para contrastes bivariantes usar la función grangertest 
# Ejemplo: grangertest(as.ts(rEurUsd) ~ as.ts(rGbpUsd), order=2)
# Modelos VAR con estructura GARCH (multivariante)
#
ArchTest(VAR2$varresult$rEurUsd$residuals , lags = 1)
ArchTest(VAR2$varresult$rGbpUsd$residuals , lags = 1)
ArchTest(VAR2$varresult$rUsdJpy$residuals , lags = 1)
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
VAR_GARCH_fit_2@mfit$Rt              # Correlaciones condicionales entre los residuos estandarizados
VAR_GARCH_fit_2@model[["varcoef"]]   # Coeficientes estimados del modelo VAR para la media condicional
VAR_GARCH_fit_2@mfit[["coef"]]       # Coeficientes estimados del modelo GARCH multivariante para la varianza condicional
#
