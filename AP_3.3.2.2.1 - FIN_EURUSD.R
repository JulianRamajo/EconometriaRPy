#
library(AER)
library(tseries) # Para obtener datos financieros en internet
library(dynlm)
library(ggplot2)
library(imputeTS)
library(FinTS)
library(forecast)
library(fGarch)
library(rugarch)
#
EURUSD <- get.hist.quote("EURUSD=X", start = "2004-01-01", end = "2020-12-15")
plot(EURUSD)
autoplot(EURUSD$Close) + facet_free()
statsNA(EURUSD$Close)
ggplot_na_distribution(EURUSD$Close)
ggplot_na_intervals(EURUSD$Close)
EurUsd <-  na.approx(EURUSD$Close) # Reemplaza NAs por valores interpolados linealmente (na.spline usa splines cúbicos)
statsNA(EurUsd)
autoplot(cbind(EURUSD$Close, EurUsd))
#
# Si se quieren guardar los valores de la variable EurUsd para luego usarlos de nuevo ...
#
# write.zoo(EurUsd, "FIN_EURUSD.CSV", index.name = "Index")
#
# EurUsd <- read.zoo("FIN_EURUSD.CSV", header = FALSE, sep = " ", format = "%Y-%m-%d")
#
autoplot(EurUsd)
class(EurUsd)
#
rEurUsd <- 100 * diff(log(EurUsd))
statsNA(rEurUsd)
autoplot(rEurUsd)
summary(rEurUsd)
#
# Contraste de Engel de efectos ARCH
#
auto.arima(rEurUsd) # Para determinar el modelo estructural
S(rEurUsd_dyn <- dynlm(rEurUsd ~ L(rEurUsd,1:2)))
ArchTest(rEurUsd_dyn$residuals , lags = 1)
#
# Modelizaciónn GARCH 
#
# Librería fGarch
#
rEurUsd_ARMA_GARCH <- garchFit(~ arma(2,0) + garch(1,1), data = rEurUsd, cond.dist = "norm", trace = FALSE)
summary(rEurUsd_ARMA_GARCH)
plot(rEurUsd_ARMA_GARCH)
#
# Librería rugarch
#
rEurUsd_ARMA_GARCH_spec <-  ugarchspec(
  mean.model = list(armaOrder=c(2,0), include.mean = TRUE),
  variance.model = list(model="sGARCH", garchOrder=c(1,1)),
  distribution.model ="norm")
rEurUsd_ARMA_GARCH_fit <- ugarchfit(spec = rEurUsd_ARMA_GARCH_spec, data = rEurUsd)
show(rEurUsd_ARMA_GARCH_fit)
plot(rEurUsd_ARMA_GARCH_fit)
#