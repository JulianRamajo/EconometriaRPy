#
library(AER)
library(tseries)
library(dynlm)
library(ggplot2)
library(imputeTS)
library(fGarch)
library(FinTS)
#
EURUSD <- get.hist.quote("EURUSD=X", start = "2004-01-01", end = "2020-11-15")
plot(EURUSD)
autoplot(EURUSD$Close) + facet_free()
statsNA(EURUSD$Close)
ggplot_na_distribution(EURUSD$Close)
ggplot_na_intervals(EURUSD$Close)
EurUsd <-  na.approx(EURUSD$Close)
statsNA(EurUsd)
autoplot(cbind(EURUSD$Close, EurUsd))
#
# write.zoo(EurUsd, "FIN_EURUSD.CSV", index.name = "Index")
#
# EurUsd <- read.zoo("FIN_EURUSD.CSV", header = FALSE, sep = " ", format = "%Y-%m-%d")
#
plot(EurUsd)
# autoplot(EurUsd) + facet_free()
class(EurUsd)
#
rEurUsd <- 100 * diff(log(EurUsd))
statsNA(rEurUsd)
autoplot(rEurUsd)
summary(rEurUsd)
#
# Contraste de Engel de efectos ARCH
#
S(rEurUsd_dyn <- dynlm(rEurUsd ~ L(rEurUsd,1:2)))
ArchTest(rEurUsd_dyn$residuals , lags = 1)
#
# Modelización GARCH
#
rEurUsd_ARMA_GARCH <- garchFit(~ arma(2,0) + garch(1,1), data = rEurUsd, cond.dist = "norm", trace = FALSE)
summary(rEurUsd_ARMA_GARCH)
plot(rEurUsd_ARMA_GARCH)
#

