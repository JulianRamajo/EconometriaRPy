library("AER")
library("tseries")
library("ggplot2")
library("imputeTS")
library("fGarch")
#
EURUSD <- get.hist.quote("EURUSD=X", start = "2004-01-01", end = "2020-06-30")
plot(EURUSD)
autoplot(EURUSD$Close) + facet_free()
EurUsd <-  na.approx(EURUSD$Close)
autoplot(cbind(EURUSD$Close, EurUsd)) + facet_free()
#
plot(EurUsd)
class(EurUsd)
plotNA.distribution(EurUsd)
plotNA.distributionBar(EurUsd, breaks = 20)
dim(EurUsd)
autoplot(cbind(EURUSD$Close, EurUsd)) + facet_free()
statsNA(EURUSD$Close)
statsNA(EurUsd)
rEurUsd <- 100 * diff(log(EurUsd))
statsNA(rEurUsd)
plot(rEurUsd)
summary(rEurUsd)
#
rEurUsd_GARCH <- garchFit(~ garch(1,1), data = rEurUsd, cond.dist = "norm", trace = FALSE)
summary(rEurUsd_GARCH)
plot(rEurUsd_GARCH)
#
rEurUsd_ARMA_GARCH <- garchFit(~ arma(1,0) + garch(1,1), data = rEurUsd, trace = FALSE)
summary(rEurUsd_ARMA_GARCH)
#
rEurUsd_ARMA_APARCH <- garchFit(~ arma(1,0) + aparch(1,1), data = rEurUsd, trace = FALSE)
summary(rEurUsd_ARMA_APARCH)
#

