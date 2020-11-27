library("AER")
library("tseries")
library("ggplot2")
library("imputeTS")
library("fGarch")
#
EURUSD <- get.hist.quote("EURUSD=X", start = "2004-01-01", end = "2020-11-15")
plot(EURUSD)
autoplot(EURUSD$Close) + facet_free()
statsNA(EURUSD$Close)
ggplot_na_distribution(EURUSD$Close)
ggplot_na_intervals(EURUSD$Close)
EurUsd <-  na.approx(EURUSD$Close)
statsNA(EurUsd)
autoplot(cbind(EURUSD$Close, EurUsd)) + facet_free()
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
plot(rEurUsd)
# autoplot(rEurUsd) + facet_free()
summary(rEurUsd)
#
rEurUsd_GARCH <- garchFit(~ garch(1,1), data = rEurUsd, cond.dist = "norm", trace = FALSE)
summary(rEurUsd_GARCH)
plot(rEurUsd_GARCH)
#
rEurUsd_ARMA_GARCH <- garchFit(~ arma(1,0) + garch(1,1), data = rEurUsd, trace = FALSE)
summary(rEurUsd_ARMA_GARCH)
#

