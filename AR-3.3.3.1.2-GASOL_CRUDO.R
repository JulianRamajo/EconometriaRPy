#
library(tidyverse)
GASOL_CRUDO <- read_csv("GASOL_CRUDO.csv")
#
library(zoo)
zoo_GASOL_CRUDO <- read.zoo(GASOL_CRUDO)
plot(zoo_GASOL_CRUDO)
#
PCRUDO <- zoo_GASOL_CRUDO[,"PCRUDO"]
PGASOL <- zoo_GASOL_CRUDO[,"PGASOL"]
D2008JD <- zoo_GASOL_CRUDO[,"D2008JD"]
time <- zoo_GASOL_CRUDO[,"time"]
#
# Modelo dinámico para el precio de la gasolina (precio minorista)
#
library(dynlm)
DYN_model <- dynlm (log(PGASOL) ~ D2008JD + time + L(log(PGASOL), 1:2) + L(log(PCRUDO),0:2))
summary(DYN_model)
#
# Efectos a corto y largo plazo
#
library(nlWaldTest)
nlWaldtest(DYN_model, "b[6]")
nlWaldtest(DYN_model, "(b[6]+b[7]+b[8])/(1-b[4]-b[5])")
nlConfint(DYN_model, c("b[6]","(b[6]+b[7]+b[8])/(1-b[4]-b[5])"))
#
library(FinTS)
summary(dynlm(I(DYN_model$residuals^2) ~ L(I(DYN_model$residuals^2), 1:1)))
ArchTest(DYN_model$residuals, lag = 1)
#
summary(dynlm(I(DYN_model$residuals^2) ~ L(I(DYN_model$residuals^2), 1:2)))
ArchTest(DYN_model$residuals, lag = 2)
#
# Modelo GARCH para los errores del modelo
#
library(tseries)
resid_DYN <- as.zoo(DYN_model$residuals)
plot(resid_DYN)
resid.ARCH <- garch(resid_DYN,c(0,2), trace=FALSE)
summary(resid.ARCH)
hhat_1 <- as.zoo(resid.ARCH$fitted.values[,1]^2)
plot(hhat_1)
#
library(rugarch)
resid_GARCH_spec <-  ugarchspec(
  mean.model = list(armaOrder=c(0,0), include.mean = FALSE),
  variance.model = list(model="sGARCH", garchOrder=c(2,0)),
  distribution.model ="norm")
resid_GARCH_fit <- ugarchfit(spec = resid_GARCH_spec, data = resid_DYN)
show(resid_GARCH_fit)
hhat_2 <- as.zoo(resid_GARCH_fit@fit$sigma^2)
plot.ts(hhat_2) # ts.plot(Resid_GARCH_fit@fit$sigma^2)
plot(resid_GARCH_fit)
#
   