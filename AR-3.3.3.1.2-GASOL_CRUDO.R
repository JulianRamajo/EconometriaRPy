#
library(tidyverse)
GASOL_CRUDO <- read_csv("GASOL_CRUDO.csv")
#
library(zoo)
GASOL_CRUDO_ts <- read.zoo(GASOL_CRUDO)
plot(GASOL_CRUDO_ts)
#
# Modelo dinámico para el precio de la gasolina (precio minorista)
#
library(dynlm)
dyn_model <- dynlm (log(PGASOL) ~ D2008JD + time + L(log(PGASOL), 1:2) + L(log(PCRUDO),0:2), data=GASOL_CRUDO_ts)
summary(dyn_model)
#
# Efectos a corto y largo plazo
#
library(nlWaldTest)
nlConfint(dyn_model, c("b[6]","(b[6]+b[7]+b[8])/(1-b[4]-b[5])"))
nlWaldtest(dyn_model, "b[6]")
nlWaldtest(dyn_model, "(b[6]+b[7]+b[8])/(1-b[4]-b[5])")
#
# Contraste de heteroscedasticidad autoregresiva condicional (efectos ARCH)
#
resid <- as.zoo(dyn_model$residuals)
plot(resid)
plot(resid^2)
#
library(FinTS)
summary(dynlm(I(dyn_model$residuals^2) ~ L(I(dyn_model$residuals^2), 1:1)))
ArchTest(dyn_model$residuals, lag = 1)
#
summary(dynlm(I(dyn_model$residuals^2) ~ L(I(dyn_model$residuals^2), 1:2)))
ArchTest(dyn_model$residuals, lag = 2)
#
# Modelo GARCH para los errores del modelo
#
library(tseries)
resid.ARCH <- garch(resid,c(0,2), trace=FALSE)
summary(resid.ARCH)
hhat_1 <- as.zoo(resid.ARCH$fitted.values[,1]^2)
plot(hhat_1)
#
library(rugarch)
resid_GARCH_spec <-  ugarchspec(
  mean.model = list(armaOrder=c(0,0), include.mean = FALSE),
  variance.model = list(model="sGARCH", garchOrder=c(2,0)),
  distribution.model ="norm")
resid_GARCH_fit <- ugarchfit(spec = resid_GARCH_spec, data = resid)
show(resid_GARCH_fit)
hhat_2 <- as.zoo(resid_GARCH_fit@fit$sigma^2)
plot.ts(hhat_2) # ts.plot(resid_GARCH_fit@fit$sigma^2)
# plot(resid_GARCH_fit) # Modo interactivo: permite seleccionar el tipo de gráfica
plot(resid_GARCH_fit, which=1)
plot(resid_GARCH_fit, which=3)
#