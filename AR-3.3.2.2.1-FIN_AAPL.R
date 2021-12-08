library(zoo);library(quantmod);library(dynlm); library(rugarch)

# Obtener los datos usando la librería quantmod:
getSymbols("AAPL", auto.assign = TRUE)

# Cálculo del rendimiento mediante log-idrerencias:
rAAPL <- diff( log(AAPL$AAPL.Adjusted) )
# Si se quiere restringir la serie temporal se puede usar el indezado especial de objetos xts:
# rAAPL <- rAAPL["200X/20XX"]

# Modelo AR(1) para los rendimientos
rAAPL <- as.zoo(rAAPL)
plot.zoo(rAAPL)
reg.AAPL <- dynlm( rAAPL ~ L(rAAPL) ) 
summary(reg.AAPL)

# Residuos
resid.AAPL <- resid(reg.AAPL)
plot.zoo(resid.AAPL)

# Residuos al cuadrado 
residsq.AAPL <- resid(reg.AAPL)^2
plot.zoo(residsq.AAPL)

# Modelo ARCH para los residuos al cuadrado del modelo
ARCHreg.AAPL <- dynlm(residsq.AAPL ~ L(residsq.AAPL,1:5)) 
summary(ARCHreg.AAPL)

# Modelo GARCH(1,1) para los residuos del modelo
resid.AAPL_GARCH_spec <-  ugarchspec(
  mean.model = list(armaOrder=c(0,0), include.mean = FALSE),
  variance.model = list(model="sGARCH", garchOrder=c(1,1)),
  distribution.model ="norm")
resid.AAPL_GARCH_fit <- ugarchfit(spec = resid.AAPL_GARCH_spec, data = resid.AAPL)
show(resid.AAPL_GARCH_fit)
hhat <- ts(resid.AAPL_GARCH_fit@fit$sigma^2)
plot.ts(hhat)
plot(resid.AAPL_GARCH_fit)
#
