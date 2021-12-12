# Lectura de librerías
library(tidyverse)
library(zoo)
library(quantmod)
library(dynlm)
library(rugarch)
# Obtener los datos usando la librería quantmod (probar con AAPL, GOOGL, MSFT, AMZN, FB, NFLX)
# Para buscar una empresa concreta X, utilizar en Google: ticker symbol X. Por ejemplo, el ticker
# de Apple Inc. es AAPL. También puede utilizarse la página https://finance.yahoo.com/
getSymbols("AAPL", auto.assign = TRUE)
# Si se quieren importar datos para un período concreto se puede usar el comando
# getSymbols("AAPL", from='2010-01-04',to='2021-12-10')
# Si se quiere restringir la serie temporal a un período concreto se puede usar el 
# indexado especial de objetos xts: AAPL <- AAPL["200X/20XX"]
# También se pueden restringir los datos a los últimos X años con la función last() de xts:
# AAPL5y <- last(AAPL,'5 years')
plot(AAPL$AAPL.Adjusted)
plot(diff(log(AAPL$AAPL.Adjusted)))
# También se puede usar la función chartSeries() de la librería quantmod
chartSeries(AAPL, type="line", theme=chartTheme('white'))
chartSeries(AAPL, type="line", subset='2021', theme=chartTheme('white'))
chartSeries(AAPL, type="line", subset='2021-06::2021-12', theme=chartTheme('white'))
# Para buscar varios a la vez:
# getSymbols(c('AAPL','GOOGL','MSFT'))
# getSymbols(c(AMZN','FB', 'NFLX'))
#
# Para extraer las columnas, se pueden usar las funciones Op, Hi, Lo, Cl, Vo y Ad:
OpenAAPL <- Op(AAPL)   #Open Price
HighAAPL <- Hi(AAPL)    # High price
LowAAPL <- Lo(AAPL)  # Low price
CloseAAPL <- Cl(AAPL)   #Close Price
VolumeAAPL <- Vo(AAPL)   #Volume
AdjCloseAAPL <- Ad(AAPL) # Adjusted close
#
# Cálculo del rendimiento mediante log-diferencias
rAAPL <- diff(log(AAPL$AAPL.Adjusted))
#
# Modelo AR(1) para los rendimientos
#
rAAPL <- as.zoo(rAAPL)
plot.zoo(rAAPL)
ARreg.AAPL <- dynlm( rAAPL ~ L(rAAPL) ) 
summary(ARreg.AAPL)
# Residuos
resid.AAPL <- resid(ARreg.AAPL)
plot.zoo(ARresid.AAPL)
# Residuos al cuadrado 
resid2.AAPL <- resid(ARreg.AAPL)^2
plot.zoo(resid2.AAPL)
#
# Modelo ARCH para los residuos al cuadrado del modelo
#
ARCHreg.AAPL <- dynlm(resid2.AAPL ~ L(resid2.AAPL,1:5)) 
summary(ARCHreg.AAPL)
#
# Modelo GARCH(1,1) para los residuos del modelo
#
resid.AAPL_GARCH_spec <-  ugarchspec(
  mean.model = list(armaOrder=c(0,0), include.mean = FALSE),
  variance.model = list(model="sGARCH", garchOrder=c(1,1)),
  distribution.model ="norm")
resid.AAPL_GARCH_fit <- ugarchfit(spec = resid.AAPL_GARCH_spec, data = resid.AAPL)
resid.AAPL_GARCH_fit
# Resultados específicos
coefficients <-coef(resid.AAPL_GARCH_fit)
volatility <- sigma(resid.AAPL_GARCH_fit)
long.run.variance <- uncvariance(resid.AAPL_GARCH_fit)
# Gráficas asociadas
hhat <- zoo(resid.AAPL_GARCH_fit@fit$sigma^2)
plot.zoo(hhat)
# plot(resid.AAPL_GARCH_fit) 
plot(resid.AAPL_GARCH_fit, which=1)
plot(resid.AAPL_GARCH_fit, which=3)
plot(resid.AAPL_GARCH_fit, which=8)
plot(resid.AAPL_GARCH_fit, which=9)
plot(resid.AAPL_GARCH_fit, which=10)
plot(resid.AAPL_GARCH_fit, which=11)
#
