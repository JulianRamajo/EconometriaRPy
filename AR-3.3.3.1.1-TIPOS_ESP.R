#
library(tidyverse)
library(dynlm)
library(car)
library(lmtest)
library(sandwich)
library(orcutt)
#
TIPOS_ESP <- read_csv("TIPOS_ESP.csv")
TIPOS_ESP_ts <- ts(TIPOS_ESP, start=c(1982,1), end = c(1990,3), frequency = 4)
plot(TIPOS_ESP_ts)
#
# Modelo de Klein-Monti (estático)
#
summary(lm_KM <- lm(RL ~ R3M + RD, data=TIPOS_ESP_ts))
#
summary(dynlm_KM_0 <- dynlm(RL ~ R3M + RD, data=TIPOS_ESP_ts)) # Especificaciónn ARDL(0,0,0) 
#
# Contrastes de correlación en los errores (autocorrelación)
#
resid <-residuals(dynlm_KM_0)
plot(resid)
abline(h=0, lty=2)
#
# Correlograma de los residuos
#
corr <- acf(resid)
corr$acf[2:10]
#
# Test de Durbin-Watson
#
dwtest(dynlm_KM_0, alternative = "two.sided")
dwtest(dynlm_KM_0, alternative = "greater")
#
# Test de Breusch-Godfrey
#
bgtest(dynlm_KM_0, order=1, type="Chisq", fill=0)
#
# Corrección de la autocorrelación: MCO corregidos, MCG-AR, modelos dinámicos del tipo ARDL
#
# MCO corregidos: errores estándar robustos, HAC (Newey-West)
#
summary(dynlm_KM_0 <- dynlm(RL ~ R3M + RD, data=TIPOS_ESP_ts), vcov.=vcovHAC(dynlm_KM_0))
#
# Mínimos cuadrados generalizados (MCG): errores AR(1)
#
cochrane.orcutt(dynlm_KM_0)
#
# Modelo dinámico ARDL(1,1,1) 
#
summary(dynlm_KM_1 <- dynlm(RL ~ L(RL, 1:1) + L(R3M, 0:1) + L(RD, 0:1), data=TIPOS_ESP_ts))
#
# Comparación de modelos
#
compareCoefs(dynlm_KM_0,dynlm_KM_1)
#
# Efectos estimados
#

# Efectos a corto y largo plazo
#
library(nlWaldTest)
# Modelo estático (c.p=l.p)
nlConfint(dynlm_KM_0, c("b[2]","b[3]"))
nlWaldtest(dynlm_KM_0, "b[2]")
nlWaldtest(dynlm_KM_0, "b[3]")
# Modelo dinámico
# Corto plazo
nlConfint(dynlm_KM_1, c("b[3]","b[5]"))
nlWaldtest(dynlm_KM_1, "b[3]")
nlWaldtest(dynlm_KM_1, "b[5]")
# Largo plazo
nlConfint(dynlm_KM_1, c("(b[3]+b[4])/(1-b[2])","(b[5]+b[6])/(1-b[2])"))
nlWaldtest(dynlm_KM_1, "(b[3]+b[4])/(1-b[2])")
nlWaldtest(dynlm_KM_1, "(b[5]+b[6])/(1-b[2])")
#