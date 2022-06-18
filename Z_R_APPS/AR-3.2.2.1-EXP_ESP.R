# Lectura de librerías
library(tidyverse)
library(car)
# Lectura de datos
EXP_ESP <- read_csv("EXP_ESP_Y.csv")
# Dvisión de la muestra preUE (1970-1985) y postUE (1986-1997)
Y1986=match(1986,EXP_ESP$obs)
Y1986
#
UE <- factor(c(rep(0, 16), rep(1, 12)), labels=c("preUE", "postUE"))
UE
class(UE)
#
EXP_ESP$D1986 <- as.numeric(UE)-1
EXP_ESP$D1986
class(EXP_ESP$D1986)
#
EXP_ESP_ts <- ts(EXP_ESP[,2:5], start=c(1970), end = c(1997), frequency = 1)
plot(EXP_ESP_ts)
#
# Ecuación de exportaciones (1970-1997)
#
lm_X_ESP <- lm(log(XGS) ~ log(WGDP) + log(REER), data = EXP_ESP_ts)
summary(lm_X_ESP)
#
scatterplot(log(XGS) ~ log(WGDP)| D1986, data=EXP_ESP_ts, smooth=FALSE, boxplots=FALSE, 
            ylab="Relación parcial Exportaciones/PIB mundial (logs)")
scatterplot(log(XGS) ~ log(REER)| D1986, data=EXP_ESP_ts, smooth=FALSE, boxplots=FALSE, 
            ylab="Relación parcial Exportaciones/Tipo de cambio (logs)")
#
# ¿Existe diferenciación por períodos?
#
# Test de Chow de cambio estructural
#
#  Cálculo manual
#
summary(lm_X_ESP)
SRCT <- sum(residuals(lm_X_ESP)^2)
SRCT
T <- nobs(lm_X_ESP)
T
K <- T -df.residual(lm_X_ESP)
K
# PreUE
preUE <- window(EXP_ESP_ts, start=1970, end = 1985)
lm_X_ESP_preUE <- lm(log(XGS) ~ log(WGDP) + log(REER) , data = preUE)
summary(lm_X_ESP_preUE)
SRC1 <- sum(residuals(lm_X_ESP_preUE)^2)
SRC1
T1 <- nobs(lm_X_ESP_preUE)
T1
# PostUE
postUE <- window(EXP_ESP_ts, start=1986, end = 1997)
lm_X_ESP_postUE <- lm(log(XGS) ~ log(WGDP) + log(REER) , data = postUE)
summary(lm_X_ESP_postUE)
SRC2 <- sum(residuals(lm_X_ESP_postUE)^2)
SRC2
T2 <- nobs(lm_X_ESP_postUE)
T2
#
CHOW=((SRCT-(SRC1+SRC2))/K)/((SRC1+SRC2)/(T-2*K))
CHOW
pval <-  1-pf(CHOW,K,(T-2*K))
pval
# Comparación de parámetros
compareCoefs(lm_X_ESP_preUE,lm_X_ESP_postUE)
#
# Cálculo automático
#
# Método 1 (ANOVA)
summary(lm_X_EXP_int <- lm(log(XGS) ~ log(WGDP) + log(REER) + D1986/(log(WGDP) + log(REER)), data = EXP_ESP_ts))
anova(lm_X_ESP, lm_X_EXP_int)
# Método 2 (librería structchange)
library(strucchange)
sctest(log(XGS) ~ log(WGDP) + log(REER), data=EXP_ESP_ts, type = "Chow", point = Y1986-1)
#
# Contrastes tipo Chow basados en estimaciones recursivas
# 
sbtest <- Fstats(log(XGS) ~ log(WGDP) + log(REER), data = EXP_ESP_ts, from = 0.15, to = 0.85)
sbtest[["Fstats"]]
# Gráfica de los estadísticos F 
plot(sbtest, alpha = 0.05)
# Gráfica de los correspondientes P-valores
plot(sbtest, pval = TRUE, alpha = 0.05)
# Test de Chow (1960) [versión Chi2]
Chow_F <- sbtest$Fstats[Y1986-4] # Punto de ruptura: 17 - 4 (15% suprimidos a la izquierda)
Chow_F # Se puede comprobar que Chow_F/K=CHOW
# Chow_F tiene una distribución asintótica Chi^2 mientras Chow_F/K tiene una distribución exacta F_K,T-2*K
pval <-  1-pchisq(Chow_F,sbtest$nreg) 
pval
#
# Contrastes de Andrews (1993) y Andrews y Ploberger (1994) [punto de ruptura desconocido]
#
sctest(sbtest, type = "supF")
sctest(sbtest, type = "aveF")
sctest(sbtest, type = "expF")
#
# Test CUSUM (Brown, Durbin y Evans, 1975)
plot(efp(log(XGS) ~ log(WGDP) + log(REER), data = EXP_ESP_ts))
#
# Regresión diferenciada por tramos
#
summary(lm(log(XGS) ~ (log(WGDP) + log(REER))*D1986, data=EXP_ESP_ts))
# Versión alternativa
summary(lm(log(XGS) ~ (log(WGDP) + log(REER))*UE, data=EXP_ESP_ts))
#
compareCoefs(lm_X_ESP_preUE,lm_X_ESP_postUE)
#