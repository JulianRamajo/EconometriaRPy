library(readr)
library(car)
library(strucchange)
#
EXP_ESP <- read_csv("EXP_ESP.csv")
EXP_ESP_ts <- ts(EXP_ESP, start=c(1970), end = c(1997), frequency = 1)
plot(EXP_ESP_ts[,2:5])
#
XGS <- EXP_ESP_ts[,"XGS"]
WGDP <- EXP_ESP_ts[,"WGDP"]
REER <- EXP_ESP_ts[,"REER"]
DUE <- EXP_ESP_ts[,"DUE"]
#
scatterplot(XGS ~ WGDP| DUE, data=EXP_ESP, smooth=FALSE, boxplots=FALSE, ylab="Relación parcial Exportaciones/PIB mundial")
scatterplot(XGS ~ REER| DUE, data=EXP_ESP, smooth=FALSE, boxplots=FALSE, ylab="Relación parcial Exportaciones/Tipo de cambio")
#
#
# Ecuación de exportaciones (1970-1997)
#
lm_X_ESP <- lm(log(XGS) ~ log(WGDP) + log(REER))
S(lm_X_ESP)
#
# Test de Chow de cambio estructural
#
SRCT <- sum(residuals(lm_X_ESP)^2)
SRCT
T <- nobs(lm_X_ESP)
T
K <- T -df.residual(lm_X_ESP)
K
# Pre UE
nUE_dat <- window(EXP_ESP_ts, start=1970, end = 1985)
lm_X_ESP_nUE <- lm(log(XGS) ~ log(WGDP) + log(REER) , data = nUE_dat)
S(lm_X_ESP_nUE)
SRC1 <- sum(residuals(lm_X_ESP_nUE)^2)
SRC1
T1 <- nobs(lm_X_ESP_nUE)
T1
# Post UE
UE_dat <- window(EXP_ESP_ts, start=1986, end = 1997)
lm_X_ESP_UE <- lm(log(XGS) ~ log(WGDP) + log(REER) , data = UE_dat)
S(lm_X_ESP_UE)
SRC2 <- sum(residuals(lm_X_ESP_UE)^2)
SRC2
T2 <- nobs(lm_X_ESP_UE)
T2
# Comparación de parámetros
compareCoefs(lm_X_ESP_nUE,lm_X_ESP_UE)
# Cálculo manual del test de Chow
CHOW=((SRCT-(SRC1+SRC2))/K)/((SRC1+SRC2)/(T-2*K))
CHOW
pval <-  1-pf(CHOW,3,(28-2*3))
pval
#
# Cálculo automático del test de Chow
#
sctest(log(XGS) ~ log(WGDP) + log(REER), data=EXP_ESP_ts, type = "Chow", point = T1)
#
# Test de Chow recursivo
# 
fs <- Fstats(lm(log(XGS) ~ log(WGDP) + log(REER)), data=EXP_ESP_ts, from = 0.15, to = 0.85)
# Gráfica de los estadísticos F 
plot(fs, alpha = 0.05)
# Gráfica de los correspondientes P-valores
plot(fs, pval = TRUE, alpha = 0.05)
#
# Regresión diferenciada por tramos
#
lm_X_ESP_2 <- lm(log(XGS) ~ (log(WGDP) + log(REER))*DUE)
S(lm_X_ESP_2)
# Versión alternativa
UE <- factor(DUE, labels=c("nUE", "UE"))
lm_X_ESP_3 <- lm(log(XGS) ~ (log(WGDP) + log(REER))*UE)
S(lm_X_ESP_3)
#
compareCoefs(lm_X_ESP, lm_X_ESP_2)