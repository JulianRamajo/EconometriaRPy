library(readr)
CAPUB_USA <- read_csv("~/Documents/R/_ECOMET_code/CAPUB_USA.csv")
View(CAPUB_USA)
CAPUB_USA_ts <- ts(CAPUB_USA, start=c(1956), end = c(1989), frequency = 1)
plot(CAPUB_USA_ts)
#
Y <- CAPUB_USA_ts[,"Y"]
L <- CAPUB_USA_ts[,"L"]
K <- CAPUB_USA_ts[,"K"]
PK <- CAPUB_USA_ts[,"PK"]
trend <- seq(1:length(Y))
#
lm_cd <- lm(log(Y) ~ trend + log(L) + log(K) + log(PK))
summary(lm_cd)
# Matríz de correlaciones de las variables explicativas
# with(CAPUB_USA_ts, cor(cbind(L, K, PK, trend)))
var_exp <- data.frame(trend , log(L) , log(K) , log(PK))
cor(var_exp)
# 
vif(lm_cd)
vif(lm_cd) > 10 # problema de colinealidad
sqrt(vif(lm_cd)) > 2 # cota alternativa
#