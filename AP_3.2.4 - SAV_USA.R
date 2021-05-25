library(tidyverse)
library(dynlm)
library(car)
library(lmtest)
library(performance)
#
SAV_USA <- read_csv("SAV_USA.csv")
AHORRO_RENTA_ts <- ts(SAV_USA[,2:3], start=c(1970), end = c(2005))
plot(AHORRO_RENTA_ts)
#
AH <- AHORRO_RENTA_ts[,"AH"]
Y <- AHORRO_RENTA_ts[,"Y"] 
#
# Modelo A
S(dynlm_A <- dynlm(AH ~ L(AH, 1:2)))
#
# Modelo B
#
S(dynlm_B <- dynlm(AH ~ L(AH, 1:1) + L(Y, 0:0)))
#
# Contraste de Cox
#
coxtest(dynlm_A, dynlm_B)
#
#  Contraste J de Davidson y MacKinnon
#
jtest(dynlm_A, dynlm_B)
#
# Modelo anidado
#
S(dynlm_AB <- dynlm(AH ~ L(AH, 1:2)  + L(Y, 0:0)))
#
# Comparación de modelos (librería performance)
#
model_performance(dynlm_A)
model_performance(dynlm_B)
compare_performance(dynlm_A, dynlm_B, rank = TRUE)
plot(compare_performance(dynlm_A, dynlm_B, rank = TRUE))
