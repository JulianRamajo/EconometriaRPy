#
library(tidyverse)
library(dynlm)
library(car)
library(lmtest)
library(performance)
#
AHORRO_RENTA <- read_csv("SAV_USA.csv")
AHORRO_RENTA_ts <- ts(AHORRO_RENTA[,2:3], start=c(1970), end = c(2005))
plot(AHORRO_RENTA_ts)
#
# Modelo A
#
summary(dynlm_A <- dynlm(AH ~ L(AH, 1:2), data = AHORRO_RENTA_ts, start = c(1972,1), end = c(2005,1)))
#
# Modelo B
#
summary(dynlm_B <- dynlm(AH ~ L(Y,0:0) + L(AH, 1:1), data = AHORRO_RENTA_ts, start = c(1972,1), end = c(2005,1)))
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
summary(dynlm_B <- dynlm(AH ~ L(AH, 1:2) + L(Y,0:0), data = AHORRO_RENTA_ts, start = c(1972,1), end = c(2005,1)))
#
# Comparación de modelos (librería performance)
#
model_performance(dynlm_A)
model_performance(dynlm_B)
compare_performance(dynlm_A, dynlm_B, rank = TRUE)
plot(compare_performance(dynlm_A, dynlm_B, rank = TRUE))
#