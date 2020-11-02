library(readr)
library(dynlm)
library(car)
library(lmtest)
#
AHORRO_RENTA <- read_csv("AHORRO_RENTA.csv")
AHORRO_RENTA_ts <- ts(AHORRO_RENTA[,2:3], start=c(1970), end = c(2005))
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
