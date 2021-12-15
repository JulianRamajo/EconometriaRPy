#
library(tidyverse)
CONS_ESP <- read_csv("CONS_ESP.csv")
#
ts_CONS_ESP <- ts(CONS_ESP[,2:8], start=c(1995), end = c(2017), frequency = 4)
plot(ts_CONS_ESP)
#
C <- ts_CONS_ESP[,"PCR"]
Y <- ts_CONS_ESP[,"HDYR"]
ts.plot(Y, C)
#
# Modelo Keynesiano de consumo
#
KEYNES_model <- lm (C ~ Y)
summary(KEYNES_model)
#
