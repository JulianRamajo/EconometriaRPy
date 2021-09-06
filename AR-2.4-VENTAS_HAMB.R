setwd("~/Documents/R/_ECOMET_code")
library(readr)
library(car)
library(MASS)
library(effects)
library(tidyverse)
VENT_HAMB <- read_csv("VENT_HAMB.csv")
VENT_HAMB_ts <- ts(VENT_HAMB, start=c(1), end = c(20), frequency = 1)
Q <- VENT_HAMB_ts[,"Q"]
P <- VENT_HAMB_ts[,"P"]
A <- VENT_HAMB_ts[,"A"]
plot(VENT_HAMB_ts)
#
scatterplot(Q ~ P, pch=".")
scatterplot(Q ~ A)
#
ggplot(VENT_HAMB, aes(x=P, y=Q)) + geom_point() + labs(title="Diagrama de puntos", x="Precio", y="Ventas")
ggplot(VENT_HAMB, aes(x=A, y=Q)) + geom_point() + labs(title="Diagrama de puntos", x="Publicidad", y="Ventas")
#
lm_VENTAS <- lm(Q ~ P + A)
summary(lm_VENTAS)
#
lm_VENTAS_poly <- lm(Q ~ P + poly(A,2, raw=TRUE))
# lm_VENTAS_poly <- lm(Q ~ P + A + I(A^2))
summary(lm_VENTAS_poly)
plot(Effect("P", lm_VENTAS_poly))
plot(Effect("A", lm_VENTAS_poly))
#
# Efectos marginales
#
b <- coef(lm_VENTAS_poly)
b
dQdP <- b[2]
dQdP
dQdA <- b[3] + 2*b[4]*A
dQdA
scatterplot(dQdA ~ A)
#
# Predicciones
new_P_A <- data.frame(P = c(5.25, 5.30, 5.40, 5.50 , 5.60), A = c(200, 210, 220, 230 , 240))
new_P_A
predict(lm_VENTAS_poly, new_P_A, interval = "prediction")

