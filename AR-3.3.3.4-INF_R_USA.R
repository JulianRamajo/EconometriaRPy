library(readr)
INF_R_USA <- read_csv("INF_R_USA.csv")
INF_R_USA.ts <- ts(INF_R_USA[,2:4], start=c(1954,8), end=c(2016,4),frequency=12)
#
par(mfrow = c(3, 1))
ts.plot(INF_R_USA.ts[,"inf"] , xlab="Inflación", ylab="")
ts.plot(INF_R_USA.ts[,"ffr"] , xlab="Tipo de interés de los Fondos Federales", ylab="")
ts.plot(INF_R_USA.ts[,"br"] , xlab="Tipo de interés de los Bonos del Tesoro a tres años", ylab="")
#
# Modelo de corrección del eror
#
# Primera etapa: ecuación de largo plazo
#
library(dynlm)
summary(reg.lp <- dynlm(br~ffr, data=INF_R_USA.ts))
u.lp <- resid(reg.lp)
plot(u.lp)
#
# Segunda etapa: ecuación de corto plazo
#
summary(reg.cp <- dynlm(d(br)~L(u.lp) + d(ffr), data=INF_R_USA.ts))
#
