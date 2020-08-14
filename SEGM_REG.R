library(alr4)
plot(C ~ Temp, segreg, xlab="Temperatura media del mes (ºF)", ylab="Consumo de electricidad (KWH/día")
#
segm.reg <- nls(C ~ th0 + th1 * (pmax(0, Temp - gamma)), data=segreg, start=list(th0=70, th1=.5, gamma=40))
summary(segm.reg)
#
plot(C ~ Temp, segreg, xlab="Temperatura media (ºF)", ylab="Consumo de electricidad (KWH/día)")
x <- (0:90)
lines(x, predict(segm.reg, data.frame(Temp=x)))
#
segm.reg.boot <- Boot(segm.reg, R=999)
summary(segm.reg.boot)
confint(segm.reg.boot)
hist(segm.reg.boot, layout=c(1, 3))