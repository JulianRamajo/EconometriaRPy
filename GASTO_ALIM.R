View(GastoAlim)
attach(GastoAlim)
# Análisis exploratorio
summary(GastoAlim)
#
hist(log(GTotal), freq = FALSE)
lines(density(log(GTotal)), col = 4)
# Diagramas de puntos
plot(wAlim ~ as.factor(SexoPRHog))
plot(wAlim ~ TamHog)
plot(wAlim ~ TamMun)
plot(wAlim ~ EdadPRHog)
plot(wAlim ~log(GTotal))
# Regresión lineal
Engel_lm <- lm(wAlim ~ log(GTotal) + EdadPRHog + TamHog + TamMun + as.factor(SexoPRHog))
summary(Engel_lm)
par(mfrow = c(2, 2))
plot(Engel_lm)
par(mfrow = c(1, 1))


