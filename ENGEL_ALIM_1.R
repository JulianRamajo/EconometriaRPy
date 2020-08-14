# Packages
library(readr)
library(stargazer)
library(ggplot2)
library(lmtest)
library(splines)
#
setwd("~/Documents/R/R_ECOMET_Ramajo")
engel <- read_delim("~/Documents/R/R_ECOMET_Ramajo/ENGEL_ALIM_1.csv", ";", escape_double = FALSE, trim_ws = TRUE)
# Estadísticos descriptivos
head(engel)
summary(engel)
dim(engel)
# Diagrama de puntos (scatter plot) de las variables x e y:
plot(engel$RENTA, engel$GALIM, 
     ylim=c(0, max(engel$GALIM)), 
     xlim=c(0, max(engel$RENTA)), 
     xlab="Renta", 
     ylab="Gasto en alimentos",
     type = "p")
# Estimación de un modelo por MCO -ols en inglés- y guardar los resultados en el objeto "ols":
ols <- lm(GALIM ~ RENTA, data = engel)
class(ols)
names(ols)
str(ols)
#
# Resumen de resultados:
summary(ols)
anova(ols)
#
coef(ols)
confint(ols)
# Gráficas de diagnóstico:
par(mfrow=c(2,2))
plot(ols)
par(mfrow=c(1,1))
#
ols$coefficients
(b1 <- coef(ols)[[1]])
(b2 <- coef(ols)[[2]])
vcov(ols)
(varb1 <- vcov(ols)[1, 1])
(varb2 <- vcov(ols)[2, 2])
(covb1b2 <- vcov(ols)[1,2])
#
# Resultados formateados:
stargazer(ols, type = "text", title = "Resultados de la regresión")
# Diagrama de puntos y línea de regresión:
# Método estándar
#
plot(engel$RENTA, engel$GALIM,
     ylim=c(0, max(engel$GALIM)),
     xlim=c(0, max(engel$RENTA)),
     xlab="Renta",
     ylab="Gasto en alimentos",
     type = "p")
abline(ols)
 # También podría usarse:
# b1 <- coef(ols)[[1]]
# b2 <- coef(ols)[[2]]
# abline(b1,b2)
# Método ggplot
#
ggplot(engel, aes(x = RENTA, y = GALIM)) + geom_point() + geom_smooth(method = "lm", se = FALSE) 
+ scale_x_continuous(limits = c(350, 5000), expand = c(0, 0)) + theme_bw() + labs(x = "Renta", y = "Gasto en alimentos")
#
# Predicción con el modelo de regresión simple:
new_RENTA <- data.frame(RENTA=c(400, 2000, 4500))
pred_GALIM <- predict(ols, new_RENTA)
names(pred_GALIM) <-c("Renta = 400", "2000", "4500")
pred_GALIM
#
waldtest(ols, . ~ . + I(RENTA^2))
ols2 <- lm(GALIM ~ RENTA + I(RENTA^2), data = engel)
summary(ols2)
# Modelo semiparamétrico
plm <- lm(GALIM ~ bs(RENTA, df = 5) , data = engel)
summary(plm)
# Elección de df
# bs <- lapply(3:10, function(i) lm(GALIM ~ bs(RENTA, df = i) , data = engel))
# structure(sapply(bs, AIC, k = log(nrow(engel))), .Names = 3:10)
#
par(mar = c(5, 5, 2, 4))
engel_sim <- data.frame(RENTA = 350:5000)
engel_sim$GALIMhat1 <- predict(ols2, newdata = engel_sim)
engel_sim$GALIMhat2 <- predict(plm, newdata = engel_sim)

plot(GALIM ~ jitter(RENTA, factor = 3), pch = 19, cex = 1.5, col = rgb(0.5, 0.5, 0.5, alpha = 0.02), data = engel)
lines(GALIMhat1 ~ RENTA, data = engel_sim, lty = 2)
lines(GALIMhat2 ~ RENTA, data = engel_sim)
legend("topleft", c("Regresión cuadrática", "Regresión spline"),
       lty = c(2, 1), bty = "n")
#