library(readr)
library(car)
library(MASS)
library(effects)
library(tidyverse)
library(gvlma)
library(tseries)
library(stats)
library(lmtest)
#
DEM_HET <- read_csv("DEM_HET.csv")
View(DEM_HET)
head(DEM_HET, n=10)
dim(DEM_HET)
summary(DEM_HET)
#
# Función de demdanda estándar
#
S(lm_dem <- lm(log(QA) ~ log(PA) + log(PB) + log(PC)+ log(Y), data = DEM_HET))
S(lm_dem, vcov.=hccm(lm_dem, type = "hc1")) # (Corrección de White: errores estándar robustos)
#
# Chequeo de varianza no constante: heteroscedasticidad
r2 <- resid(lm_dem)^2
yhat <- fitted(lm_dem)
l_Y <- log(DEM_HET$Y)
plot(l_Y,r2, xlab="Renta (en logaritmos)", ylab="r^2")
plot(yhat,r2, xlab="Valores estimados", ylab="r^2")
#
spreadLevelPlot(lm_dem)
#
ncvTest(lm_dem, ~ log(Y))  # Test de Breusch-Pagan (Score test)
ncvTest(lm_dem)
#
# Test de Breusch-Pagan (versión clasíca)
alpha <- 0.05
# Resgresión auxiliar:
S(lm_r2 <- lm(r2~ log(Y), data=DEM_HET))
N <- nobs(lm_r2)
p <- 1 
slm_r2 <- summary(lm_r2)
R2_lm_r2 <- slm_r2$r.squared
BP <- N*R2_lm_r2
# Contraste Chi-cuadrado 
chisqcr <- qchisq(1-alpha, p)
pval <- 1-pchisq(BP,p)
BP ; chisqcr
pval 
#
bptest(lm_dem) # Breusch-Pagan robusto (variante de Koenker)
bptest(lm_dem, varformula = ~ log(Y), data=DEM_HET)
bptest(lm_dem, studentize = FALSE) # Breusch-Pagan estándar (escalado)
bptest(lm_dem, studentize = FALSE, varformula = ~ log(Y), data=DEM_HET)
#
# Mínimos cuadrados ponderados (MCP)
# Regresión auxiliar para la varianza estimada:
S(lm_lr2 <- lm(log(r2)~ log(Y), data=DEM_HET))
s2 <- exp(fitted(lm_lr2))
S(lm_dem_het <- lm(log(QA) ~ log(PA) + log(PB) + log(PC)+ log(Y), weights = 1/s2, data = DEM_HET))


