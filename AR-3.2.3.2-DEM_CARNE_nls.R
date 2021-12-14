#
library(tidyverse)
library(alr4)
#
DEM_CARNE <- read_csv("DEM_CARNE.csv")
#
# Gráficas
#
plot(Q ~ P, xlab="P", ylab="Q")
plot(Q ~ Y, xlab="Y", ylab="Q")
#
library(scatterplot3d)
scatterplot3d(DEM_CARNE$P, DEM_CARNE$Y, DEM_CARNE$Q, main="Scatterplot 3D DEMANDA-PRECIO-RENTA")
#
# Regresión lineal
#
lin_mod <- lm(Q ~  P + Y, data=DEM_CARNE)
summary(lin_mod)
#
# Regresión no lineal
#
nonlin_mod <- nls(Q ~  c1 + c2*P + c3*(Y^c4), start = list(c1=5, c2=-0.5, c3=0.001, c4=3), data=DEM_CARNE)
summary(nonlin_mod)
#
# Bootstrapping del modelo no lineal
#
reg_nl.boot <- Boot(nonlin_mod, R=999)
summary(reg_nl.boot)
confint(reg_nl.boot)
hist(reg_nl.boot, layout=c(2, 2))
#