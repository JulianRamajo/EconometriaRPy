library(readr)
library(alr4)
#
DEM_CARNE <- read_csv("DEM_CARNE.csv")
DEM_CARNE
attach(DEM_CARNE)
#
#
# Gráficas
#
plot(Q ~ P, xlab="P", ylab="Q")
plot(Q ~ Y, xlab="Y", ylab="Q")
scatter3D(x = P, y = Y, z = Q, 
          pch = 16, cex = 1.5, xlab = "Precio", ylab = "Renta",
          zlab = "Gasto  per cápita", clab = c("Renta"),
          main = "Demanda familiar de carne", ticktype = "detailed")
#
# Regresión no lineal
#
nonlin_mod <- nls(Q ~  c1 + c2*P + c3*(Y^c4), start = list(c1=5, c2=-0.5, c3=0.001, c4=3))
summary(nonlin_mod)
#
#
set.seed(10131985)
reg_nl.boot <- Boot(nonlin_mod, R=999)
summary(reg_nl.boot)
confint(reg_nl.boot)
hist(reg_nl.boot, layout=c(2, 2))
#