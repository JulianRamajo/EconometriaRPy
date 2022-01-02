library(tidyverse)
DEM_CARNE <- read_csv("DEM_CARNE.csv")
DEM_CARNE
attach(DEM_CARNE)
#
# Método R estándar
#
# Gráficas
#
plot(Q ~ P, xlab="P", ylab="Q")
plot(Q ~ Y, xlab="Y", ylab="Q")
#
# Regresiones
#
lin_model <- lm(Q ~ P + Y)
summary(lin_model)
#
library(coefplot)
coefplot(lin_model)
#
log_model <- lm(log(Q) ~ log(P) + log(Y))
summary(log_model)
coefplot(log_model)
#
# Método R moderno (tidyverse) 
#
# Estadísticos descriptivos
#
library(skimr)
skim(DEM_CARNE)
#
# Gráficas
#
g1 <- ggplot(DEM_CARNE, aes(x=P, y=Q)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(x = "P", y = "Q", title = "Relación parcial Q - P")
g1
g2 <- ggplot(DEM_CARNE, aes(x=Y, y=Q)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(x = "Y", y = "Q", title = "Relación parcial Q - Y")
g2
#
library(moderndive)
# Correlaciones
#
DEM_CARNE %>% get_correlation(Q ~ P)
DEM_CARNE %>% get_correlation(Q ~ Y)
#
# Regresión lineal
#
modelo_lineal <- lm(Q ~ P + Y, data = DEM_CARNE)
#
# Tabla de resultados (regression table)
get_regression_table(modelo_lineal)
get_regression_points(modelo_lineal)
#
