#
library(tidyverse)
CONS_USA <- read_csv("CONS_USA.csv")
#
# Método R tradicional
#
ts_CONS_USA <- ts(CONS_USA[,2:3], start=c(1959), end = c(2015))
plot(ts_CONS_USA)
#
C <- ts_CONS_USA[,"C"]
Y <- ts_CONS_USA[,"Y"]
plot(Y, C, type = "p",)
#
# Modelo Keynesiano de consumo
#
KEYNES_model <- lm (C ~ Y)
summary(KEYNES_model)
plot(KEYNES_model)
#
# Método R moderno: tidyverse
#
library(fpp3) # Este package contiene varios elementos
CONS_USA <- readr::read_csv("CONS_USA.csv")
# Creación del objeto tsibble
CY_USA <-  CONS_USA[,2:3] %>%
  mutate(Year = 1959:2015) %>%
  as_tsibble(index = Year)
#
CY_USA %>% autoplot(C) +
  ggtitle("Consumo privado en Estados Unidos") +
  ylab("$ million") + xlab("Year")
#
CY_USA %>% autoplot(Y) +
  ggtitle("Renta disponible en Estados Unidos") +
  ylab("$ million") + xlab("Year")
#
CY_USA %>% autoplot(vars(C,Y)) +
  ggtitle("Consumo privado y renta disponible en Estados Unidos") +
  ylab("$ million") + xlab("Year")
#
# Estimación de una función Keynesiana de consumo para Estados Unidos (1959-2015)
#
# Gráfica de las dos series temporales conjuntamente
#
CY_USA %>%
  ggplot(aes(x = Year)) +
  geom_line(aes(y = C, colour = "Consumo")) +
  geom_line(aes(y = Y, colour = "Renta")) +
  ylab("$ million") + xlab("Year") +
  guides(colour=guide_legend(title="Variables"))
#
# Para estudiar la relación entre las dos elaboramos el siguiente diagrama de puntos (scatterplot)
#
CY_USA %>%
  ggplot(aes(x = Y, y = C)) +
  geom_point() +
  ylab("Consumo privado ($ million)") + xlab("Renta disponible ($ million)")
#
# Diagrama de puntos junto con la recta de regresión estimada
#
CY_USA %>%
  ggplot(aes(x=Y, y=C)) +
  ylab("Consumo") +
  xlab("Renta") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
#
# Regresión MCO
#
CY_USA %>%
  model(tslm = TSLM(C ~ Y)) %>%
  report()
#
# O también: 
#
KEYNES_model_2 <-  CY_USA %>% model(tslm = TSLM(C ~ Y))
report(KEYNES_model_2)
#
# Ajuste del modelo
#
augment(KEYNES_model_2) %>%
  ggplot(aes(x = Year)) +
  geom_line(aes(y = C, colour = "Valores observados")) +
  geom_line(aes(y = .fitted, colour = "Valores estimados")) +
  xlab("Year") + ylab(NULL) +
  ggtitle("Ajuste del modelo Keynesiano de consumo") +
  guides(colour=guide_legend(title=NULL))
#
# Estadísticos de ajuste
#
glance(KEYNES_model_2) %>% select(r_squared, adj_r_squared, CV, AIC, AICc, BIC)
# 
# Evaluación del modelo estimado
#
# Autocorrelación y normalidad de los errores (a través de los residuos estimados)
#
KEYNES_model_2 %>% gg_tsresiduals()
#
augment(KEYNES_model_2) %>% features(.resid, ljung_box, lag = 10, dof = 2)
#
# Residuos frente a predictores (no lienalidad o mala especificación funcional)
#
tmp <- left_join(CY_USA, residuals(KEYNES_model_2), by = "Year")
ggplot(tmp, aes(x=Y, y=.resid)) +
  geom_point() + ylab("Residuals")
#
# Residuos frente a valores estimados (heteroscedasticidad)
#
augment(KEYNES_model_2) %>%
  ggplot(aes(x=.fitted, y=.resid)) +
  geom_point() +
  labs(x = "Fitted", y = "Residuals")
#
# No estacionariedad de las variables (raíces unitarias) -> Modelo con tendencia determinística
#
# CY_USA %>%
#  model( TSLM(C ~ trend() + Y) ) %>%
#  report()
# Modelo logarítmico
#
# CY_USA %>%
#  model(tslm = TSLM(log(C) ~ log(Y))) %>%
#  report()
#
# Predicciones
#
KEYNES_model_2 <- CY_USA %>%
  model(lm = TSLM(C ~ Y))

new_C <- new_data(CY_USA, n = 4) %>%
  mutate(Y = c(12500,12600,12700,12800), Scenario = "Incremento")
fcast_up <- forecast(fit.KEYNES_model, new_C) %>%
  as_fable(response = "C", key = "Scenario")
new_C <- new_data(CY_USA, n = 4) %>%
  mutate(Y = c(12000,11900,11800,11700), Scenario = "Disminución")
fcast_down <- forecast(fit.KEYNES_model, new_C) %>%
  as_fable(response = "C", key = "Scenario")

CY_USA %>%
  autoplot(C) +
  autolayer(bind_rows(fcast_up, fcast_down)) +
  ylab("Consumo privado ($ million)")
#
