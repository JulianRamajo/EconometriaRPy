library(fpp3)
# Análisis gráfico
#
us_change %>%
  gather("var", "value", Consumption, Income) %>%
  ggplot(aes(x = Quarter, y = value)) +
  geom_line() +
  facet_grid(vars(var), scales = "free_y") +
  xlab("Year") + ylab(NULL) +
  ggtitle("Variación intertrimestral en el consumo y la renta")
#
us_change %>% 
  ggplot(aes(x = Quarter)) + geom_line(aes(y = Consumption, colour = "Consumo")) + 
  geom_line(aes(y = Income, colour = "Renta")) + ylab("% variación intertrimestral") + xlab("Año") + 
  guides(colour=guide_legend(title="Variables"))
# Análisis econométrico
us_change %>%
  GGally::ggpairs(columns = 2:3)
# 
# us_change %>% model(TSLM(Consumption ~ Income)) %>% report()
#
fit.CONSmod <- us_change %>%
  model(TSLM(Consumption ~ Income))
report(fit.CONSmod)
#
augment(fit.CONSmod) %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Consumption, colour = "Observados")) +
  geom_line(aes(y = .fitted, colour = "Estimados")) +
  xlab("Year") + ylab(NULL) +
  ggtitle("Porcentaje de cambio en el consumo de Estados Unidos ") +
  guides(colour=guide_legend(title=NULL))
# Evaluación del modelo
fit.CONSmod %>% gg_tsresiduals()
# Predicciones: Escenarios optimista y pesimista
up_future <- new_data(us_change, 4) %>%
  mutate(Income = 1)
down_future <- new_data(us_change, 4) %>%
  mutate(Income = -1)
fc_up <- forecast(fit.CONSmod, new_data = up_future) %>%
  mutate(Scenario = "Incremento renta 1%") %>%
  as_fable(response="Consumption", key = "Scenario")
fc_down <- forecast(fit.CONSmod, new_data = down_future) %>%
  mutate(Scenario = "Disminución renta 1%") %>%
  as_fable(response="Consumption", key = "Scenario")
us_change %>%
  autoplot(Consumption) +
  autolayer(bind_rows(fc_up, fc_down)) +
  ylab("% variación intertrimestral en el consumo")
# Escenarios promedio y extremos (optimista y adverso)
aver_future <- new_data(us_change, n = 4) %>%
  mutate(Income = mean(us_change$Income), Scenario = "Crecimiento medio")
fc_average <- forecast(fit.CONSmod, aver_future) %>%
  as_fable(response = "Consumption", key = "Scenario")
extup_future <- new_data(us_change, n = 4) %>%
  mutate(Income = 4, Scenario = "Incremento extremo")
fc_extup <- forecast(fit.CONSmod, extup_future) %>%
  as_fable(response = "Consumption", key = "Scenario")
extdown_future <- new_data(us_change, n = 4) %>%
  mutate(Income = -4, Scenario = "Disminución extrema")
fc_extdown <- forecast(fit.CONSmod, extdown_future) %>%
  as_fable(response = "Consumption", key = "Scenario")
us_change %>%
  autoplot(Consumption) +
  autolayer(bind_rows(fc_average, fc_extup, fc_extdown)) +
  ylab("% variación intertrimestral en el consumo")
#
# Corrección de la autocorrelación en los erroes
fit.CONSmod.2 <- us_change %>%
  model(ARIMA(Consumption ~ Income))
report(fit.CONSmod.2)
#
fit.CONSmod.2 %>% gg_tsresiduals()
augment(fit.CONSmod.2) %>%
  features(.resid, ljung_box, dof = 5, lag = 8)
#
