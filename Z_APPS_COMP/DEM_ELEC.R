# DEMANDA DE ELECTRICICIDAD
# Datos cada media hora, diarios
elec_half_hourly
elec_half_hourly %>%
  autoplot(Demand) +
  labs(title = "Demanda de electricidad (cada media hora)", subtitle = "Victoria-Australia")
#
elec_half_hourly %>% gg_season(Demand, period="day") + theme(legend.position = "none")
elec_half_hourly %>% gg_season(Demand, period="week") + theme(legend.position = "none")
elec_half_hourly %>% gg_season(Demand, period="year")
# Demanda de electricidad en función de la temperatura
elec_half_hourly %>%
  ggplot(aes(x = Temperature, y = Demand)) +
  geom_point() +
  ylab("Demanda (GW)") + xlab("Temperatura (ºC)")
#
fit.dem_elec.1 <- elec_half_hourly %>%
  model(TSLM(log(Demand) ~ Temperature + I(Temperature^2))) %>%
  report()
fit.dem_elec.1 %>% gg_tsresiduals()
#
# Consumo de electricidad en función de la temperatura (datos diarios)
#
elec_daily %>%
  gather("var", "value", Demand, Temperature) %>%
  ggplot(aes(x = Date, y = value)) + geom_line() + facet_grid(vars(var), scales = "free_y") +
  xlab("Year") + ylab(NULL) + ggtitle("Datos diarios")
#
elec_daily %>%
  ggplot(aes(x=Temperature, y=Demand, colour=Day_Type)) +
  geom_point() +
  ylab("Demanda (GW)") +
  xlab("Temperatura media (ºC)")
# Demanda de electricidad en función de la temperatura
fit.dem_elec.2 <- elec_daily %>%
  model(ARIMA(Demand ~ Temperature + I(Temperature^2) + (Day_Type=="Weekday")))
  report(fit.dem_elec.2)
  fit.dem_elec.2 %>% gg_tsresiduals()
# Predicción para las próximas dos semanas (suponiendo una temparatura media de 26ª,
# aunque podrían usarse predicciones del servicio de meteorología)
Xs_2W <- new_data(elec_daily, 14) %>%
    mutate(
      Temperature = 26,
      Holiday = c(TRUE, rep(FALSE, 13)),
      Day_Type = case_when(Holiday ~ "Holiday", wday(Date) %in% 2:6 ~ "Weekday", TRUE ~ "Weekend")
    )
forecast(fit.dem_elec.2, Xs_2W)
forecast(fit.dem_elec.2, Xs_2W) %>%
    autoplot(elec_daily) + ylab("Demanda electricidad (GW)")
#
