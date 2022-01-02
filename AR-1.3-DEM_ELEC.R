# DEMANDA DE ELECTRICICIDAD EN EL ESTADO DE VICTORIA, AUSTRALIA
# Datos cada media hora, 2012-2014
#
library(tidyverse)
library(fpp3)
#
load("DEM_ELEC.RData")
class(dem_elec_vict)
#
dem_elec_vict %>% 
  autoplot(Demanda) +
  labs(title = "Demanda de electricidad (cada media hora, en MW)", subtitle = "Victoria-Australia")
#
dem_elec_vict %>% gg_season(Demanda, period="day") + theme(legend.position = "none")
dem_elec_vict %>% gg_season(Demanda, period="week") + theme(legend.position = "none")
dem_elec_vict %>% gg_season(Demanda, period="year")
#
# Demanda de electricidad en función de la temperatura
# Demanda versus temperatura en el período 2012-2014
dem_elec_vict %>%
  pivot_longer(Demanda:Temperatura, names_to = "Series") %>%
  ggplot(aes(x = Time, y = value)) +
  geom_line() +
  facet_grid(rows = vars(Series), scales = "free_y") +
  labs(y = "")
# Demanda versus temperatura
dem_elec_vict %>%
  ggplot(aes(x = Temperatura, y = Demanda)) +
  geom_point() +
  ylab("Demanda (MWh)") + xlab("Temperatura (ºC)")
# Separación por festivos
dem_elec_vict %>%
  ggplot(aes(x = Temperatura, y = Demanda, col=Fiesta)) +
  geom_point() +
  ylab("Demanda (MWh)") + xlab("Temperatura (ºC)")
# Separación por días laborables
dem_elec_vict %>%
  ggplot(aes(x = Temperatura, y = Demanda, col=Dia_lab)) +
  geom_point() +
  ylab("Demanda (MWh)") + xlab("Temperatura (ºC)")
# Separación por día de la semana
dem_elec_vict %>%
  ggplot(aes(x = Temperatura, y = Demanda, col=Dia_sem)) +
  geom_point() +
  ylab("Demanda (MWh)") + xlab("Temperatura (ºC)")
#
# Consumo de electricidad en función de la temperatura (datos originales, cada media hora)
#
dem_elec <- dem_elec_vict %>%
  model(TSLM(log(Demanda) ~ Temperatura + I(Temperatura^2) + Dia_sem + Dia_lab + Frio + Calor)) %>%
  report()
#
# Predicción para un lunes laborable, suponiendo una temparatura media de 26ª
# aunque podrían usarse predicciones del servicio de meteorología)
Xs_1d <- new_data(dem_elec_vict, 1) %>%
    mutate(Temperatura = 26, Dia_sem = "lun" , Dia_lab = TRUE, Frio = FALSE, Calor = FALSE)
dem.elec <- lm(log(Demanda) ~ Temperatura + I(Temperatura^2) + Dia_sem + Dia_lab + Frio + Calor, data = dem_elec_vict)
summary(dem.elec)
pred_dem_IC <- predict(dem.elec, Xs_1d[,2:6], interval = "confidence", level = 0.95)
pred_dem_IC

