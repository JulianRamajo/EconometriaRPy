#
library(tidyverse)
#
CONS_ESP <- read_csv("CONS_ESP.csv")
class(CONS_ESP)
#
CONS_ESP_ts <- ts(CONS_ESP[,2:8], start=c(1995,1), end = c(2017,4), frequency = 4)
class(CONS_ESP_ts)
plot(CONS_ESP_ts)
#
# Modelo Keynesiano de consumo (MCE con separación de corto y largo plazo)
#
# Librería dynlm (https://cran.r-project.org/web/packages/dynlm/index.html)
#
library(dynlm)
#
modelo_lp_1 <- dynlm (log(PCR) ~ log(HDYR) + log(FWR) + log(NFWR) + log(HCOCR), data = CONS_ESP_ts)
summary(modelo_lp_1)
#
modelo_cp_1 <- dynlm (d(log(PCR)) ~ d(log(HDYR)) + d(log(CPE)) + d(ERX) + L(modelo_lp$residuals), data = CONS_ESP_ts)
summary(modelo_cp_1)
#
# Librería fpp3 (https://cran.r-project.org/web/packages/fpp3/index.html)
#
library(fpp3)
#
CONS_ESP$dl_PCR = c(NA,diff(log(CONS_ESP$PCR)))
CONS_ESP$dl_HDYR = c(NA,diff(log(CONS_ESP$HDYR)))
CONS_ESP$dl_FWR = c(NA,diff(log(CONS_ESP$FWR)))
CONS_ESP$dl_CPE = c(NA,diff(log(CONS_ESP$CPE)))
CONS_ESP$dl_NFWR = c(NA,diff(log(CONS_ESP$NFWR)))
CONS_ESP$dl_HCOCR = c(NA,diff(log(CONS_ESP$HCOCR)))
CONS_ESP$d_ERX = c(NA,diff(CONS_ESP$ERX))
#
CONS_ESP_tbl_ts <-  CONS_ESP %>%  mutate(YearQuarter = yearquarter(date)) %>% select(-date) %>% as_tsibble(index = YearQuarter)
class(CONS_ESP_tbl_ts)
#
CONS_ESP_tbl_ts %>%
  select(-dl_PCR, -dl_HDYR, -dl_FWR, -dl_CPE, -dl_NFWR, -dl_HCOCR, -d_ERX) %>%
  pivot_longer(-YearQuarter) %>%
  ggplot(aes(YearQuarter, value, color = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  guides(colour = "none") +
  labs(y="Index")
#
CONS_ESP_tbl_ts %>%
  GGally::ggpairs(columns = 1:7)
#
modelo_lp_2 <- CONS_ESP_tbl_ts %>% model(tslm = TSLM(log(PCR) ~ log(HDYR) + log(FWR) + log(NFWR) + log(HCOCR)))
report(modelo_lp_2 )
#
CONS_ESP_tbl_ts %>%
  select(-PCR, -HDYR, -FWR, -CPE, -NFWR, -HCOCR, -ERX) %>%
  pivot_longer(-YearQuarter) %>%
  ggplot(aes(YearQuarter, value, color = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  guides(colour = "none") +
  labs(y="Change")
#
CONS_ESP_tbl_ts %>%
  GGally::ggpairs(columns = 8:14)
#
modelo_cp_2 <- CONS_ESP_tbl_ts %>% left_join(residuals(modelo_lp_2), by = "YearQuarter") %>% 
  model(tslm = TSLM(dl_PCR ~ dl_HDYR + dl_CPE + d_ERX + lag(.resid)))
report(modelo_cp_2)
# Valores observados y estimados
augment(modelo_cp_2) %>%
  ggplot(aes(x = YearQuarter)) +
  geom_line(aes(y = dl_PCR, colour = "Observado")) +
  geom_line(aes(y = .fitted, colour = "Estimado")) +
  labs(y = NULL, title = "Cambio (tanto por uno) en el consumo privado en España") +
  scale_colour_manual(values=c(Observado="black",Estimado="#D55E00")) +
  guides(colour = guide_legend(title = NULL))
# Errores del modelo
modelo_cp_2 %>% gg_tsresiduals()
#