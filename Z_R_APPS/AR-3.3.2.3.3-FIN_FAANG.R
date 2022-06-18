#
library(tidyverse)
library(tidyquant)
library(timetk)
library(TSstudio)
#
# En este apartado vamos a trabajar con las 4 primeras compañías del SP500
# 
tq_index_options()
SP500 <- tq_index("SP500")
SP500_top4 <- tq_index("SP500") %>%
  slice(1:4) %>%
  tq_get(get = "stock.prices")
SP500_top4
SP500_top4 %>%  group_by(symbol) %>%
  plot_time_series(date, adjusted, .interactive = FALSE)
SP500_top4_prices <- SP500_top4 %>% select(symbol, date, adjusted)
# Agrupación de series temporales de precios
SP500_top4_ts <- SP500_top4_prices %>% pivot_wider(names_from = symbol, values_from = adjusted)
#
ts_plot(SP500_top4_ts, title = "Top 4 Stock Prices in SP500", Ytitle = "Index") # librería TSstudio
#
# Ahora vamos a trabajar con una compañía del NASDAQ, Tesla (TSLA), o con el Índice de Producción
# Industrial (IPI) de España
tq_exchange_options()
NASDAQ <-  tq_exchange("NASDAQ")
tq_get_options()
# Acciones de Tesla
TSLA_price  <- tq_get("TSLA", get = "stock.prices")
TSLA_price
TSLA_price %>% plot_time_series(date, adjusted, .interactive = FALSE) # librería timetk
# Índice de producción industrial (IPI) de España (desestacionalizado)
IPI_ESP <- tq_get("ESPPROINDMISMEI", get = "economic.data", from = " 1965-01-01")
IPI_ESP
# Tendencia
IPI_ESP %>% plot_time_series(date, price, .interactive = FALSE) 
# Valores atípicos
IPI_ESP %>% plot_anomaly_diagnostics(date, price, .facet_ncol = 3, .interactive = FALSE)
# Estacionalidad
IPI_ESP %>% plot_seasonal_diagnostics(date, price, .interactive = FALSE)
#
# Modelización de la acciones de Facebook, Apple, Amazon, Netflix y Google (FAANG Stocks) 
#
FAANG_prices <- tq_get(c("FB", "AAPL", "AMZN", "NFLX", "GOOG"), get = "stock.prices", from = "2015-01-01")
FAANG_prices
#
FAANG_daily <- FAANG_prices %>% group_by(symbol)
#
FAANG_daily %>%
  ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line(size = 1) +
  labs(title = "FAANG Daily Stock Prices",
       x = "", y = "Adjusted Prices", color = "") +
  facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
  scale_y_continuous(labels = scales::dollar) +
  theme_tq() + 
  scale_color_tq()
# Si se quiere una periodicidad mensual
FAANG_monthly <- FAANG_prices %>%   
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = to.period, 
               period     = "months") # se puede usar también la opción "weeks"
# Opciones disponibles
tq_transmute_fun_options() %>% str()
tq_mutate_fun_options()
# Rendimientos anuales o mensuales (cambiar yearly por monthly donde corresponda)
FAANG_yearly_returns <- FAANG_prices %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted, mutate_fun = periodReturn, period = "yearly", type = "arithmetic")
FAANG_yearly_returns
#
FAANG_yearly_returns %>% ggplot(aes(x = date, y = yearly.returns, fill = symbol)) +
  geom_col() +
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "FAANG Annual Returns", subtitle = "X", y = "Annual Returns", x = "") + 
  facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
  theme_tq() + 
  scale_fill_tq()
# Rendimientos diarios
FAANG_daily_returns <- FAANG_prices %>% group_by(symbol) %>% 
  tq_transmute(select = adjusted, mutate_fun = periodReturn, period = "daily", type  = "log")
FAANG_daily_returns
#
FAANG_daily_returns %>% ggplot(aes(x = date, y = daily.returns, fill = symbol)) + geom_col() +
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "FAANG Daily Returns", subtitle = "(Log-returns)", y = "Daily Returns", x = "") + 
  facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
  theme_tq() + 
  scale_fill_tq()
#
FAANG_daily_returns %>% ggplot(aes(x = daily.returns, fill = symbol)) +
  geom_density(alpha = 0.5) +
  labs(title = "FAANG Daily LReturns",  subtitle = "(Log-returns)", x = "", y = "Density") +
  theme_tq() +
  scale_fill_tq() + 
  facet_wrap(~ symbol, ncol = 2)
# Librería timetk
FAANG_daily_returns %>%
  group_by(symbol) %>%
  plot_anomaly_diagnostics(date, daily.returns, .facet_ncol = 3, .interactive = FALSE)
#
# Regresiones por parejas (ejmplo: Amazon versus Netflix; Google versus Facebook)
#
FAANG_daily_returns %>%
  mutate(price.index = 100 * cumprod(1 + daily.returns)) %>%
  ggplot(aes(x = date, y = price.index, color = symbol)) +
  geom_line(size = 1) +
  labs(title = "FAANG Stock Prices" , subtitle = "2015-01-02 = 100") +
  theme_tq() + 
  scale_color_tq()
#
FAANG_daily_returns_spread <- FAANG_daily %>% 
  tq_transmute(select = adjusted, mutate_fun = periodReturn, period = "daily", type = "log") %>% 
  pivot_wider(names_from = symbol, values_from = daily.returns)
#
FAANG_daily_returns_spread %>%
  ggplot(aes(x = AMZN, y = NFLX)) +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(title = "Visualizing Returns Relationship of Stock Pairs: Netflix versus Amazon", x = "Amazon", y = "Netflix") +
  theme_tq()
# Reg. estándar
lm(NFLX ~ AMZN, data = FAANG_daily_returns_spread) %>%
  summary()
# Reg. cambiante (rolling regression)
reg_fun <- function(data) {
  coef(lm(NFLX ~ AMZN, data = timetk::tk_tbl(data, silent = TRUE)))
}
coef_pairs <- FAANG_daily_returns_spread %>%
  tq_mutate(mutate_fun = rollapply,
            width      = 90,
            FUN        = reg_fun,
            by.column  = FALSE,
            col_rename = c("coef.0", "coef.1"))
coef_pairs
coef_pairs %>%
  ggplot(aes(x = date, y = coef.1)) +
  geom_line(size = 1, color = palette_light()[[1]]) +
  geom_hline(yintercept = 0.7342, size = 1, color = palette_light()[[2]]) +
  labs(title = "FB ~ AMZN: Visualizing Rolling Regression Slope Coefficient", x = "") +
  theme_tq()
#
