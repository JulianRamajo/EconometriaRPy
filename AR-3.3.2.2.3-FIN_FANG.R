library(tidyverse)
library(tidyquant)
library(timetk)
#
tq_index_options()
tq_index("SP500")
# Trabajar con los 5 primeras compañías del SP500
SP500_top5 <- tq_index("SP500") %>%
  slice(1:5) %>%
  tq_get(get = "stock.prices")
SP500_top5
SP500_top5 %>%  group_by(symbol) %>%
  plot_time_series(date, adjusted, .interactive = FALSE)
SP500_top5_prices <- SP500_top5 %>%  select(symbol, date, adjusted)
SP500_top5_spread <- SP500_top5_prices %>% 
  pivot_wider(names_from = symbol, values_from = adjusted)
#
library(TSstudio)
ts_plot(SP500_top5_spread, title = "Top 5 Stock Prices in SP500", Ytitle = "Index")
#
tq_exchange_options()
tq_exchange("NASDAQ")
tq_get_options()
# Acciones de Apple
AAPL_price  <- tq_get("AAPL", get = "stock.prices", from = " 1990-01-01")
AAPL_price
# librería timetk
AAPL_price %>%
  plot_time_series(date, adjusted, .interactive = FALSE)
# Índice de producción industrial (IPI) de España (desestacionalizado)
IPI_ESP <- tq_get("ESPPROINDMISMEI", get = "economic.data", from = " 1965-01-01")
IPI_ESP
# Tendencia
IPI_ESP %>%
  plot_time_series(date, price, .interactive = FALSE)
# Valores atípicos
IPI_ESP %>%
  plot_anomaly_diagnostics(date, price, .facet_ncol = 3, .interactive = FALSE)
# Estacionalidad
IPI_ESP %>%
  plot_seasonal_diagnostics(date, price, .interactive = FALSE)
# Acciones de Facebook, Amazon, Netflix y Google (FANG) 
# Si se añade Apple (AAPL) quedaría el grupo FANGA
FANG_prices <- tq_get(c("FB" , "AMZN", "NFLX", "GOOG"), get = "stock.prices", from = "2015-01-01")
FANG_prices
#
FANG_daily <- FANG_prices %>% group_by(symbol)
#
FANG_daily %>%
  ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line(size = 1) +
  labs(title = "FANG Daily Stock Prices",
       x = "", y = "Adjusted Prices", color = "") +
  facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
  scale_y_continuous(labels = scales::dollar) +
  theme_tq() + 
  scale_color_tq()
# Si se quiere una periodicidad superior
FANG_monthly <- FANG_prices %>%  # weekly
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = to.period, 
               period     = "months") #"weeks"
tq_transmute_fun_options() %>% str()
tq_mutate_fun_options()
# Rendimientos anuales o mensuales (cambiar yearly por monthly donde corresponda)
FANG_yearly_returns <- FANG_prices %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted, mutate_fun = periodReturn, period = "yearly", type = "arithmetic")
FANG_annual_returns
#
FANG_yearly_returns %>% ggplot(aes(x = date, y = yearly.returns, fill = symbol)) +
  geom_col() +
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "FANG Annual Returns", subtitle = "X", y = "Annual Returns", x = "") + 
  facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
  theme_tq() + 
  scale_fill_tq()
# Rendimientos diarios
FANG_daily_returns <- FANG_prices %>% group_by(symbol) %>% 
  tq_transmute(select = adjusted, mutate_fun = periodReturn, period = "daily", type  = "log")
FANG_daily_returns
#
FANG_daily_returns %>% ggplot(aes(x = date, y = daily.returns, fill = symbol)) + geom_col() +
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "FANG Daily Returns", subtitle = "(Log-returns)", y = "Daily Returns", x = "") + 
  facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
  theme_tq() + 
  scale_fill_tq()
#
FANG_daily_returns %>% ggplot(aes(x = daily.returns, fill = symbol)) +
  geom_density(alpha = 0.5) +
  labs(title = "FANG Daily LReturns",  subtitle = "(Log-returns)", x = "", y = "Density") +
  theme_tq() +
  scale_fill_tq() + 
  facet_wrap(~ symbol, ncol = 2)
# Librería timetk
FANG_daily_returns %>%
  group_by(symbol) %>%
  plot_anomaly_diagnostics(date, daily.returns, .facet_ncol = 3, .interactive = FALSE)
#
# Regresiones por parejas (Amazon versus Netflix; Google versus Facebook)
#
FANG_daily_returns %>%
  mutate(price.index = 100 * cumprod(1 + daily.returns)) %>%
  ggplot(aes(x = date, y = price.index, color = symbol)) +
  geom_line(size = 1) +
  labs(title = "FANG Stock Prices" , subtitle = "2015-01-02 = 100") +
  theme_tq() + 
  scale_color_tq()
#
FANG_daily_returns_spread <- FANG_daily %>% 
  tq_transmute(select = adjusted, mutate_fun = periodReturn, period = "daily", type = "log") %>% 
  pivot_wider(names_from = symbol, values_from = daily.returns)
#
FANG_daily_returns_spread %>%
  ggplot(aes(x = AMZN, y = NFLX)) +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(title = "Visualizing Returns Relationship of Stock Pairs: Netflix versus Amazon", x = "Amazon", y = "Netflix") +
  theme_tq()
# Reg. estándar
lm(NFLX ~ AMZN, data = FANG_daily_returns_spread) %>%
  summary()
# Reg. cambiante (rolling regression)
reg_fun <- function(data) {
  coef(lm(NFLX ~ AMZN, data = timetk::tk_tbl(data, silent = TRUE)))
}
coef_pairs <- FANG_daily_returns_spread %>%
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
