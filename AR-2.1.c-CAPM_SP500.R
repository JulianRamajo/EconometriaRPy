#
library(tidyverse)
library(tidyquant)
library(timetk)
#
tq_index_options()
SP500 <- tq_index("SP500")
SP500
#
Ra_prices <- c("AAPL", "GOOG", "NFLX") %>% tq_get(get  = "stock.prices", from = "2010-01-01", to   = "2020-12-20")
Ra_prices %>%
  ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line(size = 1) +
  labs(title = "Daily Stock Prices of Apple, Google and Netflix",
       x = "", y = "Adjusted Prices", color = "") +
  facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
  scale_y_continuous(labels = scales::dollar) +
  theme_tq() + 
  scale_color_tq()
Ra <- Ra_prices %>% group_by(symbol) %>%
  tq_transmute(select = adjusted, mutate_fun = periodReturn,period = "monthly", col_rename = "Ra")
Ra
Ra %>% ggplot(aes(x = date, y = Ra, fill = symbol)) + geom_col() +
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Monthly Returns of Apple, Google and Netflix", subtitle = "", y = "Monthly Returns", x = "") + 
  facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
  theme_tq() + 
  scale_fill_tq()
Ra %>%
  tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.Stats)
#
Rb_price <- "XLK" %>% tq_get(get  = "stock.prices", from = "2010-01-01", to = "2020-12-20")
Rb_price %>%
  ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line(size = 1) +
  labs(title = "Daily Stock Prices of SPDR Technology Exchange Traded Fund (XLK)",
       x = "", y = "Adjusted Prices", color = "") +
  facet_wrap(~ symbol, ncol = 2, scales = "free_y") +
  scale_y_continuous(labels = scales::dollar) +
  theme_tq() + 
  scale_color_tq()
Rb <- Rb_price %>%
  tq_transmute(select = adjusted, mutate_fun = periodReturn, period  = "monthly", col_rename = "Rb")
Rb
Rb %>% ggplot(aes(x = date, y = Rb)) + geom_col() +
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Monthly Returns of XLK", subtitle = "", y = "Monthly Returns", x = "") + 
  theme_tq() + 
  scale_fill_tq()
Rb %>%
  tq_performance(Ra = Rb, Rb = NULL, performance_fun = table.Stats)
#
RaRb <- left_join(Ra, Rb, by = c("date" = "date"))
RaRb
# CAPM automático
RaRb_CAPM <- RaRb %>%
  tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM)
RaRb_CAPM
RaRb_CAPM %>% select(symbol, Alpha, Beta)
# CAPM manual
library(FinTS)
#
CAPM_AAPL <- lm(Ra ~ Rb, data=subset(RaRb, symbol %in%  grep("AAPL", symbol, value = TRUE)))
summary(CAPM_AAPL)
ArchTest(CAPM_AAPL$residuals , lags = 1)
#
CAPM_GOOG <- lm(Ra ~ Rb, data=subset(RaRb, symbol %in%  grep("GOOG", symbol, value = TRUE)))
summary(CAPM_GOOG)
ArchTest(CAPM_GOOG$residuals , lags = 1)
#
CAPM_NFLX <- lm(Ra ~ Rb, data=subset(RaRb, symbol %in%  grep("NFLX", symbol, value = TRUE)))
summary(CAPM_NFLX)
ArchTest(CAPM_NFLX$residuals , lags = 1)
#