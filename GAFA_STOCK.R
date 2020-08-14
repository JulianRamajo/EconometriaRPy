library(fpp3)
load("~/GAFA_STOCK.RData")
#
google.data <- GAFA_STOCK %>%
  filter(Symbol == "GOOG") %>%
  mutate(dclose = difference(Close))
#
google.data %>%
  autoplot(dclose) +
  labs(title = "Daily changes in the closing price for the stock ", subtitle = "Google") +
  xlab("Year")
#
