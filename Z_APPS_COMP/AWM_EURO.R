library(fpp3)
euro.area.data <- readr::read_csv("AWM_EURO.csv")
#
euro <- euro.area.data %>%
  mutate(quarter = yearquarter(X1)) %>%
  select(-X1) %>%
  as_tsibble(index = quarter)
euro
#
euro %>%
  autoplot(YER) +
  labs(title = "GDP at market prices of the euro area", subtitle = "Eurozone") +
  xlab("Year")
#

