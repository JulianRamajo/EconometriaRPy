library(fpp3)
AIREF.ESP.data <- readr::read_csv("AIREF_ESP.csv")
#
spain <- AIREF.ESP.data %>%
  mutate(quarter = yearquarter(obs)) %>%
  select(-obs) %>%
  as_tsibble(index = quarter)
spain
#
spain %>%
  autoplot(YER) +
  labs(title = "PIB", subtitle = "Volumen (2006=100)") +
  xlab("Year")
#
