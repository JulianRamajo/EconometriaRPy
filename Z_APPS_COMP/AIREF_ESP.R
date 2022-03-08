library(fpp3)
AIREF.data <- readr::read_csv("AIREF_ESP.csv")
#
macro.ESP <- AIREF.data %>%
  mutate(quarter = yearquarter(obs)) %>%
  select(-obs) %>%
  as_tsibble(index = quarter)
macro.ESP
#
macro.ESP %>%
  autoplot(YER) +
  labs(title = "GDP", subtitle = "España") +
  xlab("Year")
#

