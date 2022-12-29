library(ggplot2)
library(dplyr)
library(readr)
# Number of questions asked by month for both Python and R:
# https://data.stackexchange.com/stackoverflow/query/1599213/r-python-questions
so <- read_csv("QueryResults.csv")
names(so) <- c("month", "tag", "count")
so$month <- as.Date(so$month)
so <- so %>% filter(month > as.Date("2014-01-01")) 

# Explosive growth of both Python and R tags
ggplot(so, aes(month, count)) + 
  geom_smooth(aes(group = tag), method = MASS::rlm, se = F, colour = "grey70", size = 0.5) +
  geom_line(aes(colour = tag)) +
  scale_y_log10()

# If we standardise Python to 1, we see that R is growing relative to Python over time
rel <- so %>% 
  group_by(month) %>% 
  mutate(rel = count / max(count)) %>%
  filter(tag == "r")

ggplot(rel, aes(month, rel)) + 
  geom_line() + 
  ylab("R questions as proportion of Python questions")
