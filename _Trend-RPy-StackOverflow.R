library(ggplot2)
library(dplyr)
library(readr)
# Number of questions asked by month for both python and R:
# http://data.stackexchange.com/stackoverflow/query/150296/r-and-python-questions
so <- read_csv("http://data.stackexchange.com/stackoverflow/csv/186078")
names(so) <- c("month", "tag", "count")
so$month <- as.Date(so$month)
so <- so %>% filter(month > as.Date("2010-01-01")) # too noisy before 2010

# Explosive growth of both python and R tags
ggplot(so, aes(month, count)) + 
  geom_smooth(aes(group = tag), method = MASS::rlm, se = F, colour = "grey70", size = 0.5) +
  geom_line(aes(colour = tag)) +
  scale_y_log10()

# If we standardise python to 1, we see that R is growing relative to pythonover time
rel <- so %>% 
  group_by(month) %>% 
  mutate(rel = count / max(count)) %>%
  filter(tag == "r")

ggplot(rel, aes(month, rel)) + 
  geom_line() + 
  ylab("R questions as proportion of python questions")
