#
library(tidyverse)
library(AER)
library(car)
library(ivreg)
#
INF_INT <- read_csv("INF_INTERN.csv")
summary(INF_INT)
#
# MCO
lm_MCO <- lm(INF ~ RM + RY, data=INF_INT)
summary(lm_MCO)
# VI
lm_IV <- ivreg(INF ~ RM + RY | RM + EDUC + INV + RPOB + YPC0, data=INF_INT)
summary(lm_IV, diagnostics=TRUE)
#
car::compareCoefs(lm_MCO, lm_IV)
#