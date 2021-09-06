#
library(tidyverse)
library(AER)
library(car)
library(ivreg)
#
INF_INT <- read_csv("INF_INTERN.csv")
View(INF_INT)
summary(INF_INT)
attach(INF_INT)
#
# MCO
lm_MCO <- lm(INF ~ RM + RY)
summary(lm_MCO)
# VI
lm_IV <- ivreg(INF ~ RM + RY | RM + EDUC + INV + RPOB + YPC0)
summary(lm_IV, diagnostics=TRUE)
#
car::compareCoefs(lm_MCO, lm_IV)
