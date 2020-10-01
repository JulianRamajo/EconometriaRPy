library(AER)
library(car)
library(readr)
INF_INT <- read_csv("_ECOMET_code/INF_INT.csv")
View(INF_INT)
summary(INF_INT)
attach(INF_INT)
#
# MCO
lm_INF <- lm(INF ~ RM + RY)
S(lm_INF)
# VI
iv_INF <- ivreg(INF ~ RM + RY | RM + EDUC + INV + RPOB + YPC0)
S(iv_INF, diagnostics=TRUE)
