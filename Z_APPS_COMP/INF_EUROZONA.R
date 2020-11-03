library(readr)
INF_EUROZONA <- read_delim("INF_EUROZONA.csv", ";", escape_double = FALSE, trim_ws = TRUE)
INF_zonaeuro <- ts(INF_EUROZONA, start=c(1971,1), end = c(2018,4), frequency = 4)
ts.plot(INF_zonaeuro[,"INF"])
ts.plot(INF_zonaeuro[,"INF"])
ts.plot(INF_zonaeuro[,"INF"])
plot(INF_zonaeuro)
#
INF <- INF_zonaeuro[,"INF"]
D4M3 <- INF_zonaeuro[,"R4M3"]
D4Y <- INF_zonaeuro[,"R4Y"]
#
lm_INF_0 <- lm(INF~D4M3 + D4Y)
summary(lm_INF_0)
library(dynlm)
lm_INF <- dynlm(INF~D4M3 + D4Y)
summary(lm_INF)
#