#
AIREF.ESP.data <- readr::read_csv("AIREF_ESP.csv")
# Método tidyverse (tsibble)
library(fpp3)
macro.Spain <- AIREF.ESP.data %>%
  mutate(quarter = yearquarter(obs)) %>%
  select(-obs) %>%
  as_tsibble(index = quarter)
macro.Spain
#
macro.Spain %>%
  autoplot(PIR) +
  labs(title = "Inversión en Bienes de Equipo", subtitle = "[Índice de volumen]") + xlab("Year")
# Largo plazo
reg.lp <-  macro.Spain %>%
  model(TSLM(log(PIR) ~ log(YER) + log(QT) + log(CPE) + LTR + UCAP)) %>%
  report()
augment(reg.lp)
reg.lp %>% gg_tsresiduals()
macro.Spain$u.lp <- augment(reg.lp)$.innov
# Corto plazo
#
macro.Spain %>%
  mutate(dl_PIR = difference(log(PIR)), 
         dl_YER = difference(log(YER)), 
         dl_QT = difference(log(QT)), 
         dl_CPE = difference(log(CPE)), 
         d_LTR = difference(LTR), 
         d_UCAP = difference(UCAP) ) %>%
  model(TSLM(dl_PIR ~ dl_YER + dl_QT + d_LTR + d_UCAP + lag(u.lp)))  %>%
  report()
# Método tradicional
AIREF.ESP.ts <- ts(AIREF.ESP.data[,2:34], start=c(1995,1), end=c(2017,4),frequency=4)
ts.plot(AIREF.ESP.ts[,"PIR"] , xlab="Inversión en Bienes de Equipo", ylab="")
#
library(dynlm)
# Largo plazo
summary(reg.lp <- dynlm(log(PIR) ~ log(YER) + log(QT) + log(CPE) + LTR + UCAP, data=AIREF.ESP.ts))
u.lp <- resid(reg.lp)
plot(u.lp)
# Corto plazo
summary(reg.cp <- dynlm(d(log(PIR)) ~ d(log(YER)) + d(log(QT)) + d(LTR) + d(UCAP) + L(u.lp), data=AIREF.ESP.ts))
#