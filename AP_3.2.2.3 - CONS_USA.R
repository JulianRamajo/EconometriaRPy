library(tidyverse)
CONS_USA <- read_csv("CONS_USA.csv")
ts_CONS_USA <- ts(CONS_USA[,2:3], start=c(1959), end = c(2015))
#
CONS_USA %>%
ggplot(aes(x = Y, y = C)) +
geom_point() +
ylab("Consumo privado ($ million)") + xlab("Renta disponible ($ million)")
#
KEYNES_model <- lm (C ~ Y, data = ts_CONS_USA)
summary(KEYNES_model)
#
library(strucchange)
bp.cons <- breakpoints(C ~ Y, data = ts_CONS_USA)
summary(bp.cons)
#
C <- ts(CONS_USA$C, start=c(1959, 1), end=c(2015, 1), frequency=1)
plot(bp.cons)
plot(C)
lines(bp.cons)
#
ci.cons <- confint(bp.cons)
ci.cons
lines(ci.cons)
