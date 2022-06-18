library(tidyverse)
CONS_USA <- read_csv("CONS_USA.csv")
CONS_USA_ts <- ts(CONS_USA[,2:3], start=c(1959), end = c(2015))
plot.ts(CONS_USA_ts)
#
CONS_USA %>%
ggplot(aes(x = Y, y = C)) +
geom_point() +
ylab("Consumo privado ($ million)") + xlab("Renta disponible ($ million)")
#
KEYNES_model <- lm (C ~ Y, data = CONS_USA_ts)
summary(KEYNES_model)
#
library(strucchange)
bp.cons <- breakpoints(C ~ Y, data = CONS_USA_ts)
summary(bp.cons)
plot(bp.cons)
ci.cons <- confint(bp.cons)
ci.cons
lines(ci.cons)
#