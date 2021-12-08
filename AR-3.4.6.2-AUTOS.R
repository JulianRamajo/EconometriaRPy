library(tidyverse)
library(AER)
library(censReg)
GASTO_AUTOS <- read_csv("AUTOS.csv")
summary(AUTOS)
#
summary(tobit_AUTOS <- tobit(GAUT ~ Y + HIJOSM18 + EDAD, data=GASTO_AUTOS))
#
summary(tobit_AUTOS <- censReg(GAUT ~ Y + HIJOSM18 + EDAD, data=GASTO_AUTOS))
# Efectos marginales en un valor concreto de las var. explicativas (valor promedio en este caso)
margEff(tobit_AUTOS,xValues = c(1,11540,1.591,45.12))
# Efectos marginales promedio
margEff(tobit_AUTOS)
#
