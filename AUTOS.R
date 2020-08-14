library(readr)
library(AER)
library(censReg)
AUTOS <- read_csv("_ECOMET_code/AUTOS.csv")
View(AUTOS)
S(AUTOS)
attach(AUTOS)
S(tobit_AUTOS <- tobit(GAUT ~ Y + HIJOSM18 + EDAD))
#
S(tobit_AUTOS <- censReg(GAUT ~ Y + HIJOSM18 + EDAD))
# Efectos marginales en un valor concreto de las var. explicativas (valor promedio en este caso)
margEff(tobit_AUTOS,xValues = c(1,11540,1.591,45.12))
# Efectos marginales promedio
margEff(tobit_AUTOS)
#
