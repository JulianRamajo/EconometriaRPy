#
library(tidyverse)
library(car)
library(sampleSelection)
#
SAL_MUJ <- read_csv("SAL_MUJ.csv")
summary(SAL_MUJ)
#
heckit.SAL <- selection(PMT ~ EDUC + EDAD + HIJOSMEN + TMIMP , log(SALARIO) ~ EDUC + EXPER, data=SAL_MUJ, method="ml")
summary(heckit.SAL)
#