library(readr)
library(car)
library(sampleSelection)
SAL_MUJ <- read_csv("_ECOMET_code/SAL_MUJ.csv")
View(SAL_MUJ)
S(SAL_MUJ)
attach(SAL_MUJ)
#
heckit.SAL <- selection(PMT ~ EDUC + EDAD + HIJOSMEN + TMIMP , log(SALARIO) ~ EDUC + EXPER, method="ml")
summary(heckit.SAL)
#
