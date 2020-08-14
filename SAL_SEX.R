library(readr)
library(car)
library(stargazer)
SAL_SEX <- read_csv("SAL_SEX.csv")
View(SAL_SEX)
S(lm_SAL <- lm(log(SALARIO) ~ EDUC + EXPER , data = SAL_SEX))
#
SAL_SEX_h <- SAL_SEX[which(SAL_SEX$MUJER==0),] #hombres
SAL_SEX_m <- SAL_SEX[which(SAL_SEX$MUJER==1),] #mujeres

S(lm_SAL_h <- lm(log(SALARIO) ~ EDUC + EXPER , data = SAL_SEX_h))
S(lm_SAL_m <- lm(log(SALARIO) ~ EDUC + EXPER , data = SAL_SEX_m))
#
# Test de Chow
S(lm_SAL_int <- lm(log(SALARIO) ~ EDUC + EXPER + MUJER/(EDUC + EXPER) , data = SAL_SEX))
anova(lm_SAL, lm_SAL_int)
#
compareCoefs(lm_SAL, lm_SAL_h, lm_SAL_m)
#