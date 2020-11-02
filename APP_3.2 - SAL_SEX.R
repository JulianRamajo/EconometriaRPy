library(readr)
library(car)
library(stargazer)
SAL_SEX <- read_csv("SAL_SEX.csv")
View(SAL_SEX)
#
Boxplot(~SALARIO, data=SAL_SEX, main="", ylab="SALARIO")
#
#
class(SAL_SEX$MUJER)
SEXO <- factor(SAL_SEX$MUJER, labels=c("Hombre", "Mujer"))
summary(SEXO)
Boxplot(SALARIO~SEXO, data=SAL_SEX, id=list(method = "none"))
#
scatterplot(SALARIO ~ EDUC| SEXO, data=SAL_SEX, smooth=FALSE, boxplots=FALSE, ylab="Relación parcial Salario/Educación")
scatterplot(SALARIO ~ EXPER| SEXO, data=SAL_SEX, smooth=FALSE, boxplots=FALSE, ylab="Relación parcial Salario/Experiencia")
#
S(lm_SAL <- lm(log(SALARIO) ~ EDUC + EXPER , data = SAL_SEX))
#
SAL_SEX_h <- SAL_SEX[which(SAL_SEX$MUJER==0),] #hombres
SAL_SEX_m <- SAL_SEX[which(SAL_SEX$MUJER==1),] #mujeres
#
S(lm_SAL_h <- lm(log(SALARIO) ~ EDUC + EXPER , data = SAL_SEX_h))
S(lm_SAL_m <- lm(log(SALARIO) ~ EDUC + EXPER , data = SAL_SEX_m))
#
compareCoefs(lm_SAL, lm_SAL_h, lm_SAL_m)
#
# Test de Chow
S(lm_SAL_int <- lm(log(SALARIO) ~ EDUC + EXPER + MUJER/(EDUC + EXPER), data = SAL_SEX))
anova(lm_SAL, lm_SAL_int)
#
# Regresión diferenciada por sexos
#
S(lm_SAL_int <- lm(log(SALARIO) ~ (EDUC + EXPER)*MUJER, data = SAL_SEX))
#
compareCoefs(lm_SAL, lm_SAL_int)
#