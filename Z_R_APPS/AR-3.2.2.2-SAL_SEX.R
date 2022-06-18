# Lectura de librerías
library(tidyverse)
library(car)
# Lectura de datos
SAL_SEX <- read_csv("SAL_SEX.csv")
#
Boxplot(~SALARIO, data=SAL_SEX, main="", ylab="SALARIO", id=list(method = "none"))
#
class(SAL_SEX$MUJER)
SEXO <- factor(SAL_SEX$MUJER, labels=c("Hombre", "Mujer"))
summary(SEXO)
Boxplot(SALARIO~SEXO, data=SAL_SEX, ylab="SALARIO", id=list(method = "none"))
#
# Ecuación de salarios
#
summary(lm_SAL <- lm(log(SALARIO) ~ EDUC + EXPER , data = SAL_SEX))
#
scatterplot(log(SALARIO) ~ EDUC| SEXO, data=SAL_SEX, smooth=FALSE, boxplots=FALSE, ylab="Relación parcial log(Salario)/Educación")
scatterplot(log(SALARIO) ~ EXPER| SEXO, data=SAL_SEX, smooth=FALSE, boxplots=FALSE, ylab="Relación parcial log(Salario)/Experiencia")
# 
# ¿Existe diferenciación por sexos?
#
SAL_SEX_h <- SAL_SEX[which(SAL_SEX$MUJER==0),] # datos hombres
SAL_SEX_m <- SAL_SEX[which(SAL_SEX$MUJER==1),] # datos mujeres
#
summary(lm_SAL_h <- lm(log(SALARIO) ~ EDUC + EXPER , data = SAL_SEX_h))
summary(lm_SAL_m <- lm(log(SALARIO) ~ EDUC + EXPER , data = SAL_SEX_m))
#
compareCoefs(lm_SAL_h, lm_SAL_m)
#
# Test de Chow
summary(lm_SAL_int <- lm(log(SALARIO) ~ EDUC + EXPER + MUJER/(EDUC + EXPER), data = SAL_SEX))
anova(lm_SAL, lm_SAL_int)
#
# Regresión diferenciada por sexos
#
summary(lm_SAL_int <- lm(log(SALARIO) ~ (EDUC + EXPER)*MUJER, data = SAL_SEX))
#
compareCoefs(lm_SAL_h, lm_SAL_m)
#