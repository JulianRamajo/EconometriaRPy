# Lectura de librerías
library(tidyverse)
library(alr4)
library(AER)
library(MASS)
# Datos: American Math Society (AMS) Survey 
# Description
# Counts of new PhDs in the mathematical sciences for 2008-09 and 2011-12 categorized by 
# type of institution, gender, and US citizenship status.
# Variables
# type: a factor with levels I(Pu) for group I public universities, I(Pr) for group I private universities, 
# II and III for groups II and III, IV for statistics and biostatistics programs, and Va for applied mathemeatics programs.
# sex: a factor with levels Female, Male of the recipient
# citizen: a factor with levels Non-US, US giving citizenship status
# count: The number of individuals of each type in 2008-09
# count11: The number of individuals of each type in 2011-12
dim(AMS_PhDsMATH)
# Reordenación de los niveles de acuerdo el número de Tesis en cada nivel
AMS_PhDsMATH$type <- factor(AMS_PhDsMATH$type, levels=levels(AMS_PhDsMATH$type)[order(xtabs(count ~ type, AMS_PhDsMATH))])
AMS_PhDsMATH
# Modelo de Poisson
modelo_Poisson <- glm(count ~ type*sex + type*citizen, family = poisson, AMS_PhDsMATH)
S(modelo_Poisson)
Anova(modelo_Poisson)
# Gráficas parciales
plot(Effect(c("type", "citizen"), modelo_Poisson), multiline=TRUE, ci.style="bars", main="", 
     xlab="Tipo de programa de doctorado", 
     ylab="Número de nuevos Doctores", 
     rescale.axis=FALSE, grid=TRUE)
plot(Effect(c("type", "sex"), modelo_Poisson), multiline=TRUE, ci.style="bars", main="", 
     xlab="Tipo de programa de doctorado", 
     ylab="Número de nuevos Doctores", 
     rescale.axis=FALSE, grid=TRUE)
# Contraste de sobredispersión
dispersiontest(modelo_Poisson)
dispersiontest(modelo_Poisson, trafo = 2)
# Modelos alternativos cuando se rechaza la hipótesis nula de equidispersión (varianza=media)
# Modelo quasiPoisson (errores estándar QMLE robustos) [binomial negativa con función de varianza lineal, NB1]
modelo_quasiPoisson <- glm(count ~ type*sex + type*citizen, family = quasipoisson, AMS_PhDsMATH)
S(modelo_quasiPoisson)
Anova(modelo_quasiPoisson)
# Modelo Binomial Negativo (NB2)
modelo_BinNeg <- glm.nb(count ~ type*sex + type*citizen, data = AMS_PhDsMATH)
S(modelo_BinNeg)
#