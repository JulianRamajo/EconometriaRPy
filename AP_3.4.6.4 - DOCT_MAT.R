library(alr4)
library(AER)
#
dim(AMSsurvey)
# Reordenación de los niveles de acuerdo el número de Tesis en cada nivel
AMSsurvey$type <- factor(AMSsurvey$type, levels=levels(AMSsurvey$type)[order(xtabs(count ~ type, AMSsurvey))])
#
AMSsurvey
#
Po_PHDs <- glm(count ~ type*sex + type*citizen, poisson, AMSsurvey)
S(Po_PHDs)
#
Anova(Po_PHDs)
#
plot(Effect(c("type", "citizen"), Po_PHDs), multiline=TRUE, ci.style="bars", main="", xlab="Tipo de programa de doctorado", ylab="Número de nuevos Doctores", rescale.axis=FALSE, grid=TRUE)
#
plot(Effect(c("type", "sex"), Po_PHDs), multiline=TRUE, ci.style="bars", main="", xlab="Tipo de programa de doctorado", ylab="Número de nuevos Doctores", rescale.axis=FALSE, grid=TRUE)
# Contraste de sobredispersión
dispersiontest(Po_PHDs)
dispersiontest(Po_PHDs, trafo = 2)
