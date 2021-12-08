library(tidyverse)
library(AER)
library(alr4)
library(margins)
PREST_HIP <- read_csv("PREST_HIP.csv")
# Modelo logit
ti.logit <- glm(Y ~ FI + MARG + YLD + PTS + MAT + BA + BS + FTB + CB + MC + SE + MOB + NW + LA + STL, data=PREST_HIP, family=binomial(link="logit"))
summary(ti.logit)
# Modelo probit
ti.probit <- glm(Y ~ FI + MARG + YLD + PTS + MAT + BA + BS + FTB + CB + MC + SE + MOB + NW + LA + STL, data=PREST_HIP, family=binomial(link="probit"))
summary(ti.probit)
# Significación de las características personales
linearHypothesis ( ti.logit , c("BA = 0", "BS = 0","FTB = 0","CB = 0","MC = 0","SE = 0","MOB = 0","NW = 0","LA = 0","STL = 0"))
#
ti.logit.1 <- glm(Y ~ FI + MARG + YLD + PTS + MAT, data=PREST_HIP, family=binomial(link="logit"))
summary(ti.logit.1)
linearHypothesis ( ti.logit.1 , c("PTS = 0", "MAT = 0"))
ti.logit.2 <- glm(Y ~ FI + MARG + YLD, data=PREST_HIP, family=binomial(link="logit"))
summary(ti.logit.2)
c(dev=deviance(ti.logit.2), df=df.residual(ti.logit.2))
#
# Ajuste del modelo
#
pseudoR2 <- 1 - (ti.logit.2$deviance) / (ti.logit.2$null.deviance)
pseudoR2
#
ti.logit.0 <- glm(Y ~ 1, data=PREST_HIP, family=binomial(link="logit"))
1 - logLik(ti.logit.2)[1]/logLik(ti.logit.0)[1]
#
coeftest(ti.logit.2, vcov. = vcovHC, type = "HC1")
Anova(ti.logit.2)
# Tabla de éxito-fracaso
table(true=PREST_HIP$Y, predicted=round(fitted(ti.logit.2)))
# Efectos marginales
margins(ti.logit.2)
# Gráficas de efectos marginales
effs <- Effect(c("FI", "MARG","YLD"), ti.logit.2)
plot(effs, multiline=TRUE, grid=TRUE, lines=c(1, 2, 3), 
     xlab="FI",main="", rotx=45, roty = 45,
     ylab="Prob. of Y=1", rescale.axis=FALSE, rug=FALSE)
#
#
# Gráficas de efectos marginales
#
effs <- Effect(c("FI", "MARG","YLD"), ti.logit.2)
plot(effs, multiline=TRUE, grid=TRUE, lines=c(1, 2, 3), 
     xlab="FI",main="", rotx=45, roty = 45,
     ylab="Prob. of Y=1", rescale.axis=FALSE, rug=FALSE)
#
# Gráfica de la función de probabilidad estimada (para la variable FI)
# 
plot( Y ~ FI, PREST_HIP, xlab="FI", ylab="Prob", ylim=c(0,1))
FInew <- seq(10, 20, length=78)
lines(FInew, predict(ti.logit.2, newdata=data.frame(FI=FInew, MARG=rep(mean(PREST_HIP$MARG), 78), YLD=rep(mean(PREST_HIP$YLD), 78)), type="response"), lwd=1.5)
grid(col="gray", lty="solid")
#
# Gráfica de la función de probabilidad estimada (para la variable MARG)
# 
plot( Y ~ MARG, PREST_HIP, xlab="MARG", ylab="Prob", ylim=c(0,1))
MARGnew <- seq(-1, 6, length=78)
lines(MARGnew, predict(ti.logit.2, newdata=data.frame(FI=rep(mean(PREST_HIP$FI), 78), MARG=MARGnew, YLD=rep(mean(PREST_HIP$YLD), 78)), type="response"), lwd=1.5)
grid(col="gray", lty="solid")
#
# Gráfica de la función de probabilidad estimada (para la variable YLD)
# 
plot( Y ~ YLD, PREST_HIP, xlab="YLD", ylab="Prob", ylim=c(0,1))
YLDnew <- seq(1, 2.5, length=78)
lines(YLDnew, predict(ti.logit.2, newdata=data.frame(FI=rep(mean(PREST_HIP$FI), 78), MARG=rep(mean(PREST_HIP$MARG), 78), YLD=YLDnew), type="response"), lwd=1.5)
grid(col="gray", lty="solid")
