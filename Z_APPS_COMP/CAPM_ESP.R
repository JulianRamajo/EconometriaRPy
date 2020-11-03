setwd("~/Documents/R/R_ECOMET_Ramajo")
library(readr)
CAPM_ESP <- read_csv("CAPM_ESP.csv")
ts_CAPM_ESP <- ts(CAPM_ESP, start=c(2001,5), end = c(2018,1), frequency = 12)
#
ts.plot(ts_CAPM_ESP[,"P_IBEX35"])
ts.plot(ts_CAPM_ESP[,"P_SANTANDER"])
ts.plot(ts_CAPM_ESP[,"P_TELEFONICA"])
ts.plot(ts_CAPM_ESP[,"P_INDITEX"])
#
R_IBEX35 <- ts_CAPM_ESP[,"R_IBEX35"]
R_SANTANDER <- ts_CAPM_ESP[,"R_SANTANDER"]
R_TELEFONICA <- ts_CAPM_ESP[,"R_TELEFONICA"]
R_INDITEX <- ts_CAPM_ESP[,"R_IBEX35"]
R_LT1Y<- ts_CAPM_ESP[,"R_LT1Y"]
ts.plot(R_IBEX35)
ts.plot(R_SANTANDER, R_TELEFONICA, R_INDITEX)
ts.plot(R_LT1Y)
#
ER_IBEX35 <- R_IBEX35-R_LT1Y
ER_SANTANDER <- R_SANTANDER-R_LT1Y
ER_TELEFONICA <- R_TELEFONICA-R_LT1Y
ER_INDITEX <- R_INDITEX-R_LT1Y
#
# Modelo CAPM para las acciones del Banco Santander (repetir los cálculos para Telefónica e Inditex)
#
CAPM_SANTANDER <- lm (ER_SANTANDER~ER_IBEX35)
summary(CAPM_SANTANDER)
# Usando el package dynlm
library(dynlm)
lm_CAPM_SANTANDER <- dynlm(ER_SANTANDER~ER_IBEX35)
summary(lm_CAPM_SANTANDER)
#
# Contrastes de hipótesis
#
# beta1=0 versus beta1≠0
#
# Estadístico t
# Método del valor crítico
alpha <- 0.05 # nivel de significación
b1 <- coef(CAPM_SANTANDER)[[1]] # estimación del parámetro beta1
seb1 <- sqrt(vcov(CAPM_SANTANDER)[1,1]) # estimación de la desviación típica de beta1
c <- 0
df <- df.residual(CAPM_SANTANDER) # grados de libertad
t <- (b1-c)/seb1 # estadístico t
t
tcr <- qt(1-alpha/2, df) # valor crítico
tcr
# Gráfico de la función de densidad t de Student, valor crítico y estadístico t:
curve(dt(x, df), -5, 5, ylab=" ", xlab="t")
abline(v=c(-tcr, tcr, t), col=c("red", "red", "blue"), lty=c(2,2,3))
legend("topleft", legend=c("-tcr", "tcr", "t"), col=c("red", "red", "blue"), lty=c(2, 2, 3))
# Método del P-valor
p <- 2*(1-pt(abs(t), df))
p
# Estadístico F
library(car)
H_0 <- "(Intercept)=0"
linearHypothesis(CAPM_SANTANDER,H_0,test="F")
#
# beta2=1 versus beta1≠1
#
# Estadístico t
# Método del valor crítico
alpha <- 0.05 # nivel de significación
b2 <- coef(CAPM_SANTANDER)[[2]] # estimación del parámetro beta1
seb2 <- sqrt(vcov(CAPM_SANTANDER)[2,2]) # estimación de la desviación típica de beta1
c <- 1
df <- df.residual(CAPM_SANTANDER) # grados de libertada
t <- (b2-c)/seb2 # estadístico t
t
tcr <- qt(1-alpha/2, df) # valor crítico
tcr
# Gráfico de la función de densidad t de Student, valor crítico y estadístico t:
curve(dt(x, df), -5, 5, ylab=" ", xlab="t")
abline(v=c(-tcr, tcr, t), col=c("red", "red", "blue"), lty=c(2,2,3))
legend("topleft", legend=c("-tcr", "tcr", "t"), col=c("red", "red", "blue"), lty=c(2, 2, 3))
# Método del P-valor
p <- 2*(1-pt(abs(t), df))
p
# Estadístico F
hyp <- "ER_IBEX35=1"
linearHypothesis(CAPM_SANTANDER,hyp,test="F")
#
# Contraste conjunto: beta1=0, beta2=1
hyp <- c("(Intercept) = 0", "ER_IBEX35 = 1")
linearHypothesis(CAPM_SANTANDER,hyp,test="F")
#
# Contrastes unilaterales
# 
# beta2≤1 versus beta2>1
#
c <- 1
alpha <- 0.05
t <- (b2-c)/seb2
t
tcr <- qt(1-alpha, df) # alpha no se divide por 2
tcr
curve(dt(x, df), -5, 5, ylab=" ", xlab="t")
abline(v=c(tcr, t), col=c("red", "blue"), lty=c(2, 3))
legend("topleft", legend=c("tcr", "t"),
       col=c("red", "blue"), lty=c(2, 3))
#
p <- 1-pt(t, df)
p
# 
# beta2≥2 versus beta2<2
#
c <- 2
alpha <- 0.05
t <- (b2-c)/seb2
t
tcr <- qt(alpha, df) # alpha no se divide por 2
tcr
curve(dt(x, df), -5, 5, ylab=" ", xlab="t")
abline(v=c(tcr, t), col=c("red", "blue"), lty=c(2, 3))
legend("topleft", legend=c("tcr", "t"),
       col=c("red", "blue"), lty=c(2, 3))
#
p <- pt(t, df)
p
# Intervalos de confianza
IntConf <- confint(CAPM_SANTANDER)
print(IntConf)
#
# Manualmente
#
alpha <- 0.05
tc <- qt(1-alpha/2, df)
#
inf_b1 <- b1-tc*seb1 # cota inferior
sup_b1 <- b1+tc*seb1 # cota superior
inf_b1 ; sup_b1
#
#
inf_b2 <- b2-tc*seb2 # cota inferior
sup_b2 <- b2+tc*seb2 # cota superior
inf_b2 ; sup_b2
#
# Ajuste del modelo (R^2) y ANOVA
s_CAPM_SANTANDER <- summary(CAPM_SANTANDER)
print(s_CAPM_SANTANDER)
names(s_CAPM_SANTANDER)
R2 <- s_CAPM_SANTANDER$r.squared
R2
anova(CAPM_SANTANDER)
# 
T <- nobs(CAPM_SANTANDER)
K <- T-df-residual(CAPM_SANTANDER)
F_0 <- (R2/(K-1))/((1-R2)/(T-K))
F_0
# Residuos del modelo
res <- s_CAPM_SANTANDER$residuals
res <- ts(res, start=c(2001,5), end = c(2018,1), frequency = 12)
plot(res)
hist(res, col="grey")
#
ehat <- resid(CAPM_SANTANDER)
ebar <- mean(ehat)
sdehat <- sd(ehat)
hist(ehat, col="grey", freq=FALSE, main="",
     ylab="density", xlab="ehat")
curve(dnorm(x, ebar, sdehat), col=2, add=TRUE,
      ylab="density", xlab="ehat")



