setwd("~/Documents/R/_ECOMET_code")
library(readr)
CAPM_MOBIL <- read_csv("CAPM_MOBIL.csv")
ts_CAPM_MOBIL <- ts(CAPM_MOBIL, start=c(1978,1), end = c(1987,12), frequency = 12)
plot(ts_CAPM_MOBIL)
#
ER_MOBIL <- ts_CAPM_MOBIL[,"ER_MOBIL"]
ER_M <- ts_CAPM_MOBIL[,"ER_M"]
ts.plot(ER_MOBIL)
ts.plot(ER_M)
#
#
# Modelo CAPM para las acciones de la empresa Mobil
#
CAPM <- lm (ER_MOBIL~ER_M)
summary(CAPM)
#
# Contrastes de hipótesis
#
# beta1=0 versus beta1≠0
#
# Estadístico t
# Método del valor crítico
alpha <- 0.05 # nivel de significación
b1 <- coef(CAPM)[[1]] # estimación del parámetro beta1
seb1 <- sqrt(vcov(CAPM)[1,1]) # estimación de la desviación típica de beta1
c <- 0
df <- df.residual(CAPM) # grados de libertad
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
linearHypothesis(CAPM,H_0,test="F")
#
# beta2=1 versus beta1≠1
#
# Estadístico t
# Método del valor crítico
alpha <- 0.05 # nivel de significación
b2 <- coef(CAPM)[[2]] # estimación del parámetro beta1
seb2 <- sqrt(vcov(CAPM)[2,2]) # estimación de la desviación típica de beta1
c <- 1
df <- df.residual(CAPM) # grados de libertada
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
H_0 <- "ER_M=1"
linearHypothesis(CAPM,H_0,test="F")
#
# Contraste conjunto: beta1=0, beta2=1
H_0 <- c("(Intercept) = 0", "ER_M = 1")
linearHypothesis(CAPM,H_0,test="F")
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
curve(dt(x, df), -20, 20, ylab=" ", xlab="t")
abline(v=c(tcr, t), col=c("red", "blue"), lty=c(2, 3))
legend("topleft", legend=c("tcr", "t"),
       col=c("red", "blue"), lty=c(2, 3))
#
p <- pt(t, df)
p
# Intervalos de confianza
IntConf <- confint(CAPM)
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
s_CAPM <- summary(CAPM)
print(s_CAPM)
names(s_CAPM)
R2 <- s_CAPM$r.squared
R2
# 
T <- nobs(CAPM)
K <- T-df.residual(CAPM)
T ; K
F_0 <- (R2/(K-1))/((1-R2)/(T-K))
F_0
anova(CAPM)
# Intervalo confianza para sigma
s2 <- s_CAPM$sigma^2
s2
#
alpha <- 0.05
chisqcr1 <- qchisq(alpha/2, df) 
chisqcr2 <- qchisq(1-alpha/2, df)
chisqcr1 ; chisqcr2
inf_s2 <- (T-K)*s2/chisqcr2 # cota inferior
sup_s2 <- (T-K)*s2/chisqcr1 # cota superior
inf_s2 ; sup_s2
#
s <-  sqrt(s2)
inf_s <- sqrt(inf_s2)
sup_s <- sqrt(sup_s2)
inf_s ; s ; sup_s
#

