#
library(tidyverse)
library(car)
library(corrplot)
#
CAPUB_USA <- read_csv("CAPUB_USA.csv")
View(CAPUB_USA)
#
CAPUB_USA_ts <- ts(CAPUB_USA, start=c(1956), end = c(1989), frequency = 1)
plot(CAPUB_USA_ts)
#
Y <- CAPUB_USA_ts[,"Y"]
L <- CAPUB_USA_ts[,"L"]
K <- CAPUB_USA_ts[,"K"]
PK <- CAPUB_USA_ts[,"PK"]
trend <- seq(1:length(Y))
#
lm_1 <- lm(log(Y) ~ trend + log(L) + log(K) + log(PK))
summary(lm_1)
#
# DETECCIÓN DEL PROBLEMA
#
# Matríz de correlaciones de las variables explicativas
#
X <- data.frame(trend , log(L) , log(K) , log(PK))
cor(X)
corrplot(cor(X))
#
# Factores de inflación de la varianza (VIF)
#
vif(lm_1)
vif(lm_1) > 10 # problema de colinealidad (se coresponde con un VIF=10, es decir, Rj^2=0.90)
sqrt(vif(lm_1))
sqrt(vif(lm_1)) > 2 # cota alternativa (se coresponde con un VIF=4, es decir, Rj^2=0.75)
#
# TRATAMIENTO DEL PROBLEMA
#
# Método de componentes principales
#
prX <- prcomp(X, scale. = TRUE)
dim(prX$rot)
dim(prX$x)
summary(prX)
#
lm_2 <- lm(log(Y) ~ prX$x[,1:1])
summary(lm_2)
# Significado de la variable explicativa
round(prX$rot[,1],2)
#
# Método ridge
#
require(MASS)
lm_ridge <- lm.ridge(log(Y) ~ trend + log(L) + log(K) + log(PK), lambda = seq(0, 1, len=1000))
matplot(lm_ridge$lambda, coef(lm_ridge), type="l", xlab=expression(lambda), ylab=expression(hat(beta)),col=1)
which.min(lm_ridge$GCV)
lm_3 <- lm.ridge(log(Y) ~ trend + log(L) + log(K) + log(PK), lambda = 0.8)
lm_3
#
# Método PLS
#
require(pls)
lm_4 <- plsr(log(Y) ~ trend + log(L) + log(K) + log(PK), ncomp=1, validation="CV")
summary(lm_4)
plsCV <- RMSEP(lm_4, estimate="CV")
plot(plsCV,main="")
#
coefplot(lm_4, ncomp=1)
#
# Método LASSO
#
require(lars)
CAPUB_USA$trend <- seq(1:length(CAPUB_USA$Y))
class(CAPUB_USA)
data <- data.frame(cbind(CAPUB_USA$trend,log(CAPUB_USA$L),log(CAPUB_USA$K),log(CAPUB_USA$PK),log(CAPUB_USA$Y)))
names(data)=c("trend","l_L","l_K","l_PK","l_Y")
lm_5 <- lars(as.matrix(data[,-5]),data$l_Y)
plot(lm_5)
cv_lars_mod <- cv.lars(as.matrix(data[,-5]),data$l_Y)
cv_lars_mod$index[which.min(cv_lars_mod$cv)]
predict(lm_5,s=0.989899,type="coef",mode="fraction")$coef
coef(lm(log(Y) ~ trend+log(L)+log(K)+log(PK), CAPUB_USA))
#