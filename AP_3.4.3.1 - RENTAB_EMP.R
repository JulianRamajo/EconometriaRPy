#
library(tidyverse)
library(car)
library(corrplot)
library(broom)
library(cowplot)
#
RENTAB_EMP <- read_delim("RENTAB_EMP.csv", ";", escape_double = FALSE, trim_ws = TRUE)
View(RENTAB_EMP)
#
lm_1 <- lm(RENTAB ~ log(VT) + R1 + R2 + R3 + R4 + R5 + R6 + R7 + R8 + R9 + R10, data=RENTAB_EMP)
S(lm_1)
#
# DETECCIÓN DEL PROBLEMA
#
# Matríz de correlaciones de las variables explicativas (sin incluir la constante)
#
attach(RENTAB_EMP)
X <- data.frame(log(VT), R1, R2, R3, R4, R5, R6, R7, R8, R9, R10)
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
# Método de componentes principales (PCA)
# Clásico
PCA_X <- prcomp(X, scale. = TRUE)
dim(PCA_X$rotation)
PCA_X$rotation
dim(PCA_X$x)
summary(PCA_X)
# plot(PCA_X)
# Estilo tidyverse
PCA_X %>% tidy(matrix = "eigenvalues")
PCA_X %>% tidy(matrix = "rotation")
#
PCA_X %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC, percent)) +
  geom_col(fill = "#56B4E9", alpha = 0.8) +
  scale_x_continuous(breaks = 1:11) +
  scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.01))) +
  theme_minimal_hgrid(12)
#
PCA_X %>%
  augment(RENTAB_EMP) %>% 
  ggplot(aes(.fittedPC1, .fittedPC2)) + geom_point(size = 1.5) +
  theme_half_open(12) + background_grid()
#
arrow_style <- arrow(angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt"))
PCA_X %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value") %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text(aes(label = column),hjust = 1, nudge_x = -0.02, color = "#904C2F") +
  xlim(-1.25, .5) + ylim(-.5, 1) +
  coord_fixed() + 
  theme_minimal_grid(12)
# Regresión con los cuatros primeros factores (ortogonales entre sí, y con una explicatividad > 80%)
lm_2 <- lm(RENTAB ~ PCA_X$x[,1:4])
S(lm_2)
# Significado de la variables explicativas
round(PCA_X$rotation[,1],2)
round(PCA_X$rotation[,2],2)
round(PCA_X$rotation[,3],2)
round(PCA_X$rotation[,4],2)
#
# Método de mínimos cuadrados parciales (PLS)
#
require(pls)
lm_3 <- plsr(RENTAB ~ log(VT) + R1 + R2 + R3 + R4 + R5 + R6 + R7 + R8 + R9 + R10, data=RENTAB_EMP, ncomp=11, validation="CV")
summary(lm_3)
explvar(lm_3)
plsCV <- RMSEP(lm_3, estimate="CV")
plot(plsCV,main="")
#
plot(lm_3, ncomp = 3, asp = 1, line = TRUE)
plot(lm_3, plottype = "scores", comps = 1:3)
plot(lm_3, plottype = "correlation")
plot(lm_3, plottype = "coef", comps = 1:3, legendpos = "bottomright")
#
# Método ridge
#
# Librería MASS
require(MASS)
#
lm_4_1 <- lm.ridge(RENTAB ~ log(VT) + R1 + R2 + R3 + R4 + R5 + R6 + R7 + R8 + R9 + R10, data=RENTAB_EMP) # lambda por defecto
lm_4_1
lm_4_2 <- lm.ridge(RENTAB ~ log(VT) + R1 + R2 + R3 + R4 + R5 + R6 + R7 + R8 + R9 + R10, data=RENTAB_EMP, lambda = 0.05)
lm_4_2
#
# Librería glmnet
library(glmnet)
# Variables dependiente e independientes del modelo
data <- data.frame(cbind(log(RENTAB_EMP$VT),RENTAB_EMP$R1,RENTAB_EMP$R2,RENTAB_EMP$R3,
                         RENTAB_EMP$R4, RENTAB_EMP$R5, RENTAB_EMP$R6, RENTAB_EMP$R7, 
                         RENTAB_EMP$R8,RENTAB_EMP$R9,RENTAB_EMP$R10, RENTAB_EMP$RENTAB))
names(data)=c("l_VT","R1","R2","R3","R4","R5","R6","R7","R8","R9","R10","RENTAB")
x_vars <- data.matrix(data[, 1:11])
y_var <- data[, "RENTAB"]
#
# Ajuste del modelo
fit_ridge <- glmnet(x_vars,y_var,alpha=0)
plot(fit_ridge, label=TRUE)
plot(fit_ridge, label=TRUE, xvar="lambda")
plot(fit_ridge, label=TRUE, xvar="dev")
print(fit_ridge)
coef(fit_ridge,s=0.05) # Parámetros estimados para un lambda s=X concreto
# Selección del lambda óptimo (criterio CV)
cvfit_ridge = cv.glmnet(x_vars, y_var, alpha=0)
plot(cvfit_ridge)
cvfit_ridge$lambda.min
coef(cvfit_ridge, s = "lambda.min")
#
# Método LASSO
#
# Librería lars
require(lars)
lm_5 <- lars(as.matrix(data[,-12]),data$RENTAB)
plot(lm_5)
cv_lars_mod <- cv.lars(as.matrix(data[,-12]),data$RENTAB)
min <- cv_lars_mod$index[which.min(cv_lars_mod$cv)]  # El valor mínimo se utiliza en el paso siguiente
predict(lm_5,s=min,type="coef",mode="fraction")$coef  # estimaciones LASSO
#
# Librería glmnet
# Ajuste del modelo
fit_lasso <- glmnet(x_vars,y_var,alpha=1)
plot(fit_lasso, label=TRUE)
plot(fit_lasso, label=TRUE, xvar="lambda")
plot(fit_lasso, label=TRUE, xvar="dev")
print(fit_lasso)
coef(fit_lasso,s=0.0006) # Parámetros estimados para un lambda s=X concreto
# Selección del lambda óptimo (criterio CV)
cvfit_lasso = cv.glmnet(x_vars, y_var, alpha=1)
plot(cvfit_lasso)
cvfit_lasso$lambda.min
coef(cvfit_lasso, s = "lambda.min")
#
# NOTA FINAL: Posible estandarización de las variables del modelo antes de aplicar los modelos lm.ridge y lars 
# (glmnet estandariza las variables X e y por defecto para la familia Gaussiana de modelos)
# 
library(caret)
preProcValues <- preProcess(data[,-12], method = c("center", "scale"))
dataTransformed <- predict(preProcValues, data)
x_vars <- data.matrix(dataTransformed[, 1:11])
y_var <- dataTransformed[, "RENTAB"]
#
