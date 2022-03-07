#
library(tidyverse)
library(AER)
library(alr4)
library(modelsummary)
library(plm)
#
QUIM_CHINA <- read_csv("QUIM_CHINA.csv")
# Estructura de panel de datos 
QUIM_CHINA.pdata  <-  pdata.frame(QUIM_CHINA,index=c("firm", "year"))
pdim(QUIM_CHINA.pdata)
#
# Modelos estáticos
# Modelo plano
CD_panel_pool_mod <- plm(log(Y) ~ log(K)+log(L)+log(M), data = QUIM_CHINA.pdata, model = "pooling")
summary(CD_panel_pool_mod)
# Modelo de efectos fijos (transversales)
CD_panel_fe_mod <- plm(log(Y) ~ log(K)+log(L)+log(M), data = QUIM_CHINA.pdata, model = "within")
summary(CD_panel_fe_mod)
# Comparación del mdelo plano frente al modelo de efectos fijos
pFtest(CD_panel_fe_mod, CD_panel_pool_mod)
# Modelo de efectos fijos (transversales y temporales)
# CD_panel_fete_mod <- plm(log(Y) ~ log(K)+log(L)+log(M), data = QUIM_CHINA.pdata, model = "within", effect = "twoways")
# summary(CD_panel_fete_mod)
#
# Modelo de efectos aleatorios (transversales)
CD_panel_re_mod <- plm(log(Y) ~ log(K)+log(L)+log(M), data = QUIM_CHINA.pdata, model = "random")
summary(CD_panel_re_mod)
# Significación de efectos individuales
plmtest(CD_panel_pool_mod, effect="individual")
plmtest(CD_panel_pool_mod, effect = "twoways" )
# Comparación del modelo de efectos aleatorios frente al modelo de efectos fijos (test de Hausman)
phtest(CD_panel_fe_mod, CD_panel_re_mod)
# # Modelo de efectos aleatorios (transversales) y fijos (temporales)
# CD_panel_fete_mod <- plm(log(Y) ~ log(K)+log(L)+log(M), data = QUIM_CHINA.pdata, model = "random", effect = "twoways")
# summary(CD_panel_fete_mod)
# Comparación de resultados
modelos <- list(
  "Pool MCO"     = plm(log(Y) ~ log(K) + log(L) + log(M), data = QUIM_CHINA_pdata, model = "pooling"),
  "Modelo EF" = plm(log(Y) ~ log(K) + log(L) + log(M), data = QUIM_CHINA_pdata, model = "within"),
  "Modelo EA"     = plm(log(Y) ~ log(K) + log(L) + log(M), data = QUIM_CHINA_pdata, model = "random")
)
modelsummary(modelos)