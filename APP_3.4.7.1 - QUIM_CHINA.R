#
library(readr)
QUIM_CHINA <- read_csv("QUIM_CHINA.csv")
# Estructura de panel de datos 
QUIM_CHINA.pdata  <-  pdata.frame(QUIM_CHINA,index=c("firm", "year"))
pdim(QUIM_CHINA.pdata)
#
library(AER)
library(alr4)
library(stargazer)
# Modelos estáticos
library(plm)
# Modelo plano
CD_panel_pool_mod <- plm(log(Y) ~ log(K)+log(L)+log(M), data = QUIM_CHINA.pdata, model = "pooling")
stargazer(CD_panel_pool_mod, type="text")
# Modelo de efectos fijos (transversales)
CD_panel_fe_mod <- plm(log(Y) ~ log(K)+log(L)+log(M), data = QUIM_CHINA.pdata, model = "within")
stargazer(CD_panel_fe_mod, type="text")
# Comparación del mdelo plano frente al modelo de efectos fijos
pFtest(CD_panel_fe_mod, CD_panel_pool_mod)
# Modelo de efectos fijos (transversales y temporales)
# CD_panel_fete_mod <- plm(log(Y) ~ log(K)+log(L)+log(M), data = QUIM_CHINA.pdata, model = "within", effect = "twoways")
# stargazer(CD_panel_fete_mod, type="text")
#
# Modelo de efectos aleatorios (transversales)
CD_panel_re_mod <- plm(log(Y) ~ log(K)+log(L)+log(M), data = QUIM_CHINA.pdata, model = "random")
stargazer(CD_panel_re_mod, type="text")
# Significación de efectos individuales
plmtest(CD_panel_pool_mod, effect="individual")
plmtest(CD_panel_pool_mod, effect = "twoways" )
# Comparación del modelo de efectos aleatorios frente al modelo de efectos fijos (test de Hausman)
phtest(CD_panel_fe_mod, CD_panel_re_mod)
# # Modelo de efectos aleatorios (transversales) y fijos (temporales)
# CD_panel_fete_mod <- plm(log(Y) ~ log(K)+log(L)+log(M), data = QUIM_CHINA.pdata, model = "random", effect = "twoways")
# stargazer(CD_panel_fete_mod, type="text")
