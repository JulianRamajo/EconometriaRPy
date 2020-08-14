library(plm)
pdata  <-  pdata.frame(panelx, index=c("firm_ident", "year"))
pdim(pdata)
summary(pdata[c("return", "beta")])
# Pool (mezcla) de datos (sin efectos individuales)
ef_0 <-  plm(return~beta, model="pooling", data=pdata)
summary(ef_0)
# Modelo de efectos fijos
ef_f <-  plm(return~beta, model="within", data=pdata)
summary(ef_f)
pFtest(ef_f,ef_0) # Contraste de significación de los efectos fijos
# Modelo de efecto aleatorios
ef_a <-  plm(return~beta, model="random", data=pdata)  
summary(ef_a)
plmtest(ef_a, effect = "individual", type = "bp")
# Test de Hausman (efectos fijos versus efectos aleatorios)
phtest(ef_f, ef_a)
#