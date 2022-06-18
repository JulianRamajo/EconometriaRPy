# Lectura de librerías
library(tidyverse)
library(plm)
# Lectura de datos
EMP_SAL_UK <- read_csv("EMP_SAL_UK.csv")
summary(EMP_SAL_UK)
# Estructura de datos de panel
EMP_SAL_UK_pdata <-  pdata.frame(EMP_SAL_UK, index=c( "firm", "year"))
pdim(EMP_SAL_UK_pdata)
#
dem_emp_GMM_1 <-pgmm(log(L) ~ lag(log(L), 1:2) + lag(log(W), 0:1) + log(K) + lag(log(Y), 0:1) | 
                        lag(log(L), 2:99),
                      data=EMP_SAL_UK_pdata, effect="twoways", model="twosteps")
summary(dem_emp_GMM_1, robust = FALSE )
#
dem_emp_GMM_2 <-pgmm(log(L) ~ lag(log(L), 1:2) + lag(log(W), 0:1) + log(K) + lag(log(Y), 0:1) | 
                        lag(log(L), 2:99) + lag(log(W), 2:99) + lag(log(K), 1:99) + lag(log(Y), 2:99),
                      data=EMP_SAL_UK_pdata, effect="twoways", model="onestep", transformation="ld")
summary(dem_emp_GMM_2, robust = FALSE )
#