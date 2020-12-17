library(plm)
data( "EmplUK", package="plm" )
help( "EmplUK", package="plm" )
EmplUK.pdata <-  pdata.frame(EmplUK,index=c( "firm", "year"))
pdim(EmplUK.pdata)
#
dem_empl.GMM.1 <-pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1) + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),data=EmplUK.pdata, effect="twoways", model="twosteps")
summary(dem_empl.GMM.1, robust = FALSE )
#
#
dem_empl.GMM.2 <-pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1) + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99) + lag(log(wage), 2:99) + lag(log(capital), 1:99) + lag(log(output), 2:99),data=EmplUK.pdata, effect="twoways", model="onestep", transformation="ld")
summary(dem_empl.GMM.2, robust = FALSE )
#