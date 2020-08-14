# Paquetes utilizados
#
library(car)
library(quantreg)
library(lmtest)
library(sandwich)
library(dynlm)
library(moments)
library(strucchange)
#
load("APT_USA.RData")
View(APT_USA)
APT_ts <- ts(macro, start=c(1986,3), end = c(2018,3), frequency = 12)
plot(APT_ts[,2:10])
#
# X <- APT_ts[,"X"]
#
S(lm_msoft_0  <- lm( ermsoft ~ ersandp + dprod + dcredit + dinflation + dmoney + dspread + rterm , data = APT_ts ))
linearHypothesis(lm_msoft,c("dprod=0","dcredit=0","dmoney=0","dspread=0"))
S(lm_msoft_1  <- lm( ermsoft ~ ersandp + dinflation + rterm , data = APT_ts ))
#
# Regresiones cuantilíticas
#
S(qreg_msoft_1  <- rq( ermsoft ~ ersandp + dinflation + rterm , data = APT_ts , tau=0.5 ))
S(qreg_msoft_2  <- rq( ermsoft ~ ersandp + dinflation + rterm , data = APT_ts , tau=seq(0.1,0.9,0.1)) )
plot(summary(qreg_msoft_2), level=0.95)
# 

############################## Heteroscedasticidad y autocorrelación ##############################

S(dynlm_msoft_1  <- dynlm( ermsoft ~ ersandp + dinflation + rterm , data = APT_ts ))

plot(dynlm_msoft_1$residuals)
 
bptest(formula(dynlm_msoft_1), data = APT_ts , studentize = F)
bptest(formula(dynlm_msoft_1), data = APT_ts , studentize = T)

dwtest(dynlm_msoft_1)
bgtest(dynlm_msoft_1,order = 10)

S(dynlm_msoft_1 , vcov. = vcovHC(dynlm_msoft_1,type="HC1")) # White
S(dynlm_msoft_1 , vcov. = vcovHAC(dynlm_msoft_1))           # HAC (automático)
S(dynlm_msoft_1 , vcov. = NeweyWest(dynlm_msoft_1,lag = 6,adjust = T,prewhite = F)) # HAC (manual)

############################## Normalidad de los residuos ##############################

hist(dynlm_msoft_1$residuals , main = "")
box()

skewness(dynlm_msoft_1$residuals)
kurtosis(dynlm_msoft_1$residuals)

jarque.test(lm_msoft_1$residuals)

agostino.test(dynlm_msoft_1$residuals)
anscombe.test(dynlm_msoft_1$residuals)


############################## Variables ficticias ##############################


# macro$Date = as.Date(macro$Date)
# macro$APR00DUM = as.integer(macro$Date == as.Date("2000-04-01"))
# macro$DEC00DUM = as.integer(macro$Date == as.Date("2000-12-01"))

# require(lubridate)
# macro$JANDUM = as.integer(month(macro$Date) == 1)
# Añadir JANDUM a la regresión siguiente (+JANDUM)

S(lm_msoft_1_dummy  <- lm( ermsoft ~ ersandp + dinflation + rterm + APR00DUM + DEC00DUM , data = APT_ts ))

jarque.test(lm_msoft_1_dummy$residuals)
agostino.test(lm_msoft_1_dummy$residuals)
anscombe.test(lm_msoft_1_dummy$residuals)

############################## Multicolinealidad y especificación funcional ##############################

cor(APT_ts[-(1:2),c("ersandp", "dprod","dcredit","dinflation","dmoney","dspread","rterm")])
vif(lm_msoft_0)

resettest(lm_msoft_0,power = 2)
resettest(lm_msoft_0,power = 2:3)

############################## Contrastes de cambio estructural #######################

S(lm_msoft_1_dummy  <- lm( ermsoft ~ ersandp + dinflation + rterm + APR00DUM + DEC00DUM , data = APT_ts ))
S(lm_msoft_1  <- lm( ermsoft ~ ersandp + dinflation + rterm , data = APT_ts ))
anova(lm_msoft_1, lm_msoft_1_dummy)

sbtest = Fstats(formula(lm_msoft_1_dummy),data = APT_ts)
plot(sbtest$Fstats) # Desde el 15% hasta el 85% de los datos
sbtest[["Fstats"]]
nFstats <- NROW(sbtest$Fstats) #dimensión del vector: T=383 ; 383x0.15=57.45 ≈ 57 
# Fstats comienzan en: Mayo 1986 (tener en cuenta los 2 NAs de dinflation) + 57 = 59 (Enero 1991)
Fs_CHOW <- ts(sbtest$Fstats, start=c(1991,1), frequency = 12)
Fs_CHOW
plot(Fs_CHOW)

# localización temporal de los Fstats: 
enero96 = match(as.Date("1996-01-01"),macro$Date)
chow = sbtest$Fstats[enero96-2-57]
chow
1-pchisq(chow,sbtest$nreg)

# Test SupWald
sctest(sbtest)
bp = which.max(sbtest$Fstats)+59
macro$Date[bp]
# Test CUSUM
plot(efp(lm_msoft_1_dummy,data=APT_ts))
#

