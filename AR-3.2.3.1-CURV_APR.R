library(readr)
library(alr4)
#
CURV_APR <- read_csv("CURV_APR.csv")
CURV_APR_ts <- ts(CURV_APR, start=c(1), end = c(60), frequency = 1)
plot(CURV_APR_ts)
#
curv.apr.dat <- cbind(CURV_APR_ts,lag(CURV_APR_ts[,2],-1))
curv.apr <- data.frame(curv.apr.dat)
names(curv.apr) <- c("T", "X", "LX")
summary(curv.apr)
#
reg_nl <- nls(T~c1*(X^(c2+1)-LX^(c2+1)), data=curv.apr, start=list(c1=10, c2=-0.5))
summary(reg_nl)
coef(reg_nl)
gamma <- coef(reg_nl)[[1]]
delta <- coef(reg_nl)[[2]]
#
curve(gamma*x^delta, from=0, to=100, xlab="Unidades producidas", ylab="Costes medios" )
#

