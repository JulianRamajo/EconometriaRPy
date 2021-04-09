library(readr)
APT_MICROSOFT <- read_csv("~/Documents/GitHub/EcoMetricsR/APT_MICROSOFT.csv")
# MSFT <- ts(APT_MICROSOFT, start=c(1986,3), frequency = 12) # Método ts clásico
# MSFT_2 <- window(MSFT, start=c(1986,4), end=c(2007,4))    # Para los NAs de 1986Q3
MSFT <- read.zoo(APT_MICROSOFT)  # Da un resultado erróneo debido al "index" usado (primera columna)
n <- nrow(APT_MICROSOFT)
datetimes <- seq(as.POSIXct("1986-03-01"), length.out = n, by = "months")
MSFT <- xts(APT_MICROSOFT[,2:13], datetimes)
summary(MSFT)
plot.xts(MSFT[,"R_MICROSOFT"])
plot.xts(MSFT[,"R_SP500"])
plot.xts(MSFT[,"R_USTB3M"])
MSFT$ER_MICROSOFT <- MSFT$R_MICROSOFT-MSFT$R_USTB3M
MSFT$ER_SP500 <- MSFT$R_SP500-MSFT$R_USTB3M
plot.xts(MSFT[,"ER_MICROSOFT"])
plot.xts(MSFT[,"ER_SP500"])
library(dynlm)
APT_MSFT <- dynlm(ER_MICROSOFT ~ ER_SP500 + DINF + DPROD + DM1 +  DCRED + DSPR + DSPCRED, data=MSFT)
summary(APT_MSFT)
# ...
