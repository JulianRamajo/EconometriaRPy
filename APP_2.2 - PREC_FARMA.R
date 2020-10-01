setwd("~/Documents/R/_ECOMET_code")
library(readr)
PREC_FARMA <- read_csv("PREC_FARMA.csv")
library(car)
library(sfsmisc)
#
mod_lin <- lm(P ~ GDPN + CVN, data=PREC_FARMA)
S(mod_lin)
cis <- confint(mod_lin)
cis
#
mod_log <- lm(log(P) ~ log(GDPN) + log(CVN), data=PREC_FARMA)
S(mod_log)
cis <- confint(mod_log)
cis
H_0 <- c("log(GDPN) = 1")
linearHypothesis(mod_log,H_0,test="F")
H_0 <- c("log(CVN) = -1")
linearHypothesis(mod_log,H_0,test="F")
#
b <- coef(mod_log)
confidenceEllipse(mod_log, segments=500, levels=c(0.95), col="black", 
                  fill=TRUE, 
                  axes=TRUE, ann=TRUE, xlab="GDPN", ylab="CVN", grid=TRUE)
box()
usr <- par("usr")
abline(v=cis[2, ], h=cis[3, ], lty=2)
lines(x=c(usr[1], b[2]), y=c(b[3], b[3]))
lines(x=c(b[2], b[2]), y=c(usr[3], b[3]))
par <- par("xpd"=TRUE)
p.arrows(cis[2, 1], usr[3], cis[2, 2], usr[3], lwd=3, fill="black", 
         xpd=TRUE, size=1.25)
p.arrows(cis[2, 2], usr[3], cis[2, 1], usr[3], lwd=3, fill="black", 
         xpd=TRUE, size=1.25)
p.arrows(usr[1], cis[3, 1], usr[1], cis[3, 2], lwd=3, fill="black", 
         xpd=TRUE, size=1.25)
p.arrows(usr[1], cis[3, 2], usr[1], cis[3, 1], lwd=3, fill="black", 
         xpd=TRUE, size=1.25)
par(par) 
#
mod_log_fact <- lm(log(P) ~ log(GDPN) + log(CVN) + PP + IPC + DPC, data=PREC_FARMA)
S(mod_log_fact)
#
anova(mod_log,mod_log_fact)
#
H_0 <- c("PP = 0", "IPC = 0", "DPC = 0")
linearHypothesis(mod_log_fact,H_0,test="F")

