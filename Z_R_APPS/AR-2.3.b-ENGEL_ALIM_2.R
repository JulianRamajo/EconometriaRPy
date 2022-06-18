library(readr)
library(car)
ENGEL_ALIM_2 <- read_csv("ENGEL_ALIM_2.csv")
View(ENGEL_ALIM_2)
attach(ENGEL_ALIM_2)
WALIM <- GALIM/GTOTAL
plot(GTOTAL, WALIM)
#
S(lm_lin <- lm(WALIM ~ GTOTAL))
S(lm_inv <- lm(WALIM ~ I(1/GTOTAL)))
S(lm_semilog <- lm(WALIM ~ log(GTOTAL)))
#
# Curva de Engel Box-Tidwell 
#
t_BT <- boxTidwell(WALIM ~ GTOTAL)
print(t_BT)
S(lm_BT <- lm(WALIM ~ basicPower(GTOTAL, -0.307)))
#
#
# Transformación de Box-Cox
#
par(mfrow=c(1, 1))

n <- 500
x <- seq(0.1, 3, length=n)
x1 <- bcPower(x, 1)
x0.5 <- bcPower(x, 0.5)
x0 <- bcPower(x, 0)
xm0.5 <- bcPower(x, -0.5)
xm1 <- bcPower(x, -1)
x2 <- bcPower(x, 2)
x3 <- bcPower(x, 3)
xlim <- range(c(x1, x0.5, x0, xm0.5, xm1, x2, x3))

plot(range(x)+ c(-0.6, 0.5), c(-5, 10), type="n", xlab="", ylab="", las=1)
usr <- par("usr")
text(usr[2], usr[3] - 1, label="x", xpd=TRUE)
text(usr[1] - 0.2, usr[4] + 0.75, label=expression(t[BC](x, lambda)), xpd=TRUE)
lines(x, x1, lwd=2)
text(x[n]+0.0625, x1[n], labels=expression(lambda == 1), adj=c(0, 0.2))
lines(x, x2, lwd=2)
text(x[n]+0.0625, x2[n], labels=expression(lambda == 2), adj=c(0, 0.2))
lines(x, x3, lwd=2)
text(x[n]+0.0625, x3[n], labels=expression(lambda == 3), adj=c(0, 0.2))
lines(x, x0.5, lwd=2)
text(x[1]-0.025, x0.5[1], labels=expression(lambda == 0.5), adj=c(1, 0.3))
lines(x, x0, lwd=2)
text(x[1]-0.025, x0[1], labels=expression(lambda == 0), adj=c(1, 0.3))
lines(x, xm0.5, lwd=2)
text(x[1]-0.025, xm0.5[1], labels=expression(lambda == -0.5), adj=c(1, 0.3))
lines(x=c(1, 1), y=c(usr[3], 0), lty=2)
lines(x=c(usr[1], 1), y=c(0, 0), lty=2)

#