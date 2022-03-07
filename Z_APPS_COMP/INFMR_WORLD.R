# Mortalidad infantil en el mundo
INFMR <- read.table("INFMR_WORLD.txt", header=TRUE)
#
# Análisis exploratorios de los datos (EDA)
#
library(DataExplorer)
library(ggplot2)
#
# create_report(INFMR, y = "infmr")
introduce(INFMR)
plot_intro(INFMR) # Descripción gráfica de las propiedades de la base de datos
plot_missing(INFMR) # Descripción gráfica de los datos ausentes (missing data)
plot_histogram(INFMR) # Histograma de las variables continuas
plot_density(INFMR) # Densidades estimadas de las variables continuas
plot_qq(INFMR) # Gráficas QQ (quantile-quantile) de las variables continuas
plot_qq(INFMR, by = "region") # Desagregado por `region`
plot_boxplot(INFMR, by = "region") # Diagrama de puntos desagregado por `region`
plot_bar(INFMR) # Distribución de frecuencias de las variables discretas
plot_bar(INFMR, with = "infmr")
# Diagrama de puntos de `infmr` con respecto al resto de variables
plot_scatterplot(INFMR, by = "infmr")
plot_scatterplot(split_columns(INFMR)$continuous, by = "infmr") # Sólo las variables continuas
# Mapa de calor (correlaciones) de las variables continuas (se ha quitado la columna `region`)
plot_correlation(INFMR[, -c(5)])
# Análisis de componentes principales de las variables explicativas (continuas y discretas)
plot_prcomp(INFMR[, -c(2)])
#
library(autoEDA)
EDA_INFMR <- autoEDA(INFMR, y="infmr")
#
library(RcmdrMisc)
#
par(mfrow=c(2, 2))
Hist(INFMR$infmr, xlab="Tasa de mortalidad infantil (por 1000)", ylab="Frecuencia", 
     col="gray", main="(a)")
Boxplot(~infmr, data=INFMR, main="(b)", ylab="Mortalidad infantil")
densityPlot(INFMR$infmr, from=0, normalize=TRUE, 
            xlab="Mortalidad infantil", main="(c)")
qqPlot(~infmr, data=INFMR, ylab="Mortalidad infantil", 
       xlab="Cuantiles Normal", main="(d)",
       id=list(method=c(TRUE, rep(FALSE, 132), TRUE)), col.lines="black")
par(par)
#
# Análisis econométrico (regresión MCO del modelo lineal con las variables sin transformar)
#
library(car)
library(effects)
#
linMod <- lm(infmr ~ gdp  + gini + hexp, data=INFMR)
S(linMod)
#
# Added-Variable plots
#
avPlots(lm(infmr ~ gdp  + gini + hexp, data=INFMR))
#
# Residual plots
#
residualPlots(linMod)
#
# Predictor-effect plots
#
plot(predictorEffects(linMod))
plot(predictorEffects(linMod, ~ gdp , partial.residuals=T))
plot(predictorEffects(linMod, ~ gini , partial.residuals=T))
plot(predictorEffects(linMod, ~ hexp , partial.residuals=T))
#
# Marginal-Model plots
#
marginalModelPlots(linMod)   # Alias: mmps
#
# Marginal/Conditional plots (marginal plot (both variables centered) + added-variable plot)
#
mcPlots(linMod)
#
# Validación global de las hipótesis del modelo (A1-A4)
#
library(gvlma)
#
gv_linMod <- gvlma(lm(infmr ~ gdp  + gini + hexp, data=INFMR)) 
summary(gv_linMod) 
par(mfrow=c(2,2))
plot(gv_linMod, onepage = FALSE)
#
# Modelo cuadrático en GDP y gasto sanitario
#
linMod.poly <- update(linMod, . ~ poly(gdp, 2, raw=TRUE) + gini + poly(hexp, 2, raw=TRUE))
S(linMod.poly)
#
gv_linMod.poly <- gvlma(linMod.poly) 
summary(gv_linMod.poly) 
#
# Transformación de la variable dependiente
#
library(trafo)
#
assumptions(linMod)
#
linMod_trafoBC <- trafo_lm(linMod, trafo = "boxcox")
S(linMod_trafoBC)
diagnostics(linMod_trafoBC)
# plot(linMod_trafoBC)
#
linMod_trafoLOG <- trafo_lm(linMod, trafo = "log")
S(linMod_trafoLOG)
diagnostics(linMod_trafoLOG)
# plot(linMod_trafoLOG)
#
# Comparación de las dos versiones
linMod_BoxCox <- boxcox(linMod)
linMod_Log <- logtrafo(linMod)
linMod_comp <- trafo_compare(object = linMod, trafos = list(linMod_BoxCox, linMod_Log))
diagnostics(linMod_comp)
#
# Transformación de las variables explicativas (continuas)
#
boxTidwell(log(infmr) ~ gdp + gini + hexp, data=INFMR)
# boxTidwell(log(infmr) ~ gdp + gini + hexp, other.x=~factor(region), data=INFMR)
#
# Nueva regresión Box-Tidwell para las variables explicativas
#
linMod_BT <- lm(log(infmr) ~ gdp + gini + hexp + I(gdp*log(gdp)) + I(gini*log(gini)) + I(hexp*log(hexp)), data=INFMR)
S(linMod_BT)
#
# Familia de transformaciones Box-Cox
#
par(mfrow=c(1, 1))
#
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
#
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
par(par)
#
#  Análisis de simetría a través de boxplots
#
symbox(~infmr, data=INFMR, xlab=expression("Potencias,"~lambda), ylab="", 
       powers = c(-1, -0.5, -0.25, 0, 0.25, 0.5, 1))
mtext(2, 1, text=expression(t[BC]("Mortalidad infantil",~lambda)))
#
#  Log-transformación de la mortalidad infantil
#
densityPlot(~log(infmr), data=INFMR, adjust=0.75, xlab="log(Mortalidad infantil)", ylab="Densidad")
basicPowerAxis(0, side="above", at=c(1, 5, 10, 20, 50, 100), 
               axis.title="Tasa de mortalidad infantil (por 1000)")
#
# Estimación del parámetro lambda de transformación de la variable dependiente
#
S(pTy <- powerTransform(infmr ~ 1, data=INFMR))
pTy$lambda # Estimación de lambda 
sqrt(pTy$invHess) # Desviación típica de la estimación de lambda
#
# Estimación conjunta del parámetro lambda de transformación de las variables continuas del modelo
#
S(pTyXs <- powerTransform(cbind(infmr, gdp, gini, hexp) ~ 1, data=INFMR))
#
# Matriz de diagrama de puntos (scatterplot matrix) de los datos originales
#
scatterplotMatrix(~infmr + gdp + gini + hexp, data=INFMR, 
                  var.labels=c("Mortalidad infantil", "PIB per cápita", 
                               "Coeficiente de Gini", "Gasto sanitario"),
                  smooth=list(smoother=loessLine, var=FALSE, lwd.smooth=3), 
                  col="black")
#
# Matriz de diagrama de puntos (scatterplot matrix) de los datos transformados
#
scatterplotMatrix(~log(infmr) + basicPower(gdp, 0.2) + log(gini) + log(hexp), data=INFMR,
                  var.labels=c(expression(log("Mortalidad infantil")), 
                               expression("PIB per cápita"^0.2), 
                               expression(log("Coeficiente de Gini")), 
                               expression(log("Gasto sanitario"))),
                  smooth=list(smoother=loessLine, var=FALSE, lwd.smooth=3), 
                  col="black")
#
# Regresión MCO del modelo BC-transformado
#
linMod_BC <- lm(log(infmr) ~ basicPower(gdp, 0.2) + log(gini) + log(hexp), data=INFMR)
S(linMod_BC)
#
avPlots(lm(log(infmr) ~ basicPower(gdp, 0.2) + log(gini) + log(hexp), data=INFMR))
#
# Validación global del modelo BC-transformado
library(gvlma)
#
gv_linMod_BC <- gvlma(lm(log(infmr) ~ basicPower(gdp, 0.2) + log(gini) + log(hexp), data=INFMR)) 
summary(gv_linMod_BC) 
#
# Regresión MCO del modelo logarítmico
#
linMod_log <- lm(log(infmr) ~ log(gdp) + log(gini) + log(hexp), data=INFMR)
S(linMod_log)
#
avPlots(lm(log(infmr) ~ log(gdp) + log(gini) + log(hexp), data=INFMR))
#
# Validación global del modelo logarítmico
library(gvlma)
#
gv_linMod_log <- gvlma(lm(log(infmr) ~ log(gdp) + log(gini) + log(hexp), data=INFMR)) 
summary(gv_linMod_log) 
#
# Valores atípicos
#
library(MASS)
library(car)
#
S(linMod_BC <- lm(log(infmr) ~ basicPower(gdp, 0.2) + log(gini) + log(hexp), data=INFMR))
#
# Estadísticos de diagnóstico
#
max(hatvalues(linMod_BC))
which.max(hatvalues(linMod_BC))
outlierTest(linMod_BC)
max(cooks.distance(linMod_BC))
which.max(cooks.distance(linMod_BC))
dffits(linMod_BC)[114]
which.max(abs(dffits(linMod_BC)))
round(dfbeta(linMod_BC)[114, ], 5)
round(dfbetas(linMod_BC)[114, ], 5)
influencePlot(linMod_BC, xlab="Hatvalues")
#
#
gv_linMod_BC <- gvlma(lm(log(infmr) ~ basicPower(gdp, 0.2) + log(gini) + log(hexp), data=INFMR)) 
del_linMod_BC <- deletion.gvlma(gv_linMod_BC) 
summary(del_linMod_BC)
par(mfrow=c(2,2))
plot(del_linMod_BC, onepage = FALSE)
#
S(linMod_BC)
# 
# QQ plot of studentized residuals
#
qqPlot(linMod_BC, reps=1000, id=FALSE, col.lines="black", 
       ylab="Ordered Studentized Residuals")
#
# Density plot of studentized residuals
#
densityPlot(rstudent(linMod_BC), adjust=0.75, n=1000, xlab="Studentized Residuals")
#
# Constructed-variable plot
#
avPlots(linMod_BC)
#
# Plots of studentized residuals vs fitted values 
#
scatterplot(fitted(linMod_BC), rstudent(linMod_BC), smooth=list(span=2/3, 
                                lwd.smooth=3, lwd.spread=3), regLine=FALSE,
            boxplots=FALSE, col=c("black", "black"), main="(b)",
            xlab="Fitted Values", ylab="Studentized Residuals")
abline(0, 0)
#
# Spread-level plots 
#
spreadLevelPlot(linMod_BC, smooth=FALSE, col.lines="black")
#
# Breusch-Pagan tests of nonconstant error variance 
#
ncvTest(linMod_BC)
ncvTest(linMod_BC, ~ region)
ncvTest(linMod_BC, ~ gdp + gini + hexp)
#
# Robust and bootstrapped SEs
#
S(linMod_BC, vcov.=hccm(linMod_BC))
#
boot.linMod_BC <- Boot(linMod_BC, R=1000)
S(linMod_BC, vcov.=vcov(boot.linMod_BC))
#
# Component-plus-residuals and CERES plots
#
crPlots(linMod_BC) # convential
crPlots(linMod_BC, order=2) # quadratic
ceresPlots(linMod_BC)
#
# Component-plus-residual plots frente a las variables Xs sin transformar
#
plot(Effect("gdp", linMod_BC, residuals=TRUE), 
     lines=list(col=c("black", "black"), lty=2), 
     axes=list(grid=TRUE), confint=FALSE, 
     partial.residuals=list(plot=TRUE, smooth.col="black", lty=1, span=3/4), 
     xlab="GDP per Capita", ylab="Component+Residual")

plot(Effect("gini", linMod_BC, residuals=TRUE), 
     lines=list(col=c("black", "black"), lty=2), 
     axes=list(grid=TRUE), confint=FALSE, 
     partial.residuals=list(plot=TRUE, smooth.col="black", lty=1, span=3/4), 
     xlab="Coeficiente de Gini", ylab="Component+Residual")

plot(Effect("hexp", linMod_BC, residuals=TRUE), 
     lines=list(col=c("black", "black"), lty=2), 
     axes=list(grid=TRUE), confint=FALSE, 
     partial.residuals=list(plot=TRUE, smooth.col="black", lty=1, span=3/4),
     xlab="Gasto sanitario", ylab="Component+Residual")
#
# Término cuadrático en el logaritmo del gasto sanitario
#
linMod_BC.poly <- lm(log(infmr) ~ basicPower(gdp, 0.2) + log(gini) + poly(log(hexp), 2, raw=TRUE) , data=INFMR)
S(linMod_BC.poly)
#
anova(linMod_BC,linMod_BC.poly)
Anova(linMod_BC,linMod_BC.poly)
#
# Términos de interacción por continente (variable region)
# 
# Reordenación de los factores
#
INFMR$region <- factor(INFMR$region,
                       levels=c("Europe", "America", "Oceania", "Asia", "Africa"))
# Boxplots de las variables del modelo (y, Xs) versus region (continentes)

Boxplot(infmr ~ region, data=INFMR, id=list(location="lr"), 
        ylab="Tasa de mortalidad", xlab="Región del mundo (continente")
#
Boxplot(gdp ~ region, data=INFMR, id=list(location="lr"), 
        ylab="PIB per cápita", xlab="Región del mundo (continente")
#
Boxplot(gini ~ region, data=INFMR, id=list(location="lr"), 
        ylab="Coeficiente de Gini", xlab="Región del mundo (continente")
#
Boxplot(hexp ~ region, data=INFMR, id=list(location="lr"), 
        ylab="Gasto sanitario", xlab="Región del mundo (continente")
#
# Modelo econométrico con términos de interacción
#
linMod_BC.inter <- lm(log(infmr) ~ region*(basicPower(gdp, 0.2) + log(gini) + log(hexp)), data=INFMR)
S(linMod_BC.inter)
#
anova(linMod_BC, linMod_BC.inter)
Anova(linMod_BC.inter)

# Predictor-effect plots para el modelo con términos de interacción por continente

plot(predictorEffect("gdp", linMod_BC.inter, residuals=TRUE), 
     lines=list(col=c("black", "black"), lty=2), 
     axes=list(grid=TRUE), 
     partial.residuals=list(plot=TRUE, smooth.col="black", lty=1, span=1.0, pch=20),
     xlab="GDP per Capita ($1000s)", ylab="log(Mortalidad infantil)", 
     lattice=list(layout=c(5, 1)))

plot(predictorEffect("gini", linMod_BC.inter, residuals=TRUE), 
     lines=list(col=c("black", "black"), lty=2), 
     axes=list(grid=TRUE), 
     partial.residuals=list(plot=TRUE, smooth.col="black", lty=1, span=1.0, pch=20),
     xlab="Coefciente de Gini", ylab="log(Mortalidad infantil)", 
     lattice=list(layout=c(5, 1)))

plot(predictorEffect("hexp", linMod_BC.inter, residuals=TRUE), 
     lines=list(col=c("black", "black"), lty=2), 
     axes=list(grid=TRUE), 
     partial.residuals=list(plot=TRUE, smooth.col="black", lty=1, span=1.0, pch=20),
     xlab="Gasto sanitario", ylab="log(Mortalidad infantil)", 
     lattice=list(layout=c(5, 1)))

# Comando simple para las tres gráficas anteriores

plot(predictorEffects(linMod_BC.inter, ~ gdp + gini + hexp, residuals=TRUE), 
     partial.residuals=list(span=0.9))

# Marginal model plots
par <- par(mfrow=c(2, 2))
mmp(linMod_BC.inter, variable=INFMR$gdp, col.line=c(data=gray(.50), model="black"), 
    xlab="PIB per cápita", ylab="log(Mortalidad infattil)",
    col=gray(.50), main="(a)", key=FALSE, 
    id=list(n=2, method="mahal", cex=0.7))
mmp(linMod_BC.inter, variable=INFMR$gini, col.line=c(data=gray(.50), model="black"), 
    xlab="Coeficiente de Gini", ylab="log(Mortalidad infantil)",
    col=gray(.50), main="(b)", key=FALSE, 
    id=list(n=2, method="mahal", cex=0.7))
mmp(linMod_BC.inter, variable=INFMR$hexp, col.line=c(data=gray(.50), model="black"), 
    xlab="Gasto sanitario", ylab="log(Mortalidad infanttil)",
    col=gray(.50), main="(c)", key=FALSE, 
    id=list(n=2, method="mahal", cex=0.7))
mmp(linMod_BC.inter, col.line=c(data=gray(.50), model="black"), 
    ylab="log(Mortalidad infantil)",
    col=gray(.50), main="(d)", key=FALSE, id=list(n=1, method="mahal", cex=0.7))
par(par)
#
