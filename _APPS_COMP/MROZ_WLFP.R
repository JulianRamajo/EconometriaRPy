# R script
library(car)
# logistic regression for Mroz data
Mroz <- read.table("MROZ_WLFP.txt", header=TRUE)
# 
mod.mroz <- glm(lfp ~ k5 + k618 + age +wc + hc + inc + lwg, family=binomial, data=Mroz)
S(mod.mroz)

# Influence bubble plot for Mroz logistic regression

influencePlot(mod.mroz, id=FALSE, xlab="Hatvalues")   

# Bonferroni outlier test for Mroz logistic regression

outlierTest(mod.mroz)

# Added-variable plots for Mroz logistic regression

avPlots(mod.mroz, id=FALSE, col.lines="black", main="")

# Component-plus-residual plots for Mroz logistic regression

crPlots(mod.mroz, ~ k5 + k618 + age + inc + lwg, 
        col.lines=c("black", "black"), 
        ylab="Component + Residual", main="", layout=c(2, 3))

# Test for nonlinearity of k5 in Mroz logistic regression

mod.mroz.2 <- update(mod.mroz, . ~ . - k5 + as.factor(k5))
anova(mod.mroz, mod.mroz.2, test="LRT")

# Marginal model plots for Mroz logistic regression

mmps(mod.mroz, col.line=c(data=gray(.50), model="black"), col=gray(.50),  
     main="", key=FALSE)
# warning is innocuous

# variance inflation factors for Mroz logistic regression

vif(mod.mroz)
