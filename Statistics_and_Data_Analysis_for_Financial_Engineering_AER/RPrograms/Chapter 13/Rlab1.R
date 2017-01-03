library(AER)
data("USMacroG")
MacroDiff= apply(USMacroG,2,diff)
postscript("USMacroscatterplot.ps",width=6,height=6)
pairs(cbind(consumption,dpi,cpi,government,unemp))
graphics.off()
fitLm1 = lm(consumption~dpi+cpi+government+unemp)
summary(fitLm1)
confint(fitLm1)
anova(fitLm1)

library(MASS)
fitLm2 = stepAIC(fitLm1)
summary(fitLm2)
AIC(fitLm1)
AIC(fitLm2)
AIC(fitLm1)-AIC(fitLm2)

library(car)
vif(fitLm1)
vif(fitLm2)


postscript("USMacroCRPlot.ps",width=6,height=6)
par(mfrow=c(2,2))
sp = 0.8
cr.plot(fitLm1,dpi,span=sp,col="black")
cr.plot(fitLm1,cpi,span=sp,col="black")
cr.plot(fitLm1,government,span=sp,col="black")
cr.plot(fitLm1,unemp,span=sp,col="black")
graphics.off()





