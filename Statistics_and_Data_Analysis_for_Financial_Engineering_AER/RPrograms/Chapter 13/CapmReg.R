data(Capm,package="Ecdat")
attach(Capm)

#plot(Capm)
rfood2 = rfood/100
rmrf2 = rmrf/100

par(mfrow=c(1,1))

postscript("capm_regression_xyplot.ps",width=6,height=5)
plot(rmrf2,rfood2,ylab="Food industry excess return",
     xlab="Market excess return")
graphics.off()

fit = lm(rfood2~rmrf2)
summary(fit)



acf(residuals(fit))

par(mfrow=c(2,2))
plot(fit,lwd=2,cex=1.5,cex.lab=1.2)


summary(fit)