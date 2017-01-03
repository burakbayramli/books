#  Example 12.2 and Figure 12.3

data(Capm,package="Ecdat")
attach(Capm)
rfood = rfood/100
rmrf = rmrf/100

postscript("capm_regression_xyplot.ps",width=6,height=5)  #  Figure 12.3
par(mfrow=c(1,1))
plot(rmrf,rfood,ylab="Food industry excess return",
     xlab="Market excess return")
graphics.off()

fit = lm(rfood~rmrf)
summary(fit) #  Example 12.2

