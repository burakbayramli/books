library(AER)
data(CPS1988)
attach(CPS1988)
#############  Model 1  ##########
fitLm1 = lm(wage~education+experience+ethnicity)
summary(fitLm1)

postscript("CPS1988Model1Residuals1.ps",width=5,height=7)
par(mfrow=c(3,2))
resid1 = rstudent(fitLm1)
plot(fitLm1$fit,resid1,
  ylim=c(-1500,1500),main="(a)")
lines(lowess(fitLm1$fit,resid1,f=.2),lwd=5,col="red")
abline(h=0,col="blue",lwd=5)

plot(fitLm1$fit,abs(resid1),
  ylim=c(0,1500),main="(b)")
lines(lowess(fitLm1$fit,abs(resid1),f=.2),lwd=5,col="red")
abline(h=mean(abs(resid1)),col="blue",lwd=5)

qqnorm(resid1,datax=F,main="(c)")
qqline(resid1,datax=F,lwd=5,col="blue")

plot(education,resid1,ylim=c(-1000,1500),main="(d)")
lines(lowess(education,resid1),lwd=5,col="red")
abline(h=0,col="blue",lwd=5)

plot(experience,resid1,ylim=c(-1000,1500),main="(e)")
lines(lowess(experience,resid1),lwd=5,col="red")
abline(h=0,col="blue",lwd=5)
graphics.off()

############### Model 2  ###############

fitLm2  = lm(log(wage)~education+experience+ethnicity)
summary(fitLm2)
resid2=rstudent(fitLm2)
postscript("CPS1988Model1Residuals2.ps",width=5,height=7)
par(mfrow=c(3,2))

plot(fitLm2$fit,resid2,
  main="(a)")
lines(lowess(fitLm2$fit,resid2,f=.2),lwd=5,col="red")
abline(h=0,col="blue",lwd=5)

plot(fitLm2$fit,abs(resid2),
  main="(b)")
lines(lowess(fitLm2$fit,abs(resid2),f=.2),lwd=5,col="red")
abline(h=mean(abs(resid)),col="blue",lwd=5)

qqnorm(resid2,datax=F,main="(c)")
qqline(resid2,datax=F,lwd=5,col="blue")

plot(education,resid2,main="(d)")
lines(lowess(education,resid2),lwd=5,col="red")
abline(h=0,col="blue",lwd=5)

plot(experience,resid2,main="(e)")
lines(lowess(experience,resid2),lwd=5,col="red")
abline(h=0,col="blue",lwd=5)
graphics.off()


##################  Model 3  ###############

fitLm3  = lm(log(wage)~education+poly(experience,2)+ethnicity)
summary(fitLm3)
resid3 = rstudent(fitLm3)

postscript("CPS1988Model1Residuals3.ps",width=5,height=7)
par(mfrow=c(3,2))

plot(fitLm3$fit,resid3,
  main="(a)")
lines(lowess(fitLm3$fit,fitLm3$resid,f=.2),lwd=5,col="red")
abline(h=0,col="blue",lwd=5)

plot(fitLm3$fit,abs(fitLm3$resid),
  main="(b)")
lines(lowess(fitLm3$fit,abs(resid3),f=.2),lwd=5,col="red")
abline(h=mean(abs(fitLm3$resid)),col="blue",lwd=5)

qqnorm(resid3,datax=F,main="(c)")
qqline(resid3,datax=F,lwd=5,col="blue")

plot(education,resid3,main="(d)")
lines(lowess(education,resid3),lwd=5,col="red")
abline(h=0,col="blue",lwd=5)

plot(experience,fitLm3$resid,main="(e)")
lines(lowess(experience,resid3),lwd=5,col="red")
abline(h=0,col="blue",lwd=5)
graphics.off()



##################  Model 4  ###############

fitLm4  = lm(log(wage)~poly(education,2)+poly(experience,2)+ethnicity)
summary(fitLm4)
resid4 = rstudent(fitLm4)

postscript("CPS1988Model1Residuals4.ps",width=5,height=7)
par(mfrow=c(3,2))

plot(fitLm4$fit,resid4,
  main="(a)")
lines(lowess(fitLm4$fit,resid4,f=.2),lwd=5,col="red")
abline(h=0,col="blue",lwd=5)

plot(fitLm4$fit,abs(resid4),
  main="(b)")
lines(lowess(fitLm4$fit,abs(resid4),f=.2),lwd=5,col="red")
abline(h=mean(abs(resid4)),col="blue",lwd=5)

qqnorm(resid4,datax=F,main="(c)")
qqline(resid4,datax=F,lwd=5,col="blue")

plot(education,resid4,main="(d)")
lines(lowess(education,resid4),lwd=5,col="red")
abline(h=0,col="blue",lwd=5)

plot(experience,resid4,main="(e)")
lines(lowess(experience,resid4),lwd=5,col="red")
abline(h=0,col="blue",lwd=5)
graphics.off()


eduQuad = poly(education,2) %*% fitLm4$coef[2:3]
experQuad = poly(experience,2) %*% fitLm4$coef[4:5]
postscript("CPSEffectsPlots.ps",width=6,height=3.5)
par(mfrow=c(1,2))
plot(education,eduQuad)
plot(experience,experQuad)
graphics.off()




postscript("CPS1988Model1Diagnostics4.ps",width=6,height=2.5)
par(mfrow=c(1,3))
plot(hatvalues(fitLm4))
plot(sqrt(cooks.distance(fitLm4)))
halfnorm(sqrt(cooks.distance(fitLm4)))
graphics.off()

n = length(experience)
2*6/n

sqrt(cooks.distance(fitLm4))[15380:15390]
CPS1988[15387,]
boxplot(wage,ylim=c(0,1000))
max(wage)*52
7716.05 *52









