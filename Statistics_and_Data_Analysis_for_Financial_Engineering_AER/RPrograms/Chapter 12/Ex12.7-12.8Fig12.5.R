library(leaps)
dat = read.table(file="WeekInt.txt",header=T)
attach(dat)
cm10_dif = diff( cm10 )
aaa_dif = diff( aaa )
cm30_dif = diff( cm30 )
ff_dif = diff( ff )
subsets = regsubsets(aaa_dif~.,
  data=as.data.frame(cbind(cm10_dif,cm30_dif,ff_dif)),nbest=1)
summary(subsets)                         #  Example 12.7

postscript("WeekInt_model_selection.ps",width=6,height=3)  #  Figure 12.5
par(mfrow=c(1,3),lab=c(2,5,3),pch=19)
plot(1:3,b$bic,type="b",xlab="number of variables",
   ylab="BIC",cex=2.5)
plot(1:3,b$cp,type="b",xlab="number of variables",
   ylab="Cp",cex=2.5)
plot(1:3,b$adjr2,type="b",xlab="number of variables",
   ylab="adjusted R2",cex=2.5)
graphics.off()


vif(lm(aaa_dif~cm10_dif+cm30_dif+ff_dif))   #  Example 12.8
