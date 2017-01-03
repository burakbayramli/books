dat = read.table(file="WeekInt.txt",header=T)
attach(dat)
cm10_dif = diff(cm10)
aaa_dif = diff(aaa)
cm30_dif = diff(cm30)
ff_dif = diff(ff)

fit1 = lm(aaa_dif ~ cm10_dif)
fit2 = lm(aaa_dif~cm10_dif+cm30_dif)

summary(fit2)          #  Example 12.4
anova(fit2)            #  Section 12.4.1

postscript("weekint_splotm.ps",width=6,height=5)     #  Figure 12.4
plot(as.data.frame(cbind(aaa_dif,cm10_dif,cm30_dif,ff_dif)))
graphics.off()

anova(fit1,fit2)       #  Example 12.5

