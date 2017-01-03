#  Example 12.1 and Figures 12.2 and 12.4

dat = read.table(file="WeekInt.txt",header=T)
attach(dat)
aaa_dif = diff(aaa)
cm10_dif = diff(cm10)

postscript("cm10aaa.ps",width=6,height=5)  #  Figure 12.2
plot(cm10_dif,aaa_dif,xlab="change in 10YR T rate",
   ylab="change in AAA rate")
graphics.off()

fit1 = lm(aaa_dif ~ cm10_dif)
summary(fit1)


