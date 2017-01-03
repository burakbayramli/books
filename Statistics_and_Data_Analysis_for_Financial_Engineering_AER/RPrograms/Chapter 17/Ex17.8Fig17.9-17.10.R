#  Example 17.8 and Figures 17.9 and 17.10

library("fEcofin")
returns = berndtInvest[,-c(1,11,18)]
ind_codes = as.factor(c(3,3,2,1,1,2, 3,3,1,2,2,3,1,2,3))
codes = as.matrix(model.matrix(~ind_codes))
codes[,1] =  1 - codes[,2] - codes[,3]
codes[,2:3] = codes[,1:2]
codes[,1] = 1
betas = as.data.frame(codes[1:15,1:3])
colnames(betas) = c("intercept","tech","oil")

betas

factors = matrix(0,nrow=120,ncol=3)
for (i in 1:120)
{
return_data = cbind(t(returns[i,]),betas)
colnames(return_data)[1] = "return"
lmfit = lm(return~.-1,data=return_data)
factors[i,]=lmfit$coef
}

print(sd(factors),digits=4)

postscript("berndt_cross_section_factors.ps",width=7,height=3)  #  Figure 17.9
par(mfrow=c(1,3),cex.axis=1.08,cex.lab=1.08,cex.main=1.05)
plot(factors[,1],type="b",lty="dotted",
   lwd=2,xlab="month",ylab="factor",main="market")
plot(factors[,2],lty="dotted",lwd=2,type="b",
   xlab="month",ylab="factor",main="technology")
plot(factors[,3],lty="dotted",lwd=2,type="b",
   xlab="month",ylab="factor",main="oil")
graphics.off()

colnames(factors) = c("market","tech","oil")
cor(factors)
sqrt(diag(cov(factors)))

postscript("berndt_cross_section_factors_acf.ps",width=6,height=6)  #  Figure 17.10
acf(factors,ylab="",xlab="lag")
graphics.off()