#  Example 14.9 and Figures 14.13 and 14.14

library("AER")
data("CreditCard")  #  Greene (2008) pages 304 and 1091
CreditCard_clean = CreditCard[CreditCard$age>18,]
attach(CreditCard_clean)
names(CreditCard)

postscript("CreditCardHist.ps",width=5.5,height=5.5)  #  Figure 14.12
par(mfrow=c(3,3)) 
hist(reports,main="reports")
hist(income,main="income")
hist(share,main="share")
hist(age,main="age")
owner2 = c( sum((owner=="yes")),sum((owner=="no")))
hist(as.numeric(owner), main="owner",
   breaks=2,xlab=" no   yes",axes=F,ylab="")
h=hist(dependents,main="dependents",breaks=(0:7)-.5)
hist(months,main="months")
hist(log(share),main="log(share)")
hist(log(reports+1),main="log(reports+1)")
graphics.off()

fit1= glm(card~log(reports+1)+income+log(share)+age+owner+dependents+months,
   family="binomial",data=CreditCard_clean)
summary(fit1)
stepAIC(fit1)

log_reports_c = log(reports+1)
log_reports_c = log_reports_c - mean(log_reports_c)
income_c = income - mean(income)
log_share_c = log(share) - mean(log(share))
dependents_c = dependents - mean(dependents)
glm_fit03 = glm(card~log_reports_c+income_c+log_share_c+dependents_c,
  family="binomial",data=CreditCard_clean)
summary(glm_fit03)

reports_grid = 0:14
reports_grid2 = log(reports_grid+1) - mean(log(reports+1))
income_grid = seq(min(income),max(income),.01)
share_grid = exp(seq(min(log(share)),max(log(share)),.01))
share_grid2 = log(share_grid) - mean(log(share))

postscript("CreditCards_effects.ps",width=6,height=5)  #  Figure 14.13
par(mfrow=c(2,2))
plot(reports_grid, plogis(9.5238 -2.8953 * reports_grid2 ),type="b",
   lwd=2,xlab="reports",ylab="P(accept)",ylim=c(0,1))
plot(income_grid, plogis(9.5238 + 0.8717 *(income_grid-mean(income)) ),type="l",
   cex=2,lwd=2,xlab="income",ylab="P(accept)",ylim=c(0,1))
plot(log(share_grid), plogis(9.5238  + 3.3102  * share_grid2 ),type="l",
   cex=2,lwd=2,xlab="log(share)",ylab="P(accept)",ylim=c(0,1))
plot(0:6,plogis(9.5238 - .5506*((0:6)-mean(dependents)) ),type="b",
   lwd=2,xlab="dependents",ylab="P(accept)",ylim=c(0,1))
graphics.off()

postscript("CreditCards_share_card.ps",width=5.5,height=5.5)  #  Figure 14.14
par(mfrow=c(2,2))
plot(log(share),as.numeric(card)-1,ylab="card",main="(a)" )
plot(log(share),reports,main="(b)" )
plot(log(share),income,main="(c)" )
plot(log(share),majorcards,main="(d)" )
graphics.off()





