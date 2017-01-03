# Ex 6.4 and Figure 6.2
# The bootstrap results do not agree exactly with those in Figure 6.2
# This may be due to a new version of R

library("fEcofin")     #  for midCap returns data
library("bootstrap")
data(midcapD.ts)
attach(midcapD.ts)

postscript("LSCC_CSGSQQ.ps",width=6,height=5)  # Figure 6.2
qqplot(LSCC,CSGS)
lmfit = lm(quantile(CSGS,c(.25,.75)) ~ quantile(LSCC,c(.25,.75)) )
abline(lmfit)
graphics.off()

n=length(LSCC)

quKurt = function(y,p1=0.025,p2=0.25)
{
Q = quantile(y,c(p1,p2,1-p2,1-p1))
as.numeric((Q[4]-Q[1]) / (Q[3]-Q[2]))
}

compareQuKurt = function(x,p1=0.025,p2=0.25,xdata)
{
quKurt(xdata[x,1],p1,p2)/quKurt(xdata[x,2],p1,p2)
}


quKurt(LSCC)
quKurt(CSGS)
xdata=cbind(LSCC,CSGS)
compareQuKurt(1:n,xdata=xdata)


set.seed("5640")
bca_kurt= bcanon((1:n),5000,compareQuKurt,xdata=xdata)
bca_kurt$confpoints











