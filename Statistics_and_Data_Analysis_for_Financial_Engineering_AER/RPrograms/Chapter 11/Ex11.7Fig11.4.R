#  Example 11.7 and Figure 11.4

library(Ecdat)
library(quadprog)
data(CRSPday)
R = 100*CRSPday[,4:6]
mean_vect = apply(R,2,mean)
cov_mat = cov(R)
sd_vect = sqrt(diag(cov_mat))

Amat = cbind(rep(1,3),mean_vect,diag(1,nrow=3))  # set the constraints matrix

muP = seq(min(mean_vect)+.0001,max(mean_vect)-.0001,length=300)  
                              # set of 300 possible target values 
                              # for the expect portfolio return
sdP = muP # set up storage for standard deviations of portfolio returns
weights = matrix(0,nrow=300,ncol=3) # storage for portfolio weights

for (i in 1:length(muP))  # find the optimal portfolios for each target expected return
{
bvec = c(1,muP[i],rep(0,3))
result = 
   solve.QP(Dmat=2*cov_mat,dvec=rep(0,3),Amat=Amat,bvec=bvec,meq=2)
sdP[i] = sqrt(result$value)
weights[i,] = result$solution
}
postscript("quad_prog_plotNoShort.ps",width=6,height=5)  #  Figure 11.4
plot(sdP,muP,type="l",xlim=c(0,2.5),ylim=c(0,.15),lty=3)  #  plot 
                                     # the efficient frontier (and inefficient frontier)
mufree = 1.3/253 # input value of risk-free interest rate
points(0,mufree,cex=4,pch="*")  # show risk-free asset
sharpe =( muP-mufree)/sdP # compute Sharpe ratios
ind = (sharpe == max(sharpe)) # Find maximum Sharpe ratio
weights[ind,] # Find tangency portfolio
lines(c(0,sdP[ind]),c(mufree,muP[ind]),lwd=4,lty=2) # show line of optimal portfolios
points(sdP[ind],muP[ind],cex=4,pch="*") # show tangency portfolio
ind2 = (sdP == min(sdP)) # find the minimum variance portfolio
points(sdP[ind2],muP[ind2],cex=2,pch="+") # show minimum variance portfolio
ind3 = (muP > muP[ind2])
lines(sdP[ind3],muP[ind3],type="l",xlim=c(0,.25),
   ylim=c(0,.3),lwd=2)  #  plot the efficient frontier
text(sd_vect[1],mean_vect[1],"GE",cex=1.5)
text(sd_vect[2],mean_vect[2],"IBM",cex=1.5)
text(sd_vect[3],mean_vect[3],"Mobil",cex=1.5)
graphics.off()
