n_400
p_seq(0,1,length=100)
postscript("binomial.risk.ps")
r1_p*(1-p)/n
r2_rep(n/(4*(n+sqrt(n))^2),100)
plot(p,r1,type="l",xlab="p",ylab="Risk",sub=" _____ delta1        _ _ _ delta2")
lines(p,r2,lty=3)


