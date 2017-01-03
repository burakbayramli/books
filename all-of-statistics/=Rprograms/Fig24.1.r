### Figure 24.1
x = 8
y = 6
n = 10
m = 10
B = 10000
p1 = rbeta(B,x+1,n-x+1)
p2 = rbeta(B,y+1,m-y+1)
delta = p2 - p1
print(mean(delta))
left  = quantile(delta,.025)
right = quantile(delta,.975)
print(c(left,right))

postscript("two.binom.sim.eps",horizontal=F,onefile=F,print.it=F)
hist(delta,xlab="",ylab="",main="",cex=2,density=5,
     prob=T,lwd=3,xaxt="n",yaxt="n",bty="n")
axis(1,at=c(-0.6,0,0.6),cex.axis=2)
dev.off()


