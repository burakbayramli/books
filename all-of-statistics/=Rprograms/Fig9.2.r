### Figure 9.2

n = 10
x = runif(n)
m = 100
postscript("uniform.likelihood.eps",horizontal=F,onefile=F,print.it=F)
par(mfrow=c(2,2),pty="s")
f    = rep(0,m)
grid = seq(0,2,length=m)
f[grid < .75] = 1/.75
plot(grid,f,type="l",ylim=c(0,1.5),xlab="",ylab="",lwd=3,cex.lab=1.5)
points(x,rep(.5,n))
for(i in 1:n){
     lines(c(x[i],x[i]),c(0,.5),lty=2,col=2)
     }


f    = rep(0,m)
grid = seq(0,2,length=m)
f[grid < 1] = 1
plot(grid,f,type="l",ylim=c(0,1.5),xlab="",ylab="",lwd=3,cex.lab=1.5)
points(x,rep(.5,n))
for(i in 1:n){
     lines(c(x[i],x[i]),c(0,.5),lty=2,col=2)
     }

f    = rep(0,m)
grid = seq(0,2,length=m)
f[grid < 1.25] = 1/1.25
plot(grid,f,type="l",ylim=c(0,1.5),xlab="",ylab="",lwd=3,cex.lab=1.5)
points(x,rep(.5,n))
for(i in 1:n){
     lines(c(x[i],x[i]),c(0,.5),lty=2,col=2)
     }




theta = seq(.6,1.5,length=m)     
lik   = rep(0,m)
lik   = 1/theta^n
lik[theta < max(x)] =0
plot(theta,lik,type="l",lwd=3,xlab="",ylab="",
     cex.lab=1.5)
dev.off()


