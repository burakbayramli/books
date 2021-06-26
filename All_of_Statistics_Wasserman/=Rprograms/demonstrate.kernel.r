postscript("kernel.demonstrate.eps",horizontal=F,onefile=F,print.it=F)
x <- c(-7,-5,1,4,5)
n <- length(x)
h <- 2.5
f <- density(x,width=h,from=min(x)-3,to=max(x)+3,n=100)
plot(f,type="l",xlab="",ylab="",main="",lwd=3,xaxt="n",yaxt="n",bty="n",xlim=c(-10,10))
axis(1,c(-10,-5,0,5,10),cex.axis=2)
grid <- seq(-10,8,length=1000)
for(i in 1:n){
#     grid <- seq(x[i]-3*h,x[i]+3*h,length=100)
     lines(grid,dnorm(grid-x[i])/(2*n),lty=1,col=2)
     lines(c(x[i],x[i]),c(0,.02),lwd=2,col=3)
     points(x[i],.02,lwd=2,col=3,pch=19)
     }

dev.off()


