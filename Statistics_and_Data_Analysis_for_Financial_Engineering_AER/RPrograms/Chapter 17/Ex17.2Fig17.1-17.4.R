#  Example 17.2 and Figures 17.1-17.4

datNoOmit = read.table("treasury_yields.txt",header=T)
diffdatNoOmit = diff(as.matrix(datNoOmit[,2:12]))
dat=na.omit(datNoOmit)
diffdat = na.omit(diffdatNoOmit)


n = dim(diffdat)[1]
options(digits=5)
pca = prcomp(diffdat)

summary(pca)

postscript("yields01.ps",width=7,height=6)  #  Figure 17.1
par(mfrow=c(2,2))
time = c(1/12,.25,.5,1, 2, 3, 5, 7, 10, 20, 30)
plot(time,as.vector(dat[1,2:12]),ylim=c(0,6),type="b",lty=1,lwd=2,
  ylab="Yield",xlab="T",main="(a)") #,log="x",xaxs="r")
lines(time,as.vector(dat[486,2:12]),type="b",lty=2,lwd=2)
lines(time,as.vector(dat[n+2,2:12]),type="b",lty=3,lwd=2)
legend("bottomright",c("07/31/01","07/02/07","10/31/08"),lty=c(1,2,3),lwd=2,
  cex=1)
plot(pca,main="(b)")

plot(time,pca$rotation[,1],,ylim=c(-.8,.8),type="b",lwd=2,ylab="PC",xlab="T",
   main="(c)")
lines(time,pca$rotation[,2],lty=2,type="b",lwd=2)
lines(time,pca$rotation[,3],lty=3,type="b",lwd=2)
lines(0:30,0*(0:30),lwd=1)
legend("bottomright",c("PC 1","PC 2","PC 3"),lty=c(1,2,3),lwd=2)

plot(time,pca$rotation[,1],ylim=c(-.8,.8),type="b",lwd=2,ylab="PC",xlab="T",
  xlim=c(0,3),main="(d)")
lines(time,pca$rotation[,2],lty=2,type="b",lwd=2)
lines(time,pca$rotation[,3],lty=3,type="b",lwd=2)
lines(0:30,0*(0:30),lwd=1)
legend("bottomright",c("PC 1","PC 2","PC 3"),lty=c(1,2,3),lwd=2)

graphics.off()




postscript("yields02.ps",width=7,height=6)  #  Figure 17.2
par(mfrow=c(2,2))
plot(time,mu,ylim=c(0,6),type="b",lwd=4,xlab="T",ylab="Yield",
   main="(a)",xlim=c(0,7))
lines(time,mu+pca$rotation[,1],lty=5,type="b",lwd=2)
lines(time,mu-pca$rotation[,1],lty=3,type="b",lwd=2)
legend("bottomright",c("mean","mean + PC1",
   "mean -  PC1"),lty=c(1,5,3),lwd=c(4,2,2))

plot(time,mu,ylim=c(0,6),type="b",lwd=4,xlab="T",ylab="Yield",
   main="(b)", xlim=c(0,7))
lines(time,mu+pca$rotation[,2],lty=5,type="b",lwd=2)
lines(time,mu-pca$rotation[,2],lty=3,type="b",lwd=2)
legend("bottomright",c("mean","mean + PC2",
   "mean -  PC2"),lty=c(1,5,3),lwd=c(4,2,2))

plot(time,mu,ylim=c(0,6),type="b",lwd=4,xlab="T",ylab="Yield",
   main="(c)",xlim=c(0,7))
lines(time,mu+pca$rotation[,3],lty=5,type="b",lwd=2)
lines(time,mu-pca$rotation[,3],lty=3,type="b",lwd=2)
legend("bottomright",c("mean","mean + PC3",
   "mean -  PC3"),lty=c(1,5,3),lwd=c(4,2,2))

par(lwd=1)
plot(time,pca$rotation[,4],ylim=c(-1,1),type="b",lwd=2,ylab="PC",xlab="T",
 xlim=c(0,30),main="(d)")
lines(time,pca$rotation[,5],lty=2,type="b",lwd=2)
lines(0:30,0*(0:30),lwd=1)
legend("topright",c("PC 4","PC 5"),lty=c(1,2),lwd=2)
graphics.off()



postscript("yields_pca_tsplots.ps",width=7,height=3)  #  Figure 17.3
par(mfrow=c(1,3))
for (i in 1:3){
plot(pca$x[,i],main=paste("PC",toString(i)),xlab="day",
ylab="")
}
graphics.off()



postscript("yields_pca_acfplots2.ps",width=5,height=5)  #  Figure 17.4
acf(pca$x[,1:3],ylab="",xlab="lag")
graphics.off()





