# Chapter 15

# Exhibit 15.1
win.graph(width=4.875, height=6.5,pointsize=8)
set.seed(2534567)
par(mfrow=c(3,2))
y=arima.sim(n=61,model=list(ar=c(1.6,-0.94),ma=-0.64))
lagplot(y)

# Exhibit 15.2
data(veilleux)
predator=veilleux[,1]
win.graph(width=4.875, height=2.5,pointsize=8)
plot(log(predator),lty=2,type='b',xlab='Day',ylab='Log(predator)')
predator.eq=window(predator,start=c(7,1))
lines(log(predator.eq))
index1=zlag(log(predator.eq),3)<=4.661
points(y=log(predator.eq)[index1],(time(predator.eq))[index1],pch=19)

# Exhibit 15.3
win.graph(width=4.875, height=6.5,pointsize=8)
lagplot(log(predator.eq))

# Exhibit 15.4
win.graph(width=4.875, height=2.5,pointsize=8)
data(spots)
plot(sqrt(spots),type='o',xlab='Year',
ylab="Sqrt Sunspot Number")

# Tests for nonlinearity
Keenan.test(sqrt(spots))
Tsay.test(sqrt(spots))
Keenan.test(log(predator.eq))
Tsay.test(log(predator.eq))

# Exhibit 15.5
set.seed(1234567)
plot(y=qar.sim(n=15,phi1=.5,sigma=1),x=1:15,type='l',ylab=expression(Y[t]),xlab='t')

# Exhibit 15.6
y=qar.sim(n=100,const=0.0,phi0=3.97, phi1=-3.97,sigma=0,init=.377)
plot(x=1:100,y=y,type='l',ylab=expression(Y[t]),xlab='t')

# Exhibit 15.7
acf(y)

# Exhibit 15.8
set.seed(1234579)
y=tar.sim(n=100,Phi1=c(0,0.5),
Phi2=c(0,-1.8),p=1,d=1,sigma1=1,thd=-1,
sigma2=2)$y
plot(y=y,x=1:100,type='o',xlab="t",ylab=expression(Y[t]))

# Exhibit 15.9
win.graph(width=2.5, height=2.5,pointsize=8)
qqnorm(y)
qqline(y)

# Exhibit 15.10
plot(y=c(2,-3),x=c(-3,3),type='n',xlab=expression(phi[list(1,1)]),
ylab=expression(phi[list(2,1)]))
lines(y=c(1,1),x=c(-3.3,1))
lines(x=c(1,1),y=c(-3.2,1))
x=seq(-3.3,-0.01,0.01)
lines(x=x,y=1/x)
xx=c(x,seq(0,1,0.01), seq(1,0,-0.01),rev(x))
yy=c(x*0+1,seq(0,1,0.01)*0+1,seq(1,0,-0.01)*0-3.2,1/rev(x))
polygon(xx,yy,col='gray')
lines(y=c(-1,1),x=c(-1,-1))
lines(y=c(-1,-1),x=c(-1,1))
abline(h=0)
abline(v=0)

# Tests for threshold nonlinearity
pvaluem=NULL
for (d in 1:5){
res=tlrt(sqrt(spots),p=5,d=d,a=0.25,b=0.75)
pvaluem= cbind( pvaluem, c(d,res$test.statistic, 
res$p.value))
}
rownames(pvaluem)=c('d','test statistic','p-value')
round(pvaluem,3) 


pvaluem=NULL
for (d in 1:4){
res=tlrt(log(predator.eq),p=4,d=d,a=0.25,b=0.75)
pvaluem= cbind( pvaluem, c(d,res$test.statistic, 
res$p.value))
}
rownames(pvaluem)=c('d','test statistic','p-value')
round(pvaluem,3)

# Exhibit 15.11
AICM=NULL
for(d in 1:4) {predator.tar=tar(y=log(predator.eq),p1=4,p2=4,d=d,a=.1,b=.9)
AICM=rbind(AICM, c(d,predator.tar$AIC,signif(predator.tar$thd,4),
predator.tar$p1,predator.tar$p2))
}
colnames(AICM)=c('d','nominal AIC','r','p1','p2')
rownames(AICM)=NULL
AICM

# Exhibit 15.12
predator.tar.1=tar(y=log(predator.eq),p1=4,p2=4,d=3,a=.1,b=.9,print=T)

#Repeat the model fit but using the CLS method of estimation, with p1=1 and p2=4
tar(y=log(predator.eq),p1=1,p2=4,d=3,a=.1,b=.9,print=T,method='CLS')
# Note that the CLS option does not allow the selection of AR order for each sub-model.
# If we let p1=p2=4, the model fit using CLS will be different because the
# order of the lower regime is fixed to be 4, but the threshold is found to be 
# same as that of the MAIC estimate. 
tar(y=log(predator.eq),p1=4,p2=4,d=3,a=.1,b=.9,print=T,method='CLS')


# Discussions below Exhibit 15.12
predator.tar.2=tar(log(predator.eq),p1=1,p2=1,d=3,a=.1,b=.9,print=T)

win.graph(width=4.875, height=2.5,pointsize=8)

# Exhibit 15.13
tar.skeleton(predator.tar.1,n=50)

# Exhibit 15.14
set.seed(356813)
plot(y=tar.sim(n=57,object=predator.tar.1)$y,x=1:57,ylab=expression(Y[t]),
xlab=expression(t),type='o')



# Exhibit 15.15
tar.skeleton(predator.tar.2,n=50)

# Exhibit 15.16
set.seed(356813)
plot(y=tar.sim(n=57,object=predator.tar.2)$y,x=1:57,ylab=expression(Y[t]),
xlab=expression(t),type='o')

# Exhibit 15.18
set.seed(2357125)
yy.1.4=tar.sim(predator.tar.1,n=10000)$y
yy.1=tar.sim(predator.tar.2,n=10000)$y
spec.1.4=spec(yy.1.4,taper=.1, span=c(200,200),plot=F)
spec.1=spec(yy.1,taper=.1, span=c(200,200),plot=F)
spec.predator=spec(log(predator.eq),taper=.1, span=c(3,3),plot=F)
spec.predator=spec(log(predator.eq),taper=.1, span=c(3,3),
ylim=range(c(spec.1.4$spec, spec.1$spec,spec.predator$spec)),sub='')
lines(y=spec.1.4$spec,x=spec.1.4$freq,lty=2)
lines(y=spec.1$spec,x=spec.1$freq,lty=3)

# Exhibit 15.19
win.graph(width=4.875, height=4.5)
tsdiag(predator.tar.2,gof.lag=20)

# Exhibit 15.20
tsdiag(predator.tar.1,gof.lag=20)

# Exhibit 15.21
win.graph(width=2.5, height=2.5,pointsize=8)
qqnorm(predator.tar.1$std.res)
qqline(predator.tar.1$std.res)

# Exhibit 15.22
set.seed(2357125)
win.graph(width=4.875, height=2.5,pointsize=8)
# In the book, the simulation size to 10000, but this will take a while so
# it is reduced to 1000 here.
pred.predator=predict(predator.tar.1,n.ahead=60,n.sim=1000)
yy=ts(c(log(predator.eq),pred.predator$fit),frequency=2,start=start(predator.eq))
plot(yy,type='n',ylim=range(c(yy,pred.predator$pred.interval)),ylab='Log Predator',
xlab=expression(t))
lines(log(predator.eq))
lines(window(yy, start=end(predator.eq)+c(0,1)),lty=2)
lines(ts(pred.predator$pred.interval[2,],start=end(predator.eq)+c(0,1),freq=2),lty=2)
lines(ts(pred.predator$pred.interval[1,],start=end(predator.eq)+c(0,1),freq=2),lty=2)

# Exhibit 15.23
plot(ts(apply(pred.predator$pred.interval,2,function(x){x[2]-x[1]})),ylab='Length of Prediction Intervals',
xlab="No of Steps Ahead")

# Exhibit 15.24
win.graph(width=2.5, height=2.5,pointsize=8)
qqnorm(pred.predator$pred.matrix[,3])
qqline(pred.predator$pred.matrix[,3])

# Exhibit 15.25
qqnorm(pred.predator$pred.matrix[,6])
qqline(pred.predator$pred.matrix[,6])
