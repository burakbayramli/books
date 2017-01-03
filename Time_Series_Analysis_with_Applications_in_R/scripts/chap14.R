# Chapter 14 Scripts


# Exhibit 14.1
# Simulated AR(1) with phi=-0.6 and n = 200
win.graph(width=4.875,height=2.5,pointsize=8)
set.seed(271435); n=200; phi=-0.6
y=arima.sim(model=list(ar=phi),n=n)
k=kernel('daniell',m=5)
sp=spec(y,kernel=k,log='no',main='',sub='',
 xlab='Frequency',ylab='Sample Spectral Density')
lines(sp$freq,ARMAspec(model=list(ar=phi),freq=sp$freq,plot=F)$spec,
lty='dotted')

# Exhibit 14.2
k=kernel('daniell',m=15)
sp=spec(y,kernel=k,log='no',main='',sub='',
 xlab='Frequency',ylab='Sample Spectral Density')
lines(sp$freq,ARMAspec(model=list(ar=phi),freq=sp$freq,plot=F)$spec,
lty='dotted')

# Exhibit 14.3 Code not given in text
win.graph(width=4.875,height=2,pointsize=10)
oldpar=par
par(mfrow=c(1,3))
plot(kernel("modified.daniell", c(3)),lwd=2,main='',ylim=c(0,.17),ylab=expression(W(k)))
abline(h=0)
plot(kernel("modified.daniell", c(3,3)),lwd=2,main='',ylim=c(0,.17),ylab=expression(W(k)))
abline(h=0)
plot(kernel("modified.daniell", c(3,3,3)),lwd=2,main='',ylim=c(0,.17),ylab=expression(W(k)))
abline(h=0)
par=oldpar

# Exhibit 14.4
win.graph(width=4.875,height=2.5,pointsize=8)
set.seed(271435); n=200; phi=-0.6
y=arima.sim(model=list(ar=phi),n=n)
k=kernel('daniell',m=15)
sp=spec(y,kernel=k,main='',sub='',xlab='Frequency',
ylab='Log(Smoothed Sample Spectrum)',ci.plot=T,ci.col=NULL)
lines(sp$freq,ARMAspec(model=list(ar=phi),sp$freq,plot=F)$spec,lty="dashed")
abline(h=0)

# Exhibit 14.5 
sp=spec(y,span=31,sub='',xlab='Frequency',
ylab='Log(Smoothed Sample Spectrum)')
lines(sp$freq,ARMAspec(model=list(ar=phi),sp$freq,
plot=F)$spec,lty='dotted')


# Exhibit 14.6
# leakage (The arrows and extra labels were added with the Framemaker software)
win.graph(width=4.875,height=2.5,pointsize=8)
t=1:96
f1=0.088; f2=14/96 # Note: 0.088 is NOT of the form m/96 for any integer m
y=3*cos(f1*2*pi*t)+sin(f2*2*pi*t)
spec(y,log='no',main='',sub='',type='h',lwd=2,
 ylab='Periodogram',xlab='Frequency')
abline(h=0)

# Dirichlet kernel  Exhibit 14.7 Code not given in text
win.graph(width=4.875,height=2.5,pointsize=8)
olfpar=par
par(mfrow=c(1,2))
n=100
f=seq(-.1,.1,.0001)
D=(1/n)*sin(pi*n*f)/sin(pi*f)
D[1001]=1
plot(x=f,y=D, typ='l',ylab='Dirichlet Kernel',xlab='Frequency',ylim=c(-.25,1.05))
abline(h=0)
# Tapered
Dt=filter(D,filter=c(.5,rep(0,100),1,rep(0,100),.5),method='convolution')
plot(x=f,y=Dt,typ='l',xlab='Frequency',ylim=c(-.25,1.05),ylab=expression(D[T]))
abline(h=0)
par=oldpar

# Exhibit 14.8 Code not given in text
# Tapering Cosine Bell
oldpar=par
par(mfrow=c(1,2))
t=1:100
y=rep(1,100)
tp=spec.taper(y,p=.5)
abline(h=0)
plot(t,tp,typ='l',ylab='Cosine Bell',xlab='Time')
abline(h=0)
taper=spec.taper(y,p=.1)
plot(t,taper,ylab='Split Cosine Bell',xlab='Time',typ='l')
abline(h=0)
par=oldpar


# Exhibit 14.9
win.graph(width=4.875,height=7,pointsize=12)
opar=par()
par(mfrow=c(4,1),mar=c(2,1,1,1)+.1,oma=c(3,3,2,0))
temp=NULL
data(star)
for (taper in c(0,0.1,0.2,0.5) ) {
sp=spec(star,sub='',main='',taper=taper,plot=F)
temp=c(temp,sp$spec)
}
for (taper in c(0,0.1,0.2,0.5) ) {
sp=spec(star,sub='',main='',taper=taper,ylim=pmax(extendrange(temp),0.0000001),ci.col='black')
}
par(opar)

# Exhibit 14.10
# Simulated AR(1) with phi=-0.6 and n = 200 for Exhibits 13.18,13.26, and 13.27
win.graph(width=4.875,height=2.5,pointsize=8)
set.seed(271435)
n=200
phi=-0.6
y=arima.sim(model=list(ar=phi),n=n)
sp=spec(y,method='ar',main='',sub='',
 xlab='Frequency', ylab='Sample Spectral Density')
lines(sp$freq,ARMAspec(model=list(ar=phi),freq=sp$freq,plot=F)$spec,
lty='dotted')

# Exhibits 14.11 & 14.12
# Spectral analysis of simulated series
win.graph(width=4.875,height=2.5,pointsize=8)
set.seed(271435)
n=100
phi1=1.5; phi2=-.75 # Reset parameter values to obtain Exhibits 14.9 & 14.10
y=arima.sim(model=list(ar=c(phi1,phi2)),n=n)
sp1=spec(y,main='',spans=3,sub='',lty='dotted', xlab='Frequency',
 ylab='Log(Estimated Spectral Density)')
sp2=spec(y,,spans=9,plot=F)
sp3=spec(y,,spans=15,plot=F)
lines(sp2$freq,sp2$spec,lty='dashed') # Use different colors to see the results more easily!
lines(sp3$freq,sp3$spec,lty='dotdash')
f=seq(0.001,.5,by=.001)
lines(f,ARMAspec(model=list(ar=c(phi1,phi2)),freq=f,plot=F)$spec,
lty='solid')

# Exhibit 14.12
sp4=spec(y,method='ar',main='',lty='dotted', xlab='Frequency',
 ylab='Log(Estimated AR Spectral Density)')
f=seq(0.001,.5,by=.001)
lines(f,ARMAspec(model=list(ar=c(phi1,phi2)),freq=f,plot=F)$spec,lty='solid')
sp4$method # This will tell you order of the AR model selected.

# Exhibit 14.13 & 14.14: See script for Exhibits 14.9 & 14.10

# Exhibit 14.15
win.graph(width=4.875,height=2.5,pointsize=8) # Exhibits 14.13 & 14.14
set.seed(324135)
n=500
phi=.5; theta=.8
y=arima.sim(model=list(ar=phi,ma=-theta),n=n)
sp1=spec(y,spans=11,main='',sub='',lty='dotted', xlab='Frequency',
 ylab='Log(Estimated Spectral Density)')
sp2=spec(y,spans=22,plot=F)
sp3=spec(y,spans=44,plot=F)
lines(sp2$freq,sp2$spec,lty='dashed')
lines(sp3$freq,sp3$spec,lty='dotdash')
f=seq(0.001,.5,by=.001)
lines(f,ARMAspec(model=list(ar=phi,ma=-theta),f,plot=F)$spec,lty='solid')

# Exhibit 14.16
sp4=spec(y,method='ar',main='',lty='dotted',ylim=c(.15,1.9), xlab='Frequency',
 ylab='Log(Estimated AR Spectral Density)')
f=seq(0.001,.5,by=.001)
lines(f,ARMAspec(model=list(ar=phi,ma=-theta),f,plot=F)$spec,lty='solid')
sp4$method # This will tell you order of the AR model selected.

# Exhibits 14.17, 14.18, & 14.19
# Seasonal process
win.graph(width=4.875,height=2.5,pointsize=8)
set.seed(247135)
n=144
theta=.4;THETA=.9
y=arima.sim(model=list(ma=c(-theta,rep(0,10),-THETA,theta*THETA)),n=n)
sp1=spec(y,spans=6,main='',sub='',lty='dotted',ylim=c(.15,9),
 xlab='Frequency',ylab='Log(Estimated Spectral Density)')
sp2=spec(y,,spans=12,plot=F)
sp3=spec(y,,spans=24,plot=F)
lines(sp2$freq,sp2$spec,lty='dashed')
lines(sp3$freq,sp3$spec,lty='dotdash')
f=seq(0.001,.5,by=.001)
lines(f,ARMAspec(model=list(ma=-theta,seasonal=list(sma=-THETA,period=12)),
freq=f,plot=F)$spec,lty='solid')

# Exhibit 14.18
sp4=spec(y,method='ar',main='',ylim=c(.15,15),lty='dotted',
 xlab='Frequency',ylab='Log(Estimated AR Spectral Density)')
f=seq(0.001,.5,by=.001)
lines(f,ARMAspec(model=list(ma=-theta,seasonal=list(sma=-THETA,period=12)),
freq=f,plot=F)$spec,lty='solid')
sp4$method

# Exhibit 14.19
sp5=spec(y,spans=c(3,3),main='',sub='',lty='dotted',demean=T,
 xlab='Frequency',ylab='Log(Estimated Spectral Density)')
f=seq(0.001,.5,by=.001)
lines(f,ARMAspec(model=list(ma=-theta,seasonal=list(sma=-THETA,period=12)),
freq=f,plot=F)$spec,lty='solid')

# Exhibit 14.20
# Industrial robot
data(robot)
plot(robot,ylab='End Position Offset',xlab='Time',main='')

# Exhibit 14.21
spec(robot,spans=c(7,7),taper=.1,demean=T,main='',sub='',
 ylab='Log(Spectrum)', xlab='Frequency')
s=spec(robot,method='ar',plot=F)
lines(s$freq,s$spec,lty='dotted')

# Exhibits 14.22 & 14.23
# Iowa River flow series
data(flow)
plot(flow,ylab='River Flow')

# Exhibit 14.23
spec(log(flow),spans=c(7,7),main='',sub='',ylim=c(.02,13),
 ylab='Log(Spectrum)',xlab='Frequency')
s=spec(log(flow),method='ar',plot=F)
lines(s$freq,s$spec,lty='dotted')
s$method

# Exhibit 14.24
# Monthly milk production
data(milk)
spec(milk,spans=c(3,3),detrend=T,sub='',main='',
 ylab='Estimated Log(Spectrum)',xlab='Frequency',ci.col='black')
abline(v=seq(1:6)/12,lty='dotted')
s=spec(milk.lm$resid,method='ar',ylab='Estimated Log(Spectrum)',xlab='Frequency',main='')
# lines(s$freq,s$spec,lty='dotdash') # You might add this to the plot to see how they match.
s$method

# Alternatively
milk.lm=lm(milk~seq(1:length(milk)))
spec(milk.lm$resid,spans=c(3,3),sub='',main='',
 ylab='Estimated Log(Spectrum)',xlab='Frequency',ci.col='black')
abline(v=seq(1:6)/12,lty='dotted')


# Exhibit 14.25
win.graph(width=4.875,height=4,pointsize=8)
data(tbone); data(euph); par(mfrow=(c(2,1)))
trombone=(tbone-mean(tbone))/sd(tbone)
euphonium=(euph-mean(euph))/sd(euph)
plot(window(trombone,end=400),main='Trombone Bb',
ylab='Waveform',yaxp=c(-1,+1,2))
plot(window(euphonium,end=400),main='Euphonium Bb',
ylab='Waveform',yaxp=c(-1,+1,2)); par=oldpar


# Exhibit 14.26

win.graph(width=4.875,height=2.5,pointsize=8)
spec(euphonium,spans=c(11,11),ylab='Log Spectra',
xlab='Frequency',sub='')
s=spec(trombone,spans=c(11,11),plot=F)
lines(s$freq,s$spec,lty='dotted')
