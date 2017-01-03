# Chapter 13 Scripts

# Exhibit 13.1
win.graph(width=4.875,height=2.5,pointsize=8)
t=1:96 # n=96
cos1=cos(2*pi*t*4/96)
cos2=cos(2*pi*(t*14/96+.3))
plot(t,cos1, type='o', ylab='Cosines')
lines(t,cos2,lty='dotted',type='o',pch=4)

# Exhibit 13.2
# You may want to plot many of the graphs at fullscreen resolution to see more detail
win.graph(width=4.875,height=2.5,pointsize=8)
t=1:96 # n=96
cos1=cos(2*pi*t*4/96)
cos2=cos(2*pi*(t*14/96+.3))
y=2*cos1+3*cos2
plot(t,y,type='o',ylab=expression(y[t]))


# Exhibit 13.3
# Assumes that y was created for Exhibit 13.2
sp=periodogram(y)
 # Note that spec has been altered to default to taper=0 and detrend=F
abline(h=0)
axis(1,at=c(0.04167,.14583))

# Exhibit 13.4
# Hidden periodicities
win.graph(width=4.875,height=2.5,pointsize=8)
t=1:96 # n=96
set.seed(134)
integer=sample(48,2) # randomly sample 2 integers from 1:48 without replacement
freq1=integer[1]/96; freq2=integer[2]/96
A1=rnorm(1,0,2); B1=rnorm(1,0,2)  # sample normal "amplitudes"
A2=rnorm(1,0,3); B2=rnorm(1,0,3)
w=2*pi*t
y=A1*cos(w*freq1)+B1*sin(w*freq1)+A2*cos(w*freq2)+B2*sin(w*freq2)+rnorm(96,0,1)
plot(t,y,type='o',ylab=expression(y[t]))

# Exhibit 13.5
# Assumes that y was created for Exhibit 13.4 above
periodogram(y)
abline(h=0)

# Exhibit 13.6
data(star)
plot(star,xlab='Day',ylab='Brightness')

# Exhibit 13.7
periodogram(star,ylab='Variable Star Periodogram');  abline(h=0)



# Exhibit 13.8 aliasing
win.graph(width=4.875, height=2.5,pointsize=8)
t=seq(0,8,by=.05)
plot(t,cos(2*pi*t/4),axes=F,type='l',ylab=expression(Y[t]),
 xlab='Discrete Time t')
axis(1,at=c(1,2,3,4,5,6,7))
axis(1); axis(2); box()
lines(t,cos(2*pi*t*3/4),lty='dashed',type='l')
abline(h=0)
points(x=c(0:8),y=cos(2*pi*c(0:8)/4),pch=19)

# Exhibits 13.9 & 13.10
# MA1 spectral density
win.graph(width=4.875, height=2.5,pointsize=8)
theta=.9 # Reset theta for other MA(1) plots
ARMAspec(model=list(ma=-theta)) 
# R uses the plus convention in the MA specification.


# Exhibits 13.11
# MA(2) models
win.graph(width=4.875, height=2.5,pointsize=8)
theta1=1; theta2=-0.6 # Reset values of theta1 & theta2 for other MA(2) models
ARMAspec(model=list(ma=-c(theta1,theta2)))


# Exhibit 13.12 & 13.13
# AR(1)
win.graph(width=4.875, height=2.5,pointsize=8)
phi=0.9 # Reset value of phi for other AR(1) models
ARMAspec(model=list(ar=phi))


# Exhibit 13.14 & 13.15
# AR(2)
win.graph(width=4.875, height=2.5,pointsize=8)
phi1=1.5; phi2=-.75  # Reset values of phi1 & phi2 for other AR(2) models
ARMAspec(model=list(ar=c(phi1,phi2)))


# Exhibit 13.17
# ARMA(1,1)
win.graph(width=4.875, height=2.5,pointsize=8)
theta=0.8 ; phi=0.5 # Reset parameters for other ARMA(1,1) models
ARMAspec(model=list(ar=phi,ma=-theta))


# Exhibit 13.18
# Seasonal AR
win.graph(width=4.875, height=2.5,pointsize=8)
phi=.5 ; PHI=.9 # Reset parameters for other models
ARMAspec(model=list(ar=phi,seasonal=list(sar=PHI,peroid=12)))


# Exhibit 13.19
# Seasonal MA
win.graph(width=4.875, height=2.5,pointsize=8)
theta=.4 ; Theta=.9 # Reset parameters for other models
ARMAspec(model=list(ma=-theta,seasonal=list(sma=-Theta,period=12)))


# Exhibit 13.20
# Simulated AR(1) with phi=-0.6 and n = 200 for Exhibits 13.18,13.26, and 13.27
win.graph(width=4.875,height=2.5,pointsize=8)
set.seed(271435)
n=200
phi=-0.6
y=arima.sim(model=list(ar=phi),n=n)
sp=spec(y,log='no',xlab='Frequency',ylab='Sample Spectral Density',main='',
sub='',demean=T)
lines(sp$freq,ARMAspec(model=list(ar=phi),freq=sp$freq,plot=F)$spec,
lty='dotted')
abline(h=0)

# Exhibits 13.21,13.22, 13.23, 13.24, 13.25, & 13.26
#Simulation of spectral density sampling properties
win.graph(width=4.875,height=2.5,pointsize=8)
set.seed(145357)
phi1=1.5; phi2=-.75; theta1=0; theta2=0
specm=NULL
nrepl=1000
n=96; df=5 # df used only when generating non-normal white noise terms
for (i in 1:nrepl){
# Remove the # signs for the next three lines when generating non-normal white noise terms
#y=arima.sim(model=list(ar=c(phi1,phi2),ma=c(-theta1,-theta2)),
# rand.gen=function(n,...){if(df<=2) stop('df must be greater than 2') else
# return(rt(n,...)*sqrt((df-2)/df)) },n=n,df=df)
# comment out the line below when generating non-normal white noise terms
y=arima.sim(model=list(ar=c(phi1,phi2),ma=c(-theta1,-theta2)),n=n)
res=spec(y,plot=F,demean=T)
specm=cbind(specm,res$spec)
}
dim1=dim(specm)[1]
means=apply(specm,1,mean)
vars=apply(specm,1,var)
cor(t(specm[1:6,]))
# Below, compute the empirical frequency of a significant correlation
# between two spectral density estimates.
mean(abs(cor(t(specm))[outer(1:dim1,1:dim1,"<")])>1.96/sqrt(n))

sdf1=ARMAspec(model=list(ar=c(phi1,phi2),ma=-c(theta1,theta2)),freq=res$freq,
plot=F)$spec
plot(y=sdf1,x=res$freq,
xlab='Frequency',ylab='Average Spectral Density Estimates',type='l',
main='',ylim=extendrange(c(means,sdf1)))
points(y=means,x=res$freq)
abline(h=0)

m=round(n/2)
plot(y=sdf1[-m],x=res$freq[-m],xlab='Frequency',
 ylab='Standard Deviation of Spectral Density Estimate',
 type='l',main='',ylim=extendrange(c(sqrt(vars[-m]),sdf1)))
points(y=sqrt(vars[-m]),x=res$freq[-m])
abline(h=0)

# Exhibits 13.23 & 13.25
win.graph(width=2.5,height=2.5,pointsize=8)
# Replace 40 to obtain any frequency needed
qqplot(qchisq(((1:nrepl)-.5)/nrepl,df=2),specm[40,],
 xlab='Chi-Square Quantitles',ylab='Sample Spectral Distribution')
