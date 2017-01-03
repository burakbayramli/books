"cr.test" <-
function(betatrue){
pf(t(betahat-betatrue)%*%t(X)%*%X%*%(betahat-betatrue)/(3*MSE),3,17)[1,1]}
"litters.cr" <-
function () 
{
betahat <- coef(litters.lm)
betatrue<-matrix(rnorm(15000,mean=rep(coef(litters.lm),5000),rep(c(0,.006779/10,.00313/10),5000)),nrow=3)
betapass<-betatrue[-1,apply(betatrue,2,cr.test)<.95]
ylim<-range(betatrue[3,])
xlim<-range(betatrue[2,])
betatrue[1,]<-rep(.16,5000)
betapass<-betatrue[-1,apply(betatrue,2,cr.test)<.05]
plot(t(betapass),ylab="beta2",xlab="beta1",main="Confidence Region when b0 = .160",xlim=xlim,ylim=ylim)
pause()
betatrue[1,]<-rep(.166,5000)
betapass<-betatrue[-1,apply(betatrue,2,cr.test)<.05]
plot(t(betapass),ylab="beta2",xlab="beta1",main="Confidence Region when b0 = .166",xlim=xlim,ylim=ylim)
pause()
betatrue[1,]<-rep(.169,5000)
betapass<-betatrue[-1,apply(betatrue,2,cr.test)<.05]
plot(t(betapass),ylab="beta2",xlab="beta1",main="Confidence Region when b0 = .169",xlim=xlim,ylim=ylim)
pause()
betatrue[1,]<-rep(.172,5000)
betapass<-betatrue[-1,apply(betatrue,2,cr.test)<.05]
plot(t(betapass),ylab="beta2",xlab="beta1",main="Confidence Region when b0 = .172",xlim=xlim,ylim=ylim)
betatrue[1,]<-rep(.175,5000)
betapass<-betatrue[-1,apply(betatrue,2,cr.test)<.05]
pause()
plot(t(betapass),ylab="beta2",xlab="beta1",main="Confidence Region when b0 = .175",xlim=xlim,ylim=ylim)
betatrue[1,]<-rep(.177,5000)
betapass<-betatrue[-1,apply(betatrue,2,cr.test)<.05]
pause()
plot(t(betapass),ylab="beta2",xlab="beta1",main="Confidence Region when b0 = .177",xlim=xlim,ylim=ylim)
pause()
betatrue[1,]<-rep(.178,5000)
betapass<-betatrue[-1,apply(betatrue,2,cr.test)<.05]
plot(t(betapass),ylab="beta2",xlab="beta1",main="Confidence Region when b0 = .178",xlim=xlim,ylim=ylim)
betatrue[1,]<-rep(.179,5000)
betapass<-betatrue[-1,apply(betatrue,2,cr.test)<.05]
pause()
plot(t(betapass),ylab="beta2",xlab="beta1",main="Confidence Region when b0 = .179",xlim=xlim,ylim=ylim)

betatrue[1,]<-rep(.181,5000)

betapass<-betatrue[-1,apply(betatrue,2,cr.test)<.05]
pause()
plot(t(betapass),ylab="beta2",xlab="beta1",main="Confidence Region when b0 = .181",xlim=xlim,ylim=ylim)
betatrue[1,]<-rep(.184,5000)
betapass<-betatrue[-1,apply(betatrue,2,cr.test)<.05]
pause()
plot(t(betapass),ylab="beta2",xlab="beta1",main="Confidence Region when b0 = .184",xlim=xlim,ylim=ylim)
betatrue[1,]<-rep(.187,5000)
betapass<-betatrue[-1,apply(betatrue,2,cr.test)<.05]
pause()
plot(t(betapass),ylab="beta2",xlab="beta1",main="Confidence Region when b0 = .187",xlim=xlim,ylim=ylim)
betatrue[1,]<-rep(.190,5000)
betapass<-betatrue[-1,apply(betatrue,2,cr.test)<.05]
pause()
plot(t(betapass),ylab="beta2",xlab="beta1",main="Confidence Region when b0 = .190",xlim=xlim,ylim=ylim)
betatrue[1,]<-rep(.195,5000)
betapass<-betatrue[-1,apply(betatrue,2,cr.test)<.05]
pause()
plot(t(betapass),ylab="beta2",xlab="beta1",main="Confidence Region when b0 = .195",xlim=xlim,ylim=ylim)
     }
"litters.comparison" <-
function () 
{

plot(brainwt ~ bodywt, data = litters, pch=16)
litters.lm <- lm(brainwt ~ bodywt, data = litters)
abline(litters.lm,lwd=2)
litters.lm <- lm(log(brainwt) ~ log(bodywt),
                 data = litters)
coeffs <- coef(litters.lm)
MSE <- summary(litters.lm)$sigma^2
x <- seq(5,10,length=101)
lines(x,x^(coeffs[2])*exp(coeffs[1]+ MSE/2),col=2,lwd=2)
legend(7,.4,legend=c("allometric model","linear model"),
    col=c(2,1),lwd=c(2,2), lty=c(1,1))
}
"extrap.fn" <-
function (lm.object, lm.data, n=1) 
{
# plots data and hat diagonal values for 2 regressor variables.
x <- lm.data[,2]
y <- lm.data[,1]
plot(x,y,pch=16)
identify(x,y,labels=round(hat(model.matrix(lm.object)),2),n=n)

}
"hidden.extrap" <-
function (lm.object,xo) 
{
t(xo)%*%summary(lm.object)$cov.unscaled%*%xo

}
