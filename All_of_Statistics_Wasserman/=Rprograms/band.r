## confidence band for density estimate


source("Utility.functions.r")




k.fun = function(x,y,h){
     u = (x-y)/h
     out = (15/16*h)*(1-u^2)^2
     out[ abs(u) > 1] = 0
     out
     }



fit.fun = function(h,x,y){
     o = order(x)
     x = x[o]
     y = y[o]
     n    = length(x)
     nh   = length(h)
     risk = rep(0,nh)
     for(j in 1:nh){
          r = rep(0,n)
          w = rep(0,n)
          a = rep(0,n)
          for(i in 1:n){
               w = k.fun(x[i],x,h[j])
               w = w/sum(w)
               r[i] = sum(w*y)
               a[i] = (y[i] - r[i])^2/(1-w[i])^2
               }
          a[is.na(a)] = 0
          risk[j] = sum(a)
          }
     h = (h[risk==min(risk)])[1]
     r = rep(0,n)
     w = rep(0,n)
          for(i in 1:n){
               w = k.fun(x[i],x,h)/sum(k.fun(x[i],x,h))
               r[i] = sum(w*y)
               }
     return(risk=risk,h=h,r=r)
     }



band.fun = function(x,y,xgrid,h,alpha=0.05){
     o = order(x)
     x = x[o]
     y = y[o]
     n = length(x)
     m = length(xgrid)
     r = rep(0,m)
     sigma = sqrt(sum((y[-1]-y[-n])^2)/(2*(n-1)))
     z = seq(-1,1,length=1000)
     K = (15/16)*(1-z^2)^2
     V = sqrt(int.fun(K^2,z))
     Kp = (15/4)*z*(1-z^2)
     C = log(sqrt(int.fun(Kp^2,z)/int.fun(K^2,z))/(2*pi))
     a = -log(-log(1-alpha)/2)
     for(j in 1:m){
          w = k.fun(xgrid[j],x,h)
          w = w/sum(w)
          r[j] = sum(w*y)
          }

     junk = sigma*V*(sqrt(-2*log(h)) + (C+a)/sqrt(-2*log(h)))/sqrt(n*h)
     u = r + junk
     l = r - junk
     return(r=r,l=l,u=u)
     }



################################################

data1 = scan("max_050801.dat")
data1 = matrix(data1,ncol=4,byrow=T)
data2 = scan("boom_050801.dat")
data2 = matrix(data2,ncol=4,byrow=T)
data = rbind(data1,data2)
x = data[,1]
y = data[,2]
data3 = scan("dasi.dat");data3 = matrix(data3,ncol=5,byrow=T)
x = c(x,data3[,1])
y = c(y,data3[,4])
o = order(x)
x = x[o]
y = y[o]
xgrid = x
h     = seq(50,120,length=100)

out = fit.fun(h,x,y)
r = out$r
risk = out$risk


postscript("cmb.band.eps")
par(mfrow=c(2,2))


plot(h,out$risk)
plot(x,y,pch=19,xlab="",ylab="",lwd=3)
lines(x,r,lwd=3)
h = out$h

out = band.fun(x,y,xgrid,h)
par(mfrow=c(2,1))

plot(x,y,pch=19,xlab="",ylab="",lwd=3)
lines(xgrid,out$r,lwd=3)
lines(xgrid,out$u,lwd=3)
lines(xgrid,out$l,lwd=3)

k = length(xgrid)
u = out$u
l = out$l
Grid = (xgrid[-1]+xgrid[-k])/2
Grid = rep(Grid,rep(2,k-1))
Grid = c(min(grid),Grid,max(grid))
U = rep(u,rep(2,k))
L = rep(l,rep(2,k))
plot(xgrid,u,type="l",lwd=4,xlab="",ylab="",ylim=c(-750,6000))
lines(xgrid,l,type="l",lwd=4)
polygon(c(Grid,rev(Grid)),c(L,rev(U)),density=25,col=4)
dev.off()



