postscript("dose.response.ps")
par(mfrow=c(2,2))
x <- 1:10
n <- rep(15,10)
p <- exp(x-5)
p <- p/(1+p)
y <- rbinom(10,n,p)
print(y)
plot(x,y/n,xlab="dose",ylab="proportion who died")
B <- 10000
p <- matrix(0,B,10)
check <- rep(0,B)
for(i in 1:10){
     p[,i] <- rbeta(B, y[i]+1, n[i]-y[i]+1)
     }
for(i in 1:B){
     check[i] <- min(diff(p[i,]))
     }
good <- (1:B)[check >= 0]
p <- p[good,]
B <- nrow(p)
delta <- rep(0,B)
for(i in 1:B){
     temp <- cumsum(p[i,])
     j <- (1:10)[temp > .5]
     j <- min(j)
     delta[i] <- x[j]
     }

print(mean(delta))
left <- quantile(delta,.025)
right <- quantile(delta,.975)
print(c(left,right))
print(B)
matplot(x,t(p),ylab="",xlab="dose")
plot(table(delta)/B,xlab="LD50",ylab="f(delta | data)",type="h",lwd=3,
     xlim=c(1,10))
dev.off()




