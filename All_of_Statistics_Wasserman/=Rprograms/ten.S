postscript("ten.ps",horizontal=F)
par(mfrow=c(2,2))

##Q(1)
x <- geyser$waiting
y <- geyser$duration
y[y<4] <- 0
y[y>=4] <- 1
plot(x,y,pch=17)
w <- sum(y)/length(y)

x0 <- x[y==0]
x1 <- x[y==1]
n0 <- length(x0)
n1 <- length(x1)
n <- length(x)

mu0 <- mean(x0)
mu1 <- mean(x1)
s0 <- sqrt(var(x0))
s1 <- sqrt(var(x1))

a <- min(x)
b <- max(x)
xgrid <- seq(a,b,length=1000)

h <- rep(0,1000)
cc <- s1*(1-w)/(s0*w)
h [  ((xgrid-mu0)^2/s0^2) - ((xgrid-mu1)^2/s1^2) > 2* log(cc) ] <- 1
plot(xgrid,h)

theta.hat <- dnorm(90,mu1,s1)*w/(dnorm(90,mu1,s1)*w+dnorm(90,mu0,s0)*(1-w))
print(theta.hat)
B <- 1000
theta.star <- rep(0,B)
for(i in 1:B){
     ystar <- rbinom(1,n,w)
     wstar <- sum(ystar)/n
     x0star <- rnorm(n0,mu0,s0)
     x1star <- rnorm(n1,mu1,s1)
     xstar <- rep(0,n)
     xstar[ystar==1] <- x1star
     xstar[ystar==0] <- x0star
     mu0star <- mean(x0star)
     mu1star <- mean(x1star)
     s0star <- sqrt(var(x0star))
     s1star <- sqrt(var(x1star))
     u <- dnorm(90,mu1star,s1star)*wstar
     v <- dnorm(90,mu0star,s0star)*(1-wstar)
     theta.star[i] <- u/(u+v)
     }

print(quantile(theta.star,c(.025,.975)))





library(MASS)

h0 <- ucv(x0)
temp <- density(x0,from=a,to=b,n=1000,width=h0,window="gaussian")
f0 <- temp$y
xgrid <- temp$x
plot(xgrid,f0,type="l",xlab="",ylab="f0")
h1 <- ucv(x1)
temp <- density(x1,from=a,to=b,n=1000,width=h1,window="gaussian")
f1 <- temp$y
plot(xgrid,f1,type="l",xlab="",ylab="f1")
bayesrule <- w*f1/( w*f1 + (1-w)*f0 )
plot(xgrid,bayesrule,type="l",xlab="",ylab="Pr(Y=1|Age)",ylim=c(0,1))
lines(xgrid,rep(.5,length(xgrid)))
temp <- xgrid[bayesrule > .5]
print(range(temp))

print(xgrid[723])

theta.hat <- w*f1[723]/(w*f1[723] + (1-w)*f0[723])
print(theta.hat)

######### Q(3)
psi.hat <- (12*49)/(4*39)
gamma.hat <- log(psi.hat)
print(psi.hat);print(gamma.hat)

B <- 1000
y1star <- rbinom(B,51,12/51)
y2star <- rbinom(B,53,4/53)
psi.star <- y1star*(53-y2star)/(y2star*(51-y1star))
I <- 1:1000
J <- I[ (psi.star!=0) & (psi.star!=Inf) ]
gamma.star <- log(psi.star[J])
se <- sqrt(var(gamma.star))
z <- gamma.hat/se
print(z)
pvalue <- 2*(1-pnorm(abs(z)))
print(pvalue)
print(quantile(psi.star,c(.025,.975)))

######### Q(4)

n1 <- c(55,33,70,49)
n2 <- c(141,145,139,161)
N1 <- sum(n1)
N2 <- sum(n2)
p1 <- n1/N1
p2 <- n2/N2
loglik <- sum(n1*log(p1))+sum(n2*log(p2))

n <- n1+n2
N <- N1+N2
p <- n/N
loglik0 <- sum(n*log(p))

lambda <- 2*(loglik - loglik0)
print(lambda)
pvalue <- 1-pchisq(lambda,3)
print(pvalue)

###### (Q5)

x <- c(7,27,55,5,32,50,3,8,7,13,18,24)
x <- matrix(x,3,4)
print(x)
m1 <- apply(x,1,sum)
m2 <- apply(x,2,sum)
n <- sum(x)
E <- outer(m1,m2,FUN="*")/n
chi2 <- sum((x-E)^2/E)
print(chi2)
pvalue <- 1-pchisq(chi2,6)
print(pvalue)
print(chisq.test(x))


p <- x/n
r <- E/n

rmultinomial.fun <- function(n,p){
     ### draw random vector X ~ multinomial(n,p)
     ### p is a vector of probabilities
     k <- length(p)
     x <- sample(1:k,size=n,replace=T,prob=p)
     y <- tabulate(x,k)
     return(y)
     }


theta.hat <- max(abs(p-r))
print(theta.hat)
B <- 1000
theta.star <- rep(0,B)
for(i in 1:B){
     xstar <- rmultinomial.fun(n,c(p))
     xstar <- matrix(xstar,3,4)
     m1 <- apply(xstar,1,sum)
     m2 <- apply(xstar,2,sum)
     E <- outer(m1,m2,FUN="*")/n
     pstar <- xstar/n
     r <- E/n
     theta.star[i] <- max(abs(pstar-r))
     }

print(quantile(theta.star,c(.025,.975)))



###### Q(6)
n <- c(80,75,85,67,85)
y <- c(45,26,52,35,37)
x <- matrix(0,2,2)
x[1,1] <- y[1]
x[2,1] <- n[1]-y[1]

for(i in 2:5){
     x[1,2] <- y[i]
     x[2,2] <- n[i]-y[i]
     p <- fisher.test(x)$p.value
     print(p)
     psi <- x[1,1]*x[2,2]/(x[1,2]*x[2,1])
     print(psi)
     }


