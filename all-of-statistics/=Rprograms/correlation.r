### data p 191 sendecor cochran (blood pressure)
x1 <- c(100 , 105 , 110 , 110 , 120 , 120 , 125 , 130 , 130 , 150 , 170 , 195)
x2 <- c(65 , 65 , 75 , 70 , 78 , 80 , 75 , 82 , 80 , 90 , 95 , 90)
rho.hat <- cor(x1,x2)
n <- length(x1)
print(rho.hat)
B <- 10000
rho <- rep(0,B)
for(i in 1:B){
     o <- sample(1:n,replace=T,size=n)
     x1star <- x1[o]
     x2star <- x2[o]
     rho[i] <- cor(x1star,x2star)
     }
se <- sqrt(var(rho))
print(se)
w <- rho.hat/se
print(w)
print(2*pnorm(-abs(w)))
print(c(2*rho.hat-quantile(rho,.975),2*rho.hat-quantile(rho,.025)))
r1 <- rank(x1)
r2 <- rank(x2)
rho.hat <- cor(r1,r2)
print(rho.hat)
### null
r1 <- 1:n
for(i in 1:B){
     r2 <- sample(1:n,replace=F,size=n)
     rho[i] <- cor(r1,r2)
     }
pvalue <- sum( abs(rho) > abs(rho.hat))/B
print(pvalue)

