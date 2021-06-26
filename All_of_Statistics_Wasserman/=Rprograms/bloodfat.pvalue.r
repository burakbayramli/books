### two sample/ bloodfat data 
x <- scan("=hand.data/datasets/bloodfat.dat")
x <- matrix(x,ncol=2,byrow=T)
x <- x[,1]
x1 <- x[1:51]
x2 <- x[52:371]


n1 <- length(x1);mu1 <- mean(x1);s1 <-  sqrt(sum((x1-mu1)^2)/n1)
n2 <- length(x2);mu2 <- mean(x2);s2 <-  sqrt(sum((x2-mu2)^2)/n2)
se1 <- s1/sqrt(n1)
se2 <- s2/sqrt(n2)
se <- sqrt(se1^2 + se2^2)

z <- abs(mu1-mu2)/se
cat("Wald Statistic = ",z,"\n")
p <- 2*pnorm(-z)
cat("p-value = ",p,"\n")

print("estimates")
print(mu1)
print(mu2)
print("standard errors")
print(se1)
print(se2)
print(se)


cat("Difference of Medians","\n")
B <- 1000
m1 <- median(x1)
m2 <- median(x2)
theta <- m2 - m1
cat("theta hat = ",theta,"\n")
Tboot <- rep(0,B)
for(i in 1:B){
     xx1 <- sample(x1,replace=T)
     xx2 <- sample(x2,replace=T)
     Tboot[i] <- median(xx2) - median(xx1)
     }
se <- sqrt(var(Tboot))
cat("standard error = ",se,"\n")
z <- abs(m2-m1)/se
p <- 2*pnorm(-z)
cat("m1 = ",m1," m2 = ",m2," se = ",se," z = ",z,"p value = ",p,"\n")




















