### two sample/ bloodfat data 
postscript("bloodfat.ps",horizontal = FALSE, onefile = FALSE, paper ="special",
           width=6,height=6)
par(mfrow=c(2,1))
x <- scan("=hand.data/datasets/bloodfat.dat")
x <- matrix(x,ncol=2,byrow=T)
x <- x[,1]
x1 <- x[1:51]
x2 <- x[52:371]
b <- seq(min(x),max(x),length=11)
hist(x1,xlim=c(min(x),max(x)),prob=T,breaks=b,yaxt="n",ylab="",main="",
     xlab="plasma cholesterol for patients without heart disease")
hist(x2,xlim=c(min(x),max(x)),prob=T,breaks=b,yaxt="n",ylab="",main="",
     xlab="plasma cholesterol for patients with heart disease")


n1 <- length(x1);mu1 <- mean(x1);s1 <-  sqrt(sum((x1-mu1)^2)/n1)
n2 <- length(x2);mu2 <- mean(x2);s2 <-  sqrt(sum((x2-mu2)^2)/n2)
se1 <- s1/sqrt(n1)
se2 <- s2/sqrt(n2)
print("standard errors")
print(se1)
print(se2)
print(c(mu1-2*se1,mu1+2*se1))
print(c(mu2-2*se2,mu2+2*se2))
delta <- mu1 - mu2
se <- sqrt((s1^2/n1) + (s2^2/n2))
print(c(delta-2*se,delta+2*se))

cat("Difference of Medians","\n")
### bootstrap se for difference of medians
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
C <- c(theta - 2*se,theta + 2*se)
cat("Normal interval = ",C,"\n")
C <- c(quantile(Tboot,.025),quantile(Tboot,.975))
cat("percentile interval = ",C,"\n")
C <- c(2*theta-quantile(Tboot,.975),2*theta-quantile(Tboot,.025))
cat("pivotal = ",C,"\n")



dev.off()



















