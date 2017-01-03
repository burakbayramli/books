theta.fun <- function(y,z){
     ### this function computes the correlation coefficient
     mean.y <- mean(y)
     mean.z <- mean(z)
     s.y    <- sqrt(var(y))
     s.z    <- sqrt(var(z))
     top    <- sum((y-mean.y)*(z-mean.z))
     bottom <- sqrt( sum((y-mean.y)^2)*sum((z-mean.z)^2) )
     output <- top/bottom
     return(output)
     }

y <- c(576,635,558,578,666,580,555,661,651,605,653,575,545,572,594)
z <- c(3.39,3.30,2.81,3.03,3.44,3.07,3.00,3.43,3.36,3.13,3.12,2.74,2.76,2.88,2.96)


n <- length(y)
theta.hat <- theta.fun(y,z)
print(theta.hat)
B <- 1000
theta.boot <- rep(0,B)
index <- 1:n
for(i in 1:B){
     j <- sample(index,replace=T)
     ystar <- y[j]
     zstar <- z[j]
     theta.boot[i] <- theta.fun(ystar,zstar)
     }


se <- sqrt(var(theta.boot))
print(se)

postscript("lsat.ps",horizontal=F)
par(mfrow=c(2,1)) ### put several plots per page
                  ### 2 rows and 1 column of plots
plot(y,z,xlab="LSAT",ylab="GPA")
hist(theta.boot,nclass=20,xlab="Bootstrap Samples")

