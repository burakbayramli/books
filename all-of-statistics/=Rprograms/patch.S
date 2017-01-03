x_scan("patch.data")
x_matrix(x,ncol=6,byrow=T)

y_x[,6]
z_x[,5]
n_length(y)

theta.hat_mean(y)/mean(z)
print(theta.hat)

n <- length(y)
B <- 1000
theta.boot <- rep(0,B)
index <- 1:n
for(i in 1:B){
     j <- sample(index,replace=T)
     ystar <- y[j]
     zstar <- z[j]
     theta.boot[i] <- mean(ystar)/mean(zstar)
     }
se <- sqrt(var(theta.boot))
print(se)

postscript("patch.ps",horizontal=F)
par(mfrow=c(1,1)) ### put several plots per page
                  ### 2 rows and 1 column of plots
hist(theta.boot,nclass=20,xlab="Bootstrap Samples")

alpha <- .05
j <- round(B*alpha/2)
k <- round(B*(1-(alpha/2)))
theta.boot <- sort(theta.boot)
lower <- theta.boot[j]
upper <- theta.boot[k]
print(lower)
print(upper)







