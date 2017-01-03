### small data sets p 60
x <- c(74,68,154,18,18,16,54,10,12,12,58,44)
x <- matrix(x,4,3)
n <- sum(x)
row <- apply(x,1,sum)
col <- apply(x,2,sum)
E <- matrix(0,4,3)
for(i in 1:4){
     for(j in 1:3){
          E[i,j] <- row[i]*col[j]/n
          }
     }
U <- sum((x-E)^2/E)
print(U);print(1-pchisq(U,6))
T <- 0
for(i in 1:4){
     for(j in 1:3){
          T <- T + x[i,j]*log( (x[i,j]*n)/(row[i]*col[j]) )
          }
     }
T <- 2*T
print(T);print(1-pchisq(T,6))


### compute d_{TV}(P, QtimesR)
rmultinomial <- function(n,P){
     nr <- nrow(P)
     nc <- ncol(P)
     p <- c(P)
     k <- length(p)
     x <- sample(1:k,replace=T,size=n,prob=p)
     x <- tabulate(x,k)
     x <- matrix(x,nr,nc)
     }



d <- function(X,nboot){
     n <- sum(X)
     nr <- nrow(X)
     mc <- ncol(X)
     P <- X/n
     r <- apply(P,1,sum)
     c <- apply(P,2,sum)
     Q <- outer(r,c,FUN="*")
     d.hat <- .5*sum(abs(P-Q))
     out <- rep(0,nboot)

     ### bootstrap
     for(i in 1:B){
          Xstar <- rmultinomial(n,P)
          Pstar <- Xstar/n
          r <- apply(Pstar,1,sum)
          c <- apply(Pstar,2,sum)
          Q <- outer(r,c,FUN="*")
          out[i] <- .5*sum(abs(Pstar-Q))
          }
     left  <- 2*d.hat - quantile(out,.975)
     right <- 2*d.hat - quantile(out,.025)
     if(left <0)left <- 0
     if(right >1)right <- 1
     ci <- c(left,right)
     return(d.hat,ci)
     }


X <- x
nboot <- 10000
out <- d(X,nboot)
print(out)


