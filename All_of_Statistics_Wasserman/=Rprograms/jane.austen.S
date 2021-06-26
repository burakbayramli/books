n1 <- c(147,25,32,94,59,18)
n2 <- c(186,26,39,105,74,10)
n3 <- c(101,11,15,37,28,10)
n4 <- c(83,29,15,22,43,4)

multinomial.loglik.fun <- function(n,p){
     N <- sum(n)
     return(sum(n*log(p)))
     }

n <- n1+n2 + n3
p <- n/sum(n)
lik0 <- multinomial.loglik.fun(n,p)

p1 <- n1/sum(n1);p2 <- n2/sum(n2);p3 <- n3/sum(n3);
lik1 <- multinomial.loglik.fun(n1,p1)+
      multinomial.loglik.fun(n2,p2)+
      multinomial.loglik.fun(n3,p3)

LR <- 2*(lik1-lik0)
print(LR)
df <- (length(n1)-1)+(length(n2)-1)+(length(n3)-1)-(length(n)-1)
print(df)
pvalue <- 1-pchisq(LR,df)
print(pvalue)


############
n <- n1+n2+n3;p <- n/sum(n)
              p4 <- n4/sum(n4)

lik1 <- multinomial.loglik.fun(n,p)+multinomial.loglik.fun(n4,p4)
n <- n1+n2+n3+n4;p <- n/sum(n)
lik0 <- multinomial.loglik.fun(n,p)
LR <- 2*(lik1-lik0)
print(LR)
df <- 5
print(df)
pvalue <- 1-pchisq(LR,df)
print(pvalue)


