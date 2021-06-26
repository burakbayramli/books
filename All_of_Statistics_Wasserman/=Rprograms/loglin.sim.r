onesim <- function(n){
     x1  <- (rbinom(n,1,.5))
     x2  <- (rbinom(n,1,.25*(1-x1) + .75*x1))
     x3  <- (rbinom(n,1,.25*(1-x2) + .75*x2))
     x4  <- (rbinom(n,1,.25*(1-x3) + .75*x3))
     x5  <- (rbinom(n,1,.25*(1-x4) + .75*x4))


     X1 <- (c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))
     X2 <- (c(0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1))
     X3 <- (c(0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1))
     X4 <- (c(0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1))
     X5 <- (c(0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1))
     p <- rep(.5,32)*(.75^X1)*(.25^(1-X1))*(.75^X2)*(.25^(1-X2))*
                     (.75^X3)*(.25^(1-X3))*(.75^X4)*(.25^(1-X4))

     X1 <- as.factor(X1);X2 <- as.factor(X2);X3 <- as.factor(X3);
     X4 <- as.factor(X4);X5 <- as.factor(X5);
     counts <- rep(0,32)
     code <- ( x5 + (2*x4) + (4*x3) + (8*x2) + (16*x1)) + 1
     counts <- pmax(tabulate(code,32),1)
     out <- glm(counts ~ X1*X2*X3*X4*X5,family=poisson)
     bic <- step(out,direction="forward",trace=0,k=log(n));     bic <- bic$fitted/n
     aic <- step(out,direction="forward",trace=0);              aic <- aic$fitted/n
     mle <- counts/n; mle <- mle/sum(mle)
     d.mle <- sum(p*log(p/mle))
     d.aic <- sum(p*log(p/aic))
     d.bic <- sum(p*log(p/bic))
     return(d.mle,d.aic,d.bic)
     }



n <- 100
nsim <- 100
mle <- rep(0,nsim)
aic <- rep(0,nsim)
bic <- rep(0,nsim)
for(i in 1:nsim){
     print(i)
     temp <- onesim(n)
     mle[i] <- temp[[1]]
     aic[i] <- temp[[2]]
     bic[i] <- temp[[3]]
     }





